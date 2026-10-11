# evaluation of the backtest forecasts, all models on the same hours
# accuracy (mae, rmse, rmae), diebold-mariano and giacomini-white tests,
# bias (mincer-zarnowitz), shape of the day (cheapest / most expensive hour),
# and a model confidence set over the 16 BMT-X combinations
# needs forecasts_lear.rds (04_lear.R), forecasts_bmt.rds (06_bmt.R) and forecasts_bmtx.rds (08_bmtx.R)
# writes small csv files to results/ that the dashboard reads

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
source("R/lear.R")

# forecasts and actual prices
prices <- readRDS("data/processed/prices_dk1_hourly.rds")
daily  <- build_daily(prices)
P      <- as.matrix(daily[, -1])

naive_day <- tibble(date      = rep(daily$date + 1, each = 24),
                    hour      = rep(0:23, times = nrow(P)),
                    naive_day = as.vector(t(P)))

fc <- readRDS("data/processed/forecasts_lear.rds") |>
  select(date, hour, actual, lear, naive_mix) |>
  inner_join(select(readRDS("data/processed/forecasts_bmt.rds"), date, hour, bmt),
             by = c("date", "hour")) |>
  inner_join(naive_day, by = c("date", "hour"))

# BMT-X and LEAR-X with all four groups as extra models
bmtx <- readRDS("data/processed/forecasts_bmtx.rds")
all_groups <- "nb+wind+solar+hol"
fc <- fc |>
  inner_join(bmtx |> filter(combo == all_groups) |>
               select(date, hour, model, forecast) |>
               pivot_wider(names_from = model, values_from = forecast),
             by = c("date", "hour")) |>
  drop_na() |>
  group_by(date) |>
  filter(n() == 24) |>
  ungroup() |>
  arrange(date, hour)

models <- c("bmtx", "learx", "bmt", "lear", "naive_day", "naive_mix")
cat("evaluation period:", format(min(fc$date)), "to", format(max(fc$date)),
    "-", n_distinct(fc$date), "days,", nrow(fc), "hours\n")


# accuracy
mae_naive <- mean(abs(fc$actual - fc$naive_day))
accuracy <- tibble(model = models) |>
  rowwise() |>
  mutate(mae  = mean(abs(fc$actual - fc[[model]])),
         rmse = sqrt(mean((fc$actual - fc[[model]])^2)),
         rmae = mae / mae_naive) |>
  ungroup() |>
  mutate(n_hours = nrow(fc), n_days = n_distinct(fc$date),
         from = min(fc$date), to = max(fc$date)) |>
  arrange(mae)
print(accuracy)


# tests on the daily loss differential (multivariate dm, as in lago et al. 2021)
# d_t = mean over the 24 hours of |e_a| - |e_b|, negative = model a more accurate
daily_loss <- function(m, loss = c("abs", "sq")) {
  loss <- match.arg(loss)
  e <- fc$actual - fc[[m]]
  l <- if (loss == "abs") abs(e) else e^2
  as.numeric(tapply(l, fc$date, mean))
}

# dm test with the harvey-leybourne-newbold correction, one-step ahead
dm_test <- function(d) {
  n    <- length(d)
  stat <- mean(d) / sqrt(var(d) / n)
  stat <- stat * sqrt((n - 1) / n)   # hln correction for h = 1
  c(stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}

# giacomini-white conditional test, instruments = constant and yesterday's differential
gw_test <- function(d) {
  Z  <- cbind(1, head(d, -1)) * tail(d, -1)
  n  <- nrow(Z)
  zb <- colMeans(Z)
  stat <- n * drop(t(zb) %*% solve(crossprod(Z) / n) %*% zb)
  c(stat = stat, p = pchisq(stat, df = ncol(Z), lower.tail = FALSE))
}

# per hour: in how many of the 24 hours is a significantly better / worse (holm corrected)
hour_tests <- function(a, b) {
  p_better <- sapply(0:23, \(h) {
    x <- fc[fc$hour == h, ]
    d <- abs(x$actual - x[[a]]) - abs(x$actual - x[[b]])
    stat <- mean(d) / sqrt(var(d) / length(d))
    c(lower = pt(stat, length(d) - 1), upper = pt(stat, length(d) - 1, lower.tail = FALSE))
  })
  c(hours_a_better = sum(p.adjust(p_better["lower", ], "holm") < 0.05),
    hours_b_better = sum(p.adjust(p_better["upper", ], "holm") < 0.05))
}

pairs <- combn(models, 2, simplify = FALSE)
tests <- bind_rows(lapply(pairs, \(ab) {
  a <- ab[1]; b <- ab[2]
  d_abs <- daily_loss(a, "abs") - daily_loss(b, "abs")
  d_sq  <- daily_loss(a, "sq")  - daily_loss(b, "sq")
  dm  <- dm_test(d_abs)
  dm2 <- dm_test(d_sq)
  gw  <- gw_test(d_abs)
  ht  <- hour_tests(a, b)
  tibble(model_a = a, model_b = b,
         mae_diff = mean(d_abs),
         dm_stat = dm[["stat"]], dm_p = dm[["p"]],
         dm_sq_stat = dm2[["stat"]], dm_sq_p = dm2[["p"]],
         gw_stat = gw[["stat"]], gw_p = gw[["p"]],
         hours_a_better = ht[["hours_a_better"]], hours_b_better = ht[["hours_b_better"]])
}))
print(select(tests, model_a, model_b, mae_diff, dm_p, gw_p, hours_a_better, hours_b_better))


# bias: mean error and mincer-zarnowitz (actual = a + b * forecast, test a = 0 and b = 1)
# newey-west covariance with 24 lags because hourly errors are autocorrelated
nw_vcov <- function(X, e, lag = 24) {
  n <- nrow(X)
  u <- X * e
  S <- crossprod(u) / n
  for (k in 1:lag) {
    G <- crossprod(u[-(1:k), , drop = FALSE], u[1:(n - k), , drop = FALSE]) / n
    S <- S + (1 - k / (lag + 1)) * (G + t(G))
  }
  B <- solve(crossprod(X) / n)
  B %*% S %*% B / n
}

bias <- bind_rows(lapply(models, \(m) {
  X    <- cbind(1, fc[[m]])
  y    <- fc$actual
  beta <- drop(solve(crossprod(X), crossprod(X, y)))
  V    <- nw_vcov(X, drop(y - X %*% beta))
  r    <- beta - c(0, 1)
  wald <- drop(t(r) %*% solve(V) %*% r)
  tibble(model = m,
         mean_error   = mean(fc$actual - fc[[m]]),
         mz_intercept = beta[1],
         mz_slope     = beta[2],
         mz_p         = pchisq(wald, df = 2, lower.tail = FALSE))
}))
print(bias)


# shape of the day: does the forecast find the cheapest and most expensive hour?
shape <- bind_rows(lapply(models, \(m) {
  fc |>
    group_by(date) |>
    summarise(cheap_true = hour[which.min(actual)], cheap_fc = hour[which.min(.data[[m]])],
              peak_true  = hour[which.max(actual)], peak_fc  = hour[which.max(.data[[m]])],
              spearman   = cor(actual, .data[[m]], method = "spearman"),
              .groups = "drop") |>
    summarise(model          = m,
              cheapest_exact = mean(cheap_true == cheap_fc),
              cheapest_pm1   = mean(abs(cheap_true - cheap_fc) <= 1),
              peak_exact     = mean(peak_true == peak_fc),
              peak_pm1       = mean(abs(peak_true - peak_fc) <= 1),
              spearman       = mean(spearman, na.rm = TRUE))
}))
print(shape)


# model confidence set (hansen, lunde & nason 2011) over the 16 BMT-X combinations, LEAR-X and LEAR
# loss = daily average absolute error, Tmax statistic, circular block bootstrap with 7-day blocks.
# the set at 10% = the combinations that can't be told apart from the best one
mcs <- function(L, B = 2000, block = 7, seed = 1) {
  set.seed(seed)
  n <- nrow(L)
  idx <- replicate(B, {
    starts <- sample.int(n, ceiling(n / block), replace = TRUE)
    (as.vector(outer(0:(block - 1), starts, "+")) - 1) %% n + 1
  })[1:n, ]
  boot_means_all <- apply(idx, 2, \(i) colMeans(L[i, , drop = FALSE]))   # models x B

  alive <- colnames(L)
  p_run <- 0
  out   <- tibble(model = character(), mcs_p = numeric())
  while (length(alive) > 1) {
    lbar  <- colMeans(L[, alive, drop = FALSE])
    d_i   <- lbar - mean(lbar)
    bm    <- boot_means_all[alive, , drop = FALSE]
    bd_i  <- sweep(bm, 2, colMeans(bm))                    # bootstrap d_i
    v_i   <- rowMeans((bd_i - d_i)^2)
    t_i   <- d_i / sqrt(v_i)
    t_max <- max(t_i)
    boot  <- apply((bd_i - d_i) / sqrt(v_i), 2, max)
    p_run <- max(p_run, mean(boot >= t_max))
    worst <- alive[which.max(t_i)]
    out   <- bind_rows(out, tibble(model = worst, mcs_p = p_run))
    alive <- setdiff(alive, worst)
  }
  bind_rows(out, tibble(model = alive, mcs_p = 1))
}

combo_fc <- bmtx |>
  semi_join(fc, by = c("date", "hour")) |>
  mutate(name = if_else(model == "bmtx", paste0("bmtx:", combo), "learx")) |>
  select(date, hour, name, forecast, actual)
L <- bind_rows(combo_fc,
               fc |> transmute(date, hour, name = "lear", forecast = lear, actual)) |>
  group_by(date, name) |>
  summarise(loss = mean(abs(actual - forecast)), .groups = "drop") |>
  pivot_wider(names_from = name, values_from = loss) |>
  select(-date) |>
  as.matrix()
mcs_res <- mcs(L)

combo_tab <- combo_fc |>
  filter(startsWith(name, "bmtx:")) |>
  group_by(name) |>
  summarise(mae = mean(abs(actual - forecast)), rmse = sqrt(mean((actual - forecast)^2)),
            .groups = "drop") |>
  mutate(rmae  = mae / mae_naive,
         combo = sub("^bmtx:", "", name)) |>
  left_join(mcs_res, by = c("name" = "model")) |>
  mutate(nb    = grepl("nb", combo), wind = grepl("wind", combo),
         solar = grepl("solar", combo), hol = grepl("hol", combo),
         in_mcs = mcs_p >= 0.10) |>
  select(combo, nb, wind, solar, hol, mae, rmse, rmae, mcs_p, in_mcs) |>
  arrange(mae)
print(combo_tab, n = Inf)
print(mcs_res, n = Inf)

dir.create("results", showWarnings = FALSE)
write_csv(accuracy, "results/eval_accuracy.csv")
write_csv(tests,    "results/eval_tests.csv")
write_csv(bias,     "results/eval_bias.csv")
write_csv(shape,    "results/eval_shape.csv")
write_csv(combo_tab, "results/bmtx_combos.csv")
write_csv(mcs_res,  "results/mcs.csv")

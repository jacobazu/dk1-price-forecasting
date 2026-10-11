# evaluation, runs daily on github
# backtest csv rebuilt locally

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
source("R/lear.R")
source("R/mcs.R")
source("R/forecasts.R")
source("R/postprocess.R")

# models on the dashboard
models <- c("ensemble", "bmtx", "learx", "xgbx", "naive_mix")
# kept in backtest file
stored <- c("bmtx", "learx", "xgbx", "bmt", "lear", "xgb", "naive_day", "naive_mix")
dir.create("results", showWarnings = FALSE)

prices <- readRDS("data/processed/prices_dk1_hourly.rds")
daily  <- build_daily(prices)
P      <- as.matrix(daily[, -1])
actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))


# rebuild backtest forecasts
bt_files <- paste0("data/processed/forecasts_", c("lear", "bmt", "bmtx", "xgb", "weather"), ".rds")
if (all(file.exists(bt_files))) {
  wide <- function(x) x |> select(date, hour, model, forecast) |>
    pivot_wider(names_from = model, values_from = forecast)
  backtest <- readRDS(bt_files[1]) |>
    select(date, hour, actual, lear, naive_mix) |>
    inner_join(select(readRDS(bt_files[2]), date, hour, bmt), by = c("date", "hour")) |>
    # -x models with weather
    inner_join(wide(readRDS(bt_files[5])), by = c("date", "hour")) |>
    # plain xgboost
    inner_join(wide(filter(readRDS(bt_files[4]), model == "xgb")), by = c("date", "hour")) |>
    inner_join(transmute(actual, date = date + 1, hour, naive_day = actual), by = c("date", "hour")) |>
    drop_na() |>
    group_by(date) |>
    filter(n() == 24) |>
    ungroup() |>
    select(date, hour, actual, all_of(stored)) |>
    arrange(date, hour)
  write_csv(mutate(backtest, across(-c(date, hour), \(x) round(x, 2))), "results/backtest_forecasts.csv")
}
fc <- all_forecasts(models)
backtest <- filter(fc, source == "backtest")
live     <- filter(fc, source == "live")

cat("evaluation period:", format(min(fc$date)), "to", format(max(fc$date)), "-",
    n_distinct(backtest$date), "backtest days and", n_distinct(live$date), "live days\n")


# accuracy, rmae vs weekly naive
mae_naive <- mean(abs(fc$actual - fc$naive_mix))
accuracy <- tibble(model = models) |>
  rowwise() |>
  mutate(mae  = mean(abs(fc$actual - fc[[model]])),
         rmse = sqrt(mean((fc$actual - fc[[model]])^2)),
         rmae = mae / mae_naive) |>
  ungroup() |>
  mutate(n_hours = nrow(fc), n_days = n_distinct(fc$date),
         from = min(fc$date), to = max(fc$date),
         n_backtest_days = n_distinct(fc$date[fc$source == "backtest"]),
         backtest_to     = max(fc$date[fc$source == "backtest"]),
         n_live_days     = n_distinct(fc$date[fc$source == "live"])) |>
  arrange(mae)
print(accuracy)


# tests on daily loss differential
# negative = a more accurate
daily_loss <- function(m, loss = c("abs", "sq")) {
  loss <- match.arg(loss)
  e <- fc$actual - fc[[m]]
  l <- if (loss == "abs") abs(e) else e^2
  as.numeric(tapply(l, fc$date, mean))
}

# dm test, hln correction
dm_test <- function(d) {
  n    <- length(d)
  stat <- mean(d) / sqrt(var(d) / n)
  stat <- stat * sqrt((n - 1) / n)   # hln, h = 1
  c(stat = stat, p = 2 * pt(-abs(stat), df = n - 1))
}

# giacomini-white test
gw_test <- function(d) {
  Z  <- cbind(1, head(d, -1)) * tail(d, -1)
  n  <- nrow(Z)
  zb <- colMeans(Z)
  stat <- n * drop(t(zb) %*% solve(crossprod(Z) / n) %*% zb)
  c(stat = stat, p = pchisq(stat, df = ncol(Z), lower.tail = FALSE))
}

# hours better / worse (holm)
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


# bias: mean error and mz
# newey-west, 24 lags
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


# cheapest and peak hour
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


# model confidence set
L_models <- sapply(models, \(m) daily_loss(m, "abs"))
mcs_models <- mcs(L_models) |>
  mutate(in_mcs = mcs_p >= 0.10)
print(mcs_models)

# bias correction and bands
# mz vs raw and median
scale <- vol_scale(bind_rows(select(fc, date, hour, actual), actual), sort(unique(fc$date)))
post <- bind_rows(lapply(setdiff(models, "naive_mix"), \(m) {
  x  <- postprocess(fc, m, scale)
  dl <- x |>
    group_by(date) |>
    summarise(raw = mean(abs(actual - forecast)),
              med = mean(abs(actual - corrected)),
              mz  = mean(abs(actual - corrected_mz)), .groups = "drop")
  dm        <- dm_test(dl$med - dl$raw)
  dm_mz     <- dm_test(dl$mz - dl$raw)
  dm_mz_med <- dm_test(dl$mz - dl$med)
  tibble(model        = m,
         days         = n_distinct(x$date),
         mae_raw      = mean(abs(x$actual - x$forecast)),
         mae_corr     = mean(abs(x$actual - x$corrected)),
         dm_p         = dm[["p"]],
         mean_error_raw  = mean(x$actual - x$forecast),
         mean_error_corr = mean(x$actual - x$corrected),
         coverage     = mean(x$actual >= x$lower_s & x$actual <= x$upper_s),   # scaled band (shown)
         below        = mean(x$actual < x$lower_s),
         above        = mean(x$actual > x$upper_s),
         width        = mean(x$upper_s - x$lower_s),
         coverage_plain = mean(x$actual >= x$lower & x$actual <= x$upper),   # without scaling
         width_plain    = mean(x$upper - x$lower),
         # interval score, lower is better
         score        = mean((x$upper_s - x$lower_s) + 2 / (1 - diff(q_probs)) *
                               (pmax(x$lower_s - x$actual, 0) + pmax(x$actual - x$upper_s, 0))),
         score_plain  = mean((x$upper - x$lower) + 2 / (1 - diff(q_probs)) *
                               (pmax(x$lower - x$actual, 0) + pmax(x$actual - x$upper, 0))),
         nominal      = diff(q_probs),
         mae_mz          = mean(abs(x$actual - x$corrected_mz)),
         mean_error_mz   = mean(x$actual - x$corrected_mz),
         dm_p_mz         = dm_mz[["p"]],
         dm_p_mz_med     = dm_mz_med[["p"]],
         mz_slope_median = median(x$mz_b))
}))
print(post)
write_csv(post, "results/eval_post.csv")

write_csv(accuracy, "results/eval_accuracy.csv")
write_csv(tests,    "results/eval_tests.csv")
write_csv(bias,     "results/eval_bias.csv")
write_csv(shape,    "results/eval_shape.csv")
write_csv(mcs_models, "results/mcs_models.csv")

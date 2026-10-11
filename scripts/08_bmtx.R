# bmt-x, 16 group combinations
# neighbours, wind, solar, holidays
# lear-x on same variables

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(parallel)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/mcs.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P
D        <- features$X_dow[, -1]   # monday = baseline

exog <- make_exog(daily$date,
                  readRDS("data/raw/exog_neighbours.rds"),
                  readRDS("data/raw/exog_wind_solar.rds"),
                  readRDS("data/processed/holidays_dk.rds"))

groups   <- c("nb", "wind", "solar", "hol")
combos   <- expand.grid(rep(list(c(FALSE, TRUE)), 4))
names(combos) <- groups
combo_id <- apply(combos, 1, \(r) if (!any(r)) "prices" else paste(groups[r], collapse = "+"))

# wind/solar only in study
learx_combos <- c("nb+hol", "nb+wind+solar+hol")

window <- 730
pval   <- 0.05
delta1 <- 1
delta2 <- 1
cores  <- max(1, detectCores() - 1)

test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))


forecast_day <- function(d) {
  train <- (d - window):(d - 1)
  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)

  ex     <- transform_exog(exog, train)
  X_full <- cbind(transform_p(features$X_price, med, s), ex$nb, ex$wind, ex$solar, ex$hol)
  grp    <- rep(c("prices", groups), c(ncol(features$X_price), ncol(ex$nb), ncol(ex$wind),
                                       ncol(ex$solar), ncol(ex$hol)))
  if (anyNA(X_full[d, ])) return(NULL)

  # same training rows everywhere
  keep <- train[complete.cases(X_full[train, ]) & complete.cases(P[train, ])]
  Xtr  <- X_full[keep, ]
  Dtr  <- D[keep, ]

  # controls removed once
  qC  <- qr(cbind(1, Dtr))
  E0  <- qr.resid(qC, Xtr)
  df0 <- length(keep) - qC$rank
  ss0 <- colSums(scale(Xtr, scale = FALSE)^2)

  rows <- list()
  for (h in 1:24) {
    y   <- transform_p(P[keep, h], med, s)
    ey0 <- qr.resid(qC, y)

    for (k in seq_along(combo_id)) {
      cols <- which(grp %in% c("prices", groups[unlist(combos[k, ])]))
      sel  <- cols[bmt_select_resid(E0[, cols], ey0, df0, ss0[cols], pval, delta1, delta2)]

      Z    <- cbind(1, Dtr, Xtr[, sel, drop = FALSE])
      beta <- qr.coef(qr(Z), y)
      beta[is.na(beta)] <- 0
      z    <- c(1, D[d, ], X_full[d, sel])

      rows[[length(rows) + 1]] <- tibble(
        date = daily$date[d], hour = h - 1, model = "bmtx", combo = combo_id[k],
        forecast = inverse_p(sum(z * beta), med, s),
        n_sel = length(sel),
        n_prices = sum(grp[sel] == "prices"), n_nb = sum(grp[sel] == "nb"),
        n_wind = sum(grp[sel] == "wind"), n_solar = sum(grp[sel] == "solar"),
        n_hol = sum(grp[sel] == "hol"))
    }

    # lear-x, same pool
    for (lc in learx_combos) {
      cols <- which(grp %in% c("prices", strsplit(lc, "+", fixed = TRUE)[[1]]))
      m  <- fit_lasso_bic(cbind(Xtr[, cols], features$X_dow[keep, ]), y)
      xl <- matrix(c(X_full[d, cols], features$X_dow[d, ]), nrow = 1)
      rows[[length(rows) + 1]] <- tibble(
        date = daily$date[d], hour = h - 1, model = "learx", combo = lc,
        forecast = as.numeric(inverse_p(predict(m$fit, xl, s = m$lambda), med, s)))
    }
  }
  bind_rows(rows)
}

t0  <- Sys.time()
out <- mclapply(test_days, \(d) tryCatch(forecast_day(d), error = \(e) NULL), mc.cores = cores)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

actual <- tibble(date   = rep(daily$date, each = 24),
                 hour   = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))
fc <- bind_rows(out) |>
  inner_join(actual, by = c("date", "hour"))

# mae per combination
fc |>
  group_by(model, combo) |>
  summarise(mae = mean(abs(actual - forecast)), n_sel = mean(n_sel), .groups = "drop") |>
  arrange(mae) |>
  print(n = Inf)

saveRDS(fc, "data/processed/forecasts_bmtx.rds")

# variables picked per group
dir.create("results", showWarnings = FALSE)
fc |>
  filter(model == "bmtx") |>
  group_by(combo) |>
  summarise(across(c(n_sel, n_prices, n_nb, n_wind, n_solar, n_hol), mean), .groups = "drop") |>
  write_csv("results/bmtx_selection.csv")


# mcs over combinations
lear <- readRDS("data/processed/forecasts_lear.rds") |>
  transmute(date, hour, name = "lear", forecast = lear, actual)
study <- fc |>
  filter(model == "bmtx" | combo == "nb+wind+solar+hol") |>
  mutate(name = if_else(model == "bmtx", paste0("bmtx:", combo), "learx")) |>
  select(date, hour, name, forecast, actual) |>
  bind_rows(lear) |>
  group_by(date) |>
  filter(n_distinct(name) == 18, n() == 18 * 24) |>   # complete days only
  ungroup()

naive <- tibble(date = rep(daily$date + 1, each = 24), hour = rep(0:23, times = nrow(P)),
                naive_day = as.vector(t(P))) |>
  semi_join(study, by = c("date", "hour")) |>
  inner_join(distinct(study, date, hour, actual), by = c("date", "hour"))
mae_naive <- mean(abs(naive$actual - naive$naive_day))

L <- study |>
  group_by(date, name) |>
  summarise(loss = mean(abs(actual - forecast)), .groups = "drop") |>
  tidyr::pivot_wider(names_from = name, values_from = loss) |>
  select(-date) |>
  as.matrix()
mcs_res <- mcs(L)

combo_tab <- study |>
  filter(startsWith(name, "bmtx:")) |>
  group_by(name) |>
  summarise(mae = mean(abs(actual - forecast)), rmse = sqrt(mean((actual - forecast)^2)),
            .groups = "drop") |>
  mutate(rmae  = mae / mae_naive,
         combo = sub("^bmtx:", "", name)) |>
  left_join(mcs_res, by = c("name" = "model")) |>
  mutate(nb    = grepl("nb", combo), wind = grepl("wind", combo),
         solar = grepl("solar", combo), hol = grepl("hol", combo),
         in_mcs = mcs_p >= 0.10,
         from = min(study$date), to = max(study$date)) |>
  select(combo, nb, wind, solar, hol, mae, rmse, rmae, mcs_p, in_mcs, from, to) |>
  arrange(mae)
print(combo_tab, n = Inf)

write_csv(combo_tab, "results/bmtx_combos.csv")
write_csv(mcs_res, "results/mcs.csv")

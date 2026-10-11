# XGBoost backtest, prices only (XGB) and with all exogenous groups (XGB-X)
# one pooled model for all 24 hours, 2 year rolling window, retrained every day like LEAR and BMT
# tree depth and number of rounds are tuned on apr-sep 2025 (before the test period starts),
# then kept fixed for the whole test period
# needs the files from 02_clean.R and 07_download_exog.R

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(parallel)
source("R/lear.R")
source("R/exog.R")
source("R/xgb.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P

exog <- make_exog(daily$date,
                  readRDS("data/raw/exog_neighbours.rds"),
                  readRDS("data/raw/exog_wind_solar.rds"),
                  readRDS("data/processed/holidays_dk.rds"))

window <- 730
cores  <- max(1, detectCores() - 1)
variants <- list(xgb = NULL, xgbx = exog)


# tuning: every third day of apr-sep 2025
depths <- c(4, 6, 8)
rounds <- c(250, 500, 750, 1000, 1500)
val_days <- which(daily$date >= as_date("2025-04-01") & daily$date <= as_date("2025-09-30") &
                    complete.cases(P))
val_days <- val_days[seq(1, length(val_days), by = 3)]

grid <- expand.grid(model = names(variants), depth = depths, stringsAsFactors = FALSE)
t0 <- Sys.time()
tuning <- bind_rows(mclapply(seq_len(nrow(grid)), \(g) {
  params <- modifyList(xgb_params, list(max_depth = grid$depth[g]))
  err <- lapply(val_days, \(d) {
    fc <- tryCatch(xgb_forecast_day(features, d, window, variants[[grid$model[g]]], params, rounds),
                   error = \(e) NULL)
    if (is.null(fc)) NULL else abs(P[d, ] - fc)
  })
  err <- do.call(rbind, err)
  tibble(model = grid$model[g], max_depth = grid$depth[g], rounds = rounds,
         mae = colMeans(err), n_days = nrow(err) / 24)
}, mc.cores = cores))
cat("tuning done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
print(arrange(tuning, model, mae), n = Inf)

best <- tuning |>
  group_by(model) |>
  slice_min(mae, n = 1, with_ties = FALSE) |>
  ungroup()
print(best)


# test period, same days as the other models
test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))

t0 <- Sys.time()
out <- mclapply(test_days, \(d) {
  bind_rows(lapply(names(variants), \(v) {
    b  <- best[best$model == v, ]
    fc <- tryCatch(xgb_forecast_day(features, d, window, variants[[v]],
                                    modifyList(xgb_params, list(max_depth = b$max_depth)),
                                    b$rounds),
                   error = \(e) NULL)
    if (is.null(fc)) NULL else tibble(date = daily$date[d], hour = 0:23, model = v, forecast = fc)
  }))
}, mc.cores = cores)
cat("backtest done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

actual <- tibble(date   = rep(daily$date, each = 24),
                 hour   = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))
fc <- bind_rows(out) |>
  inner_join(actual, by = c("date", "hour"))

fc |>
  group_by(model) |>
  summarise(mae = mean(abs(actual - forecast)), days = n_distinct(date), .groups = "drop") |>
  print()

saveRDS(fc, "data/processed/forecasts_xgb.rds")


# which inputs the trees use most (gain), from the model on the latest window
d   <- max(test_days)
imp <- bind_rows(lapply(names(variants), \(v) {
  b <- best[best$model == v, ]
  train <- (d - window):(d - 1)
  X <- xgb_design(features, train, variants[[v]])
  y <- transform_p(long(P[train, ]), median(P[train, ], na.rm = TRUE), mad(P[train, ], na.rm = TRUE))
  set.seed(d)
  m <- xgb.train(params = modifyList(xgb_params, list(max_depth = b$max_depth)),
                 data = xgb.DMatrix(X[!is.na(y), ], label = y[!is.na(y)]),
                 nrounds = b$rounds, verbose = 0)
  as_tibble(xgb.importance(model = m)) |>
    transmute(model = v, feature = Feature, gain = Gain)
}))

dir.create("results", showWarnings = FALSE)
write_csv(mutate(tuning, chosen = paste(model, max_depth, rounds) %in%
                   paste(best$model, best$max_depth, best$rounds)),
          "results/xgb_tuning.csv")
write_csv(imp, "results/xgb_importance.csv")

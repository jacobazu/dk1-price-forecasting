# -x models with weather
# weather archive starts mar 2024
# xgboost settings from 09_xgb.R

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(parallel)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/xmodels.R")
source("R/xgb.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P

exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"), NULL,
                  readRDS("data/processed/holidays_dk.rds"))
exog$wx <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))

groups <- c("nb", "hol", "wx")
tun    <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |> filter(model == "xgbx", chosen)
cores  <- as.integer(Sys.getenv("CORES", max(1, detectCores() - 1)))

test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))

run_day <- function(d) {
  f <- list(
    bmtx  = tryCatch(bmtx_forecast_day(features, exog, d, groups), error = \(e) NULL),
    learx = tryCatch(learx_forecast_day(features, exog, d, groups), error = \(e) NULL),
    xgbx  = tryCatch(xgb_forecast_day(features, d, exog = exog[groups],
                                      params = modifyList(xgb_params, list(max_depth = tun$max_depth)),
                                      rounds = tun$rounds, seed = as.integer(daily$date[d])),
                     error = \(e) NULL))
  bind_rows(lapply(names(f), \(m) if (is.null(f[[m]])) NULL else
    tibble(date = daily$date[d], hour = 0:23, model = m, forecast = f[[m]])))
}

t0 <- Sys.time()
if (.Platform$OS.type == "windows" && cores > 1) {
  # windows has no forking, so start workers
  cl <- makeCluster(cores)
  clusterCall(cl, \(lib, wd) { .libPaths(lib); setwd(wd) }, .libPaths(), getwd())
  clusterEvalQ(cl, {
    library(dplyr); library(tidyr); library(lubridate)
    source("R/lear.R"); source("R/bmt.R"); source("R/exog.R"); source("R/xmodels.R"); source("R/xgb.R")
  })
  clusterExport(cl, c("features", "exog", "groups", "tun", "daily"))
  out <- parLapply(cl, test_days, run_day)
  stopCluster(cl)
} else {
  out <- mclapply(test_days, run_day, mc.cores = cores)
}
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))
fc <- bind_rows(out) |> inner_join(actual, by = c("date", "hour"))
fc |>
  group_by(model) |>
  summarise(mae = mean(abs(actual - forecast)), days = n_distinct(date), .groups = "drop") |>
  print()

saveRDS(fc, "data/processed/forecasts_weather.rds")
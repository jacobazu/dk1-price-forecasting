# the same models on shorter training windows, to average with the two-year fit (hubicka, marcjasz & weron 2019)
# WINDOWS=56,112,365  MODELS=bmtx,learx  GAS=live/gas_live.csv
library(dplyr); library(tidyr); library(lubridate); library(readr)
source("R/lear.R"); source("R/bmt.R"); source("R/exog.R"); source("R/xmodels.R")
source("R/parallel.R"); source("R/inputs.R")
if (requireNamespace("xgboost", quietly = TRUE)) source("R/xgb.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P
exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"), readRDS("data/processed/holidays_dk.rds"))
exog$wx   <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))
exog$wxde <- make_weather(daily$date, read_csv("live/weather_de_live.csv", show_col_types = FALSE), "wxde")
exog$fuel <- make_fuel(daily$date, read_csv(Sys.getenv("GAS", "live/gas_live.csv"), show_col_types = FALSE))

tun <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |> filter(model == "xgbx", chosen)
test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))
if (nzchar(Sys.getenv("NDAYS"))) test_days <- head(test_days, as.integer(Sys.getenv("NDAYS")))
windows  <- as.integer(strsplit(Sys.getenv("WINDOWS", "56,112,365"), ",")[[1]])
variants <- list(base = forecast_groups, fuel = c(forecast_groups, "fuel"))
want <- function(m) !nzchar(Sys.getenv("MODELS")) || m %in% strsplit(Sys.getenv("MODELS"), ",")[[1]]

run_day <- function(d) bind_rows(lapply(windows, \(w) bind_rows(lapply(names(variants), \(v) {
  g <- variants[[v]]
  f <- list(
    bmtx  = if (want("bmtx")) tryCatch(bmtx_forecast_day(features, exog, d, g, window = w), error = \(e) NULL),
    learx = if (want("learx")) tryCatch(learx_forecast_day(features, exog, d, g, window = w), error = \(e) NULL),
    xgbx  = if (want("xgbx") && exists("xgb_forecast_day")) tryCatch(
      xgb_forecast_day(features, d, window = w, exog = exog[g],
                       params = modifyList(xgb_params, list(max_depth = tun$max_depth)),
                       rounds = tun$rounds, seed = as.integer(daily$date[d])), error = \(e) NULL))
  bind_rows(lapply(names(f), \(m) if (is.null(f[[m]])) NULL else
    tibble(date = daily$date[d], hour = 0:23, window = w, variant = v, model = m, forecast = f[[m]])))
}))))

t0 <- Sys.time()
out <- par_lapply(test_days, run_day)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")
actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)), actual = as.vector(t(P)))
saveRDS(bind_rows(out) |> inner_join(actual, by = c("date", "hour")), Sys.getenv("OUT", "data/processed/forecasts_windows.rds"))

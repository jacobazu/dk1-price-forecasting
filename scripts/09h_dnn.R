# neural network on the backtest days, gas included, past data only. one file per day in OUT so it can be stopped
# and started again. compare with scripts/09i_dnn_compare.R
library(dplyr); library(tidyr); library(lubridate); library(readr)
source("R/lear.R"); source("R/bmt.R"); source("R/exog.R"); source("R/xmodels.R"); source("R/dnn.R")
source("R/parallel.R"); source("R/inputs.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"), readRDS("data/processed/holidays_dk.rds"))
exog$wx   <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))
exog$wxde <- make_weather(daily$date, read_csv("live/weather_de_live.csv", show_col_types = FALSE), "wxde")
exog$fuel <- make_fuel(daily$date, read_csv(Sys.getenv("GAS", "live/gas_live.csv"), show_col_types = FALSE))

out <- Sys.getenv("OUT", "data/processed/dnn")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
g <- c(forecast_groups, "fuel")
days <- which(daily$date >= as_date("2025-10-01") & complete.cases(features$P))
days <- days[!file.exists(file.path(out, paste0(daily$date[days], ".rds")))]

run_day <- function(d) {
  f <- tryCatch(dnn_forecast_day(features, exog, d, g), error = \(e) NULL)
  if (!is.null(f)) saveRDS(tibble(date = daily$date[d], hour = 0:23, dnn = f), file.path(out, paste0(daily$date[d], ".rds")))
  NULL
}
invisible(par_lapply(days, run_day))

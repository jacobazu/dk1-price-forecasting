# all forecasts for the evaluation and the battery: the backtest (results/backtest_forecasts.csv,
# written by 10_evaluate.R) plus every live day after it where all models were saved before the
# auction. actual prices from data/processed/prices_dk1_hourly.rds

library(dplyr)
library(tidyr)
library(readr)

all_forecasts <- function(models) {
  daily  <- build_daily(readRDS("data/processed/prices_dk1_hourly.rds"))
  P      <- as.matrix(daily[, -1])
  actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                   actual = as.vector(t(P)))

  backtest <- read_csv("results/backtest_forecasts.csv", show_col_types = FALSE) |>
    select(date, hour, actual, all_of(models)) |>
    mutate(source = "backtest")

  live <- read_csv("forecasts/forecasts_live.csv", show_col_types = FALSE,
                   col_types = cols(forecast_made_utc = col_character(), date = col_date(),
                                    hour = col_integer(), .default = col_double()))
  for (m in setdiff(models, names(live))) live[[m]] <- NA_real_
  live <- live |>
    filter(date > max(backtest$date)) |>
    inner_join(actual, by = c("date", "hour")) |>
    select(date, hour, actual, all_of(models)) |>
    drop_na() |>
    group_by(date) |>
    filter(n() == 24) |>
    ungroup() |>
    mutate(source = "live")

  bind_rows(backtest, live) |>
    arrange(date, hour)
}

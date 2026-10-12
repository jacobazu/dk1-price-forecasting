# -x models with weather and entso-e load forecasts (dk1 and germany), compared with 09b_weather.R
# rule fixed before looking: keep the load forecasts if the ensemble's daily mae is lower from oct 2025
# to feb 2026 (dm p < 0.05). mar to oct 2026 is the check

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/xmodels.R")
source("R/xgb.R")
source("R/parallel.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P

exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"), NULL,
                  readRDS("data/processed/holidays_dk.rds"))
exog$wx   <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))
exog$load <- make_load(daily$date, read_csv("live/load_live.csv", show_col_types = FALSE))

groups <- c("nb", "hol", "wx", "load")
tun    <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |> filter(model == "xgbx", chosen)

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
out <- par_lapply(test_days, run_day)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

actual <- tibble(date = rep(daily$date, each = 24), hour = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))
fc <- bind_rows(out) |> inner_join(actual, by = c("date", "hour"))
saveRDS(fc, "data/processed/forecasts_load.rds")

# with vs without, same hours, ensemble = average of the three
with_ens <- function(x) {
  bind_rows(x, x |> group_by(date, hour, actual) |>
              summarise(forecast = mean(forecast), n = n(), .groups = "drop") |>
              filter(n == 3) |> select(-n) |> mutate(model = "ensemble"))
}
both <- inner_join(with_ens(readRDS("data/processed/forecasts_weather.rds")) |> rename(without = forecast),
                   with_ens(fc) |> rename(with = forecast), by = c("date", "hour", "model", "actual"))
dm <- function(d) { n <- length(d); s <- mean(d) / sqrt(var(d) / n) * sqrt((n - 1) / n); 2 * pt(-abs(s), n - 1) }
both |>
  mutate(period = if_else(date < as_date("2026-03-01"), "1 decide (oct-feb)", "2 check (mar-oct)")) |>
  group_by(period, model, date) |>
  summarise(without = mean(abs(actual - without)), with = mean(abs(actual - with)), .groups = "drop") |>
  group_by(period, model) |>
  summarise(days = n(), p = dm(with - without), mae_without = mean(without), mae_with = mean(with),
            change = sprintf("%+.1f%%", 100 * (mean(with) / mean(without) - 1)), .groups = "drop") |>
  mutate(across(c(mae_without, mae_with), \(v) round(v, 2)), p = signif(p, 2)) |>
  arrange(model, period) |>
  print(n = Inf)

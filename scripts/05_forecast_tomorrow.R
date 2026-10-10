# forecasting tomorrows DK1 prices
# ideally runs before noon danish time, before auction closes


library(readr)
source("R/lear.R")

prices <- readRDS("data/processed/prices_dk1_hourly.rds")

today  <- as_date(with_tz(Sys.time(), "Europe/Copenhagen"))
target <- today + 1

# daily price matrix - empty row or tomorrow if no prices yet
daily <- build_daily(prices) |>
  filter(date <= target) |>
  bind_rows(tibble(date = target)) |>
  distinct(date, .keep_all = TRUE) |>
  complete(date = seq(min(date), target, by = "day")) |>
  arrange(date)

features <- make_features(daily)
d        <- which(daily$date == target)

# forecasts
lear      <- lear_forecast_day(features, d)
naive_day <- features$P[d - 1, ]   # same hour yesterday

new_forecast <- tibble(
  forecast_made_utc = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"),
  date              = target,
  hour              = 0:23,
  lear              = round(lear, 2),
  naive_day         = round(naive_day, 2)
)

# append to live file but never overwrite forecast
dir.create("forecasts", showWarnings = FALSE)
file <- "forecasts/forecasts_live.csv"

if (file.exists(file)) {
  # read the time stamp as text, so it matches the new rows when combined
  old <- read_csv(file, show_col_types = FALSE,
                  col_types = cols(forecast_made_utc = col_character(),
                                   .default = col_guess()))
  if (target %in% as_date(old$date)) {
    message("Forecast for ", target, " already exists - not overwriting.")
  } else {
    write_csv(bind_rows(mutate(old, date = as_date(date)), new_forecast), file)
    message("Forecast for ", target, " added.")
  }
} else {
  write_csv(new_forecast, file)
  message("Forecast file created with forecast for ", target, ".")
}

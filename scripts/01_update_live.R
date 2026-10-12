# daily update, rolling 2 years

library(dplyr)
library(lubridate)
library(readr)
source("R/get_eds.R")

keep_days <- 740                       # 730 + buffer for lags
switch_dt <- as.Date("2025-10-01")     # 15 min prices from here
file_live <- "live/prices_dk1_live.csv"
dir.create("live", showWarnings = FALSE)

# 15 min to hourly
qh_to_hourly <- function(qh) {
  qh |>
    mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
    group_by(time) |>
    summarise(price = mean(DayAheadPriceEUR), n_quarters = n(), .groups = "drop") |>
    filter(n_quarters == 4) |>
    select(time, price)
}

if (file.exists(file_live)) {
  # normal day: only new days
  live <- read_csv(file_live, col_types = cols(time = col_datetime(), price = col_double()))
  from <- as_date(max(live$time)) - 1
} else {
  # first run: 2 years
  start <- Sys.Date() - keep_days
  live  <- tibble(time = as.POSIXct(character(), tz = "UTC"), price = numeric())
  if (start < switch_dt) {
    h    <- get_eds("Elspotprices", format(start), format(switch_dt))
    live <- tibble(time = ymd_hms(h$HourUTC, tz = "UTC"), price = h$SpotPriceEUR)
  }
  from <- max(start, switch_dt)
}

new <- qh_to_hourly(get_eds("DayAheadPrices", format(from), format(Sys.Date() + 2)))

# overwrite old rows, drop oldest
cutoff <- as.POSIXct(Sys.Date() - keep_days, tz = "UTC")
live <- bind_rows(new, live) |>
  distinct(time, .keep_all = TRUE) |>
  filter(time >= cutoff) |>
  arrange(time)

write_csv(live, file_live)

# only overwrite on github
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  saveRDS(live, "data/processed/prices_dk1_hourly.rds")
}

cat("Live prices:", format(min(live$time)), "to", format(max(live$time)),
    "UTC,", nrow(live), "hours\n")


# neighbour prices, may fail
tryCatch({
  areas   <- c("DE", "DK2", "NO2", "SE3", "SE4")
  file_nb <- "live/neighbours_live.csv"
  nb_live <- read_csv(file_nb, col_types = cols(time = col_datetime(), .default = col_double()))
  from_nb <- as_date(max(nb_live$time)) - 1

  nb_new <- get_eds("DayAheadPrices", format(from_nb), format(Sys.Date() + 2), area = areas) |>
    mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
    group_by(time, area = PriceArea) |>
    summarise(price = mean(DayAheadPriceEUR), n = n(), .groups = "drop") |>
    filter(n == 4) |>
    select(-n) |>
    tidyr::pivot_wider(names_from = area, values_from = price, names_prefix = "price_")

  nb_live <- bind_rows(nb_new, nb_live) |>
    distinct(time, .keep_all = TRUE) |>
    filter(time >= cutoff) |>
    arrange(time)

  write_csv(nb_live, file_nb)
  cat("Live neighbour prices:", format(min(nb_live$time)), "to", format(max(nb_live$time)), "UTC\n")
}, error = \(e) message("Neighbour prices not updated: ", conditionMessage(e)))


# open-meteo, made 48h ahead
tryCatch({
  source("R/weather.R")
  file_wx <- "live/weather_live.csv"
  wx_live <- read_csv(file_wx, col_types = cols(time = col_datetime(), .default = col_double()))
  wx_new  <- get_weather(as_date(max(wx_live$time)) - 2, Sys.Date() + 1)

  wx_live <- bind_rows(wx_new, wx_live) |>
    filter(if_all(-time, \(x) !is.na(x))) |>
    distinct(time, .keep_all = TRUE) |>
    filter(time >= cutoff) |>
    arrange(time)

  write_csv(wx_live, file_wx)
  cat("Live weather:", format(min(wx_live$time)), "to", format(max(wx_live$time)), "UTC\n")
}, error = \(e) message("Weather not updated: ", conditionMessage(e)))


# entso-e day-ahead load forecasts, needs ENTSOE_TOKEN (secret on github). kept from 2023 for the backtest
if (nzchar(Sys.getenv("ENTSOE_TOKEN"))) tryCatch({
  source("R/entsoe.R")
  file_ld <- "live/load_live.csv"
  ld_live <- if (file.exists(file_ld)) read_csv(file_ld, col_types = cols(time = col_datetime(), .default = col_double()))
  from_ld <- if (is.null(ld_live)) as_date("2023-01-01") else as_date(max(ld_live$time)) - 2
  ld_new  <- full_join(get_entsoe_load("DK1", from_ld, Sys.Date() + 1),
                       get_entsoe_load("DE", from_ld, Sys.Date() + 1), by = "time")

  ld_live <- bind_rows(ld_new, ld_live) |>
    distinct(time, .keep_all = TRUE) |>
    arrange(time)

  write_csv(ld_live, file_ld)
  cat("Live load forecasts:", format(min(ld_live$time)), "to", format(max(ld_live$time)), "UTC\n")
}, error = \(e) message("Load forecasts not updated: ", conditionMessage(e)))

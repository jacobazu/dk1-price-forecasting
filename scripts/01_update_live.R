# updating the live price file on github every day
# adds the newest days and removes days older than 2 years
# full history for backtests still comes from 01_download.R and 02_clean.R

library(dplyr)
library(lubridate)
library(readr)
source("R/get_eds.R")

keep_days <- 740                       # 730 days for training + buffer for D-7 lag
switch_dt <- as.Date("2025-10-01")     # switch from hourly to 15 min prices
file_live <- "live/prices_dk1_live.csv"
dir.create("live", showWarnings = FALSE)

# 15 min prices to hourly average, same as in 02_clean.R
qh_to_hourly <- function(qh) {
  qh |>
    mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
    group_by(time) |>
    summarise(price = mean(DayAheadPriceEUR), n_quarters = n(), .groups = "drop") |>
    filter(n_quarters == 4) |>
    select(time, price)
}

if (file.exists(file_live)) {
  # normal day, only download from last saved day
  live <- read_csv(file_live, col_types = cols(time = col_datetime(), price = col_double()))
  from <- as_date(max(live$time)) - 1
} else {
  # first run, download 2 years once
  start <- Sys.Date() - keep_days
  live  <- tibble(time = as.POSIXct(character(), tz = "UTC"), price = numeric())
  if (start < switch_dt) {
    h    <- get_eds("Elspotprices", format(start), format(switch_dt))
    live <- tibble(time = ymd_hms(h$HourUTC, tz = "UTC"), price = h$SpotPriceEUR)
  }
  from <- max(start, switch_dt)
}

new <- qh_to_hourly(get_eds("DayAheadPrices", format(from), format(Sys.Date() + 2)))

# new rows overwrite old ones, then remove the oldest days
cutoff <- as.POSIXct(Sys.Date() - keep_days, tz = "UTC")
live <- bind_rows(new, live) |>
  distinct(time, .keep_all = TRUE) |>
  filter(time >= cutoff) |>
  arrange(time)

write_csv(live, file_live)

# forecast and dashboard read this rds. only overwrite on github
# so the full history on my pc is kept
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  saveRDS(live, "data/processed/prices_dk1_hourly.rds")
}

cat("Live prices:", format(min(live$time)), "to", format(max(live$time)),
    "UTC,", nrow(live), "hours\n")


# neighbour prices for BMT-X / LEAR-X / XGB-X, same rolling window
# (the file was started from 07_download_exog.R, after that only the newest days are added)
# if this fails the dk1 prices above are still saved, the -X models just get no forecast
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


# weather forecasts made 48 hours ahead (open-meteo, see 07b_download_weather.R), same rolling window
# the values for tomorrow were forecast on the day before yesterday, so they are known now
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

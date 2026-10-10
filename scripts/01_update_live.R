# daily update of the live price file used by GitHub Actions:
# add the newest days, drop days older than the 2-year window.
# the full 2019+ history for backtests still comes from 01_download.R + 02_clean.R

library(dplyr)
library(lubridate)
library(readr)
source("R/get_eds.R")

keep_days <- 740                       # 730 training days + buffer for the D-7 lags
switch_dt <- as.Date("2025-10-01")     # hourly prices end, 15-minute prices start
file_live <- "live/prices_dk1_live.csv"
dir.create("live", showWarnings = FALSE)

# 15-minute prices turned into hourly averages (same as 02_clean.R)
qh_to_hourly <- function(qh) {
  qh |>
    mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
    group_by(time) |>
    summarise(price = mean(DayAheadPriceEUR), n_quarters = n(), .groups = "drop") |>
    filter(n_quarters == 4) |>
    select(time, price)
}

if (file.exists(file_live)) {
  # normal day: only download from the last saved day onwards
  live <- read_csv(file_live, col_types = cols(time = col_datetime(), price = col_double()))
  from <- as_date(max(live$time)) - 1
} else {
  # first run: download the 2-year window once
  start <- Sys.Date() - keep_days
  live  <- tibble(time = as.POSIXct(character(), tz = "UTC"), price = numeric())
  if (start < switch_dt) {
    h    <- get_eds("Elspotprices", format(start), format(switch_dt))
    live <- tibble(time = ymd_hms(h$HourUTC, tz = "UTC"), price = h$SpotPriceEUR)
  }
  from <- max(start, switch_dt)
}

new <- qh_to_hourly(get_eds("DayAheadPrices", format(from), format(Sys.Date() + 2)))

# new rows replace overlapping old ones, then drop the oldest days
cutoff <- as.POSIXct(Sys.Date() - keep_days, tz = "UTC")
live <- bind_rows(new, live) |>
  distinct(time, .keep_all = TRUE) |>
  filter(time >= cutoff) |>
  arrange(time)

write_csv(live, file_live)

# the forecast script and dashboard read this rds; only overwrite it on GitHub,
# so the full 2019+ history on your PC is kept for backtests
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
  saveRDS(live, "data/processed/prices_dk1_hourly.rds")
}

cat("Live prices:", format(min(live$time)), "to", format(max(live$time)),
    "UTC,", nrow(live), "hours\n")

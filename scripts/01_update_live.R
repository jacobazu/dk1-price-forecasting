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

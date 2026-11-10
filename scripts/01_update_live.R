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


# open-meteo for germany, made 48h ahead, kept from march 2024
tryCatch({
  source("R/weather.R")
  file_wd <- "live/weather_de_live.csv"
  wd_live <- if (file.exists(file_wd)) read_csv(file_wd, col_types = cols(time = col_datetime(), .default = col_double()))
  from_wd <- if (is.null(wd_live)) as_date("2024-03-01") else as_date(max(wd_live$time)) - 2
  wd_new  <- get_weather(from_wd, Sys.Date() + 1, sites_de)

  wd_live <- bind_rows(wd_new, wd_live) |>
    filter(if_all(-time, \(x) !is.na(x))) |>
    distinct(time, .keep_all = TRUE) |>
    arrange(time)

  write_csv(wd_live, file_wd)
  cat("Live german weather:", format(min(wd_live$time)), "to", format(max(wd_live$time)), "UTC\n")
}, error = \(e) message("German weather not updated: ", conditionMessage(e)))


# flows on dk1's six cables, hourly means of energinet's minute data, + = into dk1. for the neighbours page
tryCatch({
  file_fl <- "live/flows_live.csv"
  fl_old <- read_csv(file_fl, col_types = cols(time = col_datetime(), .default = col_double()))
  cab <- c(DE = "Exchange_DK1_DE", NL = "Exchange_DK1_NL", GB = "Exchange_DK1_GB", NO = "Exchange_DK1_NO",
           SE = "Exchange_DK1_SE", DK2 = "Exchange_DK1_DK2")
  r <- request("https://api.energidataservice.dk/dataset/PowerSystemRightNow") |>
    req_url_query(start = format(max(fl_old$time) - hours(2), "%Y-%m-%dT%H:%M", tz = "UTC"), limit = 0,
                  columns = paste(c("Minutes1UTC", cab), collapse = ",")) |>
    req_retry(max_tries = 4, retry_on_failure = TRUE) |>
    req_perform() |> resp_body_json(simplifyVector = TRUE)
  fl_new <- as_tibble(r$records) |>
    mutate(time = floor_date(ymd_hms(Minutes1UTC, tz = "UTC"), "hour")) |>
    group_by(time) |>
    summarise(across(all_of(unname(cab)), \(x) round(mean(x, na.rm = TRUE))), n = n(), .groups = "drop") |>
    filter(n >= 50) |> select(-n) |>            # whole hours only
    rename(all_of(cab))
  fl_live <- bind_rows(fl_new, fl_old) |> distinct(time, .keep_all = TRUE) |>
    filter(time >= Sys.time() - days(60)) |> arrange(time)
  write_csv(fl_live, file_fl)
  cat("Flows:", format(min(fl_live$time)), "to", format(max(fl_live$time)), "UTC\n")
}, error = \(e) message("Flows not updated: ", conditionMessage(e)))


# daily gas price (EEX spot index, gas day), for the gas challenger
tryCatch({
  file_gas <- "live/gas_live.csv"
  gas_old <- read_csv(file_gas, col_types = cols(day = col_date(), gas = col_double()))
  r <- request("https://api.energidataservice.dk/dataset/GasDailyBalancingPrice") |>
    req_url_query(start = format(max(gas_old$day) - 7), limit = 0) |>
    req_retry(max_tries = 4, retry_on_failure = TRUE) |>
    req_perform() |> resp_body_json(simplifyVector = TRUE)
  gas_new <- tibble(day = as_date(substr(r$records$GasDay, 1, 10)), gas = round(r$records$EEXSpotIndexEUR_MWh, 2))
  gas_live <- bind_rows(gas_new, gas_old) |> filter(!is.na(gas)) |> distinct(day, .keep_all = TRUE) |> arrange(day)
  write_csv(gas_live, file_gas)
  cat("Gas prices:", format(min(gas_live$day)), "to", format(max(gas_live$day)), "\n")
}, error = \(e) message("Gas prices not updated: ", conditionMessage(e)))


# timing check for entso-e wind/solar forecasts, not a model input
if (nzchar(Sys.getenv("ENTSOE_TOKEN"))) tryCatch(source("scripts/check_entsoe_timing.R", local = new.env()),
                                                 error = \(e) message("ENTSO-E timing check failed: ", conditionMessage(e)))

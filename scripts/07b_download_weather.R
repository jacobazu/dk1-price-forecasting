# weather forecasts for DK1 from open-meteo, as they looked before the auction
# previous runs api: "_previous_day2" = the value forecast 48 hours before that hour, so for every
# hour of delivery day D it was made on D-2 at the latest, before the noon auction on D-1
# (the energi data service wind/solar forecasts are published at 18:00 on D-1, too late)
# wind speed at 100 m at a few wind areas, solar radiation at a few solar areas
# the archive starts in january 2024
# saves data/raw/weather_openmeteo.rds (one row per hour, utc)

library(httr2)
library(dplyr)
library(tidyr)
library(lubridate)

sites <- tribble(
  ~site,          ~lat,  ~lon,  ~type,
  "horns_rev",   55.53, 7.91,  "wind",    # offshore, north sea
  "west_jutland",56.10, 8.30,  "wind",    # onshore, west coast
  "north_jutland",57.05, 9.30, "wind",
  "south_jutland",55.20, 9.20, "solar",
  "mid_jutland", 56.20, 9.50,  "solar",
  "funen",       55.30, 10.30, "solar"
)
api_var <- c(wind = "wind_speed_100m_previous_day2", solar = "shortwave_radiation_previous_day2")

start <- as_date("2024-01-01")
end   <- Sys.Date() + 1

# one request per site and quarter, so the answers stay small
get_site <- function(site, lat, lon, type, from, to) {
  var <- api_var[[type]]
  resp <- request("https://previous-runs-api.open-meteo.com/v1/forecast") |>
    req_url_query(latitude = lat, longitude = lon, hourly = var,
                  start_date = format(from), end_date = format(to), timezone = "GMT") |>
    req_retry(max_tries = 5, backoff = \(i) 20) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  tibble(time  = ymd_hm(resp$hourly$time, tz = "UTC"),
         name  = paste0(type, "_", site),
         value = as.numeric(resp$hourly[[var]]))
}

periods <- tibble(from = seq(start, end, by = "quarter")) |>
  mutate(to = pmin(lead(from, default = end + 1) - 1, end))

jobs <- crossing(sites, periods)
weather <- lapply(seq_len(nrow(jobs)), \(i) {
  j <- jobs[i, ]
  message(j$site, " ", j$from)
  Sys.sleep(0.5)
  get_site(j$site, j$lat, j$lon, j$type, j$from, j$to)
}) |>
  bind_rows() |>
  pivot_wider(names_from = name, values_from = value) |>
  arrange(time)

# coverage: first hour with data and share missing after that, per column
print(bind_rows(lapply(setdiff(names(weather), "time"), \(v) {
  x <- weather[[v]]
  first <- which(!is.na(x))[1]
  tibble(variable = v, from = weather$time[first],
         missing_pct = round(100 * mean(is.na(x[first:length(x)])), 2))
})))

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
saveRDS(weather, "data/raw/weather_openmeteo.rds")
cat("saved", nrow(weather), "hours from", format(min(weather$time)), "to", format(max(weather$time)), "\n")

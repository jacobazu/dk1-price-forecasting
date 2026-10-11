# weather forecasts for DK1 from open-meteo, as they looked before the auction
# previous runs api: "_previous_day2" = the value forecast 48 hours before that hour, so for every
# hour of delivery day D it was made on D-2 at the latest, before the noon auction on D-1
# (the energi data service wind/solar forecasts are published at 18:00 on D-1, too late)
# wind speed at 100 m at a few wind areas, solar radiation at a few solar areas
# the archive starts in january 2024
# saves data/raw/weather_openmeteo.rds (one row per hour, utc)

source("R/weather.R")   # sites and get_weather()

weather <- get_weather(as_date("2024-01-01"), Sys.Date() + 1)

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

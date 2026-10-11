# downloading exogenous variables for BMT-X
# neighbour prices (DE, DK2, NO2, SE3, SE4), DK1 wind and solar day-ahead forecasts, danish holidays
# only ForecastDayAhead is used, known before the noon auction

library(dplyr)
library(tidyr)
library(lubridate)
source("R/get_eds.R")

start     <- "2019-01-01"
switch_dt <- "2025-10-01"
end       <- format(Sys.Date() + 2)
areas     <- c("DE", "DK2", "NO2", "SE3", "SE4")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# empty tibble instead of an error when a dataset has nothing for an area
fetch <- function(dataset, from, to, area) {
  r <- tryCatch(get_eds(dataset, from, to, area = area), error = \(e) {
    message(dataset, " ", area, ": ", conditionMessage(e)); NULL
  })
  if (is.null(r) || NROW(r) == 0) tibble() else as_tibble(r)
}


# neighbour prices, hourly until 30 sep 2025, 15 min after (averaged to hourly)
nb_old <- bind_rows(lapply(areas, \(a) fetch("Elspotprices", start, switch_dt, a)))
nb_new <- bind_rows(lapply(areas, \(a) fetch("DayAheadPrices", switch_dt, end, a)))

if (nrow(nb_old) > 0) {
  nb_old <- nb_old |>
    transmute(time = ymd_hms(HourUTC, tz = "UTC"), area = PriceArea, price = SpotPriceEUR)
}
if (nrow(nb_new) > 0) {
  nb_new <- nb_new |>
    mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
    group_by(time, area = PriceArea) |>
    summarise(price = mean(DayAheadPriceEUR), n = n(), .groups = "drop") |>
    filter(n == 4) |>
    select(-n)
}

neighbours <- bind_rows(nb_old, nb_new) |>
  filter(!is.na(price)) |>
  distinct(time, area, .keep_all = TRUE) |>
  pivot_wider(names_from = area, values_from = price, names_prefix = "price_") |>
  arrange(time)


# dk1 wind and solar, day-ahead forecasts
wind_solar <- fetch("Forecasts_Hour", start, end, "DK1")

if (nrow(wind_solar) > 0) {
  wind_solar <- wind_solar |>
    transmute(time  = floor_date(ymd_hms(HourUTC, tz = "UTC"), "hour"),
              type  = tolower(gsub(" ", "_", ForecastType)),
              value = ForecastDayAhead) |>
    group_by(time, type) |>
    summarise(value = mean(value), .groups = "drop") |>
    pivot_wider(names_from = type, values_from = value, names_prefix = "fc_") |>
    arrange(time)
}


# danish holidays (easter from the gregorian algorithm)
easter <- function(y) {
  a <- y %% 19; b <- y %/% 100; c <- y %% 100
  d <- b %/% 4; e <- b %% 4; f <- (b + 8) %/% 25; g <- (b - f + 1) %/% 3
  h <- (19 * a + b - d - g + 15) %% 30
  i <- c %/% 4; k <- c %% 4
  l <- (32 + 2 * e + 2 * i - h - k) %% 7
  m <- (a + 11 * h + 22 * l) %/% 451
  month <- (h + l - 7 * m + 114) %/% 31
  day   <- ((h + l - 7 * m + 114) %% 31) + 1
  make_date(y, month, day)
}

dk_holidays <- function(years) {
  bind_rows(lapply(years, \(y) {
    e <- easter(y)
    tibble(date = c(make_date(y, 1, 1), e - 3, e - 2, e, e + 1,
                    if (y < 2024) e + 26,             # store bededag, abolished from 2024
                    e + 39, e + 49, e + 50,
                    make_date(y, 6, 5), make_date(y, 12, 24), make_date(y, 12, 25),
                    make_date(y, 12, 26), make_date(y, 12, 31)))
  })) |>
    distinct() |>
    arrange(date)
}

holidays <- dk_holidays(year(as_date(start)):(year(Sys.Date()) + 1))


# coverage: when each variable starts and how much is missing after that
coverage <- function(df) {
  if (nrow(df) == 0) return(tibble())
  all_hours <- tibble(time = seq(min(df$time), max(df$time), by = "hour"))
  full <- left_join(all_hours, df, by = "time")
  bind_rows(lapply(setdiff(names(df), "time"), \(v) {
    x <- full[[v]]
    first <- which(!is.na(x))[1]
    tibble(variable    = v,
           from        = full$time[first],
           to          = full$time[max(which(!is.na(x)))],
           missing_pct = round(100 * mean(is.na(x[first:length(x)])), 2))
  }))
}

print(bind_rows(coverage(neighbours), coverage(wind_solar)), n = Inf)
cat("holidays:", nrow(holidays), "days from", format(min(holidays$date)), "to", format(max(holidays$date)), "\n")

saveRDS(neighbours, "data/raw/exog_neighbours.rds")
saveRDS(wind_solar, "data/raw/exog_wind_solar.rds")
saveRDS(holidays, "data/processed/holidays_dk.rds")

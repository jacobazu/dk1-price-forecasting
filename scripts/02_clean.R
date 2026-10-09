# scripts/02_clean.R
# combining hourly and 15 minute DK1 prices into one hourly series

library(dplyr)
library(lubridate)
library(ggplot2)

hourly <- readRDS("data/raw/elspot_hourly.rds")
qh     <- readRDS("data/raw/dayahead_15min.rds")

# old hourly prices until 30 september 2025
hourly_clean <- hourly |>
  transmute(
    time  = ymd_hms(HourUTC, tz = "UTC"),
    price = SpotPriceEUR
  )

# new 15 minute prices turned into hourly average, from 1 october 2025
qh_hourly <- qh |>
  mutate(time = floor_date(ymd_hms(TimeUTC, tz = "UTC"), "hour")) |>
  group_by(time) |>
  summarise(price = mean(DayAheadPriceEUR), n_quarters = n(), .groups = "drop") |>
  filter(n_quarters == 4) |>          
  select(time, price)

# stitch it together
prices <- bind_rows(hourly_clean, qh_hourly) |>
  arrange(time)

# sanity check if data set has faults or NA
all_hours <- tibble(time = seq(min(prices$time), max(prices$time), by = "hour"))

cat("Rows:             ", nrow(prices), "\n")
cat("From:             ", format(min(prices$time)), "UTC\n")
cat("To:               ", format(max(prices$time)), "UTC\n")
cat("Duplicate hours:  ", sum(duplicated(prices$time)), "\n")
cat("Missing hours:    ", nrow(anti_join(all_hours, prices, by = "time")), "\n")
cat("Missing prices:   ", sum(is.na(prices$price)), "\n")
cat("Negative prices:  ", sum(prices$price < 0, na.rm = TRUE), "hours\n")

# simple daily average plot
prices |>
  group_by(date = as_date(time)) |>
  summarise(price = mean(price)) |>
  ggplot(aes(date, price)) +
  geom_line(linewidth = 0.3) +
  geom_vline(xintercept = as_date("2025-10-01"), linetype = "dashed", colour = "grey50") +
  labs(title = "DK1 day-ahead price, daily average",
       subtitle = "Dashed line: observation time switch",
       x = NULL, y = "EUR/MWh") +
  theme_minimal()

# save the processed dataset
saveRDS(prices, "data/processed/prices_dk1_hourly.rds")

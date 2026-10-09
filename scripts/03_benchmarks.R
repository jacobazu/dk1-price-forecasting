# scripts/03_benchmarks.R
# making a naive benchmark for the prices: “tomorrow will look like a recent day we’ve already seen” 
#- copies an old price and uses it as the forecast.


# forecasting setup: on day D-1 (before the noon auction) we forecast all 24 hours
# of day D. At that time, the prices for day D-1 are already known, so using
# "same hour yesterday" is a fair forecast.

library(dplyr)
library(lubridate)
library(ggplot2)

prices <- readRDS("data/processed/prices_dk1_hourly.rds")

# lagging the prices
prices <- prices |>
  mutate(
    lag_24  = price[match(time - hours(24),  time)],   # same hour yesterday
    lag_168 = price[match(time - hours(168), time)],   # same hour last week
    weekday = wday(with_tz(time, "Europe/Copenhagen"), week_start = 1)  # 1 = Monday
  )

# benchmark forecast
forecasts <- prices |>
  mutate(
    naive_day  = lag_24,
    naive_week = lag_168,
    # naive mix: tues-fri uses yesterday as forecast
    # monday, sat and sun use the same day last week.
    naive_mix  = if_else(weekday %in% c(1, 6, 7), lag_168, lag_24)
  ) |>
  select(time, price, naive_day, naive_week, naive_mix)

# test period: the 15-minute market era (2025-10-01 onwards)
test <- forecasts |>
  filter(time >= as_datetime("2025-10-01", tz = "UTC")) |>
  filter(!is.na(price))

# accuracy (EUR/MWh) MAE and RSME
results <- tibble(
  model = c("naive_day", "naive_week", "naive_mix"),
  MAE   = sapply(model, \(m) mean(abs(test$price - test[[m]]), na.rm = TRUE)),
  RMSE  = sapply(model, \(m) sqrt(mean((test$price - test[[m]])^2, na.rm = TRUE)))
) |>
  arrange(MAE)

print(results)

# plot one example week
week_start <- max(test$time) - days(7)

test |>
  filter(time >= week_start) |>
  ggplot(aes(time)) +
  geom_line(aes(y = price,     colour = "Actual"),    linewidth = 0.6) +
  geom_line(aes(y = naive_mix, colour = "Naive mix"), linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = naive_day, colour = "Naive day"), linewidth = 0.4, linetype = "dashed") +
  geom_line(aes(y = naive_week, colour = "Naive week"), linewidth = 0.4, linetype = "dashed") +
  scale_colour_manual(values = c("Actual" = "black", "Naive mix" = "red", "Naive day" = "blue", "Naive week" = "darkgreen")) +
  labs(title = "DK1 price vs naive forecast, last 7 days",
       x = NULL, y = "EUR/MWh", colour = NULL) +
  theme_minimal()

# saving forecasts for later comparison with real models
saveRDS(forecasts, "data/processed/forecasts_benchmarks.rds")


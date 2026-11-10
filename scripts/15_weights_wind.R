# two tests on the backtest days, past data only
# 1. weights: each model weighted by its error over the last 28 days, against the plain average
# 2. wind: does our offshore wind forecast explain what the ensemble misses? rolling 60-day fit of the error
#    on the forecast share of the fleet, against the same fit with no wind (a plain bias correction)
# FC: the three models with gas (scripts/09f_fuel.R, ONLY=gas). WIND: final_forecasts.csv from the renewables repo
library(dplyr); library(tidyr); library(readr); library(lubridate)
source("R/tests.R")

fc <- readRDS(Sys.getenv("FC", "data/processed/forecasts_gas.rds")) |> filter(variant == "fuel") |>
  select(date, hour, model, forecast, actual) |> pivot_wider(names_from = model, values_from = forecast) |>
  filter(complete.cases(pick(everything()))) |> arrange(date, hour) |>
  mutate(ens = (bmtx + learx + xgbx) / 3)
ms <- c("bmtx", "learx", "xgbx")
D  <- sort(unique(fc$date))

# 1. weights
dmae <- fc |> group_by(date) |> summarise(across(all_of(ms), \(f) mean(abs(actual - f))), .groups = "drop")
w <- bind_rows(lapply(D, \(d) {
  past <- filter(dmae, date < d, date >= d - 28)
  if (nrow(past) < 20) return(NULL)
  m <- colMeans(past[ms]); iw <- (1 / m) / sum(1 / m)
  tibble(date = d, model = ms, inv = iw, best = as.numeric(ms == names(which.min(m))))
}))
wide <- function(col) w |> select(date, model, v = all_of(col)) |> pivot_wider(names_from = model, values_from = v, names_prefix = "w_")
fw <- fc |> inner_join(wide("inv"), by = "date") |> mutate(weighted = w_bmtx * bmtx + w_learx * learx + w_xgbx * xgbx) |>
  select(date, hour, actual, ens, weighted) |>
  inner_join(fc |> inner_join(wide("best"), by = "date") |> transmute(date, hour, best = w_bmtx * bmtx + w_learx * learx + w_xgbx * xgbx),
             by = c("date", "hour"))
d1 <- fw |> group_by(date) |> summarise(across(c(ens, weighted, best), \(f) mean(abs(actual - f))), .groups = "drop")
res1 <- tibble(test = "weights", forecast = c("plain average", "weighted by last 28 days", "best model of last 28 days"),
               days = nrow(d1), mae = c(mean(d1$ens), mean(d1$weighted), mean(d1$best)),
               p_vs_average = c(NA, dm_test(d1$weighted - d1$ens)[["p"]], dm_test(d1$best - d1$ens)[["p"]]))

# 2. wind
wind <- read_csv(Sys.getenv("WIND"), show_col_types = FALSE) |> filter(target == "offshore") |>
  mutate(local = with_tz(as_datetime(time, tz = "UTC"), "Europe/Copenhagen"),   # time is the start of the hour, utc
         date = as_date(local), hour = hour(local), share = forecast / cap) |>
  group_by(date, hour) |> summarise(share = mean(share), .groups = "drop")
x <- fc |> inner_join(wind, by = c("date", "hour")) |> mutate(e = actual - ens)
Dw <- sort(unique(x$date))
fit <- bind_rows(lapply(Dw, \(d) {
  tr <- filter(x, date < d, date >= d - 60); te <- filter(x, date == d)
  if (n_distinct(tr$date) < 40) return(NULL)
  b <- coef(lm(e ~ share, tr))
  te |> transmute(date, hour, actual, ens, bias = ens + mean(tr$e), wind = ens + b[1] + b[2] * share)
}))
d2 <- fit |> group_by(date) |> summarise(across(c(ens, bias, wind), \(f) mean(abs(actual - f))), .groups = "drop")
res2 <- tibble(test = "wind", forecast = c("ensemble with gas", "plus 60-day bias correction", "plus correction on our wind forecast"),
               days = nrow(d2), mae = c(mean(d2$ens), mean(d2$bias), mean(d2$wind)),
               p_vs_average = c(NA, dm_test(d2$bias - d2$ens)[["p"]], dm_test(d2$wind - d2$bias)[["p"]]))

res <- bind_rows(res1, res2)
print(as.data.frame(res), digits = 3)
write_csv(res, "results/weights_wind.csv")

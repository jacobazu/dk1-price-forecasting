# ranges from filtered historical simulation: garch(1,1) on the daily size of the errors, past errors divided by
# their own volatility, scaled back up with tomorrow's. against today's ranges, same days, same information
# result in results/garch_ranges.csv

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
source("R/forecasts.R")
source("R/quantiles.R")
source("R/tests.R")

actual <- readRDS("data/processed/prices_dk1_hourly.rds") |>
  mutate(local = with_tz(time, "Europe/Copenhagen"), date = as_date(local), hour = hour(local)) |>
  group_by(date, hour) |> summarise(actual = mean(price), .groups = "drop")
bt <- read_csv("results/backtest_forecasts.csv", show_col_types = FALSE) |> add_ensembles() |>
  select(date, hour, ensemble)
lv <- read_csv("forecasts/forecasts_live.csv", show_col_types = FALSE,
               col_types = cols(forecast_made_utc = col_character(), date = col_date(), hour = col_integer(),
                                .default = col_double())) |> select(date, hour, ensemble)
fc <- bind_rows(bt |> filter(!date %in% lv$date) |> mutate(source = "backtest"), lv |> mutate(source = "live")) |>
  filter(!is.na(ensemble)) |> inner_join(actual, by = c("date", "hour")) |> mutate(e = actual - ensemble)

em <- err_matrix(fc)
D  <- em$dates
r2 <- rowMeans(em$E^2)   # squared size of each day's errors

# garch(1,1) on r2, gaussian likelihood of the 24 hourly errors given the day's variance
garch_path <- function(p, x) {
  s2 <- numeric(length(x)); s2[1] <- mean(x, na.rm = TRUE)
  for (i in 2:length(x)) s2[i] <- p[1] + p[2] * ifelse(is.na(x[i - 1]), s2[i - 1], x[i - 1]) + p[3] * s2[i - 1]
  s2
}
garch_fit <- function(x) {
  nll <- function(q) {
    p <- c(exp(q[1]), plogis(q[2]) * plogis(q[3]), (1 - plogis(q[2])) * plogis(q[3]))  # alpha + beta < 1
    s2 <- garch_path(p, x)
    ok <- !is.na(x)
    sum(24 * log(s2[ok]) + 24 * x[ok] / s2[ok]) / 2
  }
  q <- optim(c(log(mean(x, na.rm = TRUE) * 0.05), 0, 2), nll)$par
  c(exp(q[1]), plogis(q[2]) * plogis(q[3]), (1 - plogis(q[2])) * plogis(q[3]))
}

# sigma for day i from days before i only; refit every 30 days on the last two years
sig <- rep(NA_real_, length(D)); pars <- NULL; last_fit <- as.Date(NA)
first <- which(D >= as_date("2025-12-01"))[1]   # two months of errors before the first scored day
for (i in first:length(D)) {
  past <- which(D < D[i] & D >= D[i] - 730)
  if (is.na(last_fit) || D[i] - last_fit >= 30) { pars <- garch_fit(r2[past]); last_fit <- D[i] }
  s2 <- garch_path(pars, c(r2[past], NA))
  sig[i] <- sqrt(tail(s2, 1))
}
# sigma for the past days, as known then, to standardize them
sig_in <- sqrt(garch_path(garch_fit(r2[D < D[first]]), r2))

fhs <- function(k) bind_rows(lapply(first:length(D), \(i) {
  past <- D < D[i] & D >= D[i] - k & complete.cases(em$E)
  if (sum(past) < 0.8 * min(k, 60) || is.na(sig[i])) return(NULL)
  Z <- em$E[past, , drop = FALSE] / sig_in[past]
  off <- apply(apply(Z, 2, quantile, probs = taus, names = FALSE), 2, sort) * sig[i]
  tibble(date = D[i], hour = rep(0:23, each = length(taus)), tau = rep(taus, 24), off = as.vector(off))
}))

scale <- vol_scale(actual, D)
today <- quantile_offsets(D, em$E, scale$s[match(D, scale$date)]) |> filter(method == "emp_s") |> select(-method)
ranges <- bind_rows(today |> mutate(method = "today"), fhs(60) |> mutate(method = "garch_60"),
                    fhs(365) |> mutate(method = "garch_365")) |>
  inner_join(fc |> select(date, hour, source, actual, ensemble), by = c("date", "hour")) |>
  mutate(q = ensemble + off, loss = pinball(actual, q, tau))

# same days for every method
common <- ranges |> distinct(method, date) |> count(date) |> filter(n == 3) |> pull(date)
ranges <- filter(ranges, date %in% common)
day <- ranges |>
  filter(tau %in% c(0.05, 0.95)) |> select(method, date, hour, source, actual, tau, q) |>
  pivot_wider(names_from = tau, values_from = q, names_prefix = "q") |>
  group_by(method, date, source) |>
  summarise(in90 = mean(actual >= q0.05 & actual <= q0.95), w90 = mean(q0.95 - q0.05),
            out_day = any(actual < q0.05 | actual > q0.95) * 1, .groups = "drop") |>
  inner_join(ranges |> group_by(method, date) |> summarise(pinball = mean(loss), .groups = "drop"),
             by = c("method", "date"))
res <- day |> group_by(method, source) |>
  summarise(days = n(), coverage90 = mean(in90), width90 = mean(w90), pinball = mean(pinball), .groups = "drop")
base <- day |> filter(method == "today") |> select(date, p0 = pinball)
res <- res |> left_join(day |> inner_join(base, by = "date") |> group_by(method, source) |>
                          summarise(p_vs_today = if (all(pinball == p0)) NA_real_ else dm_test(pinball - p0)[["p"]],
                                    .groups = "drop"), by = c("method", "source"))
print(as.data.frame(res), digits = 3)
write_csv(res, "results/garch_ranges.csv")
saveRDS(list(day = day, sig = tibble(date = D, sig = sig, r = sqrt(r2))), "data/processed/garch_ranges.rds")

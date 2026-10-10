# economic evaluation of the forecasts with a battery
# each day the battery plans tomorrow using the forecast, buy cheap and sell expensive
# the plan is then paid with the actual prices
# perfect foresight = knowing the actual prices, used as upper bound
# needs forecasts_lear.rds from 04_lear.R

library(dplyr)
library(tidyr)
library(lubridate)
library(lpSolve)     # install.packages("lpSolve") first time
library(ggplot2)

# battery
cap_mwh   <- 2       # storage, MWh
power_mw  <- 1       # max charge/discharge per hour, MW
eff_rt    <- 0.90    # round trip efficiency
max_cycle <- 1       # full cycles per day
eff       <- sqrt(eff_rt)   # loss split on charge and discharge

# lear forecasts from the backtest, danish time
fc <- readRDS("data/processed/forecasts_lear.rds")

# naive forecast, same hour yesterday
fc <- fc |>
  left_join(fc |> transmute(date = date + 1, hour, naive_day = actual),
            by = c("date", "hour")) |>
  filter(!is.na(naive_day)) |>
  group_by(date) |>
  filter(n() == 24) |>          # only full days
  ungroup() |>
  arrange(date, hour)


# linear program for one day. constraints are the same every day, only prices change
# variables: charge c1..c24, discharge d1..d24, state of charge s1..s24
n  <- 24
ic <- 1:n
id <- n + 1:n
soc <- 2 * n + 1:n

A <- matrix(0, 0, 3 * n); dirs <- c(); rhs <- c()
new_row <- function() numeric(3 * n)

# state of charge s_h = s_(h-1) + eff*c_h - d_h/eff, starts empty
for (h in 1:n) {
  r <- new_row()
  r[soc[h]] <- 1
  if (h > 1) r[soc[h - 1]] <- -1
  r[ic[h]] <- -eff
  r[id[h]] <- 1 / eff
  A <- rbind(A, r); dirs <- c(dirs, "="); rhs <- c(rhs, 0)
}

# empty at end of day
r <- new_row(); r[soc[n]] <- 1
A <- rbind(A, r); dirs <- c(dirs, "="); rhs <- c(rhs, 0)

# storage and power limits
for (h in 1:n) {
  r <- new_row(); r[soc[h]] <- 1
  A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, cap_mwh)
  r <- new_row(); r[ic[h]] <- 1
  A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, power_mw)
  r <- new_row(); r[id[h]] <- 1
  A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, power_mw)
}

# cycle limit
r <- new_row(); r[id] <- 1
A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, cap_mwh * max_cycle)

# optimal plan for given prices, returns MWh sold per hour (negative = bought)
schedule <- function(price) {
  obj <- c(-price, price, rep(0, n))   # pay for charging, earn from selling
  sol <- lp("max", obj, A, dirs, rhs)
  if (sol$status != 0) stop("lp did not solve")
  sol$solution[id] - sol$solution[ic]
}


# plan with each forecast, paid with actual prices
strategies <- c(perfect = "actual", lear = "lear", naive_day = "naive_day", naive_mix = "naive_mix")
days <- unique(fc$date)

profit <- lapply(days, \(dd) {
  x <- fc[fc$date == dd, ]
  sapply(strategies, \(col) sum(x$actual * schedule(x[[col]])))
})
profit <- bind_cols(tibble(date = days), as_tibble(do.call(rbind, profit)))


# summary
years   <- nrow(profit) / 365
perfect <- sum(profit$perfect)

summary_tbl <- profit |>
  pivot_longer(-date, names_to = "strategy", values_to = "profit") |>
  group_by(strategy) |>
  summarise(total_eur        = round(sum(profit)),
            eur_per_mw_year  = round(sum(profit) / power_mw / years),
            share_of_perfect = round(sum(profit) / perfect, 3)) |>
  arrange(desc(total_eur))

print(summary_tbl)


# test if lear earns significantly more, using daily profit differences
# newey west standard errors because of autocorrelation (like a dm test with profit as loss)
nw_test <- function(d, lag = 7) {
  n <- length(d)
  e <- d - mean(d)
  v <- sum(e^2) / n
  for (k in 1:lag) v <- v + 2 * (1 - k / (lag + 1)) * sum(e[-(1:k)] * e[1:(n - k)]) / n
  t <- mean(d) / sqrt(v / n)
  c(mean_eur_per_day = mean(d), t = t, p_value = 2 * pnorm(-abs(t)))
}

cat("\nLEAR vs naive day:\n"); print(round(nw_test(profit$lear - profit$naive_day), 3))
cat("\nLEAR vs naive mix:\n"); print(round(nw_test(profit$lear - profit$naive_mix), 3))


# cumulative profit plot
p <- profit |>
  pivot_longer(-date, names_to = "strategy", values_to = "profit") |>
  group_by(strategy) |>
  mutate(cum_profit = cumsum(profit)) |>
  ggplot(aes(date, cum_profit, colour = strategy)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Battery profit when planning on each forecast",
       subtitle = paste0(cap_mwh, " MWh / ", power_mw, " MW battery, ",
                         eff_rt * 100, "% round trip, max ", max_cycle, " cycle per day"),
       x = NULL, y = "Cumulative profit (EUR)", colour = NULL) +
  theme_minimal()
print(p)

# save results for the dashboard
saveRDS(list(profit = profit, summary = summary_tbl,
             battery = c(cap_mwh = cap_mwh, power_mw = power_mw,
                         eff_rt = eff_rt, max_cycle = max_cycle)),
        "data/processed/battery_results.rds")

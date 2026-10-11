# economic evaluation with a battery
# battery plans each day with the forecast and gets paid the actual prices
# perfect = plan with actual prices, best possible

library(dplyr)
library(tidyr)
library(lubridate)
library(lpSolve)
library(readr)
library(ggplot2)

# battery
cap_mwh   <- 2       # MWh
power_mw  <- 1       # MW
eff_rt    <- 0.90    # round trip
max_cycle <- 1       # cycles per day
eff       <- sqrt(eff_rt)

# backtest forecasts from 04_lear.R, 06_bmt.R, 08_bmtx.R and 09_xgb.R, same days for all
# bmtx and learx with all four groups
wide <- function(f, combo = NULL) {
  x <- readRDS(f)
  if (!is.null(combo)) x <- x[x$combo == combo, ]
  x |> select(date, hour, model, forecast) |> pivot_wider(names_from = model, values_from = forecast)
}
fc <- readRDS("data/processed/forecasts_lear.rds") |>
  inner_join(select(readRDS("data/processed/forecasts_bmt.rds"), date, hour, bmt),
             by = c("date", "hour")) |>
  inner_join(wide("data/processed/forecasts_bmtx.rds", "nb+wind+solar+hol"), by = c("date", "hour")) |>
  inner_join(wide("data/processed/forecasts_xgb.rds"), by = c("date", "hour"))

# naive, same hour yesterday
fc <- fc |>
  left_join(fc |> transmute(date = date + 1, hour, naive_day = actual),
            by = c("date", "hour")) |>
  drop_na() |>
  group_by(date) |>
  filter(n() == 24) |>
  ungroup() |>
  arrange(date, hour)


# lp for one day: charge c1..c24, discharge d1..d24, state of charge s1..s24,
# u1..u24 = 1 if charging in that hour (0/1, so it can't charge and discharge at the same time)
n  <- 24
ic <- 1:n
id <- n + 1:n
soc <- 2 * n + 1:n
iu <- 3 * n + 1:n

A <- matrix(0, 0, 4 * n); dirs <- c(); rhs <- c()
new_row <- function() numeric(4 * n)

# s_h = s_(h-1) + eff*c_h - d_h/eff, starts empty
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

# limits
for (h in 1:n) {
  r <- new_row(); r[soc[h]] <- 1
  A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, cap_mwh)
  r <- new_row(); r[ic[h]] <- 1; r[iu[h]] <- -power_mw         # c_h <= power * u_h
  A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, 0)
  r <- new_row(); r[id[h]] <- 1; r[iu[h]] <- power_mw          # d_h <= power * (1 - u_h)
  A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, power_mw)
}

# cycles
r <- new_row(); r[id] <- 1
A <- rbind(A, r); dirs <- c(dirs, "<="); rhs <- c(rhs, cap_mwh * max_cycle)

# best plan for given prices, MWh sold per hour (negative = bought)
schedule <- function(price) {
  obj <- c(-price, price, rep(0, 2 * n))
  sol <- lp("max", obj, A, dirs, rhs, binary.vec = iu)
  if (sol$status != 0) stop("lp did not solve")
  sol$solution[id] - sol$solution[ic]
}


# plan with each forecast, paid with actual prices
models     <- c("bmtx", "learx", "xgbx", "bmt", "lear", "xgb", "naive_day", "naive_mix")
strategies <- c(perfect = "actual", setNames(models, models))
days <- unique(fc$date)

profit <- lapply(days, \(dd) {
  x <- fc[fc$date == dd, ]
  sapply(strategies, \(col) sum(x$actual * schedule(x[[col]])))
})
profit <- bind_cols(tibble(date = days), as_tibble(do.call(rbind, profit)))


# summary
years   <- nrow(profit) / 365
perfect <- sum(profit$perfect)

# mae of each forecast on the same days, to compare money with accuracy
mae <- sapply(strategies, \(col) mean(abs(fc$actual - fc[[col]])))

summary_tbl <- profit |>
  pivot_longer(-date, names_to = "strategy", values_to = "profit") |>
  group_by(strategy) |>
  summarise(total_eur        = round(sum(profit)),
            eur_per_mw_year  = round(sum(profit) / power_mw / years),
            share_of_perfect = round(sum(profit) / perfect, 3)) |>
  mutate(mae = round(mae[strategy], 2)) |>
  arrange(desc(total_eur))

print(summary_tbl)


# does a model earn significantly more than another? t test on daily profit differences, newey west se
nw_test <- function(d, lag = 7) {
  n <- length(d)
  e <- d - mean(d)
  v <- sum(e^2) / n
  for (k in 1:lag) v <- v + 2 * (1 - k / (lag + 1)) * sum(e[-(1:k)] * e[1:(n - k)]) / n
  t <- mean(d) / sqrt(v / n)
  c(mean_eur_per_day = mean(d), t = t, p_value = 2 * pnorm(-abs(t)))
}

comparisons <- combn(models, 2, simplify = FALSE)
tests <- bind_rows(lapply(comparisons, \(ab) {
  r <- nw_test(profit[[ab[1]]] - profit[[ab[2]]])
  tibble(model_a = ab[1], model_b = ab[2], mean_eur_per_day = r[["mean_eur_per_day"]],
         t = r[["t"]], p_value = r[["p_value"]])
}))
print(tests)


# cumulative profit
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

# results for the dashboard, small files so they go on github
dir.create("results", showWarnings = FALSE)
write_csv(mutate(profit, across(-date, \(x) round(x, 2))), "results/battery_daily.csv")
write_csv(summary_tbl, "results/battery_summary.csv")
write_csv(tests, "results/battery_tests.csv")
write_csv(tibble(name  = c("cap_mwh", "power_mw", "eff_rt", "max_cycle"),
                 value = c(cap_mwh, power_mw, eff_rt, max_cycle)),
          "results/battery_setup.csv")

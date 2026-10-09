#LEAR LASSO estimated auto regressive model
# article: Lago (2021) - applied energy


# forecasting setup:
# on D-1 forecast all 24 hoursof day D .
# one LASSO regression per hour - 24 regressions, re estimated every day
# all prices from day D-1, D-2, D-3 and D-7 plus weekday dummies are input
# going over a rolling window over the data set time frame ( this analysis is 2 years in this case ) before each forecast day.
# lambda selection: BIC
# prices are transformed  asinh((p - median) / MAD) before fitting,
# which handles spikes and negative prices. Forecasts are transformed back.


library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)
library(ggplot2)

prices <- readRDS("data/processed/prices_dk1_hourly.rds")

# one row per D, one column per hour
daily <- prices |>
  mutate(local = with_tz(time, "Europe/Copenhagen"),
         date  = as_date(local),
         hour  = hour(local)) |>
  group_by(date, hour) |>
  summarise(price = mean(price), .groups = "drop") |>   # 25-hour day: averages the double hour
  pivot_wider(names_from = hour, values_from = price, names_prefix = "h") |>
  complete(date = seq(min(date), max(date), by = "day")) |>
  select(date, all_of(paste0("h", 0:23))) |>
  arrange(date)

# due to spring DST time, we are missing an hour, so we are averaging with hours around it
daily <- daily |>
  mutate(h2 = if_else(is.na(h2), (h1 + h3) / 2, h2))

P <- as.matrix(daily[, -1])   # price matrix. dimension := days x 24 hours

# features, lagged days and weekday dummy
lag_days <- function(M, k) rbind(matrix(NA, k, ncol(M)), M[1:(nrow(M) - k), , drop = FALSE])

X_price <- cbind(lag_days(P, 1), lag_days(P, 2), lag_days(P, 3), lag_days(P, 7))
colnames(X_price) <- paste0("d", rep(c(1, 2, 3, 7), each = 24), "_h", 0:23)
# 1 is monday
dow   <- wday(daily$date, week_start = 1)                 
X_dow <- sapply(1:7, \(d) as.numeric(dow == d))
colnames(X_dow) <- paste0("dow", 1:7)

# Helpers
transform_p <- function(x, med, s) asinh((x - med) / s)
inverse_p   <- function(z, med, s) med + s * sinh(z)

#glmnet to do LASSO BIC

fit_lasso_bic <- function(X, y) {
  fit  <- glmnet(X, y, alpha = 1)
  rss  <- colSums((y - predict(fit, X))^2)
  n    <- length(y)
  bic  <- n * log(rss / n) + fit$df * log(n)
  list(fit = fit, lambda = fit$lambda[which.min(bic)])
}

# rolling forecast over data set test frame
window    <- 730                                         # 2 years of training
test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))
F_lear    <- matrix(NA, nrow(P), 24)

for (d in test_days) {
  train <- (d - window):(d - 1)
  
  # transformation parameters from the training window only , preventing look ahead
  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)
  
  X_train <- cbind(transform_p(X_price[train, ], med, s), X_dow[train, ])
  X_test  <- cbind(transform_p(X_price[d, , drop = FALSE], med, s), X_dow[d, , drop = FALSE])
  if (anyNA(X_test)) next
  
  for (h in 1:24) {
    y    <- transform_p(P[train, h], med, s)
    keep <- complete.cases(X_train) & !is.na(y)
    m    <- fit_lasso_bic(X_train[keep, ], y[keep])
    F_lear[d, h] <- inverse_p(predict(m$fit, X_test, s = m$lambda), med, s)
  }
  
  if (which(test_days == d) %% 30 == 0) message("Forecasted up to ", daily$date[d])
}

# naive benchmark
F_naive <- lag_days(P, 1)
use_week <- dow %in% c(1, 6, 7)
F_naive[use_week, ] <- lag_days(P, 7)[use_week, ]

# comparison
results_long <- tibble(
  date      = rep(daily$date[test_days], each = 24),
  hour      = rep(0:23, times = length(test_days)),
  actual    = as.vector(t(P[test_days, ])),
  lear      = as.vector(t(F_lear[test_days, ])),
  naive_mix = as.vector(t(F_naive[test_days, ]))
) |>
  filter(!is.na(lear), !is.na(naive_mix))

results <- tibble(
  model = c("LEAR", "naive_mix"),
  MAE   = c(mean(abs(results_long$actual - results_long$lear)),
            mean(abs(results_long$actual - results_long$naive_mix))),
  RMSE  = c(sqrt(mean((results_long$actual - results_long$lear)^2)),
            sqrt(mean((results_long$actual - results_long$naive_mix)^2)))
)

print(results)
cat("LEAR improvement over naive (MAE):",
    round(100 * (1 - results$MAE[1] / results$MAE[2]), 1), "%\n")

# plot last 7 days
results_long |>
  filter(date > max(date) - 7) |>
  mutate(time = as_datetime(date, tz = "Europe/Copenhagen") + hours(hour)) |>
  ggplot(aes(time)) +
  geom_line(aes(y = actual,    colour = "Actual"),    linewidth = 0.6) +
  geom_line(aes(y = lear,      colour = "LEAR"),      linewidth = 0.5) +
  geom_line(aes(y = naive_mix, colour = "Naive mix"), linewidth = 0.4, linetype = "dashed") +
  scale_colour_manual(values = c("Actual" = "black", "LEAR" = "blue", "Naive mix" = "red")) +
  labs(title = "DK1 day-ahead price: LEAR vs naive, last 7 days",
       x = NULL, y = "EUR/MWh", colour = NULL) +
  theme_minimal()

# save lear forecasts
saveRDS(results_long, "data/processed/forecasts_lear.rds")
  

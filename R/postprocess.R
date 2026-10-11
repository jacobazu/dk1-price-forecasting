# bias correction and prediction intervals from past forecast errors
# for delivery day D only errors from days before D are used. the prices of D-1 are known from the
# auction on D-2, so on the morning of D-1 the errors up to and including D-1 are known
# settings are fixed beforehand, not tuned on the test period:
#   bias correction = median error of the last 28 days (all hours)
#   80% interval    = 10% and 90% quantile of the last 60 days of errors for that hour,
#                     errors divided by how volatile prices were in the week before each day,
#                     then multiplied by the volatility of the week before D (wider in wild weeks)

library(dplyr)

k_bias  <- 28
k_int   <- 60
k_vol   <- 7
q_probs <- c(0.1, 0.9)

# volatility before each day D: sd of the hourly prices in the k_vol days before D
# prices = data frame with date, hour, actual
vol_scale <- function(prices, dates) {
  prices <- distinct(prices, date, hour, .keep_all = TRUE)
  tibble(date = dates,
         s = sapply(dates, \(D) {
           x <- prices$actual[prices$date >= D - k_vol & prices$date < D]
           if (length(x) < 0.8 * 24 * k_vol) NA else sd(x, na.rm = TRUE)
         }))
}

# errors: data frame with date, hour, e (actual - forecast) and s (vol_scale of that date)
# returns bias, the plain interval (lo, hi) and the scaled interval (lo_s, hi_s) for day D
adjust_day <- function(errors, D, s_D) {
  past_b <- errors[errors$date >= D - k_bias & errors$date < D, ]
  past_i <- errors[errors$date >= D - k_int & errors$date < D & !is.na(errors$s), ]
  if (n_distinct(past_i$date) < 0.8 * k_int || is.na(s_D)) return(NULL)   # not enough history yet
  q <- past_i |>
    group_by(hour) |>
    summarise(lo   = quantile(e, q_probs[1]), hi = quantile(e, q_probs[2]),
              lo_s = s_D * quantile(e / s, q_probs[1]), hi_s = s_D * quantile(e / s, q_probs[2]),
              .groups = "drop")
  mutate(q, date = D, bias = median(past_b$e))
}

# for every day in fc (date, hour, actual, model column m): corrected forecast and intervals
postprocess <- function(fc, m, scale) {
  errors <- fc |>
    transmute(date, hour, e = actual - .data[[m]]) |>
    left_join(scale, by = "date")
  dates <- sort(unique(fc$date))
  adj <- bind_rows(lapply(dates, \(D) adjust_day(errors, D, scale$s[scale$date == D])))
  fc |>
    select(date, hour, actual, forecast = all_of(m)) |>
    inner_join(adj, by = c("date", "hour")) |>
    mutate(model     = m,
           corrected = forecast + bias,
           lower     = forecast + lo,   upper   = forecast + hi,
           lower_s   = forecast + lo_s, upper_s = forecast + hi_s)
}

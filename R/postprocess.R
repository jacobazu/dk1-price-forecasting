# bias correction and prediction intervals from past forecast errors
# for delivery day D only errors from days before D are used. the prices of D-1 are known from the
# auction on D-2, so on the morning of D-1 the errors up to and including D-1 are known
# settings are fixed beforehand, not tuned on the test period:
#   bias correction = median error of the last 28 days (all hours)
#   80% interval    = 10% and 90% quantile of the last 60 days of errors for that hour

library(dplyr)

k_bias  <- 28
k_int   <- 60
q_probs <- c(0.1, 0.9)

# errors: data frame with date, hour, e (actual - forecast). returns bias and interval for day D
adjust_day <- function(errors, D) {
  past_b <- errors[errors$date >= D - k_bias & errors$date < D, ]
  past_i <- errors[errors$date >= D - k_int & errors$date < D, ]
  if (n_distinct(past_i$date) < 0.8 * k_int) return(NULL)   # not enough history yet
  q <- past_i |>
    group_by(hour) |>
    summarise(lo = quantile(e, q_probs[1]), hi = quantile(e, q_probs[2]), .groups = "drop")
  mutate(q, date = D, bias = median(past_b$e))
}

# for every day in fc (date, hour, actual, model column m): corrected forecast and interval
postprocess <- function(fc, m) {
  errors <- transmute(fc, date, hour, e = actual - .data[[m]])
  adj <- bind_rows(lapply(sort(unique(fc$date)), \(D) adjust_day(errors, D)))
  fc |>
    select(date, hour, actual, forecast = all_of(m)) |>
    inner_join(adj, by = c("date", "hour")) |>
    mutate(model     = m,
           corrected = forecast + bias,
           lower     = forecast + lo,
           upper     = forecast + hi)
}

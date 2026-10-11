# bias correction and 80% intervals from past errors, only days before D are used
# bias = median error of the last 28 days. interval = 10% and 90% quantile of the last 60 days
# per hour, scaled by how volatile the week before was

library(dplyr)

k_bias  <- 28
k_int   <- 60
k_vol   <- 7
q_probs <- c(0.1, 0.9)

# sd of the hourly prices in the week before each date
vol_scale <- function(prices, dates) {
  prices <- distinct(prices, date, hour, .keep_all = TRUE)
  tibble(date = dates,
         s = sapply(dates, \(D) {
           x <- prices$actual[prices$date >= D - k_vol & prices$date < D]
           if (length(x) < 0.8 * 24 * k_vol) NA else sd(x, na.rm = TRUE)
         }))
}

# bias and intervals for day D (lo/hi plain, lo_s/hi_s scaled)
adjust_day <- function(errors, D, s_D) {
  past_b <- errors[errors$date >= D - k_bias & errors$date < D, ]
  past_i <- errors[errors$date >= D - k_int & errors$date < D & !is.na(errors$s), ]
  if (n_distinct(past_i$date) < 0.8 * k_int || is.na(s_D)) return(NULL)   # not enough history
  q <- past_i |>
    group_by(hour) |>
    summarise(lo   = quantile(e, q_probs[1]), hi = quantile(e, q_probs[2]),
              lo_s = s_D * quantile(e / s, q_probs[1]), hi_s = s_D * quantile(e / s, q_probs[2]),
              .groups = "drop")
  mutate(q, date = D, bias = median(past_b$e))
}

# corrected forecast and intervals for every day in fc
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

# bias correction and 80% intervals from past errors, only days before D are used
# bias = median error of the last 28 days, or a + b * forecast fitted on the same days (mincer-zarnowitz)
# interval = 10% and 90% quantile of the last 60 days per hour, scaled by how volatile the week before was

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

# mincer-zarnowitz correction (theil): actual = a + b * forecast on the same last 28 days
mz_day <- function(x, D) {
  past <- x[x$date >= D - k_bias & x$date < D, ]
  if (n_distinct(past$date) < 0.8 * k_bias) return(NULL)
  ab <- coef(lm(actual ~ forecast, data = past))
  tibble(date = D, mz_a = ab[[1]], mz_b = ab[[2]])
}

# corrected forecasts and intervals for every day in fc
postprocess <- function(fc, m, scale) {
  x <- select(fc, date, hour, actual, forecast = all_of(m))
  errors <- x |>
    transmute(date, hour, e = actual - forecast) |>
    left_join(scale, by = "date")
  dates <- sort(unique(fc$date))
  adj <- bind_rows(lapply(dates, \(D) adjust_day(errors, D, scale$s[scale$date == D])))
  mz  <- bind_rows(lapply(unique(adj$date), \(D) mz_day(x, D)))
  x |>
    inner_join(adj, by = c("date", "hour")) |>
    inner_join(mz, by = "date") |>
    mutate(model        = m,
           corrected    = forecast + bias,
           corrected_mz = mz_a + mz_b * forecast,
           lower        = forecast + lo,   upper   = forecast + hi,
           lower_s      = forecast + lo_s, upper_s = forecast + hi_s)
}

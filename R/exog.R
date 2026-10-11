# exogenous candidates for BMT-X, one row per day like the price lags in R/lear.R
# groups: neighbour prices (D-1), wind forecasts (D), solar forecast (D), holidays
# uses lag_days() from R/lear.R

library(dplyr)
library(tidyr)
library(lubridate)

# hourly series (time in utc) to a days x 24 matrix in danish time, same rules as build_daily()
to_daily <- function(df, col, dates) {
  d <- df |>
    transmute(local = with_tz(time, "Europe/Copenhagen"), value = .data[[col]]) |>
    mutate(date = as_date(local), hour = hour(local)) |>
    group_by(date, hour) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    mutate(value = if_else(is.nan(value), NA_real_, value)) |>
    pivot_wider(names_from = hour, values_from = value, names_prefix = "h")
  for (h in paste0("h", 0:23)) if (!h %in% names(d)) d[[h]] <- NA_real_
  d <- tibble(date = dates) |>
    left_join(d, by = "date") |>
    select(all_of(paste0("h", 0:23))) |>
    mutate(h2 = if_else(is.na(h2), (h1 + h3) / 2, h2))
  as.matrix(d)
}

# fill short gaps in an hourly series by linear interpolation (not at the ends)
fill_gaps <- function(df, cols) {
  grid <- tibble(time = seq(min(df$time), max(df$time), by = "hour"))
  df <- left_join(grid, df, by = "time")
  for (v in cols) {
    ok <- !is.na(df[[v]])
    df[[v]] <- approx(as.numeric(df$time[ok]), df[[v]][ok], xout = as.numeric(df$time), rule = 1)$y
  }
  df
}

# all exogenous candidates, raw (not transformed), per group
make_exog <- function(dates, neighbours, wind_solar, holidays) {
  areas <- c("DE", "DK2", "NO2", "SE3", "SE4")
  nb <- do.call(cbind, lapply(areas, \(a) {
    m <- lag_days(to_daily(neighbours, paste0("price_", a), dates), 1)
    colnames(m) <- paste0(tolower(a), "_d1_h", 0:23)
    m
  }))

  ws <- fill_gaps(wind_solar, c("fc_offshore_wind", "fc_onshore_wind", "fc_solar"))
  wind <- cbind(to_daily(ws, "fc_offshore_wind", dates), to_daily(ws, "fc_onshore_wind", dates))
  colnames(wind) <- c(paste0("offwind_d0_h", 0:23), paste0("onwind_d0_h", 0:23))
  solar <- to_daily(ws, "fc_solar", dates)
  colnames(solar) <- paste0("solar_d0_h", 0:23)

  hol <- cbind(hol_d0 = as.numeric(dates %in% holidays$date),
               hol_d1 = as.numeric((dates - 1) %in% holidays$date))

  list(nb = nb, wind = wind, solar = solar, hol = hol)
}

# transform with the training window only: neighbour prices like the dk1 prices (asinh with
# their own median and mad per area), wind and solar standardised, holidays as they are
transform_exog <- function(exog, train) {
  block <- function(X, f) {
    series <- sub("_h[0-9]+$", "", colnames(X))
    for (s in unique(series)) {
      j <- series == s
      X[, j] <- f(X[, j, drop = FALSE], X[train, j, drop = FALSE])
    }
    X
  }
  list(
    nb    = block(exog$nb, \(x, tr) {
      s <- mad(tr, na.rm = TRUE); if (!is.finite(s) || s == 0) s <- sd(tr, na.rm = TRUE)
      asinh((x - median(tr, na.rm = TRUE)) / s)
    }),
    wind  = block(exog$wind,  \(x, tr) (x - mean(tr, na.rm = TRUE)) / sd(tr, na.rm = TRUE)),
    solar = block(exog$solar, \(x, tr) (x - mean(tr, na.rm = TRUE)) / sd(tr, na.rm = TRUE)),
    hol   = exog$hol
  )
}

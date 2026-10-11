# BMT-X backtest: BMT with exogenous variables, all 16 combinations of four groups
# dk1 price lags (D-1, D-2, D-3, D-7) are always candidates, weekday dummies always included
# groups: neighbour prices (DE, DK2, NO2, SE3, SE4 on D-1), wind forecasts (offshore + onshore
# for D), solar forecast (D), holidays (D and D-1)
# LEAR-X = lasso on the full pool (all groups), to compare lasso and multiple testing fairly
# needs the files from 02_clean.R and 07_download_exog.R

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(parallel)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
P        <- features$P
D        <- features$X_dow[, -1]   # monday is the baseline

exog <- make_exog(daily$date,
                  readRDS("data/raw/exog_neighbours.rds"),
                  readRDS("data/raw/exog_wind_solar.rds"),
                  readRDS("data/processed/holidays_dk.rds"))

groups   <- c("nb", "wind", "solar", "hol")
combos   <- expand.grid(rep(list(c(FALSE, TRUE)), 4))
names(combos) <- groups
combo_id <- apply(combos, 1, \(r) if (!any(r)) "prices" else paste(groups[r], collapse = "+"))

window <- 730
pval   <- 0.05
delta1 <- 1
delta2 <- 1
cores  <- max(1, detectCores() - 1)

test_days <- which(daily$date >= as_date("2025-10-01") & complete.cases(P))


forecast_day <- function(d) {
  train <- (d - window):(d - 1)
  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)

  ex     <- transform_exog(exog, train)
  X_full <- cbind(transform_p(features$X_price, med, s), ex$nb, ex$wind, ex$solar, ex$hol)
  grp    <- rep(c("prices", groups), c(ncol(features$X_price), ncol(ex$nb), ncol(ex$wind),
                                       ncol(ex$solar), ncol(ex$hol)))
  if (anyNA(X_full[d, ])) return(NULL)

  # training days with all candidates and all 24 prices, same rows for every combination
  keep <- train[complete.cases(X_full[train, ]) & complete.cases(P[train, ])]
  Xtr  <- X_full[keep, ]
  Dtr  <- D[keep, ]

  # controls taken out once, shared by all combinations and hours
  qC  <- qr(cbind(1, Dtr))
  E0  <- qr.resid(qC, Xtr)
  df0 <- length(keep) - qC$rank
  ss0 <- colSums(scale(Xtr, scale = FALSE)^2)

  rows <- list()
  for (h in 1:24) {
    y   <- transform_p(P[keep, h], med, s)
    ey0 <- qr.resid(qC, y)

    for (k in seq_along(combo_id)) {
      cols <- which(grp %in% c("prices", groups[unlist(combos[k, ])]))
      sel  <- cols[bmt_select_resid(E0[, cols], ey0, df0, ss0[cols], pval, delta1, delta2)]

      Z    <- cbind(1, Dtr, Xtr[, sel, drop = FALSE])
      beta <- qr.coef(qr(Z), y)
      beta[is.na(beta)] <- 0
      z    <- c(1, D[d, ], X_full[d, sel])

      rows[[length(rows) + 1]] <- tibble(
        date = daily$date[d], hour = h - 1, model = "bmtx", combo = combo_id[k],
        forecast = inverse_p(sum(z * beta), med, s),
        n_sel = length(sel),
        n_prices = sum(grp[sel] == "prices"), n_nb = sum(grp[sel] == "nb"),
        n_wind = sum(grp[sel] == "wind"), n_solar = sum(grp[sel] == "solar"),
        n_hol = sum(grp[sel] == "hol"))
    }

    # LEAR-X, lasso with all groups (all weekday dummies as in LEAR)
    Xl <- cbind(Xtr, features$X_dow[keep, ])
    m  <- fit_lasso_bic(Xl, y)
    xl <- matrix(c(X_full[d, ], features$X_dow[d, ]), nrow = 1)
    rows[[length(rows) + 1]] <- tibble(
      date = daily$date[d], hour = h - 1, model = "learx", combo = "nb+wind+solar+hol",
      forecast = as.numeric(inverse_p(predict(m$fit, xl, s = m$lambda), med, s)))
  }
  bind_rows(rows)
}

t0  <- Sys.time()
out <- mclapply(test_days, \(d) tryCatch(forecast_day(d), error = \(e) NULL), mc.cores = cores)
cat("done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "minutes\n")

actual <- tibble(date   = rep(daily$date, each = 24),
                 hour   = rep(0:23, times = nrow(P)),
                 actual = as.vector(t(P)))
fc <- bind_rows(out) |>
  inner_join(actual, by = c("date", "hour"))

# quick look: mae per combination
fc |>
  group_by(model, combo) |>
  summarise(mae = mean(abs(actual - forecast)), n_sel = mean(n_sel), .groups = "drop") |>
  arrange(mae) |>
  print(n = Inf)

saveRDS(fc, "data/processed/forecasts_bmtx.rds")

# how many variables bmt picks from each group, per combination (for the dashboard)
dir.create("results", showWarnings = FALSE)
fc |>
  filter(model == "bmtx") |>
  group_by(combo) |>
  summarise(across(c(n_sel, n_prices, n_nb, n_wind, n_solar, n_hol), mean), .groups = "drop") |>
  write_csv("results/bmtx_selection.csv")

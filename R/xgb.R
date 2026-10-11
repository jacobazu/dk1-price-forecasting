# xgboost: one model for all 24 hours (one row per day and hour) instead of 24 models
# target = asinh-transformed price like LEAR and BMT, trees don't care about scaling the inputs
# uses build_daily(), make_features(), transform_p(), inverse_p() from R/lear.R
# and the exog list from make_exog() in R/exog.R

library(xgboost)

xgb_params <- list(objective        = "reg:squarederror",
                   learning_rate    = 0.05,
                   max_depth        = 6,
                   min_child_weight = 10,
                   subsample        = 0.8,
                   colsample_bytree = 0.8,
                   nthread          = 1)
xgb_rounds <- 500

# days x 24 matrix to one long column (day 1 hour 0, day 1 hour 1, ...)
long <- function(M) as.vector(t(M))

# one row per day and hour, for the days in rows
# prices: same hour on D-1, D-2, D-3, D-7 and the level and range of D-1
# exog (optional): neighbour prices same hour D-1 + their daily mean, holidays,
# and wind and solar forecasts for the hour + daily mean if exog has them
xgb_design <- function(features, rows, exog = NULL) {
  P   <- features$P
  lag <- function(k) lag_days(P, k)[rows, , drop = FALSE]
  d1  <- lag(1)
  rep24 <- function(x) rep(x, each = 24)

  X <- cbind(
    hour    = rep(0:23, times = length(rows)),
    dow     = rep24(features$dow[rows]),
    p_d1    = long(d1),
    p_d2    = long(lag(2)),
    p_d3    = long(lag(3)),
    p_d7    = long(lag(7)),
    d1_mean = rep24(rowMeans(d1)),
    d1_min  = rep24(apply(d1, 1, min)),
    d1_max  = rep24(apply(d1, 1, max)),
    d1_h23  = rep24(d1[, 24])
  )
  if (is.null(exog)) return(X)

  series <- function(M, name) {
    m <- M[rows, grepl(paste0("^", name, "_"), colnames(M)), drop = FALSE]
    cbind(long(m), rep24(rowMeans(m)))
  }
  nb <- do.call(cbind, lapply(c("de", "dk2", "no2", "se3", "se4"), \(a) {
    m <- series(exog$nb, paste0(a, "_d1"))
    colnames(m) <- paste0(a, c("_d1", "_d1_mean"))
    m
  }))
  # wind and solar only if they are in the list (the day-ahead forecasts are published at 18:00,
  # after the auction, so they are left out of the honest version)
  ws <- NULL
  if (!is.null(exog$wind)) {
    ws <- do.call(cbind, lapply(list(c("offwind_d0", "offwind"), c("onwind_d0", "onwind"),
                                     c("solar_d0", "solar")), \(v) {
      M <- if (v[2] == "solar") exog$solar else exog$wind
      m <- series(M, v[1])
      colnames(m) <- paste0(v[2], c("", "_mean"))
      m
    }))
  }
  cbind(X, nb, ws,
        hol_d0 = rep24(exog$hol[rows, "hol_d0"]),
        hol_d1 = rep24(exog$hol[rows, "hol_d1"]))
}

# forecast all 24 hours of day d, trained on the `window` days before it
# rounds can be a vector (for tuning), then one column per number of rounds
# seed = the forecast date as a number, so the backtest and the live run use the same random rows
xgb_forecast_day <- function(features, d, window = 730, exog = NULL, params = xgb_params,
                             rounds = xgb_rounds, seed) {
  P     <- features$P
  train <- (d - window):(d - 1)

  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)

  X_test <- xgb_design(features, d, exog)
  if (anyNA(X_test[, c("p_d1", "p_d2", "p_d3", "p_d7")])) stop("Missing input prices for the forecast day")

  X <- xgb_design(features, train, exog)
  y <- transform_p(long(P[train, , drop = FALSE]), med, s)
  keep <- !is.na(y)

  set.seed(seed)
  m <- xgb.train(params = params,
                 data = xgb.DMatrix(X[keep, ], label = y[keep]),
                 nrounds = max(rounds), verbose = 0)
  fc <- sapply(rounds, \(r) inverse_p(predict(m, X_test, iterationrange = c(1, r)), med, s))
  if (length(rounds) == 1) drop(fc) else fc
}

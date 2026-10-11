# check that the fast bmt_select() picks the same variables as boosting_glm(link = "mean")

source("R/boosting_glm.R")
source("R/bmt.R")

check <- function(y, X, Xcond, pval = 0.05, delta1 = 1, delta2 = 1) {
  slow <- boosting_glm(y, X, Xcond, pval = pval, delta1 = delta1, delta2 = delta2, link = "mean")
  fast <- bmt_select(y, X, Xcond, pval = pval, delta1 = delta1, delta2 = delta2)
  identical(slow, fast)
}

# simulated data, a few true variables and correlated noise variables
set.seed(1)
ok <- sapply(1:20, \(i) {
  Tn <- 300; N <- 40
  X  <- matrix(rnorm(Tn * N), Tn, N) + rnorm(Tn) * 0.5
  Xc <- matrix(rnorm(Tn * 2), Tn, 2)
  y  <- 1 + X[, 1] - 0.7 * X[, 5] + 0.4 * X[, 12] + Xc %*% c(0.3, -0.2) + rnorm(Tn)
  check(drop(y), X, Xc, pval = sample(c(0.01, 0.05, 0.1), 1), delta2 = sample(1:2, 1))
})
cat("simulated:", sum(ok), "of", length(ok), "identical\n")

# real prices, one hour, same inputs as the forecast
if (file.exists("data/processed/prices_dk1_hourly.rds")) {
  source("R/lear.R")
  prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
  daily    <- build_daily(prices)
  features <- make_features(daily)
  d     <- nrow(daily) - 30
  train <- (d - 730):(d - 1)
  P     <- features$P
  med   <- median(P[train, ], na.rm = TRUE)
  s     <- mad(P[train, ], na.rm = TRUE)
  X     <- transform_p(features$X_price[train, ], med, s)
  D     <- features$X_dow[train, -1]
  real_ok <- sapply(c(1, 8, 13, 19), \(h) {
    y    <- transform_p(P[train, h], med, s)
    keep <- complete.cases(X) & !is.na(y)
    check(y[keep], X[keep, ], D[keep, ])
  })
  cat("real prices:", sum(real_ok), "of", length(real_ok), "identical\n")
  ok <- c(ok, real_ok)
}

stopifnot(all(ok))
cat("all good\n")

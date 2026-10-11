# fast BMT (boosted multiple testing) with mean link
# same selection as boosting_glm(link = "mean") but without calling lm for every candidate:
# all candidates are tested at once on residuals (frisch-waugh), checked in tests/test_bmt.R
# uses transform_p() and inverse_p() from R/lear.R

bmt_select <- function(y, X, Xcond = NULL, pval = 0.05, delta1 = 1, delta2 = 1) {
  X  <- as.matrix(X)
  Tn <- nrow(X)
  N  <- ncol(X)
  if (is.null(Xcond)) Xcond <- matrix(numeric(0), Tn, 0)

  thr1 <- qnorm(1 - (pval / N^(delta1 - 1)) / (2 * N))
  thr2 <- qnorm(1 - (pval / N^(delta2 - 1)) / (2 * N))

  ind   <- rep(FALSE, N)
  first <- TRUE

  repeat {
    cand <- which(!ind)
    if (length(cand) == 0) break

    # residuals after intercept, controls and selected variables
    q  <- qr(cbind(1, Xcond, X[, ind, drop = FALSE]))
    ey <- qr.resid(q, y)
    E  <- qr.resid(q, X[, cand, drop = FALSE])

    sxx <- colSums(E^2)
    sxy <- drop(crossprod(E, ey))
    df  <- Tn - q$rank - 1
    ssr <- sum(ey^2) - sxy^2 / sxx
    t   <- abs(sxy) / sqrt(sxx * ssr / df)

    # candidates that are (almost) a combination of what is already in, lm drops them
    xc <- scale(X[, cand, drop = FALSE], scale = FALSE)
    t[sxx < 1e-12 * colSums(xc^2) | sxx == 0] <- NA

    if (all(is.na(t))) break
    best <- which.max(t)
    if (t[best] > if (first) thr1 else thr2) {
      ind[cand[best]] <- TRUE
      first <- FALSE
    } else {
      break
    }
  }
  ind
}


# forecast all 24 hours of day d with BMT, same inputs and window as lear_forecast_day()
# selection on the transformed prices, then ols on the selected variables + weekday dummies
bmt_forecast_day <- function(features, d, window = 730, pval = 0.05, delta1 = 1, delta2 = 1,
                             return_selected = FALSE) {
  P     <- features$P
  train <- (d - window):(d - 1)

  med <- median(P[train, ], na.rm = TRUE)
  s   <- mad(P[train, ], na.rm = TRUE)

  X_all <- transform_p(features$X_price, med, s)
  X_dow <- features$X_dow[, -1, drop = FALSE]   # monday is the baseline

  X_test <- X_all[d, , drop = FALSE]
  if (anyNA(X_test)) stop("Missing input prices for the forecast day")

  selected <- matrix(FALSE, 24, ncol(X_all), dimnames = list(paste0("h", 0:23), colnames(X_all)))
  fc <- numeric(24)

  for (h in 1:24) {
    y    <- transform_p(P[train, h], med, s)
    keep <- complete.cases(X_all[train, ]) & !is.na(y)
    Xtr  <- X_all[train, ][keep, , drop = FALSE]
    Dtr  <- X_dow[train, , drop = FALSE][keep, , drop = FALSE]

    sel <- bmt_select(y[keep], Xtr, Dtr, pval, delta1, delta2)
    selected[h, ] <- sel

    Z    <- cbind(1, Dtr, Xtr[, sel, drop = FALSE])
    beta <- qr.coef(qr(Z), y[keep])
    beta[is.na(beta)] <- 0
    z_test <- cbind(1, X_dow[d, , drop = FALSE], X_test[, sel, drop = FALSE])
    fc[h]  <- inverse_p(drop(z_test %*% beta), med, s)
  }

  if (return_selected) list(forecast = fc, selected = selected) else fc
}


# same selection again, but starting from candidates and y that already have the controls
# (intercept + weekday dummies) taken out: E = residual candidates, ey = residual y,
# df0 = rows minus rank of the controls, ss0 = centred sum of squares of the raw candidates.
# after each selection the selected column is taken out of all others (gram-schmidt),
# so the controls only have to be removed once per day, used for the 16 BMT-X combinations
bmt_select_resid <- function(E, ey, df0, ss0, pval = 0.05, delta1 = 1, delta2 = 1) {
  N   <- ncol(E)
  thr <- c(qnorm(1 - (pval / N^(delta1 - 1)) / (2 * N)),
           qnorm(1 - (pval / N^(delta2 - 1)) / (2 * N)))
  sel <- integer(0)

  repeat {
    sxx <- colSums(E^2)
    sxy <- drop(crossprod(E, ey))
    df  <- df0 - length(sel) - 1
    ssr <- sum(ey^2) - sxy^2 / sxx
    t   <- abs(sxy) / sqrt(sxx * ssr / df)
    t[sel] <- NA
    t[sxx < 1e-12 * ss0 | sxx == 0] <- NA
    if (all(is.na(t))) break

    j <- which.max(t)
    if (t[j] <= thr[if (length(sel) == 0) 1 else 2]) break
    sel <- c(sel, j)
    e   <- E[, j]
    ee  <- sum(e^2)
    E   <- E - outer(e, drop(crossprod(e, E)) / ee)
    ey  <- ey - e * sum(e * ey) / ee
  }
  sel
}

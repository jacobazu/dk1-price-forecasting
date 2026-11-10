# neural network for one day, base r only (no torch): the second standard model in lago et al. 2021
# one network gives all 24 hours. inputs as lear-x. two hidden layers, relu, dropout, adamw, huber loss,
# early stopping on a random fifth of the training days, averaged over a few seeds

dnn_fit <- function(X, Y, hidden = c(192, 96), drop = 0.2, lr = 2e-3, decay = 1e-3, batch = 64,
                    epochs = 250, patience = 20, seed = 1) {
  set.seed(seed)
  n <- nrow(X)
  val <- sample(n, round(n / 5))
  tr  <- setdiff(seq_len(n), val)
  sizes <- c(ncol(X), hidden, ncol(Y))
  L <- length(sizes) - 1
  W <- lapply(1:L, \(l) matrix(rnorm(sizes[l] * sizes[l + 1], sd = sqrt(2 / sizes[l])), sizes[l], sizes[l + 1]))
  b <- lapply(1:L, \(l) numeric(sizes[l + 1]))
  mW <- lapply(W, \(w) w * 0); vW <- mW; mb <- lapply(b, \(x) x * 0); vb <- mb

  forward <- function(x, W, b, train = FALSE) {
    a <- list(x); mask <- list()
    for (l in 1:L) {
      z <- sweep(a[[l]] %*% W[[l]], 2, b[[l]], "+")
      if (l < L) {
        z <- pmax(z, 0)
        if (train && drop > 0) { mask[[l]] <- (matrix(runif(length(z)), nrow(z)) > drop) / (1 - drop); z <- z * mask[[l]] }
      }
      a[[l + 1]] <- z
    }
    list(a = a, mask = mask)
  }
  huber_grad <- function(r) pmax(pmin(r, 1), -1)       # derivative of the huber loss, delta = 1
  loss <- function(W, b, i) mean(abs(forward(X[i, , drop = FALSE], W, b)$a[[L + 1]] - Y[i, , drop = FALSE]))

  best <- list(W = W, b = b, loss = Inf); wait <- 0; t <- 0
  for (ep in seq_len(epochs)) {
    for (i in split(sample(tr), ceiling(seq_along(tr) / batch))) {
      f <- forward(X[i, , drop = FALSE], W, b, train = TRUE)
      g <- huber_grad(f$a[[L + 1]] - Y[i, , drop = FALSE]) / length(i)
      t <- t + 1
      for (l in L:1) {
        gW <- crossprod(f$a[[l]], g); gb <- colSums(g)
        if (l > 1) {
          g <- tcrossprod(g, W[[l]]) * (f$a[[l]] > 0)
          if (drop > 0) g <- g * f$mask[[l - 1]]
        }
        mW[[l]] <- 0.9 * mW[[l]] + 0.1 * gW;  vW[[l]] <- 0.999 * vW[[l]] + 0.001 * gW^2
        mb[[l]] <- 0.9 * mb[[l]] + 0.1 * gb;  vb[[l]] <- 0.999 * vb[[l]] + 0.001 * gb^2
        k <- lr * sqrt(1 - 0.999^t) / (1 - 0.9^t)
        W[[l]] <- W[[l]] * (1 - lr * decay) - k * mW[[l]] / (sqrt(vW[[l]]) + 1e-8)
        b[[l]] <- b[[l]] - k * mb[[l]] / (sqrt(vb[[l]]) + 1e-8)
      }
    }
    v <- loss(W, b, val)
    if (v < best$loss - 1e-4) { best <- list(W = W, b = b, loss = v); wait <- 0 } else if ((wait <- wait + 1) >= patience) break
  }
  function(x) forward(x, best$W, best$b)$a[[L + 1]]
}

dnn_forecast_day <- function(features, exog, d, groups = c("nb", "hol", "wx"), window = 730, seeds = 3, ...) {
  inp <- x_inputs(features, exog, d, groups, window)
  X   <- cbind(inp$X[inp$keep, ], features$X_dow[inp$keep, ])
  x   <- matrix(c(inp$X[d, ], features$X_dow[d, ]), nrow = 1)
  mu  <- colMeans(X); s <- apply(X, 2, sd)
  ok  <- s > 1e-8
  Xs  <- sweep(sweep(X[, ok], 2, mu[ok]), 2, s[ok], "/")
  xs  <- pmax(pmin((x[, ok, drop = FALSE] - mu[ok]) / s[ok], 6), -6)   # no wild extrapolation
  Y   <- transform_p(features$P[inp$keep, ], inp$med, inp$s)
  out <- sapply(seq_len(seeds), \(k) dnn_fit(Xs, Y, seed = d * 10 + k, ...)(xs))
  as.numeric(inverse_p(rowMeans(out), inp$med, inp$s))
}

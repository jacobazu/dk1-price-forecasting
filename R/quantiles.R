# quantile forecasts from past errors
# emp: error quantiles, last 60 days
# emp_s: scaled by last week's sd
# aci: adaptive levels (gibbs & candes)
# garch: scaled by a garch(1,1) on the size of the forecast's own errors (filtered historical simulation)

library(dplyr)

taus      <- round(seq(0.05, 0.95, by = 0.05), 2)
k_q       <- 60     # days of errors
k_vol     <- 7      # days for sd
gamma_q   <- 0.02   # aci step
q_methods <- c(emp = "Error quantiles", emp_s = "Scaled by volatility", aci = "Adaptive (ACI)",
               garch = "GARCH on the errors")
k_garch   <- 30     # days of errors before the first garch fit, refit every 30 days on up to two years

# sd of last week's prices
vol_scale <- function(prices, dates) {
  prices <- distinct(prices, date, hour, .keep_all = TRUE)
  tibble(date = dates,
         s = sapply(dates, \(D) {
           x <- prices$actual[prices$date >= D - k_vol & prices$date < D]
           if (length(x) < 0.8 * 24 * k_vol) NA else sd(x, na.rm = TRUE)
         }))
}

# garch(1,1) on the mean squared error of each day. sigma for day i uses days before i only
garch_path <- function(p, x) {
  s2 <- numeric(length(x)); s2[1] <- mean(x, na.rm = TRUE)
  for (i in seq_along(x)[-1]) s2[i] <- p[1] + p[2] * ifelse(is.na(x[i - 1]), s2[i - 1], x[i - 1]) + p[3] * s2[i - 1]
  s2
}
garch_fit <- function(x) {
  par <- function(q) c(exp(q[1]), plogis(q[2]) * plogis(q[3]), (1 - plogis(q[2])) * plogis(q[3]))   # alpha + beta < 1
  nll <- function(q) {
    s2 <- garch_path(par(q), x)
    ok <- !is.na(x)
    sum(log(s2[ok]) + x[ok] / s2[ok])
  }
  par(optim(c(log(mean(x, na.rm = TRUE) * 0.05), 0, 2), nll)$par)
}
garch_sigma <- function(dates, E) {
  r2  <- rowMeans(E^2)
  sig <- rep(NA_real_, length(dates))
  p <- NULL; last_fit <- as.Date(NA)
  for (i in seq_along(dates)) {
    past <- which(dates < dates[i] & dates >= dates[i] - 730)
    if (sum(!is.na(r2[past])) < k_garch) next
    if (is.na(last_fit) || dates[i] - last_fit >= 30) {
      p <- tryCatch(garch_fit(r2[past]), error = \(e) p)
      last_fit <- dates[i]
    }
    if (is.null(p)) next
    sig[i] <- sqrt(tail(garch_path(p, c(r2[past], NA)), 1))
  }
  sig
}

# date, hour, e to days x 24
err_matrix <- function(x) {
  dates <- sort(unique(x$date))
  E <- matrix(NA_real_, length(dates), 24)
  E[cbind(match(x$date, dates), x$hour + 1)] <- x$e
  list(dates = dates, E = E)
}

# offsets from forecast, all methods
# only past days used
quantile_offsets <- function(dates, E, s) {
  level <- taus
  g <- garch_sigma(dates, E)
  out <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    past <- dates >= dates[i] - k_q & dates < dates[i] & complete.cases(E) & !is.na(s)
    if (sum(past) < 0.8 * k_q || is.na(s[i])) next
    Ep  <- E[past, , drop = FALSE]
    Zp  <- Ep / s[past]
    off <- list(emp   = apply(Ep, 2, quantile, probs = taus, names = FALSE),
                emp_s = s[i] * apply(Zp, 2, quantile, probs = taus, names = FALSE),
                aci   = s[i] * apply(Zp, 2, quantile, probs = level, names = FALSE))
    pg <- past & !is.na(g)
    if (sum(pg) >= 0.8 * k_q && !is.na(g[i]))
      off$garch <- g[i] * apply(E[pg, , drop = FALSE] / g[pg], 2, quantile, probs = taus, names = FALSE)
    off <- lapply(off, \(o) apply(o, 2, sort))   # no crossing
    out[[i]] <- bind_rows(lapply(names(off), \(m)
      tibble(date = dates[i], method = m, hour = rep(0:23, each = length(taus)),
             tau = rep(taus, 24), off = as.vector(off[[m]]))))
    # aci update after the day
    if (!anyNA(E[i, ])) {
      hit   <- rowMeans(sweep(off$aci, 2, E[i, ], ">"))
      level <- pmin(pmax(level + gamma_q * (taus - hit), 0.005), 0.995)
    }
  }
  if (all(sapply(out, is.null))) {
    return(tibble(date = as.Date(character()), method = character(), hour = integer(),
                  tau = numeric(), off = numeric()))
  }
  bind_rows(out)
}

pinball <- function(y, q, tau) {
  u <- y - q
  pmax(tau * u, (tau - 1) * u)
}

prob_below <- function(q, tau, x = 0) {
  if (x < min(q)) return(0)
  if (x > max(q)) return(1)
  approx(q, tau, xout = x, ties = mean)$y
}

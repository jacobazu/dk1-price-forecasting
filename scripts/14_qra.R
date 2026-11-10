# ranges from quantile regression averaging (nowotarski & weron 2015): each quantile of the price is a linear
# function of the three models' forecasts, fitted per hour on the last K days. against today's ranges, same days
# result in results/qra.csv
library(dplyr); library(tidyr); library(readr); library(lubridate); library(quantreg)
source("R/forecasts.R"); source("R/quantiles.R"); source("R/tests.R")

K <- as.integer(Sys.getenv("K", 120))
actual <- readRDS("data/processed/prices_dk1_hourly.rds") |>
  mutate(local = with_tz(time, "Europe/Copenhagen"), date = as_date(local), hour = hour(local)) |>
  group_by(date, hour) |> summarise(actual = mean(price), .groups = "drop")
fc <- read_csv("results/backtest_forecasts.csv", show_col_types = FALSE) |> add_ensembles() |>
  select(date, hour, bmtx, learx, xgbx, ensemble) |> inner_join(actual, by = c("date", "hour")) |>
  filter(complete.cases(pick(everything()))) |> mutate(e = actual - ensemble)
D <- sort(unique(fc$date))

qra <- bind_rows(lapply(D[D >= D[1] + K], \(d) bind_rows(lapply(0:23, \(h) {
  tr <- filter(fc, hour == h, date < d, date >= d - K)
  te <- filter(fc, hour == h, date == d)
  if (nrow(tr) < 0.8 * K || nrow(te) == 0) return(NULL)
  fit <- tryCatch(rq(actual ~ bmtx + learx + xgbx, tau = taus, data = tr), error = \(e) NULL)
  if (is.null(fit)) return(NULL)
  tibble(date = d, hour = h, tau = taus, q = sort(as.numeric(predict(fit, te))))
}))))

em <- err_matrix(fc)
scale <- vol_scale(actual, em$dates)
today <- quantile_offsets(em$dates, em$E, scale$s[match(em$dates, scale$date)]) |> filter(method %in% c("aci", "emp_s")) |>
  inner_join(fc |> select(date, hour, ensemble), by = c("date", "hour")) |> mutate(q = ensemble + off) |>
  select(date, hour, tau, q, method)
ranges <- bind_rows(today, qra |> mutate(method = "qra")) |> inner_join(fc |> select(date, hour, actual), by = c("date", "hour")) |>
  mutate(loss = pinball(actual, q, tau))
common <- ranges |> distinct(method, date) |> count(date) |> filter(n == 3) |> pull(date)
ranges <- filter(ranges, date %in% common)

day <- ranges |> filter(tau %in% c(0.05, 0.25, 0.75, 0.95)) |> select(method, date, hour, actual, tau, q) |>
  pivot_wider(names_from = tau, values_from = q, names_prefix = "q") |> group_by(method, date) |>
  summarise(in90 = mean(actual >= q0.05 & actual <= q0.95), in50 = mean(actual >= q0.25 & actual <= q0.75),
            w90 = mean(q0.95 - q0.05), .groups = "drop") |>
  inner_join(ranges |> group_by(method, date) |> summarise(pinball = mean(loss), .groups = "drop"), by = c("method", "date"))
res <- day |> group_by(method) |> summarise(days = n(), coverage90 = mean(in90), coverage50 = mean(in50), width90 = mean(w90),
                                            pinball = mean(pinball), .groups = "drop")
w <- day |> select(method, date, pinball) |> pivot_wider(names_from = method, values_from = pinball)
res$p_vs_aci <- sapply(res$method, \(m) if (m == "aci") NA else dm_test(w[[m]] - w$aci)[["p"]])
print(as.data.frame(res), digits = 3)
write_csv(res, "results/qra.csv")

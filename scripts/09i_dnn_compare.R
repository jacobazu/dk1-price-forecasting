# is the neural network worth a place in the ensemble? same days, gas in every model
# FC: the three models with gas (scripts/09f_fuel.R, ONLY=gas). DNN: folder from scripts/09h_dnn.R
library(dplyr); library(tidyr); library(readr)
source("R/tests.R")

fc  <- readRDS(Sys.getenv("FC", "data/processed/forecasts_gas.rds")) |> filter(variant == "fuel") |>
  select(date, hour, model, forecast, actual) |> pivot_wider(names_from = model, values_from = forecast)
dnn <- bind_rows(lapply(list.files(Sys.getenv("DNN", "data/processed/dnn"), full.names = TRUE), readRDS))
x <- inner_join(fc, dnn, by = c("date", "hour")) |>
  mutate(ens3 = (bmtx + learx + xgbx) / 3, ens4 = (bmtx + learx + xgbx + dnn) / 4)

day <- x |> pivot_longer(c(bmtx, learx, xgbx, dnn, ens3, ens4), names_to = "model", values_to = "f") |>
  group_by(model, date) |> summarise(ae = mean(abs(actual - f)), e = mean(actual - f), .groups = "drop")
res <- day |> group_by(model) |> summarise(days = n(), mae = mean(ae), bias = mean(e), .groups = "drop")
w <- day |> select(model, date, ae) |> pivot_wider(names_from = model, values_from = ae)
res$p_vs_ens3 <- sapply(res$model, \(m) if (m == "ens3") NA else dm_test(w[[m]] - w$ens3)[["p"]])
print(as.data.frame(arrange(res, mae)), digits = 3)
write_csv(arrange(res, mae), "results/dnn.csv")

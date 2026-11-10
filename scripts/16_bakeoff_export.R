# one table for the neural network comparison: prices and every input as published, untransformed, plus the
# forecasts the existing models (with gas) made for the backtest days. the python side transforms per window as R does
library(dplyr); library(tidyr); library(readr); library(lubridate)
source("R/lear.R"); source("R/exog.R"); source("R/inputs.R")

prices   <- readRDS("data/processed/prices_dk1_hourly.rds")
daily    <- build_daily(prices)
features <- make_features(daily)
exog <- make_exog(daily$date, readRDS("data/raw/exog_neighbours.rds"), readRDS("data/processed/holidays_dk.rds"))
exog$wx   <- make_weather(daily$date, readRDS("data/raw/weather_openmeteo.rds"))
exog$wxde <- make_weather(daily$date, read_csv("live/weather_de_live.csv", show_col_types = FALSE), "wxde")
exog$fuel <- make_fuel(daily$date, read_csv("live/gas_live.csv", show_col_types = FALSE))

P <- features$P; colnames(P) <- paste0("p_h", 0:23)
pre <- \(g) { x <- exog[[g]]; colnames(x) <- paste0(g, ":", colnames(x)); x }
tab <- bind_cols(tibble(date = daily$date, dow = features$dow), as_tibble(P),
                 as_tibble(do.call(cbind, lapply(c("nb", "hol", "wx", "wxde", "fuel"), pre))))
out <- Sys.getenv("OUT", "data/processed/bakeoff")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write_csv(tab, file.path(out, "daily.csv"))

fc <- readRDS(Sys.getenv("FC", "data/processed/forecasts_gas.rds")) |> filter(variant == "fuel") |>
  select(date, hour, model, forecast) |> pivot_wider(names_from = model, values_from = forecast)
dnn <- bind_rows(lapply(list.files(Sys.getenv("DNN", "data/processed/dnn"), full.names = TRUE), readRDS))
write_csv(left_join(fc, dnn, by = c("date", "hour")), file.path(out, "existing.csv"))

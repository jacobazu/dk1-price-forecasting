# forecast tomorrow, before noon


library(readr)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/xmodels.R")
source("R/forecasts.R")   # add_ensembles()
source("R/postprocess.R")
source("R/inputs.R")      # forecast_groups, see inputs.csv
has_xgb <- requireNamespace("xgboost", quietly = TRUE)
if (has_xgb) source("R/xgb.R")

prices <- readRDS("data/processed/prices_dk1_hourly.rds")

now_dk <- with_tz(Sys.time(), "Europe/Copenhagen")
today  <- as_date(now_dk)
target <- today + 1

# nothing saved after noon
too_late <- hour(now_dk) >= 12 && Sys.getenv("DK1_PREVIEW") != "true"

# empty row for tomorrow
daily <- build_daily(prices) |>
  filter(date <= target) |>
  bind_rows(tibble(date = target)) |>
  distinct(date, .keep_all = TRUE) |>
  complete(date = seq(min(date), target, by = "day")) |>
  arrange(date)

features <- make_features(daily)
d        <- which(daily$date == target)

# failed model gives NA
safe <- function(name, f) tryCatch(f(), error = \(e) {
  message(name, " failed: ", conditionMessage(e)); rep(NA_real_, 24)
})

# -x models with dk1 and german weather (german from 14 oct 2026)
exog <- make_exog(daily$date,
                  read_csv("live/neighbours_live.csv", show_col_types = FALSE),
                  dk_holidays(year(min(daily$date)):year(target)))
exog$wx   <- make_weather(daily$date, read_csv("live/weather_live.csv", show_col_types = FALSE))
exog$wxde <- make_weather(daily$date, read_csv("live/weather_de_live.csv", show_col_types = FALSE), "wxde")
# missing weather is left out, so there is still a forecast
groups  <- Filter(\(g) !g %in% c("wx", "wxde") || !anyNA(exog[[g]][d, ]), forecast_groups)
if (length(groups) < length(forecast_groups)) message("Weather missing for ", target, ", models run with: ", paste(groups, collapse = ", "))
bmtx  <- safe("BMT-X",  \() bmtx_forecast_day(features, exog, d, groups))
learx <- safe("LEAR-X", \() learx_forecast_day(features, exog, d, groups))

# settings from 09_xgb.R
xgb_day <- function(m, ex, features = get("features", globalenv())) {
  if (!has_xgb) stop("xgboost not installed")
  tun <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |>
    filter(model == m, chosen)
  xgb_forecast_day(features, d, exog = ex,
                   params = modifyList(xgb_params, list(max_depth = tun$max_depth)),
                   rounds = tun$rounds, seed = as.integer(target))
}
xgbx <- safe("XGBoost-X", \() xgb_day("xgbx", exog[groups]))
# challenger: ensemble + median error of the last 28 days, saved but not shown, decided 1 mar 2027
bias <- safe("Bias", \() {
  past <- all_forecasts("ensemble") |> transmute(date, hour, actual, forecast = ensemble)
  b <- bias_day(past, target)
  if (is.null(b)) stop("too few days for the bias") else b$bias
})[1]

# challenger: ensemble + 4.85 + 0.366 x (gas two days before - its 60 day average)
# both numbers fitted once on the backtest year (oct 2025 - oct 2026) and frozen; saved, not shown, decided 1 mar 2027
gas_adj <- safe("Gas", \() {
  g <- read_csv("live/gas_live.csv", col_types = cols(day = col_date(), gas = col_double())) |>
    filter(day <= target - 2, day > target - 2 - 60)
  if (nrow(g) < 40 || max(g$day) < target - 4) stop("gas prices missing")
  4.85 + 0.366 * (g$gas[g$day == max(g$day)] - mean(g$gas))
})[1]

# challenger from 12 nov 2026: the same three models with the gas price as an input (gas only, co2 added little),
# plus the 28 day median error of that ensemble once it has a month of live days. decided 1 mar 2027
exog$fuel <- make_fuel(daily$date, read_csv("live/gas_live.csv", col_types = cols(day = col_date(), gas = col_double())))
fuel_groups <- c(groups, "fuel")

# inputs for the python challengers (bakeoff/live.py), in the same layout as scripts/16_bakeoff_export.R. not committed
local({
  P <- features$P; colnames(P) <- paste0("p_h", 0:23)
  pre <- \(g) { x <- exog[[g]]; colnames(x) <- paste0(g, ":", colnames(x)); x }
  tab <- bind_cols(tibble(date = daily$date, dow = features$dow), as_tibble(P),
                   as_tibble(do.call(cbind, lapply(c("nb", "hol", "wx", "wxde", "fuel"), pre))))
  dir.create("tmp_py", showWarnings = FALSE)
  write_csv(tab, "tmp_py/daily.csv")
})
bmtx_f  <- safe("BMT-X gas",  \() bmtx_forecast_day(features, exog, d, fuel_groups))
learx_f <- safe("LEAR-X gas", \() learx_forecast_day(features, exog, d, fuel_groups))
xgbx_f  <- safe("XGBoost-X gas", \() xgb_day("xgbx", exog[fuel_groups]))
# challenger from 12 nov 2026: lear-x with gas, prices and inputs scaled over 4 years instead of 2, trained on the same days.
# chosen on the tuning days before any test day (jacobazu/dk1-model-comparison, windows/). decided 1 mar 2027
learx_f4y <- safe("LEAR-X gas 4y", \() learx_forecast_day(features, exog, d, fuel_groups, scale_window = 1460))
fuel_bias <- safe("Gas bias", \() {
  past <- all_forecasts("ensemble_fuel") |> transmute(date, hour, actual, forecast = ensemble_fuel) |> filter(!is.na(forecast))
  b <- bias_day(past, target)
  if (is.null(b)) stop("under a month of live days") else b$bias
})[1]

# challenger from 14 nov 2026: lear-x and xgboost-x on prices relative to the gas price (gas_relative in R/xmodels.R),
# bmt-x as it is. the live models run about 20 too low since prices and gas doubled. of the seven ways to switch
# one, two or all three models, this one had the lowest error on 405 days: 16.3 against the ensemble's 17.9, bias
# +3.8 against +7.0 (jacobazu/dk1-model-comparison, level/, rules first). early check 15 dec 2026, otherwise
# 1 mar 2027. bmt-x relative to gas is saved too
rel <- safe("Gas-relative inputs", \() gas_relative(features, exog))
rel_run <- function(name, f) safe(name, \() {
  if (!is.list(rel)) stop("no gas price")
  rel$gas[d] * f()
})
learx_rel <- rel_run("LEAR-X gas-relative", \() learx_forecast_day(rel$features, rel$exog, d, groups))
bmtx_rel  <- rel_run("BMT-X gas-relative",  \() bmtx_forecast_day(rel$features, rel$exog, d, groups))
xgbx_rel  <- rel_run("XGBoost-X gas-relative", \() xgb_day("xgbx", rel$exog[groups], rel$features))

naive_day <- features$P[d - 1, ]   # same hour yesterday
naive_mix <- if (features$dow[d] %in% c(1, 6, 7)) features$P[d - 7, ] else naive_day   # last week mon, sat, sun

new_forecast <- tibble(
  forecast_made_utc = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"),
  date              = target,
  hour              = 0:23,
  bmtx              = round(bmtx, 2),
  learx             = round(learx, 2),
  xgbx              = round(xgbx, 2),
  naive_day         = round(naive_day, 2),
  naive_mix         = round(naive_mix, 2)
) |>
  add_ensembles() |>   # NA if one failed
  mutate(learx_fuel_4y = round(learx_f4y, 2),
         ensemble_fuel = round((bmtx_f + learx_f + xgbx_f) / 3, 2),
         ensemble_fuel_4y = round((bmtx_f + learx_f4y + xgbx_f) / 3, 2),
         ensemble_fuel_bc = round(ensemble_fuel + fuel_bias, 2),
         learx_rel = round(learx_rel, 2), bmtx_rel = round(bmtx_rel, 2), xgbx_rel = round(xgbx_rel, 2),
         ensemble_gasrel = (bmtx + learx_rel + xgbx_rel) / 3,
         ensemble_bc = ensemble + bias,
         ensemble_gas = ensemble + gas_adj,
         across(starts_with("ensemble"), \(x) round(x, 2)))

# append, never overwrite
dir.create("forecasts", showWarnings = FALSE)
file <- "forecasts/forecasts_live.csv"

if (too_late) {
  message("After the noon auction (", format(now_dk, "%H:%M"), " danish time) - no forecast saved for ", target)
} else if (file.exists(file)) {
  # time stamp as text
  old <- read_csv(file, show_col_types = FALSE,
                  col_types = cols(forecast_made_utc = col_character(), date = col_date(),
                                   hour = col_integer(), .default = col_double()))
  if (target %in% as_date(old$date)) {
    message("Forecast for ", target, " already exists - not overwriting.")
  } else {
    write_csv(bind_rows(mutate(old, date = as_date(date)), new_forecast), file)
    message("Forecast for ", target, " added.")
  }
} else if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  # never recreate on github
  stop(file, " is missing - restore it from the git history instead of starting a new track record")
} else {
  write_csv(new_forecast, file)
  message("Forecast file created with forecast for ", target, ".")
}

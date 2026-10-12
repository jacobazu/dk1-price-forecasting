# forecast tomorrow, before noon


library(readr)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/xmodels.R")
source("R/forecasts.R")   # add_ensembles()
source("R/postprocess.R")
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

# -x models
exog <- make_exog(daily$date,
                  read_csv("live/neighbours_live.csv", show_col_types = FALSE),
                  NULL,
                  dk_holidays(year(min(daily$date)):year(target)))
exog$wx <- make_weather(daily$date, read_csv("live/weather_live.csv", show_col_types = FALSE))
groups  <- c("nb", "hol", "wx")
bmtx  <- safe("BMT-X",  \() bmtx_forecast_day(features, exog, d, groups))
learx <- safe("LEAR-X", \() learx_forecast_day(features, exog, d, groups))

# settings from 09_xgb.R
xgb_day <- function(m, ex) {
  if (!has_xgb) stop("xgboost not installed")
  tun <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |>
    filter(model == m, chosen)
  xgb_forecast_day(features, d, exog = ex,
                   params = modifyList(xgb_params, list(max_depth = tun$max_depth)),
                   rounds = tun$rounds, seed = as.integer(target))
}
xgbx <- safe("XGBoost-X", \() xgb_day("xgbx", exog[groups]))
# challengers, saved next to the forecast and decided on 1 march 2027 (see scripts/10_evaluate.R)
# with entso-e load forecasts, only when tomorrow's are out (published by 10:00)
load_ok <- file.exists("live/load_live.csv")
if (load_ok) {
  exog$load <- make_load(daily$date, read_csv("live/load_live.csv", show_col_types = FALSE))
  load_ok   <- !anyNA(exog$load[d, ])
}
groups_l <- c(groups, "load")
no_load  <- \() stop("no load forecasts for ", target)
bmtx_load  <- safe("BMT-X with load",     \() if (load_ok) bmtx_forecast_day(features, exog, d, groups_l) else no_load())
learx_load <- safe("LEAR-X with load",    \() if (load_ok) learx_forecast_day(features, exog, d, groups_l) else no_load())
xgbx_load  <- safe("XGBoost-X with load", \() if (load_ok) xgb_day("xgbx", exog[groups_l]) else no_load())

# bias-corrected: plus the median error of the ensemble over the 28 days before (R/postprocess.R)
bias <- safe("Bias", \() {
  past <- all_forecasts("ensemble") |> transmute(date, hour, actual, forecast = ensemble)
  b <- bias_day(past, target)
  if (is.null(b)) stop("too few days for the bias") else b$bias
})[1]

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
  naive_mix         = round(naive_mix, 2),
  bmtx_load         = round(bmtx_load, 2),
  learx_load        = round(learx_load, 2),
  xgbx_load         = round(xgbx_load, 2)
) |>
  add_ensembles() |>   # NA if one failed
  mutate(ensemble_bc = ensemble + bias,
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

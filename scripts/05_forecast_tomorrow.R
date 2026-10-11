# forecasting tomorrows DK1 prices
# must run before the noon auction on the day before delivery (danish time)


library(readr)
source("R/lear.R")
source("R/bmt.R")
source("R/exog.R")
source("R/xmodels.R")
has_xgb <- requireNamespace("xgboost", quietly = TRUE)
if (has_xgb) source("R/xgb.R")

prices <- readRDS("data/processed/prices_dk1_hourly.rds")

now_dk <- with_tz(Sys.time(), "Europe/Copenhagen")
today  <- as_date(now_dk)
target <- today + 1

# after noon the auction for tomorrow is closed, a forecast made now would not be a real forecast
# (preview_dashboard.R puts the file back afterwards, so it may write one)
too_late <- hour(now_dk) >= 12 && Sys.getenv("DK1_PREVIEW") != "true"

# daily price matrix - empty row or tomorrow if no prices yet
daily <- build_daily(prices) |>
  filter(date <= target) |>
  bind_rows(tibble(date = target)) |>
  distinct(date, .keep_all = TRUE) |>
  complete(date = seq(min(date), target, by = "day")) |>
  arrange(date)

features <- make_features(daily)
d        <- which(daily$date == target)

# a model that fails gives NA, the others are still saved
safe <- function(name, f) tryCatch(f(), error = \(e) {
  message(name, " failed: ", conditionMessage(e)); rep(NA_real_, 24)
})

# forecasts
lear <- lear_forecast_day(features, d)
bmt  <- safe("BMT", \() bmt_forecast_day(features, d))

# -X models with what is known before the auction: neighbour prices from today and holidays
# (no wind/solar, their day-ahead forecasts are published at 18:00)
exog <- make_exog(daily$date,
                  read_csv("live/neighbours_live.csv", show_col_types = FALSE),
                  NULL,
                  dk_holidays(year(min(daily$date)):year(target)))
bmtx  <- safe("BMT-X",  \() bmtx_forecast_day(features, exog, d))
learx <- safe("LEAR-X", \() learx_forecast_day(features, exog, d))

# xgboost with the depth and rounds chosen in 09_xgb.R
xgb_day <- function(m, ex) {
  if (!has_xgb) stop("xgboost not installed")
  tun <- read_csv("results/xgb_tuning.csv", show_col_types = FALSE) |>
    filter(model == m, chosen)
  xgb_forecast_day(features, d, exog = ex,
                   params = modifyList(xgb_params, list(max_depth = tun$max_depth)),
                   rounds = tun$rounds)
}
xgb  <- safe("XGBoost",   \() xgb_day("xgb", NULL))
xgbx <- safe("XGBoost-X", \() xgb_day("xgbx", exog[c("nb", "hol")]))
naive_day <- features$P[d - 1, ]   # same hour yesterday

new_forecast <- tibble(
  forecast_made_utc = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"),
  date              = target,
  hour              = 0:23,
  lear              = round(lear, 2),
  bmt               = round(bmt, 2),
  bmtx              = round(bmtx, 2),
  learx             = round(learx, 2),
  xgb               = round(xgb, 2),
  xgbx              = round(xgbx, 2),
  naive_day         = round(naive_day, 2)
)

# append to live file but never overwrite forecast
dir.create("forecasts", showWarnings = FALSE)
file <- "forecasts/forecasts_live.csv"

if (too_late) {
  message("After the noon auction (", format(now_dk, "%H:%M"), " danish time) - no forecast saved for ", target)
} else if (file.exists(file)) {
  # read the time stamp as text, so it matches the new rows when combined
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
  # the file holds the whole live track record, never start a new one on github
  stop(file, " is missing - restore it from the git history instead of starting a new track record")
} else {
  write_csv(new_forecast, file)
  message("Forecast file created with forecast for ", target, ".")
}

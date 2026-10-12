# entso-e transparency platform, token in ENTSOE_TOKEN (.Renviron locally, secret on github)

library(httr2)
library(dplyr)
library(lubridate)

entsoe_areas <- c(DK1 = "10YDK-1--------W", DE = "10Y1001A1001A82H")

# text of the first / every <name> tag
xml_tag  <- function(x, name) regmatches(x, regexpr(paste0("(?<=<", name, ">)[^<]*"), x, perl = TRUE))
xml_tags <- function(x, name) regmatches(x, gregexpr(paste0("(?<=<", name, ">)[^<]*"), x, perl = TRUE))[[1]]

entsoe_get <- function(params) {
  token <- Sys.getenv("ENTSOE_TOKEN")
  if (!nzchar(token)) stop("ENTSOE_TOKEN is not set")
  resp <- tryCatch(
    request("https://web-api.tp.entsoe.eu/api") |>
      req_url_query(securityToken = token, !!!params) |>
      req_retry(max_tries = 4, backoff = \(i) 10) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(),
    error = \(e) stop("ENTSO-E request failed (", class(e)[1], ")", call. = FALSE))   # no url, it has the token
  txt <- resp_body_string(resp)
  if (grepl("<Acknowledgement_MarketDocument", txt, fixed = TRUE)) {
    msg <- xml_tag(txt, "text")
    if (length(msg) && grepl("No matching data", msg)) return(NULL)
    stop("ENTSO-E: ", if (length(msg)) msg else paste("status", resp_status(resp)), call. = FALSE)
  }
  txt
}

# all points of a document. positions left out repeat the value before (curve type A03)
entsoe_points <- function(txt) {
  periods <- regmatches(txt, gregexpr("(?s)<Period>.*?</Period>", txt, perl = TRUE))[[1]]
  bind_rows(lapply(periods, \(p) {
    t0   <- as.POSIXct(xml_tag(p, "start"), format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
    t1   <- as.POSIXct(xml_tag(p, "end"), format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
    step <- c(PT15M = 15, PT30M = 30, PT60M = 60)[[xml_tag(p, "resolution")]]
    n    <- as.integer(difftime(t1, t0, units = "mins")) %/% step
    v    <- rep(NA_real_, n)
    v[as.integer(xml_tags(p, "position"))] <- as.numeric(xml_tags(p, "quantity"))
    idx  <- cummax(ifelse(is.na(v), 0L, seq_len(n)))
    tibble(time = t0 + (seq_len(n) - 1) * step * 60, value = ifelse(idx > 0, v[pmax(idx, 1)], NA_real_))
  }))
}

# day-ahead total load forecast (mw), hourly averages in utc. published by 10:00 the day before
get_entsoe_load <- function(area, from, to) {
  starts <- seq(as_date(from), as_date(to), by = "3 months")
  pts <- bind_rows(lapply(starts, \(s) {
    e <- min(s %m+% months(3), as_date(to) + 1)
    Sys.sleep(0.5)
    txt <- entsoe_get(list(documentType = "A65", processType = "A01",
                           outBiddingZone_Domain = entsoe_areas[[area]],
                           periodStart = format(as_datetime(s), "%Y%m%d%H%M"),
                           periodEnd   = format(as_datetime(e), "%Y%m%d%H%M")))
    if (is.null(txt)) NULL else entsoe_points(txt)
  }))
  if (nrow(pts) == 0) stop("ENTSO-E: no load forecasts for ", area, call. = FALSE)
  pts |>
    mutate(time = floor_date(time, "hour")) |>
    group_by(time) |>
    summarise(load = mean(value, na.rm = TRUE), .groups = "drop") |>
    filter(is.finite(load)) |>
    rename_with(\(x) paste0("load_", area), "load")
}

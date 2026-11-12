# the forecast page: data for forecast_view.js, which draws it. only the forecast and how uncertain it is

fv_blocks <- list(Night = 0:5, Morning = 6:11, Afternoon = 12:17, Evening = 18:23)

forecast_view <- function() {
  ms <- c(main, setdiff(setdiff(models, "naive_mix"), main))
  act <- actual |> filter(date == latest) |> arrange(hour)
  per_model <- lapply(setNames(ms, ms), \(m) {
    f  <- tomorrow |> arrange(hour) |> pull(all_of(m))
    fq <- fans[[m]]
    q  <- if (is.null(fq) || nrow(fq) == 0 || all(is.na(fq$q))) NULL else
      lapply(sort(unique(tomorrow$hour)), \(h) round(cummax(fq$q[fq$hour == h][order(fq$tau[fq$hour == h])]), 1))
    d  <- day_hits(m)
    # whole-day scenarios (hours move together): the day average and each part of the day, 5, 25, 75 and 95%
    S  <- scen[[m]]
    qs <- \(x) round(unname(quantile(x, c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE)), 1)
    day    <- if (is.null(S) || nrow(S) < 20) NULL else qs(rowMeans(S))
    blocks <- if (is.null(S) || nrow(S) < 20) NULL else unname(lapply(names(fv_blocks), \(b) {
      h <- fv_blocks[[b]] + 1
      list(name = b, from = min(fv_blocks[[b]]), to = max(fv_blocks[[b]]) + 1, f = round(mean(f[h]), 1), q = qs(rowMeans(S[, h, drop = FALSE])))
    }))
    # how wide the 90% range was on each of the last 60 days (half-width, mean over the hours), to set tomorrow against
    b  <- bands[[m]]
    wd <- if (is.null(b)) NULL else b |> filter(date >= latest - 60) |> group_by(date) |>
      summarise(w = mean(o0.95 - o0.05) / 2, .groups = "drop") |> filter(!is.na(w))
    list(label = model_labels[[m]], color = unname(model_colors[[m]]), mean = round(f, 1), q = q, day = day, blocks = blocks,
         widths = if (is.null(wd) || nrow(wd) < 10) NULL else round(wd$w, 1),
         days = if (is.null(d)) NULL else list(dates = format(d$date, "%d %b"), inside = d$inside, n = d$n))
  })
  per_model <- Filter(\(x) !anyNA(x$mean) && length(x$mean) == 24, per_model)
  bias <- Filter(\(k) grepl("^Bias", k$name), desk$checks)
  data <- list(day = trimws(format(latest, "%A %e %B")), made = format(made_dk, "%A %H:%M"), main = main,
               models = per_model, bias = if (length(bias)) bias[[1]]$x else NULL,
               actual = if (nrow(act) == 24 && !anyNA(act$actual)) round(act$actual, 1) else NULL,
               # nord pool publishes the day's prices about 13:00 danish time the day before
               publish = format(with_tz(as.POSIXct(paste(latest - 1, "13:00"), tz = "Europe/Copenhagen"), "UTC"), "%Y-%m-%dT%H:%M:%SZ"))
  paste0('<div id="fv"></div><script type="application/json" id="fv-data">',
         jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", digits = NA),
         '</script><script>', paste(readLines("forecast_view.js", warn = FALSE), collapse = "\n"), '</script>')
}

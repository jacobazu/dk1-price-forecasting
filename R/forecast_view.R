# the forecast page: data for forecast_view.js, which draws it

forecast_view <- function() {
  ms <- c(main, setdiff(setdiff(models, "naive_mix"), main))
  per_model <- lapply(setNames(ms, ms), \(m) {
    f  <- tomorrow |> arrange(hour) |> pull(all_of(m))
    fq <- fans[[m]]
    q  <- if (is.null(fq) || nrow(fq) == 0 || all(is.na(fq$q))) NULL else
      lapply(sort(unique(tomorrow$hour)), \(h) round(cummax(fq$q[fq$hour == h][order(fq$tau[fq$hour == h])]), 1))
    d  <- day_hits(m)
    list(label = model_labels[[m]], mean = round(f, 1), q = q,
         days = if (is.null(d)) NULL else list(dates = format(d$date, "%d %b"), inside = d$inside, n = d$n))
  })
  per_model <- Filter(\(x) !anyNA(x$mean) && length(x$mean) == 24, per_model)
  data <- list(day = trimws(format(latest, "%A %e %B")), made = format(made_dk, "%A %H:%M"), main = main, neg = neg_val,
               models = per_model, checks = desk$checks)
  paste0('<div id="fv"></div><script type="application/json" id="fv-data">',
         jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", digits = NA),
         '</script><script>', paste(readLines("forecast_view.js", warn = FALSE), collapse = "\n"), '</script>')
}

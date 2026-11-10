# shared by index.qmd and backtest.qmd

library(dplyr)
library(readr)
library(lubridate)
library(plotly)
source("R/battery.R")
source("R/postprocess.R")
source("R/quantiles.R")
source("R/forecasts.R")
source("R/inputs.R")
source("R/forecast_view.R")

# the build stops if the forecast uses anything but the inputs in inputs.csv
inputs_reg <- check_divider()

Sys.setlocale("LC_TIME", "C")   # english dates

orange <- "#2fa3b8"
grey   <- "#8fa6bd"
ink    <- "#e9f0f7"

# actual prices, dk time
actual <- readRDS("data/processed/prices_dk1_hourly.rds") |>
  mutate(local = with_tz(time, "Europe/Copenhagen"),
         date  = as_date(local),
         hour  = hour(local)) |>
  group_by(date, hour) |>
  summarise(actual = mean(price), .groups = "drop")

# live forecasts
fc <- read_csv("forecasts/forecasts_live.csv",
               col_types = cols(forecast_made_utc = col_character(), date = col_date(),
                                hour = col_integer(), .default = col_double()))

latest    <- max(fc$date)
tomorrow  <- fc |> filter(date == latest)
today_act <- actual |> filter(date == latest - 1)
week_act  <- actual |> filter(date == latest - 7)   # same weekday
made_utc  <- tomorrow$forecast_made_utc[1]
if (is.character(made_utc)) made_utc <- ymd_hm(made_utc, tz = "UTC")
made_dk   <- with_tz(made_utc, "Europe/Copenhagen")

# scored live days with a full forecast, all and last 30
scored_all <- fc |> filter(!is.na(ensemble)) |> inner_join(actual, by = c("date", "hour"))
scored     <- scored_all |> filter(date >= max(date) - 29)
n_live     <- n_distinct(scored_all$date)
window_lab <- if (n_live >= 30) "last 30 days" else sprintf("all %d live %s", n_live, if (n_live == 1) "day" else "days")

eur  <- function(x) format(round(x, 1), nsmall = 1)

hour_label <- function(h) sprintf("%02d:00", h)
has_scores <- nrow(scored) > 0
# main and naive on the same days
mae <- function(col) {
  cols <- intersect(unique(c(col, main, "naive_mix")), names(scored))
  if (!has_scores || !col %in% cols) return("\u2013")
  s <- scored[complete.cases(scored[, cols]), ]
  if (nrow(s) == 0) "\u2013" else eur(mean(abs(s$actual - s[[col]])))
}

# models, one button each
spot_labels <- c(ensemble  = "Ensemble",
                 bmtx      = "BMT-X",
                 learx     = "LEAR-X",
                 xgbx      = "XGBoost-X",
                 naive_mix = "Naive")
# older models, fallback only
old_labels  <- c(lear = "LEAR", bmt = "BMT", xgb = "XGBoost")
model_colors <- c(ensemble  = "#2fa3b8",
                  bmtx      = "#8f74e8",
                  learx     = "#d45c88",
                  xgbx      = "#c97d2c",
                  naive_mix = "#6f859b",
                  lear      = "#5b8fd6",
                  bmt       = "#a36a3f",
                  xgb       = "#65a30d")
has_spot <- any(sapply(setdiff(names(spot_labels), "naive_mix"),
                       \(m) m %in% names(tomorrow) && !all(is.na(tomorrow[[m]]))))
model_labels <- if (has_spot) spot_labels else c(spot_labels, old_labels)
models <- intersect(names(model_labels), names(fc))

# model buttons above a chart
# per_model = traces per model
model_buttons <- function(p, models, n_fixed = 1, per_model = 1) {
  vis <- c(lapply(models, \(m) c(rep(TRUE, n_fixed), rep(models == m, each = per_model))),
           list(c(rep(TRUE, n_fixed), rep(c(rep(FALSE, per_model - 1), TRUE), length(models)))))
  htmlwidgets::onRender(p, "
    function(el, x, data) {
      el.style.position = 'relative';
      var bar = document.createElement('div');
      bar.className = 'model-buttons';
      data.labels.forEach(function(lab, i) {
        var b = document.createElement('button');
        b.textContent = lab;
        if (i === 0) b.className = 'active';
        b.onclick = function() {
          Plotly.restyle(el, {visible: data.visible[i]});
          document.dispatchEvent(new CustomEvent('modelchange', {detail: data.keys[i]}));
          bar.querySelectorAll('button').forEach(function(o) { o.className = ''; });
          b.className = 'active';
        };
        bar.appendChild(b);
      });
      el.appendChild(bar);
    }",
    data = list(labels = c(unname(model_labels[models]), "All"), keys = c(models, "all"), visible = vis))
}

# plotly for r writes hoverinfo "skip" once per point, which leaves empty rows in the hover box.
# one value per trace keeps those traces out of it
skip_hover <- function(p) {
  p <- plotly_build(p)
  for (i in seq_along(p$x$data)) {
    h <- unique(na.omit(unlist(p$x$data[[i]]$hoverinfo)))
    if (length(h) == 1 && h == "skip") p$x$data[[i]]$hoverinfo <- "skip"
  }
  p
}

chart_layout <- function(p, models = NULL, xtitle = "", n_fixed = 1, per_model = 1) {
  p <- p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)",
           plot_bgcolor  = "rgba(0,0,0,0)",
           font = list(family = "Instrument Sans, Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey),
           xaxis = list(title = xtitle, showgrid = FALSE, zeroline = FALSE, color = grey,
                        linecolor = "rgba(255,255,255,0.15)"),
           yaxis = list(title = "", color = grey, gridcolor = "rgba(255,255,255,0.07)",
                        zerolinecolor = "rgba(255,255,255,0.28)"),
           legend = list(orientation = "h", x = 0, y = -0.15, font = list(color = ink)),
           hovermode = "x unified",
           hoverlabel = list(bgcolor = "#15324f", bordercolor = "#2b4a6b",
                             font = list(color = ink)),
           margin = list(t = 50)) |>
    config(displayModeBar = FALSE)
  if (length(models) > 0) p <- model_buttons(p, models, n_fixed, per_model)
  p
}

# all from results/*.csv
read_res <- function(f) if (file.exists(f)) read_csv(f, show_col_types = FALSE) else NULL
eval_acc   <- read_res("results/eval_accuracy.csv")
eval_tests <- read_res("results/eval_tests.csv")
eval_bias  <- read_res("results/eval_bias.csv")
eval_shape <- read_res("results/eval_shape.csv")
eval_post  <- read_res("results/eval_post.csv")
backtest   <- read_res("results/backtest_forecasts.csv")
if (!is.null(backtest)) backtest <- add_ensembles(backtest)
bat_daily  <- read_res("results/battery_daily.csv")
bat_sum    <- read_res("results/battery_summary.csv")
bat_tests  <- read_res("results/battery_tests.csv")
bat_setup  <- read_res("results/battery_setup.csv")
mcs_models <- read_res("results/mcs_models.csv")
xgb_tun    <- read_res("results/xgb_tuning.csv")
prob_sc    <- read_res("results/prob_scores.csv")
prob_cal   <- read_res("results/prob_calibration.csv")
prob_neg   <- read_res("results/prob_negative.csv")
chall      <- read_res("results/challengers.csv")
if (!is.null(chall)) for (v in c("backtest_mae_champion", "backtest_mae_challenger", "backtest_diff", "backtest_p",
                                 "live_mae_champion", "live_mae_challenger", "live_p_holm")) {
  if (!v %in% names(chall)) chall[[v]] <- NA_real_
}
has_eval    <- !is.null(eval_acc)
has_battery <- !is.null(bat_sum)
risk_tests <- read_res("results/battery_risk_tests.csv")
risk_daily <- read_res("results/battery_risk_daily.csv")
risk_next  <- read_res("results/battery_risk_next.csv")
has_risk   <- !is.null(risk_tests) && !is.null(risk_daily) && any(risk_tests$used)

long_labels <- c(perfect = "Perfect foresight",
                 ensemble = "Ensemble",
                 bmtx = "BMT-X", learx = "LEAR-X", xgbx = "XGBoost-X",
                 naive_mix = "Naive (weekly mix)", lear = "LEAR", bmt = "BMT", xgb = "XGBoost")
text_labels <- c(perfect = "perfect foresight", ensemble = "the ensemble",
                 bmtx = "BMT-X", learx = "LEAR-X", xgbx = "XGBoost-X", naive_mix = "the weekly naive",
                 lear = "LEAR", bmt = "BMT", xgb = "XGBoost")
strat_colors <- c(perfect = "rgba(255,255,255,0.28)", model_colors)

cap      <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
pct      <- function(x) paste0(format(round(100 * x, 1), nsmall = 1), "%")
eur_sign <- function(x) paste0(if (x >= 0) "+" else "\u2212", prettyNum(abs(round(x)), big.mark = ","))
p_txt    <- function(p) if (p < 0.001) "p < 0.001" else paste("p =", format(round(p, 3), nsmall = 3))
p_tab    <- function(p) ifelse(p < 0.001, "<0.001", format(round(p, 3), nsmall = 3))

# main model: lowest mae
real_models <- setdiff(models, "naive_mix")
avail <- real_models[sapply(real_models, \(m) !all(is.na(tomorrow[[m]])))]
if (length(avail) == 0) avail <- intersect("naive_mix", models)   # all models failed
main  <- if (has_eval) {
  ranked <- eval_acc$model[eval_acc$model %in% avail]
  if (length(ranked) > 0) ranked[1] else avail[1]
} else avail[1]
main_label <- model_labels[[main]]

# accuracy tile
best_all   <- if (has_eval) eval_acc$model[!grepl("^naive", eval_acc$model)][1] else NA
# fallback for older models
best_bt    <- if (has_eval && main %in% eval_acc$model) main else best_all
bt_change  <- if (!is.na(best_bt)) round(100 * (eval_acc$rmae[eval_acc$model == best_bt] - 1)) else NA
bt_improve <- if (is.na(bt_change)) "\u2013" else paste0(if (bt_change < 0) "\u2212" else "+", abs(bt_change), "%")
bt_label   <- if (is.na(best_bt)) "" else if (best_bt == main) cap(sub("^the ", "", text_labels[[best_bt]])) else
  paste(cap(sub("^the ", "", text_labels[[best_bt]])), "(from the next forecast)")
# method page model
eval_main  <- if (has_eval && main %in% eval_acc$model) main else best_all
live_txt   <- if (has_eval && !has_spot) {
  sprintf("The forecast shown for %s is from %s, an earlier model, because it was saved before the -X models ran live.",
          format(latest, "%a %d %b"), text_labels[[main]])
} else ""

find_test <- function(tab, a, b) {
  r <- tab[(tab$model_a == a & tab$model_b == b) | (tab$model_a == b & tab$model_b == a), ]
  if (nrow(r) == 0) return(NULL)
  if (r$model_a != a) {   # flip to a vs b
    for (col in intersect(c("mae_diff", "mean_eur_per_day", "dm_stat", "dm_sq_stat", "t"), names(r))) r[[col]] <- -r[[col]]
    if ("hours_a_better" %in% names(r)) r[c("hours_a_better", "hours_b_better")] <- r[c("hours_b_better", "hours_a_better")]
  }
  r
}

# accuracy text
acc_txt <- if (has_eval) {
  naive_best <- eval_acc$model[grepl("^naive", eval_acc$model)][1]
  real <- eval_acc |> filter(!grepl("^naive", model))
  imp  <- round(100 * (1 - real$mae[1] / eval_acc$mae[eval_acc$model == naive_best]))
  sprintf("%s cuts the average error by %d%% compared with the naive benchmark (%s).",
          long_labels[[real$model[1]]], imp, text_labels[[naive_best]])
} else ""

sig_txt <- if (has_eval && nrow(filter(eval_acc, !grepl("^naive", model))) >= 2) {
  top <- eval_acc$model[!grepl("^naive", eval_acc$model)][1:2]
  r   <- find_test(eval_tests, top[1], top[2])
  if (is.null(r)) {
    ""
  } else if (r$dm_p < 0.05 && r$gw_p < 0.05) {
    sprintf("%s is significantly more accurate than %s (DM %s, GW %s), significantly better in %d and worse in %d of the 24 hours.",
            long_labels[[top[1]]], long_labels[[top[2]]], p_txt(r$dm_p), p_txt(r$gw_p),
            r$hours_a_better, r$hours_b_better)
  } else {
    sprintf("%s has the lower MAE, but the difference to %s is not clearly significant (DM %s, GW %s).",
            long_labels[[top[1]]], long_labels[[top[2]]], p_txt(r$dm_p), p_txt(r$gw_p))
  }
} else ""

# bias text
post_txt <- if (!is.null(eval_post) && eval_main %in% eval_post$model) {
  r <- eval_post[eval_post$model == eval_main, ]
  n_sig <- sum(eval_post$dm_p < 0.05 & eval_post$mae_corr < eval_post$mae_raw)
  med_txt <- sprintf(paste("Bias correction (median error, last %d days): mean error %+.1f to %+.1f, MAE %.1f to %.1f (DM %s),",
                           "significant for %d of %d models. It runs live as a challenger."),
                     k_bias, r$mean_error_raw, r$mean_error_corr, r$mae_raw, r$mae_corr, p_txt(r$dm_p), n_sig, nrow(eval_post))
  mz_txt <- if ("mae_mz" %in% names(r)) {
    sprintf("a + b × forecast instead: MAE %.1f, %s (DM %s).",
            r$mae_mz, if (r$mae_mz < r$mae_corr) "better" else "worse", p_txt(r$dm_p_mz_med))
  } else ""
  paste(med_txt, mz_txt)
} else ""

xgb_txt <- if (!is.null(xgb_tun) && any(xgb_tun$chosen)) {
  ch <- xgb_tun[xgb_tun$chosen, ]
  set_txt <- function(m) { r <- ch[ch$model == m, ]; sprintf("depth %d and %s rounds", r$max_depth, r$rounds) }
  sprintf("Chosen: %s.", set_txt("xgbx"))
} else ""

eval_period <- if (has_eval) {
  n_live <- if ("n_live_days" %in% names(eval_acc)) eval_acc$n_live_days[1] else 0
  sprintf("%s to %s, %d days (%s), %s hourly forecasts",
          format(eval_acc$from[1], "%d %b %Y"), format(eval_acc$to[1], "%d %b %Y"), eval_acc$n_days[1],
          if (n_live > 0) sprintf("%d backtest days, then %d live %s", eval_acc$n_backtest_days[1], n_live,
                                  if (n_live == 1) "day" else "days")
          else "backtest only, live days are added once every model has a saved forecast",
          prettyNum(eval_acc$n_hours[1], big.mark = ","))
} else ""

# battery
bat_val <- function(strategy, col) bat_sum[[col]][bat_sum$strategy == strategy]
bat_set <- function(name) bat_setup$value[bat_setup$name == name]
bat_best <- if (has_battery) bat_sum$strategy[!bat_sum$strategy %in% c("perfect", "naive_mix")][1] else NA
bat_best_label <- if (has_battery) cap(sub("^the ", "", text_labels[[bat_best]])) else ""
bat_share <- if (has_battery) pct(bat_val(bat_best, "share_of_perfect")) else "\u2013"
bat_extra <- if (has_battery) eur_sign(bat_val(bat_best, "eur_per_mw_year") - bat_val("naive_mix", "eur_per_mw_year")) else "\u2013"
bat_tst   <- if (has_battery) find_test(bat_tests, bat_best, "naive_mix") else NULL
bat_p     <- if (is.null(bat_tst)) "\u2013" else p_txt(bat_tst$p_value)
bat_days  <- if (has_battery) nrow(bat_daily) else 0
bat_live  <- if (has_battery && "source" %in% names(bat_daily)) sum(bat_daily$source == "live") else 0
bat_period <- if (bat_live > 0) sprintf("%d backtest days, then %d live %s", bat_days - bat_live, bat_live,
                                        if (bat_live == 1) "day" else "days") else sprintf("%d backtest days", bat_days)

# battery text
bat_cmp <- function(a, b) {
  r <- find_test(bat_tests, a, b)
  if (is.null(r)) return("")
  if (r$p_value < 0.05) {
    sprintf("%s earns significantly %s than %s (%s).", text_labels[[a]],
            if (r$mean_eur_per_day > 0) "more" else "less", text_labels[[b]], p_txt(r$p_value))
  } else {
    sprintf("%s and %s are not significantly different in profit (%s).",
            text_labels[[a]], text_labels[[b]], p_txt(r$p_value))
  }
}
# money vs accuracy
bat_rank <- function(a, b) {
  more_acc   <- bat_val(a, "mae") < bat_val(b, "mae")
  more_money <- bat_val(a, "total_eur") > bat_val(b, "total_eur")
  la <- text_labels[[a]]; lb <- text_labels[[b]]
  if (more_acc == more_money) {
    w <- if (more_acc) la else lb
    sprintf("%s is both more accurate and earns more.", w)
  } else {
    acc <- if (more_acc) la else lb
    mon <- if (more_money) la else lb
    sprintf("%s has the lower MAE, but %s earns more, since a battery only needs the shape of the day, not the level.", acc, mon)
  }
}
bat_txt <- if (has_battery) {
  real <- bat_sum$strategy[!bat_sum$strategy %in% c("perfect", "naive_mix")]   # by profit
  parts <- bat_cmp(bat_best, "naive_mix")
  if (length(real) >= 2) parts <- c(parts, bat_cmp(real[1], real[2]), bat_rank(real[1], real[2]))
  parts <- parts[parts != ""]
  paste(paste0(toupper(substring(parts, 1, 1)), substring(parts, 2)), collapse = " ")
} else ""

# revenue at risk
eur      <- function(x) paste0(ifelse(x < 0, "\u2212", ""), prettyNum(abs(round(x)), big.mark = ","))
low1     <- function(x) paste0(tolower(substring(x, 1, 1)), substring(x, 2))
risk_u   <- if (has_risk) risk_tests[risk_tests$used, ][1, ] else NULL
risk_nx  <- if (has_risk && !is.null(risk_next)) risk_next[risk_next$method == risk_u$method, ] else NULL
if (!is.null(risk_nx) && nrow(risk_nx) != 1) risk_nx <- NULL
risk_day <- if (is.null(risk_nx)) "" else format(as_date(risk_nx$date), "%a %d %b")
risk_exp <- if (is.null(risk_nx)) "\u2013" else eur(risk_nx$expected)
risk_var <- if (is.null(risk_nx)) "\u2013" else eur(risk_nx$var)
risk_es  <- if (is.null(risk_nx)) "\u2013" else eur(risk_nx$es)
risk_fl  <- if (is.null(risk_nx)) "" else sprintf("1 day in 20 earns less than %s \u20ac", eur(risk_nx$floor))
risk_hit <- if (has_risk) pct(risk_u$miss_rate) else "\u2013"
risk_hit_sub <- if (has_risk) sprintf("target 5%%, %d of %d days, Kupiec %s", risk_u$misses, risk_u$days,
                                      p_txt(risk_u$kupiec_p)) else ""
risk_txt <- if (has_risk) {
  u <- risk_u
  if (u$calibrated) {
    sprintf(paste("Used: %s. Below the floor on %s of %d days (Kupiec %s). The misses don't cluster (Christoffersen %s)",
                  "and aren't worse than the ES says (Acerbi-Székely %s)."),
            low1(u$label), pct(u$miss_rate), u$days, p_txt(u$kupiec_p), p_txt(u$christoffersen_p), p_txt(u$z2_p))
  } else {
    sprintf("No method passes all three tests, so %s is shown, with the lowest FZ0 score.", low1(u$label))
  }
} else ""

# ranges for tomorrow
q_method <- function(m) {
  r <- if (!is.null(prob_sc)) prob_sc[prob_sc$model == m & prob_sc$chosen, ] else NULL
  if (is.null(r) || nrow(r) == 0) "aci" else r$method[1]
}
all_prices <- bind_rows(actual, if (!is.null(backtest)) select(backtest, date, hour, actual))
past_fc <- function(m) {
  bt <- if (!is.null(backtest) && m %in% names(backtest)) select(backtest, date, hour, actual, f = all_of(m)) else NULL
  lv <- if (m %in% names(fc)) fc |> filter(date < latest, !is.na(.data[[m]])) |>
    select(date, hour, f = all_of(m)) |> inner_join(actual, by = c("date", "hour")) else NULL
  bind_rows(lv, bt) |> distinct(date, hour, .keep_all = TRUE)
}
bands <- list()   # each past day's 5% and 95% offsets, for the misses on the track record
scen  <- list()   # tomorrow's whole-day scenarios per model
fans <- lapply(setNames(models, models), \(m) {
  if (m == "naive_mix" || !m %in% names(tomorrow) || all(is.na(tomorrow[[m]]))) return(NULL)
  x  <- bind_rows(transmute(past_fc(m), date, hour, e = actual - f),
                  tibble(date = latest, hour = 0:23, e = NA_real_))
  em <- err_matrix(x)
  q_all <- quantile_offsets(em$dates, em$E, vol_scale(all_prices, em$dates)$s) |> filter(method == q_method(m))
  bands[[m]] <<- q_all |> filter(tau %in% c(0.05, 0.95), date < latest) |>
    tidyr::pivot_wider(id_cols = c(date, hour), names_from = tau, values_from = off, names_prefix = "o")
  q  <- q_all |> filter(date == latest)
  if (nrow(q) == 0) return(NULL)
  # whole-day scenarios: tomorrow's forecast plus each of the last k_q days' full error curve, scaled to this
  # week's volatility (the same errors the range is built from). hours stay together, as they do in reality
  vs   <- vol_scale(all_prices, em$dates)$s
  past <- em$dates >= latest - k_q & em$dates < latest & complete.cases(em$E) & !is.na(vs)
  fm   <- tomorrow[[m]][match(0:23, tomorrow$hour)]
  if (any(past) && !is.na(vs[em$dates == latest])) {
    scen[[m]] <<- sweep(vs[em$dates == latest] * em$E[past, , drop = FALSE] / vs[past], 2, fm, "+")
  }
  tomorrow |> select(hour, f = all_of(m)) |> inner_join(q, by = "hour") |> mutate(q = f + off)
})

# scenario lines for the chart, one trace: each day separated by a gap
scen_xy <- function(m) {
  S <- scen[[m]]
  if (is.null(S)) return(tibble(x = NA_real_, y = NA_real_))
  tibble(x = rep(c(0:23, NA), nrow(S)), y = as.vector(t(cbind(S, NA))))
}

# how often the price really stayed inside the 90% range, last 30 scored days
trust <- function(m) {
  if (is.null(bands[[m]])) return(NULL)
  b <- scored_all |> select(date, hour, actual, f = all_of(m)) |> filter(date >= latest - 30) |>
    inner_join(bands[[m]], by = c("date", "hour")) |> filter(!is.na(o0.05), !is.na(actual))
  if (nrow(b) < 24 * 7) return(NULL)
  list(share = mean(b$actual >= b$f + b$o0.05 & b$actual <= b$f + b$o0.95), days = n_distinct(b$date))
}
fan_wide <- function(m) {
  out <- tibble(hour = 0:23, q0.05 = NA_real_, q0.25 = NA_real_, q0.75 = NA_real_, q0.95 = NA_real_)
  if (is.null(fans[[m]])) return(out)
  fans[[m]] |>
    filter(tau %in% c(0.05, 0.25, 0.75, 0.95)) |>
    select(hour, tau, q) |>
    tidyr::pivot_wider(names_from = tau, values_from = q, names_prefix = "q")
}

# real hours tomorrow (dst)
cph     <- "Europe/Copenhagen"
n_hours <- as.numeric(difftime(force_tz(as_datetime(latest + 1), cph), force_tz(as_datetime(latest), cph), units = "hours"))
tile_fc <- if (n_hours == 23) filter(tomorrow, hour != 2) else tomorrow
tile_fc <- filter(tile_fc, !is.na(.data[[main]]))

# chance of a negative price
neg <- if (!is.null(fans[[main]])) {
  fans[[main]] |>
    filter(!is.na(q), hour %in% tile_fc$hour) |>
    group_by(hour) |>
    summarise(p = prob_below(q[order(q)], tau[order(q)]), .groups = "drop")
} else NULL
if (!is.null(neg) && nrow(neg) == 0) neg <- NULL
neg_pct <- function(p) if (p <= 0) "<5%" else if (p >= 1) ">95%" else paste0(round(100 * p), "%")
neg_val <- if (is.null(neg)) "\u2013" else neg_pct(max(neg$p))
neg_sub <- if (is.null(neg)) "no range yet" else if (max(neg$p) <= 0) "below 5% in every hour" else
  sprintf("highest hourly chance, at %s", hour_label(neg$hour[which.max(neg$p)]))

# tomorrow's risk in plain words, and how yesterday went
fw <- fan_wide(main) |> filter(hour %in% tile_fc$hour)
# risk changes from hour to hour, so the line names the most uncertain hour rather than labelling the day.
# one line per model; the model buttons on the forecast chart switch between them
# each hour's spread of outcomes as a sideways density, smoothed from its 5%, 10%, ..., 95% quantiles.
# each hour gets the same width, so a tall shape is an uncertain hour
violin_xy <- function(m, width = 0.36) {
  if (is.null(fans[[m]])) return(tibble(x = NA_real_, y = NA_real_))
  bind_rows(lapply(split(fans[[m]], fans[[m]]$hour), \(x) {
    x <- x[order(x$tau), ]
    draws <- approx(x$tau, cummax(x$q), xout = seq(0.05, 0.95, length.out = 300))$y
    d <- density(draws, adjust = 1.6, from = min(draws), to = max(draws), n = 80)
    w <- d$y / max(d$y) * width
    h <- x$hour[1]
    tibble(x = c(h + w, rev(h - w), NA), y = c(d$x, rev(d$x), NA))
  }))
}

# step band between two quantiles, as one closed shape (hour h covers h-0.5 to h+0.5)
band_xy <- function(m, lo, hi) {
  f <- fan_wide(m)
  if (all(is.na(f[[lo]]))) return(tibble(x = NA_real_, y = NA_real_))
  xs <- as.vector(rbind(f$hour - 0.5, f$hour + 0.5))
  tibble(x = c(xs, rev(xs), NA), y = c(rep(f[[hi]], each = 2), rev(rep(f[[lo]], each = 2)), NA))
}

# the hours worth a closer look: cheapest, most expensive and most uncertain, each with its own colour
key_hours <- function(m) {
  f <- tomorrow |> select(hour, f = all_of(m)) |> left_join(fan_wide(m), by = "hour") |> filter(!is.na(f))
  if (nrow(f) == 0 || all(is.na(f$q0.05))) return(NULL)
  k <- tibble(hour = c(f$hour[which.min(f$f)], f$hour[which.max(f$f)], f$hour[which.max(f$q0.95 - f$q0.05)]),
              what = c("Cheapest", "Most expensive", "Most uncertain"),
              col  = c("#d45c88", "#2fa3b8", "#8f74e8"))
  # drop a key hour that sits within two hours of one already kept, so the shapes don't overlap
  keep <- logical(nrow(k))
  for (i in seq_len(nrow(k))) keep[i] <- all(abs(k$hour[i] - k$hour[keep]) > 2)
  left_join(k[keep, ], f, by = "hour")
}

# one-sided distribution lying on its side to the right of the hour, like the battery's plan chart
side_xy <- function(m, h, width = 2.2) {
  x <- fans[[m]] |> filter(hour == h) |> arrange(tau)
  draws <- approx(x$tau, cummax(x$q), xout = seq(0.05, 0.95, length.out = 300))$y
  d <- density(draws, adjust = 1.6, from = min(draws), to = max(draws), n = 80)
  w <- d$y / max(d$y) * width
  tibble(x = c(h + 0.5, h + 0.5 + w, h + 0.5, NA), y = c(d$x[1], d$x, d$x[80], NA))
}

line_models <- c(main, setdiff(setdiff(models, "naive_mix"), main))

# each of the last 30 days: how many hours landed inside the 90% range, above it, below it
day_hits <- function(m) {
  if (is.null(bands[[m]])) return(NULL)
  b <- scored_all |> select(date, hour, actual, f = all_of(m)) |> filter(date >= latest - 30) |>
    inner_join(bands[[m]], by = c("date", "hour")) |> filter(!is.na(o0.05), !is.na(actual))
  if (nrow(b) == 0) return(NULL)
  b |> group_by(date) |> summarise(n = n(), inside = sum(actual >= f + o0.05 & actual <= f + o0.95),
                                   above = sum(actual > f + o0.95), .groups = "drop") |>
    mutate(below = n - inside - above)
}
hit_col <- function(s) ifelse(s >= 0.9, "#4fd08a", ifelse(s >= 0.75, "#f5c451", "#ff7a7a"))

# one square per day, coloured by the share of hours inside the range; a bar with a tick at the 90% promise
strip_svg <- function(m) {
  d <- day_hits(m)
  if (is.null(d)) return("<div class='kpi-sub'>no live days yet</div>")
  k <- nrow(d); w <- 9; g <- 2.5
  sq <- paste0(sprintf("<rect x='%.1f' y='0' width='%d' height='%d' rx='2' fill='%s'><title>%s: %d of %d hours inside%s</title></rect>",
                       (seq_len(k) - 1) * (w + g), w, 16, hit_col(d$inside / d$n), format(d$date, "%a %d %b"), d$inside, d$n,
                       ifelse(d$above > 0, sprintf(", %d above", d$above), "")), collapse = "")
  share <- sum(d$inside) / sum(d$n); W <- k * (w + g) - g
  paste0(sprintf("<svg class='strip' viewBox='0 0 %.1f 30' width='100%%' preserveAspectRatio='xMinYMid meet'>", W), sq,
         sprintf("<rect x='0' y='23' width='%.1f' height='5' rx='2.5' fill='rgba(255,255,255,0.08)'/>", W),
         sprintf("<rect x='0' y='23' width='%.1f' height='5' rx='2.5' fill='%s'/>", W * share, hit_col(share)),
         sprintf("<rect x='%.1f' y='20' width='2' height='11' fill='#e9f0f7'><title>promise: 90%%</title></rect>", W * 0.9 - 1),
         "</svg>")
}
strip_val <- function(m) { d <- day_hits(m); if (is.null(d)) "–" else sprintf("%.0f%%", 100 * sum(d$inside) / sum(d$n)) }
strip_html <- paste0(vapply(line_models, \(m) sprintf(
  "<div class='risk-m' data-m='%s'%s><div class='kpi-value'>%s<span class='kpi-unit'>of hours · aim 90%%</span></div>%s</div>",
  m, if (m == main) "" else " style='display:none'", strip_val(m), strip_svg(m)), ""), collapse = "")

# range text and tables
q_lab   <- function(m) q_methods[[m]]
q_low   <- function(m) paste0(tolower(substring(q_lab(m), 1, 1)), substring(q_lab(m), 2))
prob_m  <- if (!is.null(prob_sc)) prob_sc[prob_sc$model == eval_main, ] else NULL
has_prob <- !is.null(prob_m) && nrow(prob_m) > 0
prob_txt <- if (has_prob) {
  ch  <- prob_m[prob_m$chosen, ]
  sprintf("Used: %s, the lowest pinball loss of the methods on target. 90%% range: %s of hours (%s), 50%% range: %s (%s), %d days.",
          q_low(ch$method), pct(ch$cov90), p_txt(ch$cov90_p), pct(ch$cov50), p_txt(ch$cov50_p), ch$days)
} else ""
neg_txt <- if (!is.null(prob_neg) && eval_main %in% prob_neg$model) {
  r <- prob_neg[prob_neg$model == eval_main, ]
  sprintf("Chance of a negative price: Brier score %.4f vs %.4f for the 28-day share, %s %s.",
          r$brier, r$brier_clim, pct(abs(r$skill)), if (r$skill >= 0) "better" else "worse")
} else ""
chall_txt <- if (!is.null(chall) && nrow(chall) > 0) {
  sprintf(paste("Saved every day next to the ensemble, but not shown. On %s a challenger replaces the ensemble only if its",
                "live MAE is lower with p < 0.05 (Diebold-Mariano, Newey-West, Holm for those started together).",
                "Rules set before each challenger's first live day."),
          sub("^0", "", format(as_date(chall$decision[1]), "%d %B %Y")))
} else ""
live_cov <- if (has_prob) prob_m[prob_m$chosen, ] else NULL
live_cov_lab <- if (has_prob) model_labels[[eval_main]] else main_label
live_cov_val <- if (!is.null(live_cov) && live_cov$days_live > 0) pct(live_cov$cov90_live) else "\u2013"
live_cov_sub <- if (!is.null(live_cov) && live_cov$days_live > 0)
  sprintf("of hours in the 90%% range, %d live %s", live_cov$days_live, if (live_cov$days_live == 1) "day" else "days") else
  "starts after the first forecast day"
rgba <- function(hex, a) { v <- grDevices::col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }

# forecast tiles
diff_today <- mean(tile_fc[[main]]) - mean(today_act$actual)
diff_html  <- if (is.na(diff_today)) "" else
  sprintf('<span class="%s">%s%s</span> vs %s',
          if (diff_today > 0) "kpi-up" else "kpi-down",
          if (diff_today > 0) "+" else "\u2212", eur(abs(diff_today)), format(latest - 1, "%a %d %b"))
cheap <- tile_fc[which.min(tile_fc[[main]]), ]
peak  <- tile_fc[which.max(tile_fc[[main]]), ]

# live battery profit
lp_def   <- battery_lp(if (has_battery) as.list(setNames(bat_setup$value, bat_setup$name)) else battery_setup)
live_days <- scored_all |> group_by(date) |> filter(n() == 24) |> ungroup()
live_bat <- if (nrow(live_days) > 0) {
  live_days |>
    arrange(date, hour) |>
    group_by(date) |>
    group_modify(\(x, k) {
      cols <- c("actual", models[sapply(models, \(m) !anyNA(x[[m]]))])
      tibble(strategy = cols, profit = sapply(cols, \(m) battery_profit(x[[m]], x$actual, lp_def)))
    }) |>
    ungroup() |>
    mutate(strategy = if_else(strategy == "actual", "perfect", strategy))
} else NULL
# days with a main model forecast
main_days  <- if (is.null(live_bat)) as.Date(character()) else live_bat$date[live_bat$strategy == main]
live_bat_days <- length(main_days)
day_s      <- function(n) if (n == 1) "day" else "days"
live_share <- "\u2013"
live_sub   <- "starts after the first forecast day"
if (live_bat_days > 0) {
  x <- live_bat |> filter(date %in% main_days) |> group_by(strategy) |> summarise(profit = sum(profit))
  live_share <- pct(x$profit[x$strategy == main] / x$profit[x$strategy == "perfect"])
  live_sub   <- sprintf("%d %s with a saved forecast", live_bat_days, day_s(live_bat_days))
}

bat_desc <- if (has_battery) sprintf("%g MWh / %g MW battery, %g%% round trip efficiency, max %g full cycle per day",
                                     bat_set("cap_mwh"), bat_set("power_mw"),
                                     100 * bat_set("eff_rt"), bat_set("max_cycle")) else ""

# live accuracy vs naive, all live days, same hours
live_acc <- if (nrow(scored_all) > 0 && all(c(main, "naive_mix") %in% names(scored_all))) {
  s <- scored_all[complete.cases(scored_all[, c(main, "naive_mix")]), ]
  if (nrow(s) == 0) NULL else list(change = round(100 * (mean(abs(s$actual - s[[main]])) / mean(abs(s$actual - s$naive_mix)) - 1)),
                                    days = n_distinct(s$date), from = min(s$date))
} else NULL
live_improve <- if (is.null(live_acc)) "–" else paste0(if (live_acc$change < 0) "−" else "+", abs(live_acc$change), "%")
live_improve_sub <- if (is.null(live_acc)) "starts after the first forecast day" else
  sprintf("%s, %d live %s since %s. Backtest: %s over %d days", main_label, live_acc$days,
          if (live_acc$days == 1) "day" else "days", format(live_acc$from, "%d %b"), bt_improve,
          eval_acc$n_backtest_days[1])

# live revenue at risk days
risk_live <- if (has_risk && "source" %in% names(risk_daily))
  risk_daily |> filter(method == risk_u$method, source == "live") |> arrange(desc(date)) else NULL


# banner when the page is more than 30 hours old
writeLines(sprintf('<meta name="built" content="%s">
<style>.stale { position: fixed; left: 0; right: 0; bottom: 0; z-index: 2000; background: #fef2f2; color: #991b1b;
  border-top: 1px solid #fecaca; font-size: 14px; padding: 10px 20px; text-align: center; }</style>
<script>
document.addEventListener("DOMContentLoaded", () => {
  const built = new Date(document.querySelector("meta[name=built]").content);
  if (Date.now() - built > 30 * 3600 * 1000) {
    const el = document.createElement("div");
    el.className = "stale";
    el.textContent = "Not updated since " + built.toLocaleString("en-GB", { timeZone: "Europe/Copenhagen",
      day: "numeric", month: "short", hour: "2-digit", minute: "2-digit" }) + " (Danish time). The daily update has not run.";
    document.body.appendChild(el);
  }
});
</script>', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")), "stale.html")

# summary for the desk page (jacobazu.github.io/desk), which pulls every project into one book
desk_30 <- scored_all |> filter(date >= latest - 30) |> transmute(actual, f = .data[[main]], naive = naive_mix)
desk_tr <- trust(main)
desk_top <- (fan_wide(main) |> filter(hour %in% tile_fc$hour) |> mutate(w = q0.95 - q0.05) |> arrange(desc(w)))[1, ]
desk_bias <- mean(desk_30$actual - desk_30$f)
desk_skill <- 1 - mean(abs(desk_30$actual - desk_30$f)) / mean(abs(desk_30$actual - desk_30$naive), na.rm = TRUE)
desk <- list(
  name = "Day-ahead price forecast", url = "https://jacobazu.github.io/dk1-price-forecasting/",
  updated = format(Sys.time(), "%Y-%m-%dT%H:%MZ", tz = "UTC"),
  status = list(ok = latest >= as_date(with_tz(Sys.time(), "Europe/Copenhagen")), text = paste("Forecast for", format(latest, "%a %d %b"), "made", format(made_dk, "%a %H:%M"))),
  forecast = list(date = format(latest), mean = round(mean(tile_fc[[main]])),
                  uncertain_hour = if (is.na(desk_top$w)) NULL else sprintf("%02d:00", desk_top$hour),
                  lo = if (is.na(desk_top$w)) NULL else round(desk_top$q0.05), hi = if (is.na(desk_top$w)) NULL else round(desk_top$q0.95)),
  curve = { f <- tomorrow |> select(hour, f = all_of(main)) |> left_join(fan_wide(main), by = "hour")
            list(hour = f$hour, mean = round(f$f), lo = round(f$q0.05), hi = round(f$q0.95)) },
  days = { d <- day_hits(main); if (is.null(d)) NULL else list(dates = format(d$date), inside = d$inside, n = d$n) },
  tomorrow = NULL, pnl = NULL,
  # x on a scale from lo to hi, aim marks the promise; the desk page draws each as a bar
  checks = list(
    if (!is.null(desk_tr)) list(name = "Inside the 90% range", value = sprintf("%.0f%%", 100 * desk_tr$share),
         x = round(100 * desk_tr$share, 1), aim = 90, lo = 50, hi = 100,
         light = if (abs(desk_tr$share - 0.9) <= 0.03) "green" else if (abs(desk_tr$share - 0.9) <= 0.06) "amber" else "red"),
    list(name = "Bias, €/MWh", value = sprintf("%+.0f", desk_bias), x = round(desk_bias, 1), aim = 0, lo = -40, hi = 40,
         light = if (abs(desk_bias) <= 5) "green" else if (abs(desk_bias) <= 12) "amber" else "red"),
    list(name = "Error cut vs naive", value = sprintf("%.0f%%", 100 * desk_skill), x = round(100 * desk_skill, 1), aim = 51, lo = 0, hi = 100,
         light = if (desk_skill >= 0.4) "green" else if (desk_skill >= 0.2) "amber" else "red")))
desk$checks <- Filter(Negate(is.null), desk$checks)
jsonlite::write_json(desk, "desk.json", auto_unbox = TRUE, null = "null")

# --- neighbours: dk1's six cables and the prices around it -------------------------------------------------
zones <- tibble(
  id   = c("DK1", "NO", "SE", "DK2", "DE", "NL", "GB"),
  name = c("DK1", "Norway", "Sweden", "East Denmark", "Germany", "Netherlands", "Great Britain"),
  x    = c(0, -0.2, 1.05, 1.25, 0.25, -1.05, -1.3),
  y    = c(0, 1.1, 0.85, -0.05, -1.1, -0.85, 0.25),
  pcol = c(NA, "price_NO2", "price_SE3", "price_DK2", "price_DE", NA, NA))
flows <- if (file.exists("live/flows_live.csv")) read_csv("live/flows_live.csv", show_col_types = FALSE) else NULL
nb_px <- if (file.exists("live/neighbours_live.csv")) read_csv("live/neighbours_live.csv", show_col_types = FALSE) else NULL
has_nb <- !is.null(flows) && nrow(flows) >= 24 && !is.null(nb_px)
if (has_nb) {
  cables  <- setdiff(names(flows), "time")
  cap     <- sapply(flows[cables], \(v) max(abs(v), na.rm = TRUE))   # the most each cable has carried in the file
  nb_time <- max(flows$time)   # the newest hour with flows
  nb_now  <- flows[flows$time == nb_time, ]
  nb_lab  <- format(with_tz(nb_time, "Europe/Copenhagen"), "%a %d %b %H:00")
  dk1_px  <- actual |> mutate(time = with_tz(force_tz(as_datetime(date) + hours(hour), "Europe/Copenhagen"), "UTC"))
  px_at   <- function(t) {
    n <- nb_px[nb_px$time == t, ]
    c(DK1 = dk1_px$actual[match(t, dk1_px$time)],
      sapply(setNames(zones$pcol[-1], zones$id[-1]), \(col) if (is.na(col) || nrow(n) == 0) NA_real_ else n[[col]][1]))
  }
  nb_price <- px_at(nb_time)
}

neighbour_map <- function() {
  z <- zones |> mutate(price = nb_price[id])
  ramp <- grDevices::colorRamp(c("#173a55", "#2fa3b8", "#62d6e8"))
  rng  <- range(z$price, na.rm = TRUE)
  fill <- rep("#22384f", nrow(z))
  ok <- !is.na(z$price)
  if (any(ok)) {
    v <- ramp(pmin(pmax((z$price[ok] - rng[1]) / max(diff(rng), 1), 0), 1))
    fill[ok] <- grDevices::rgb(v[, 1], v[, 2], v[, 3], maxColorValue = 255)
  }
  p <- plot_ly()
  ann <- list()
  for (cb in cables) {
    f <- nb_now[[cb]]; u <- abs(f) / cap[[cb]]; zz <- z[z$id == cb, ]
    full <- u >= 0.95
    p <- p |> add_trace(x = c(0.16, 0.86) * zz$x, y = c(0.16, 0.86) * zz$y, type = "scatter", mode = "lines", hoverinfo = "skip", showlegend = FALSE,
                        line = list(color = if (full) ink else "#4a6079", width = 2 + 9 * u))
    # arrow: into dk1 when the flow is positive
    from <- if (f >= 0) c(zz$x, zz$y) else c(0, 0); to <- if (f >= 0) c(0, 0) else c(zz$x, zz$y)
    a <- from + 0.2 * (to - from); b <- from + 0.36 * (to - from)
    ann[[length(ann) + 1]] <- list(x = b[1], y = b[2], ax = a[1], ay = a[2], xref = "x", yref = "y", axref = "x", ayref = "y",
                                   showarrow = TRUE, arrowhead = 2, arrowsize = 1.1, arrowwidth = 3,
                                   arrowcolor = "#2fa3b8", text = "")
    ann[[length(ann) + 1]] <- list(x = zz$x * 0.6, y = zz$y * 0.6, xref = "x", yref = "y", showarrow = FALSE,
                                   text = sprintf("<b>%s MW %s</b><br>%.0f%% full", format(abs(round(f)), big.mark = ","),
                                                  if (f >= 0) "in" else "out", 100 * u),
                                   font = list(size = 11, color = ink), bgcolor = "rgba(12,28,48,0.85)", borderpad = 2)
  }
  p |>
    add_trace(data = z, x = ~x, y = ~y, type = "scatter", mode = "markers", showlegend = FALSE,
              marker = list(size = ifelse(z$id == "DK1", 64, 50), color = fill, opacity = 1, line = list(color = "#0c1c30", width = 3)),
              text = ~ifelse(is.na(price), paste0(name, ": no price in the data"), sprintf("%s: %.0f €/MWh", name, price)),
              hovertemplate = "%{text}<extra></extra>") |>
    # labels on the outer side, away from the cables
    add_text(data = z |> filter(y <= 0.5), x = ~x, y = ~y - ifelse(id == "DK1", 0.21, 0.18), showlegend = FALSE, hoverinfo = "skip",
             text = ~ifelse(is.na(price), paste0("<b>", name, "</b>"), sprintf("<b>%s</b><br>%.0f €/MWh", name, price)),
             textposition = "bottom center", textfont = list(color = ink, size = 12)) |>
    add_text(data = z |> filter(y > 0.5), x = ~x, y = ~y + 0.18, showlegend = FALSE, hoverinfo = "skip",
             text = ~ifelse(is.na(price), paste0("<b>", name, "</b>"), sprintf("<b>%s</b><br>%.0f €/MWh", name, price)),
             textposition = "top center", textfont = list(color = ink, size = 12)) |>
    layout(annotations = ann, paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Instrument Sans, Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey),
           xaxis = list(visible = FALSE, range = c(-1.75, 1.7)), yaxis = list(visible = FALSE, range = c(-1.65, 1.7), scaleanchor = "x"),
           margin = list(l = 0, r = 0, t = 0, b = 0), hoverlabel = list(bgcolor = "#15324f", font = list(color = ink))) |>
    config(displayModeBar = FALSE)
}

# each cable over the last days as a share of what it can carry: up = into dk1, down = out
cable_strips <- function(days = 3) {
  f <- flows |> filter(time > nb_time - days(days)) |> mutate(t = with_tz(time, "Europe/Copenhagen"))
  plots <- lapply(cables, \(cb) {
    u <- f[[cb]] / cap[[cb]]
    plot_ly(x = f$t, y = u, type = "scatter", mode = "lines", fill = "tozeroy", fillcolor = "rgba(98,214,232,0.15)",
            line = list(color = "#2fa3b8", width = 1.5), showlegend = FALSE,
            text = sprintf("%s MW %s", format(abs(round(f[[cb]])), big.mark = ","), ifelse(f[[cb]] >= 0, "into DK1", "out of DK1")),
            hovertemplate = "%{x|%a %H:00}: %{text}<extra></extra>") |>
      layout(yaxis = list(range = c(-1.1, 1.1), tickvals = c(-1, 0, 1), ticktext = c("out", "0", "in"), gridcolor = "rgba(255,255,255,0.07)",
                          zeroline = TRUE, zerolinecolor = "rgba(255,255,255,0.28)", color = grey),
             xaxis = list(showgrid = FALSE, color = grey, tickformat = "%a"),
             annotations = list(list(x = 0, y = 1.02, xref = "paper", yref = "paper", xanchor = "left", yanchor = "bottom", showarrow = FALSE,
                                     text = paste0("<b>", zones$name[zones$id == cb], "</b>"), font = list(color = ink, size = 12))))
  })
  subplot(plots, nrows = 3, shareX = TRUE, margin = c(0.05, 0.05, 0.07, 0.07)) |>
    layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", margin = list(t = 25, l = 30, r = 10, b = 20),
           font = list(family = "Instrument Sans, Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey)) |>
    config(displayModeBar = FALSE)
}

# dk1 against each neighbour, last 24 hours: the price gap a cable owner earns when the cable is full
gap_chart <- function() {
  hrs <- flows$time[flows$time > nb_time - hours(24)]
  g <- bind_rows(lapply(hrs, \(t) { p <- px_at(t); tibble(time = t, id = names(p)[-1], gap = p[["DK1"]] - p[-1]) })) |>
    filter(!is.na(gap)) |> group_by(id) |>
    summarise(mean_gap = mean(gap), apart = mean(abs(gap) > 1), .groups = "drop") |>
    left_join(zones, by = "id") |> arrange(mean_gap) |> mutate(name = factor(name, name))
  lim <- max(abs(g$mean_gap), 5) * 1.35
  plot_ly(g, y = ~name, x = ~mean_gap, type = "bar", orientation = "h", marker = list(color = "#2fa3b8", cornerradius = 4),
          text = ~sprintf("%+.0f € · apart %.0f%% of hours", mean_gap, 100 * apart), textposition = "outside", cliponaxis = FALSE,
          textfont = list(color = ink), hovertemplate = "DK1 minus %{y}: %{x:+.1f} €/MWh<extra></extra>") |>
    layout(xaxis = list(range = c(-lim, lim), zeroline = TRUE, zerolinecolor = ink, zerolinewidth = 1.5, ticksuffix = " €",
                        gridcolor = "rgba(255,255,255,0.07)", title = "", color = grey),
           yaxis = list(title = "", color = ink), bargap = 0.45, margin = list(l = 10, r = 10, t = 5, b = 25),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           font = list(family = "Instrument Sans, Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey)) |>
    config(displayModeBar = FALSE)
}

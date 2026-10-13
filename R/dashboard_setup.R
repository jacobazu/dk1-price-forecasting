# shared by index.qmd (live) and backtest.qmd: data, results and the numbers in the text

library(dplyr)
library(readr)
library(lubridate)
library(plotly)
source("R/battery.R")
source("R/postprocess.R")
source("R/quantiles.R")
source("R/forecasts.R")   # add_ensembles()

Sys.setlocale("LC_TIME", "C")   # english dates

orange <- "#f97316"
grey   <- "#6b7280"
ink    <- "#1f2328"

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

# scored live days, all and last 30
scored_all <- fc |> inner_join(actual, by = c("date", "hour"))
scored     <- scored_all |> filter(date >= max(date) - 29)

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
model_colors <- c(ensemble  = "#f97316",
                  bmtx      = "#7c3aed",
                  learx     = "#0ea5e9",
                  xgbx      = "#16a34a",
                  naive_mix = "#9ca3af",
                  lear      = "#c2410c",
                  bmt       = "#2563eb",
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
          bar.querySelectorAll('button').forEach(function(o) { o.className = ''; });
          b.className = 'active';
        };
        bar.appendChild(b);
      });
      el.appendChild(bar);
    }",
    data = list(labels = c(unname(model_labels[models]), "All"), visible = vis))
}

chart_layout <- function(p, models = NULL, xtitle = "", n_fixed = 1, per_model = 1) {
  p <- p |>
    layout(paper_bgcolor = "rgba(0,0,0,0)",
           plot_bgcolor  = "rgba(0,0,0,0)",
           font = list(family = "IBM Plex Sans, Segoe UI, Helvetica Neue, Arial, sans-serif", color = grey),
           xaxis = list(title = xtitle, showgrid = FALSE, zeroline = FALSE, color = grey,
                        linecolor = "#e5ddd3"),
           yaxis = list(title = "", color = grey, gridcolor = "#f1ebe4",
                        zerolinecolor = "#cfc6bb"),
           legend = list(orientation = "h", x = 0, y = -0.15, font = list(color = ink)),
           hovermode = "x unified",
           hoverlabel = list(bgcolor = "#ffffff", bordercolor = "#efe6dc",
                             font = list(color = ink)),
           margin = list(t = 50)) |>
    config(displayModeBar = FALSE)
  if (length(models) > 0) p <- model_buttons(p, models, n_fixed, per_model)
  p
}

# all from results/*.csv
read_res <- function(f) if (file.exists(f)) read_csv(f, show_col_types = FALSE) else NULL
eval_acc   <- read_res("results/eval_accuracy.csv")    # 10_evaluate.R
eval_tests <- read_res("results/eval_tests.csv")
eval_bias  <- read_res("results/eval_bias.csv")
eval_shape <- read_res("results/eval_shape.csv")
eval_post  <- read_res("results/eval_post.csv")
backtest   <- read_res("results/backtest_forecasts.csv")
if (!is.null(backtest)) backtest <- add_ensembles(backtest)
bmt_sel    <- read_res("results/bmt_selection.csv")    # 06_bmt.R
bmtx_combo <- read_res("results/bmtx_combos.csv")      # 08_bmtx.R
bmtx_sel   <- read_res("results/bmtx_selection.csv")   # 08_bmtx.R
bat_daily  <- read_res("results/battery_daily.csv")    # 11_battery.R
bat_sum    <- read_res("results/battery_summary.csv")
bat_tests  <- read_res("results/battery_tests.csv")
bat_setup  <- read_res("results/battery_setup.csv")
mcs_models <- read_res("results/mcs_models.csv")       # 10_evaluate.R
xgb_tun    <- read_res("results/xgb_tuning.csv")       # 09_xgb.R
prob_sc    <- read_res("results/prob_scores.csv")      # 10_evaluate.R
prob_cal   <- read_res("results/prob_calibration.csv") # 10_evaluate.R
prob_neg   <- read_res("results/prob_negative.csv")    # 10_evaluate.R
chall      <- read_res("results/challengers.csv")      # 10_evaluate.R
if (!is.null(chall)) for (v in c("backtest_mae_champion", "backtest_mae_challenger", "backtest_diff", "backtest_p",
                                 "live_mae_champion", "live_mae_challenger", "live_p_holm")) {
  if (!v %in% names(chall)) chall[[v]] <- NA_real_
}
has_eval    <- !is.null(eval_acc)
has_battery <- !is.null(bat_sum)
risk_tests <- read_res("results/battery_risk_tests.csv")   # 12_battery_risk.R
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
strat_colors <- c(perfect = "#cfc6bb", model_colors)

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
  med_txt <- sprintf(paste("Bias correction (median error of the last %d days): %s mean error %+.1f to %+.1f, MAE %.1f to %.1f (DM %s);",
                           "significant for %d of %d models; it runs live as a challenger (below)."),
                     k_bias, text_labels[[eval_main]], r$mean_error_raw, r$mean_error_corr, r$mae_raw, r$mae_corr, p_txt(r$dm_p),
                     n_sig, nrow(eval_post))
  mz_txt <- if ("mae_mz" %in% names(r)) {
    sprintf("Mincer-Zarnowitz correction (a + b × forecast on the same %d days): MAE %.1f, %s than the median shift (DM %s).",
            k_bias, r$mae_mz, if (r$mae_mz < r$mae_corr) "better" else "worse", p_txt(r$dm_p_mz_med))
  } else ""
  paste(med_txt, mz_txt)
} else ""

xgb_txt <- if (!is.null(xgb_tun) && any(xgb_tun$chosen)) {
  ch <- xgb_tun[xgb_tun$chosen, ]
  set_txt <- function(m) { r <- ch[ch$model == m, ]; sprintf("depth %d and %s rounds", r$max_depth, r$rounds) }
  sprintf("Chosen: %s.", set_txt("xgbx"))
} else ""

bmt_avg_sel <- if (!is.null(bmt_sel)) format(round(sum(bmt_sel$share) / n_distinct(bmt_sel$target), 1), nsmall = 1) else "\u2013"
bmt_n_cand  <- if (!is.null(bmt_sel)) n_distinct(bmt_sel$input) else "\u2013"
eval_period <- if (has_eval) {
  n_live <- if ("n_live_days" %in% names(eval_acc)) eval_acc$n_live_days[1] else 0
  sprintf("%s to %s, %d days (%s), %s hourly forecasts",
          format(eval_acc$from[1], "%d %b %Y"), format(eval_acc$to[1], "%d %b %Y"), eval_acc$n_days[1],
          if (n_live > 0) sprintf("%d backtest days, then %d live %s", eval_acc$n_backtest_days[1], n_live,
                                  if (n_live == 1) "day" else "days")
          else "backtest only, live days are added once every model has a saved forecast",
          prettyNum(eval_acc$n_hours[1], big.mark = ","))
} else ""

# bmt-x combinations
has_bmtx  <- !is.null(bmtx_combo)
group_txt <- c(nb = "neighbour prices", wind = "wind", solar = "solar", hol = "holidays")
combo_txt <- function(combo) {
  if (combo == "prices") return("DK1 prices only")
  paste("DK1 prices +", paste(group_txt[strsplit(combo, "+", fixed = TRUE)[[1]]], collapse = " + "))
}
bmtx_txt <- if (has_bmtx) {
  best  <- bmtx_combo[1, ]
  base  <- bmtx_combo[bmtx_combo$combo == "prices", ]
  live  <- bmtx_combo[bmtx_combo$combo == "nb+hol", ]
  n_in  <- sum(bmtx_combo$in_mcs)
  ws    <- grepl("wind|solar", bmtx_combo$combo)
  paste0(
    "The best combination is ", combo_txt(best$combo), " with MAE ", format(round(best$mae, 1), nsmall = 1),
    sprintf(" (%s%.1f vs DK1 prices only). ", if (best$mae <= base$mae) "\u2212" else "+", abs(best$mae - base$mae)),
    n_in, " of the ", nrow(bmtx_combo), " combinations are in the 10% model confidence set",
    if (all(bmtx_combo$in_mcs == ws)) ", exactly the ones with wind or solar. " else ". ",
    "But the wind and solar day-ahead forecasts are published at 18:00 the day before delivery, after the noon auction, ",
    "so those combinations could not have been used for a real bid; they show what that information is worth. ",
    sprintf("With only what is known before the auction (neighbour prices and holidays) the MAE is %.2f, vs %.2f for DK1 prices only.",
            live$mae, base$mae),
    if (has_eval && "bmtx" %in% eval_acc$model)
      sprintf(" Adding weather forecasts for DK1 and Germany made 48 hours ahead instead (known before the auction), BMT-X reaches %.1f.",
              eval_acc$mae[eval_acc$model == "bmtx"]) else "")
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
eur      <- function(x) paste0(if (x < 0) "\u2212" else "", prettyNum(abs(round(x)), big.mark = ","))
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
    sprintf(paste("Used: %s. The revenue fell below its floor on %s of %d days, %s (Kupiec %s). The misses",
                  "don't cluster (Christoffersen %s), and the bad days aren't significantly worse than its ES says",
                  "(Acerbi\u2013Sz\u00e9kely %s)."),
            low1(u$label), pct(u$miss_rate), u$days,
            if (u$miss_rate > 0.05) "a bit more than the 5% target, but not significantly" else "close to the 5% target",
            p_txt(u$kupiec_p), p_txt(u$christoffersen_p), p_txt(u$z2_p))
  } else {
    sprintf("No method passes all three tests, so %s is shown, with the lowest FZ0 score. Read its numbers with care.",
            low1(u$label))
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
fans <- lapply(setNames(models, models), \(m) {
  if (m == "naive_mix" || !m %in% names(tomorrow) || all(is.na(tomorrow[[m]]))) return(NULL)
  x  <- bind_rows(transmute(past_fc(m), date, hour, e = actual - f),
                  tibble(date = latest, hour = 0:23, e = NA_real_))
  em <- err_matrix(x)
  q  <- quantile_offsets(em$dates, em$E, vol_scale(all_prices, em$dates)$s) |>
    filter(date == latest, method == q_method(m))
  if (nrow(q) == 0) return(NULL)
  tomorrow |> select(hour, f = all_of(m)) |> inner_join(q, by = "hour") |> mutate(q = f + off)
})
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

# range text and tables
q_lab   <- function(m) q_methods[[m]]
q_low   <- function(m) paste0(tolower(substring(q_lab(m), 1, 1)), substring(q_lab(m), 2))
prob_m  <- if (!is.null(prob_sc)) prob_sc[prob_sc$model == eval_main, ] else NULL
has_prob <- !is.null(prob_m) && nrow(prob_m) > 0
prob_txt <- if (has_prob) {
  ch  <- prob_m[prob_m$chosen, ]
  bst <- prob_m[prob_m$method == ch$best, ]
  n_cal <- if ("calibrated" %in% names(prob_m)) sum(prob_m$calibrated) else 0
  why <- if (n_cal == 0) "no method is on target, so the best calibrated is used" else
    if (n_cal == 1) "its ranges are the only ones not significantly off target" else
    "it has the lowest pinball loss of the methods whose ranges are on target"
  alt <- if (ch$method != ch$best) sprintf(" %s: lower pinball loss (DM %s), but its ranges are off target (50%% range %s, 90%% range %s).",
                                           q_lab(ch$best), p_txt(ch$dm_p), pct(bst$cov50), pct(bst$cov90)) else ""
  sprintf(paste("Used: %s, because %s.%s For %s its 90%% range covered %s of hours (target 90%%, %s)",
                "and the 50%% range %s (target 50%%, %s), over %d days."),
          q_low(ch$method), why, alt, text_labels[[eval_main]], pct(ch$cov90), p_txt(ch$cov90_p),
          pct(ch$cov50), p_txt(ch$cov50_p), ch$days)
} else ""
neg_txt <- if (!is.null(prob_neg) && eval_main %in% prob_neg$model) {
  r <- prob_neg[prob_neg$model == eval_main, ]
  sprintf(paste("The chance of a negative price on the Forecast tab comes from the same ranges. Brier score %.4f,",
                "against %.4f for the share of negative prices in that hour over the 28 days before: %s %s,",
                "over %s hours with %d negative prices."),
          r$brier, r$brier_clim, pct(abs(r$skill)), if (r$skill >= 0) "better" else "worse",
          prettyNum(r$hours, big.mark = ","), as.integer(r$neg_hours))
} else ""
chall_txt <- if (!is.null(chall) && nrow(chall) > 0) {
  bt <- chall[!is.na(chall$backtest_p), ]
  bt_txt <- if (nrow(bt) == 1 && bt$backtest_diff < 0 && bt$backtest_p >= 0.05) {
    sprintf("It had a lower MAE in the backtest, but not significantly (p %s), which is why it is tested live first.",
            p_tab(bt$backtest_p))
  } else if (nrow(bt) == 1 && bt$backtest_diff < 0) {
    sprintf("It is also significantly better in the backtest (p %s), but the live days decide, as set in advance.",
            p_tab(bt$backtest_p))
  } else if (nrow(bt) > 1 && all(bt$backtest_diff < 0) && all(bt$backtest_p >= 0.05)) {
    sprintf("%s had a lower MAE in the backtest, but %s significantly (p %s), which is why they are tested live first.",
            if (nrow(bt) == 2) "Both" else "All of them", if (nrow(bt) == 2) "neither" else "none",
            paste(p_tab(sort(bt$backtest_p)), collapse = " and "))
  } else ""
  test_txt <- if (nrow(chall) == 1) "Diebold-Mariano with Newey-West variance" else
    sprintf("Diebold-Mariano with Newey-West variance, Holm-adjusted for the %d challengers", nrow(chall))
  one <- nrow(chall) == 1
  sprintf(paste("%s is saved every day before the auction, like the ensemble, but not shown as the forecast.",
                "On %s %s replaces the ensemble only if its MAE over the live days from %s is lower with",
                "p < 0.05 (%s). Otherwise the ensemble stays. The rule was set before the first live day. %s"),
          if (one) "The challenger" else "Each challenger",
          sub("^0", "", format(as_date(chall$decision[1]), "%d %B %Y")), if (one) "it" else "a challenger",
          sub("^0", "", format(as_date(chall$start[1]), "%d %B %Y")),
          test_txt, bt_txt)
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
  sprintf("%s vs the weekly naive, MAE over %d live %s since %s", main_label, live_acc$days,
          if (live_acc$days == 1) "day" else "days", format(live_acc$from, "%d %b"))

# live revenue at risk days
risk_live <- if (has_risk && "source" %in% names(risk_daily))
  risk_daily |> filter(method == risk_u$method, source == "live") |> arrange(desc(date)) else NULL


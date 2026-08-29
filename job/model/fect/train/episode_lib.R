# =============================================================================
# episode_lib.R - production FECT episode scoring exclusion
#
# Rule (Tim / iFAT round-6 idea, production port): rel99_x2
#   day t is a seed if md_mean > trailing-3y p99 AND md_mean > 2 x trailing-365d
#   median, where both trailing windows end at t-1 (strictly causal).
# Then dilate +/- 1 calendar day within metro (episode shoulders).
#
# This flag is SCORING / POOLING exclusion ONLY. Never add it to X/Z covariates.
# Kill switch: CPPORTAL_FECT_EPISODE_EXCLUDE (default ON; 0/false/off disables).
# =============================================================================

fect_episode_exclude_enabled = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_FECT_EPISODE_EXCLUDE", "")))
  if (!nzchar(v)) return(TRUE)
  !(v %in% c("0", "false", "f", "no", "n", "off"))
}

fect_episode_metro_day = function(panel, outcome_col = "aq_daily_mean",
                                  metro_col = "metro_id",
                                  date_col = "date") {
  need = c(metro_col, date_col, outcome_col)
  empty = tibble::tibble(
    metro_id = integer(0), date = as.Date(character(0)),
    md_mean = numeric(0)
  )
  if (is.null(panel) || !nrow(panel) || !all(need %in% names(panel))) {
    return(empty)
  }
  md = panel |>
    dplyr::filter(!is.na(.data[[outcome_col]])) |>
    dplyr::group_by(.data[[metro_col]], .data[[date_col]]) |>
    dplyr::summarise(
      md_mean = mean(.data[[outcome_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(metro_id = dplyr::all_of(metro_col),
                  date = dplyr::all_of(date_col)) |>
    dplyr::mutate(
      metro_id = as.integer(.data$metro_id),
      date = as.Date(.data$date)
    )
  if (!nrow(md)) return(empty)
  full = md |>
    dplyr::group_by(.data$metro_id) |>
    dplyr::summarise(
      date = list(seq(min(.data$date), max(.data$date), by = "day")),
      .groups = "drop"
    ) |>
    tidyr::unnest("date")
  dplyr::left_join(full, md, by = c("metro_id", "date")) |>
    dplyr::arrange(.data$metro_id, .data$date)
}

.fect_episode_trailing = function(x, win, fun, min_obs) {
  n = length(x)
  if (n < 1L) return(numeric(0))
  roll_fun = function(v) {
    v = v[is.finite(v)]
    if (length(v) < min_obs) NA_real_ else fun(v)
  }
  rolled = if (requireNamespace("zoo", quietly = TRUE)) {
    as.numeric(zoo::rollapply(
      x, width = win, FUN = roll_fun, align = "right",
      fill = NA_real_, partial = FALSE
    ))
  } else {
    out = rep(NA_real_, n)
    if (n >= win) {
      for (i in win:n) out[[i]] = roll_fun(x[(i - win + 1L):i])
    }
    out
  }
  c(NA_real_, rolled[-n])
}

fect_episode_add_trailing = function(md) {
  if (is.null(md) || !nrow(md)) return(md)
  md |>
    dplyr::group_by(.data$metro_id) |>
    dplyr::mutate(
      tr_p99 = .fect_episode_trailing(
        .data$md_mean, 1095L,
        function(v) as.numeric(stats::quantile(v, 0.99, names = FALSE, type = 7)),
        min_obs = 365L
      ),
      tr_med365 = .fect_episode_trailing(
        .data$md_mean, 365L,
        function(v) stats::median(v),
        min_obs = 180L
      )
    ) |>
    dplyr::ungroup()
}

fect_episode_dilate = function(md, flag) {
  if (is.null(md) || !nrow(md)) return(logical(0))
  flag = as.logical(flag)
  flag[is.na(flag)] = FALSE
  md |>
    dplyr::mutate(.flg = flag) |>
    dplyr::group_by(.data$metro_id) |>
    dplyr::mutate(
      .out = .data$.flg |
        dplyr::coalesce(dplyr::lag(.data$.flg), FALSE) |
        dplyr::coalesce(dplyr::lead(.data$.flg), FALSE)
    ) |>
    dplyr::ungroup() |>
    dplyr::pull(".out")
}

fect_episode_flag_metro_day = function(panel, outcome_col = "aq_daily_mean") {
  md = fect_episode_metro_day(panel, outcome_col = outcome_col)
  if (!nrow(md)) {
    return(tibble::tibble(
      metro_id = integer(0), date = as.Date(character(0)),
      episode_flag = logical(0)
    ))
  }
  md = fect_episode_add_trailing(md)
  seed = !is.na(md$md_mean) & !is.na(md$tr_p99) & !is.na(md$tr_med365) &
    md$md_mean > md$tr_p99 & md$md_mean > 2 * md$tr_med365
  tibble::tibble(
    metro_id = md$metro_id,
    date = md$date,
    episode_flag = fect_episode_dilate(md, seed)
  )
}

FECT_EPISODE_DAY_TYPES = c(
  "per_day_metro", "per_day_metro_unit",
  "per_day_metro_imputed", "per_day_metro_unit_imputed",
  "per_day_metro_imputed_d1", "per_day_metro_unit_imputed_d1"
)

fect_stamp_episode_flag = function(att, episode_md) {
  if (is.null(att) || !nrow(att)) return(att)
  if (!("episode_flag" %in% names(att))) att$episode_flag = FALSE
  if (is.null(episode_md) || !nrow(episode_md)) return(att)

  is_day = att$type %in% FECT_EPISODE_DAY_TYPES
  if (!any(is_day)) return(att)

  date_col = if ("day" %in% names(att)) {
    "day"
  } else if ("date" %in% names(att)) {
    "date"
  } else {
    return(att)
  }

  ep = episode_md |>
    dplyr::transmute(
      metro_id = as.integer(.data$metro_id),
      .ep_day = as.Date(.data$date),
      episode_flag = as.logical(.data$episode_flag)
    ) |>
    dplyr::filter(is.finite(.data$metro_id), !is.na(.data$.ep_day))

  day_rows = att[is_day, , drop = FALSE] |>
    dplyr::mutate(
      .join_metro = as.integer(.data$metro_id),
      .join_day = as.Date(.data[[date_col]])
    ) |>
    dplyr::select(-dplyr::any_of("episode_flag")) |>
    dplyr::left_join(
      ep,
      by = c(".join_metro" = "metro_id", ".join_day" = ".ep_day")
    ) |>
    dplyr::mutate(
      episode_flag = dplyr::coalesce(.data$episode_flag, FALSE)
    ) |>
    dplyr::select(-".join_metro", -".join_day")

  att[is_day, ] = day_rows
  att$episode_flag[!is_day] = FALSE
  att
}

.fect_episode_pool_att_rows = function(rows) {
  empty = tibble::tibble(
    att = NA_real_, se_att = NA_real_, yhat0 = NA_real_, yhat1 = NA_real_,
    yhatse0 = NA_real_, yhatse1 = NA_real_, n_effects = 0L,
    t = NA_real_, df = NA_real_, p_value = NA_real_, stars = ""
  )
  if (is.null(rows) || !nrow(rows)) return(empty)
  a = suppressWarnings(as.numeric(rows$att))
  s = suppressWarnings(as.numeric(rows$se_att))
  ok = is.finite(a) & is.finite(s)
  a = a[ok]; s = s[ok]
  n = length(a)
  if (n < 1L) return(empty)
  att = mean(a)
  se_att = sqrt(sum(s^2)) / n
  y0 = if ("yhat0" %in% names(rows)) {
    suppressWarnings(as.numeric(rows$yhat0))[ok]
  } else {
    rep(NA_real_, n)
  }
  y1 = if ("yhat1" %in% names(rows)) {
    suppressWarnings(as.numeric(rows$yhat1))[ok]
  } else {
    rep(NA_real_, n)
  }
  s0 = if ("yhatse0" %in% names(rows)) {
    suppressWarnings(as.numeric(rows$yhatse0))[ok]
  } else if ("se0" %in% names(rows)) {
    suppressWarnings(as.numeric(rows$se0))[ok]
  } else {
    s
  }
  s1 = if ("yhatse1" %in% names(rows)) {
    suppressWarnings(as.numeric(rows$yhatse1))[ok]
  } else if ("se1" %in% names(rows)) {
    suppressWarnings(as.numeric(rows$se1))[ok]
  } else {
    s
  }
  s0[!is.finite(s0) | s0 <= 0] = NA_real_
  s1[!is.finite(s1) | s1 <= 0] = NA_real_
  ivw = function(y, se) {
    w = 1 / se^2
    if (!any(is.finite(w) & is.finite(y))) return(c(NA_real_, NA_real_))
    keep = is.finite(w) & is.finite(y)
    c(sum(y[keep] * w[keep]) / sum(w[keep]), sqrt(1 / sum(w[keep])))
  }
  y0p = ivw(y0, s0); y1p = ivw(y1, s1)
  tstat = if (is.finite(se_att) && se_att > 0) att / se_att else NA_real_
  df = as.numeric(max(n - 1L, 1L))
  p = if (is.finite(tstat)) 2 * (1 - stats::pt(abs(tstat), df = df)) else NA_real_
  stars = if (is.finite(p) && requireNamespace("gtools", quietly = TRUE)) {
    gtools::stars.pval(p)
  } else {
    ""
  }
  tibble::tibble(
    att = att, se_att = se_att,
    yhat0 = y0p[[1]], yhat1 = y1p[[1]],
    yhatse0 = y0p[[2]], yhatse1 = y1p[[2]],
    n_effects = as.integer(n),
    t = tstat, df = df, p_value = p, stars = as.character(stars)
  )
}

fect_episode_postprocess_att = function(att, panel,
                                        outcome_col = "aq_daily_mean",
                                        episode_md = NULL,
                                        apply_charged = TRUE) {
  if (is.null(att) || !nrow(att)) return(att)
  if (!fect_episode_exclude_enabled()) {
    message("[fect.train.episode] DISABLED via CPPORTAL_FECT_EPISODE_EXCLUDE",
            " -- scoring aggregates unchanged (no episode_flag column)")
    return(att)
  }
  if (is.null(episode_md)) {
    episode_md = fect_episode_flag_metro_day(panel, outcome_col = outcome_col)
  }
  n_ep = if (nrow(episode_md)) sum(episode_md$episode_flag %in% TRUE) else 0L
  message("[fect.train.episode] rel99_x2+dilate: ", n_ep,
          " metro-day(s) flagged across ",
          dplyr::n_distinct(episode_md$metro_id[episode_md$episode_flag %in% TRUE]),
          " metro(s)")

  att = fect_stamp_episode_flag(att, episode_md)

  unit = att |>
    dplyr::filter(.data$type == "per_day_metro_unit")
  if (!nrow(unit)) {
    message("[fect.train.episode] no per_day_metro_unit rows -- stamp only")
    return(att)
  }
  if (apply_charged && exists("fect_charged_days_only_filter", mode = "function")) {
    unit = fect_charged_days_only_filter(unit, date_col = "day")
  }
  unit = unit[!(unit$episode_flag %in% TRUE), , drop = FALSE]
  message("[fect.train.episode] rebuild pool: ", nrow(unit),
          " charged+non-episode per_day_metro_unit row(s)")

  rebuild_one = function(df, type_lab, keys = character(0)) {
    if (!nrow(df)) return(NULL)
    if (length(keys)) {
      df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
        dplyr::group_modify(~ .fect_episode_pool_att_rows(.x)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          type = type_lab,
          basis = "fitted",
          day = as.Date(NA),
          fullaqsid = NA_character_,
          episode_flag = FALSE
        )
    } else {
      .fect_episode_pool_att_rows(df) |>
        dplyr::mutate(
          type = type_lab,
          basis = "fitted",
          metro_id = NA_integer_,
          month = as.Date(NA),
          day = as.Date(NA),
          fullaqsid = NA_character_,
          episode_flag = FALSE
        )
    }
  }

  new_overall = rebuild_one(unit, "overall")
  new_pm = rebuild_one(unit, "per_metro", "metro_id")
  unit_m = unit |>
    dplyr::mutate(month = lubridate::floor_date(as.Date(.data$day), "month"))
  new_pmm = rebuild_one(unit_m, "per_metro_month", c("metro_id", "month"))

  if (!("basis" %in% names(att))) {
    keep = !(att$type %in% c("overall", "per_metro", "per_metro_month"))
  } else {
    is_fit_agg = att$type %in% c("overall", "per_metro", "per_metro_month") &
      (is.na(att$basis) | att$basis == "fitted")
    keep = !is_fit_agg
  }
  att_rest = att[keep, , drop = FALSE]

  if (exists("build_anchored_per_metro", mode = "function")) {
    # ORDER MATTERS. Drop the stale anchored per_metro rows BEFORE snapshotting
    # `att_for_anch`, never after. `build_anchored_per_metro()` skips any metro
    # that already carries a fitted per_metro row; if the anchored per_metro
    # rows it produced on the previous pass are still in the frame it is handed,
    # every anchored metro looks "already covered", its per_day_metro_imputed
    # rows are filtered out, and the rebuild returns NULL — after we have
    # already deleted the rows it was supposed to replace. That is exactly the
    # PR #421 regression: anchored per_metro rows deleted and never rebuilt.
    if ("basis" %in% names(att_rest)) {
      att_rest = att_rest[!(att_rest$type %in% "per_metro" &
                              !is.na(att_rest$basis) &
                              att_rest$basis == "anchored"), , drop = FALSE]
    }
    att_for_anch = att_rest
    if ("episode_flag" %in% names(att_for_anch)) {
      drop_ep = att_for_anch$type %in% "per_day_metro_imputed" &
        att_for_anch$episode_flag %in% TRUE
      att_for_anch = att_for_anch[!drop_ep, , drop = FALSE]
    }
    if (apply_charged && exists("fect_charged_days_only_filter", mode = "function") &&
        any(att_for_anch$type %in% "per_day_metro_imputed")) {
      imp = att_for_anch[att_for_anch$type %in% "per_day_metro_imputed", , drop = FALSE]
      other = att_for_anch[!att_for_anch$type %in% "per_day_metro_imputed", , drop = FALSE]
      imp = fect_charged_days_only_filter(imp, date_col = "day")
      att_for_anch = dplyr::bind_rows(other, imp)
    }
    anchored_pm = tryCatch(
      build_anchored_per_metro(att_for_anch),
      error = function(e) {
        message("[fect.train.episode] anchored per_metro rebuild failed: ",
                conditionMessage(e))
        NULL
      }
    )
    if (!is.null(anchored_pm) && nrow(anchored_pm) > 0L) {
      if (!("episode_flag" %in% names(anchored_pm))) {
        anchored_pm$episode_flag = FALSE
      }
      att_rest = dplyr::bind_rows(att_rest, anchored_pm)
      message("[fect.train.episode] rebuilt ", nrow(anchored_pm),
              " anchored per_metro row(s) on charged+non-episode days")
    }
  }

  out = dplyr::bind_rows(att_rest, new_overall, new_pm, new_pmm)
  message("[fect.train.episode] att rows after rebuild: ", nrow(out),
          " (overall/per_metro/per_metro_month = charged+non-episode)")
  out
}
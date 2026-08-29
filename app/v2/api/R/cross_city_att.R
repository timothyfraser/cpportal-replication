# =============================================================================
# cross_city_att.R — Cross-city FECT ATT summaries from trained pins + policies
#
# Requires (source before this file): helpers_format.R (%||%), logging.R,
# db.R (get_db), models.R (load_fect_bundle), data_fetch.R (bundle_spec_metro_ids,
# fetch_metros, .cpportal_partition_key).
#
# Quantity A/B/C share one event-time cache built from bundle$att
# (type == per_day_metro) joined to earliest treated start_date per metro.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
})

.cpportal_cross_city_cache_env <- new.env(parent = emptyenv())

#' Huber M-estimate of location (ADR-0012). Robust central tendency for the
#' heavy-tailed anchored per-day ATTs — down-weights outliers smoothly instead
#' of discarding them (unlike a trimmed mean). k=1.345 is the standard tuning
#' (95% efficiency at the Normal). Prefers MASS::hubers (updates scale too);
#' falls back to a self-contained IRLS with a fixed MAD scale so the serving
#' API carries no hard MASS dependency. Degenerate inputs → the plain mean.
.huber_location <- function(x, k = 1.345) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) return(NA_real_)
  if (length(x) < 3L)  return(mean(x))
  if (requireNamespace("MASS", quietly = TRUE)) {
    m <- tryCatch(MASS::hubers(x, k = k)$mu, error = function(e) NA_real_)
    if (is.finite(m)) return(m)
  }
  mu <- stats::median(x); s <- stats::mad(x)
  if (!is.finite(s) || s <= 0) return(mean(x))
  for (i in seq_len(50L)) {
    r  <- (x - mu) / s
    w  <- ifelse(abs(r) <= k, 1, k / abs(r))   # Huber weights
    mn <- sum(w * x) / sum(w)
    if (abs(mn - mu) < 1e-6 * s) { mu <- mn; break }
    mu <- mn
  }
  mu
}

.cpportal_cross_city_ttl_secs <- function() {
  as.numeric(Sys.getenv("CPPORTAL_CROSS_CITY_TTL", "3600"))
}

#' ---------------------------------------------------------------------------
#' CHARGED-DAYS ("excluding inactive days") — THE UNIVERSAL POOLING ESTIMAND
#' ---------------------------------------------------------------------------
#' Tim's ruling, 2026-08-03 (DESIGN §36/§37): a day a cordon does not actually
#' charge is only SEMI-TREATED, and pooling it beside fully-charged days muddies
#' the estimand. So it is dropped at the POOLING stage, for EVERY method and
#' EVERY basis — `fect` fitted rows included, not just anchored ones. This is
#' the estimand, not an arm flag: two cities' ATTs are only comparable if both
#' answer "effect on a charged day".
#'
#' It touches NOTHING upstream: models are still FIT on all days (weekends carry
#' the dow/seasonal structure), and the gap model, c(M) and T_f/freeze all still
#' see every day.
#'
#' EITHER/OR ACROSS SCHEMES — Tim's refinement, 2026-08-09, verbatim:
#'
#'   "it's an EITHER OR policy, not an AND/BOTH policy … those monitors IN the
#'    CCZ on weekends ARE treated during the ULEZ period because they are
#'    affected by a traffic cordon."
#'
#' So the estimand is: a (monitor, day) is charged iff AT LEAST ONE scheme whose
#' zone covers the monitor has an active period covering the day AND charges on
#' that weekday. A metro can host several schemes on different schedules —
#' London's CCZ charges Mon–Fri, its ULEZ charges 7/7 — so a single weekday map
#' per metro (the pre-2026-08-09 encoding) wrongly dropped London weekends
#' throughout the ULEZ era.
#'
#' This layer sees only (metro_id, day): it cannot tell which monitor sits in
#' which zone. It therefore implements the METRO-GRAIN collapse of the rule —
#' date-aware segments, where a weekday is excluded only if NO scheme active in
#' that window charges it. London splits at 2019-04-08, the day the first ULEZ
#' period opened.
#'
#' APPROXIMATION, STATED LOUDLY (verified at source 2026-08-09,
#' public.monitors × public.zones, ST_Intersects): during
#' 2019-04-08..2021-10-24 the central ULEZ zone covered 29 of the 34 monitors
#' inside the CCZ core zone. The five that fell OUTSIDE it were still Mon–Fri
#' only, and three of them carry weekend panel rows — 485 monitor-days — that
#' this metro-grain form counts as charged when they were not. From 2021-10-25
#' (inner expansion) onward the residual is EXACTLY ZERO: all 34 CCZ monitors
#' are inside the ULEZ zone. The train side has monitor identity and applies the
#' exact per-pair rule, so this residual is confined to the pooling/serving cut.
#' Revisit if this layer ever gains zone membership.
#'
#' The segments MIRROR `FECT_CHARGED_DAYS_METRO_SEGMENTS` in
#' `job/model/fect/train/functions.R` VERBATIM. They are duplicated rather than
#' sourced only because the Connect bundle ships `app/v2/api/**` and cannot
#' reach `job/`; the two must be edited together. ISO weekday codes
#' (1 = Mon … 7 = Sun). NYC (metro_id 1) charges 24/7 and is deliberately
#' ABSENT — absence means "exclude nothing", not "unregistered".
#'
#' Hours-of-day and public-holiday exemptions remain out of scope (task #42);
#' the mechanism is weekday grain only. Gothenburg (957) was added by Tim on
#' 2026-08-09: its trängselskatt is the same statute family as Stockholm's
#' (Lag 2004:629), extended to Göteborg on 2013-01-01, Mon–Fri only.
CPPORTAL_CHARGED_DAYS_METRO_SEGMENTS <- list(
  `943` = list(
    # London, CCZ only — Mon–Fri.
    list(from = as.Date("1900-01-01"), to = as.Date("2019-04-07"),
         excluded = c(6L, 7L)),
    # ULEZ live (central → inner → London-wide) — 7/7 somewhere in the metro.
    list(from = as.Date("2019-04-08"), to = as.Date("9999-12-31"),
         excluded = integer(0))
  ),
  `949` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = c(6L, 7L))),  # Stockholm congestion tax
  `950` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = c(6L, 7L))),  # Milan Area C/Ecopass
  `955` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = 7L)),         # Singapore ERP — no Sundays
  `957` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = c(6L, 7L))),  # Gothenburg congestion tax
  `958` = list(
    # Bergen sentrum ring (period 65, zone 34) — Mon–Fri only.
    list(from = as.Date("1900-01-01"), to = as.Date("2018-12-31"),
         excluded = c(6L, 7L)),
    # Bomringen full system (period 66, zone 35) — AutoPASS-era 24/7 charging.
    # ASSUMPTION: no repo doc pins the weekday-only → 7/7 cutover, so the
    # boundary is period 66's start; ADR-0029 lists Bergen's exact 2019
    # in-service date as an open provenance item.
    list(from = as.Date("2019-01-01"), to = as.Date("9999-12-31"),
         excluded = integer(0))
  )
)

#' Drop inactive (non-charging) days before a pool.
#'
#' Only ever REMOVES rows, never edits them, so an unregistered metro and a
#' disabled call are both exact no-ops. Rows with an unparseable date or a
#' non-finite metro id are always kept.
#'
#' @param rows Tibble with a metro id column and a Date column.
#' @param date_col,metro_col Column names (defaults match the cross-city cache).
#' @param include_inactive DIAGNOSTIC ESCAPE HATCH. TRUE returns `rows`
#'   untouched — the pre-2026-08-03 all-days pool. Never the serving default.
#' @return `rows` with inactive-day rows removed.
cpportal_charged_days_filter <- function(rows, date_col = "day",
                                         metro_col = "metro_id",
                                         include_inactive = FALSE) {
  if (isTRUE(include_inactive)) return(rows)
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) return(rows)
  if (!all(c(date_col, metro_col) %in% names(rows))) return(rows)
  mids <- suppressWarnings(as.integer(rows[[metro_col]]))
  dts  <- suppressWarnings(as.Date(rows[[date_col]]))
  wday <- as.integer(format(dts, "%u"))
  key  <- as.character(mids)
  ok   <- is.finite(mids) & !is.na(wday)
  drop <- logical(nrow(rows))
  # One pass per registered (metro, segment). A day is dropped only when NO
  # scheme active in that window charges its weekday — the either/or rule.
  for (m in intersect(unique(key[ok]), names(CPPORTAL_CHARGED_DAYS_METRO_SEGMENTS))) {
    sel_m <- ok & key == m
    if (!any(sel_m)) next
    for (s in CPPORTAL_CHARGED_DAYS_METRO_SEGMENTS[[m]]) {
      ex <- as.integer(s$excluded)
      if (!length(ex)) next
      drop[sel_m & dts >= s$from & dts <= s$to & wday %in% ex] <- TRUE
    }
  }
  rows[!drop, , drop = FALSE]
}

#' Serving pool mode — which fitted-basis aggregator the combiners run.
#'
#' "ivw_flat" (DEFAULT, Tim's 2026-08-14 estimand ruling): flat inverse-variance
#' pool over monitor-day cells, no between-monitor variance component, no D2a
#' fallback. "d2c" is the ROLLBACK to the pre-ruling behaviour (fitted D2b
#' variance split where `varc_*` are present, plus the additive s²_B/J term).
#' Set `CPPORTAL_ATT_POOL=d2c` to restore it. Anchored paths ignore this.
cpportal_att_pool_mode <- function() {
  v <- tolower(trimws(Sys.getenv("CPPORTAL_ATT_POOL", "")))
  if (v %in% c("d2c", "d2b", "legacy")) "d2c" else "ivw_flat"
}

#' Drop episode-flagged day rows before pooling (fail-open if column absent).
#'
#' Production pins may carry `episode_flag` on day-grain ATT rows (rel99_x2 +
#' +/-1d dilation; scoring/pooling exclusion only). When the column is missing
#' (older pins), rows pass through unchanged.
cpportal_episode_days_filter <- function(rows) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) return(rows)
  if (!("episode_flag" %in% names(rows))) return(rows)
  rows[!(rows$episode_flag %in% TRUE), , drop = FALSE]
}

#' D2b / IVW window aggregation — combine per-day (or unit-day) rows over an
#' arbitrary set of days into a single ATT + SE.
#'
#' Point estimate:
#'   - anchored: Huber M-estimate (heavy-tailed by construction)
#'   - fitted + D2b components present: plain mean (historical D2b contract)
#'   - fitted + D2b absent: inverse-variance weighted mean of day-level ATTs
#'
#' SE preference:
#'   - D2b (any basis, when `varc_*` present):
#'       sqrt(mean(varc_shared_md) + mean(varc_indep_md) / n_days)
#'   - fitted without D2b (Tim, 2026-08-12 — Table 1 / `/window`):
#'       IVW SE = sqrt(1 / sum(w)), w = 1/se_att² across day rows.
#'       Synthetic-control day-level SEs stay tight even at J = 1 (dozens of
#'       controls); hundreds of day-level effects must √n-shrink. Do **not**
#'       fall back to D2a `mean(se_att)` for fitted — that was a fallacy that
#'       left Milan/Bergen window CIs absurdly wide.
#'   - anchored without D2b: D2a `mean(se_att)` (conservative; shared anchor
#'     error must not √n-shrink). Anchored is not the production path for the
#'     fitted J = 1 cities Tim ruled on.
#'
#' Charged-days + episode exclusion run here (fail-open if columns/maps absent)
#' so `/window` and every other combiner share the universal estimand cut.
#' `include_inactive = TRUE` is the diagnostic escape hatch (never a reporting
#' option) — same contract as `cpportal_charged_days_filter`.
#'
#' ESTIMAND RULING — Tim, 2026-08-14. Verbatim:
#'
#'   "every ATT uses flat IVW over monitor-day effects (w = 1/se² per
#'    monitor-day cell, one pool, no s²_B between-monitor term, no D2a
#'    fallback, no per-monitor stage)"
#'
#' So on the FITTED basis this function now ALWAYS runs the flat inverse-
#' variance pool over whatever cell rows it is handed —
#'
#'   att = Σ(w·a) / Σw ,  se = sqrt(1 / Σw) ,  w = 1 / se_att²
#'
#' — tagged `se_source = "ivw_monitor_day_flat"`. The historical fitted D2b
#' branch (`varc_shared + varc_indep / n`) is retired to the rollback switch
#' `CPPORTAL_ATT_POOL=d2c`. Where only metro-day rows exist (fitted J = 1, or a
#' pin with no unit grain) the formula is identical math over those rows, so the
#' degradation is exact rather than approximate. ANCHORED paths are UNCHANGED:
#' the ruling covers the fitted monitor-day cells, and the M10 anchor has none.
#'
#' `rows` is a tibble of per-day / per-monitor-day rows already restricted to the
#' target metro and calendar window; `basis` is "fitted" or "anchored". Returns
#' a one-row tibble with `att`, `se_att`, `att_lo`, `att_hi`, `n_days`, `basis`,
#' plus optional diagnostic components (`se_source`, `varc_*`, `yhat0`, `pct`).
combine_att_window_d2b <- function(rows, basis = c("fitted", "anchored"),
                                   include_inactive = FALSE) {
  basis <- match.arg(basis)
  # Universal estimand cuts (fail-open): inactive weekdays + episode_flag==TRUE.
  rows <- cpportal_charged_days_filter(rows, include_inactive = include_inactive)
  rows <- cpportal_episode_days_filter(rows)
  if (is.null(rows) || nrow(rows) == 0L) {
    return(tibble::tibble(att = NA_real_, se_att = NA_real_,
                          att_lo = NA_real_, att_hi = NA_real_,
                          n_days = 0L, basis = basis,
                          yhat0 = NA_real_, pct = NA_real_,
                          se_source = NA_character_,
                          varc_shared = NA_real_, varc_indep = NA_real_))
  }
  numc <- function(x) suppressWarnings(as.numeric(as.character(x)))
  has  <- function(col) col %in% names(rows)

  att_vec <- numc(rows$att)
  se_vec  <- if (has("se_att")) numc(rows$se_att) else rep(NA_real_, length(att_vec))
  n <- length(att_vec)

  # D2b components — preferred when the pin carries them (anchored M10 path).
  vs <- if (has("varc_shared_md")) mean(numc(rows$varc_shared_md), na.rm = TRUE) else NA_real_
  vi <- if (has("varc_indep_md"))  mean(numc(rows$varc_indep_md),  na.rm = TRUE) else NA_real_
  se_d2b <- if (is.finite(vs) && is.finite(vi)) sqrt(vs + vi / n) else NA_real_

  se_d2a <- if (any(is.finite(se_vec))) mean(se_vec, na.rm = TRUE) else NA_real_
  se_ivw <- NA_real_
  se_source <- NA_character_

  if (identical(basis, "anchored")) {
    att_point <- .huber_location(att_vec)
    se <- dplyr::coalesce(se_d2b, se_d2a)
    se_source <- dplyr::case_when(
      is.finite(se_d2b) ~ "d2b_shared_plus_indep_over_n",
      is.finite(se_d2a) ~ "d2a_mean_se_att",
      TRUE              ~ NA_character_
    )
  } else if (is.finite(se_d2b) && identical(cpportal_att_pool_mode(), "d2c")) {
    # ROLLBACK ONLY (CPPORTAL_ATT_POOL=d2c): fitted rows that carry varc_* keep
    # the pre-2026-08-14 D2b contract.
    att_point <- mean(att_vec, na.rm = TRUE)
    se <- se_d2b
    se_source <- "d2b_shared_plus_indep_over_n"
  } else {
    # FITTED PRODUCTION PATH (Tim's 2026-08-14 ruling): flat IVW over the cell
    # rows handed in — monitor-day when the caller has them, metro-day when it
    # does not (identical math). No D2a fallback, no per-monitor stage.
    iv <- inverse_variance_mean(att_vec, se_vec)
    att_point <- iv$att
    se <- iv$se
    se_ivw <- iv$se
    if (iv$n > 0L) n <- as.integer(iv$n)
    se_source <- if (!is.finite(se_ivw)) {
      NA_character_
    } else if (identical(cpportal_att_pool_mode(), "d2c")) {
      "ivw_day_pool"
    } else {
      "ivw_monitor_day_flat"
    }
  }

  # PERCENT CHANGE vs the counterfactual (added 2026-08-03). Same definition the
  # per-metro-overall pool uses (`pct = att / mean_yhat0 * 100`, this file's
  # anchored branch): the native counterfactual level is summarised over the
  # SAME per-day rows that produced `att_point`, with the SAME location
  # estimator — plain mean for fitted, Huber M-estimate for anchored — so the
  # ratio's numerator and denominator describe the same cells and the percent
  # stays consistent with the robust point estimate. Pins that carry no `yhat0`
  # (or a degenerate ~0 counterfactual) yield NA, never a silent 0.
  yhat0_vec   <- if (has("yhat0")) numc(rows$yhat0) else rep(NA_real_, length(att_vec))
  yhat0_point <- if (!any(is.finite(yhat0_vec))) {
    NA_real_
  } else if (identical(basis, "anchored")) {
    .huber_location(yhat0_vec)
  } else {
    mean(yhat0_vec, na.rm = TRUE)
  }
  pct_change <- if (is.finite(yhat0_point) && abs(yhat0_point) > 1e-9 &&
                    is.finite(att_point)) {
    att_point / yhat0_point * 100
  } else NA_real_

  tibble::tibble(
    att      = att_point,
    se_att   = se,
    att_lo   = att_point - 1.96 * se,
    att_hi   = att_point + 1.96 * se,
    n_days   = as.integer(n),
    basis    = basis,
    # Native counterfactual level over the same cells, and ATT as a % of it.
    yhat0    = yhat0_point,
    pct      = pct_change,
    # Diagnostics — surface which SE branch fired so consumers can see it.
    se_source = se_source,
    varc_shared = vs,
    varc_indep  = vi
  )
}

#' Monitor id column on a per-monitor-day (unit-grain) ATT tibble.
#' The pin uses `fullaqsid` for both the fitted (`per_day_metro_unit`) and the
#' anchored (`per_day_metro_unit_imputed`) grain; the alternates are tolerated
#' so an older or hand-built tibble still resolves instead of silently
#' collapsing every row into one pseudo-monitor.
.cpportal_unit_id_col <- function(rows) {
  for (cand in c("fullaqsid", "monitor_id", "unit", "unit_id", "id")) {
    if (cand %in% names(rows)) return(cand)
  }
  NA_character_
}

#' D2c window aggregation — D2b plus a BETWEEN-MONITOR variance component.
#'
#' Motivation (2026-07-30 London audit, `docs/model/LONDON_ESTIMANDS_PLAN.md`):
#' D2b's se² = mean(varc_shared_md) + mean(varc_indep_md)/n_days describes the
#' uncertainty of ONE monitor's anchored level plus day-to-day noise. It says
#' nothing about how far the monitors DISAGREE with each other. In London's
#' 2008-2011 anchored window the two monitors' window ATTs differ by 4.6 µg/m³
#' (ERG-BL0 +2.95 vs ERG-MY7 +7.57 over the same days) while the D2b SE is 0.91
#' — an interval that excludes both of its own constituents. D2c adds that
#' dispersion back:
#'
#'   se²_window = varc_shared + varc_indep / n_days + s²_B / J
#'
#' where, over the J monitors present in the same metro+window at the per-
#' monitor-day grain:
#'
#'   m_j        = Huber M-estimate of that monitor's daily `att` (same robust
#'                rule the window point estimate uses for anchored rows)
#'   w_j        = that monitor's monitor-day count in the window
#'   m_bar      = sum(w_j * m_j) / sum(w_j)                     (w-weighted mean)
#'   s²_B       = sum(w_j * (m_j - m_bar)²) /
#'                (sum(w_j) - sum(w_j²) / sum(w_j))
#'
#' The denominator is the reliability-weights ("frequency-free") unbiased
#' correction — it reduces to the usual (n-1) divisor when all w_j are equal,
#' and it keeps a monitor with a handful of days from inflating the spread as
#' much as a monitor with a decade of them. **At J = 2 this estimator is noisy
#' by construction**: it is a one-degree-of-freedom variance, so the resulting
#' SE should be read as "at least this wide", not as a precise number. That is
#' the intended posture — an additive s²_B is deliberately conservative, since
#' it partly double-counts the gap model's own claimed per-monitor error AND
#' absorbs genuine spatial effect heterogeneity (a kerbside site really can
#' respond differently from a background one). Both readings argue for a wider
#' interval, so we take the wider interval.
#'
#' Degradation contract: with `unit_rows` NULL/empty, or fewer than 2 distinct
#' monitors, or a non-finite s²_B, the D2b result is returned **unchanged**
#' (same `se_att`, same `se_source`) with only the additive diagnostic columns
#' appended. Pins predating the per-monitor rows therefore behave exactly as
#' they do today.
#'
#' @param rows      per-monitor-day (or metro-day) rows for the metro+window,
#'                  as passed to `combine_att_window_d2b`.
#' @param unit_rows per-MONITOR-day rows for the SAME metro+window
#'                  (`per_day_metro_unit` for fitted, `per_day_metro_unit_imputed`
#'                  for anchored). NULL when the pin has none.
#' @param basis     "fitted" or "anchored".
#' @param include_inactive Diagnostic escape hatch (see `combine_att_window_d2b`).
#' @return the `combine_att_window_d2b` tibble plus `n_monitors` (integer; 0
#'   when unknown), `s2_between` (NA when the term was omitted), and
#'   `se_source == "d2c_shared_indep_between"` when the term was applied.
combine_att_window_d2c <- function(rows, unit_rows = NULL,
                                   basis = c("fitted", "anchored"),
                                   include_inactive = FALSE) {
  basis <- match.arg(basis)
  flat  <- identical(cpportal_att_pool_mode(), "ivw_flat") &&
           identical(basis, "fitted")
  # ESTIMAND RULING (Tim, 2026-08-14): on the fitted basis the pool is ONE flat
  # IVW over MONITOR-DAY cells. When the caller supplied the unit grain, those
  # cells ARE the pool — not the metro-day collapse. `s2_between` / `n_monitors`
  # keep being computed below, but as DISCLOSURE ONLY; the SE no longer carries
  # the s²_B/J term.
  pool_rows <- if (flat && is.data.frame(unit_rows) && nrow(unit_rows) > 0L) {
    unit_rows
  } else rows
  base  <- combine_att_window_d2b(pool_rows, basis = basis,
                                  include_inactive = include_inactive)

  add_cols <- function(x, n_monitors, s2_between) {
    x$n_monitors <- as.integer(n_monitors)
    x$s2_between <- as.numeric(s2_between)
    x
  }

  if (is.null(unit_rows) || !is.data.frame(unit_rows) || nrow(unit_rows) == 0L) {
    return(add_cols(base, 0L, NA_real_))
  }
  # Same estimand cut on the between-monitor grain (idempotent if the caller
  # already charged-filtered; required for /window which previously did not).
  unit_rows <- cpportal_charged_days_filter(unit_rows,
                                            include_inactive = include_inactive)
  unit_rows <- cpportal_episode_days_filter(unit_rows)
  if (is.null(unit_rows) || nrow(unit_rows) == 0L) {
    return(add_cols(base, 0L, NA_real_))
  }
  id_col <- .cpportal_unit_id_col(unit_rows)
  if (is.na(id_col)) return(add_cols(base, 0L, NA_real_))

  numc <- function(x) suppressWarnings(as.numeric(as.character(x)))
  u <- tibble::tibble(
    .uid = as.character(unit_rows[[id_col]]),
    .att = numc(unit_rows$att)
  )
  u <- u[is.finite(u$.att) & !is.na(u$.uid) & nzchar(u$.uid), , drop = FALSE]
  if (nrow(u) == 0L) return(add_cols(base, 0L, NA_real_))

  per_mon <- u |>
    dplyr::group_by(.data$.uid) |>
    dplyr::summarise(m_j = .huber_location(.data$.att),
                     w_j = dplyr::n(), .groups = "drop") |>
    dplyr::filter(is.finite(.data$m_j), .data$w_j > 0)

  J <- nrow(per_mon)
  if (J < 2L) return(add_cols(base, J, NA_real_))

  w  <- as.numeric(per_mon$w_j)
  m  <- as.numeric(per_mon$m_j)
  sw <- sum(w)
  denom <- sw - sum(w^2) / sw          # reliability-weights unbiased correction
  m_bar <- sum(w * m) / sw
  s2_b  <- if (is.finite(denom) && denom > 0) sum(w * (m - m_bar)^2) / denom else NA_real_

  if (!is.finite(s2_b) || !is.finite(base$se_att)) {
    return(add_cols(base, J, s2_b))
  }
  # DISCLOSURE-ONLY under the ruling: report J and s²_B, do NOT widen the SE.
  if (flat) return(add_cols(base, J, s2_b))

  se_new <- sqrt(base$se_att^2 + s2_b / J)
  out <- base
  out$se_att    <- se_new
  out$att_lo    <- out$att - 1.96 * se_new
  out$att_hi    <- out$att + 1.96 * se_new
  out$se_source <- "d2c_shared_indep_between"
  add_cols(out, J, s2_b)
}

#' Hybrid calendar-window ATT — ONE estimate pooling BOTH bases.
#'
#' Motivation (2026-07-30 London audit): a window like CCZ 2003→2011 contains
#' fect-fitted monitor-days (KC1/KC2, identified by the Western Extension
#' removal) AND M7 satellite-anchored monitor-days (the continuously-treated
#' cordon monitors fect had to drop). `/window`'s auto behaviour prefers the
#' fitted pool and silently discards the anchored one whenever any fitted row
#' overlaps the window; this combiner keeps both. Each pool is first combined
#' with `combine_att_window_d2b` (so the anchored shared variance keeps its
#' no-√n-shrink treatment — nothing here re-shrinks it), then the two pools are
#' pooled with monitor-day weights (sum of per-row `n_effects`, 1 per row when
#' absent), treating their errors as independent (fect sampling noise vs
#' anchor/gap prediction error):
#'
#'   att_h = w_f * att_f + w_a * att_a,   w_x = md_x / (md_f + md_a)
#'   se_h  = sqrt(w_f^2 * se_f^2 + w_a^2 * se_a^2)
#'
#' Returns the combine_att_window_d2b shape plus per-pool components, so
#' consumers can always print both sub-estimates next to the pooled number —
#' never a hybrid figure without its parts. If only one pool has rows, the
#' pooled number equals that pool (weight 1) and the components say so. An SE
#' is NA whenever a contributing pool's SE is NA (no silent zero-variance).
#'
#' Each pool's sub-combine runs through `combine_att_window_d2c`, so a pool with
#' J >= 2 monitors in the window carries its between-monitor dispersion into
#' `se_f` / `se_a` before the w² pooling below. The pooling formula itself is
#' unchanged; the per-pool components gain `n_monitors_fitted` /
#' `n_monitors_anchored`. Pass the matching per-monitor-day rows
#' (`per_day_metro_unit` / `per_day_metro_unit_imputed`) for the same
#' metro+window; omit them and every pool degrades to plain D2b.
combine_att_window_hybrid <- function(fitted_rows, anchored_rows,
                                      fitted_unit_rows = NULL,
                                      anchored_unit_rows = NULL) {
  md_of <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0L) return(0)
    ne <- if ("n_effects" %in% names(rows)) {
      suppressWarnings(as.numeric(as.character(rows$n_effects)))
    } else {
      rep(NA_real_, nrow(rows))
    }
    sum(dplyr::coalesce(ne, 1), na.rm = TRUE)
  }
  f <- combine_att_window_d2c(fitted_rows, fitted_unit_rows, basis = "fitted")
  a <- combine_att_window_d2c(anchored_rows, anchored_unit_rows, basis = "anchored")
  md_f <- if (is.finite(f$att)) md_of(fitted_rows) else 0
  md_a <- if (is.finite(a$att)) md_of(anchored_rows) else 0
  if (md_f + md_a <= 0) {
    return(tibble::tibble(
      att = NA_real_, se_att = NA_real_, att_lo = NA_real_, att_hi = NA_real_,
      n_days = 0L, basis = "hybrid", se_source = NA_character_,
      w_fitted = NA_real_, w_anchored = NA_real_,
      md_fitted = 0, md_anchored = 0,
      att_fitted = NA_real_, se_fitted = NA_real_, n_days_fitted = 0L,
      att_anchored = NA_real_, se_anchored = NA_real_, n_days_anchored = 0L,
      n_monitors_fitted = 0L, n_monitors_anchored = 0L,
      yhat0 = NA_real_, pct = NA_real_
    ))
  }
  w_f <- md_f / (md_f + md_a)
  w_a <- 1 - w_f
  att <- w_f * (if (is.finite(f$att)) f$att else 0) +
         w_a * (if (is.finite(a$att)) a$att else 0)
  se <- if ((w_f > 0 && !is.finite(f$se_att)) || (w_a > 0 && !is.finite(a$se_att))) {
    NA_real_
  } else {
    sqrt(w_f^2 * (if (w_f > 0) f$se_att^2 else 0) +
         w_a^2 * (if (w_a > 0) a$se_att^2 else 0))
  }
  .yh <- {
    .yf <- suppressWarnings(as.numeric(f$yhat0)); .ya <- suppressWarnings(as.numeric(a$yhat0))
    .v <- (if (w_f > 0 && is.finite(.yf)) w_f * .yf else 0) +
          (if (w_a > 0 && is.finite(.ya)) w_a * .ya else 0)
    if ((w_f > 0 && !is.finite(.yf)) || (w_a > 0 && !is.finite(.ya))) NA_real_ else .v
  }
  .pct_h <- if (is.finite(.yh) && abs(.yh) > 1e-9 && is.finite(att)) att / .yh * 100 else NA_real_
  tibble::tibble(
    att = att, se_att = se,
    att_lo = att - 1.96 * se, att_hi = att + 1.96 * se,
    n_days = as.integer(f$n_days + a$n_days),
    basis = "hybrid",
    se_source = "hybrid_monitor_day_weighted",
    w_fitted = w_f, w_anchored = w_a,
    md_fitted = md_f, md_anchored = md_a,
    att_fitted = f$att, se_fitted = f$se_att, n_days_fitted = f$n_days,
    att_anchored = a$att, se_anchored = a$se_att, n_days_anchored = a$n_days,
    n_monitors_fitted = as.integer(f$n_monitors),
    n_monitors_anchored = as.integer(a$n_monitors),
    # Counterfactual level blended with the SAME monitor-day weights as `att`,
    # so the hybrid percent is the hybrid ATT over the hybrid baseline rather
    # than a mix of two different denominators.
    yhat0 = .yh, pct = .pct_h
  )
}

#' Filter bundle `att` tibble by pollutant (same rules as fetch_att_results).
cross_city_filter_bundle_att <- function(att, pollutant = "PM2.5") {
  if (is.null(att) || nrow(att) == 0) {
    return(tibble::tibble())
  }
  att_cols <- names(att)
  out <- att
  if ("parameter" %in% att_cols) {
    out <- out |> dplyr::filter(.data$parameter == !!pollutant)
  }
  if ("pollutant" %in% att_cols) {
    out <- out |> dplyr::filter(.data$pollutant == !!pollutant)
  }
  if ("partition_key" %in% att_cols) {
    pk <- .cpportal_partition_key(pollutant)
    out <- out |> dplyr::filter(.data$partition_key == !!pk)
  }
  out
}

#' Earliest treated policy start per metro (calendar adoption date).
fetch_congestion_treatment_starts <- function(conn, metro_ids) {
  if (is.null(conn) || length(metro_ids) == 0L) {
    return(tibble::tibble(
      metro_id = integer(),
      treatment_start_date = as.Date(character())
    ))
  }
  # as.integer() already makes injection impossible here, but a non-numeric id
  # coerces to NA and would be interpolated as the literal token `NA` -> a SQL
  # syntax error. Drop NAs and bind the rest as a parameter.
  mids <- unique(stats::na.omit(as.integer(metro_ids)))
  if (length(mids) == 0L) {
    return(tibble::tibble(
      metro_id = integer(),
      treatment_start_date = as.Date(character())
    ))
  }
  q <- paste0(
    "SELECT metro_id, MIN(start_date)::date AS treatment_start_date ",
    "FROM public.congestion_pricing_periods ",
    "WHERE treated IS TRUE AND metro_id = ANY($1::int[]) ",
    "GROUP BY metro_id"
  )
  ids_arr <- paste0("{", paste(mids, collapse = ","), "}")
  DBI::dbGetQuery(conn, q, params = list(ids_arr)) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      metro_id = as.integer(.data$metro_id),
      treatment_start_date = as.Date(.data$treatment_start_date)
    )
}

# Anchored cordon metros: event-time % trajectories from per_day_metro_imputed.
# Fitted (NYC, London CCZ): per_day_metro. London stays on fitted rows here;
# projection.R uses imputed for London in the pooled anniversary model only.
CROSS_CITY_ANCHORED_TRAJ_METRO_IDS <- c(949L, 950L, 955L)
LONDON_CCZ_METRO_ID <- 943L
LONDON_ULEZ_METRO_ID <- 94301L
LONDON_ULEZ_START_DATE <- as.Date("2019-04-08")

#' London CCZ+ULEZ era daily rows (event-time 0 = ULEZ adoption), from imputed
#' per-day ATTs windowed to the ULEZ era — matches projection.R `$lonulez`.
build_london_ulez_cache_rows <- function(bundle, pollutant, outcome = "aq_daily_mean",
                                         metro_lookup = NULL) {
  att <- bundle$att
  att <- cross_city_filter_bundle_att(att, pollutant)
  ulez <- att |>
    dplyr::filter(
      .data$type == "per_day_metro_imputed",
      .data$metro_id == !!LONDON_CCZ_METRO_ID
    ) |>
    dplyr::mutate(
      metro_id = LONDON_ULEZ_METRO_ID,
      day = as.Date(.data$day),
      treatment_start_date = LONDON_ULEZ_START_DATE,
      basis = "observed"
    ) |>
    dplyr::filter(.data$day >= LONDON_ULEZ_START_DATE)
  if (nrow(ulez) == 0L) {
    return(tibble::tibble())
  }
  se_col <- if ("se_att" %in% names(ulez)) {
    "se_att"
  } else if ("se" %in% names(ulez)) {
    "se"
  } else {
    NULL
  }
  if (is.null(se_col)) {
    ulez <- ulez |> dplyr::mutate(se_att = NA_real_)
  } else if (!identical(se_col, "se_att")) {
    ulez <- ulez |>
      dplyr::mutate(se_att = .data[[se_col]]) |>
      dplyr::select(-dplyr::all_of(se_col))
  }
  ne_col <- if ("n_effects" %in% names(ulez)) "n_effects" else NULL
  ulez <- ulez |>
    dplyr::mutate(
      event_day = as.integer(.data$day - .data$treatment_start_date),
      metro_name = "London — CCZ+ULEZ era",
      iso_id = NA_character_
    )
  if (!is.null(metro_lookup) && nrow(metro_lookup) > 0) {
    lon_row <- metro_lookup |>
      dplyr::filter(.data$metro_id == !!LONDON_CCZ_METRO_ID) |>
      dplyr::slice_head(n = 1L)
    if (nrow(lon_row) > 0L && "iso_id" %in% names(lon_row)) {
      ulez$iso_id <- lon_row$iso_id[[1]]
    }
  }
  if (!is.null(ne_col) && !identical(ne_col, "n_effects")) {
    ulez <- ulez |>
      dplyr::mutate(n_effects = .data[[ne_col]]) |>
      dplyr::select(-dplyr::all_of(ne_col))
  } else if (is.null(ne_col)) {
    ulez <- ulez |> dplyr::mutate(n_effects = NA_integer_)
  }
  if (!"pct_change" %in% names(ulez)) {
    ulez <- ulez |> dplyr::mutate(pct_change = NA_real_)
  } else {
    ulez <- ulez |>
      dplyr::mutate(pct_change = suppressWarnings(as.numeric(.data$pct_change)))
  }
  if (!"yhat0" %in% names(ulez)) {
    ulez <- ulez |> dplyr::mutate(yhat0 = NA_real_)
  } else {
    ulez <- ulez |>
      dplyr::mutate(yhat0 = suppressWarnings(as.numeric(.data$yhat0)))
  }
  if (!"yhat1" %in% names(ulez)) {
    ulez <- ulez |> dplyr::mutate(yhat1 = NA_real_)
  } else {
    ulez <- ulez |>
      dplyr::mutate(yhat1 = suppressWarnings(as.numeric(.data$yhat1)))
  }
  if (!"att" %in% names(ulez)) {
    ulez <- ulez |> dplyr::mutate(att = NA_real_)
  } else {
    ulez <- ulez |>
      dplyr::mutate(att = suppressWarnings(as.numeric(.data$att)))
  }
  yhatse0 <- if ("yhatse0" %in% names(ulez)) {
    suppressWarnings(as.numeric(ulez$yhatse0))
  } else {
    rep(NA_real_, nrow(ulez))
  }
  ulez <- ulez |>
    dplyr::mutate(
      yhatse0 = yhatse0,
      pct_change = dplyr::if_else(
        is.finite(.data$pct_change),
        .data$pct_change,
        dplyr::if_else(
          is.finite(.data$yhat0) & abs(.data$yhat0) > 0 & is.finite(.data$att),
          100 * .data$att / .data$yhat0,
          NA_real_
        )
      ),
      yhat0_lo = dplyr::if_else(
        is.finite(.data$yhat0) & is.finite(.data$yhatse0),
        .data$yhat0 - 1.96 * .data$yhatse0,
        NA_real_
      ),
      yhat0_hi = dplyr::if_else(
        is.finite(.data$yhat0) & is.finite(.data$yhatse0),
        .data$yhat0 + 1.96 * .data$yhatse0,
        NA_real_
      )
    )
  outcome_col <- as.character(outcome %||% "aq_daily_mean")
  dmin <- min(ulez$day, na.rm = TRUE)
  dmax <- max(ulez$day, na.rm = TRUE)
  panel_obs <- NULL
  if (is.finite(dmin) && is.finite(dmax) && exists("get_panel_data", mode = "function")) {
    panel_obs <- tryCatch(
      get_panel_data(LONDON_CCZ_METRO_ID, pollutant, dmin, dmax),
      error = function(e) NULL
    )
  }
  if (!is.null(panel_obs) && nrow(panel_obs) > 0L && outcome_col %in% names(panel_obs)) {
    obs_daily <- panel_obs |>
      dplyr::filter(is.finite(.data[[outcome_col]])) |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        obs = mean(.data[[outcome_col]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(day = date)
    ulez <- ulez |> dplyr::left_join(obs_daily, by = "day")
  } else {
    ulez <- ulez |> dplyr::mutate(obs = NA_real_)
  }
  ulez <- ulez |>
    dplyr::mutate(
      obs = dplyr::if_else(
        is.finite(.data$obs),
        .data$obs,
        dplyr::if_else(
          is.finite(.data$yhat0) & is.finite(.data$att),
          .data$yhat0 + .data$att,
          NA_real_
        )
      )
    )
  # Propagate D2b variance components (present on newer imputed rows) so the
  # ULEZ slice can participate in windowed D2b combines. Absent → NA.
  if (!"varc_shared_md" %in% names(ulez)) {
    ulez <- ulez |> dplyr::mutate(varc_shared_md = NA_real_)
  } else {
    ulez <- ulez |>
      dplyr::mutate(varc_shared_md = suppressWarnings(as.numeric(.data$varc_shared_md)))
  }
  if (!"varc_indep_md" %in% names(ulez)) {
    ulez <- ulez |> dplyr::mutate(varc_indep_md = NA_real_)
  } else {
    ulez <- ulez |>
      dplyr::mutate(varc_indep_md = suppressWarnings(as.numeric(.data$varc_indep_md)))
  }
  ulez |>
    dplyr::select(
      "metro_id", "metro_name", "iso_id", "treatment_start_date",
      "day", "event_day", "att", "se_att",
      "varc_shared_md", "varc_indep_md",
      "pct_change", "obs",
      "yhat0", "yhat0_lo", "yhat0_hi", "yhat1", "n_effects", "basis"
    ) |>
    dplyr::arrange(.data$day)
}

#' Long cache: one row per metro × calendar day with event-time days since adoption.
build_event_time_att_cache <- function(bundle, pollutant, periods_df, metro_lookup = NULL,
                                       outcome = "aq_daily_mean") {
  .empty_event_time_cache <- function() {
    tibble::tibble(
      metro_id = integer(),
      metro_name = character(),
      iso_id = character(),
      treatment_start_date = as.Date(character()),
      day = as.Date(character()),
      event_day = integer(),
      att = double(),
      se_att = double(),
      varc_shared_md = double(),
      varc_indep_md = double(),
      pct_change = double(),
      obs = double(),
      yhat0 = double(),
      yhat0_lo = double(),
      yhat0_hi = double(),
      yhat1 = double(),
      n_effects = integer(),
      basis = character()
    )
  }

  att <- bundle$att
  att <- cross_city_filter_bundle_att(att, pollutant)
  fitted <- att |>
    dplyr::filter(.data$type == "per_day_metro") |>
    dplyr::mutate(basis = "fitted")
  imputed <- att |> dplyr::filter(.data$type == "per_day_metro_imputed")
  if (nrow(imputed) > 0L) {
    imputed <- imputed |>
      dplyr::mutate(metro_id = as.integer(.data$metro_id)) |>
      dplyr::filter(.data$metro_id %in% CROSS_CITY_ANCHORED_TRAJ_METRO_IDS) |>
      dplyr::mutate(basis = "anchored")
  }
  daily <- dplyr::bind_rows(fitted, imputed)
  if (nrow(daily) == 0L) {
    return(.empty_event_time_cache())
  }
  if (!"day" %in% names(daily)) {
    stop("cross_city_att: bundle att daily rows missing `day` column")
  }
  se_col <- if ("se_att" %in% names(daily)) "se_att" else if ("se" %in% names(daily)) "se" else NULL
  if (is.null(se_col)) {
    daily <- daily |> dplyr::mutate(se_att = NA_real_)
  } else if (!identical(se_col, "se_att")) {
    daily <- daily |>
      dplyr::mutate(se_att = .data[[se_col]]) |>
      dplyr::select(-dplyr::all_of(se_col))
  }
  ne_col <- if ("n_effects" %in% names(daily)) "n_effects" else NULL
  daily <- daily |>
    dplyr::mutate(
      metro_id = as.integer(.data$metro_id),
      day = as.Date(.data$day)
    ) |>
    dplyr::left_join(periods_df, by = "metro_id") |>
    dplyr::mutate(
      event_day = as.integer(.data$day - .data$treatment_start_date)
    ) |>
    dplyr::filter(!is.na(.data$treatment_start_date))

  if (!is.null(metro_lookup) && nrow(metro_lookup) > 0) {
    lk <- metro_lookup |>
      dplyr::transmute(
        metro_id = as.integer(.data$metro_id),
        metro_name = as.character(.data$metro_name %||% ""),
        iso_id = .data$iso_id
      )
    daily <- daily |>
      dplyr::left_join(lk, by = "metro_id") |>
      dplyr::mutate(
        metro_name = dplyr::if_else(
          is.na(.data$metro_name) | !nzchar(.data$metro_name),
          paste0("Metro ", .data$metro_id),
          .data$metro_name
        )
      )
  } else {
    daily <- daily |>
      dplyr::mutate(
        metro_name = paste0("Metro ", .data$metro_id),
        iso_id = NA_character_
      )
  }
        if (!is.null(ne_col)) {
    if (!identical(ne_col, "n_effects")) {
      daily <- daily |>
        dplyr::mutate(n_effects = .data[[ne_col]]) |>
        dplyr::select(-dplyr::all_of(ne_col))
    }
  } else {
    daily <- daily |> dplyr::mutate(n_effects = NA_integer_)
  }
  if (!"pct_change" %in% names(daily)) {
    daily <- daily |> dplyr::mutate(pct_change = NA_real_)
  } else {
    daily <- daily |>
      dplyr::mutate(pct_change = suppressWarnings(as.numeric(.data$pct_change)))
  }
  if (!"yhat0" %in% names(daily)) {
    daily <- daily |> dplyr::mutate(yhat0 = NA_real_)
  } else {
    daily <- daily |>
      dplyr::mutate(yhat0 = suppressWarnings(as.numeric(.data$yhat0)))
  }
  if (!"yhat1" %in% names(daily)) {
    daily <- daily |> dplyr::mutate(yhat1 = NA_real_)
  } else {
    daily <- daily |>
      dplyr::mutate(yhat1 = suppressWarnings(as.numeric(.data$yhat1)))
  }
  yhatse0 <- if ("yhatse0" %in% names(daily)) {
    suppressWarnings(as.numeric(daily$yhatse0))
  } else {
    rep(NA_real_, nrow(daily))
  }
  daily <- daily |>
    dplyr::mutate(
      yhatse0 = yhatse0,
      pct_change = dplyr::if_else(
        is.finite(.data$pct_change),
        .data$pct_change,
        dplyr::if_else(
          is.finite(.data$yhat0) & abs(.data$yhat0) > 0 & is.finite(.data$att),
          100 * .data$att / .data$yhat0,
          NA_real_
        )
      ),
      yhat0_lo = dplyr::if_else(
        is.finite(.data$yhat0) & is.finite(.data$yhatse0),
        .data$yhat0 - 1.96 * .data$yhatse0,
        NA_real_
      ),
      yhat0_hi = dplyr::if_else(
        is.finite(.data$yhat0) & is.finite(.data$yhatse0),
        .data$yhat0 + 1.96 * .data$yhatse0,
        NA_real_
      )
    )
  outcome_col <- as.character(outcome %||% "aq_daily_mean")
  mids <- unique(daily$metro_id)
  dmin <- min(daily$day, na.rm = TRUE)
  dmax <- max(daily$day, na.rm = TRUE)
  panel_obs <- NULL
  if (length(mids) > 0L && is.finite(dmin) && is.finite(dmax) &&
      exists("get_panel_data", mode = "function")) {
    panel_obs <- tryCatch(
      get_panel_data(mids, pollutant, dmin, dmax),
      error = function(e) NULL
    )
  }
  if (!is.null(panel_obs) && nrow(panel_obs) > 0L && outcome_col %in% names(panel_obs)) {
    obs_daily <- panel_obs |>
      dplyr::filter(is.finite(.data[[outcome_col]])) |>
      dplyr::group_by(.data$metro_id, .data$date) |>
      dplyr::summarise(
        obs = mean(.data[[outcome_col]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::rename(day = .data$date)
    daily <- daily |>
      dplyr::left_join(obs_daily, by = c("metro_id", "day"))
  } else {
    daily <- daily |> dplyr::mutate(obs = NA_real_)
  }
  daily <- daily |>
    dplyr::mutate(
      obs = dplyr::if_else(
        is.finite(.data$obs),
        .data$obs,
        dplyr::if_else(
          is.finite(.data$yhat0) & is.finite(.data$att),
          .data$yhat0 + .data$att,
          NA_real_
        )
      )
    )
  ulez <- build_london_ulez_cache_rows(bundle, pollutant, outcome, metro_lookup)
  if (nrow(ulez) > 0L) {
    daily <- dplyr::bind_rows(daily, ulez)
  }
  # Carry the D2b native-scale variance components through the cache so
  # windowed anchored aggregations can use the shared/√n-shrink split at any
  # sub-window (matches `/window` and `/att` monthly). Older pins lacking the
  # columns → NA and the D2b branch skips, falling back to D2a.
  if (!"varc_shared_md" %in% names(daily)) {
    daily <- daily |> dplyr::mutate(varc_shared_md = NA_real_)
  } else {
    daily <- daily |>
      dplyr::mutate(varc_shared_md = suppressWarnings(as.numeric(.data$varc_shared_md)))
  }
  if (!"varc_indep_md" %in% names(daily)) {
    daily <- daily |> dplyr::mutate(varc_indep_md = NA_real_)
  } else {
    daily <- daily |>
      dplyr::mutate(varc_indep_md = suppressWarnings(as.numeric(.data$varc_indep_md)))
  }
  daily |>
    dplyr::select(
      "metro_id", "metro_name", "iso_id", "treatment_start_date",
      "day", "event_day", "att", "se_att",
      "varc_shared_md", "varc_indep_md",
      "pct_change", "obs",
      "yhat0", "yhat0_lo", "yhat0_hi", "yhat1", "n_effects", "basis"
    ) |>
    dplyr::arrange(.data$metro_id, .data$day)
}

cross_city_fingerprint <- function(spec_id, outcome, pollutant, bundle, periods_df) {
  ui <- bundle$ui_metadata %||% list()
  run_id <- as.character(ui$run_id %||% ui$trained_run %||% "")
  digest_vec <- paste(
    spec_id, outcome, pollutant, run_id,
    format(bundle$trained_at %||% NA, usetz = TRUE),
    paste(periods_df$metro_id %||% integer(0), collapse = ","),
    paste(as.character(periods_df$treatment_start_date), collapse = ","),
    sep = "|"
  )
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(digest_vec, algo = "xxhash64"))
  }
  digest_vec
}

#' Build or return TTL-cached event-time tibble for FECT
#' `spec_id` x `outcome` x `pollutant`. `outcome` defaults to the canonical
#' `aq_daily_mean` bundle (matches legacy callers); other outcomes resolve to
#' the corresponding `fect_{spec_id}_{outcome}` pin via `load_fect_bundle()`.
get_cross_city_cache <- function(spec_id = "all_priority",
                                 pollutant = "PM2.5",
                                 conn = NULL,
                                 refresh = FALSE,
                                 outcome = NULL,
                                 scope = "zone") {
  if (is.null(conn) && isTRUE(cpportal_use_db())) {
    conn <- tryCatch(get_db(), error = function(e) NULL)
  }
  outcome <- if (exists("validate_fect_outcome", mode = "function")) {
    tryCatch(validate_fect_outcome(outcome), error = function(e) "aq_daily_mean")
  } else {
    outcome %||% "aq_daily_mean"
  }
  # scope selects the zone (cordon) vs metro (whole-metro) pin. The cache key
  # MUST include it — the two pins share spec/outcome/pollutant and would
  # otherwise collide in the TTL cache.
  scope <- if (identical(tolower(scope %||% "zone"), "metro")) "metro" else "zone"
  bundle <- tryCatch(load_fect_bundle(spec_id, outcome = outcome, scope = scope),
                     error = function(e) NULL)
  if (is.null(bundle) || is.null(bundle$att) || nrow(bundle$att) == 0) {
    return(NULL)
  }
  mids <- bundle_spec_metro_ids(bundle, model_type = "synth", spec_id = spec_id)
  if (length(mids) == 0L) {
    return(NULL)
  }
  periods <- fetch_congestion_treatment_starts(conn, mids)
  fp <- cross_city_fingerprint(spec_id, outcome, pollutant, bundle, periods)
  key <- paste(spec_id, outcome, pollutant, scope, fp, sep = "/")
  now <- Sys.time()
  if (!isTRUE(refresh) && exists(key, envir = .cpportal_cross_city_cache_env, inherits = FALSE)) {
    entry <- get(key, envir = .cpportal_cross_city_cache_env)
    age <- as.numeric(difftime(now, entry$built_at, units = "secs"))
    if (age < .cpportal_cross_city_ttl_secs()) {
      log_fetch("cross_city_att", source = "cache", spec = spec_id,
                outcome = outcome, pollutant = pollutant,
                age_secs = round(age, 1))
      return(entry$df)
    }
  }
  metros <- tryCatch(fetch_metros(priority_only = FALSE), error = function(e) NULL)
  metro_lookup <- if (!is.null(metros) && nrow(metros) > 0) {
    # `fetch_metros()` strips `iso_id` from its public projection; fall back to
    # NA when the column is absent so the join in build_event_time_att_cache()
    # still tags rows with `metro_name` even without country code.
    lk <- metros |>
      dplyr::transmute(
        metro_id = as.integer(.data$metro_id),
        metro_name = as.character(.data$metro_name)
      )
    lk$iso_id <- if ("iso_id" %in% names(metros)) metros$iso_id else NA_character_
    lk
  } else {
    NULL
  }
  cache_df <- build_event_time_att_cache(
    bundle, pollutant, periods, metro_lookup, outcome = outcome
  )
  assign(
    key,
    list(df = cache_df, built_at = now, fingerprint = fp, spec_id = spec_id,
         outcome = outcome, pollutant = pollutant),
    envir = .cpportal_cross_city_cache_env
  )
  log_fetch("cross_city_att", source = "pin+db", spec = spec_id,
            outcome = outcome, pollutant = pollutant, rows = nrow(cache_df))
  cache_df
}

cross_city_invalidate_cache <- function() {
  rm(list = ls(envir = .cpportal_cross_city_cache_env),
     envir = .cpportal_cross_city_cache_env)
  invisible(TRUE)
}

#' Event-day bounds for a window kind.
#'
#' `point`       — the 365-day band centred on N x 365 (+/- 182 days).
#' `cumulative`  — 0 .. N x 365.
#' `all`         — THE HEADLINE WINDOW: every post-treatment event day, no upper
#'                 bound. `n_years` is ignored (and may be NA), because "all the
#'                 treated days a city has" is not a horizon. This is the window
#'                 the paper's headline (Table 1 / Table 2) row lives on, and it
#'                 exists so that the headline pool is the SAME code path as the
#'                 per-year pools rather than a second estimator that happens to
#'                 agree. Additive: `point` / `cumulative` are untouched, so no
#'                 served per-year number can move.
cross_city_window_bounds <- function(n_years, window_kind) {
  if (identical(window_kind, "all")) {
    return(c(lo = 0L, hi = .Machine$integer.max))
  }
  n_years <- as.numeric(n_years)
  nd <- floor(n_years * 365)
  if (identical(window_kind, "cumulative")) {
    return(c(lo = 0L, hi = as.integer(nd)))
  }
  half <- 182L
  center <- as.integer(nd)
  c(lo = max(0L, center - half), hi = center + half)
}

inverse_variance_mean <- function(att, se) {
  ok <- !is.na(att) & !is.na(se) & se > 0 & is.finite(att)
  if (!any(ok)) {
    return(list(att = NA_real_, se = NA_real_, n = 0L))
  }
  att <- att[ok]
  se <- se[ok]
  w <- 1 / (se^2)
  mu <- sum(w * att) / sum(w)
  se_out <- sqrt(1 / sum(w))
  list(att = mu, se = se_out, n = length(att))
}

#' Windowed per-metro ATT — SE aggregation is basis-appropriate.
#'
#' Anchored cities: route through `combine_att_window_d2b` so the sub-window SE
#' uses the D2b variance split (shared anchor error does NOT √n-shrink, per-day
#' independent term does) — the same math powering `/window`, `/att` monthly,
#' and `per_metro_overall_att`. Falls back to D2a (mean of per-day `se_att`)
#' inside the combine when varc_shared/indep are absent. The point estimate
#' uses the Huber M-estimator for the heavy-tailed anchored per-day ATTs.
#'
#' Fitted cities: inverse-variance-weighted pooling — per-day predictive SEs on
#' fitted rows are approximately independent, so IVW is the right pool even at
#' J = 1. `/window` now uses the same IVW branch inside `combine_att_window_d2b`
#' when D2b `varc_*` are missing (Tim 2026-08-12; rejects D2a for fitted).
metro_window_estimate <- function(cache, metro_id, n_years, window_kind,
                                  include_inactive = FALSE) {
  b <- cross_city_window_bounds(n_years, window_kind)
  mid <- as.integer(metro_id)
  sub <- cache |>
    dplyr::filter(
      .data$metro_id == mid,
      .data$event_day >= b[["lo"]],
      .data$event_day <= b[["hi"]]
    ) |>
    # THE UNIVERSAL ESTIMAND (Tim, 2026-08-03): inactive (non-charging) days
    # leave the pool for EVERY basis -- fitted rows included -- so an anchored
    # city's ATT and a fitted city's ATT answer the same question. Applied to
    # the ROWS FED IN; the D2b/IVW combiners below are untouched.
    cpportal_charged_days_filter(include_inactive = include_inactive) |>
    # Episode exclusion (fail-open if episode_flag absent): wildfire/smoke
    # episode days leave the pool the same way charged-days do.
    cpportal_episode_days_filter()
  if (nrow(sub) == 0L) {
    return(list(att = NA_real_, se = NA_real_, n_days = 0L,
                basis = NA_character_, yhat0 = NA_real_, pct = NA_real_))
  }
  # ONE basis per window, chosen deterministically, and the pool sees ONLY that
  # basis's rows. Before 2026-08-07 this took `unique(sub$basis)[[1]]` — the
  # first basis in ROW ORDER — and then fed the combiner the WHOLE `sub`, mixing
  # bases. `build_event_time_att_cache` emits BOTH a fitted `per_day_metro` row
  # and an anchored `per_day_metro_imputed` row for the metros in
  # `CROSS_CITY_ANCHORED_TRAJ_METRO_IDS`, and Stockholm (949) is still in that
  # list even though the 2006-trial re-encoding made it a fitted city — so from
  # event_day 1278 on, a Stockholm window carried both, and which estimator ran
  # depended on which day sorted first. The paper's year-1/2/3 windows all end at
  # event_day 1277, so no served number moves; longer horizons are now
  # well-defined instead of order-dependent.
  sub <- window_rows_single_basis(sub)
  basis <- attr(sub, "cpportal_basis") %||% "fitted"
  if (identical(basis, "anchored")) {
    out <- combine_att_window_d2b(sub, basis = "anchored")
    return(list(att = out$att, se = out$se_att, n_days = as.integer(out$n_days),
                basis = basis, yhat0 = out$yhat0, pct = out$pct))
  }
  # FITTED: route through the shared combiner rather than calling
  # `inverse_variance_mean` here, so `/window` cannot drift from `/per-metro-
  # overall`, the Table 2 pipeline, or the paper generators. Under the default
  # pool mode the combiner's fitted branch IS the flat IVW this line used to do
  # inline (Tim, 2026-08-14) — same numbers, one implementation. `yhat0`/`pct`
  # come out of the combiner with the same plain-mean location rule.
  # `observed` rows take the fitted aggregator (as they always have here); the
  # REPORTED basis label is still the one `window_rows_single_basis` picked.
  out <- combine_att_window_d2b(sub, basis = "fitted",
                                include_inactive = include_inactive)
  list(att = out$att, se = out$se_att, n_days = as.integer(out$n_days),
       basis = basis, yhat0 = out$yhat0, pct = out$pct)
}

#' Basis ranking used whenever a window has to commit to ONE pool.
#'
#' `fitted` outranks `anchored` outranks `observed`: a fect-identified row is a
#' real counterfactual, an M10 satellite anchor is a constructed one, and the
#' `observed` rows (the London CCZ+ULEZ pseudo-metro 94301) are a descriptive
#' series, not an estimator output. Preference — not a merge: the losing basis's
#' rows are DROPPED, because pooling a fitted SE beside an anchored one under a
#' single combiner mixes two different error models. Genuine cross-basis pooling
#' has its own combiner (`combine_att_window_hybrid`, `/window?basis=hybrid`).
CPPORTAL_WINDOW_BASIS_PREFERENCE <- c("fitted", "anchored", "observed")

#' Restrict window rows to a single basis, preferring `fitted`.
#' Returns `rows` filtered to the winning basis, with the chosen basis attached
#' as the `cpportal_basis` attribute. Rows with no `basis` column are treated as
#' fitted (the pre-basis-column contract).
window_rows_single_basis <- function(rows) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) {
    attr(rows, "cpportal_basis") <- NA_character_
    return(rows)
  }
  if (!"basis" %in% names(rows)) {
    attr(rows, "cpportal_basis") <- "fitted"
    return(rows)
  }
  bv <- unique(stats::na.omit(as.character(rows$basis)))
  bv <- bv[nzchar(bv)]
  if (length(bv) == 0L) {
    attr(rows, "cpportal_basis") <- "fitted"
    return(rows)
  }
  ranked <- intersect(CPPORTAL_WINDOW_BASIS_PREFERENCE, bv)
  pick <- if (length(ranked)) ranked[[1L]] else sort(bv)[[1L]]
  out <- rows[!is.na(rows$basis) & as.character(rows$basis) == pick, , drop = FALSE]
  attr(out, "cpportal_basis") <- pick
  out
}

#' Native counterfactual level over a window's cells, using the basis-matched
#' location rule (Huber for anchored, plain mean for fitted/observed).
.cpportal_window_yhat0 <- function(rows, basis) {
  if (is.null(rows) || !("yhat0" %in% names(rows)) || nrow(rows) == 0L) return(NA_real_)
  v <- suppressWarnings(as.numeric(as.character(rows$yhat0)))
  if (!any(is.finite(v))) return(NA_real_)
  if (identical(basis, "anchored")) .huber_location(v) else mean(v, na.rm = TRUE)
}

#' ATT as a percent of the counterfactual level. NA — never 0 — for a missing or
#' degenerate baseline.
.cpportal_pct <- function(att, yhat0) {
  if (is.finite(yhat0) && abs(yhat0) > 1e-9 && is.finite(att)) att / yhat0 * 100 else NA_real_
}

#' Quantity A — per-metro ATT averaged over the N-years window.
#' @param include_inactive Diagnostic escape hatch, forwarded to
#'   `metro_window_estimate()`. FALSE (the default and the serving posture)
#'   pools charged days only, for every basis.
summarize_window_att <- function(cache,
                                 n_years,
                                 window_kind = c("point", "cumulative"),
                                 metro_ids = NULL,
                                 include_inactive = FALSE) {
  window_kind <- match.arg(window_kind)
  mids <- cache |> dplyr::distinct(.data$metro_id) |> dplyr::pull(.data$metro_id)
  if (!is.null(metro_ids) && length(metro_ids) > 0) {
    mids <- intersect(mids, as.integer(metro_ids))
  }
  purrr::map_dfr(mids, function(mid) {
    est <- metro_window_estimate(cache, mid, n_years, window_kind,
                                 include_inactive = include_inactive)
    row <- cache |> dplyr::filter(.data$metro_id == mid) |> dplyr::slice_head(n = 1)
    tibble::tibble(
      metro_id = mid,
      metro_name = if (nrow(row) > 0) row$metro_name[1] else paste0("Metro ", mid),
      treatment_start_date = if (nrow(row) > 0) row$treatment_start_date[1] else as.Date(NA),
      n_years = as.numeric(n_years),
      window_kind = window_kind,
      event_day_lo = cross_city_window_bounds(n_years, window_kind)[["lo"]],
      event_day_hi = cross_city_window_bounds(n_years, window_kind)[["hi"]],
      att = est$att,
      se_att = est$se,
      n_days = est$n_days,
      # EVERY row states which pool produced it. Callers label the ribbon from
      # this column and NEVER infer it from the city name — Stockholm served an
      # anchored row until the 2006-trial re-encoding gave `fect` a pre-period.
      # NA means the window is empty, i.e. no pool ran at all.
      basis = est$basis %||% NA_character_,
      yhat0 = est$yhat0 %||% NA_real_,
      pct = est$pct %||% NA_real_
    )
  })
}

# =============================================================================
# PER-YEAR (event-time) CROSS-CITY POOLS — the fitted-city Table 2 extension
# =============================================================================

#' The three cities `fect` actually IDENTIFIES (basis = "fitted"): NYC (1),
#' London (943), Stockholm (949). `paper_effects` is scoped to these three; the
#' satellite-anchored metros (950 Milan, 955 Singapore, 956/957/958 Oslo /
#' Gothenburg / Bergen) are out of that paper entirely.
#'
#' This is a DEFAULT, not an assertion. `basis` is read off each row and
#' returned; if a retrain flips one of these back to anchored the pooled row
#' says so rather than mislabelling it.
CROSS_CITY_FITTED_METRO_IDS <- c(1L, 943L, 949L)

#' Equal-weighted (unweighted) mean of k city-level estimates.
#'
#' WHY EQUAL WEIGHTS — Tim's ruling for `paper_effects`. Across cities the target
#' is "the average congestion-pricing city", so each city counts once. An
#' inverse-variance mean would answer a different question (it weights by panel
#' length and monitor density, so London's two decades of monitor-days would
#' swamp NYC's eighteen months), and a random-effects mean would add a τ² the
#' three-city sample cannot estimate. Inverse-variance weighting stays where it
#' belongs — BELOW this level, pooling cells within a city.
#'
#'   att_eq = (1/k) Σ att_i
#'   se_eq  = sqrt(Σ se_i²) / k
#'
#' The SE is the exact SE of that linear combination under independence across
#' cities — three separate panels, separate countries, separate donor pools, so
#' zero cross-city covariance is the right default. It is NOT sd(att_i)/√k
#' (that would be a between-city dispersion estimate on k = 3, a two-degree-of-
#' freedom number) and NOT a random-effects SE. `sd_between` is returned
#' ALONGSIDE as a diagnostic so the heterogeneity is visible without being
#' folded into the interval.
#'
#' A single NA `se_i` makes `se_eq` NA rather than dropping that city's variance
#' — a mean whose SE quietly ignores one of its own terms is worse than no SE.
equal_weighted_mean <- function(att, se) {
  att <- suppressWarnings(as.numeric(att))
  se  <- suppressWarnings(as.numeric(se))
  ok  <- is.finite(att)
  if (!any(ok)) {
    return(list(att = NA_real_, se = NA_real_, k = 0L, sd_between = NA_real_))
  }
  a <- att[ok]; s <- se[ok]
  k <- length(a)
  list(
    att = mean(a),
    se  = if (all(is.finite(s))) sqrt(sum(s^2)) / k else NA_real_,
    k   = as.integer(k),
    sd_between = if (k >= 2L) stats::sd(a) else NA_real_
  )
}

# =============================================================================
# POOLED PERCENT vs the counterfactual — ONE implementation, three callers.
#
# A pooled native ATT (ug/m3) has an obvious meaning; a pooled PERCENT does not,
# because each city's percent is measured against a different counterfactual
# level. The wrong answer — a plain mean of the per-city percents — is a ratio
# of ratios over k different denominators, and it silently over-weights the
# cleanest city. The right answer, and the ONLY one this project uses, is
# `effect_pool_overall()`'s `simple` branch (app/v2/api/R/effect_sizes.R):
#
#   b_i = the city's own NATIVE counterfactual level (its yhat0)
#   B   = sum(w_i b_i) / sum(w_i)      w_i = the SAME weights the native pool used
#   pct = 100 * mu / B                 mu = the pooled native, NEVER recomputed
#   pct_ci = 100 * native_ci / B       the same scaling, so the interval's
#                                      significance is identical in both units
#
# Because the native is SCALED rather than re-derived, the percent and the
# ug/m3 value beside it cannot drift.
#
# This construction had been written out three times: results/build_results.R
# (`.eqw_pct`, the paper's t2.pooled.eqwfit.pct), att_by_year.R's `pool_one`,
# and — as an NA hole — data_fetch.R's fitted branch. The two API callers now
# share the functions below; build_results.R stays a separate process (it reads
# results.json-era cells, not a bundle) but its arithmetic is documented as the
# same and is the parity oracle in app/v2/api/tests/test_att_basis.R.
#
# WEIGHTS. `weights = NULL` means equal weights, which reduces B to a plain
# mean of the finite baselines — exactly what an equal-weight native pool
# (`equal_weighted_mean()`) requires for coherence. Pass the pool's real weights
# for an inverse-variance or random-effects native pool.
#
# DEGENERATE B. A missing or ~zero baseline yields NA, never 0 and never Inf: a
# percent against no counterfactual is not a small percent, it is no percent.
# =============================================================================

#' Per-city native counterfactual level b_i.
#'
#' Prefers the pinned `yhat0`. Where a pin carries a percent but no level (older
#' pins, and some fitted `per_metro` rows — paper PR cpportal_paper#70) the
#' level is recovered by the identity `b = 100 * att / pct`, which is the same
#' inversion build_results.R and `effect_pool_overall()` use. Vectorized; NA
#' wherever neither source is usable.
#'
#' @param att   native ATT per city.
#' @param pct   percent-vs-counterfactual per city, or NULL if the pin has none.
#' @param yhat0 native counterfactual level per city, or NULL if absent.
counterfactual_baseline <- function(att, pct = NULL, yhat0 = NULL) {
  num <- function(x) suppressWarnings(as.numeric(as.character(x)))
  a <- num(att)
  y <- if (is.null(yhat0)) rep(NA_real_, length(a)) else num(yhat0)
  p <- if (is.null(pct))   rep(NA_real_, length(a)) else num(pct)
  if (length(y) != length(a)) y <- rep(NA_real_, length(a))
  if (length(p) != length(a)) p <- rep(NA_real_, length(a))
  ifelse(
    is.finite(y), y,
    ifelse(is.finite(a) & is.finite(p) & abs(p) > 1e-9, 100 * a / p, NA_real_)
  )
}

#' Scale ONE native value to a percent against a baseline. NA in, NA out; NA for
#' a degenerate baseline. The single scaler both pooled and per-city percents go
#' through, so the two can never use different conventions.
pct_vs_baseline <- function(x, baseline) {
  x <- suppressWarnings(as.numeric(x)); baseline <- suppressWarnings(as.numeric(baseline))
  if (length(x) != 1L) x <- x[1L]
  if (length(baseline) != 1L) baseline <- baseline[1L]
  if (isTRUE(is.finite(baseline)) && abs(baseline) > 1e-9 && isTRUE(is.finite(x))) {
    x * 100 / baseline
  } else {
    NA_real_
  }
}

#' Method-weighted pooled percent for an already-pooled native estimate.
#'
#' @param att_pooled the pooled native point estimate. NOT recomputed here.
#' @param baselines  per-city b_i (see `counterfactual_baseline()`).
#' @param ci         length-2 native CI of `att_pooled`; scaled by the same
#'                   100/B so it stays coherent with the point estimate.
#' @param weights    per-city pool weights; NULL = equal (plain mean of b_i).
#' @return list(pct, pct_lo, pct_hi, baseline = B, baseline_k = cities that
#'   contributed a finite b_i). `baseline_k` is returned so a caller can say the
#'   percent and the native estimate came from different city sets rather than
#'   quietly publishing a mismatch.
pooled_pct_vs_counterfactual <- function(att_pooled, baselines,
                                         ci = c(NA_real_, NA_real_),
                                         weights = NULL) {
  b <- suppressWarnings(as.numeric(baselines))
  w <- if (is.null(weights)) rep(1, length(b)) else suppressWarnings(as.numeric(weights))
  if (length(w) != length(b)) w <- rep(1, length(b))
  ok <- is.finite(b) & is.finite(w) & w > 0
  B  <- if (any(ok)) sum(w[ok] * b[ok]) / sum(w[ok]) else NA_real_
  ci <- suppressWarnings(as.numeric(ci))
  if (length(ci) != 2L) ci <- c(NA_real_, NA_real_)
  list(
    pct        = pct_vs_baseline(att_pooled, B),
    pct_lo     = pct_vs_baseline(ci[1L], B),
    pct_hi     = pct_vs_baseline(ci[2L], B),
    baseline   = B,
    baseline_k = as.integer(sum(ok))
  )
}

#' OVERALL year-N ATT — ONE pool over every treated cell in `metro_ids`.
#'
#' This is the *lower-level* pool: the cities' cells are thrown into a single
#' inverse-variance combine rather than each city being summarised first. Same
#' estimator `metro_window_estimate` runs per city (`inverse_variance_mean` over
#' the fitted per-day cells, each cell already the mean over that metro-day's
#' treated monitors and carrying an SE that reflects how many) — the only change
#' is that the filter admits a SET of metros. Nothing new is estimated here, so
#' this is not a parallel estimator; it is the existing one with a wider filter.
#'
#' Charged days only (universal estimand) and native scale throughout — the pin
#' has stored native ATTs since the M7 retrain, so there is NO per-query
#' back-transform to apply and none is applied.
#'
#' BASIS CONTRACT: each metro is first reduced to its own single basis
#' (`window_rows_single_basis`, fitted preferred), then the surviving bases must
#' AGREE. A mixed set returns `att = NA` with `basis = "mixed"` — a fitted SE and
#' an M10 anchored SE are different error models and cell-pooling them would
#' produce a number nobody can interpret. `/window?basis=hybrid` exists for
#' deliberate cross-basis pooling and is the right tool if that is ever wanted.
window_overall_estimate <- function(cache, metro_ids, n_years, window_kind,
                                    include_inactive = FALSE) {
  b <- cross_city_window_bounds(n_years, window_kind)
  mids <- unique(stats::na.omit(as.integer(metro_ids)))
  empty <- list(att = NA_real_, se = NA_real_, n_days = 0L, n_monitor_days = 0,
                n_metros = 0L, metro_ids = integer(0), basis = NA_character_,
                yhat0 = NA_real_, pct = NA_real_,
                event_day_min = NA_integer_, event_day_max = NA_integer_)
  if (length(mids) == 0L) return(empty)

  sub <- cache |>
    dplyr::filter(
      .data$metro_id %in% mids,
      .data$event_day >= b[["lo"]],
      .data$event_day <= b[["hi"]]
    ) |>
    cpportal_charged_days_filter(include_inactive = include_inactive) |>
    cpportal_episode_days_filter()
  if (nrow(sub) == 0L) return(empty)

  # Reduce EACH metro to one basis before unioning, so a city that happens to
  # carry both row types in this window contributes only its preferred pool —
  # identical to what `metro_window_estimate` does for that city on its own.
  parts <- lapply(split(sub, sub$metro_id), window_rows_single_basis)
  bases <- unique(stats::na.omit(vapply(
    parts, function(p) attr(p, "cpportal_basis") %||% NA_character_, character(1))))
  pooled <- dplyr::bind_rows(lapply(parts, function(p) {
    attr(p, "cpportal_basis") <- NULL
    p
  }))
  used <- sort(unique(as.integer(pooled$metro_id)))
  md <- if ("n_effects" %in% names(pooled)) {
    sum(dplyr::coalesce(suppressWarnings(as.numeric(pooled$n_effects)), 1), na.rm = TRUE)
  } else {
    as.numeric(nrow(pooled))
  }
  # Event-day span the pool actually SPENT, not the span it asked for. On the
  # unbounded `all` window these are the only honest description of coverage:
  # London's fitted rows do not start at its treatment date (see
  # `per_metro_overall_pooled`), so `event_day_min` is how a reader learns the
  # headline pool's London contribution begins years after the CCZ did.
  .ed <- suppressWarnings(as.integer(pooled$event_day))
  edmin <- if (any(is.finite(.ed))) min(.ed, na.rm = TRUE) else NA_integer_
  edmax <- if (any(is.finite(.ed))) max(.ed, na.rm = TRUE) else NA_integer_

  if (length(bases) > 1L) {
    return(list(att = NA_real_, se = NA_real_, n_days = nrow(pooled),
                n_monitor_days = md, n_metros = length(used), metro_ids = used,
                basis = "mixed", yhat0 = NA_real_, pct = NA_real_,
                event_day_min = edmin, event_day_max = edmax))
  }
  basis <- if (length(bases) == 1L) bases[[1L]] else "fitted"
  if (identical(basis, "anchored")) {
    out <- combine_att_window_d2b(pooled, basis = "anchored")
    return(list(att = out$att, se = out$se_att, n_days = as.integer(out$n_days),
                n_monitor_days = md, n_metros = length(used), metro_ids = used,
                basis = basis, yhat0 = out$yhat0, pct = out$pct,
                event_day_min = edmin, event_day_max = edmax))
  }
  iv <- inverse_variance_mean(pooled$att, pooled$se_att)
  yh <- .cpportal_window_yhat0(pooled, basis)
  list(att = iv$att, se = iv$se, n_days = as.integer(iv$n), n_monitor_days = md,
       n_metros = length(used), metro_ids = used, basis = basis,
       yhat0 = yh, pct = .cpportal_pct(iv$att, yh),
       event_day_min = edmin, event_day_max = edmax)
}

#' Per-year window ATT for a city set, WITH its two cross-city summaries.
#'
#' Returns ONE long tibble so a caller can never print an aggregate without its
#' parts (the posture `combine_att_window_hybrid` already takes). The
#' `statistic` column tags each row:
#'
#'   "metro"          — one row per city, straight from `summarize_window_att`
#'   "overall"        — `window_overall_estimate`: a single cell-level
#'                      inverse-variance pool over EVERY treated monitor-day in
#'                      the set. Cities enter in proportion to their precision
#'                      and their day count.
#'   "equal_weighted" — `equal_weighted_mean` over the per-city rows. Each city
#'                      counts once. THE cross-city statistic for `paper_effects`.
#'
#' `n_metros` on the `equal_weighted` row is the number of cities that actually
#' produced a finite ATT, and `metro_ids_missing` names the ones that did not, so
#' a "3-city mean" built from fewer than three cities is impossible to mistake
#' for a complete one. `complete` is TRUE only when every requested city landed.
#' A city can be missing for a mundane reason: its treated panel may not reach
#' this event-time window at all (London's fect-identified monitor-days begin at
#' event_day ~2130, so years 1-3 of the CCZ have no fitted rows whatsoever).
#'
#' @param metro_ids Cities to summarise. Defaults to `CROSS_CITY_FITTED_METRO_IDS`.
#' @param include_inactive Diagnostic escape hatch (see `cpportal_charged_days_filter`).
summarize_window_att_pooled <- function(cache,
                                        n_years,
                                        window_kind = c("point", "cumulative"),
                                        metro_ids = CROSS_CITY_FITTED_METRO_IDS,
                                        include_inactive = FALSE) {
  window_kind <- match.arg(window_kind)
  requested <- unique(stats::na.omit(as.integer(metro_ids)))
  bounds <- cross_city_window_bounds(n_years, window_kind)

  per <- summarize_window_att(cache, n_years, window_kind = window_kind,
                              metro_ids = requested,
                              include_inactive = include_inactive)
  ov <- window_overall_estimate(cache, requested, n_years, window_kind,
                                include_inactive = include_inactive)
  eq <- equal_weighted_mean(per$att, per$se_att)

  finite_ids <- if (nrow(per)) sort(unique(as.integer(per$metro_id[is.finite(per$att)]))) else integer(0)
  missing_ids <- setdiff(requested, finite_ids)
  csv <- function(x) if (length(x)) paste(x, collapse = ",") else ""

  agg <- tibble::tibble(
    statistic = c("overall", "equal_weighted"),
    metro_id = NA_integer_,
    metro_name = c("Overall (all treated monitors)",
                   paste0(length(finite_ids), "-city equal-weighted mean")),
    treatment_start_date = as.Date(NA),
    n_years = as.numeric(n_years),
    window_kind = window_kind,
    event_day_lo = bounds[["lo"]],
    event_day_hi = bounds[["hi"]],
    att = c(ov$att, eq$att),
    se_att = c(ov$se, eq$se),
    n_days = c(as.integer(ov$n_days), NA_integer_),
    basis = c(ov$basis, .cpportal_agg_basis(per)),
    yhat0 = c(ov$yhat0, NA_real_),
    pct = c(ov$pct, NA_real_),
    n_monitor_days = c(ov$n_monitor_days, NA_real_),
    n_metros = c(as.integer(ov$n_metros), eq$k),
    # `se_eq` weights cities equally; sd_between is reported but NOT folded in.
    sd_between = c(NA_real_, eq$sd_between),
    metro_ids_used = c(csv(ov$metro_ids), csv(finite_ids)),
    metro_ids_missing = c(csv(setdiff(requested, ov$metro_ids)), csv(missing_ids)),
    complete = c(length(ov$metro_ids) == length(requested),
                 length(finite_ids) == length(requested)),
    se_source = c(if (identical(ov$basis, "fitted")) "ivw_cell_pool" else
                    if (identical(ov$basis, "mixed")) NA_character_ else "d2b_shared_plus_indep_over_n",
                  "equal_weight_sqrt_sum_se2_over_k")
  )

  parts <- per |>
    dplyr::mutate(
      statistic = "metro",
      n_monitor_days = NA_real_, n_metros = NA_integer_, sd_between = NA_real_,
      metro_ids_used = dplyr::if_else(is.finite(.data$att), as.character(.data$metro_id), ""),
      metro_ids_missing = dplyr::if_else(is.finite(.data$att), "", as.character(.data$metro_id)),
      complete = is.finite(.data$att),
      # NA basis == the window held no rows at all, so no pool ran and naming one
      # would be a fiction.
      se_source = dplyr::case_when(
        is.na(.data$basis) ~ NA_character_,
        .data$basis == "anchored" ~ "d2b_shared_plus_indep_over_n",
        TRUE ~ "ivw_cell_pool"
      )
    )

  dplyr::bind_rows(parts, agg) |>
    dplyr::select(dplyr::any_of(names(agg)))
}

#' Basis label for a cross-city aggregate: the shared basis when every
#' contributing city agrees, "mixed" when they do not, NA when none contributed.
.cpportal_agg_basis <- function(per) {
  if (is.null(per) || !nrow(per) || !"basis" %in% names(per)) return(NA_character_)
  b <- unique(stats::na.omit(as.character(per$basis[is.finite(per$att)])))
  if (length(b) == 0L) NA_character_ else if (length(b) == 1L) b[[1L]] else "mixed"
}

#' Per-metro OVERALL zone ATT (all treated days; NOT an event-time window),
#' covering EVERY cordon metro incl. the satellite-anchored cities, each tagged
#' with a `basis` flag. Source for the paper's Table 2.
#'
#' - **fitted** cities (fect identified them): the stored `per_metro` ATT/SE — the
#'   canonical aggregate from `get_att_did` (`sqrt(sum se^2)/n` model SE).
#' - **anchored** cities (no pre-policy data; dropped by fect, satellite-anchored
#'   under the production M7 method): mean of the `per_day_metro_imputed` daily ATTs minus the mean
#'   calibration bias. **SE: per-metro pooled from the per-monitor-day `se_att`
#'   already in the pin** (ADR-0011 §7.2 / D2a — each city now differs).
#'   Conservative aggregation across the city's days (`mean(se_att)`, no √n
#'   shrink) because the shared anchor/alpha error dominates and shrinking it
#'   by sqrt(n) would be spuriously tiny — D2b's variance split will refine
#'   this. The global calibration band (`cal_q05`/`cal_q95`, appendix A.7.4)
#'   is retained as a diagnostic `method_band_*` and used as a fallback only
#'   when older pins lack per-day `se_att`.
#'
#' A metro can appear twice (London: fitted extension monitors + anchored central
#' monitors); callers map the two rows to the CCZ / CCZ+ULEZ table rows.

#' Keep only the rows of one `basis` on a per-metro ATT frame (T1a, 2026-08-28).
#'
#' MIRRORS the filter `per_metro_overall_pooled()` already applies to the same
#' frame (`keep & as.character(pmo$basis) == basis`) — extracted so the
#' `/cross-city-att/per-metro-overall` endpoint and `publish_ref.R` can serve
#' and cache a fitted-only variant without a second implementation. `"all"`,
#' `""`, and NULL all keep every row, so the default response is untouched.
#' `basis` is read off the row and never inferred from a city roster
#' (CLAUDE.md "ATT wiring" 3: trust the column, not the name).
cpportal_filter_att_basis <- function(df, basis = "all") {
  b <- as.character(basis %||% "all")[1L]
  if (is.na(b) || !nzchar(b) || identical(tolower(b), "all")) return(df)
  if (is.null(df) || !is.data.frame(df) || !"basis" %in% names(df)) return(df)
  df[!is.na(df$basis) & as.character(df$basis) == b, , drop = FALSE]
}

per_metro_overall_att <- function(spec_id = "all_priority",
                                  pollutant = "PM2.5",
                                  conn = NULL,
                                  outcome = NULL,
                                  scope = "zone") {
  suppressWarnings(requireNamespace("bit64", quietly = TRUE))  # metro_id is integer64
  outcome <- outcome %||% "aq_daily_mean"
  # scope="metro" reads the whole-metro pin (`..._metro`): NYC fect-identified
  # per_metro (basis=fitted) + the 4 satellite-level-anchored per_day_metro_imputed
  # cities (basis=anchored, method M7). Same row schema as zone, so the fitted/
  # anchored extraction below is unchanged.
  bundle <- tryCatch(load_fect_bundle(spec_id, outcome = outcome, scope = scope),
                     error = function(e) NULL)
  if (is.null(bundle) || is.null(bundle$att) || nrow(bundle$att) == 0L) {
    return(tibble::tibble())
  }
  att <- cross_city_filter_bundle_att(bundle$att, pollutant)
  if (nrow(att) == 0L) return(tibble::tibble())
  intc <- function(x) suppressWarnings(as.integer(as.character(x)))
  numc <- function(x) suppressWarnings(as.numeric(as.character(x)))
  has  <- function(col, df) col %in% names(df)

  # ADR-0009: `per_metro` rows are now the single source of truth for BOTH
  # fect-fitted and satellite-anchored cities — the anchored whole-metro ATT is
  # aggregated (Huber + D2b SE) at TRAIN time and pinned as a per_metro row with
  # `basis = "anchored"`. This branch reads any per_metro row through unchanged,
  # taking `basis` from the row (defaulting to "fitted" for pre-ADR-0009 pins,
  # which only ever pinned fitted per_metro rows). The per-day aggregation below
  # is retained ONLY as a fallback for those older pins.
  fitted <- att |> dplyr::filter(.data$type == "per_metro")
  fitted <- if (nrow(fitted)) {
    a <- numc(fitted$att); s <- numc(fitted$se_att)
    b <- if (has("basis", fitted)) as.character(fitted$basis) else NA_character_
    b <- ifelse(is.na(b) | !nzchar(b), "fitted", b)
    # Pinned p_value: fect-identified rows carry a real p from get_att_did.
    # Anchored rows have NA (fect couldn't fit them) — but the pinned SE is
    # a real Wald SE (D2b variance split, README_fect_uncertainty.md §S6),
    # so we DERIVE the p-value from a two-sided z-test on att/se. Reporting
    # "—" for a metro whose SE clearly exceeds |ATT| was misleading — no
    # p at all read as "we don't know," when the correct answer is
    # "definitely not significant."
    pinned_p <- if (has("p_value", fitted)) numc(fitted$p_value) else rep(NA_real_, length(a))
    wald_p <- ifelse(
      is.finite(a) & is.finite(s) & s > 0,
      2 * stats::pnorm(-abs(a) / s),
      NA_real_
    )
    p_out <- ifelse(is.finite(pinned_p), pinned_p, wald_p)
    # pct_change is often NA on fitted per_metro rows even when yhat0 is finite
    # (paper PR timothyfraser/cpportal_paper#70). Prefer pinned pct_change; else
    # 100 * att / yhat0 — same definition as window / day pools.
    pct_pin <- if (has("pct_change", fitted)) numc(fitted$pct_change) else rep(NA_real_, length(a))
    y0_pin  <- if (has("yhat0", fitted)) numc(fitted$yhat0) else rep(NA_real_, length(a))
    pct_fill <- ifelse(
      is.finite(pct_pin), pct_pin,
      ifelse(is.finite(a) & is.finite(y0_pin) & abs(y0_pin) > 1e-9, 100 * a / y0_pin, NA_real_)
    )
    tibble::tibble(
      metro_id = intc(fitted$metro_id),
      att = a, se_att = s, ci_lo = a - 1.96 * s, ci_hi = a + 1.96 * s,
      pct = pct_fill,
      yhat0 = y0_pin,
      p_value = p_out,
      n = if (has("n_effects", fitted)) intc(fitted$n_effects) else NA_integer_,
      # `method` names the ESTIMATOR family, not a frozen strategy id. It was
      # hard-coded "M7" here long after M7 was condemned and M10 became the
      # anchored production method (CLAUDE.md "ATT wiring" 4), so every served
      # anchored row carried a stale, wrong token. The anchored method is
      # env-switchable (CPPORTAL_FECT_ANCHOR_METHOD), so naming any single
      # strategy here would just re-create the same lie; "anchored" is the
      # honest label and matches what /monitor-att-series reports.
      basis = b, method = ifelse(b == "anchored", "anchored", "fect")
    )
  } else tibble::tibble()

  # Robust aggregation of the anchored per-day ATTs (2026-07-02, ADR-0012). The
  # anchored counterfactual is a MONTHLY satellite level; the per-day ATT =
  # observed²(daily) − counterfactual², so it is heavy-tailed by construction (a
  # monthly anchor vs a daily observation, squared). A plain mean over a city's
  # days is dominated by that tail — most visibly Milan at metro scope (Po-Valley
  # winter spikes: plain mean −7.8). A Huber M-estimator (k=1.345, the standard
  # 95%-efficient tuning) down-weights the tail WITHOUT discarding any day: it
  # leaves light-tailed cities essentially unchanged (all move <0.1 from the
  # plain mean) but pulls Milan to a sane −0.45. Preferred over a trimmed mean —
  # no data thrown away, principled tuning. Applied uniformly, both scopes.
  # Fallback only (ADR-0009): aggregate per-day imputed rows for metros that do
  # NOT already have a per_metro row (i.e. pins trained before ADR-0009). Newer
  # pins carry the anchored per_metro row directly and skip this entirely.
  .pm_metros <- intc((att |> dplyr::filter(.data$type == "per_metro"))$metro_id)
  imp <- att |> dplyr::filter(.data$type == "per_day_metro_imputed",
                              !(intc(.data$metro_id) %in% .pm_metros))
  anchored <- if (nrow(imp)) {
    imp |>
      dplyr::mutate(
        metro_id = intc(.data$metro_id),
        att = numc(.data$att),
        yhat0 = if (has("yhat0", imp)) numc(.data$yhat0) else NA_real_,
        se_att_day = if (has("se_att", imp)) numc(.data$se_att) else NA_real_,
        # D2b (ADR-0011 §7.2): native-scale variance components, when present.
        # Older pins lack these and fall through to D2a (mean(se_att)).
        varc_shared_md = if (has("varc_shared_md", imp)) numc(.data$varc_shared_md) else NA_real_,
        varc_indep_md  = if (has("varc_indep_md",  imp)) numc(.data$varc_indep_md)  else NA_real_,
        cal_bias = if (has("cal_bias", imp)) numc(.data$cal_bias) else 0
      ) |>
      dplyr::group_by(.data$metro_id) |>
      dplyr::summarise(
        # Huber M-estimate (robust to the heavy tail, discards nothing); yhat0
        # matched so `pct` stays consistent with the robust point estimate.
        att_raw = .huber_location(.data$att),
        mean_yhat0 = .huber_location(.data$yhat0),
        mean_bias = mean(.data$cal_bias, na.rm = TRUE),
        # D2a fallback: per-metro pooled SE from per-monitor-day se_att in
        # the pin (no √n shrink — conservative).
        mean_se_att_day = mean(.data$se_att_day, na.rm = TRUE),
        # D2b: variance components for the principled shared/√n_days split.
        mean_varc_shared = mean(.data$varc_shared_md, na.rm = TRUE),
        mean_varc_indep  = mean(.data$varc_indep_md,  na.rm = TRUE),
        n = dplyr::n(), .groups = "drop"
      ) |>
      dplyr::transmute(
        metro_id = .data$metro_id,
        # Point estimate = the robust Huber location, WITHOUT the global
        # `cal_bias` subtraction (ADR-0012, 2026-07-02). `mean_bias` stays
        # computed above as a diagnostic only.
        att = .data$att_raw,
        # SE preference order:
        #   D2b (if present): shared (no shrink) + indep/n_days (full shrink)
        #   D2a (fallback):   mean(se_att) — conservative no-shrink
        #
        # The pre-D2 legacy calibration band (`cal_q05`/`cal_q95`) is
        # deliberately NOT offered as a fallback here (2026-07-08): it was
        # retired 2026-06-30 (ADR-0011 §7.2 D2a) and every pin trained since
        # then carries per-day `se_att`, so the legacy branch never fired in
        # practice. If D2a is unavailable too, `se_att = NA` surfaces the miss
        # to the consumer rather than silently substituting a global band.
        se_att = dplyr::case_when(
          is.finite(.data$mean_varc_shared) & is.finite(.data$mean_varc_indep) ~
            sqrt(.data$mean_varc_shared + .data$mean_varc_indep / .data$n),
          is.finite(.data$mean_se_att_day) & .data$mean_se_att_day > 0 ~ .data$mean_se_att_day,
          TRUE ~ NA_real_
        ),
        # % vs counterfactual: ATT over the native counterfactual mean.
        pct = dplyr::if_else(is.finite(.data$mean_yhat0) & abs(.data$mean_yhat0) > 1e-9,
                             .data$att_raw / .data$mean_yhat0 * 100, NA_real_),
        yhat0 = .data$mean_yhat0,
        # CI uses the new se_att (1.96 × SE) — consistent with the fitted
        # cities' convention above; the global cal band is surfaced as a
        # separate diagnostic in method_band_q05/q95 columns by callers
        # that want it.
        ci_lo = .data$att - 1.96 * .data$se_att,
        ci_hi = .data$att + 1.96 * .data$se_att,
        # Two-sided Wald p from att / se_att — same reasoning as the fitted
        # branch above; the D2b SE is a real confidence SE, so a z-test is
        # legitimate (README_fect_uncertainty.md §S6).
        p_value = dplyr::if_else(
          is.finite(.data$att) & is.finite(.data$se_att) & .data$se_att > 0,
          2 * stats::pnorm(-abs(.data$att) / .data$se_att),
          NA_real_
        ),
        # "anchored", not "M7" — see the fitted branch above.
        n = .data$n, basis = "anchored", method = "anchored"
      )
  } else tibble::tibble()

  out <- dplyr::bind_rows(fitted, anchored)
  if (nrow(out) == 0L) return(out)

  metros <- tryCatch(fetch_metros(priority_only = FALSE), error = function(e) NULL)
  if (!is.null(metros) && nrow(metros) > 0 && "metro_id" %in% names(metros)) {
    lk <- tibble::tibble(metro_id = as.integer(metros$metro_id),
                         metro_name = as.character(metros$metro_name))
    out <- out |> dplyr::left_join(lk, by = "metro_id")
  } else {
    out$metro_name <- NA_character_
  }
  out |>
    dplyr::mutate(metro_name = dplyr::if_else(
      is.na(.data$metro_name) | !nzchar(.data$metro_name),
      paste0("Metro ", .data$metro_id), .data$metro_name)) |>
    dplyr::select("metro_id", "metro_name", "att", "se_att", "ci_lo", "ci_hi",
                  "pct", "yhat0", "p_value", "n", "basis", "method") |>
    dplyr::arrange(.data$basis, .data$metro_id)
}

# =============================================================================
# HEADLINE (all-time) CROSS-CITY POOLS — the Table 1 sibling of
# `summarize_window_att_pooled`
# =============================================================================

#' Post-treatment event-day coverage per metro, on the charged-day rows the
#' pools actually see. One row per metro: `event_day_min` / `event_day_max` /
#' `n_days` / `n_monitor_days`.
.cpportal_metro_coverage <- function(cache, metro_ids, include_inactive = FALSE) {
  mids <- unique(stats::na.omit(as.integer(metro_ids)))
  empty <- tibble::tibble(metro_id = integer(0), event_day_min = integer(0),
                          event_day_max = integer(0), n_days = integer(0),
                          n_monitor_days = numeric(0))
  if (is.null(cache) || !nrow(cache) || length(mids) == 0L) return(empty)
  sub <- cache |>
    dplyr::filter(.data$metro_id %in% mids, .data$event_day >= 0L) |>
    cpportal_charged_days_filter(include_inactive = include_inactive) |>
    cpportal_episode_days_filter()
  if (!nrow(sub)) return(empty)
  parts <- lapply(split(sub, sub$metro_id), window_rows_single_basis)
  sub <- dplyr::bind_rows(lapply(parts, function(p) {
    attr(p, "cpportal_basis") <- NULL
    p
  }))
  ne <- if ("n_effects" %in% names(sub)) {
    dplyr::coalesce(suppressWarnings(as.numeric(sub$n_effects)), 1)
  } else {
    rep(1, nrow(sub))
  }
  sub$ne_wt <- ne
  sub |>
    dplyr::group_by(.data$metro_id) |>
    dplyr::summarise(
      event_day_min = as.integer(min(.data$event_day, na.rm = TRUE)),
      event_day_max = as.integer(max(.data$event_day, na.rm = TRUE)),
      n_days = dplyr::n(),
      n_monitor_days = sum(.data$ne_wt, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(metro_id = as.integer(.data$metro_id))
}

#' HEADLINE per-metro overall ATT for a city SET, with its two cross-city
#' summaries — the all-time (Table 1) sibling of `summarize_window_att_pooled`.
#'
#' Rows are tagged by `statistic`:
#'
#'   "metro"          — the city's OVERALL zone ATT, straight from
#'                      `per_metro_overall_att()`. Byte-identical to what
#'                      `/cross-city-att/per-metro-overall` serves and therefore
#'                      to the per-city rows the paper's headline table prints.
#'   "overall"        — `window_overall_estimate()` on the `all` window: ONE
#'                      inverse-variance pool over EVERY treated monitor-day in
#'                      the set, no event-time bound. The "average treated
#'                      monitor-day".
#'   "equal_weighted" — `equal_weighted_mean()` over the `metro` rows above.
#'                      att = (1/k) Σ att_i, se = sqrt(Σ se_i²)/k. The "average
#'                      congestion-pricing city", and THE cross-city statistic
#'                      the paper reports.
#'
#' WHY THE TWO AGGREGATES READ DIFFERENT SOURCES, deliberately:
#' the equal-weighted mean must be reproducible by hand from the rows printed
#' above it, so it averages the pinned `per_metro` ATTs the table displays — not
#' a re-derivation of them. The overall pool cannot come from `per_metro` at all:
#' those rows are already city-level aggregates with no cells left to weight, so
#' it pools the per-day cells out of the event-time cache. Both are charged-days
#' only and native scale; the two paths land on the same estimand (see
#' CLAUDE.md, "Charged days").
#'
#' ACROSS CITIES, EQUAL WEIGHTS ONLY. No inverse-variance and no random-effects
#' mean is emitted at the city level: IVW would let London's two decades of
#' monitor-days swamp NYC's eighteen months, and k = 3 cannot estimate a τ².
#' Inverse-variance weighting stays below this level, inside the overall pool.
#' `sd_between` rides along as a diagnostic and is NOT folded into the interval.
#'
#' KNOWN COVERAGE ASYMMETRY, and it is a property of the data, not of this code:
#' the panel's earliest London row is 2003-05-09 and the CCZ began 2003-02-17, so
#' the original-CCZ monitors have no pre-period and `fect` drops them. London's
#' fitted rows come only from monitors first treated by a LATER zone (Western
#' Extension 2007, ULEZ 2019/2021/2023). London therefore enters the `overall`
#' pool thousands of event-days after its own treatment date. `event_day_min`
#' on each row makes that visible instead of implicit — read it before quoting
#' the pooled number as "since 2003".
#'
#' @param metro_ids Cities to summarise. Defaults to `CROSS_CITY_FITTED_METRO_IDS`.
#' @param basis Basis to select from `per_metro_overall_att()` for the `metro`
#'   rows; `"fitted"` (the `paper_effects` scope) by default. `NULL`/`NA` takes
#'   whatever basis the pin serves for each metro.
#' @param include_inactive Diagnostic escape hatch (see `cpportal_charged_days_filter`).
per_metro_overall_pooled <- function(spec_id = "all_priority",
                                     pollutant = "PM2.5",
                                     conn = NULL,
                                     outcome = NULL,
                                     scope = "zone",
                                     metro_ids = CROSS_CITY_FITTED_METRO_IDS,
                                     basis = "fitted",
                                     include_inactive = FALSE) {
  requested <- unique(stats::na.omit(as.integer(metro_ids)))
  csv <- function(x) if (length(x)) paste(x, collapse = ",") else ""

  pmo <- per_metro_overall_att(spec_id = spec_id, pollutant = pollutant,
                               conn = conn, outcome = outcome, scope = scope)
  if (is.null(pmo) || !nrow(pmo)) return(tibble::tibble())
  keep <- as.integer(pmo$metro_id) %in% requested
  if (!is.null(basis) && length(basis) == 1L && !is.na(basis) && nzchar(basis) &&
      "basis" %in% names(pmo)) {
    keep <- keep & as.character(pmo$basis) == basis
  }
  per <- pmo[keep, , drop = FALSE]
  # Preserve the caller's city order — the table prints in the order asked for,
  # not in whatever order the pin happened to store.
  per <- per[order(match(as.integer(per$metro_id), requested)), , drop = FALSE]

  cache <- tryCatch(
    get_cross_city_cache(spec_id, pollutant, conn, refresh = FALSE,
                         outcome = outcome, scope = scope),
    error = function(e) NULL
  )
  ov <- if (is.null(cache) || !nrow(cache)) {
    list(att = NA_real_, se = NA_real_, n_days = 0L, n_monitor_days = NA_real_,
         n_metros = 0L, metro_ids = integer(0), basis = NA_character_,
         yhat0 = NA_real_, pct = NA_real_,
         event_day_min = NA_integer_, event_day_max = NA_integer_)
  } else {
    window_overall_estimate(cache, requested, n_years = NA_real_,
                            window_kind = "all",
                            include_inactive = include_inactive)
  }
  cov <- .cpportal_metro_coverage(cache, requested, include_inactive = include_inactive)

  eq <- equal_weighted_mean(per$att, per$se_att)
  finite_ids <- as.integer(per$metro_id[is.finite(per$att)])
  missing_ids <- setdiff(requested, finite_ids)

  z <- 1.96
  agg <- tibble::tibble(
    statistic = c("overall", "equal_weighted"),
    metro_id = NA_integer_,
    metro_name = c("Overall (all treated monitor-days)",
                   paste0(length(finite_ids), "-city equal-weighted mean")),
    att = c(ov$att, eq$att),
    se_att = c(ov$se, eq$se),
    ci_lo = c(ov$att - z * ov$se, eq$att - z * eq$se),
    ci_hi = c(ov$att + z * ov$se, eq$att + z * eq$se),
    pct = c(ov$pct, NA_real_),
    n = c(as.integer(ov$n_days), NA_integer_),
    n_monitor_days = c(as.numeric(ov$n_monitor_days), NA_real_),
    basis = c(ov$basis, .cpportal_pooled_basis(per)),
    method = c("ivw_cell_pool_all_days", "equal_weight_sqrt_sum_se2_over_k"),
    n_metros = c(as.integer(ov$n_metros), eq$k),
    sd_between = c(NA_real_, eq$sd_between),
    event_day_min = c(ov$event_day_min, NA_integer_),
    event_day_max = c(ov$event_day_max, NA_integer_),
    metro_ids_used = c(csv(ov$metro_ids), csv(finite_ids)),
    metro_ids_missing = c(csv(setdiff(requested, ov$metro_ids)), csv(missing_ids)),
    complete = c(length(ov$metro_ids) == length(requested),
                 length(finite_ids) == length(requested))
  )

  parts <- per |>
    dplyr::mutate(statistic = "metro") |>
    dplyr::left_join(cov |> dplyr::select("metro_id", "event_day_min",
                                          "event_day_max", "n_monitor_days"),
                     by = "metro_id") |>
    dplyr::mutate(
      n_metros = NA_integer_, sd_between = NA_real_,
      metro_ids_used = dplyr::if_else(is.finite(.data$att),
                                      as.character(.data$metro_id), ""),
      metro_ids_missing = dplyr::if_else(is.finite(.data$att), "",
                                         as.character(.data$metro_id)),
      complete = is.finite(.data$att)
    )

  dplyr::bind_rows(parts, agg) |>
    dplyr::select(dplyr::any_of(c(names(agg), "p_value")))
}

#' Basis label for a headline aggregate: the shared basis when every
#' contributing city agrees, "mixed" when they do not, NA when none contributed.
.cpportal_pooled_basis <- function(per) {
  if (is.null(per) || !nrow(per) || !"basis" %in% names(per)) return(NA_character_)
  b <- unique(stats::na.omit(as.character(per$basis[is.finite(per$att)])))
  if (length(b) == 0L) NA_character_ else if (length(b) == 1L) b[[1L]] else "mixed"
}

.mc_diff_two <- function(att1, se1, att2, se2, n_draws, seed) {
  if (any(is.na(c(att1, se1, att2, se2)))) {
    return(list(
      diff_obs = NA_real_, mean_sim = NA_real_, median_sim = NA_real_,
      ci_lo = NA_real_, ci_hi = NA_real_, p_value = NA_real_, n_draws = as.integer(n_draws)
    ))
  }
  set.seed(as.integer(seed))
  d1 <- rnorm(n_draws, att1, se1)
  d2 <- rnorm(n_draws, att2, se2)
  diff_sim <- d1 - d2
  obs <- att1 - att2
  p <- 2 * min(mean(diff_sim <= 0), mean(diff_sim >= 0))
  p <- min(p, 1)
  list(
    diff_obs = obs,
    mean_sim = mean(diff_sim),
    median_sim = stats::median(diff_sim),
    ci_lo = as.numeric(stats::quantile(diff_sim, 0.025)),
    ci_hi = as.numeric(stats::quantile(diff_sim, 0.975)),
    p_value = p,
    n_draws = as.integer(n_draws)
  )
}

.mc_diff_vs_pooled_peers <- function(att_x, se_x, att_peers, se_peers, n_draws, seed) {
  k <- length(att_peers)
  if (k == 0L || any(is.na(c(att_x, se_x))) || any(is.na(att_peers)) || any(is.na(se_peers))) {
    return(list(
      diff_obs = NA_real_, mean_sim = NA_real_, median_sim = NA_real_,
      ci_lo = NA_real_, ci_hi = NA_real_, p_value = NA_real_, n_draws = as.integer(n_draws)
    ))
  }
  set.seed(as.integer(seed))
  pool_obs <- mean(att_peers)
  diff_obs <- att_x - pool_obs
  sim <- matrix(
    rnorm(n_draws * (1L + k), mean = 0, sd = 1),
    nrow = n_draws,
    ncol = 1L + k
  )
  sim[, 1L] <- att_x + sim[, 1L] * se_x
  for (j in seq_len(k)) {
    sim[, j + 1L] <- att_peers[j] + sim[, j + 1L] * se_peers[j]
  }
  pool_sim <- rowMeans(sim[, -1L, drop = FALSE])
  diff_sim <- sim[, 1L] - pool_sim
  p <- 2 * min(mean(diff_sim <= 0), mean(diff_sim >= 0))
  p <- min(p, 1)
  list(
    diff_obs = diff_obs,
    mean_sim = mean(diff_sim),
    median_sim = stats::median(diff_sim),
    ci_lo = as.numeric(stats::quantile(diff_sim, 0.025)),
    ci_hi = as.numeric(stats::quantile(diff_sim, 0.975)),
    p_value = p,
    n_draws = as.integer(n_draws)
  )
}

#' Quantity B — compare metro_x to each peer and to pooled peers (Monte Carlo).
compare_with_mc <- function(cache,
                            n_years,
                            window_kind = c("point", "cumulative"),
                            metro_x,
                            peers = "all_other",
                            n_draws = 1000L,
                            seed = 1L) {
  window_kind <- match.arg(window_kind)
  n_draws <- min(50000L, max(100L, as.integer(n_draws)))
  mx <- as.integer(metro_x)
  ex <- metro_window_estimate(cache, mx, n_years, window_kind)
  if (is.na(ex$att)) {
    return(list(
      metro_x = mx,
      pair_results = tibble::tibble(),
      pooled = NULL,
      error = "metro_x has no ATT rows in window"
    ))
  }
  all_mids <- cache |> dplyr::distinct(.data$metro_id) |> dplyr::pull(.data$metro_id)
  if (is.character(peers) && length(peers) == 1L && identical(peers, "all_other")) {
    peer_ids <- setdiff(all_mids, mx)
  } else {
    peer_ids <- as.integer(peers)
    peer_ids <- peer_ids[!is.na(peer_ids)]
    peer_ids <- intersect(peer_ids, all_mids)
    peer_ids <- setdiff(peer_ids, mx)
  }
  pair_rows <- purrr::map_dfr(peer_ids, function(pid) {
    ey <- metro_window_estimate(cache, pid, n_years, window_kind)
    mc <- .mc_diff_two(ex$att, ex$se, ey$att, ey$se, n_draws, seed + pid)
    pname <- cache |> dplyr::filter(.data$metro_id == pid) |> dplyr::slice_head(n = 1)
    tibble::tibble(
      peer_metro_id = pid,
      peer_metro_name = if (nrow(pname) > 0) pname$metro_name[1] else paste0("Metro ", pid),
      diff_obs = mc$diff_obs,
      ci_lo = mc$ci_lo,
      ci_hi = mc$ci_hi,
      p_value = mc$p_value,
      n_draws = mc$n_draws,
      att_x = ex$att,
      se_x = ex$se,
      att_peer = ey$att,
      se_peer = ey$se
    )
  })
  att_peers <- purrr::map_dbl(peer_ids, function(pid) {
    metro_window_estimate(cache, pid, n_years, window_kind)$att
  })
  se_peers <- purrr::map_dbl(peer_ids, function(pid) {
    metro_window_estimate(cache, pid, n_years, window_kind)$se
  })
  pooled <- .mc_diff_vs_pooled_peers(ex$att, ex$se, att_peers, se_peers, n_draws, seed)
  nm <- cache |> dplyr::filter(.data$metro_id == mx) |> dplyr::slice_head(n = 1)
  list(
    metro_x = mx,
    metro_x_name = if (nrow(nm) > 0) nm$metro_name[1] else paste0("Metro ", mx),
    n_years = as.numeric(n_years),
    window_kind = window_kind,
    pair_results = pair_rows,
    pooled = pooled,
    error = NULL
  )
}

#' Quantity C — delta ATT between N-year and M-year windows; optional cross-city MC.
compute_delta <- function(cache,
                          n_years,
                          m_years,
                          window_kind = c("point", "cumulative"),
                          cross_city = FALSE,
                          metro_ids = NULL,
                          n_draws = 1000L,
                          seed = 1L) {
  window_kind <- match.arg(window_kind)
  n_draws <- min(50000L, max(100L, as.integer(n_draws)))
  mids <- cache |> dplyr::distinct(.data$metro_id) |> dplyr::pull(.data$metro_id)
  if (!is.null(metro_ids) && length(metro_ids) > 0) {
    mids <- intersect(mids, as.integer(metro_ids))
  }
  per_metro <- purrr::map_dfr(mids, function(mid) {
    eN <- metro_window_estimate(cache, mid, n_years, window_kind)
    eM <- metro_window_estimate(cache, mid, m_years, window_kind)
    delta <- if (!any(is.na(c(eN$att, eM$att)))) eN$att - eM$att else NA_real_
    se_d <- if (!any(is.na(c(eN$se, eM$se)))) sqrt(eN$se^2 + eM$se^2) else NA_real_
    row <- cache |> dplyr::filter(.data$metro_id == mid) |> dplyr::slice_head(n = 1)
    tibble::tibble(
      metro_id = mid,
      metro_name = if (nrow(row) > 0) row$metro_name[1] else paste0("Metro ", mid),
      att_n = eN$att,
      se_n = eN$se,
      att_m = eM$att,
      se_m = eM$se,
      delta_att = delta,
      se_delta = se_d,
      n_days_n = eN$n_days,
      n_days_m = eM$n_days
    )
  })
  cross_tbl <- NULL
  if (isTRUE(cross_city) && nrow(per_metro) >= 2L) {
    mc_rows <- list()
    kk <- 0L
    v_ids <- per_metro$metro_id
    for (i in seq_along(v_ids)) {
      for (j in seq_along(v_ids)) {
        if (j <= i) next
        a <- v_ids[i]
        b <- v_ids[j]
        ra <- per_metro |> dplyr::filter(.data$metro_id == a) |> dplyr::slice_head(n = 1)
        rb <- per_metro |> dplyr::filter(.data$metro_id == b) |> dplyr::slice_head(n = 1)
        if (nrow(ra) == 0 || nrow(rb) == 0) next
        # MC on delta: draw N and M windows for each metro, form delta, then difference
        set.seed(as.integer(seed + a * 1000L + b))
        d_a <- if (!any(is.na(c(ra$delta_att, ra$se_delta)))) {
          rnorm(n_draws, ra$delta_att, ra$se_delta)
        } else {
          rep(NA_real_, n_draws)
        }
        d_b <- if (!any(is.na(c(rb$delta_att, rb$se_delta)))) {
          rnorm(n_draws, rb$delta_att, rb$se_delta)
        } else {
          rep(NA_real_, n_draws)
        }
        if (any(is.na(d_a)) || any(is.na(d_b))) next
        diff_sim <- d_a - d_b
        obs <- ra$delta_att - rb$delta_att
        p <- 2 * min(mean(diff_sim <= 0), mean(diff_sim >= 0))
        p <- min(p, 1)
        kk <- kk + 1L
        mc_rows[[kk]] <- tibble::tibble(
          metro_a = a,
          metro_a_name = ra$metro_name[1],
          metro_b = b,
          metro_b_name = rb$metro_name[1],
          delta_diff_obs = obs,
          ci_lo = as.numeric(stats::quantile(diff_sim, 0.025)),
          ci_hi = as.numeric(stats::quantile(diff_sim, 0.975)),
          p_value = p,
          n_draws = as.integer(n_draws)
        )
      }
    }
    if (length(mc_rows) > 0) {
      cross_tbl <- dplyr::bind_rows(mc_rows)
    }
  }
  list(per_metro = per_metro, cross_metro = cross_tbl)
}

#' Aggregate event-time cache rows into per-metro trajectories for charting.
#'
#' @param cache Output of [get_cross_city_cache()].
#' @param agg `month` (default) buckets by `floor(event_day / 30)`; `day` keeps daily rows.
#' @param metro_ids Optional metro filter; `NULL` = all metros in cache.
#' @param event_day_min Minimum event day (default 0 = adoption day).
#' @param event_day_max Optional upper bound on event days.
#' @return Named list with `metros` — one entry per metro, each with `points` tibble.
summarize_cross_city_trajectories <- function(cache,
                                              agg = c("month", "day"),
                                              metro_ids = NULL,
                                              event_day_min = 0L,
                                              event_day_max = NULL) {
  agg <- match.arg(agg)
  empty_pts <- tibble::tibble(
    event_day = integer(),
    event_month = integer(),
    years_since = double(),
    pct_change = double(),
    att = double(),
    se_att = double(),
    obs = double(),
    yhat0 = double(),
    yhat0_lo = double(),
    yhat0_hi = double(),
    yhat1 = double()
  )
  if (is.null(cache) || nrow(cache) == 0L) {
    return(list(metros = list()))
  }

  sub <- cache |>
    dplyr::filter(.data$event_day >= !!as.integer(event_day_min))
  if (!is.null(event_day_max) && length(event_day_max) == 1L &&
      is.finite(event_day_max)) {
    sub <- sub |> dplyr::filter(.data$event_day <= !!as.integer(event_day_max))
  }

  mids <- sub |>
    dplyr::distinct(.data$metro_id) |>
    dplyr::pull(.data$metro_id)
  if (!is.null(metro_ids) && length(metro_ids) > 0L) {
    mids <- intersect(mids, as.integer(metro_ids))
    sub <- sub |> dplyr::filter(.data$metro_id %in% mids)
  }
  if (length(mids) == 0L || nrow(sub) == 0L) {
    return(list(metros = list()))
  }

  .bucket_points <- function(msub) {
    if (nrow(msub) == 0L) {
      return(empty_pts)
    }
    if (identical(agg, "day")) {
      return(msub |>
        dplyr::transmute(
          event_day = as.integer(.data$event_day),
          event_month = as.integer(floor(.data$event_day / 30)),
          years_since = .data$event_day / 365.25,
          pct_change = .data$pct_change,
          att = .data$att,
          se_att = .data$se_att,
          obs = .data$obs,
          yhat0 = .data$yhat0,
          yhat0_lo = .data$yhat0_lo,
          yhat0_hi = .data$yhat0_hi,
          yhat1 = .data$yhat1
        ) |>
        dplyr::arrange(.data$event_day))
    }
    msub |>
      dplyr::mutate(event_month = as.integer(floor(.data$event_day / 30))) |>
      dplyr::group_by(.data$event_month) |>
      dplyr::summarize(
        event_day = max(.data$event_day, na.rm = TRUE),
        # pct_change before att/se_att: later columns shadow .data$se_att in dplyr summarize.
        pct_change = {
          ok <- is.finite(.data$pct_change)
          if (!any(ok)) {
            NA_real_
          } else {
            stats::weighted.mean(
              .data$pct_change[ok],
              w = 1 / pmax(.data$se_att[ok], 1e-6)^2,
              na.rm = TRUE
            )
          }
        },
        obs = {
          ok <- is.finite(.data$obs)
          if (!any(ok)) {
            NA_real_
          } else {
            stats::weighted.mean(
              .data$obs[ok],
              w = 1 / pmax(.data$se_att[ok], 1e-6)^2,
              na.rm = TRUE
            )
          }
        },
        yhat0 = {
          ok <- is.finite(.data$yhat0)
          if (!any(ok)) {
            NA_real_
          } else {
            stats::weighted.mean(
              .data$yhat0[ok],
              w = 1 / pmax(.data$se_att[ok], 1e-6)^2,
              na.rm = TRUE
            )
          }
        },
        yhat0_lo = {
          ok <- is.finite(.data$yhat0_lo)
          if (!any(ok)) {
            NA_real_
          } else {
            stats::weighted.mean(
              .data$yhat0_lo[ok],
              w = 1 / pmax(.data$se_att[ok], 1e-6)^2,
              na.rm = TRUE
            )
          }
        },
        yhat0_hi = {
          ok <- is.finite(.data$yhat0_hi)
          if (!any(ok)) {
            NA_real_
          } else {
            stats::weighted.mean(
              .data$yhat0_hi[ok],
              w = 1 / pmax(.data$se_att[ok], 1e-6)^2,
              na.rm = TRUE
            )
          }
        },
        yhat1 = {
          ok <- is.finite(.data$yhat1)
          if (!any(ok)) {
            NA_real_
          } else {
            stats::weighted.mean(
              .data$yhat1[ok],
              w = 1 / pmax(.data$se_att[ok], 1e-6)^2,
              na.rm = TRUE
            )
          }
        },
        att = {
          iv <- inverse_variance_mean(.data$att, .data$se_att)
          iv$att
        },
        se_att = {
          iv <- inverse_variance_mean(.data$att, .data$se_att)
          iv$se
        },
        .groups = "drop"
      ) |>
      dplyr::mutate(years_since = .data$event_day / 365.25) |>
      dplyr::select(
        "event_day", "event_month", "years_since",
        "pct_change", "att", "se_att", "obs", "yhat0", "yhat0_lo", "yhat0_hi", "yhat1"
      ) |>
      dplyr::arrange(.data$event_month)
  }

  metros <- purrr::map(mids, function(mid) {
    msub <- sub |> dplyr::filter(.data$metro_id == mid)
    meta <- msub |> dplyr::slice_head(n = 1)
    pts <- .bucket_points(msub)
    basis_vals <- unique(stats::na.omit(as.character(msub$basis)))
    metro_basis <- if (length(basis_vals) == 0L) {
      NA_character_
    } else if (length(basis_vals) == 1L) {
      basis_vals[[1]]
    } else if ("fitted" %in% basis_vals) {
      "fitted"
    } else {
      "anchored"
    }
    list(
      metro_id = as.integer(mid),
      metro_name = meta$metro_name[1],
      treatment_start_date = as.character(meta$treatment_start_date[1]),
      max_event_day = as.integer(max(msub$event_day, na.rm = TRUE)),
      partial_window = FALSE,
      basis = metro_basis,
      points = pts
    )
  })

  list(metros = metros)
}

# =============================================================================
# Hypothetical-zone ATT — "what would a cordon drawn HERE have done?"
#
# Powers POST /cross-city-att/zone-att. The caller posts an arbitrary GeoJSON
# polygon (a hypothetical congestion-pricing zone) plus a calendar interval; we
# answer with the ATT a policy over that footprint is estimated to have had.
#
# There is exactly ONE estimator behind this — the same pinned FECT ATT rows and
# the same `combine_att_window_d2*` pool that /window, /att monthly, and the
# paper's Table 2 use. Nothing here re-derives an effect. The polygon only ever
# selects WHICH pinned rows enter the pool:
#
#   polygon -> in-polygon monitors (PostGIS ST_Covers)
#           -> their per-monitor-day ATT rows in [date_from, date_to]
#           -> charged-days filter
#           -> combine_att_window_d2c()
#
# Resolution ladder, most to least specific. Every response says which rung it
# landed on (`zone_resolution`) and on which basis (`basis`), so a caller can
# never mistake a metro-grain answer for a sub-metro one:
#
#   polygon / fitted    per_day_metro_unit rows for the in-polygon monitors.
#                       This IS the hypothetical-zone ATT.
#   polygon / anchored  per_day_metro_unit_imputed rows (pins trained after
#                       2026-07-30 carry the anchored unit grain).
#   metro   / fitted    per_day_metro — the metro cannot be resolved below its
#                       own boundary for this pin.
#   metro   / anchored  per_day_metro_imputed — the M7 anchor sits at the metro
#                       basis (Milan, Singapore, the Nordics). A first-class
#                       answer, NOT a fallback; it just isn't sub-metro.
#   projection          the metro has no policy at all: there is no ATT to
#                       report. We return the cross-city pooled per-metro effect
#                       as an explicitly-labelled TRANSFER PROJECTION with
#                       `is_att = FALSE`. Never call this an ATT.
# =============================================================================

#' Normalize any GeoJSON payload to a bare geometry JSON string.
#'
#' Accepts a geometry, a Feature, or a FeatureCollection (features are merged
#' into a GeometryCollection so a multi-part zone still works). Returns NULL
#' when the payload is not recognizable GeoJSON — the caller turns that into a
#' 400 rather than letting PostGIS raise.
cpportal_zone_geojson_geometry <- function(gj) {
  if (is.character(gj) && length(gj) == 1L) {
    gj <- tryCatch(jsonlite::fromJSON(gj, simplifyVector = FALSE),
                   error = function(e) NULL)
  }
  if (!is.list(gj) || is.null(gj$type)) return(NULL)
  type <- as.character(gj$type)[1]
  geom_types <- c("Point", "MultiPoint", "LineString", "MultiLineString",
                  "Polygon", "MultiPolygon", "GeometryCollection")

  geom <- if (type %in% geom_types) {
    gj
  } else if (identical(type, "Feature")) {
    gj$geometry
  } else if (identical(type, "FeatureCollection")) {
    feats <- gj$features
    if (!is.list(feats) || length(feats) == 0L) return(NULL)
    geoms <- purrr::compact(purrr::map(feats, function(f) f$geometry))
    if (length(geoms) == 0L) return(NULL)
    if (length(geoms) == 1L) geoms[[1]]
    else list(type = "GeometryCollection", geometries = geoms)
  } else {
    NULL
  }
  if (!is.list(geom) || is.null(geom$type)) return(NULL)
  if (!as.character(geom$type)[1] %in% geom_types) return(NULL)
  tryCatch(jsonlite::toJSON(geom, auto_unbox = TRUE, digits = NA),
           error = function(e) NULL)
}

#' Monitors covered by a posted polygon, plus the metro the polygon sits in.
#'
#' One PostGIS round trip per question, both parameterized on the geometry text
#' (never string-concatenated). Returns a list with `monitors` (tibble of
#' monitor_id/metro_id), `metro_id`, `metro_name`, and `error` (non-NULL when
#' the geometry itself was rejected by ST_GeomFromGeoJSON).
cpportal_zone_resolve <- function(conn, geom_text, pollutant = "PM2.5",
                                  metro_id = NA_integer_) {
  empty <- tibble::tibble(monitor_id = character(0), metro_id = integer(0))
  if (is.null(conn) || is.null(geom_text)) {
    return(list(monitors = empty, metro_id = NA_integer_,
                metro_name = NA_character_, error = "no_db"))
  }

  # Resolve the containing metro first: it doubles as the GeoJSON validity
  # check, so an unparseable polygon fails once, cleanly, before we touch
  # monitors. Largest-overlap wins when a polygon straddles two metros.
  metro_sql <- paste0(
    "WITH poly AS (SELECT ST_SetSRID(ST_GeomFromGeoJSON($1::text), 4326) AS g) ",
    "SELECT mp.metro_id::int AS metro_id, mp.name::text AS metro_name ",
    "  FROM public.metro_polygons mp, poly p ",
    " WHERE ST_Intersects(mp.geometry::geometry, p.g) ",
    " ORDER BY ST_Area(ST_Intersection(mp.geometry::geometry, p.g)) DESC ",
    " LIMIT 1"
  )
  mrow <- tryCatch(DBI::dbGetQuery(conn, metro_sql, params = list(geom_text)),
                   error = function(e) {
                     structure(conditionMessage(e), class = "cpportal_geo_error")
                   })
  if (inherits(mrow, "cpportal_geo_error")) {
    return(list(monitors = empty, metro_id = NA_integer_,
                metro_name = NA_character_, error = as.character(mrow)))
  }

  mid <- if (!is.null(mrow) && nrow(mrow) > 0L) as.integer(mrow$metro_id[1]) else NA_integer_
  mnm <- if (!is.null(mrow) && nrow(mrow) > 0L) as.character(mrow$metro_name[1]) else NA_character_
  if (is.finite(suppressWarnings(as.integer(metro_id)))) mid <- as.integer(metro_id)

  mon_sql <- paste0(
    "WITH poly AS (SELECT ST_SetSRID(ST_GeomFromGeoJSON($1::text), 4326) AS g) ",
    "SELECT DISTINCT m.fullaqsid::text AS monitor_id, m.metro_id::int AS metro_id ",
    "  FROM public.monitors m, poly p ",
    " WHERE m.parameter = $2::text ",
    "   AND m.longitude IS NOT NULL AND m.latitude IS NOT NULL ",
    "   AND ST_Covers(p.g, ST_SetSRID(ST_MakePoint(m.longitude::double precision, ",
    "                                              m.latitude::double precision), 4326))"
  )
  mons <- tryCatch(
    DBI::dbGetQuery(conn, mon_sql, params = list(geom_text, as.character(pollutant))),
    error = function(e) {
      log_warn("zone_att", paste0("monitor lookup: ", conditionMessage(e)))
      NULL
    }
  )
  mons <- if (is.null(mons) || nrow(mons) == 0L) empty else tibble::as_tibble(mons)

  list(monitors = mons, metro_id = mid, metro_name = mnm, error = NULL)
}

#' Zone ATT over a calendar window, given the pin's ATT table and a monitor set.
#'
#' `att` must already be pollutant-filtered (`cross_city_filter_bundle_att`).
#' Walks the resolution ladder documented above and returns a one-row tibble in
#' `combine_att_window_d2c` shape plus `zone_resolution`, `n_monitors_in_zone`,
#' and `method`. Returns NULL when the metro has no ATT rows at all (the caller
#' then decides between a transfer projection and a 4xx).
zone_window_att <- function(att, monitor_ids, metro_id, day_start, day_end,
                            include_inactive = FALSE) {
  if (!is.data.frame(att) || nrow(att) == 0L || !"type" %in% names(att)) return(NULL)
  intc <- function(x) suppressWarnings(as.integer(as.character(x)))
  date_col <- intersect(c("day", "date"), names(att))[1]
  if (is.na(date_col)) return(NULL)
  ds <- as.Date(day_start); de <- as.Date(day_end)
  mid <- suppressWarnings(as.integer(metro_id))
  monitor_ids <- as.character(monitor_ids)

  in_window <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0L) return(rows)
    d <- suppressWarnings(as.Date(rows[[date_col]]))
    rows[!is.na(d) & d >= ds & d <= de, , drop = FALSE]
  }
  charged <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0L) return(rows)
    cpportal_charged_days_filter(rows, date_col = date_col,
                                 include_inactive = include_inactive)
  }
  rows_of <- function(type_name, restrict_monitors) {
    r <- att |> dplyr::filter(.data$type == !!type_name)
    if (nrow(r) == 0L) return(r)
    if (is.finite(mid) && "metro_id" %in% names(r)) {
      r <- r[intc(r$metro_id) == mid, , drop = FALSE]
    }
    if (restrict_monitors) {
      id_col <- .cpportal_unit_id_col(r)
      if (is.na(id_col)) return(r[0, , drop = FALSE])
      r <- r[as.character(r[[id_col]]) %in% monitor_ids, , drop = FALSE]
    }
    charged(in_window(r))
  }
  n_units <- function(rows) {
    if (is.null(rows) || nrow(rows) == 0L) return(0L)
    id_col <- .cpportal_unit_id_col(rows)
    if (is.na(id_col)) return(0L)
    dplyr::n_distinct(as.character(rows[[id_col]]))
  }

  finish <- function(out, resolution, n_mon, method) {
    out$zone_resolution    <- resolution
    out$n_monitors_in_zone <- as.integer(n_mon)
    out$method             <- method
    out$is_att             <- TRUE
    out
  }

  have_monitors <- length(monitor_ids) > 0L

  # Rung 1/2 — sub-metro. Per-MONITOR-day rows for the in-polygon monitors are
  # both the pool AND the between-monitor variance input (D2c), so a zone with
  # one monitor degrades to D2b rather than claiming a spread it cannot see.
  if (have_monitors) {
    fit_u <- rows_of("per_day_metro_unit", TRUE)
    if (nrow(fit_u) > 0L) {
      return(finish(combine_att_window_d2c(fit_u, fit_u, basis = "fitted",
                                           include_inactive = include_inactive),
                    "polygon", n_units(fit_u),
                    "S4 direct pool of in-polygon per-monitor-day fitted ATTs (D2c)"))
    }
    anc_u <- rows_of("per_day_metro_unit_imputed", TRUE)
    if (nrow(anc_u) > 0L) {
      return(finish(combine_att_window_d2c(anc_u, anc_u, basis = "anchored",
                                           include_inactive = include_inactive),
                    "polygon", n_units(anc_u),
                    "S4 direct pool of in-polygon per-monitor-day anchored ATTs (D2c)"))
    }
  }

  # Rung 3/4 — metro grain. The pin carries no per-monitor decomposition for
  # this metro/window, so the zone CANNOT be resolved below the metro boundary.
  # Say so in `zone_resolution`; never synthesize within-metro detail.
  fit_m <- rows_of("per_day_metro", FALSE)
  if (nrow(fit_m) > 0L) {
    unit_m <- rows_of("per_day_metro_unit", FALSE)
    return(finish(combine_att_window_d2c(fit_m, unit_m, basis = "fitted",
                                         include_inactive = include_inactive),
                  "metro", n_units(unit_m),
                  "metro-grain fitted window ATT (D2c) — zone not sub-metro resolvable"))
  }
  anc_m <- rows_of("per_day_metro_imputed", FALSE)
  if (nrow(anc_m) > 0L) {
    unit_a <- rows_of("per_day_metro_unit_imputed", FALSE)
    return(finish(combine_att_window_d2c(anc_m, unit_a, basis = "anchored",
                                         include_inactive = include_inactive),
                  "metro", n_units(unit_a),
                  "metro-grain anchored window ATT (D2c) — zone not sub-metro resolvable"))
  }
  NULL
}

#' Transfer projection for a metro with NO congestion-pricing policy.
#'
#' There is no counterfactual for a zone that was never treated, so there is no
#' ATT. What we CAN say is what the policy did elsewhere: the inverse-variance
#' pool of every treated metro's `per_metro` ATT in the pin. Returned with
#' `is_att = FALSE` and `basis = "projected"` so no caller can render it as a
#' measured effect for this city.
zone_transfer_projection <- function(att) {
  if (!is.data.frame(att) || nrow(att) == 0L || !"type" %in% names(att)) return(NULL)
  pm <- att |> dplyr::filter(.data$type == "per_metro")
  if (nrow(pm) == 0L) return(NULL)
  numc <- function(x) suppressWarnings(as.numeric(as.character(x)))
  iv <- inverse_variance_mean(numc(pm$att), numc(pm$se_att))
  if (!is.finite(iv$att)) return(NULL)
  tibble::tibble(
    att = iv$att, se_att = iv$se,
    att_lo = iv$att - 1.96 * iv$se, att_hi = iv$att + 1.96 * iv$se,
    n_days = 0L, basis = "projected",
    se_source = "inverse_variance_pool_of_per_metro",
    varc_shared = NA_real_, varc_indep = NA_real_,
    n_monitors = 0L, s2_between = NA_real_,
    zone_resolution = "projection", n_monitors_in_zone = 0L,
    method = paste0("cross-city transfer projection — inverse-variance pool of ",
                    iv$n, " treated metros' per_metro ATTs; NOT an ATT for this zone"),
    is_att = FALSE, n_metros_pooled = as.integer(iv$n)
  )
}

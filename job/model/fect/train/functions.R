# =============================================================================
# functions.R (job/model/fect/train)
#
# Multi-spec FECT (cfe) training: metro_ids from public.metro_polygons,
# get_fect(), get_qis_fect(), diagnostics via get_gof_fect() for app pins.
#
# Assumes job/model/functions.R, job/model/did/functions_did.R, and
# job/model/fect/functions_fect.R are already source()'d (see job.R).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr, warn.conflicts = FALSE)
  library(dbplyr, warn.conflicts = FALSE)
  library(DBI, warn.conflicts = FALSE)
  library(RPostgres, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  library(purrr, warn.conflicts = FALSE)
  library(pins, warn.conflicts = FALSE)
})

`%||%` = function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------------------------
# Outcome catalogue from public.model_panel_daily
# -----------------------------------------------------------------------------

# AQ outcomes: same column family as the legacy aq_daily_mean default; sqrt
# transform mirrors default_outcome_aq() in job/model/functions.R.
fect_aq_outcomes = c(
  "aq_daily_mean", "aq_daily_med", "aq_daily_max",
  "aq_mp_mean", "aq_mp_med",
  "aq_ep_mean", "aq_ep_med",
  "aq_do_mean", "aq_do_med",
  "aq_no_mean", "aq_no_med"
)

# Traffic outcomes: speeds in km/h (or analogous); identity transform — bounded,
# not log-skewed. Pre-treatment data is sparse for most treated metros, so MC.
fect_traffic_outcomes = c(
  "traffic_daily_mean",
  "traffic_mp_mean", "traffic_ep_mean",
  "traffic_do_mean", "traffic_no_mean"
)

# Sample catalogue: drop eu_priority — keep US and All only.
#
# As of 2026-06, US-only models are unused — every consumer reads the All
# sample. us_priority is left in the catalogue (commented) so it can be
# revived later by uncommenting and re-running the job, or by passing
# CPPORTAL_FECT_SAMPLES=us_priority,all_priority on the Connect content.
fect_sample_catalogue = list(
  # us_priority  = list(label = "US priority", iso_id = "USA"),  # disabled 2026-06
  all_priority = list(label = "All priority", iso_id = NULL)
)

# -----------------------------------------------------------------------------
# Date window (shared by all FECT specs)
# -----------------------------------------------------------------------------

fect_default_date_window = function() {
  date_to_env = Sys.getenv("CPPORTAL_FECT_DATE_TO", unset = "")
  date_to = if (nzchar(date_to_env)) {
    as.Date(date_to_env)
  } else {
    Sys.Date()
  }
  date_from_env = Sys.getenv("CPPORTAL_FECT_DATE_FROM", unset = "")
  date_from = if (nzchar(date_from_env)) {
    as.Date(date_from_env)
  } else {
    as.Date("2000-01-01")
  }
  list(date_from = date_from, date_to = date_to)
}

# -----------------------------------------------------------------------------
# Metro IDs from metro_polygons (priority flag; optional iso_id e.g. USA)
# -----------------------------------------------------------------------------

fetch_priority_metro_ids = function(db, iso_id = NULL) {
  # Raw DBI rather than dbplyr: on Connect the dbplyr `db |> tbl(in_schema(...))`
  # chain has surfaced an "Invalid connection" error even when DBI::dbGetQuery
  # against the same connection works. Investigation: HANDOFF.md (2026-06-09).
  has_iso = !is.null(iso_id) && nzchar(as.character(iso_id))
  if (has_iso) {
    res = DBI::dbGetQuery(
      db,
      "SELECT DISTINCT metro_id FROM public.metro_polygons
       WHERE priority = TRUE AND iso_id = $1",
      params = list(as.character(iso_id))
    )
  } else {
    res = DBI::dbGetQuery(
      db,
      "SELECT DISTINCT metro_id FROM public.metro_polygons
       WHERE priority = TRUE"
    )
  }
  ids = sort(unique(as.integer(res$metro_id)))
  message(
    "[fect.train.fetch_priority_metro_ids] iso_filter=",
    if (!has_iso) "(none)" else as.character(iso_id),
    " n=", length(ids)
  )
  ids
}

# -----------------------------------------------------------------------------
# Spec assembly helpers
# -----------------------------------------------------------------------------

fect_outcome_kind = function(outcome_var) {
  if (startsWith(outcome_var, "aq_"))      return("aq")
  if (startsWith(outcome_var, "traffic_")) return("traffic")
  stop("[fect.train] unknown outcome family for: ", outcome_var, call. = FALSE)
}

# -----------------------------------------------------------------------------
# Partition (hourly vs daily) selection
# -----------------------------------------------------------------------------
# model_panel_daily stores one row per (monitor, local-day): partition_key
# '<pollutant>-1HR' when any hourly reading existed that day (value = mean of
# hourly), else '<pollutant>-24HR' (a daily-only reading). Historically FECT read
# ONLY -1HR, so daily-only metros (e.g. Singapore 2017-2024, Taipei) were absent.
#
# CPPORTAL_FECT_PARTITION_MODE:
#   "1hr"  (default) — hourly-only; identical to prior behavior (no-op).
#   "both"           — ALSO read -24HR rows, but only for daily-aggregate
#                      outcomes (see fect_outcome_daily_compatible). This lets
#                      daily-only metros enter the daily-mean / daily-median
#                      models. A monitor-day is only ever ONE partition, so
#                      reading both never produces duplicate (fullaqsid, date)
#                      unit-time keys.
fect_partition_mode = function() {
  m = tolower(Sys.getenv("CPPORTAL_FECT_PARTITION_MODE", "1hr"))
  if (!(m %in% c("1hr", "both"))) {
    message("[fect.train] unknown CPPORTAL_FECT_PARTITION_MODE='", m,
            "', falling back to '1hr'")
    m = "1hr"
  }
  m
}

# Outcomes well-defined from a single daily (-24HR) reading: the daily mean and
# daily median both equal that day's value. daily_max and the time-of-day window
# means (mp/ep/do/no) require sub-daily data and are NULL on -24HR rows, so they
# stay hourly-only even in "both" mode (including them would model a daily mean
# as if it were a peak/window).
fect_outcome_daily_compatible = function(outcome_var) {
  outcome_var %in% c("aq_daily_mean", "aq_daily_med")
}

# The partition_key set an outcome should read. Always includes <pollutant>-1HR;
# adds <pollutant>-24HR only for daily-compatible AQ outcomes in "both" mode.
fect_outcome_partition_keys = function(outcome_var, pollutant = "PM2.5") {
  pk_1hr = paste0(pollutant, "-1HR")
  if (identical(fect_partition_mode(), "both") &&
      identical(fect_outcome_kind(outcome_var), "aq") &&
      fect_outcome_daily_compatible(outcome_var)) {
    c(pk_1hr, paste0(pollutant, "-24HR"))
  } else {
    pk_1hr
  }
}

# AQ: sqrt for variance stabilization (matches default_outcome_aq()).
# Traffic: identity (speeds are bounded).
fect_outcome_formula = function(outcome_var) {
  kind = fect_outcome_kind(outcome_var)
  if (identical(kind, "aq")) {
    return(stats::as.formula(paste0("~ sqrt(", outcome_var, ")")))
  }
  stats::as.formula(paste0("~ ", outcome_var))
}

# AQ specs reuse the legacy default covariates + Z; traffic specs use the
# narrower set (default_covariates_traffic). All declared in functions_fect.R.
#
# CPPORTAL_FECT_AQ_METHOD overrides the method for AQ specs (default "cfe").
# Set to "mc" to use matrix completion, which doesn't drop never-pre-treated
# units the way CFE's <5-untreated-periods rule does. Used to ad-hoc-test
# whether MC produces ATTs for Stockholm/Milan/Singapore. Not for production.
#
# The AQ covariate list is overridable via CPPORTAL_FECT_AQ_COVARIATES_OVERRIDE
# (comma-separated column names). Use it to chase covariate-collinearity
# singular-matrix errors from `fect::fect(method="cfe")` — see HANDOFF.md and
# scripts/diagnose_fect_mp_mean_covariates.sh. The override only applies to
# AQ specs; traffic specs use the static narrower set below.
fect_aq_default_covariates = c(
  "temp_daily_mean", "rhum_daily_mean", "precip_daily_mean",
  "ws_daily_mean", "wd_daily_mean_deg", "barpr_daily_mean",
  "population", "bg3",
  # sat_monthly_mean is the per-monitor per-month 1km² ACAG satellite PM2.5
  # (already attached inline to model_panel_daily by
  # sp_model_panel_refresh_slice). Adding it as a time-varying covariate —
  # rather than the historical `sat_longrun` 1998–2024 mean used only in
  # the hedonic α — lets the counterfactual track actual per-month
  # pollution shifts (fleet turnover, seasonal signals). This addresses
  # the temporal-circularity concern in the anchored path: using a
  # treated-era-inclusive mean as the "baseline" was pulling counterfactuals
  # below the true no-policy trajectory and driving spurious positive ATTs.
  # (See ADR-0011 §7.2 and Tim, 2026-06-30.)
  "sat_monthly_mean",
  # metro_pop_density_year: per-(metro, year) WorldPop-derived population
  # density (people / km²), computed from population_raster_cache 1km²
  # rasters clipped to metro_polygons.geometry. Populated by
  # sp_panel_attach_metro_pop_density (see migration 20260701050000).
  # Purpose (Tim 2026-06-30): the existing `population` covariate is the
  # per-monitor BUFFER count and captures only local siting density; it
  # does NOT capture metro-wide urban expansion. New residents who move
  # to London/Milan without moving into a specific monitor's buffer still
  # drive around the city and add PM2.5 there. metro_pop_density_year
  # varies per (metro, year) so it is not absorbed by the unit OR date
  # FE — unlike sat_global_monthly_mean which fect correctly rejected as
  # time-invariant (§7.3). Density (not raw total) because metro area is
  # fixed per city so total = density × area would be collinear.
  "metro_pop_density_year"
  # NOTE 2026-07-01: metro_total_pop_year and metro_zone_share_year were
  # tested alongside metro_pop_density_year. Per Tim: pick ONE population
  # covariate (density chosen — more physically meaningful, captures urban
  # intensity rather than metro-size scaling). Zone-share is likely
  # endogenous as-is (near-zero pre-policy, jumps at policy start) so it
  # will be re-tested LAGGED once the 3-year pre-observation pop backfill
  # (task #15) is done. The columns still exist on model_panel_daily as
  # diagnostics; not in the fect fit.
  # NOTE 2026-07-01: sat_global_monthly_mean was tested here and REJECTED
  # by fect with "Variable ... is time-invariant. Try to remove it." The
  # global monthly mean is a scalar per date (identical across all
  # monitors), so it's perfectly collinear with the date fixed effect in
  # the two-way FE model. That's actually the answer to Tim's original
  # concern about M_t "leaking" global trend: **the date FE already
  # captures global fleet-turnover trend by construction.** The positive
  # ATTs on London / Milan are the model's honest read of "given the
  # global trend, treated central-city monitors deviated positively" —
  # not a bug in M_t. sat_metro_month_pooled + v_sat_global_month +
  # sat_global_monthly_mean on model_panel_daily remain in the DB as
  # useful diagnostics + paper-reporting infrastructure, but not as a
  # FECT covariate. See docs/model/README_fect_uncertainty.md §7.3.
)
fect_outcome_covariates = function(outcome_var) {
  if (identical(fect_outcome_kind(outcome_var), "aq")) {
    aq_override = Sys.getenv("CPPORTAL_FECT_AQ_COVARIATES_OVERRIDE", "")
    aq_cov_terms = if (nzchar(aq_override)) {
      v = trimws(strsplit(aq_override, ",", fixed = TRUE)[[1]])
      v[nzchar(v)]
    } else {
      fect_aq_default_covariates
    }
    aq_method = Sys.getenv("CPPORTAL_FECT_AQ_METHOD", "cfe")
    if (!(aq_method %in% c("cfe", "mc"))) {
      message("[fect.train] unknown CPPORTAL_FECT_AQ_METHOD='", aq_method,
              "', falling back to 'cfe'")
      aq_method = "cfe"
    }
    list(
      covariates    = stats::as.formula(
        paste0("~ ", paste(aq_cov_terms, collapse = " + "))
      ),
      z_covariates  = "dist_km_motorway_trunk_primary_secondary",
      fect_method   = aq_method
    )
  } else {
    list(
      covariates   = stats::as.formula(
        paste0(
          "~ temp_daily_mean + rhum_daily_mean + population + ",
          "dist_km_motorway_trunk_primary_secondary"
        )
      ),
      z_covariates = character(0),
      fect_method  = "mc"
    )
  }
}

# -----------------------------------------------------------------------------
# Model spec catalogue: cartesian product (sample × outcome)
# -----------------------------------------------------------------------------

#' Build one fect spec list per (sample × outcome)
#'
#' Pin name convention: `fect_{sample}_{outcome}` — e.g.
#' `fect_us_priority_aq_daily_mean`, `fect_all_priority_traffic_mp_mean`.
#' The legacy short pins `fect_us_priority` / `fect_all_priority` are written
#' separately by `write_bundle()` as aliases of the matching `_aq_daily_mean`
#' bundles, so existing app loaders keep resolving.
#'
#' @param db DBI connection.
#' @param outcomes Character vector of outcome columns; defaults to all 16
#'   (11 AQ + 5 traffic). Filterable via `CPPORTAL_FECT_OUTCOMES`.
#' @param sample_ids Character vector of sample ids (subset of `us_priority`,
#'   `all_priority`).
#' @param system_types Character vector of `congestion_pricing_periods.system_type`
#'   values to count as treated under zone-spatial classification.
#' @return Named list of spec lists, keyed by `pin_name`.
fect_model_specs = function(
    db,
    outcomes = c(fect_aq_outcomes, fect_traffic_outcomes),
    sample_ids = names(fect_sample_catalogue),
    system_types = c("cordon"),
    treatment_scopes = c("zone")
) {
  treatment_scopes = match.arg(treatment_scopes, c("zone", "metro"),
                               several.ok = TRUE)
  dw = fect_default_date_window()

  metro_ids_by_sample = purrr::map(
    fect_sample_catalogue[sample_ids],
    \(s) fetch_priority_metro_ids(db, iso_id = s$iso_id)
  )

  # Compute traffic min-date per sample once (one SQL each). NULL if no traffic
  # rows exist for the sample's metros — fetch_panel falls back to spec$date_from.
  needs_traffic_min_date = any(
    vapply(outcomes, \(o) identical(fect_outcome_kind(o), "traffic"), logical(1))
  )
  traffic_min_date_by_sample = if (needs_traffic_min_date) {
    purrr::map(
      sample_ids,
      \(sid) traffic_panel_min_date(db, metro_ids_by_sample[[sid]])
    ) |> rlang::set_names(sample_ids)
  } else {
    rlang::set_names(vector("list", length(sample_ids)), sample_ids)
  }

  # AQ min-date per (sample, outcome). Defended against the
  # `inv_sympd(): matrix is singular or not positive definite` failure when
  # the panel includes sparse early years where the outcome appears in only
  # a handful of treated cells. One query per (sample, outcome) — cheap.
  aq_outcomes_in_scope = outcomes[
    vapply(outcomes, \(o) identical(fect_outcome_kind(o), "aq"), logical(1))
  ]
  aq_min_date_by_sample_outcome = list()
  for (sid in sample_ids) {
    for (ov in aq_outcomes_in_scope) {
      aq_min_date_by_sample_outcome[[paste(sid, ov, sep = "::")]] =
        aq_panel_min_date(db, metro_ids_by_sample[[sid]], ov,
                          system_types = system_types,
                          partition_keys = fect_outcome_partition_keys(ov, "PM2.5"))
    }
  }

  grid = tidyr::expand_grid(
    sample_id       = sample_ids,
    outcome_var     = outcomes,
    treatment_scope = treatment_scopes
  )

  specs = purrr::pmap(grid, function(sample_id, outcome_var, treatment_scope) {
    cov_set     = fect_outcome_covariates(outcome_var)
    kind        = fect_outcome_kind(outcome_var)
    # ZONE specs keep the historical name `fect_{sample}_{outcome}` exactly —
    # PAPER and the live dashboard read those 22 pins concurrently and must not
    # move. METRO specs get a `_metro` suffix (a new, parallel pin namespace).
    pin_name    = if (identical(treatment_scope, "metro")) {
      paste0("fect_", sample_id, "_", outcome_var, "_metro")
    } else {
      paste0("fect_", sample_id, "_", outcome_var)
    }
    sample_meta = fect_sample_catalogue[[sample_id]]
    min_date    = if (identical(kind, "traffic")) {
      traffic_min_date_by_sample[[sample_id]]
    } else {
      aq_min_date_by_sample_outcome[[paste(sample_id, outcome_var, sep = "::")]]
    }
    treat_desc  = if (identical(treatment_scope, "metro")) {
      "treatment = monitor anywhere in a metro with an active period"
    } else {
      "treatment = monitor inside zones.geometry"
    }
    list(
      id           = pin_name,
      pin_name     = pin_name,
      sample_id    = sample_id,
      outcome_var  = outcome_var,
      outcome_kind = kind,
      treatment_scope = treatment_scope,
      label        = sprintf("FECT %s (%s%s)", sample_meta$label, outcome_var,
                             if (identical(treatment_scope, "metro")) ", metro" else ""),
      name         = sprintf(
        "FECT %s — %s (%s, %s, %s)",
        sample_meta$label, outcome_var, cov_set$fect_method,
        if (identical(kind, "aq")) "sqrt" else "identity",
        treatment_scope
      ),
      description  = paste0(
        "Counterfactual (FECT ", toupper(cov_set$fect_method),
        ") on ", sample_meta$label, " metros (priority=TRUE",
        if (!is.null(sample_meta$iso_id)) {
          paste0(" AND iso_id='", sample_meta$iso_id, "'")
        } else {
          ""
        },
        ") with outcome '", outcome_var, "'; ",
        treat_desc, " for system_type in (",
        paste(system_types, collapse = ", "), "); analytic SEs."
      ),
      metro_ids    = metro_ids_by_sample[[sample_id]],
      outcome      = fect_outcome_formula(outcome_var),
      covariates   = cov_set$covariates,
      z_covariates = cov_set$z_covariates,
      fect_method  = cov_set$fect_method,
      system_types = as.character(system_types),
      pollutant    = "PM2.5",
      partition_key = "PM2.5-1HR",
      partition_keys = fect_outcome_partition_keys(outcome_var, "PM2.5"),
      date_from    = dw$date_from,
      date_to      = dw$date_to,
      min_date     = min_date
    )
  })

  rlang::set_names(specs, vapply(specs, \(s) s$pin_name, character(1)))
}

# -----------------------------------------------------------------------------
# Flatten one-row GOF tibble for pins::pin_write metadata (scalar JSON-friendly)
# -----------------------------------------------------------------------------

fect_pin_metadata_gof = function(gof_tbl) {
  if (is.null(gof_tbl) || nrow(gof_tbl) < 1L) {
    return(list())
  }
  g = gof_tbl[1L, , drop = FALSE]
  vs = g$vif_scope
  if (length(vs) == 1L && !is.na(vs)) {
    vs = as.character(vs)
  } else {
    vs = NA_character_
  }
  ro = g$rmse_on
  if (length(ro) == 1L && !is.na(ro)) {
    ro = as.character(ro)
  } else {
    ro = NA_character_
  }
  list(
    rsq              = as.numeric(g$rsq)[1],
    r_squared_fect   = as.numeric(g$r_squared_fect)[1],
    sigma            = as.numeric(g$sigma)[1],
    rmse             = as.numeric(g$rmse)[1],
    mae              = as.numeric(g$mae)[1],
    ymin             = as.numeric(g$ymin)[1],
    ymax             = as.numeric(g$ymax)[1],
    y_range          = as.numeric(g$range)[1],
    rmse_vs_yrange   = as.numeric(g$rmsevsrange)[1],
    mae_vs_yrange    = as.numeric(g$maevsrange)[1],
    vifmax           = as.numeric(g$vifmax)[1],
    vif_scope        = vs,
    gof_nobs         = as.integer(g$nobs)[1],
    rmse_on          = ro
  )
}

fect_pin_metadata_tests = function(tests_tbl) {
  if (is.null(tests_tbl) || nrow(tests_tbl) < 1L) {
    return(list())
  }
  t = tests_tbl[1L, , drop = FALSE]
  list(
    pretrend_f_p = as.numeric(t$pretrend_f_p)[1],
    equiv_p      = as.numeric(t$equiv_p)[1]
  )
}

# -----------------------------------------------------------------------------
# Panel column resolution
# -----------------------------------------------------------------------------

# Always-present columns regardless of outcome/covariates choice.
fect_panel_required_cols = function() {
  c("metro_id", "fullaqsid", "date")
}

# Covariate NAMES from a spec's `covariates` entry.
#
# READ THIS BEFORE USING `spec$covariates` AS IF IT WERE A CHARACTER VECTOR.
# `fect_outcome_covariates()` builds it with `stats::as.formula()`, so it is a
# ONE-SIDED FORMULA (`~ temp_daily_mean + rhum_daily_mean + ...`). Calling
# `as.character()` on such a formula returns
#     c("~", "temp_daily_mean + rhum_daily_mean + ...")
# — the tilde, plus the ENTIRE right-hand side as a single string. It never
# returns a vector of column names. Any downstream
# `intersect(x_vars, names(panel))` therefore comes back EMPTY, and the caller
# silently proceeds as though the spec had no covariates at all. That is exactly
# how Stage F shipped running with cov_adj = 0 (fixed 2026-08-07).
#
# Always resolve through this helper. Character input is passed through
# unchanged so callers may hand it either form; NULL yields character(0).
fect_cov_names = function(x) {
  if (is.null(x)) return(character(0))
  if (inherits(x, "formula")) return(all.vars(x))
  as.character(x)
}

# Outcome + covariate column names from a spec, deduplicated.
fect_spec_panel_cols = function(spec) {
  cov_vars = if (is.null(spec$covariates)) character(0) else all.vars(spec$covariates)
  z_vars   = if (is.null(spec$z_covariates) || length(spec$z_covariates) == 0L) {
    character(0)
  } else {
    as.character(spec$z_covariates)
  }
  unique(c(
    fect_panel_required_cols(),
    spec$outcome_var,
    cov_vars,
    z_vars
  ))
}

# Skip threshold on **observed** treated rows (i.e. treated rows where the
# spec's outcome is non-NA). Traffic specs default to 1 because TomTom is just
# starting to backfill cordon monitors — one observed treated row is enough to
# attempt MC; below that, the bundle is written as a placeholder. AQ specs
# default to 20.
fect_min_treated_obs = function(spec) {
  if (identical(spec$outcome_kind, "traffic")) {
    env = Sys.getenv("CPPORTAL_FECT_MIN_TREATED_OBS_TRAFFIC", unset = "")
    if (nzchar(env)) {
      v = suppressWarnings(as.integer(env))
      if (is.finite(v) && v >= 0L) return(v)
    }
    return(1L)
  }
  20L
}

# -----------------------------------------------------------------------------
# Dynamic traffic min-date (from public.model_panel_daily traffic coverage)
# -----------------------------------------------------------------------------

# Returns the earliest date in `model_panel_daily` for the given metros where
# `traffic_daily_mean` is non-NA. NULL when no traffic rows exist in scope. An
# explicit `CPPORTAL_FECT_TRAFFIC_MIN_DATE` env var overrides the lookup.
traffic_panel_min_date = function(db, metro_ids) {
  override = Sys.getenv("CPPORTAL_FECT_TRAFFIC_MIN_DATE", unset = "")
  if (nzchar(override)) {
    d = tryCatch(as.Date(override), error = function(e) NA)
    if (!is.na(d)) {
      message("[fect.train.traffic_panel_min_date] override=", format(d))
      return(d)
    }
    message(
      "[fect.train.traffic_panel_min_date] CPPORTAL_FECT_TRAFFIC_MIN_DATE='",
      override, "' did not parse; ignoring"
    )
  }
  if (is.null(db) || length(metro_ids) == 0L) return(NULL)

  metro_arr = paste0("{", paste(as.integer(metro_ids), collapse = ","), "}")
  q = "SELECT MIN(date)::text AS d
       FROM public.model_panel_daily
       WHERE traffic_daily_mean IS NOT NULL
         AND metro_id = ANY($1::bigint[])"
  res = tryCatch(
    DBI::dbGetQuery(db, q, params = list(metro_arr)),
    error = function(e) {
      message("[fect.train.traffic_panel_min_date] SQL failed: ",
              conditionMessage(e))
      NULL
    }
  )
  if (is.null(res) || nrow(res) < 1L) return(NULL)
  d_chr = res$d[[1]]
  if (is.null(d_chr) || is.na(d_chr) || !nzchar(d_chr)) return(NULL)
  as.Date(d_chr)
}

# -----------------------------------------------------------------------------
# Dynamic AQ min-date (control-availability mechanism)
# -----------------------------------------------------------------------------

# Why this exists: the cfe estimator drops periods that contain no untreated
# observations. When the early panel for an outcome is dominated by ONE
# in-zone monitor (= a single treated unit, no control), every early period
# gets dropped and the remaining matrix is degenerate, producing
# inv_sympd: matrix is singular or not positive definite from cfe_sub.cpp.
#
# Mechanism-grounded fix: start the panel at the first month where at least
# one OUT-OF-ZONE monitor reports the outcome (a candidate control), sustained
# for the next CPPORTAL_FECT_AQ_CONTROL_SUSTAIN_MONTHS (default 3) months.
# Zone membership comes from ST_Intersects(monitors.geom, zones.geometry)
# filtered to `system_types`, mirroring fetch_zone_treated_pairs() exactly so
# the heuristic agrees with the per-row treatment assignment.
#
# Replaces the earlier row-density heuristic, which was empirically calibrated
# but mechanistically wrong; see appendix A.3.
aq_panel_min_date = function(db, metro_ids, outcome_var,
                             system_types = c("cordon"),
                             sustain_months = NULL,
                             partition_keys = "PM2.5-1HR") {
  if (is.null(sustain_months)) {
    env_v = Sys.getenv("CPPORTAL_FECT_AQ_CONTROL_SUSTAIN_MONTHS", unset = "")
    sustain_months = if (nzchar(env_v)) {
      v = suppressWarnings(as.integer(env_v))
      if (is.finite(v) && v >= 1L) v else 3L
    } else {
      3L
    }
  }
  if (is.null(db) || length(metro_ids) == 0L) return(NULL)
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", outcome_var)) {
    stop("[fect.train.aq_panel_min_date] refusing unsafe outcome_var: ",
         outcome_var)
  }
  metro_arr     = paste0("{", paste(as.integer(metro_ids), collapse = ","), "}")
  system_arr    = paste0("{", paste(as.character(system_types), collapse = ","), "}")
  partition_arr = paste0("{", paste(as.character(partition_keys), collapse = ","), "}")
  q = paste0("
    WITH zoned_monitors AS (
      SELECT DISTINCT m.metro_id, m.fullaqsid
      FROM public.monitors m
      JOIN public.zones z
        ON ST_Intersects(m.geom, z.geometry)
      JOIN public.congestion_pricing_periods cpp
        ON cpp.id = z.policy_id
      WHERE cpp.treated = TRUE
        AND cpp.system_type = ANY($2::text[])
        AND m.metro_id = ANY($1::bigint[])
    ),
    cal AS (
      SELECT generate_series(
        date_trunc('month', (SELECT MIN(date) FROM public.model_panel_daily
                             WHERE partition_key = ANY($3::text[])
                               AND metro_id = ANY($1::bigint[])))::date,
        date_trunc('month', CURRENT_DATE)::date,
        interval '1 month'
      )::date AS month
    ),
    monthly_control AS (
      SELECT date_trunc('month', mpd.date)::date AS month,
             COUNT(DISTINCT mpd.fullaqsid) AS n_control_monitors
      FROM public.model_panel_daily mpd
      LEFT JOIN zoned_monitors zm
        ON zm.fullaqsid = mpd.fullaqsid AND zm.metro_id = mpd.metro_id
      WHERE mpd.partition_key = ANY($3::text[])
        AND mpd.metro_id = ANY($1::bigint[])
        AND mpd.", outcome_var, " IS NOT NULL
        AND zm.fullaqsid IS NULL
      GROUP BY 1
    ),
    filled AS (
      SELECT cal.month, COALESCE(mc.n_control_monitors, 0) AS n
      FROM cal LEFT JOIN monthly_control mc USING (month)
    ),
    sustain AS (
      SELECT month,
             MIN(n) OVER (ORDER BY month
                          ROWS BETWEEN CURRENT ROW AND ",
                          as.integer(sustain_months) - 1L, " FOLLOWING) AS min_n_window
      FROM filled
    )
    SELECT MIN(month)::text AS d FROM sustain WHERE min_n_window >= 1")
  res = tryCatch(
    DBI::dbGetQuery(db, q, params = list(metro_arr, system_arr, partition_arr)),
    error = function(e) {
      message("[fect.train.aq_panel_min_date] SQL failed (",
              outcome_var, "): ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res) || nrow(res) < 1L) return(NULL)
  d_chr = res$d[[1]]
  if (is.null(d_chr) || is.na(d_chr) || !nzchar(d_chr)) return(NULL)
  d = as.Date(d_chr)
  message("[fect.train.aq_panel_min_date] outcome=", outcome_var,
          " system_types=", paste(system_types, collapse = ","),
          " sustain_months=", sustain_months,
          " min_date=", format(d))
  d
}

# -----------------------------------------------------------------------------
# Read-once panel cache
# -----------------------------------------------------------------------------
# The trainer fits MANY specs (samples x outcomes) that all read
# public.model_panel_daily, and fetch_panel() queries it once per spec — the
# `all_priority` reads are ~the whole table, x ~11 outcomes = pure egress/IO
# waste. fect_prefetch_full_panel() reads the UNION of everything any spec needs
# (all metros x partition_keys x columns, widest window) ONCE into memory;
# fetch_panel() then subsets that frame. Best-effort: a missing cache falls back
# to per-spec DB queries, so model output is unchanged either way.
.fect_panel_cache = new.env(parent = emptyenv())

fect_clear_panel_cache = function() {
  if (exists("full", envir = .fect_panel_cache, inherits = FALSE)) {
    rm("full", envir = .fect_panel_cache)
  }
  invisible(NULL)
}

# Read the union [date_from, date_to] x metros x pks x cols directly from the DB.
.fect_read_full_from_db = function(db, all_pks, all_metros, date_from, date_to, all_cols) {
  db |>
    tbl(in_schema("public", "model_panel_daily")) |>
    filter(
      partition_key %in% !!all_pks,
      metro_id %in% !!all_metros,
      date >= !!date_from,
      date <= !!date_to
    ) |>
    select(dplyr::all_of(all_cols)) |>
    collect()
}

# Cold-blob assembly (CPPORTAL_FECT_PANEL_SOURCE=cold_blob): serve the stable cold
# history (date < cutoff) from the published cache parquet and read ONLY the hot
# tail (date >= cutoff) live from the DB, then union. Returns the assembled frame,
# or NULL on ANY problem (cache missing/misconfigured, schema-stale, or the cold
# history changed since publish) so the caller falls back to a full DB read.
# Correctness never depends on the cache; the hot tail is always live.
.fect_assemble_cold_blob = function(db, all_pks, all_metros, date_from, date_to, all_cols) {
  if (!exists("fect_cache_read_cold_meta", mode = "function") ||
      !exists("fect_cache_read_cold_parquet", mode = "function")) {
    message("[fect.train.coldblob] cache reader not loaded (panel_cache_io.R) — DB fallback")
    return(NULL)
  }
  meta = fect_cache_read_cold_meta()
  if (is.null(meta) || is.null(meta$cutoff_date)) {
    message("[fect.train.coldblob] no cold artifact / meta — DB fallback"); return(NULL)
  }
  cutoff = tryCatch(as.Date(meta$cutoff_date), error = function(e) NA)
  if (is.na(cutoff)) { message("[fect.train.coldblob] bad cutoff_date — DB fallback"); return(NULL) }
  # Schema guard: every column any spec needs must be in the artifact.
  missing_cols = setdiff(all_cols, meta$columns %||% character(0))
  if (length(missing_cols)) {
    message("[fect.train.coldblob] artifact missing cols (", paste(missing_cols, collapse = ","),
            ") — DB fallback"); return(NULL)
  }
  # Consistency guard: has the cold history (date < cutoff) changed since publish?
  # A cheap aggregate (no row egress); compare epoch-seconds of MAX(built_at) and
  # the row count. Any mismatch => a historical rebuild happened => DB fallback.
  chk = tryCatch(
    db |> tbl(in_schema("public", "model_panel_daily")) |>
      filter(date < !!cutoff) |>
      summarise(n = dplyr::n(),
                m = max(.data$built_at, na.rm = TRUE)) |>
      collect(),
    error = function(e) NULL)
  if (is.null(chk) || nrow(chk) != 1L) {
    message("[fect.train.coldblob] consistency probe failed — DB fallback"); return(NULL)
  }
  db_n = as.numeric(chk$n[[1]]); db_m = as.numeric(as.POSIXct(chk$m[[1]]))
  meta_n = as.numeric(meta$source_count %||% NA)
  meta_m = as.numeric(meta$source_max_built_epoch %||% NA)
  if (!isTRUE(db_n == meta_n) || is.na(db_m) || is.na(meta_m) || abs(db_m - meta_m) > 1) {
    message("[fect.train.coldblob] cold history changed since publish (db_n=", db_n,
            " meta_n=", meta_n, " dmax=", round(abs(db_m - meta_m), 1),
            "s) — DB fallback"); return(NULL)
  }
  # Cold from cache, hot from DB. hot_from = max(cutoff, date_from) so a spec whose
  # window starts after cutoff reads entirely from the DB (cold contributes 0).
  hot_from = max(cutoff, date_from)
  cold = fect_cache_read_cold_parquet(col_select = all_cols)
  if (is.null(cold)) { message("[fect.train.coldblob] parquet read failed — DB fallback"); return(NULL) }
  cold = cold |>
    dplyr::filter(.data$partition_key %in% all_pks,
                  .data$metro_id %in% as.integer(all_metros),
                  .data$date >= date_from, .data$date < cutoff)
  hot = if (hot_from <= date_to) {
    db |> tbl(in_schema("public", "model_panel_daily")) |>
      filter(partition_key %in% !!all_pks, metro_id %in% !!all_metros,
             date >= !!hot_from, date <= !!date_to) |>
      select(dplyr::all_of(all_cols)) |> collect()
  } else cold[0, , drop = FALSE]
  full = dplyr::bind_rows(cold, hot)
  message("[fect.train.coldblob] cutoff=", as.character(cutoff),
          " cold_rows=", nrow(cold), " (cache) hot_rows=", nrow(hot),
          " (db, >= ", as.character(hot_from), ") total=", nrow(full))
  full
}

fect_prefetch_full_panel = function(db, specs) {
  if (length(specs) == 0L) return(invisible(NULL))
  pk_of = function(s) s$partition_keys %||% (s$partition_key %||% paste0(s$pollutant, "-1HR"))
  eff_from = function(s) {
    if (!is.null(s$min_date) && !is.na(s$min_date)) {
      max(as.Date(s$min_date), as.Date(s$date_from))
    } else as.Date(s$date_from)
  }
  all_pks    = unique(unlist(lapply(specs, pk_of), use.names = FALSE))
  all_metros = sort(unique(as.integer(unlist(lapply(specs, function(s) s$metro_ids),
                                             use.names = FALSE))))
  date_from  = min(do.call(c, lapply(specs, eff_from)))
  date_to    = max(do.call(c, lapply(specs, function(s) as.Date(s$date_to))))
  # partition_key is filtered on but not in fect_panel_required_cols(); add it (and
  # aq_daily_mean for the traffic unit-set gate) so in-memory subsetting can filter.
  all_cols   = unique(c("partition_key", "aq_daily_mean",
                        unlist(lapply(specs, fect_spec_panel_cols), use.names = FALSE)))

  # Source: "db" (default) reads the whole union live; "cold_blob" serves the cold
  # history (< cutoff) from the published cache parquet and reads only the hot tail
  # (>= cutoff) live, cutting DB egress to the last ~3 months. cold_blob degrades to
  # a full DB read on any cache problem, so output is identical either way.
  source_mode = tolower(Sys.getenv("CPPORTAL_FECT_PANEL_SOURCE", unset = "db"))
  message("[fect.train.prefetch] reading model_panel_daily ONCE — source=", source_mode,
          " metros=", length(all_metros), " partition_keys=", paste(all_pks, collapse = ","),
          " cols=", length(all_cols), " window=", date_from, "..", date_to)
  full = NULL
  if (identical(source_mode, "cold_blob")) {
    full = .fect_assemble_cold_blob(db, all_pks, all_metros, date_from, date_to, all_cols)
  }
  if (is.null(full)) {
    full = .fect_read_full_from_db(db, all_pks, all_metros, date_from, date_to, all_cols)
  }
  .fect_panel_cache$full = full
  message("[fect.train.prefetch] cached rows=", nrow(full),
          " monitors=", dplyr::n_distinct(full$fullaqsid),
          " dates=", dplyr::n_distinct(full$date),
          " — fetch_panel() now subsets this in memory")
  invisible(full)
}

# -----------------------------------------------------------------------------
# Deterministic (fullaqsid, date) dedupe
# -----------------------------------------------------------------------------
# fect requires unique unit-time keys, so duplicate (fullaqsid, date) rows have
# to be collapsed. The SURVIVOR must be chosen by a stated rule — not by
# whatever order the rows happened to arrive in.
#
# The one real source of a genuine collision is `partition_key`. Under
# CPPORTAL_FECT_PARTITION_MODE="both" (what production runs) a daily-compatible
# AQ spec reads BOTH '<pollutant>-1HR' and '<pollutant>-24HR'. The block comment
# on fect_partition_mode() asserts that a monitor-day is only ever ONE
# partition — but that is an assumption about the upstream ETL, not an invariant
# enforced here. If it is ever violated, the old `arrange(metro_id, fullaqsid,
# date)` left the winner entirely to collection order, silently changing that
# monitor-day's outcome value from run to run.
#
# PREFERENCE, STATED EXPLICITLY: -1HR beats -24HR. The hourly partition is the
# richer source (its value is the mean of that day's hourly readings) and it is
# the partition FECT read exclusively before "both" mode existed, so preferring
# it makes a collision resolve to the historical value. This is a DELIBERATE
# rank — it is deliberately NOT left to the incidental fact that the string
# "-1HR" happens to sort before "-24HR".
fect_partition_pref_rank = function(pk) {
  pk = as.character(pk)
  ifelse(grepl("-1HR$", pk), 1L,
         ifelse(grepl("-24HR$", pk), 2L,
                3L))  # any future partition sorts last until ranked on purpose
}

#' Collapse a panel to one row per (fullaqsid, date), deterministically.
#'
#' @param panel Panel rows. When a `partition_key` column is present it drives
#'   the survivor choice via fect_partition_pref_rank(); when it is absent the
#'   ordering is unchanged from the historical behaviour (by then any partition
#'   collision has already been resolved — see the call site in fetch_panel()).
#' @param .tag,.what Message prefix, kept identical to the previous inline log
#'   lines so existing greps still match.
fect_dedupe_unit_time = function(panel, .tag = "fect.train",
                                 .what = "dedupe fullaqsid+date") {
  n_before = nrow(panel)
  if (n_before == 0L) return(panel)
  has_pk = "partition_key" %in% names(panel)
  panel = if (has_pk) {
    panel |>
      dplyr::mutate(.pk_rank = fect_partition_pref_rank(.data$partition_key)) |>
      dplyr::arrange(.data$metro_id, .data$fullaqsid, .data$date,
                     .data$.pk_rank, .data$partition_key) |>
      dplyr::distinct(.data$fullaqsid, .data$date, .keep_all = TRUE) |>
      dplyr::select(-".pk_rank")
  } else {
    panel |>
      dplyr::arrange(.data$metro_id, .data$fullaqsid, .data$date) |>
      dplyr::distinct(.data$fullaqsid, .data$date, .keep_all = TRUE)
  }
  n_drop = n_before - nrow(panel)
  if (n_drop > 0L) {
    message(
      "[", .tag, "] ", .what, " dropped_rows=", n_drop,
      " (fect requires unique unit-time keys",
      if (has_pk) "; survivor by explicit partition preference -1HR > -24HR"
      else "", ")"
    )
  }
  panel
}

# -----------------------------------------------------------------------------
# Panel fetch (spec-driven column selection)
# -----------------------------------------------------------------------------

fetch_panel = function(db, spec) {
  pks = spec$partition_keys %||% (spec$partition_key %||% paste0(spec$pollutant, "-1HR"))

  effective_date_from = if (!is.null(spec$min_date) && !is.na(spec$min_date)) {
    max(as.Date(spec$min_date), as.Date(spec$date_from))
  } else {
    as.Date(spec$date_from)
  }

  message("[fect.train.fetch_panel] spec=", spec$id,
          " outcome=", spec$outcome_var,
          " kind=", spec$outcome_kind,
          " partition_key=", paste(pks, collapse = ","),
          " n_metros=", length(spec$metro_ids),
          " date_range=", effective_date_from, "..", spec$date_to,
          if (!is.null(spec$min_date) && !is.na(spec$min_date)) {
            paste0(" (min_date=", format(spec$min_date), ")")
          } else "")

  cols_needed = fect_spec_panel_cols(spec)
  # Traffic specs gate the unit set on AQ activity, even though the outcome
  # itself may be NA on most rows. Make sure aq_daily_mean is fetched so we can
  # filter on it after collect().
  if (identical(spec$outcome_kind, "traffic")) {
    cols_needed = unique(c(cols_needed, "aq_daily_mean"))
  }
  # partition_key is filtered on but is NOT part of fect_spec_panel_cols(), so
  # it used to be dropped by the select() below — leaving the (fullaqsid, date)
  # dedupe with no way to prefer -1HR over -24HR. Fetch it, dedupe with it, then
  # drop it immediately, so nothing downstream of fetch_panel() sees a new
  # column.
  cols_needed = unique(c(cols_needed, "partition_key"))

  # Read-once: if the full panel was prefetched (fect_prefetch_full_panel), subset
  # it IN MEMORY — same filters + same columns as the DB query, so the result is
  # identical, but the whole run hits model_panel_daily once instead of per spec.
  # If the cache is absent (prefetch disabled/failed), query the DB per spec
  # (original behaviour) — correctness never depends on the cache.
  full_cache = .fect_panel_cache$full
  panel = if (!is.null(full_cache)) {
    full_cache |>
      dplyr::filter(
        .data$partition_key %in% pks,
        .data$metro_id %in% as.integer(spec$metro_ids),
        .data$date >= effective_date_from,
        .data$date <= as.Date(spec$date_to)
      ) |>
      dplyr::select(dplyr::all_of(cols_needed))
  } else {
    db |>
      tbl(in_schema("public", "model_panel_daily")) |>
      filter(
        partition_key %in% !!pks,
        metro_id %in% !!as.integer(spec$metro_ids),
        date >= !!effective_date_from,
        date <= !!spec$date_to
      ) |>
      select(dplyr::all_of(cols_needed)) |>
      collect()
  }

  # Outcome / unit-set filter:
  #  - AQ specs: drop rows where the AQ outcome is NA (dense; fect drops anyway).
  #  - Traffic specs: KEEP rows where outcome is NA so the panel reflects the
  #    user's "every AQ monitor is eligible from min_date" design. Restrict to
  #    AQ-active rows (aq_daily_mean IS NOT NULL) so the unit set = AQ monitors.
  n_pre = nrow(panel)
  if (identical(spec$outcome_kind, "aq")) {
    panel = panel |>
      dplyr::filter(!is.na(.data[[spec$outcome_var]]))
    n_drop = n_pre - nrow(panel)
    if (n_drop > 0L) {
      message(
        "[fect.train.fetch_panel] dropped_rows_outcome_na=", n_drop,
        " (outcome=", spec$outcome_var, ")"
      )
    }
  } else {
    panel = panel |>
      dplyr::filter(!is.na(.data$aq_daily_mean))
    n_drop = n_pre - nrow(panel)
    if (n_drop > 0L) {
      message(
        "[fect.train.fetch_panel] dropped_rows_aq_active=", n_drop,
        " (traffic spec gates unit set on aq_daily_mean IS NOT NULL)"
      )
    }
    n_outcome_obs = sum(!is.na(panel[[spec$outcome_var]]))
    n_outcome_na  = nrow(panel) - n_outcome_obs
    message(
      "[fect.train.fetch_panel] traffic_outcome_obs=", n_outcome_obs,
      " traffic_outcome_na=", n_outcome_na,
      " (outcome=", spec$outcome_var, ")"
    )
  }

  # Deterministic: partition_key is still on the frame here, so a -1HR/-24HR
  # collision resolves by the explicit preference rank, not by row order.
  panel = fect_dedupe_unit_time(panel, .tag = "fect.train.fetch_panel",
                                .what = "dedupe fullaqsid+date")
  # Drop the helper column — fetch_panel()'s contract is unchanged.
  panel = panel |> dplyr::select(-dplyr::any_of("partition_key"))

  message("[fect.train.fetch_panel] spec=", spec$id,
          " rows=", nrow(panel),
          " monitors=", dplyr::n_distinct(panel$fullaqsid),
          " dates=", dplyr::n_distinct(panel$date))

  panel
}

# -----------------------------------------------------------------------------
# Panel summary used by both fitted and placeholder bundles
# -----------------------------------------------------------------------------

fect_panel_summary = function(panel, spec, n_treated, n_control,
                              n_treated_obs, n_zone_pairs) {
  list(
    n_rows         = nrow(panel),
    n_monitors     = dplyr::n_distinct(panel$fullaqsid),
    n_metros       = dplyr::n_distinct(panel$metro_id),
    n_dates        = dplyr::n_distinct(panel$date),
    n_treated      = n_treated,
    n_control      = n_control,
    n_treated_obs  = n_treated_obs,
    n_zone_pairs   = n_zone_pairs,
    n_treated_units = dplyr::n_distinct(
      panel$fullaqsid[panel$treated %in% TRUE]
    ),
    date_min       = if (nrow(panel) > 0L) min(panel$date, na.rm = TRUE) else NA,
    date_max       = if (nrow(panel) > 0L) max(panel$date, na.rm = TRUE) else NA
  )
}

# -----------------------------------------------------------------------------
# Placeholder bundle — written when n_treated_obs is below the spec's threshold.
# Apps can detect this via bundle$status == "pending_observation" or pin
# metadata `status` field. Re-running the job once TomTom (or any upstream ETL)
# backfills observations will replace the placeholder with a fitted bundle.
# -----------------------------------------------------------------------------

make_placeholder_bundle = function(spec, panel, n_treated, n_control,
                                   n_treated_obs, n_zone_pairs, reason) {
  list(
    spec          = spec,
    status        = "pending_observation",
    status_reason = reason,
    trained_at    = Sys.time(),
    panel_stats   = fect_panel_summary(panel, spec, n_treated, n_control,
                                       n_treated_obs, n_zone_pairs),
    diagnostics   = list(gof = list(), fect_tests = list()),
    gof_tbl       = NULL,
    tests_tbl     = NULL,
    model         = NULL,
    att           = tibble::tibble(),
    model_family  = "fect",
    ui_metadata   = list(
      model_family   = "fect",
      model_type     = "synth",
      group_label    = spec$label %||% spec$id,
      model_label    = spec$name %||% spec$id,
      description    = paste0(
        spec$description %||% "",
        " [PLACEHOLDER: ", reason, "]"
      ),
      outcome_var    = spec$outcome_var,
      outcome_kind   = spec$outcome_kind,
      fect_method    = spec$fect_method,
      system_types_csv = paste(spec$system_types, collapse = ","),
      control_metro_ids = sort(unique(as.integer(spec$metro_ids))),
      n_metros       = dplyr::n_distinct(panel$metro_id),
      n_monitors     = dplyr::n_distinct(panel$fullaqsid),
      status         = "pending_observation"
    )
  )
}

# -----------------------------------------------------------------------------
# Covariate-only collinearity diagnostic (called from train_fect_bundle).
# Returns a tibble with covariate, r2, vif, verdict — same metric set as
# job/model/fect/diagnostics/check_collinearity.R, just operating on the
# in-memory panel that has already been fetched and treatment-flagged.
# NULL if fixest unavailable or the panel is too thin for a sensible fit.
#
# VIF SCALE NOTE: every production covariate at this writing is continuous,
# so Df = 1 for every predictor and the reported `vif = 1/(1 - R^2)` already
# equals both `car::vif()`'s GVIF column and the squared `GVIF^(1/(2*Df))`
# transformation that's needed to compare categorical predictors on the
# VIF scale. If a categorical/factor covariate is ever added to the
# production set, this helper needs to switch to fitting an lm() on the
# demeaned design matrix and calling `car::vif(fit)` so it returns a proper
# GVIF table — then square the third column ("GVIF^(1/(2*Df))") before
# stamping it into the pin metadata as `covariate_vif_max`.
# -----------------------------------------------------------------------------
compute_panel_vif = function(panel, covariates,
                             unit_col = "fullaqsid", date_col = "date") {
  if (!requireNamespace("fixest", quietly = TRUE)) {
    message("[fect.train.vif] fixest not available; skipping VIF")
    return(NULL)
  }
  covariates = intersect(covariates, names(panel))
  if (length(covariates) < 2L) return(NULL)
  if (!all(c(unit_col, date_col) %in% names(panel))) return(NULL)

  rows_ok = stats::complete.cases(panel[, covariates, drop = FALSE])
  if (sum(rows_ok) < 5L * length(covariates)) {
    message("[fect.train.vif] too few complete-case rows; skipping VIF")
    return(NULL)
  }

  t_start = Sys.time()
  X   = as.matrix(panel[rows_ok, covariates, drop = FALSE])
  fes = as.data.frame(panel[rows_ok, c(unit_col, date_col), drop = FALSE])

  X_demeaned = tryCatch(
    fixest::demean(X, f = fes),
    error = function(e) {
      message("[fect.train.vif] fixest::demean failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(X_demeaned)) return(NULL)
  colnames(X_demeaned) = covariates

  out = lapply(seq_along(covariates), function(i) {
    yvec = X_demeaned[, i]
    yc   = yvec - mean(yvec)
    yss  = sum(yc * yc)
    if (yss <= 1e-12) {
      return(tibble::tibble(
        covariate = covariates[[i]],
        r2 = NA_real_, vif = NA_real_, absorbed_by_fe = TRUE
      ))
    }
    X_other = X_demeaned[, -i, drop = FALSE]
    fit = stats::lm.fit(cbind(1, X_other), yvec)
    r2 = 1 - sum(fit$residuals * fit$residuals) / yss
    vif = 1 / (1 - r2)
    tibble::tibble(
      covariate = covariates[[i]],
      r2 = r2, vif = vif, absorbed_by_fe = FALSE
    )
  })
  vif_df = dplyr::bind_rows(out) |>
    dplyr::arrange(dplyr::desc(.data$vif))
  vif_df$verdict = dplyr::case_when(
    isTRUE(vif_df$absorbed_by_fe) ~ "absorbed by unit FE (Z covariate)",
    vif_df$vif < 2.5 ~ "gold standard (< 2.5)",
    vif_df$vif < 5   ~ "acceptable (< 5)",
    vif_df$vif < 10  ~ "tolerable (< 10)",
    TRUE             ~ "BAD (>= 10)"
  )
  vif_df$elapsed_sec = round(
    as.numeric(difftime(Sys.time(), t_start, units = "secs")), 3)
  vif_df
}

# -----------------------------------------------------------------------------
# Plan B (M2) — intercept-corrected post-hoc ATT imputation for fect-dropped units
#
# fect requires every treated unit to have at least min.T0 untreated periods
# (default 5 for CFE/MC — MC does NOT relax this). Zone monitors in cities
# whose AQ panel coverage starts AFTER the cordon (Stockholm, Milan,
# Singapore; parts of London) have zero untreated rows in model_panel_daily
# and are dropped by fect before fitting. They never appear in fect's att.
#
# The original (naive, "M0") imputation set Y(0)_it = M_t, the contemporaneous
# same-metro out-of-zone control mean. That omits the monitor's own intercept:
# in-zone monitors sit in the urban core / kerbside and run systematically
# higher at baseline (+0.20 sqrt-units measured at NYC pre-policy), so the
# siting premium leaked into the "ATT" as a spurious POSITIVE effect of
# congestion pricing.
#
# This version implements the validated correction ("M2" in
# diagnostics/validate_imputation.R; appendix.md §A.6). The two-way model
# implies, for dropped monitor i in metro X on day t:
#
#   Y_i(0)_t = M_t + (alpha_i - mean alpha_ctrl,t) + (X_it - Xbar_ctrl,t)' beta
#
# where:
#   * beta-hat and alpha-hat_j come from a companion fixest::feols two-way FE
#     fit on the panel's UNTREATED rows — the same estimand as fect-CFE's
#     within fit (companion beta matches fect's beta to 4 decimals);
#   * alpha_i for a dropped monitor is unidentified (no untreated rows), so it
#     is predicted from a hedonic model of alpha-hat_j on time-invariant
#     monitor observables (road distance Z, monitor-mean bg3, log population)
#     plus metro intercepts, trained on OUT-OF-ZONE monitors only — the model
#     never learns from treated zones, which is exactly the extrapolation
#     production faces;
#   * mean alpha_ctrl,t is composition-matched: averaged over the same control
#     monitors that contribute to M_t that day.
#
# Validation (NYC pre-treatment placebo, truth = 0): naive M0 bias = +0.328;
# M2 bias = +0.018 (94% reduction). See appendix.md §A.6 for the full tables.
#
# ATT_it = y_it - Y(0)_it on the model (sqrt) scale, aggregated to
# (metro_id, date) and appended to bundle$att with type =
# "per_day_metro_imputed" so app/paper consumers can filter on that label.
# These remain cross-sectional long-run contrasts, NOT event-study ATTs:
# flag them in the paper.
#
# se_att (approximate, documented): per monitor-day,
#   se^2 = sigma2_cm / n_ctrl + Var(alpha_pred)
# where sigma2_cm is the companion-fit residual variance (drives the error of
# M_t as an estimate of the metro-day mean) and Var(alpha_pred) is the hedonic
# prediction-interval variance (se.fit^2 + sigma_hedonic^2). Aggregation to
# (metro, date) takes the mean of monitor-level SEs — conservative, because
# the alpha_pred and M_t errors are shared across monitors, not independent.
#
# Graceful degradation: if the companion fit fails -> M0 with a warning; if
# the hedonic model fails or a unit's metro is absent from hedonic training ->
# M1 (covariate adjustment only) for the affected rows. Per-row `imp_method`
# records which formula produced each cell.
#
# Returns a tibble matching get_qis_fect()'s column layout where possible,
# with attr(out, "imputation") = list(method, alpha_model_r2, ...) for the
# bundle diagnostics. NULL when there is nothing to impute.
# -----------------------------------------------------------------------------
#' @param sat_anchors Optional tibble (fullaqsid, sat_longrun) from
#'   fetch_sat_anchors() — long-run ACAG satellite PM at each monitor
#'   (source 15). When supplied, the hedonic alpha model gains the satellite
#'   anchor as a predictor ("M4a", validated in appendix A.8); when NULL the
#'   hedonic is the satellite-free M2. Toggled in train_fect_bundle() via
#'   CPPORTAL_FECT_SAT_ANCHOR=1 (default off per Tim 2026-06-11).
# -----------------------------------------------------------------------------
# Siting-class harmonisation map (gap-model covariate; gap upgrade R1-c,
# accepted by Tim 2026-07-27). Promoted from
# job/model/fect/gap_upgrade/ref_monitor_siting.csv: the operator-recorded
# monitors.monitor_type harmonised to 7 classes (kerbside / traffic /
# background / suburban / industrial / regional / unknown). 'unknown' is an
# ADMINISTRATIVE label, not missing data — US EPA AQS / NYCCAS / Singapore NEA
# / Taiwan MOENV networks do not encode EEA-style station classification, so
# their monitors are a pooled real level: never imputed, never dropped, and
# they behave exactly as they did before this covariate existed.
# Ships in the Connect bundle as a train-dir sibling (siting_class_map.csv).
# -----------------------------------------------------------------------------
fect_load_siting_map = function() {
  candidates = c(
    "siting_class_map.csv",                      # Connect: cwd = bundle root (train/)
    "job/model/fect/train/siting_class_map.csv"  # local: cwd = repo root
  )
  for (p in candidates) {
    if (file.exists(p)) {
      map = tryCatch(
        utils::read.csv(p, colClasses = "character"),
        error = function(e) NULL
      )
      if (!is.null(map) && all(c("fullaqsid", "siting_class") %in% names(map))) {
        return(map[, c("fullaqsid", "siting_class")])
      }
    }
  }
  message("[fect.train.impute] siting_class_map.csv not found; ",
          "all monitors take siting_class='unknown' (term will drop as degenerate)")
  NULL
}

# M9-shape (DESIGN §33b) — the shape-constrained gap model lives in a sibling
# file so all three gap_rhs call sites can source ONE spec (the §33b lockstep
# requirement). Default OFF: without CPPORTAL_FECT_M9_SHAPE=1 the loader is a
# no-op for behaviour — cpportal_gap_fit() just calls stats::lm().
fect_source_gap_shape = function() {
  if (exists("cpportal_gap_fit", mode = "function")) return(invisible(TRUE))
  candidates = c(
    "gap_shape.R",                          # Connect: cwd = bundle root (train/)
    "job/model/fect/train/gap_shape.R",     # local: cwd = repo root
    "../train/gap_shape.R"                  # diagnostics/ callers
  )
  for (p in candidates) {
    if (file.exists(p)) {
      source(p, local = FALSE)
      return(invisible(TRUE))
    }
  }
  # Degrade to the shipped linear estimator rather than erroring: if the
  # sibling is missing (older bundle), the pipeline behaves exactly as today.
  assign("cpportal_gap_fit",
         function(formula, data, spec = NULL, shape = NULL, where = "headline")
           stats::lm(formula, data = data),
         envir = globalenv())
  assign("cpportal_gap_r2", function(fit) summary(fit)$r.squared, envir = globalenv())
  assign("cpportal_gap_sigma", function(fit) summary(fit)$sigma, envir = globalenv())
  message("[fect.train.impute] gap_shape.R not found; gap model stays linear (M9-shape unavailable)")
  invisible(FALSE)
}

#' Is the monitor-MONTH gap refinement (R4) in play?
#'
#' F4 RESOLUTION AMENDED (Tim's standing directive, 2026-07-31, DESIGN §33b).
#' The earlier call DISABLED the refinement on the M9 shape arm, on the reasoning
#' that an UNCONSTRAINED monthly `lm()` would overwrite the constrained h(d)
#' prediction at every monitor-month it covers. Tim overruled the remedy, not the
#' diagnosis: "monitor month is the more correct route ... use appropriate
#' controls for the heteroskedasticity but don't throw away large sample size for
#' no reason. This is a time series cross-sectional problem - don't throw away
#' the time series."
#'
#' So the refinement is CONSTRAINED rather than removed. `gap_hed_mm` is now fit
#' by `cpportal_gap_fit()` with the same monotone I-spline basis and the same C1
#' cone on `dist_road`; its monthly terms stay in the free parametric block; and
#' the leverage/heteroskedasticity controls are weights (n_days_mm for cell
#' reliability, 1/m_i for cross-monitor balance on the static block). Both grains
#' obey C1, so the coalesce is shape-consistent and the gate reads the lever it
#' claims to test on every row.
#'
#' DIAGNOSTIC SWITCH (kept deliberately): CPPORTAL_FECT_M9_MONTHLY_OFF=1 restores
#' the refinement-off behaviour, so the gate can run the monitor-level-only arm
#' as an explicit comparison. It is OFF by default on every arm.
#'
#' Production / M7 (shape flag off) is unchanged: this returns TRUE and every
#' coalesce site behaves byte-identically to today.
fect_m9_monthly_refinement_off = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_FECT_M9_MONTHLY_OFF", "")))
  v %in% c("1", "true", "yes", "on")
}

fect_m9_monthly_refinement_enabled = function() {
  !fect_m9_monthly_refinement_off()
}

# T2 — distance from the monitor to its metro's urban congestion core (km),
# from the R4 gap campaign (TomTom hex capture, contiguity-grown cores).
# Static per monitor; used only by the monitor-MONTH gap refinement below.
# Ships in the Connect bundle as a train-dir sibling (t2_core_dist.csv).
fect_load_t2_map = function() {
  candidates = c(
    "t2_core_dist.csv",                      # Connect: cwd = bundle root (train/)
    "job/model/fect/train/t2_core_dist.csv"  # local: cwd = repo root
  )
  for (p in candidates) {
    if (file.exists(p)) {
      map = tryCatch(utils::read.csv(p), error = function(e) NULL)
      if (!is.null(map) &&
          all(c("fullaqsid", "metro_id", "t2_core_dist_km") %in% names(map))) {
        map$fullaqsid = as.character(map$fullaqsid)
        return(map)
      }
    }
  }
  message("[fect.train.impute] t2_core_dist.csv not found; ",
          "monthly gap model runs without the T2 term")
  NULL
}

impute_dropped_zone_atts = function(model_fit, panel, zone_pairs, spec,
                                    sat_anchors = NULL, zone_monitor_ids = NULL) {
  if (is.null(model_fit) || is.null(panel) || nrow(panel) == 0L) return(NULL)
  if (is.null(zone_pairs) || nrow(zone_pairs) == 0L) return(NULL)
  if (!"treated" %in% names(panel)) return(NULL)

  # Scope drives the control set for M_t. ZONE: same-metro out-of-zone untreated
  # monitors (keyed metro x date). METRO: every in-metro monitor is treated, so
  # the controls are the UNTREATED metros — pool them by date and let the hedonic
  # alpha-model carry each dropped monitor's level (alpha_offset).
  scope = spec$treatment_scope %||% "zone"
  metro_scope = identical(scope, "metro")
  ctrl_keys = if (metro_scope) "date" else c("metro_id", "date")

  # M6 (Tim 2026-07-01): anchor the ZONE-branch counterfactual LEVEL directly
  # on sat_monthly_mean_i,month(t) rather than on M_t + hedonic-α. Falls back
  # to M4c per-row where sat_monthly_mean is NA. Toggle:
  #
  #   CPPORTAL_FECT_IMPUTE_MODE = M6    -> sat-anchored level (DEFAULT)
  #   CPPORTAL_FECT_IMPUTE_MODE = M4c   -> control-mean + hedonic α (legacy)
  #
  # Metro-scope always uses M5 (untouched).
  # Default 2026-07-01 evening: M7 (sat-anchored + hedonic gap correction).
  # M4c: no satellite anchor (out-of-zone hedonic α extrapolation for level).
  # M6:  pure satellite anchor; suffers from per-city sat-vs-monitor gap
  #      artifacts (Milan +6.73 → -9.10 ATT artifact).
  # M7:  satellite anchor + hedonic prediction of the observed
  #      sqrt(pm25) - sqrt(sat) gap from monitor observables. Corrects the
  #      per-city calibration bias without leaning on out-of-zone level
  #      extrapolation. Falls back per-row to M4c when gap prediction fails
  #      (e.g. dropped monitor's metro not in hedonic training set).
  impute_mode = toupper(Sys.getenv("CPPORTAL_FECT_IMPUTE_MODE", unset = "M7"))
  if (!impute_mode %in% c("M4C", "M6", "M7")) {
    message("[fect.train.impute] unknown CPPORTAL_FECT_IMPUTE_MODE='", impute_mode,
            "'; defaulting to M7")
    impute_mode = "M7"
  }

  fitted_units = as.character(model_fit$id)
  zone_units   = unique(as.character(zone_pairs$fullaqsid))
  dropped_units = setdiff(zone_units, fitted_units)

  # Gap-model exclusion set (Option A, 2026-07-02): the sat-vs-monitor gap is a
  # SITING property, independent of treatment scope, so it must always be
  # estimated from out-of-CORDON-ZONE monitors — which exist in every city,
  # including the anchored ones (London/Stockholm/Milan/Singapore). In ZONE
  # scope `zone_units` already IS the in-zone set, so gap_exclude_ids == zone_units
  # (no change). In METRO scope `zone_units` is the whole metro (every monitor is
  # treated), which wrongly drops the anchored cities entirely from the gap model
  # and loses their per-city gap FE — the Milan blow-up (yhat0 → 369 µg/m³). The
  # caller passes the true in-zone set as `zone_monitor_ids` so metro can exclude
  # ONLY in-zone monitors and keep each city's out-of-zone monitors for the FE.
  gap_exclude_ids = if (!is.null(zone_monitor_ids)) {
    as.character(zone_monitor_ids)
  } else {
    zone_units
  }
  if (length(dropped_units) == 0L) {
    message("[fect.train.impute] no dropped zone monitors; skipping imputation")
    return(NULL)
  }
  message("[fect.train.impute] imputing ATTs for ", length(dropped_units),
          " dropped zone monitor(s): ",
          paste(utils::head(dropped_units, 5L), collapse = ", "),
          if (length(dropped_units) > 5L) ", ..." else "")
  ilap = fect_stage_timer(paste0(spec$id, "/impute"))

  # Response on the model scale (fect_outcome_formula returns ~ sqrt(<aq_*>)).
  lhs_expr = if (!is.null(spec$outcome)) spec$outcome[[2L]] else NULL
  if (is.null(lhs_expr)) return(NULL)
  panel$.y_model = as.numeric(eval(lhs_expr, envir = panel, enclos = baseenv()))

  x_vars = if (is.null(spec$covariates)) character(0) else
    intersect(all.vars(spec$covariates), names(panel))
  z_var = if (length(spec$z_covariates %||% character(0)) > 0L) {
    intersect(as.character(spec$z_covariates)[[1L]], names(panel))
  } else {
    character(0)
  }

  # --- 1. Companion two-way FE fit on untreated rows (beta-hat, alpha-hat) ---
  cm = NULL
  beta_hat = NULL
  alpha_tbl = NULL
  sigma2_cm = NA_real_
  if (requireNamespace("fixest", quietly = TRUE) && length(x_vars) > 0L) {
    unt = panel |>
      dplyr::filter(!(.data$treated %in% TRUE), is.finite(.data$.y_model))
    fml = stats::as.formula(paste0(
      ".y_model ~ ", paste(x_vars, collapse = " + "), " | fullaqsid + date"
    ))
    cm = tryCatch(fixest::feols(fml, data = unt, notes = FALSE),
                  error = function(e) {
                    message("[fect.train.impute] companion feols failed: ",
                            conditionMessage(e))
                    NULL
                  })
  }
  if (!is.null(cm)) {
    beta_hat  = stats::coef(cm)
    sigma2_cm = mean(stats::residuals(cm)^2, na.rm = TRUE)
    alpha_fe  = tryCatch(fixest::fixef(cm)$fullaqsid, error = function(e) NULL)
    if (!is.null(alpha_fe)) {
      alpha_tbl = tibble::tibble(
        fullaqsid = names(alpha_fe),
        alpha     = as.numeric(alpha_fe)
      )
    }
  }
  if (is.null(cm) || is.null(alpha_tbl)) {
    message("[fect.train.impute] WARNING: companion fit unavailable; ",
            "falling back to naive control-mean imputation (M0)")
  }
  ilap("companion_feols")

  # --- 2. Hedonic alpha model on out-of-zone monitors -------------------------
  hed = NULL
  alpha_model_r2 = NA_real_
  alpha_pred_tbl = NULL
  use_sat = FALSE   # flipped inside the block when sat_anchors are usable
  if (!is.null(alpha_tbl)) {
    mon_obs = panel |>
      dplyr::group_by(.data$fullaqsid, .data$metro_id) |>
      dplyr::summarise(
        dist_road = if (length(z_var) == 1L) {
          dplyr::first(stats::na.omit(.data[[z_var]]))
        } else {
          NA_real_
        },
        bg3_mean = if ("bg3" %in% names(panel)) {
          mean(.data$bg3, na.rm = TRUE)
        } else {
          NA_real_
        },
        pop_mean = if ("population" %in% names(panel)) {
          mean(.data$population, na.rm = TRUE)
        } else {
          NA_real_
        },
        .groups = "drop"
      )
    # Siting class (gap-model covariate; gap upgrade R1-c): harmonised operator
    # label joined from the checked-in map. Unlabeled monitors keep the REAL
    # 'unknown' level (coalesce, not imputation). Reference level 'background'
    # mirrors the gap_upgrade harness so coefficients read as premiums vs an
    # urban-background station; guarded for the degenerate no-background case.
    siting_map = fect_load_siting_map()
    if (!is.null(siting_map)) {
      mon_obs = mon_obs |> dplyr::left_join(siting_map, by = "fullaqsid")
    } else {
      mon_obs$siting_class = NA_character_
    }
    mon_obs = mon_obs |>
      dplyr::mutate(siting_class = {
        f = factor(dplyr::coalesce(.data$siting_class, "unknown"))
        if ("background" %in% levels(f)) stats::relevel(f, ref = "background") else f
      })
    use_sat = !is.null(sat_anchors) && nrow(sat_anchors) > 0L
    if (use_sat) {
      mon_obs = mon_obs |>
        dplyr::left_join(sat_anchors, by = "fullaqsid")
    }
    hed_train = alpha_tbl |>
      dplyr::inner_join(mon_obs, by = "fullaqsid") |>
      dplyr::filter(
        !(.data$fullaqsid %in% zone_units),
        is.finite(.data$alpha)
      ) |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::any_of(c("dist_road", "bg3_mean", "pop_mean")),
          is.finite
        )
      )
    # sat_longrun REMOVED from the hedonic 2026-06-30 (Tim). The 1998-2024
    # mean averaged treated-era months into the "baseline" and pulled
    # counterfactuals too low → spurious positive ATTs. `sat_monthly_mean`
    # now enters the FECT fit as a time-varying covariate
    # (fect_aq_default_covariates), which is the right way to use ACAG
    # data. `use_sat` is kept as a signal that sat_monthly is active in
    # the model, but it no longer gates the hedonic RHS.
    # Metro scope: the dropped units' metros are never in the training set
    # (controls are the untreated metros), so a metro fixed effect can't be
    # extrapolated to them — drop it and predict the baseline purely from
    # monitor observables (bg3 / dist-road / population). Zone scope keeps
    # the metro FE (dropped in-zone units share metros with their controls).
    hed_rhs = c(
      if (length(z_var) == 1L) "dist_road",
      if ("bg3" %in% names(panel)) "bg3_mean",
      if ("population" %in% names(panel)) "log1p(pop_mean)",
      if (!metro_scope) "factor(metro_id)"
    )
    if (nrow(hed_train) >= 30L) {
      hed = tryCatch(
        stats::lm(
          stats::as.formula(paste0("alpha ~ ", paste(hed_rhs, collapse = " + "))),
          data = hed_train
        ),
        error = function(e) {
          message("[fect.train.impute] hedonic alpha model failed: ",
                  conditionMessage(e))
          NULL
        }
      )
    } else {
      message("[fect.train.impute] hedonic training set too small (",
              nrow(hed_train), " < 30); skipping alpha offset (M1)")
    }
    if (!is.null(hed)) {
      alpha_model_r2 = summary(hed)$r.squared
      sigma_hed = summary(hed)$sigma
      known_metros = unique(as.character(hed_train$metro_id))
      # Predict alpha (+ prediction-interval SE) for an arbitrary unit set.
      # Used for the dropped units AND for the fitted-zone calibration
      # testbed below — both are zone units, so both are genuinely
      # out-of-sample for `hed` (which trains on out-of-zone monitors only).
      predict_alpha_units = function(units) {
        newd = mon_obs |> dplyr::filter(.data$fullaqsid %in% units)
        # Zone scope keeps the metro FE, so only predict for trained metros.
        # Metro scope has no metro FE (observables only) -> predict any monitor.
        if (!metro_scope) {
          newd = newd |> dplyr::filter(as.character(.data$metro_id) %in% known_metros)
        }
        if (nrow(newd) == 0L) return(NULL)
        pr = tryCatch(
          stats::predict(hed, newdata = newd, se.fit = TRUE),
          error = function(e) NULL
        )
        if (is.null(pr)) return(NULL)
        tibble::tibble(
          fullaqsid     = newd$fullaqsid,
          alpha_pred    = as.numeric(pr$fit),
          alpha_pred_se = sqrt(as.numeric(pr$se.fit)^2 + sigma_hed^2)
        )
      }
      alpha_pred_tbl = predict_alpha_units(dropped_units)
      message(sprintf(
        "[fect.train.impute] hedonic alpha model: n=%d R2=%.3f predicted=%d/%d dropped units",
        nrow(hed_train), alpha_model_r2,
        if (is.null(alpha_pred_tbl)) 0L else nrow(alpha_pred_tbl),
        length(dropped_units)
      ))
    }
  }
  if (is.null(hed)) predict_alpha_units = function(units) NULL
  ilap("hedonic_alpha")

  # --- 2b. Hedonic GAP model for M7 (Tim 2026-07-01) --------------------------
  # Purpose: correct the systematic sat-vs-monitor calibration gap when we
  # anchor the counterfactual on satellite. For each monitor j with untreated
  # observations we can measure gap_j = mean(sqrt(pm25_jt) - sqrt(sat_jt)).
  # Kerbside monitors read *dirtier* than their 1 km² satellite tile average;
  # rural/regional monitors read *cleaner*. Cross-city gap on the sqrt scale is
  # a physical property of the sensor's siting + the sat product's spatial
  # smoothing, NOT of the treatment. We can predict it from the SAME monitor
  # observables the α hedonic already uses (dist_road, bg3_mean, log(pop),
  # metro_id) trained on out-of-zone (untreated) monitors from every metro
  # (crucially, including the anchored cities' own out-of-zone monitors,
  # which fect kept as controls).
  #
  # M7 counterfactual: sqrt(yhat0_it) = sqrt(sat_it) + gap_pred_i + cov_adj
  # Shared SE:         gap_pred_se_i (same role alpha_pred_se played in M4c)
  gap_hed = NULL
  gap_pred_tbl = NULL
  gap_pred_mm_tbl = NULL
  gap_model_r2 = NA_real_
  if ("sat_monthly_mean" %in% names(panel) && !is.null(alpha_tbl)) {
    # Per-monitor mean sqrt-scale gap over OUT-OF-ZONE monitors (Option A). The
    # cordon does not affect a monitor outside the zone, so its √obs − √sat is a
    # clean siting measurement on ALL its days — we no longer restrict to the
    # scope's `treated` flag (which, in metro scope, marks every in-metro monitor
    # treated and would drop the anchored cities' out-of-zone monitors, killing
    # their FE). Excluding only in-zone monitors keeps each city's out-of-zone
    # monitors in the training set for both scopes.
    gap_by_mon = panel |>
      dplyr::filter(!(.data$fullaqsid %in% gap_exclude_ids),
                    is.finite(.data$.y_model),
                    is.finite(.data$sat_monthly_mean),
                    .data$sat_monthly_mean > 0) |>
      dplyr::mutate(.sat_sqrt = sqrt(pmax(.data$sat_monthly_mean, 0))) |>
      dplyr::group_by(.data$fullaqsid) |>
      dplyr::summarise(gap = mean(.data$.y_model - .data$.sat_sqrt, na.rm = TRUE),
                       gap_n = dplyr::n(), .groups = "drop")

    gap_train = gap_by_mon |>
      dplyr::inner_join(mon_obs, by = "fullaqsid") |>
      dplyr::filter(
        !(.data$fullaqsid %in% gap_exclude_ids),   # out-of-zone monitors only
        is.finite(.data$gap),
        .data$gap_n >= 30L                         # enough obs to trust the mean
      ) |>
      dplyr::filter(
        dplyr::if_all(
          dplyr::any_of(c("dist_road", "bg3_mean", "pop_mean")),
          is.finite
        )
      )

    # Metro FE is now included for BOTH scopes (Option A): with the anchored
    # cities' out-of-zone monitors in `gap_train`, their per-city gap offset
    # (Milan's −0.83 √-units of Po-Valley satellite-vs-monitor gap) is estimable
    # and gets subtracted from the satellite anchor exactly as in zone scope.
    gap_rhs = c(
      # Rung 2 (DESIGN §25) TESTED log(dist_road + 0.01) and REJECTED it:
      # with the siting-class FE in place (rung 1a) the curvature buys
      # nothing (truth-monitor bias -0.150 -> -0.165, sigma_gap flat).
      # Linear stays. NOTE: run_d1_falsification.R replicates this formula
      # -- keep the two in lockstep.
      if (length(z_var) == 1L) "dist_road",
      if ("bg3" %in% names(panel)) "bg3_mean",
      if ("population" %in% names(panel)) "log1p(pop_mean)",
      "factor(metro_id)",
      # Siting-class FE (gap upgrade R1-c, accepted 2026-07-27): the only
      # covariate of the 14-candidate campaign to clear its pre-registered bar
      # (+0.05 leave-monitors-out CV R2; kerbside/traffic premiums positive as
      # pre-registered). Guarded: a degenerate single-level factor would make
      # lm() error, so the term drops gracefully and the spec degrades to the
      # pre-upgrade formula.
      if ("siting_class" %in% names(gap_train) &&
          dplyr::n_distinct(as.character(gap_train$siting_class)) > 1L)
        "factor(siting_class)"
    )
    # M9-shape spec (DESIGN §33b, default OFF). Built here so the SAME object
    # governs the headline fit, every honest-CV fold refit (ADR-0027: the
    # constraint set is part of the estimator) and prediction. Boundary knots
    # use the in-zone COVARIATE distribution only (amendment A2) — no in-zone
    # outcome is touched.
    fect_source_gap_shape()
    gap_shape_spec = NULL
    gap_shape_target_d = NULL   # kept in scope for the per-fold specs (F11)
    if (exists("cpportal_gap_fallback_reset", mode = "function")) {
      cpportal_gap_fallback_reset()   # F7: count degradations for THIS build
    }
    if (exists("cpportal_gap_spec", mode = "function") &&
        exists("cpportal_gap_shape_enabled", mode = "function") &&
        cpportal_gap_shape_enabled()) {
      target_d = tryCatch({
        td = mon_obs[mon_obs$fullaqsid %in% dropped_units, , drop = FALSE]
        if ("dist_road" %in% names(td)) as.numeric(td$dist_road) else NULL
      }, error = function(e) NULL)
      gap_shape_target_d = target_d
      gap_shape_spec = tryCatch(
        cpportal_gap_spec(gap_rhs, gap_train, target_dist_km = target_d),
        error = function(e) NULL)
      if (!is.null(gap_shape_spec)) {
        message("[fect.train.impute] M9-shape ON: ", gap_shape_spec$axis,
                " k=", gap_shape_spec$k, " constraints=",
                paste(gap_shape_spec$constraints, collapse = "+"),
                " spec_digest=", gap_shape_spec$digest)
      }
    }
    if (nrow(gap_train) >= 30L) {
      gap_hed = tryCatch(
        cpportal_gap_fit(
          stats::as.formula(paste0("gap ~ ", paste(gap_rhs, collapse = " + "))),
          data = gap_train, spec = gap_shape_spec
        ),
        error = function(e) {
          message("[fect.train.impute] hedonic GAP model failed: ",
                  conditionMessage(e))
          NULL
        }
      )
    } else {
      message("[fect.train.impute] gap training set too small (",
              nrow(gap_train), " < 30); M7 will not be available")
    }
    if (!is.null(gap_hed)) {
      gap_model_r2 = cpportal_gap_r2(gap_hed)
      # sigma_gap (gap upgrade Card 14, accepted 2026-07-27): the honest
      # predictive SD is the leave-MONITORS-out 10-fold CV RMSE of the gap
      # model, not the in-sample residual SD (which is optimistic by
      # construction). Deterministic folds: rows sorted by fullaqsid, fixed
      # seed 20260727, sample(rep_len(0:9, n)) — the exact fold recipe of the
      # gap_upgrade harness (_common.R::gu_eval). Rows whose held-out fold
      # model cannot score them (unseen metro / siting level) drop from the
      # RMSE rather than erroring. Falls back to the in-sample sigma if CV
      # yields too few scored rows.
      sigma_gap_insample = cpportal_gap_sigma(gap_hed)
      gap_model_r2_loo = NA_real_
      gap_fml = stats::as.formula(paste0("gap ~ ", paste(gap_rhs, collapse = " + ")))
      sigma_gap = tryCatch({
        gt = gap_train[order(gap_train$fullaqsid), , drop = FALSE]
        n_cv = nrow(gt)
        set.seed(20260727L)
        fold = sample(rep_len(0:9, n_cv))
        pred = rep(NA_real_, n_cv)
        for (k in 0:9) {
          te = fold == k
          if (!any(te) || all(te)) next
          # ADR-0027 honest CV: the constrained model is RE-FIT inside every
          # fold (the constraint set is part of the estimator). Folds are
          # unchanged.
          # F11: the KNOTS are part of the estimator too. `gap_shape_spec`
          # carries interior knots at quantiles of the FULL out-of-zone s
          # distribution, i.e. of rows the fold is supposed to be holding out
          # — a (mild) leak that flatters sigma_gap. Rebuild the spec from the
          # FOLD'S TRAINING ROWS. Boundary knots still use the in-zone
          # COVARIATE distribution (amendment A2), which is fixed by design and
          # touches no held-out outcome.
          # Boundary knots keep the FULL covariate union (in-zone targets plus
          # every out-of-zone dist_road, held-out rows included) so the fold's
          # basis still SPANS the rows it must score; only the INTERIOR knots
          # become fold-honest. dist_road is a covariate, so this is the same
          # A2 argument, not selection on the answer.
          spec_k = if (is.null(gap_shape_spec)) NULL else tryCatch(
            cpportal_gap_spec(gap_rhs, gt[!te, , drop = FALSE],
                              target_dist_km = c(gap_shape_target_d,
                                                 as.numeric(gt$dist_road))),
            error = function(e) gap_shape_spec)
          mk = tryCatch(cpportal_gap_fit(gap_fml, data = gt[!te, , drop = FALSE],
                                         spec = spec_k,
                                         where = sprintf("cv_fold_%d", k)),
                        error = function(e) NULL)
          if (is.null(mk)) next
          nd = gt[te, , drop = FALSE]
          ok_row = rep(TRUE, nrow(nd))
          xl = mk$xlevels
          if (!is.null(xl[["factor(metro_id)"]])) {
            ok_row = ok_row & as.character(nd$metro_id) %in% xl[["factor(metro_id)"]]
          }
          if (!is.null(xl[["factor(siting_class)"]])) {
            ok_row = ok_row & as.character(nd$siting_class) %in% xl[["factor(siting_class)"]]
          }
          if (!any(ok_row)) next
          pk = tryCatch(
            suppressWarnings(stats::predict(mk, newdata = nd[ok_row, , drop = FALSE])),
            error = function(e) rep(NA_real_, sum(ok_row))
          )
          pred[which(te)[ok_row]] = as.numeric(pk)
        }
        ok = is.finite(pred) & is.finite(gt$gap)
        if (sum(ok) < 10L) stop("too few CV predictions (", sum(ok), ")")
        gap_model_r2_loo = 1 - sum((gt$gap[ok] - pred[ok])^2) /
          sum((gt$gap[ok] - mean(gt$gap[ok]))^2)
        sqrt(mean((gt$gap[ok] - pred[ok])^2))
      }, error = function(e) {
        message("[fect.train.impute] gap CV RMSE failed (", conditionMessage(e),
                "); falling back to in-sample sigma")
        sigma_gap_insample
      })
      # Variance re-split (gap upgrade R1-a/R2-b/R3-c, accepted 2026-07-27;
      # supersedes ADR-0011 s7.2's rho = 1.0 assertion). sigma_gap is the
      # WITHIN-metro idiosyncratic leftover after the metro FE — treating it as
      # perfectly correlated across a metro's monitors was wrong. Measured
      # within-metro residual correlation at <2 km: rho_hat = 0.1656
      # (bootstrap SE 0.3092; the cautious rho_hat + 1SE = 0.4748 is the
      # reviewer-proof value, documented in the ADR, deliberately NOT used
      # here per Tim's acceptance of the point estimate). rho_hat = 1
      # reproduces the pre-upgrade behaviour exactly.
      gap_rho_hat = 0.1656
      known_gap_metros = unique(as.character(gap_train$metro_id))
      # PARTIAL POOLING (2026-08-04): with alpha_m ~ N(0, tau^2) a metro that
      # contributed NO out-of-cordon donors is still scorable -- it takes the
      # grand mean (alpha_hat = 0) and the full prior variance nu^2 = tau^2, so
      # its prediction is honest AND honestly wide. Under the metro FIXED effect
      # such a metro was simply unidentified, which is why this filter existed.
      # Keep the filter on the FE arm (flag off / lm fallback), drop it when the
      # fit actually carries a pooled intercept.
      gap_pooled = inherits(gap_hed, "cpportal_gap_shape_fit") &&
        !is.null(gap_hed$pool)
      if (gap_pooled) {
        pl = gap_hed$pool
        message(sprintf(paste0("[fect.train.impute] gap metro intercept is ",
                "PARTIALLY POOLED: tau2=%.5f (tau=%.4f) sigma2=%.5f ",
                "(sigma=%.4f) K=%d adequate=%d/%d metros, %d backfit sweep(s), ",
                "edf_pool=%.2f"),
                pl$tau2, sqrt(pl$tau2), pl$sigma2, sqrt(pl$sigma2),
                pl$min_donors, length(pl$adequate), length(pl$k),
                pl$n_sweep %||% NA_integer_, gap_hed$edf_pool %||% NA_real_))
        shrink_m = (pl$k * pl$tau2) / (pl$k * pl$tau2 + pl$sigma2)
        for (m in names(pl$k)) {
          message(sprintf(paste0("[fect.train.impute] gap pool  metro %-6s ",
                  "k=%3d  rbar=%+.4f  alpha_hat=%+.4f  shrink=%.3f  nu=%.4f%s"),
                  m, pl$k[[m]], pl$rbar[[m]], pl$alpha[[m]], shrink_m[[m]],
                  sqrt(pl$nu2[[m]]),
                  if (pl$k[[m]] < pl$min_donors) "   <-- BELOW K" else ""))
        }
        unseen = setdiff(unique(as.character(
          mon_obs$metro_id[mon_obs$fullaqsid %in% dropped_units])),
          known_gap_metros)
        if (length(unseen)) {
          message(sprintf(paste0("[fect.train.impute] gap pool: metro(s) %s ",
                  "have ZERO donors -- scored at the grand mean (alpha=0) with ",
                  "the full prior SD nu=tau=%.4f, instead of being dropped"),
                  paste(unseen, collapse = ", "), sqrt(pl$tau2)))
        }
      }
      # `with_se = FALSE` skips the SE path entirely. That matters because the
      # B=400 monitor-cluster bootstrap lives inside
      # `predict.cpportal_gap_shape_fit()` and only runs under `se.fit = TRUE`
      # (gap_shape.R:1062 returns before B is even read). The FITTED-unit call
      # further down feeds `compute_shared_drift_curve()`, which builds
      # z = sqrt(sat) + gap_pred - mu - tau_bar and then takes medians/loess —
      # it never reads `gap_pred_se`. Bootstrapping there was ~880 s/spec of
      # work whose only output was discarded. `.d1_build_z()` already tolerates
      # a missing SE via `%||% NA_real_`.
      predict_gap_units = function(units, with_se = TRUE) {
        newd = mon_obs |> dplyr::filter(.data$fullaqsid %in% units)
        # FE arm only: a metro absent from gap_train has no estimable level, so
        # its monitors cannot be scored and fall back to M6. The POOLED arm can
        # score them (alpha = 0, nu^2 = tau^2), so the filter lifts.
        if (!gap_pooled) {
          newd = newd |> dplyr::filter(as.character(.data$metro_id) %in% known_gap_metros)
        }
        # Same guard for the siting-class FE: a monitor whose harmonised class
        # never appeared in gap_train cannot be scored by lm(); it drops here
        # and falls back per-row to M6 downstream (gap_pred stays NA).
        siting_lv = gap_hed$xlevels[["factor(siting_class)"]]
        if (!is.null(siting_lv)) {
          n_unseen = sum(!(as.character(newd$siting_class) %in% siting_lv))
          if (n_unseen > 0L) {
            message("[fect.train.impute] gap: dropping ", n_unseen,
                    " unit(s) with siting_class unseen in training (M6 fallback)")
            newd = newd |>
              dplyr::filter(as.character(.data$siting_class) %in% siting_lv)
          }
        }
        if (nrow(newd) == 0L) return(NULL)
        if (!with_se) {
          pf = tryCatch(as.numeric(stats::predict(gap_hed, newdata = newd)),
                        error = function(e) NULL)
          if (is.null(pf)) return(NULL)
          return(tibble::tibble(
            fullaqsid   = newd$fullaqsid,
            gap_pred    = pf,
            gap_pred_se = NA_real_
          ))
        }
        pr = tryCatch(
          stats::predict(gap_hed, newdata = newd, se.fit = TRUE),
          error = function(e) NULL
        )
        if (is.null(pr)) return(NULL)
        # Re-split: sigma_gap^2 -> sigma_gap^2 * (rho + (1 - rho)/n_m), where
        # n_m = gap-predicted monitors pooled into that metro's metro-day mean.
        # The result stays in the day-persistent SHARED bucket downstream
        # (.se0_m_shared) — the gap residual is one draw per monitor, constant
        # across days, so it must NOT be sqrt(n_days)-shrunk; see
        # job/model/fect/gap_upgrade/fn_gap_se_resplit.R for the derivation.
        n_m = as.integer(table(as.character(newd$metro_id))[as.character(newd$metro_id)])
        shrink = gap_rho_hat + (1 - gap_rho_hat) / pmax(n_m, 1L)
        tibble::tibble(
          fullaqsid   = newd$fullaqsid,
          gap_pred    = as.numeric(pr$fit),
          gap_pred_se = sqrt(as.numeric(pr$se.fit)^2 + sigma_gap^2 * shrink)
        )
      }
      # NOTE (M9-r3): `predict_gap_units()` is CALLED further down, AFTER the
      # monitor-MONTH block. It is the call that runs the monitor-cluster
      # bootstrap, and per Tim's directive that bootstrap must now cover BOTH
      # stages — so stage 2 has to exist and be attached to `gap_hed` before the
      # first (and only) scoring pass. Moving the call is purely an ordering
      # change; nothing between here and there reads `gap_pred_tbl`.
      # F7: constrained-fit failures must degrade LOUDLY. Each fallback already
      # raised an immediate warning inside cpportal_gap_fit(); summarise the
      # run-level count here so a reader of the training log sees at a glance
      # whether the "M9-shape" arm was actually the constrained estimator.
      if (exists("cpportal_gap_fallback_count", mode = "function")) {
        n_fb = cpportal_gap_fallback_count()
        if (n_fb > 0L) {
          message(sprintf(paste0("[fect.train.impute] M9-shape DEGRADED: %d ",
                                 "constrained fit(s) fell back to lm() ",
                                 "(headline and/or CV folds): %s"),
                          n_fb,
                          paste(utils::head(cpportal_gap_fallback_reasons(), 12L),
                                collapse = " | ")))
        } else if (!is.null(gap_shape_spec)) {
          message("[fect.train.impute] M9-shape: 0 constrained-fit fallbacks ",
                  "(headline + all CV folds are the registered estimator)")
        }
      }

      # ---- Monitor-MONTH gap refinement (R4; Tim's grain ruling 2026-07-27) --
      # The satellite side of the M7 counterfactual is already monthly
      # (sat_monthly_mean); the gap correction was the last monitor-constant
      # term. The monthly model refines the POINT correction only — gap_pred_se
      # keeps the monitor-level machinery above, which treats the whole gap
      # residual as day-persistent. That is conservative for the
      # month-transient error component (R4 CV decomposition: persistent
      # sigma_p^2 = .043, transient sigma_t^2 = .100, within-monitor AR1
      # phi = .334), so the E3 anchored-SE bound is preserved a fortiori. The
      # cross-month averaging gain (K_eff) would need a month-persistent
      # variance bucket in cross_city_att.R — future ADR, deliberately not
      # taken here. T2 (distance to the metro's congestion core) enters per
      # Tim's R4 ruling: policy impacts concentrate near the cordon and shape
      # the satellite tiles there, so remoter monitors need MORE adjustment —
      # hence its positive coefficient (a predictive control; the sign is the
      # opposite of the §G-2 congestion-proximity mechanism).
      # F4 RESOLUTION AMENDED (Tim's standing directive 2026-07-31, §33b): the
      # refinement is RETAINED on the M9 arm and CONSTRAINED, not disabled. It
      # goes through cpportal_gap_fit() with the same monotone I-spline basis and
      # the same C1 cone on dist_road; factor(moy)/rh_mm/t2 stay in the free
      # parametric block. Constraining the more-correct grain is the way to keep
      # the shape lever honest without discarding the time series.
      # The disable path survives ONLY as the explicit diagnostic
      # CPPORTAL_FECT_M9_MONTHLY_OFF=1 (the gate may want the comparison arm).
      mm_refine_on = fect_m9_monthly_refinement_enabled()
      if (!mm_refine_on) {
        message("[fect.train.impute] monitor-MONTH gap refinement DISABLED by ",
                "CPPORTAL_FECT_M9_MONTHLY_OFF=1 (diagnostic comparison arm) - ",
                "the monitor-level point prediction carries end to end")
      }
      rh_col = intersect("rhum_daily_mean", names(panel))
      if (mm_refine_on && length(rh_col) == 1L) {
        t2_map = fect_load_t2_map()
        t2_join = function(df) {
          # Join by monitor; fill missing with the metro median from the map
          # itself, then the global median, so no training/prediction row is
          # lost to an NA in a control.
          if (is.null(t2_map)) return(df)
          fills = stats::aggregate(t2_core_dist_km ~ metro_id, data = t2_map,
                                   FUN = stats::median)
          names(fills)[2] = ".t2_fill"
          g_fill = stats::median(t2_map$t2_core_dist_km, na.rm = TRUE)
          df |>
            dplyr::left_join(t2_map[, c("fullaqsid", "t2_core_dist_km")],
                             by = "fullaqsid") |>
            dplyr::left_join(fills, by = "metro_id") |>
            dplyr::mutate(
              t2_core_dist_km = dplyr::coalesce(.data$t2_core_dist_km,
                                                .data$.t2_fill, g_fill)
            ) |>
            dplyr::select(-".t2_fill")
        }
        gap_mm_train = panel |>
          dplyr::filter(.data$fullaqsid %in% gap_train$fullaqsid,
                        is.finite(.data$.y_model),
                        is.finite(.data$sat_monthly_mean),
                        .data$sat_monthly_mean > 0) |>
          dplyr::mutate(.ym = format(.data$date, "%Y-%m")) |>
          dplyr::group_by(.data$fullaqsid, .data$.ym) |>
          dplyr::summarise(
            gap_mm = mean(.data$.y_model - sqrt(pmax(.data$sat_monthly_mean, 0)),
                          na.rm = TRUE),
            rh_mm = mean(.data[[rh_col]], na.rm = TRUE),
            n_days_mm = dplyr::n(), .groups = "drop") |>
          dplyr::filter(.data$n_days_mm >= 15L, is.finite(.data$gap_mm),
                        is.finite(.data$rh_mm)) |>
          dplyr::mutate(moy = substr(.data$.ym, 6, 7)) |>
          dplyr::inner_join(
            gap_train |> dplyr::select(-dplyr::any_of(c("gap", "gap_n"))),
            by = "fullaqsid") |>
          t2_join()
        gap_mm_rhs = c(
          gap_rhs, "factor(moy)", "rh_mm",
          if ("t2_core_dist_km" %in% names(gap_mm_train)) "t2_core_dist_km"
        )
        gap_mm_fml = stats::as.formula(paste0("gap_mm ~ ",
                                              paste(gap_mm_rhs, collapse = " + ")))
        # Same spec machinery as the monitor-level fit: dist_road becomes the
        # constrained monotone spline, everything else (INCLUDING the monthly
        # terms) lands in spec$parametric via setdiff().
        gap_mm_spec = if (is.null(gap_shape_spec)) NULL else tryCatch(
          cpportal_gap_spec(gap_mm_rhs, gap_mm_train,
                            target_dist_km = gap_shape_target_d),
          error = function(e) NULL)
        # Which ARM is this monthly fit on? Mirrors `cpportal_gap_fit()`'s own
        # branch predicate exactly (flag AND a usable dist spec), so the weight
        # recipe below can never disagree with the estimator that consumes it.
        mm_shape_arm = !is.null(gap_mm_spec) && isTRUE(gap_mm_spec$has_dist) &&
          exists("cpportal_gap_shape_enabled", mode = "function") &&
          cpportal_gap_shape_enabled()
        # ---- WEIGHTS (Tim's directive: control the heteroskedasticity, keep
        # the sample). Two multiplicative pieces:
        #   n_days_mm  — the cell is a MEAN of n_days_mm daily gaps, so its
        #     sampling variance goes ~1/n_days_mm. This is the weight the
        #     shipped monthly lm() already used, on BOTH arms, unchanged.
        #   1/m_i      — m_i is the monitor's month count. The STATIC block
        #     (h(dist_road), bg3, pop, siting, metro FE) is constant within a
        #     monitor, so without this a 300-month monitor supplies 20x the
        #     leverage on the SHAPE of h(d) that a 15-month monitor does, and
        #     h(d) would be estimated off whichever handful of monitors happen
        #     to have the longest records. 1/m_i makes every monitor contribute
        #     the same total weight to the static block while every one of its
        #     months still contributes to the MONTHLY terms — that is exactly
        #     "don't throw away the time series" with the leverage balanced.
        #
        # GATING (round 4): 1/m_i exists to protect the estimate of the
        # CONSTRAINED h(d), so it belongs to the M9 arm and ONLY the M9 arm.
        # Round 3 applied it unconditionally, which silently re-weighted
        # production/M7 (flag off) and broke the byte-identity claim. With the
        # flags off the weight is `n_days_mm` exactly as at 80424f8 / main.
        # LOCKSTEP: rebuilt per bootstrap replicate through `weight_fn` in the
        # stage-2 recipe below (m_i changes under resampling) and mirrored in
        # diagnostics/validate_m7_from_panel.R.
        mm_weights = if (mm_shape_arm) function(df) {
          m_i = as.numeric(table(as.character(df$fullaqsid))[as.character(df$fullaqsid)])
          as.numeric(df$n_days_mm) / pmax(m_i, 1)
        } else function(df) {
          as.numeric(df$n_days_mm)
        }
        gap_hed_mm = if (nrow(gap_mm_train) >= 100L) tryCatch(
          cpportal_gap_fit(gap_mm_fml, data = gap_mm_train,
                           spec = gap_mm_spec,
                           where = "monthly_refinement",
                           weights = mm_weights(gap_mm_train)),
          error = function(e) {
            message("[fect.train.impute] monthly GAP model failed (",
                    conditionMessage(e), "); monitor-level corrections only")
            NULL
          }
        ) else NULL
        if (!is.null(gap_hed_mm)) {
          # Split out so the SAME scoring frame the point prediction uses is
          # also what the two-stage bootstrap re-scores each replicate on.
          mm_newdata_for = function(units) {
            # Same pooling relaxation as predict_gap_units(): a zero-donor
            # metro is scorable under the pooled intercept.
            base = mon_obs |>
              dplyr::filter(.data$fullaqsid %in% units)
            if (!(inherits(gap_hed_mm, "cpportal_gap_shape_fit") &&
                  !is.null(gap_hed_mm$pool))) {
              base = base |>
                dplyr::filter(as.character(.data$metro_id) %in% known_gap_metros)
            }
            siting_lv = gap_hed_mm$xlevels[["factor(siting_class)"]]
            if (!is.null(siting_lv)) {
              base = base |>
                dplyr::filter(as.character(.data$siting_class) %in% siting_lv)
            }
            if (nrow(base) == 0L) return(NULL)
            mm = panel |>
              dplyr::filter(.data$fullaqsid %in% base$fullaqsid) |>
              dplyr::mutate(.ym = format(.data$date, "%Y-%m")) |>
              dplyr::group_by(.data$fullaqsid, .data$.ym) |>
              dplyr::summarise(rh_mm = mean(.data[[rh_col]], na.rm = TRUE),
                               .groups = "drop") |>
              dplyr::filter(is.finite(.data$rh_mm)) |>
              dplyr::mutate(moy = substr(.data$.ym, 6, 7)) |>
              dplyr::inner_join(base, by = "fullaqsid") |>
              t2_join()
            moy_lv = gap_hed_mm$xlevels[["factor(moy)"]]
            if (!is.null(moy_lv)) mm = mm |> dplyr::filter(.data$moy %in% moy_lv)
            if (nrow(mm) == 0L) return(NULL)
            mm
          }
          predict_gap_units_monthly = function(units) {
            mm = mm_newdata_for(units)
            if (is.null(mm)) return(NULL)
            pr = tryCatch(
              as.numeric(stats::predict(gap_hed_mm, newdata = mm)),
              error = function(e) NULL
            )
            if (is.null(pr)) return(NULL)
            tibble::tibble(fullaqsid = mm$fullaqsid, .ym = mm$.ym,
                           gap_pred_m = pr)
          }
          gap_pred_mm_tbl = predict_gap_units_monthly(dropped_units)
          # ---- two-stage cluster bootstrap wiring -------------------------
          # The reported gap is coalesce(monthly, monitor-level), so the SE has
          # to resample MONITORS ONCE and refit BOTH stages on that draw. The
          # base fit carries the stage-2 recipe; the resample, the weight
          # rebuild (1/m_i is draw-dependent) and the failure accounting all
          # live in gap_shape.R::.gap_shape_boot_se().
          if (inherits(gap_hed_mm, "cpportal_gap_shape_fit") &&
              inherits(gap_hed, "cpportal_gap_shape_fit") &&
              exists("cpportal_gap_attach_stage2", mode = "function")) {
            mm_nd = tryCatch(mm_newdata_for(dropped_units), error = function(e) NULL)
            if (!is.null(mm_nd) && nrow(mm_nd) > 0L) {
              gap_hed = cpportal_gap_attach_stage2(gap_hed, list(
                train     = as.data.frame(gap_mm_train),
                formula   = gap_mm_fml,
                spec      = gap_mm_spec,
                weight_fn = mm_weights,
                newdata   = as.data.frame(mm_nd),
                key       = "fullaqsid"
              ))
              message("[fect.train.impute] M9-shape: bootstrap is TWO-STAGE ",
                      "(monitors resampled once; base + monthly refinement ",
                      "both refit per replicate)")
            } else {
              message("[fect.train.impute] M9-shape: two-stage bootstrap NOT ",
                      "wired (no monthly scoring rows for the dropped units); ",
                      "the SE covers the monitor-level stage only")
            }
          }
          message(sprintf(
            paste0(
              "[fect.train.impute] monthly GAP refinement [%s]: n=%d ",
              "monitor-months (%d monitors) R2=%.3f sigma=%.4f; monthly point ",
              "corrections for %d unit-months across %d dropped units ",
              "(monitor-level prediction remains the fallback and the SE carrier)"
            ),
            if (exists("cpportal_gap_fit_label", mode = "function"))
              cpportal_gap_fit_label(gap_hed_mm) else "linear-lm",
            nrow(gap_mm_train), dplyr::n_distinct(gap_mm_train$fullaqsid),
            cpportal_gap_r2(gap_hed_mm), cpportal_gap_sigma(gap_hed_mm),
            if (is.null(gap_pred_mm_tbl)) 0L else nrow(gap_pred_mm_tbl),
            if (is.null(gap_pred_mm_tbl)) 0L
            else dplyr::n_distinct(gap_pred_mm_tbl$fullaqsid)
          ))
          # In-zone monotonicity spot-check: C1 is imposed on knot coefficients;
          # assert it survives on the km axis at the SCORING path, for the rows
          # the refinement actually governs.
          if (exists("cpportal_gap_monotone_check", mode = "function")) {
            mc = tryCatch(cpportal_gap_monotone_check(gap_hed_mm),
                          error = function(e) NULL)
            if (!is.null(mc)) {
              message(sprintf(paste0("[fect.train.impute] monthly h(d) ",
                      "monotonicity: %s (max increase %.3g over %.3f..%.3f km)"),
                      if (isTRUE(mc$monotone)) "OK (non-increasing)" else "VIOLATED",
                      mc$max_increase, mc$lo_km, mc$hi_km))
            }
          }
        }
      }
      # ---- gap point predictions + (two-stage) bootstrap SE ---------------
      # Deliberately AFTER the monthly block: this is the call that runs the
      # bootstrap, and stage 2 must already be attached to `gap_hed`.
      gap_pred_tbl = predict_gap_units(dropped_units)
      message(sprintf(
        paste0(
          "[fect.train.impute] hedonic GAP model: n=%d R2=%.3f R2_loo=%.3f ",
          "sigma_gap=%.4f (leave-monitors-out 10-fold CV RMSE, deterministic ",
          "folds seed 20260727; in-sample sigma=%.4f) rho_hat=%.4f ",
          "(bootstrap SE 0.3092; cautious rho+1SE=0.4748 lives in the ADR, ",
          "not in code) predicted=%d/%d dropped units (M7 available)"
        ),
        nrow(gap_train), gap_model_r2, gap_model_r2_loo,
        sigma_gap, sigma_gap_insample, gap_rho_hat,
        if (is.null(gap_pred_tbl)) 0L else nrow(gap_pred_tbl),
        length(dropped_units)
      ))
    }
  }
  if (is.null(gap_hed)) predict_gap_units = function(units, with_se = TRUE) NULL
  ilap("hedonic_gap")

  # METRO scope: anchor the baseline LEVEL to the SATELLITE (bg3 stays an ordinary
  # covariate, NOT the level-setter). For a dropped monitor the only unknown in
  #   yhat0_it = theta_i + tau_t + X_it' beta
  # is the intercept theta_i. Anchor it as B_i = a + b_sat * sqrt(sat_longrun_i),
  # calibrating (a, b_sat) by regressing the FITTED monitors' OBSERVED baseline
  # sqrt(PM2.5) (mean over their untreated days) on sqrt(satellite). This is a
  # level-on-level fit -> b_sat ~ +1, unlike the old companion-alpha calibration
  # (it regressed the bg3-RESIDUAL alpha on satellite and got b_sat ~ 0, because
  # bg3 already absorbs the city baseline). Covariates (incl bg3) then enter the
  # counterfactual ONLY as within-monitor deviations (X_it - Xbar_i) and the
  # control pool only as a level-free deviation (M_t - M_grand) — see
  # impute_unit_days below. ZONE scope is untouched.
  metro_anchor_on = metro_scope && use_sat
  B_of = function(units) NULL
  if (metro_anchor_on) {
    base_by_mon = panel |>
      dplyr::filter(!(.data$treated %in% TRUE), is.finite(.data$.y_model)) |>
      dplyr::group_by(.data$fullaqsid) |>
      dplyr::summarise(y_base = mean(.data$.y_model), .groups = "drop")
    base_cal = base_by_mon |>
      dplyr::inner_join(mon_obs, by = "fullaqsid") |>
      dplyr::filter(is.finite(.data$y_base), is.finite(.data$sat_longrun))
    a0 = 0; b_sat = 1; B_sigma = if (is.finite(sigma2_cm)) sqrt(sigma2_cm) else 1
    if (nrow(base_cal) >= 10L) {
      bfit = tryCatch(stats::lm(y_base ~ sqrt(sat_longrun), data = base_cal),
                      error = function(e) NULL)
      if (!is.null(bfit)) {
        cf = stats::coef(bfit)
        a0 = unname(cf[[1L]]); b_sat = unname(cf[[2L]])
        B_sigma = summary(bfit)$sigma
      }
    }
    # metro-mean sqrt(sat) fallback for dropped monitors lacking their own anchor
    metro_satbar = mon_obs |>
      dplyr::filter(is.finite(.data$sat_longrun)) |>
      dplyr::group_by(.data$metro_id) |>
      dplyr::summarise(satsqrt = mean(sqrt(.data$sat_longrun)), .groups = "drop")
    B_of = function(units) {
      m = mon_obs |>
        dplyr::filter(.data$fullaqsid %in% units) |>
        dplyr::left_join(metro_satbar, by = "metro_id") |>
        dplyr::mutate(ssq = dplyr::if_else(is.finite(.data$sat_longrun),
                                           sqrt(.data$sat_longrun), .data$satsqrt)) |>
        dplyr::filter(is.finite(.data$ssq)) |>
        dplyr::transmute(fullaqsid = .data$fullaqsid,
                         B_i = a0 + b_sat * .data$ssq, B_se = B_sigma)
      if (nrow(m) == 0L) NULL else m
    }
    message(sprintf(
      "[fect.train.impute] metro satellite LEVEL anchor: b_sat=%+.3f a=%+.3f (n_cal=%d monitors); bg3 is an ordinary covariate",
      b_sat, a0, nrow(base_cal)))
  }

  # --- 3. Composition-matched control aggregates per (metro_id, date) --------
  ctrl_rows = panel |>
    dplyr::filter(
      .data$treated %in% FALSE,
      !(.data$fullaqsid %in% zone_units),
      is.finite(.data$.y_model)
    )
  if (!is.null(alpha_tbl)) {
    ctrl_rows = ctrl_rows |>
      dplyr::left_join(alpha_tbl, by = "fullaqsid")
  } else {
    ctrl_rows$alpha = NA_real_
  }
  ctrl = ctrl_rows |>
    dplyr::group_by(dplyr::across(dplyr::all_of(ctrl_keys))) |>
    dplyr::summarise(
      M_t        = mean(.data$.y_model, na.rm = TRUE),
      n_ctrl     = dplyr::n(),
      alpha_ctrl = mean(.data$alpha, na.rm = TRUE),
      dplyr::across(dplyr::all_of(x_vars), ~ mean(.x, na.rm = TRUE),
                    .names = "ctrl_{.col}"),
      .groups = "drop"
    )
  # METRO anchor: grand control level (level-free reference for the (M_t - M_grand)
  # common-time DEVIATION, so the level comes solely from the satellite B_i).
  M_grand = if (metro_anchor_on) mean(ctrl$M_t, na.rm = TRUE) else NA_real_

  # Square back-transform (sqrt model scale -> native ug/m3), the SAME closed form
  # as get_simeffects_fect(backtransform="square"): for X~N(mu,s), Y=X^2 =>
  # E[Y]=mu^2+s^2, SD[Y]=s*sqrt(2 s^2 + 4 mu^2). Imputation is AQ-only (sqrt
  # outcome), so the square inverse always applies here. Anchored ATTs are stored
  # in NATIVE units, consistent with the fect path (closes the A.10 back-transform
  # gap that previously left imputed rows on the sqrt scale).
  bt_mean_sq = function(mu, s) mu^2 + s^2
  bt_sd_sq   = function(mu, s) s * sqrt(2 * s^2 + 4 * mu^2)

  # --- 4. Per monitor-day counterfactual: M2, degrading to M1/M0 -------------
  # Reusable for any unit set: the dropped units (production output) and the
  # fitted-zone calibration testbed (bias bands) share the identical formula.
  impute_unit_days = function(units, alpha_preds) {
    rows = panel |>
      dplyr::filter(
        .data$fullaqsid %in% units,
        .data$treated %in% TRUE,
        is.finite(.data$.y_model)
      ) |>
      dplyr::select(dplyr::all_of(c("metro_id", "fullaqsid", "date", ".y_model", x_vars)))
    if (nrow(rows) == 0L) return(NULL)
    # LEFT (not inner) join: a treated monitor-day whose (metro, date) key has no
    # control aggregate used to be DELETED here, before any counterfactual was
    # even attempted. That silently erased London's original-CCZ era — ERG-VS1
    # has in-cordon data from 2003-05, but metro 943 had ZERO out-of-zone
    # monitors in 2003 and two in 2004, so every one of VS1's monitor-days lost
    # the join. Nothing in the M7 counterfactual needs a same-day same-metro
    # control (the level is sqrt(sat_monthly_mean) + gap_pred, and the gap model
    # trains on out-of-cordon monitors from ALL metros); only the
    # control-centered cov_adj and the sigma2_cm/n_ctrl SE term do. The ZONE
    # branch below therefore keeps control-less rows as M7-only rows with
    # within-monitor centering (the validated metro-scope pattern). METRO
    # behaviour is unchanged — see the filter at the top of that branch.
    rows = rows |>
      dplyr::left_join(ctrl, by = ctrl_keys)
    if (nrow(rows) == 0L) return(NULL)
    rows$.has_ctrl = !is.na(rows$n_ctrl)

    if (metro_anchor_on) {
      # METRO branch: unchanged semantics — no control aggregate, no row.
      rows = rows |> dplyr::filter(.data$.has_ctrl)
      rows$.has_ctrl = NULL
      if (nrow(rows) == 0L) return(NULL)
      # ---- METRO satellite-anchored path (M7 default; M6/M5 per-row fallback) ----
      # Paper eq:metro-anchor — the counterfactual LEVEL for a dropped whole-metro
      # monitor is set by the SATELLITE plus a hedonic gap correction; covariates
      # enter ONLY as within-monitor deviations (X_itk - Xbar_ik), so they carry
      # no cross-city level. This is the SAME construction as the ZONE M7 path
      # (§7.4); the only scope difference is the dropped-unit set and that the gap
      # model drops its metro FE for metro scope (predict_gap_units). Per-row ladder:
      #   M7: sqrt(yhat0_it) = sqrt(sat_monthly_mean_i,m(t)) + gap_pred_i + cov_adj
      #       shared SE = gap_pred_se  (proper hedonic-gap prediction uncertainty)
      #   M6: sqrt(yhat0_it) = sqrt(sat_monthly_mean_i,m(t)) + cov_adj      (no gap)
      #       shared SE = sqrt(sigma2_cm)  (crude — no fitted correction model)
      #   M5: sqrt(yhat0_it) = B_i + (M_t - M_grand) + cov_adj    (static longrun anchor)
      #       shared SE = B_se  (satellite-LEVEL calibration intercept SE)
      # M5 is the safety net for rows the monthly tile doesn't cover; the four
      # anchored cities have satellite coverage, so M7/M6 rows dominate.
      Bt = B_of(units)                        # M5 static-anchor fallback (may be NULL)
      mon_means = rows |>
        dplyr::group_by(.data$fullaqsid) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(x_vars), ~ mean(.x, na.rm = TRUE),
                        .names = "mbar_{.col}"),
          .groups = "drop")
      rows = rows |>
        dplyr::left_join(mon_means, by = "fullaqsid")
      if (!is.null(Bt)) {
        rows = rows |> dplyr::left_join(Bt, by = "fullaqsid")
      } else {
        rows$B_i = NA_real_; rows$B_se = NA_real_
      }
      # Hedonic GAP predictions (per monitor) — the SAME table the zone path uses.
      if (!is.null(gap_pred_tbl)) {
        rows = rows |> dplyr::left_join(gap_pred_tbl, by = "fullaqsid")
        # F4 as AMENDED (Tim's directive 2026-07-31): the monthly predictions
        # coalesced in here are now CONSTRAINED on the M9 arm (same monotone
        # basis + C1 cone as h(d)), so this override is shape-consistent rather
        # than an unconstrained overwrite. The guard STAYS: it is what
        # CPPORTAL_FECT_M9_MONTHLY_OFF=1 switches to run the monitor-level-only
        # diagnostic arm.
        if (!is.null(gap_pred_mm_tbl) && fect_m9_monthly_refinement_enabled()) {
          # R4 monthly refinement: month-resolved point correction where
          # available; the monitor-level prediction stays as fallback and
          # as the (conservative, day-persistent) SE carrier.
          rows = rows |>
            dplyr::mutate(.ym = format(.data$date, "%Y-%m")) |>
            dplyr::left_join(gap_pred_mm_tbl, by = c("fullaqsid", ".ym")) |>
            dplyr::mutate(gap_pred = dplyr::coalesce(.data$gap_pred_m,
                                                     .data$gap_pred)) |>
            dplyr::select(-".ym", -"gap_pred_m")
        }
      } else {
        rows$gap_pred = NA_real_; rows$gap_pred_se = NA_real_
      }
      if (nrow(rows) == 0L) return(NULL)
      # cov_adj = WITHIN-monitor covariate deviation, EXCLUDING sat_monthly_mean
      # (ADR-0013). sat is the anchor base — re-adding it here via β would
      # double-count the satellite signal, because the monthly √-anchor already
      # carries it (and the double-use inflated high-satellite Milan). Weather /
      # population / bg3 enter as within-monitor deviations. Within-monitor (not
      # control-centered) is required at metro scope: the "controls" are OTHER
      # metros, so control-centering imports a cross-city covariate gradient and
      # blows up (verified: Milan −24.9 under control vs −0.07 under within).
      cov_keys = setdiff(x_vars, "sat_monthly_mean")
      cov_adj = rep(0, nrow(rows))
      if (!is.null(beta_hat)) {
        for (k in cov_keys) {
          d = rows[[k]] - rows[[paste0("mbar_", k)]]   # within-monitor deviation
          d[!is.finite(d)] = 0
          cov_adj = cov_adj + d * beta_hat[[k]]
        }
      }
      rows$cov_adj = cov_adj
      # NB: the heavy negative tail in Milan's per-day anchored ATT (monthly
      # satellite anchor vs daily observation, squared) is handled downstream by
      # a robust Huber location estimator in per_metro_overall_att (ADR-0012), NOT
      # by winsorizing the counterfactual here — an earlier √-scale cap was found
      # redundant with the robust aggregator (identical results at any trim ≥5%)
      # and was removed.
      # D2b variance split (ADR-0011 §7.2): shared component (gap_pred_se / B_se —
      # one estimate per monitor, correlated 1.0 across the city's days) and
      # independent component (sigma2_cm/n_ctrl — control-mean SE, independent
      # across days). Native-scale variance squares are approximately additive
      # under the delta method, so the downstream aggregator √n_days-shrinks only
      # the independent part.
      m7_active   <- (impute_mode == "M7")
      m6_active   <- (impute_mode == "M6")
      # THE LABEL FOLLOWS THE MATH (2026-08-03 correction). This closure computes
      # the CONTEMPORANEOUS M7 anchor — sqrt(sat_monthly_mean_it) + gap_pred +
      # cov_adj — with no trailing D(lambda*) window, no T_f freeze and no
      # donor-drift correction. It therefore stamps M7, ALWAYS, whatever
      # CPPORTAL_FECT_ANCHOR_METHOD says. M10 rows are produced by the Stage P/F
      # construction and promoted in train_fect_bundle() (see
      # `fect_promote_d1_to_production()`); stamping M10 here produced a pin
      # whose rows read "M10" while the arithmetic was M7 (PR #340 regression).
      .anchor_imp_label <- FECT_M7_IMP_METHOD
      has_sat_col <- "sat_monthly_mean" %in% names(rows)
      rows |>
        dplyr::mutate(
          .sat_ok = has_sat_col &
            (if (has_sat_col) is.finite(.data$sat_monthly_mean) & .data$sat_monthly_mean > 0
             else FALSE),
          .m7_row = m7_active & .data$.sat_ok & is.finite(.data$gap_pred),
          .m6_row = (m6_active | (m7_active & .data$.sat_ok & !is.finite(.data$gap_pred))) &
                    .data$.sat_ok,
          # The anchored branch's label tracks the resolved anchor method:
          # FECT_M10_IMP_METHOD under M10 (default), FECT_M7_IMP_METHOD under
          # the legacy M7 rollback. `basis` tagging is untouched either way.
          imp_method = dplyr::case_when(
            .data$.m7_row ~ .anchor_imp_label,
            .data$.m6_row ~ "M6_sat_monthly_level_anchor_metro",
            TRUE          ~ "M5_sat_level_anchor_metro"
          ),
          .yhat0_m = dplyr::case_when(
            .data$.m7_row ~
              sqrt(pmax(.data$sat_monthly_mean, 0)) + .data$gap_pred + .data$cov_adj,
            .data$.m6_row ~
              sqrt(pmax(.data$sat_monthly_mean, 0)) + .data$cov_adj,
            TRUE ~
              .data$B_i + (.data$M_t - M_grand) + .data$cov_adj
          ),
          .yhat1_m = .data$.y_model,
          .se0_m_shared = dplyr::case_when(
            .data$.m7_row ~ dplyr::coalesce(.data$gap_pred_se, 0),
            .data$.m6_row ~ sqrt(pmax(if (is.finite(sigma2_cm)) sigma2_cm else 0, 0)),
            TRUE          ~ dplyr::coalesce(.data$B_se, 0)
          ),
          .se0_m_indep  = sqrt(
            (if (is.finite(sigma2_cm)) sigma2_cm else 0) / pmax(.data$n_ctrl, 1L)
          ),
          .se0_m   = sqrt(.data$.se0_m_shared^2 + .data$.se0_m_indep^2),
          yhat0  = bt_mean_sq(.data$.yhat0_m, .data$.se0_m),
          yhat1  = .data$.yhat1_m^2,
          att    = .data$yhat1 - .data$yhat0,
          se_att = bt_sd_sq(.data$.yhat0_m, .data$.se0_m),
          se_att_shared_sq = bt_sd_sq(.data$.yhat0_m, .data$.se0_m_shared)^2,
          se_att_indep_sq  = bt_sd_sq(.data$.yhat0_m, .data$.se0_m_indep)^2
        ) |>
        dplyr::filter(is.finite(.data$.yhat0_m)) |>
        dplyr::select(-".yhat0_m", -".yhat1_m",
                      -".se0_m", -".se0_m_shared", -".se0_m_indep",
                      -".sat_ok", -".m6_row", -".m7_row")
    } else {
      # ---- ZONE path: control-centered cov_adj, EXCLUDING sat_monthly_mean ----
      # Covariate adjustment (X_it - Xbar_ctrl,t)' beta, NA-component-safe.
      # sat_monthly_mean is dropped (ADR-0013): it is the anchor base, so
      # re-adding it via β double-counts the satellite. Centering stays
      # control-centered — at ZONE scope the controls are same-metro
      # out-of-zone monitors, a valid *local* reference (unlike metro scope,
      # whose controls are other cities and require within-monitor centering).
      #
      # CONTROL-FREE ROWS (2026-07-30): a monitor-day with no same-metro
      # out-of-zone control that day cannot be control-centered. Rather than
      # deleting it (the old inner_join), center it on the monitor's OWN mean —
      # exactly the metro-branch `mon_means` construction — and mark it
      # centering = "within". Rows WITH a control keep centering = "ctrl" and
      # are numerically untouched.
      cov_keys = setdiff(x_vars, "sat_monthly_mean")
      no_ctrl_any = any(!rows$.has_ctrl)
      if (no_ctrl_any && length(x_vars) > 0L) {
        mon_means = rows |>
          dplyr::group_by(.data$fullaqsid) |>
          dplyr::summarise(
            dplyr::across(dplyr::all_of(x_vars), ~ mean(.x, na.rm = TRUE),
                          .names = "mbar_{.col}"),
            .groups = "drop")
        rows = rows |> dplyr::left_join(mon_means, by = "fullaqsid")
      }
      no_ctrl = !rows$.has_ctrl
      cov_adj = rep(0, nrow(rows))
      if (!is.null(beta_hat)) {
        for (k in cov_keys) {
          d = rows[[k]] - rows[[paste0("ctrl_", k)]]
          if (no_ctrl_any) {
            mb = paste0("mbar_", k)
            if (mb %in% names(rows)) {
              dw = rows[[k]] - rows[[mb]]         # within-monitor deviation
              d[no_ctrl] = dw[no_ctrl]
            }
          }
          d[!is.finite(d)] = 0
          cov_adj = cov_adj + d * beta_hat[[k]]
        }
      }
      rows$cov_adj = cov_adj
      rows$centering = dplyr::if_else(rows$.has_ctrl, "ctrl", "within")

      if (!is.null(alpha_preds)) {
        rows = rows |> dplyr::left_join(alpha_preds, by = "fullaqsid")
      } else {
        rows$alpha_pred    = NA_real_
        rows$alpha_pred_se = NA_real_
      }
      # M7 gap predictions (parallel to alpha_preds); attached to rows for
      # per-row availability. Falls back to NA when gap model was unavailable
      # or the monitor's metro wasn't in the gap training set.
      if (!is.null(gap_pred_tbl)) {
        rows = rows |> dplyr::left_join(gap_pred_tbl, by = "fullaqsid")
        # F4 as AMENDED (Tim's directive 2026-07-31): the monthly predictions
        # coalesced in here are now CONSTRAINED on the M9 arm (same monotone
        # basis + C1 cone as h(d)), so this override is shape-consistent rather
        # than an unconstrained overwrite. The guard STAYS: it is what
        # CPPORTAL_FECT_M9_MONTHLY_OFF=1 switches to run the monitor-level-only
        # diagnostic arm.
        if (!is.null(gap_pred_mm_tbl) && fect_m9_monthly_refinement_enabled()) {
          # R4 monthly refinement: month-resolved point correction where
          # available; the monitor-level prediction stays as fallback and
          # as the (conservative, day-persistent) SE carrier.
          rows = rows |>
            dplyr::mutate(.ym = format(.data$date, "%Y-%m")) |>
            dplyr::left_join(gap_pred_mm_tbl, by = c("fullaqsid", ".ym")) |>
            dplyr::mutate(gap_pred = dplyr::coalesce(.data$gap_pred_m,
                                                     .data$gap_pred)) |>
            dplyr::select(-".ym", -"gap_pred_m")
        }
      } else {
        rows$gap_pred    = NA_real_
        rows$gap_pred_se = NA_real_
      }

      # D2b variance split (ADR-0011 §7.2): shared (alpha_pred_se or, for
      # M6, satellite-anchor uncertainty ≈ sqrt(sigma2)) and independent
      # (sigma2_cm/n_ctrl — control-mean SE for the covariate adjustment).
      # Native-scale variance squares are approximately additive under the
      # delta method.
      #
      # M6 mode (Tim 2026-07-01, DEFAULT):
      #   .yhat0_m = sqrt(sat_monthly_mean_it) + cov_adj
      #   The satellite tile at monitor i for month(t) IS the counterfactual
      #   level. cov_adj still adjusts for weather etc. deviations. No
      #   alpha_offset. No M_t. Falls back per-row to M4c when
      #   sat_monthly_mean is NA (rare — ~15% of panel days per earlier audit).
      #
      # M4c mode (env override for backward compatibility):
      #   .yhat0_m = M_t + cov_adj + alpha_offset       (original)
      #
      # CONTROL-FREE ROWS (.has_ctrl == FALSE): M_t, alpha_ctrl and n_ctrl are
      # all NA, so every control-mean method (M4c / M2 / M1 / M0) is undefined
      # — those rows are DROPPED below rather than silently produced. Only the
      # satellite-anchored formulas survive: M7 always, M6 only when the
      # operator explicitly opted into M6 mode (the automatic M7->M6 per-row
      # fallback is NOT taken without a control, because its shared SE would be
      # the crude sqrt(sigma2_cm) on top of an already control-free row).
      # Their independent SE term drops the /n_ctrl shrink (n_ctrl treated as
      # 1 in the formula; stored as 0 in the output to flag the provenance) and
      # alpha_offset is exactly 0.
      m6_active <- (impute_mode == "M6")
      m7_active <- (impute_mode == "M7")
      # THE LABEL FOLLOWS THE MATH (2026-08-03 correction). This closure computes
      # the CONTEMPORANEOUS M7 anchor — sqrt(sat_monthly_mean_it) + gap_pred +
      # cov_adj — with no trailing D(lambda*) window, no T_f freeze and no
      # donor-drift correction. It therefore stamps M7, ALWAYS, whatever
      # CPPORTAL_FECT_ANCHOR_METHOD says. M10 rows are produced by the Stage P/F
      # construction and promoted in train_fect_bundle() (see
      # `fect_promote_d1_to_production()`); stamping M10 here produced a pin
      # whose rows read "M10" while the arithmetic was M7 (PR #340 regression).
      .anchor_imp_label <- FECT_M7_IMP_METHOD
      has_sat_col <- "sat_monthly_mean" %in% names(rows)
      rows |>
        dplyr::mutate(
          alpha_offset = dplyr::if_else(
            .data$.has_ctrl &
              is.finite(.data$alpha_pred) & is.finite(.data$alpha_ctrl),
            .data$alpha_pred - .data$alpha_ctrl,
            0
          ),
          # Per-row mode eligibility:
          #   M7 wants: sat available AND gap_pred available for this monitor
          #   M6 wants: sat available (fallback / opt-in)
          .sat_ok = has_sat_col &
            (if (has_sat_col) is.finite(.data$sat_monthly_mean) & .data$sat_monthly_mean > 0
             else FALSE),
          .m7_row = m7_active & .data$.sat_ok & is.finite(.data$gap_pred),
          .m6_row = (m6_active | (m7_active & .data$.sat_ok & !is.finite(.data$gap_pred))) &
                    .data$.sat_ok,
          # Control-free eligibility: M7, or an explicitly opted-in M6.
          .m6_row = .data$.m6_row & (.data$.has_ctrl | m6_active),
          .keep_row = .data$.has_ctrl | .data$.m7_row | .data$.m6_row,
          imp_method = dplyr::case_when(
            .data$.m7_row                                                    ~ .anchor_imp_label,
            .data$.m6_row                                                    ~ "M6_sat_monthly_level_anchor",
            is.null(beta_hat)                                                ~ "M0_naive",
            is.finite(.data$alpha_pred) & is.finite(.data$alpha_ctrl) &
              use_sat                                                        ~ "M4c_hedonic_alpha_plus_sat_monthly_covariate",
            is.finite(.data$alpha_pred) & is.finite(.data$alpha_ctrl)       ~ "M2_hedonic_alpha",
            TRUE                                                             ~ "M1_cov_adj"
          ),
          # Level anchor per row:
          #   M7:  sqrt(sat) + gap_pred + cov_adj
          #   M6:  sqrt(sat) + cov_adj  (no gap correction)
          #   M4c/M2/M1/M0: M_t + cov_adj + alpha_offset
          .yhat0_m = dplyr::case_when(
            .data$.m7_row ~
              sqrt(pmax(.data$sat_monthly_mean, 0)) + .data$gap_pred + .data$cov_adj,
            .data$.m6_row ~
              sqrt(pmax(.data$sat_monthly_mean, 0)) + .data$cov_adj,
            TRUE ~
              .data$M_t + .data$cov_adj + .data$alpha_offset
          ),
          .yhat1_m = .data$.y_model,
          # Shared SE per row:
          #   M7: gap_pred_se (proper prediction uncertainty from hedonic gap)
          #   M6: sqrt(sigma2_cm) (crude — no fitted correction model)
          #   M4c: alpha_pred_se (unchanged)
          .se0_m_shared = dplyr::case_when(
            .data$.m7_row ~ dplyr::coalesce(.data$gap_pred_se, 0),
            .data$.m6_row ~ sqrt(pmax(if (is.finite(sigma2_cm)) sigma2_cm else 0, 0)),
            TRUE          ~ dplyr::coalesce(.data$alpha_pred_se, 0)
          ),
          # n_ctrl is NA on control-free rows -> pmax(NA, 1L) is NA, which would
          # poison every SE downstream. Treat those rows as n_ctrl = 1 in the
          # formula (no 1/n shrink of the companion residual variance: the
          # control mean simply is not available, so nothing averages the noise
          # down) and record n_ctrl = 0 in the output so consumers can see it.
          .n_ctrl_eff = dplyr::if_else(
            is.finite(.data$n_ctrl) & .data$n_ctrl >= 1L,
            as.integer(.data$n_ctrl), 1L
          ),
          n_ctrl = dplyr::if_else(.data$.has_ctrl,
                                  as.integer(.data$n_ctrl), 0L),
          .se0_m_indep  = sqrt(
            (if (is.finite(sigma2_cm)) sigma2_cm else 0) / .data$.n_ctrl_eff
          ),
          .se0_m   = sqrt(.data$.se0_m_shared^2 + .data$.se0_m_indep^2),
          yhat0  = bt_mean_sq(.data$.yhat0_m, .data$.se0_m),
          yhat1  = .data$.yhat1_m^2,
          att    = .data$yhat1 - .data$yhat0,
          se_att = bt_sd_sq(.data$.yhat0_m, .data$.se0_m),
          se_att_shared_sq = bt_sd_sq(.data$.yhat0_m, .data$.se0_m_shared)^2,
          se_att_indep_sq  = bt_sd_sq(.data$.yhat0_m, .data$.se0_m_indep)^2
        ) |>
        # Drop the control-free rows no satellite-anchored formula can serve
        # (and any non-finite counterfactual among them). Rows WITH a control
        # are kept exactly as before — .keep_row is TRUE for all of them, so
        # this filter is a no-op on the pre-existing row set.
        dplyr::filter(.data$.keep_row,
                      .data$.has_ctrl | is.finite(.data$.yhat0_m)) |>
        dplyr::select(-".yhat0_m", -".yhat1_m", -".se0_m",
                      -".se0_m_shared", -".se0_m_indep",
                      -".sat_ok", -".m6_row", -".m7_row",
                      -".has_ctrl", -".keep_row", -".n_ctrl_eff",
                      -dplyr::starts_with("mbar_"))
    }
  }

  ilap("ctrl_aggregates")
  imputed_per_monitor = impute_unit_days(dropped_units, alpha_pred_tbl)
  if (is.null(imputed_per_monitor)) {
    message("[fect.train.impute] dropped zone monitors have zero TREATED panel rows; nothing to impute")
    return(NULL)
  }
  # Covariate-centering provenance rides along on every row (zone branch sets it
  # per row; the metro branch is within-monitor by construction).
  if (!("centering" %in% names(imputed_per_monitor))) {
    imputed_per_monitor$centering = if (metro_anchor_on) "within" else "ctrl"
  }
  {
    ctr_tab = table(imputed_per_monitor$centering)
    message("[fect.train.impute] covariate centering: ",
            paste(sprintf("%s=%d", names(ctr_tab), as.integer(ctr_tab)),
                  collapse = " "),
            " monitor-day rows")
  }
  ilap("impute_dropped_rows")

  # --- 4b. Empirical bias band (self-calibration) -----------------------------
  # Apply the SAME imputation formula to the zone monitors fect DID fit (the
  # leave-out testbed: NYC, parts of London) and diff against fect's own
  # per-(monitor,day) effects. The residual distribution r = att_imputed -
  # eff_fect is the procedure's measured error; its quantiles ride along on
  # every imputed row so downstream consumers can show
  #   att_corrected = att - cal_bias,  band = [att - cal_q95, att - cal_q05].
  # Computed fresh per (outcome, run) — no external calibration file needed.
  cal = NULL
  fitted_zone = intersect(zone_units, fitted_units)
  if (length(fitted_zone) > 0L && !is.null(beta_hat)) {
    cal_rows = impute_unit_days(fitted_zone, predict_alpha_units(fitted_zone))
    grid = tryCatch(get_grid_fect(model_fit), error = function(e) NULL)
    if (!is.null(grid) && !("se0" %in% names(grid))) grid$se0 = NA_real_
    if (!is.null(cal_rows) && !is.null(grid)) {
      mt = attr(model_fit, "fect_meta")
      truth = grid |>
        dplyr::filter(
          .data[[mt$treated_col]] %in% TRUE,
          as.character(.data[[mt$unit_col]]) %in% fitted_zone,
          is.finite(.data$.y_fect), is.finite(.data$yhat0)
        ) |>
        dplyr::transmute(
          fullaqsid = as.character(.data[[mt$unit_col]]),
          date      = as.Date(.data[[mt$date_col]]),
          # Native (square) fect effect, consistent with the now-native imputed
          # att: observed^2 - (counterfactual^2 + se0^2); observed is fixed.
          eff_fect  = .data$.y_fect^2 -
                      bt_mean_sq(.data$yhat0, dplyr::coalesce(.data$se0, 0))
        )
      j = cal_rows |>
        dplyr::inner_join(truth, by = c("fullaqsid", "date")) |>
        dplyr::mutate(r = .data$att - .data$eff_fect) |>
        dplyr::filter(is.finite(.data$r))
      if (nrow(j) >= 30L) {
        qs = stats::quantile(j$r, probs = c(0.05, 0.50, 0.95), names = FALSE)
        cal = list(
          n    = nrow(j),
          n_units = dplyr::n_distinct(j$fullaqsid),
          bias = mean(j$r),
          mae  = mean(abs(j$r)),
          q05  = qs[[1]], q50 = qs[[2]], q95 = qs[[3]]
        )
        message(sprintf(
          "[fect.train.impute] calibration (leave-out, %d cells / %d monitors): bias=%+.3f mae=%.3f band=[%+.3f, %+.3f]",
          cal$n, cal$n_units, cal$bias, cal$mae, cal$q05, cal$q95
        ))
      } else {
        message("[fect.train.impute] calibration skipped: <30 leave-out cells")
      }
    }
  }
  ilap("calibration")

  # --- 5. Aggregate to (metro_id, date) — matches fect's per_day_metro shape --
  imputed_agg = imputed_per_monitor |>
    dplyr::group_by(.data$metro_id, .data$date) |>
    dplyr::summarise(
      yhat1   = mean(.data$yhat1, na.rm = TRUE),
      yhat0   = mean(.data$yhat0, na.rm = TRUE),
      att     = mean(.data$att,   na.rm = TRUE),
      # Conservative: mean (not /sqrt(n)) — alpha_pred and M_t errors are
      # shared across same-metro monitors, not independent.
      se_att  = mean(.data$se_att, na.rm = TRUE),
      # D2b variance components (native-scale variances; ADR-0011 §7.2):
      #   varc_shared_md: mean of per-monitor shared-component variance for this
      #     metro-day. Persists across days — alpha/B intercept is one estimate
      #     per monitor; correlated 1.0 across days. Downstream pooling across
      #     days does NOT √n-shrink this.
      #   varc_indep_md:  mean of per-monitor independent-component variance
      #     for this metro-day. Comes from sigma2_cm/n_ctrl; independent across
      #     days. Downstream pooling DOES √n_days-shrink this.
      varc_shared_md = mean(.data$se_att_shared_sq, na.rm = TRUE),
      varc_indep_md  = mean(.data$se_att_indep_sq,  na.rm = TRUE),
      n_units = dplyr::n_distinct(.data$fullaqsid),
      n_ctrl  = mean(.data$n_ctrl, na.rm = TRUE),
      imp_method = paste(sort(unique(.data$imp_method)), collapse = "+"),
      # Same concatenated-label treatment as imp_method: "ctrl", "within", or
      # "ctrl+within" when a metro-day mixes both centerings.
      centering = paste(sort(unique(.data$centering)), collapse = "+"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      day       = .data$date,
      month     = lubridate::floor_date(.data$date, "month"),
      type      = "per_day_metro_imputed",
      fullaqsid = NA_character_,
      yhatse1   = 0,
      yhatse0   = .data$se_att,
      se_att    = dplyr::if_else(is.finite(.data$se_att) & .data$se_att > 0,
                                 .data$se_att, NA_real_),
      t         = .data$att / .data$se_att,
      df        = NA_real_,
      p_value   = 2 * stats::pnorm(-abs(.data$t)),
      stars     = dplyr::case_when(
        !is.finite(.data$p_value) ~ NA_character_,
        .data$p_value < 0.001     ~ "***",
        .data$p_value < 0.01      ~ "**",
        .data$p_value < 0.05      ~ "*",
        TRUE                      ~ ""
      ),
      pct_change = NA_real_,
      n_effects = .data$n_units,
      # Empirical bias band from the leave-out testbed (constant per run):
      # att_corrected = att - cal_bias; band = [att - cal_q95, att - cal_q05].
      cal_bias = if (is.null(cal)) NA_real_ else cal$bias,
      cal_q05  = if (is.null(cal)) NA_real_ else cal$q05,
      cal_q95  = if (is.null(cal)) NA_real_ else cal$q95
    ) |>
    dplyr::select(dplyr::any_of(c(
      "yhat1", "yhatse1", "yhat0", "yhatse0", "att", "se_att", "t", "df",
      "p_value", "stars", "pct_change", "n_effects", "type", "metro_id",
      "month", "fullaqsid", "day", "n_units", "n_ctrl", "imp_method",
      "centering", "cal_bias", "cal_q05", "cal_q95",
      # D2b (ADR-0011 §7.2): native-scale variance components for
      # downstream √n_days shrinkage of the independent part.
      "varc_shared_md", "varc_indep_md"
    )))

  method_label = if (metro_anchor_on && identical(impute_mode, "M7") && !is.null(gap_pred_tbl)) {
    "M7_sat_anchor_plus_hedonic_gap_correction"
  } else if (metro_anchor_on && identical(impute_mode, "M6")) {
    "M6_sat_monthly_level_anchor_metro"
  } else if (metro_anchor_on) {
    "M5_sat_level_anchor_metro"
  } else if (is.null(beta_hat)) {
    "M0_naive_ctrl_mean"
  } else if (identical(impute_mode, "M7") && !is.null(gap_pred_tbl)) {
    "M7_sat_anchor_plus_hedonic_gap_correction"
  } else if (identical(impute_mode, "M6")) {
    "M6_sat_monthly_level_anchor"
  } else if (!is.null(alpha_pred_tbl) && use_sat) {
    "M4c_ctrl_mean_cov_adj_hedonic_alpha_plus_sat_monthly_covariate"
  } else if (!is.null(alpha_pred_tbl)) {
    "M2_ctrl_mean_cov_adj_hedonic_alpha"
  } else {
    "M1_ctrl_mean_cov_adj"
  }
  attr(imputed_agg, "imputation") = list(
    method            = method_label,
    alpha_model_r2    = alpha_model_r2,
    sat_anchor        = use_sat,
    n_dropped_units   = length(dropped_units),
    n_imputed_units   = dplyr::n_distinct(imputed_per_monitor$fullaqsid),
    n_hedonic_train   = if (!is.null(hed)) nrow(hed_train) else NA_integer_,
    sigma2_companion  = sigma2_cm,
    # Leave-out calibration (NULL when no fitted-zone testbed exists)
    cal_n     = if (is.null(cal)) NA_integer_ else cal$n,
    cal_units = if (is.null(cal)) NA_integer_ else cal$n_units,
    cal_bias  = if (is.null(cal)) NA_real_ else cal$bias,
    cal_mae   = if (is.null(cal)) NA_real_ else cal$mae,
    cal_q05   = if (is.null(cal)) NA_real_ else cal$q05,
    cal_q50   = if (is.null(cal)) NA_real_ else cal$q50,
    cal_q95   = if (is.null(cal)) NA_real_ else cal$q95
  )

  # --- 5b. Keep the per-monitor grain ----------------------------------------
  # Step 5 above averages the monitor dimension away. That aggregate is what
  # the pin has always carried, and because `per_day_metro_unit` was therefore
  # absent for anchored metros, downstream readers concluded anchored cities
  # could not support sub-city (borough / corridor / monitor-subset) effects.
  # They can: `imputed_per_monitor` IS the per-(monitor, day) anchored ATT,
  # counterfactual and variance components included. Nothing about the anchor
  # is metro-level — the satellite level and the hedonic gap are both resolved
  # per monitor; only the persistence step was.
  #
  # Emit it alongside, shaped like fect's fitted `per_day_metro_unit` rows so
  # every existing consumer (S4 direct pool, /monitor-set-att-series,
  # combine_att_window_d2b) works on it unchanged. `basis = "anchored"` is what
  # tells them apart, exactly as it does at the per_metro grain.
  imputed_unit = imputed_per_monitor |>
    dplyr::mutate(
      day        = .data$date,
      month      = lubridate::floor_date(.data$date, "month"),
      type       = "per_day_metro_unit_imputed",
      basis      = "anchored",
      yhatse1    = 0,
      yhatse0    = .data$se_att,
      t          = .data$att / .data$se_att,
      df         = NA_real_,
      p_value    = 2 * stats::pnorm(-abs(.data$t)),
      stars      = dplyr::case_when(
        !is.finite(.data$p_value) ~ NA_character_,
        .data$p_value < 0.001     ~ "***",
        .data$p_value < 0.01      ~ "**",
        .data$p_value < 0.05      ~ "*",
        TRUE                      ~ ""
      ),
      pct_change = NA_real_,
      n_effects  = 1L,
      n_units    = 1L,
      # D2b components carry the SAME names the metro-day rows use, so the
      # shared pool function needs no per-type special-casing.
      varc_shared_md = .data$se_att_shared_sq,
      varc_indep_md  = .data$se_att_indep_sq,
      cal_bias = if (is.null(cal)) NA_real_ else cal$bias,
      cal_q05  = if (is.null(cal)) NA_real_ else cal$q05,
      cal_q95  = if (is.null(cal)) NA_real_ else cal$q95
    ) |>
    dplyr::select(dplyr::any_of(c(
      "yhat1", "yhatse1", "yhat0", "yhatse0", "att", "se_att", "t", "df",
      "p_value", "stars", "pct_change", "n_effects", "type", "basis",
      "metro_id", "month", "fullaqsid", "day", "n_units", "n_ctrl",
      "imp_method", "centering", "cal_bias", "cal_q05", "cal_q95",
      "varc_shared_md", "varc_indep_md"
    )))
  attr(imputed_agg, "per_unit") = imputed_unit

  # DESIGN task 010 — Option D (Stage P) needs the SAME hedonic gap predictions
  # M7 just built, to translate the satellite level into monitor units. Rather
  # than refit the gap model, carry it out on attributes.
  #
  # THE GATE (2026-08-03 fix, PR #344 follow-up). These attributes used to be
  # gated on `fect_d1_export_enabled()` ALONE, back when Stage P/F was a
  # diagnostic-only path behind CPPORTAL_FECT_D1_EXPORT. PR #344 made Stage P/F
  # the PRODUCTION anchored counterfactual under CPPORTAL_FECT_ANCHOR_METHOD=M10
  # and taught `fect_emit_d1_rows(force = TRUE)` to bypass the export gate — but
  # left THIS gate alone. The consequence in production (D1_EXPORT unset, the
  # default): no `gap_pred` attribute was ever attached, so the very first thing
  # the forced Stage P/F run does — `attr(imputed, "gap_pred")` — came back NULL
  # and the emitter bailed with "no gap predictions available", d1 = NULL, and
  # the truthful-degrade guard stamped the whole pin M7. That is exactly what
  # happened to pin version 3006 (2026-08-03 16:42): drift_on/shape_on TRUE (they
  # are read from env) while lambda_star = NA and freeze_on = FALSE (they are read
  # from the NULL d1 diagnostics).
  #
  # So the gate is now "the D path is going to run": either the diagnostic export
  # is on, OR the resolved anchor method is M10 and the production path needs
  # these. Under ANCHOR_METHOD=M7 with the export off, nothing is attached and
  # this object is identical to the legacy one — that is still the rollback.
  if (fect_d1_export_enabled() || fect_anchor_is_m10()) {
    attr(imputed_agg, "gap_pred")         = gap_pred_tbl
    attr(imputed_agg, "gap_pred_monthly") = gap_pred_mm_tbl
    attr(imputed_agg, "dropped_units")    = dropped_units
    # DESIGN §13: the shared drift curve c(M) is estimated on the monitors fect
    # DID fit (they are the only ones with a theta_i to residualise against), so
    # the D path needs gap predictions for the FITTED set too. Same gate, same
    # "predict, never refit" rule as the dropped-unit table above.
    attr(imputed_agg, "gap_pred_fitted") = tryCatch(
      # with_se = FALSE: this table feeds compute_shared_drift_curve() only,
      # which reads gap_pred and never gap_pred_se. Skips the B=400 bootstrap.
      predict_gap_units(fitted_units, with_se = FALSE), error = function(e) NULL)
    attr(imputed_agg, "gap_pred_fitted_monthly") =
      if (exists("predict_gap_units_monthly", inherits = FALSE)) {
        tryCatch(predict_gap_units_monthly(fitted_units), error = function(e) NULL)
      } else NULL
  }

  message(sprintf(
    "[fect.train.impute] method=%s produced %d imputed per_day_metro rows across %d metro(s): %s",
    method_label,
    nrow(imputed_agg),
    dplyr::n_distinct(imputed_agg$metro_id),
    paste(sort(unique(as.integer(imputed_agg$metro_id))), collapse = ", ")
  ))
  message(sprintf(
    "[fect.train.impute] + %d per_day_metro_unit_imputed rows across %d monitor(s)",
    nrow(imputed_unit), dplyr::n_distinct(imputed_unit$fullaqsid)
  ))
  imputed_agg
}

# -----------------------------------------------------------------------------
# Robust Huber location (ADR-0012). Mirrors `.huber_location` in the serving
# layer (app/v2/api/R/cross_city_att.R) so that the anchored per-metro ATT is
# computed identically whether at train time (here) or in the serving fallback.
# k=1.345 = standard 95%-efficient tuning. MASS::hubers with an IRLS fallback.
# -----------------------------------------------------------------------------
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
    w  <- ifelse(abs(r) <= k, 1, k / abs(r))
    mn <- sum(w * x) / sum(w)
    if (abs(mn - mu) < 1e-6 * s) { mu <- mn; break }
    mu <- mn
  }
  mu
}

# -----------------------------------------------------------------------------
# ADR-0009: aggregate the anchored per-(metro,day) rows into ONE `per_metro`
# row per anchored metro, stored in the pin alongside the fect-fitted per_metro
# rows (with a `basis` flag). This moves the whole-metro anchored ATT from the
# serving layer INTO the pin, so the pin is the single source of truth. The math
# mirrors `per_metro_overall_att` (Huber point estimate; D2b SE = shared +
# indep/n_days) so served numbers are unchanged; serving becomes a read-through.
# Only emits rows for metros that do NOT already have a fect-fitted per_metro row.
# -----------------------------------------------------------------------------
build_anchored_per_metro = function(att) {
  if (is.null(att) || !("type" %in% names(att))) return(NULL)
  imp = att[att$type %in% "per_day_metro_imputed", , drop = FALSE]
  if (nrow(imp) == 0L) return(NULL)
  fitted_metros = unique(att$metro_id[att$type %in% "per_metro"])
  imp = imp |> dplyr::filter(!(.data$metro_id %in% fitted_metros))
  if (nrow(imp) == 0L) return(NULL)
  imp |>
    dplyr::group_by(.data$metro_id) |>
    dplyr::summarise(
      .att   = .huber_location(.data$att),
      .yhat0 = .huber_location(.data$yhat0),
      .vs    = mean(.data$varc_shared_md, na.rm = TRUE),
      .vi    = mean(.data$varc_indep_md,  na.rm = TRUE),
      .sed   = mean(.data$se_att, na.rm = TRUE),
      .cb    = mean(.data$cal_bias, na.rm = TRUE),
      .imp   = paste(sort(unique(.data$imp_method)), collapse = "+"),
      .n     = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::transmute(
      metro_id   = .data$metro_id,
      att        = .data$.att,
      # D2b SE (matches per_metro_overall_att): shared + independent/n_days,
      # with the same D2a fallback to mean per-day se when components are absent.
      se_att     = dplyr::case_when(
        is.finite(.data$.vs) & is.finite(.data$.vi) ~ sqrt(.data$.vs + .data$.vi / .data$.n),
        is.finite(.data$.sed) & .data$.sed > 0      ~ .data$.sed,
        TRUE                                        ~ NA_real_
      ),
      yhat0      = .data$.yhat0,
      yhat1      = NA_real_, yhatse1 = 0, yhatse0 = .data$se_att,
      t          = .data$att / .data$se_att,
      df         = NA_real_,
      p_value    = NA_real_,   # anchored: no sampling p (consistent with serving)
      stars      = NA_character_,
      pct_change = dplyr::if_else(is.finite(.data$.yhat0) & abs(.data$.yhat0) > 1e-9,
                                  .data$att / .data$.yhat0 * 100, NA_real_),
      n_effects  = .data$.n,
      type       = "per_metro",
      basis      = "anchored",
      month      = as.Date(NA), day = as.Date(NA), fullaqsid = NA_character_,
      n_units    = .data$.n, n_ctrl = NA_real_,
      imp_method = .data$.imp,
      cal_bias   = .data$.cb, cal_q05 = NA_real_, cal_q95 = NA_real_,
      varc_shared_md = .data$.vs, varc_indep_md = .data$.vi
    )
}

# -----------------------------------------------------------------------------
# Satellite intercept anchors for the imputation hedonic (job/sat pipeline,
# source 15 = ACAG SatPM2.5; see SATELLITE_ANCHOR_PLAN.md §3 and appendix
# A.8). One row per monitor: the long-run satellite mean at the monitor
# location — a near-direct measurement of the baseline level the hedonic is
# trying to predict for fect-dropped zone monitors. NULL on any failure or
# when sat_monitors is empty, so callers degrade gracefully to M2.
# -----------------------------------------------------------------------------
fetch_sat_anchors = function(db, spec) {
  out = tryCatch(
    DBI::dbGetQuery(db, "
      SELECT fullaqsid, AVG(value) AS sat_longrun
      FROM public.sat_monitors
      WHERE source_id = 15 AND buffer_km = 0 AND resolution = 'fine'
        AND pollutant = $1
      GROUP BY fullaqsid",
      params = list(as.character(spec$pollutant %||% "PM2.5"))),
    error = function(e) {
      message("[fect.train.sat] anchor fetch failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(out) || nrow(out) == 0L) return(NULL)
  message("[fect.train.sat] satellite anchors for ", nrow(out), " monitors")
  tibble::as_tibble(out)
}

# -----------------------------------------------------------------------------
# Train one bundle (zone-spatial treatment + method dispatch)
# -----------------------------------------------------------------------------

# Per-spec stage timer for bottleneck triage (Tim 2026-06-12: the dry-run took
# ~92 min for one spec with no per-stage visibility). Emits one grep-friendly
# line per stage boundary:
#   [fect.train.timing] spec=<id> stage=<name> stage_s=<since last lap> total_s=<since start>
# Sub-second stages are still logged — the absence of a big number is itself
# diagnostic. Companion fine-grained timers live in functions_fect.R
# ([fect.timing] get_fect fit vs analytic SE) and impute_dropped_zone_atts.
fect_stage_timer = function(spec_id) {
  t0 = Sys.time()
  t_last = t0
  function(stage) {
    now = Sys.time()
    message(sprintf(
      "[fect.train.timing] spec=%s stage=%s stage_s=%.1f total_s=%.1f",
      spec_id, stage,
      as.numeric(difftime(now, t_last, units = "secs")),
      as.numeric(difftime(now, t0, units = "secs"))
    ))
    t_last <<- now
    invisible(NULL)
  }
}

train_fect_bundle = function(db, spec) {
  if (length(spec$metro_ids) < 1L) {
    message("[fect.train] spec=", spec$id, " SKIP (no metro_ids from metro_polygons)")
    return(NULL)
  }
  lap = fect_stage_timer(spec$id)

  panel = fetch_panel(db, spec)
  if (nrow(panel) == 0) {
    message("[fect.train] spec=", spec$id, " SKIP (empty panel)")
    return(NULL)
  }
  lap("fetch_panel")

  # Treated-pairs source depends on treatment_scope (default "zone"):
  #   zone  → monitors inside a zone polygon (ST_Intersects)
  #   metro → every monitor in a metro with an active period (diluted)
  # Both return the identical schema, so add_treatment_zones() / add_groups() /
  # get_fect() downstream are byte-for-byte unchanged. See
  # job/model/HANDOFF_METRO_MODELS.md.
  scope = spec$treatment_scope %||% "zone"
  fetch_fn = if (identical(scope, "metro")) {
    "fetch_metro_treated_pairs"
  } else {
    "fetch_zone_treated_pairs"
  }
  message("[fect.train] spec=", spec$id, " scope=", scope,
          " ", fetch_fn, " system_types=",
          paste(spec$system_types, collapse = ","))
  zone_pairs = if (identical(scope, "metro")) {
    fetch_metro_treated_pairs(db, metro_ids = spec$metro_ids,
                              system_types = spec$system_types)
  } else {
    fetch_zone_treated_pairs(db, metro_ids = spec$metro_ids,
                             system_types = spec$system_types)
  }
  message("[fect.train] spec=", spec$id,
          " treated_pairs n=", nrow(zone_pairs),
          " n_monitors=", dplyr::n_distinct(zone_pairs$fullaqsid),
          " n_policies=", dplyr::n_distinct(zone_pairs$policy_id))

  panel = add_treatment_zones(panel, zone_pairs)

  # Tim ruling 2026-08-08 (uncharged days are UNTREATED), refined 2026-08-09
  # (EITHER/OR across schemes): a treated monitor-day stays treated if ANY
  # covering (zone, period) pair charges that weekday. London's CCZ charges
  # Mon-Fri but its ULEZ charges 7/7, so an in-ULEZ monitor keeps its weekends
  # from 2019-04-08 while a CCZ-only monitor still loses them. This is
  # deliberately TRAIN-LOCAL: `add_treatment_zones()` and the pair fetchers live
  # in the shared job/model/functions.R, which is VENDORED into
  # _bundled/model_functions.R and read by other jobs, so both the flip and the
  # per-policy schedule lookup stay here rather than re-vendoring and changing
  # treatment for every consumer. Rows are kept (only `treated` moves),
  # `days_since_treatment` is untouched. Kill switch:
  # CPPORTAL_FECT_UNTREAT_UNCHARGED=0.
  policy_schedules = fect_policy_schedules(db, zone_pairs$policy_id)
  panel = fect_untreat_uncharged_days(panel, spec_id = spec$id,
                                      zone_pairs = zone_pairs,
                                      policy_schedules = policy_schedules)

  # D_unch covariate (default ON): in-window untreated days AFTER the uncharged
  # flip — days_since_treatment >= 0 & treated == FALSE. Added to X so the fit
  # can separate "cordon off" days from never-treated controls. Counterfactual
  # ATT cells are treated charged days where D_unch == 0 by construction, so
  # yhat0 is already evaluated at D_unch = 0. Kill switch:
  # CPPORTAL_FECT_UNCH_DUMMY=0. Never confuse with episode_flag (scoring only).
  d_unch = fect_add_d_unch(panel, spec)
  panel = d_unch$panel
  spec  = d_unch$spec

  panel = add_groups(panel)

  # Plan (b) — metro one-hot dummies as time-invariant Z covariates. Each
  # monitor belongs to exactly one metro for life, so metro indicators are
  # time-invariant per unit and belong in Z (not X). We one-hot encode the
  # set of metros that actually appear in the fetched panel, drop one as
  # reference, and append the dummy column names to spec$z_covariates so
  # get_fect() picks them up. Gated by CPPORTAL_FECT_METRO_AS_Z=1 so Plan (a)
  # remains the default until this is validated.
  metro_as_z = tolower(Sys.getenv("CPPORTAL_FECT_METRO_AS_Z", "")) %in%
    c("1", "true", "t", "yes", "y")
  if (metro_as_z && identical(spec$outcome_kind, "aq")) {
    unique_metros = sort(unique(as.integer(panel$metro_id)))
    if (length(unique_metros) >= 2L) {
      ref_metro = unique_metros[[1L]]
      dummy_metros = unique_metros[-1L]
      metro_dummy_cols = paste0("metro_dummy_", dummy_metros)
      for (m in dummy_metros) {
        panel[[paste0("metro_dummy_", m)]] = as.integer(panel$metro_id == m)
      }
      spec$z_covariates = unique(c(spec$z_covariates, metro_dummy_cols))
      message("[fect.train] Plan b: added ", length(metro_dummy_cols),
              " metro dummies to z_covariates (ref_metro=", ref_metro, ")")
    } else {
      message("[fect.train] Plan b: only ",
              length(unique_metros),
              " metro(s) in panel — skipping metro dummies (need >=2).")
    }
  }

  # Plan (c) — metro × year one-hot dummies as time-varying X covariates.
  # Unlike metro-only dummies (which are constant within unit and collinear
  # with the unit FE — see Plan b), metro × year dummies vary within unit
  # because a monitor's metro_year indicator flips when the year changes.
  # They allow metro-specific year shocks beyond the global time FE.
  # Drop one (metro, year) pair as reference to avoid the trivial dummy-trap.
  # Gated by CPPORTAL_FECT_METRO_YEAR_AS_X=1.
  metro_year_as_x = tolower(Sys.getenv("CPPORTAL_FECT_METRO_YEAR_AS_X", "")) %in%
    c("1", "true", "t", "yes", "y")
  if (metro_year_as_x && identical(spec$outcome_kind, "aq")) {
    panel$.year = lubridate::year(panel$date)
    my_pairs = panel |>
      dplyr::distinct(metro_id, .year) |>
      dplyr::arrange(metro_id, .year)
    if (nrow(my_pairs) >= 2L) {
      ref_pair = my_pairs[1L, , drop = FALSE]
      keep_pairs = my_pairs[-1L, , drop = FALSE]
      my_dummy_cols = paste0("my_", keep_pairs$metro_id, "_", keep_pairs$.year)
      for (i in seq_len(nrow(keep_pairs))) {
        m = keep_pairs$metro_id[[i]]
        y = keep_pairs$.year[[i]]
        col = paste0("my_", m, "_", y)
        panel[[col]] = as.integer(panel$metro_id == m & panel$.year == y)
      }
      # Append to covariates formula (X, time-varying), not Z.
      rhs_existing = if (is.null(spec$covariates)) character(0) else
        all.vars(spec$covariates)
      rhs_new = unique(c(rhs_existing, my_dummy_cols))
      spec$covariates = stats::as.formula(
        paste0("~ ", paste(rhs_new, collapse = " + "))
      )
      message("[fect.train] Plan c: added ", length(my_dummy_cols),
              " metro-year dummies to covariates X (ref=metro ",
              ref_pair$metro_id, " year ", ref_pair$.year, ").")
    } else {
      message("[fect.train] Plan c: only ", nrow(my_pairs),
              " (metro,year) pair(s) in panel — skipping.")
    }
    panel$.year = NULL
  }

  # Any duplicate here was introduced by the zone join, NOT by partition_key:
  # fetch_panel() already collapsed the panel to one row per (fullaqsid, date),
  # so every row of a monitor-day now shares the same partition. Routed through
  # the same helper anyway so there is exactly one dedupe rule in this file.
  panel = fect_dedupe_unit_time(panel, .tag = "fect.train",
                                .what = "dedupe after add_treatment_zones")
  lap("zone_pairs_join_dedupe")

  outcome_col   = spec$outcome_var
  is_treated    = panel$treated %in% TRUE
  outcome_obs   = !is.na(panel[[outcome_col]])
  n_treated     = sum(is_treated, na.rm = TRUE)
  n_control     = sum(!is_treated, na.rm = TRUE)
  n_treated_obs = sum(is_treated & outcome_obs, na.rm = TRUE)
  n_control_obs = sum(!is_treated & outcome_obs, na.rm = TRUE)
  min_treated_obs = fect_min_treated_obs(spec)
  n_zone_pairs  = nrow(zone_pairs)

  message(
    "[fect.train] spec=", spec$id,
    " n_treated=", n_treated,
    " n_treated_obs=", n_treated_obs,
    " n_control=", n_control,
    " n_control_obs=", n_control_obs
  )

  # Placeholders are for TRAFFIC specs only: TomTom is still backfilling cordon
  # monitors, so a traffic spec may legitimately have no treated observations
  # for months. AQ specs always have data, so any AQ shortfall is a real bug —
  # surface it as a stop() so the job's per-spec error handler in job.R skips
  # the spec and the summary shows it as "skipped (no bundle returned)" rather
  # than writing a pin that claims to be a real model but isn't.
  fail_or_placeholder = function(reason) {
    if (identical(spec$outcome_kind, "aq")) {
      stop("[fect.train] spec=", spec$id, " AQ FIT FAILED: ", reason,
           call. = FALSE)
    }
    message("[fect.train] spec=", spec$id, " PLACEHOLDER ", reason)
    make_placeholder_bundle(spec, panel,
                            n_treated, n_control,
                            n_treated_obs, n_zone_pairs, reason)
  }

  if (n_treated_obs < min_treated_obs) {
    return(fail_or_placeholder(sprintf(
      "n_treated_obs=%d < min=%d (treated rows=%d, zone_pairs=%d) -- awaiting upstream observations",
      n_treated_obs, min_treated_obs, n_treated, n_zone_pairs
    )))
  }
  if (n_control_obs < min_treated_obs) {
    return(fail_or_placeholder(sprintf(
      "n_control_obs=%d < min=%d (control rows=%d) -- awaiting upstream observations",
      n_control_obs, min_treated_obs, n_control
    )))
  }

  # -----------------------------------------------------------------------------
  # Covariate-only collinearity diagnostic. Runs on the SAME in-memory panel
  # that fect is about to fit on, so no second DB pull. Reports per-covariate
  # VIF, max VIF excluding any time-invariant Z covariates (those are absorbed
  # by the unit FE and report VIF = NA mechanically), and max VIF across all
  # covariates. Stored on the bundle so write_bundle() can stamp it into the
  # pin metadata. See appendix.md A.5 for the procedure and the baseline run.
  cov_terms = unique(c(
    if (is.null(spec$covariates)) character(0) else all.vars(spec$covariates),
    if (is.null(spec$z_covariates)) character(0) else as.character(spec$z_covariates)
  ))
  vif_tbl = compute_panel_vif(
    panel,
    covariates = cov_terms,
    unit_col = "fullaqsid",
    date_col = "date"
  )
  if (!is.null(vif_tbl)) {
    z_cov_set = as.character(spec$z_covariates %||% character(0))
    non_absorbed = vif_tbl |> dplyr::filter(!isTRUE(.data$absorbed_by_fe))
    non_z_non_absorbed = non_absorbed |>
      dplyr::filter(!(.data$covariate %in% z_cov_set))
    vif_max_excl_z   = if (nrow(non_z_non_absorbed) > 0L)
      max(non_z_non_absorbed$vif, na.rm = TRUE) else NA_real_
    vif_max_with_all = if (nrow(non_absorbed) > 0L)
      max(non_absorbed$vif, na.rm = TRUE) else NA_real_
    message(sprintf(
      "[fect.train.vif] spec=%s max_vif_excl_z=%.3f max_vif_all=%.3f n_covariates=%d elapsed=%.2fs",
      spec$id,
      vif_max_excl_z, vif_max_with_all, nrow(vif_tbl),
      sum(vif_tbl$elapsed_sec)
    ))
  } else {
    vif_max_excl_z   = NA_real_
    vif_max_with_all = NA_real_
  }
  lap("vif")

  message("[fect.train] spec=", spec$id,
          " get_fect method=", spec$fect_method,
          " outcome=", deparse(spec$outcome)[1],
          " n_treated_obs=", n_treated_obs,
          " n_control_obs=", n_control_obs)

  # MC needs NA outcomes preserved (matrix completion treats them as missing
  # entries to impute); CFE drops them.
  na_rm_for_method = identical(spec$fect_method, "cfe")

  # Debug switch: when CPPORTAL_FECT_DEBUG_DIRECT_FIT=1, bypass purrr::possibly
  # so the underlying fect::fect error propagates and shows up in the rendered
  # report instead of being silently swallowed into a placeholder bundle.
  # Use with CPPORTAL_FECT_DRY_RUN=1 + CPPORTAL_FECT_SPECS=<one spec> to avoid
  # overwriting good pins.
  debug_direct_fit = tolower(Sys.getenv("CPPORTAL_FECT_DEBUG_DIRECT_FIT", "")) %in%
    c("1", "true", "t", "yes", "y")
  if (debug_direct_fit) {
    message("[fect.train] DEBUG_DIRECT_FIT=1 — bypassing purrr::possibly")
  }
  fit_safe = if (debug_direct_fit) get_fect else purrr::possibly(get_fect, otherwise = NULL, quiet = FALSE)
  # Plan (a) — drop the metro-index path. metro_id is unit-invariant (each
  # monitor belongs to one metro), so adding it as an extra additive FE on
  # top of the unit FE is mathematically redundant and was silently failing
  # on every spec we inspected (Mat::elem / inv_sympd), with the two-way
  # fallback doing all the actual work. Forcing index2 makes that explicit.
  # Investigate metro-as-Z and metro-year-as-X under separate experiments
  # (Plans b/c in appendix A.4 follow-ups).
  # Convergence-tolerance knobs (2026-08-03 retrain-cost experiment). Unset =
  # the fect package defaults (tol=1e-3, max.iteration=1000 — get_fect passes
  # neither). PRODUCTION SETS CPPORTAL_FECT_TOL=0.003; it is not running the
  # package default.
  #
  # THE "~96% of train wall time" CLAIM THAT USED TO BE HERE IS DEAD. That was
  # measured before the tolerance change. Re-measured 2026-08-04 on the real
  # nightly run (Connect job lMjfs27P4NsJX3bU, 22 specs, 7.99 h): the EM fit is
  # 5.6% of wall time. `impute` is 69.4% and `qis_aggregates` is 24.1%. tol is
  # NOT the lever for the retrain-cost question any more — driving the fit to
  # zero would save ~6%. See docs/model/TIMING.md before optimizing anything.
  #
  # get_fect() forwards `...` into fect::fect(), so these thread through
  # without a signature change.
  .fect_tol   = suppressWarnings(as.numeric(Sys.getenv("CPPORTAL_FECT_TOL", "")))
  .fect_maxit = suppressWarnings(as.integer(Sys.getenv("CPPORTAL_FECT_MAX_ITERATION", "")))
  .fect_extra = list()
  if (is.finite(.fect_tol) && .fect_tol > 0) {
    .fect_extra$tol = .fect_tol
    message("[fect.train] CPPORTAL_FECT_TOL override: tol=", .fect_tol)
  }
  if (is.finite(.fect_maxit) && .fect_maxit > 0) {
    .fect_extra$max.iteration = .fect_maxit
    message("[fect.train] CPPORTAL_FECT_MAX_ITERATION override: ", .fect_maxit)
  }

  # ---- per-treated-unit eligibility tally (PRE-FIT) -------------------------
  # fect requires every treated unit to have at least min.T0 = 5 UNTREATED
  # periods, counted AFTER listwise deletion on the outcome + covariate
  # columns. A unit that falls short is dropped before fitting and simply never
  # appears in `att` — which, after the fact, is indistinguishable from a unit
  # that had no panel rows in the first place. Tally it HERE, while the panel
  # still exists, so the two are separable.
  #
  # A unit can therefore be dropped for two very different reasons:
  #   * it genuinely has < 5 pre-treatment days, or
  #   * it has plenty of pre-treatment days but one covariate column is NA on
  #     most of them, so complete.cases() eats them.
  # Only the second is fixable upstream, hence the per-column NA counts.
  #
  # Cheap by construction: one complete.cases() pass plus table() counts. No
  # joins, no DB round-trips.
  eligibility_tbl = tryCatch({
    elig_cols = intersect(fect_spec_panel_cols(spec), names(panel))
    uid = as.character(panel$fullaqsid)
    trt = panel$treated %in% TRUE
    tr_units = sort(unique(uid[trt]))
    if (!length(tr_units) || !length(elig_cols)) {
      NULL
    } else {
      MIN_T0 = 5L
      cc = stats::complete.cases(panel[, elig_cols, drop = FALSE])
      lv = function(x) as.integer(table(factor(x, levels = tr_units)))
      in_tr = uid %in% tr_units
      n_panel = lv(uid[in_tr])
      n_complete = lv(uid[in_tr & cc])
      n_untr_complete = lv(uid[in_tr & cc & !trt])

      # Per-column NA counts, but ONLY for the units actually at risk — this is
      # the expensive branch and it should stay proportional to the failures.
      at_risk = n_untr_complete < MIN_T0
      blocking = rep(NA_character_, length(tr_units))
      for (i in which(at_risk)) {
        idx = (uid == tr_units[[i]]) & !trt
        if (!any(idx)) {
          blocking[[i]] = "(no untreated rows at all)"
          next
        }
        nas = vapply(elig_cols, function(cl) sum(is.na(panel[[cl]][idx])),
                     integer(1))
        nas = nas[nas > 0L]
        blocking[[i]] = if (!length(nas)) {
          "(none - untreated rows are complete, just too few)"
        } else {
          nas = sort(nas, decreasing = TRUE)
          paste(sprintf("%s(%d)", names(nas), as.integer(nas)), collapse = ",")
        }
      }

      tibble::tibble(
        spec_id                 = as.character(spec$id),
        fullaqsid               = tr_units,
        panel_rows              = n_panel,
        complete_rows           = n_complete,
        untreated_complete_rows = n_untr_complete,
        min_t0                  = MIN_T0,
        at_risk_of_drop         = at_risk,
        blocking_cols           = blocking
      )
    }
  }, error = function(e) {
    message("[fect.train.eligibility] tally failed (non-fatal): ",
            conditionMessage(e))
    NULL
  })

  if (!is.null(eligibility_tbl)) {
    .elig_fail = eligibility_tbl[eligibility_tbl$at_risk_of_drop, , drop = FALSE]
    message("[fect.train.eligibility] spec=", spec$id,
            " treated_units=", nrow(eligibility_tbl),
            " at_risk_of_drop=", nrow(.elig_fail),
            " min_t0=5 elig_cols=", length(intersect(fect_spec_panel_cols(spec),
                                                     names(panel))))
    .elig_cap = 50L
    for (i in seq_len(min(nrow(.elig_fail), .elig_cap))) {
      message("[fect.train.eligibility] unit=", .elig_fail$fullaqsid[[i]],
              " panel_rows=", .elig_fail$panel_rows[[i]],
              " complete=", .elig_fail$complete_rows[[i]],
              " untreated_complete=", .elig_fail$untreated_complete_rows[[i]],
              " blocking_cols=", .elig_fail$blocking_cols[[i]])
    }
    if (nrow(.elig_fail) > .elig_cap) {
      message("[fect.train.eligibility] ... and ",
              nrow(.elig_fail) - .elig_cap, " more at-risk unit(s) ",
              "(full table on bundle$diagnostics$eligibility)")
    }
  }

  m = do.call(fit_safe, c(list(
    data = panel,
    treated_col = "treated",
    date_col = "date",
    unit_col = "fullaqsid",
    metro_col = "metro_id",
    outcome = spec$outcome,
    covariates = spec$covariates,
    z_covariates = spec$z_covariates,
    method = spec$fect_method,
    na.rm = na_rm_for_method,
    se = TRUE,
    vartype = "analytic",
    parallel = TRUE,
    cores = 8L,
    use_metro_index = FALSE
  ), .fect_extra))

  lap("get_fect_total")  # fit vs analytic-SE split logged inside via [fect.timing]

  if (is.null(m)) {
    return(fail_or_placeholder("get_fect returned NULL (model fit failed)"))
  }

  # Z-covariate fallback provenance (see get_fect() Z singular-matrix fallback).
  # `dist_included` records, per model, whether the distance-to-road Z covariate
  # actually entered the fit — it always should, UNLESS including it made the
  # cfe solver singular, in which case get_fect() dropped it and fit without.
  fmeta        = attr(m, "fect_meta")
  z_requested  = as.character(fmeta$z_covariate_vars %||% character(0))
  z_dropped    = as.character(fmeta$z_dropped_singular %||% character(0))
  # Derive "used" as requested-minus-dropped. Do NOT read z_covariate_vars_used
  # through `%||%`: it is length-0-safe, so an intentionally-empty vector (Z
  # fully dropped) would fall back to z_requested and wrongly flag Z as kept.
  z_used       = setdiff(z_requested, z_dropped)
  dist_included = any(grepl("^dist_km", z_used))
  if (length(z_dropped)) {
    message("[fect.train] spec=", spec$id, " Z DROPPED (singular matrix): [",
            paste(z_dropped, collapse = ", "),
            "] — model fit WITHOUT the distance covariate for this spec.")
  }

  message("[fect.train] spec=", spec$id, " get_gof_fect / summarize_fect_tests")
  gof_tbl = tryCatch(
    get_gof_fect(m, rmse_on = "control"),
    error = function(e) {
      message("[fect.train] get_gof_fect failed: ", conditionMessage(e))
      NULL
    }
  )
  tests_tbl = tryCatch(
    summarize_fect_tests(m),
    error = function(e) {
      message("[fect.train] summarize_fect_tests failed: ", conditionMessage(e))
      NULL
    }
  )
  diagnostics = list(
    gof = if (!is.null(gof_tbl)) {
      as.list(as.data.frame(gof_tbl)[1L, , drop = FALSE])
    } else {
      list()
    },
    fect_tests = if (!is.null(tests_tbl)) {
      as.list(as.data.frame(tests_tbl)[1L, , drop = FALSE])
    } else {
      list()
    }
  )

  lap("gof_tests")

  message(
    "[fect.train] spec=", spec$id,
    " get_qis_fect (overall + per_metro + per_metro_month + per_day_metro",
    " + per_monitor + per_day_metro_unit)"
  )
  qis_safe = purrr::possibly(get_qis_fect, otherwise = NULL, quiet = FALSE)
  # per_day_metro_unit (metro x monitor x day) is the per-monitor-day grain the
  # e_itz / e_itm / e_it_adj pooling protocol needs (see
  # job/model/HANDOFF_METRO_MODELS.md). It is persisted for BOTH zone and metro
  # specs so the harmonized super-pin can pool them. ~panel-sized but cheap in
  # numeric columns; the heavy fect `model` object is what gets dropped later.
  att = qis_safe(
    m,
    data = panel,
    start = spec$date_from,
    end = spec$date_to,
    aggregates = c("overall", "per_metro", "per_metro_month",
                   "per_day_metro", "per_monitor", "per_day_metro_unit")
  )
  if (is.null(att)) {
    att = tibble::tibble()
    message("[fect.train] spec=", spec$id, " get_qis_fect returned NULL; att = empty tibble")
  }
  lap("qis_aggregates")

  # Plan B — impute ATTs for zone monitors fect dropped because they have
  # zero untreated periods in the panel (Stockholm/Milan/Singapore today).
  # Disable by exporting CPPORTAL_FECT_DISABLE_IMPUTE=1 — useful if a future
  # estimator-side change makes the imputation unnecessary.
  disable_impute = tolower(Sys.getenv("CPPORTAL_FECT_DISABLE_IMPUTE", "")) %in%
    c("1", "true", "t", "yes", "y")
  # Impute ATTs for dropped monitors in BOTH scopes. impute_dropped_zone_atts()
  # is scope-aware: ZONE uses same-metro out-of-zone controls; METRO pools the
  # untreated metros for M_t (every in-metro monitor is treated) and lets the
  # hedonic alpha-model transport each dropped monitor's level. This recovers
  # London/Stockholm/Milan/Singapore at metro scope (they have no pre-policy
  # data, so fect drops them — same situation the zone imputation already solves).
  if (!disable_impute && identical(spec$outcome_kind, "aq")) {
    # Optional satellite anchor for the imputation hedonic (M4a). Default OFF;
    # enable with CPPORTAL_FECT_SAT_ANCHOR=1 once the A.8 incl-vs-excl
    # validation supports it for the outcome family in question.
    sat_anchor_on = tolower(Sys.getenv("CPPORTAL_FECT_SAT_ANCHOR", "")) %in%
      c("1", "true", "t", "yes", "y")
    sat_anchors = if (sat_anchor_on) fetch_sat_anchors(db, spec) else NULL
    # Option A: in metro scope the hedonic gap model must be trained on
    # out-of-CORDON-ZONE monitors (not out-of-metro), so it keeps each anchored
    # city's own out-of-zone monitors and can estimate that city's satellite-gap
    # FE. Fetch the true in-zone set here (the metro `zone_pairs` is the whole
    # metro) and pass it so the imputer excludes only in-zone monitors.
    zone_monitor_ids = NULL
    if (identical(spec$treatment_scope %||% "zone", "metro")) {
      zmp = tryCatch(
        fetch_zone_treated_pairs(db, metro_ids = spec$metro_ids,
                                 system_types = spec$system_types),
        error = function(e) {
          message("[fect.train.impute] Option A: fetch_zone_treated_pairs failed: ",
                  conditionMessage(e)); NULL
        })
      if (!is.null(zmp)) zone_monitor_ids = unique(as.character(zmp$fullaqsid))
      message("[fect.train.impute] Option A: metro scope excludes ",
              length(zone_monitor_ids), " in-zone monitor(s) from the gap model")
    }
    imputed = tryCatch(
      impute_dropped_zone_atts(m, panel, zone_pairs, spec,
                               sat_anchors = sat_anchors,
                               zone_monitor_ids = zone_monitor_ids),
      error = function(e) {
        message("[fect.train.impute] error during imputation: ",
                conditionMessage(e))
        NULL
      }
    )
    lap("impute_legacy_m7_rows")
    if (!is.null(imputed) && nrow(imputed) > 0L) {
      imputation_diag = attr(imputed, "imputation")
      imputed_unit = attr(imputed, "per_unit")
      # ---- M10 PRODUCTION ANCHORED PATH (2026-08-03) ----------------------
      # Under CPPORTAL_FECT_ANCHOR_METHOD = M10 (the default) the anchored
      # counterfactual is the Stage P/F construction, NOT the contemporaneous
      # M7 anchor computed above. `imputed` is still required — it supplies the
      # gap model (attr "gap_pred") and the dropped-unit set that Stage P/F
      # consumes — but its ROWS are replaced by the promoted Stage P/F rows.
      # Under M7 (the rollback) nothing here runs and the legacy rows are bound
      # exactly as before, bit-for-bit.
      anchor_method_used = fect_anchor_method()
      d1 = tryCatch(
        fect_emit_d1_rows(m, panel, spec, db, imputed = imputed,
                          zone_pairs = zone_pairs,
                          zone_monitor_ids = zone_monitor_ids,
                          force = fect_anchor_is_m10()),
        error = function(e) {
          message("[fect.train.d1] error building Option D rows: ",
                  conditionMessage(e))
          NULL
        }
      )
      lap("d1_emit_stage_pf")
      d1_diag = if (is.null(d1)) NULL else d1$diagnostics
      promoted = if (fect_anchor_is_m10()) fect_promote_d1_to_production(d1) else NULL
      lap("d1_promote")
      if (fect_anchor_is_m10() && is.null(promoted)) {
        # TRUTHFUL DEGRADE: Stage P/F could not run for this fit. Serve the
        # legacy rows — but they are M7 arithmetic and are stamped M7 (the
        # closure above already does that), and the diagnostics say so. Never
        # ship an M10 label over M7 math again.
        message("[fect.train.impute] !! ANCHOR_METHOD=M10 but Stage P/F produced ",
                "no rows — FALLING BACK to the legacy M7 contemporaneous anchor. ",
                "Rows are stamped M7 and diagnostics$imputation$method reports M7.")
        anchor_method_used = "M7"
      }
      # ---- "London is NOT ANCHORED." (Tim, 2026-08-09) --------------------
      # A metro with a FITTED per_metro contrast is a fitted city, full stop.
      # Its handful of unfittable in-zone monitors must NOT re-enter the study
      # through the anchored door at ANY grain.
      #
      # `build_anchored_per_metro()` has always gated the per_metro row exactly
      # this way (`fitted_metros` / `!(metro_id %in% fitted_metros)` above), but
      # the per-DAY and per-MONITOR grains were never gated — they follow
      # `dropped_units = setdiff(zone_units, fitted_units)`, which is a MONITOR
      # set, so any fitted metro with even one unfittable monitor still emitted
      # `basis="anchored"` rows. On the 2026-08-09 pin that meant London (943)
      # shipped 2,113 per_day_metro_imputed + 19,621 per_day_metro_unit_imputed
      # anchored rows while ALSO serving a fitted per_metro row (+0.0647), and
      # Bergen (958) shipped 3,247 + 3,247 the same way. Those rows are served
      # by /monitor-att-series and /monitor-set-att-series, so a caller asking
      # for London monitors got anchored rows mixed into a fitted city.
      #
      # The gate is computed BEFORE any anchored row is bound, so `att` here
      # holds only the fect-fitted output — the same basis on which
      # build_anchored_per_metro() makes the identical decision downstream.
      fitted_metros_gate = unique(att$metro_id[att$type %in% "per_metro"])
      gate_anchored = function(df, what) {
        if (is.null(df) || nrow(df) == 0L) return(df)
        keep = !(df$metro_id %in% fitted_metros_gate)
        if (!all(keep)) {
          message("[fect.train.impute] fitted-metro gate: dropped ", sum(!keep),
                  " ", what, " row(s) for metro(s) ",
                  paste(sort(unique(df$metro_id[!keep])), collapse = ","),
                  " — they serve a FITTED per_metro row, so they are not ",
                  "anchored cities at any grain")
        }
        df[keep, , drop = FALSE]
      }
      if (!is.null(promoted)) {
        att = dplyr::bind_rows(att, gate_anchored(promoted$per_metro_day,
                                                  "per_day_metro_imputed"))
        message("[fect.train.impute] M10: added ", nrow(promoted$per_metro_day),
                " per_day_metro_imputed row(s) from the Stage P/F construction ",
                "(trailing lambda*, freeze, donor-drift) — the legacy M7 rows ",
                "were NOT emitted")
        if (!is.null(promoted$per_unit)) {
          att = dplyr::bind_rows(att, gate_anchored(promoted$per_unit,
                                                    "per_day_metro_unit_imputed"))
          message("[fect.train.impute] M10: added ", nrow(promoted$per_unit),
                  " per_day_metro_unit_imputed row(s) across ",
                  dplyr::n_distinct(promoted$per_unit$fullaqsid), " monitor(s)")
        }
      } else {
        att = dplyr::bind_rows(att, gate_anchored(imputed, "per_day_metro_imputed"))
        # Per-monitor anchored rows (step 5b): the sub-city grain. Without these
        # the pin can only answer "what happened to this metro", never "which
        # part of it", for every anchored city.
        if (!is.null(imputed_unit) && nrow(imputed_unit) > 0L) {
          att = dplyr::bind_rows(att, gate_anchored(imputed_unit,
                                                    "per_day_metro_unit_imputed"))
          message("[fect.train.impute] added ", nrow(imputed_unit),
                  " per_day_metro_unit_imputed row(s) across ",
                  dplyr::n_distinct(imputed_unit$fullaqsid), " monitor(s)")
        }
      }
      # ---- provenance: label MUST equal the math that ran ------------------
      if (is.null(imputation_diag)) imputation_diag = list()
      imputation_diag$anchor_method   = anchor_method_used
      imputation_diag$method          = if (identical(anchor_method_used, "M10"))
        FECT_M10_IMP_METHOD else (imputation_diag$method %||% FECT_M7_IMP_METHOD)
      imputation_diag$legacy_m7_method = attr(imputed, "imputation")$method %||% NA_character_
      imputation_diag$anchor_construction = if (identical(anchor_method_used, "M10"))
        "stage_p_theta_star_lambda + stage_f_counterfactual (promoted)" else
        "contemporaneous sqrt(sat_monthly_mean) + gap_pred + cov_adj"
      imputation_diag$lambda_star     = d1_diag$lambda_months %||% NA_real_
      imputation_diag$window_align    = d1_diag$window_align %||% NA_character_
      imputation_diag$shape_on        = fect_anchor_component_on("CPPORTAL_FECT_M9_SHAPE")
      imputation_diag$freeze_on       = isTRUE(d1_diag$m9_freeze)
      imputation_diag$freeze_active   = isTRUE(d1_diag$freeze_active)
      imputation_diag$freeze_month    = d1_diag$theta_star_freeze_month %||% NA
      imputation_diag$tf_basis        = d1_diag$tf_basis %||% NA_character_
      imputation_diag$tf_arm          = d1_diag$tf_arm %||% NA_character_
      imputation_diag$freeze_window_months = d1_diag$freeze_window_months %||% NA_real_
      imputation_diag$drift_on        = fect_anchor_component_on("CPPORTAL_FECT_M9_FREEZE_DRIFT")
      imputation_diag$drift           = d1_diag$drift %||% NULL
      imputation_diag$n_rows_metro_day = if (!is.null(promoted))
        nrow(promoted$per_metro_day) else nrow(imputed)
      imputation_diag$n_rows_unit     = if (!is.null(promoted))
        nrow(promoted$per_unit %||% data.frame()) else
        nrow(imputed_unit %||% data.frame())
      # ADR-0009: also fold the anchored per-(metro,day) rows into ONE per_metro
      # row per anchored metro (Huber + D2b SE), so the whole-metro anchored ATT
      # lives IN THE PIN next to the fect-fitted per_metro rows. `basis` tags the
      # two apart; serving reads them through instead of re-aggregating.
      if (!("basis" %in% names(att))) att$basis = NA_character_
      att$basis[att$type %in% "per_metro" & is.na(att$basis)] = "fitted"
      anchored_pm = tryCatch(build_anchored_per_metro(att),
                             error = function(e) {
                               message("[fect.train.impute] build_anchored_per_metro failed: ",
                                       conditionMessage(e)); NULL
                             })
      if (!is.null(anchored_pm) && nrow(anchored_pm) > 0L) {
        att = dplyr::bind_rows(att, anchored_pm)
        message("[fect.train.impute] added ", nrow(anchored_pm),
                " anchored per_metro row(s) (ADR-0009): metro(s) ",
                paste(anchored_pm$metro_id, collapse = ", "))
      }
      message("[fect.train.impute] att now has ", nrow(att),
              " rows (incl. ", nrow(imputed), " imputed per_day_metro_imputed rows)")

      # DESIGN task 010 — Option D "Stage P/F" rows under their DIAGNOSTIC type
      # names (`per_day_metro_unit_imputed_d1` / `per_day_metro_imputed_d1`).
      # These are additive and env-gated (CPPORTAL_FECT_D1_EXPORT, default OFF).
      # When M10 already PROMOTED the same rows onto the production types above,
      # emitting them again here would double-count, so it is skipped.
      if (is.null(promoted) && !is.null(d1) && fect_d1_export_enabled()) {
        if (!is.null(d1$per_unit) && nrow(d1$per_unit) > 0L) {
          att = dplyr::bind_rows(att, d1$per_unit)
        }
        if (!is.null(d1$per_metro_day) && nrow(d1$per_metro_day) > 0L) {
          att = dplyr::bind_rows(att, d1$per_metro_day)
        }
        message("[fect.train.d1] att now has ", nrow(att), " rows (incl. ",
                nrow(d1$per_unit %||% data.frame()), " + ",
                nrow(d1$per_metro_day %||% data.frame()), " D1 rows)")
      }
    } else {
      imputation_diag = NULL
      d1_diag = NULL
    }
  } else {
    imputation_diag = NULL
    d1_diag = NULL
  }
  lap("impute")

  # Episode scoring exclusion (default ON): stamp episode_flag on day-grain ATT
  # rows (rel99_x2 + +/-1d dilate) and rebuild fitted overall/per_metro/
  # per_metro_month (+ anchored per_metro) on charged+non-episode cells.
  # Episode is NEVER a train covariate. Kill switch:
  # CPPORTAL_FECT_EPISODE_EXCLUDE=0. Serving drops episode_flag==TRUE before
  # combine_att_window_d2b when the column is present (fail-open if absent).
  if (identical(spec$outcome_kind, "aq") && nrow(att) > 0L &&
      exists("fect_episode_postprocess_att", mode = "function")) {
    ep_outcome = if ("aq_daily_mean" %in% names(panel)) {
      "aq_daily_mean"
    } else {
      spec$outcome_var
    }
    att = tryCatch(
      fect_episode_postprocess_att(att, panel, outcome_col = ep_outcome),
      error = function(e) {
        message("[fect.train.episode] postprocess failed: ",
                conditionMessage(e), " — leaving att unchanged")
        att
      }
    )
    lap("episode_postprocess")
  }

  # Pre-fit treated-unit eligibility (computed just before get_fect). Lets a
  # consumer tell "fect dropped this unit for < 5 untreated periods" apart from
  # "this unit had no panel rows", which is not recoverable from `att` alone.
  # NULL assigns nothing (`list$x = NULL` removes the element).
  diagnostics$eligibility = eligibility_tbl
  diagnostics$covariate_vif = vif_tbl
  diagnostics$covariate_vif_max_excl_z = vif_max_excl_z
  diagnostics$covariate_vif_max_with_all = vif_max_with_all
  diagnostics$imputation = imputation_diag
  # DESIGN task 010 — Option D provenance. NULL (the gate-off case) assigns
  # nothing: `list$x = NULL` removes the element, so `diagnostics` is identical
  # to today's whenever CPPORTAL_FECT_D1_EXPORT is off.
  diagnostics$d1_projection = d1_diag
  # Z-covariate inclusion provenance (distance-to-road siting control).
  diagnostics$z_covariates_requested = z_requested
  diagnostics$z_covariates_used      = z_used
  diagnostics$z_dropped_singular     = z_dropped
  diagnostics$dist_included          = dist_included

  bundle_out = list(
    spec = spec,
    status = "fitted",
    trained_at = Sys.time(),
    panel_stats = fect_panel_summary(panel, spec, n_treated, n_control,
                                     n_treated_obs, n_zone_pairs),
    diagnostics = diagnostics,
    gof_tbl = gof_tbl,
    tests_tbl = tests_tbl,
    vif_tbl = vif_tbl,
    model = m,
    att = att,
    model_family = "fect",
    ui_metadata = list(
      model_family = "fect",
      model_type = "synth",
      group_label = spec$label %||% spec$id,
      model_label = spec$name %||% spec$id,
      description = spec$description %||% "",
      outcome_var = spec$outcome_var,
      outcome_kind = spec$outcome_kind,
      treatment_scope = spec$treatment_scope %||% "zone",
      fect_method = spec$fect_method,
      system_types_csv = paste(spec$system_types, collapse = ","),
      control_metro_ids = sort(unique(as.integer(spec$metro_ids))),
      n_metros = dplyr::n_distinct(panel$metro_id),
      n_monitors = dplyr::n_distinct(panel$fullaqsid),
      status = "fitted"
    )
  )

  # SPEC §4.7 (job/model/fect/transfer/SPEC.md) / ADR-0025 — additive,
  # env-gated, fail-soft factor-model export for the GeoAI factor-transfer
  # project. Does not touch bundle_out's existing fields; only appends
  # `factor_model` when it computes successfully, so with the guard off (or
  # on any failure) the bundle is byte-identical to today.
  factor_export_on = tolower(Sys.getenv("CPPORTAL_FECT_FACTOR_EXPORT", "1")) %in%
    c("1", "true", "t", "yes", "y")
  if (factor_export_on) {
    factor_model = tryCatch(
      compute_factor_model(m, panel, spec),
      error = function(e) {
        message("[fect.train.factor_model] spec=", spec$id,
                " compute_factor_model failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(factor_model)) {
      bundle_out$factor_model = factor_model
    }
  }

  # DESIGN §7.6 task 008 (docs/model/DESIGN_sat_anchor_fect_integration.md) —
  # additive, env-gated, fail-soft export of the FROZEN model components
  # (theta_unit / tau_time / beta / sigma_eps / mu) that Option D's satellite
  # projection needs. Same contract as the factor_model block above: computed
  # while the live fit `m` still exists (ADR-0002 drops bundle$model before
  # pin_write), appended only on success, so with the guard off — or on any
  # failure — the bundle is byte-identical to today.
  frozen_export_on = tolower(Sys.getenv("CPPORTAL_FECT_FROZEN_EXPORT", "1")) %in%
    c("1", "true", "t", "yes", "y")
  if (frozen_export_on) {
    frozen_components = tryCatch(
      compute_frozen_components(m, panel, spec),
      error = function(e) {
        message("[fect.train.frozen] spec=", spec$id,
                " compute_frozen_components failed: ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(frozen_components)) {
      bundle_out$frozen_components = frozen_components
    }
  }

  bundle_out
}

# -----------------------------------------------------------------------------
# SPEC §4.7 (job/model/fect/transfer/SPEC.md) / ADR-0025: post-fit factor-model
# export for the GeoAI factor-transfer project (job/model/fect/transfer/).
# Purely additive — derives F/Lambda from the EXISTING fect fit `m` returned by
# get_fect(); no second fect() call, no change to `m` or `att`. Called from
# train_fect_bundle() above, guarded by CPPORTAL_FECT_FACTOR_EXPORT and wrapped
# in tryCatch at the call site, so any failure here degrades to "no
# factor_model element" and never affects production training.
#
# Residual source: cfe fits expose `m$res`, a T x N matrix equal to
# `Y.dat - Y.ct` on control (untreated) cells with NA on treated&post-period
# cells (verified empirically against a synthetic method="cfe" fit mirroring
# get_fect()'s call shape — see
# job/model/fect/transfer/tests/test_factor_export.R). That is exactly the
# "observed minus counterfactual on untreated cells" residual T1/T2 of the
# transfer SPEC need. Falls back to Y.dat - Y.ct directly if `res` is ever
# absent from the fit object.
compute_factor_model = function(m, panel, spec, r_max = 8L, var_target = 0.60,
                                 max_bytes = 5e6) {
  fail = function(reason) {
    message("[fect.train.factor_model] spec=", spec$id %||% "?",
            " skip: ", reason)
    NULL
  }

  if (is.null(m) || !is.list(m)) return(fail("no fit object"))

  resid_mat = m$res %||% m$residuals
  if (is.null(resid_mat) && !is.null(m$Y.dat) && !is.null(m$Y.ct)) {
    resid_mat = m$Y.dat - m$Y.ct
  }
  if (is.null(resid_mat) || !is.matrix(resid_mat)) {
    return(fail("no residual matrix on fit object (res/residuals/Y.dat-Y.ct all absent)"))
  }

  unit_ids = as.character(m$id)
  time_ids = as.character(m$rawtime)
  if (length(unit_ids) != ncol(resid_mat) || length(time_ids) != nrow(resid_mat)) {
    return(fail("id/rawtime dims do not match residual matrix"))
  }
  dimnames(resid_mat) = list(time_ids, unit_ids)

  # Drop units/periods that are entirely missing (e.g. an always-treated unit
  # has no control-period residual at all) -- nothing to demean or impute there.
  keep_col = colSums(!is.na(resid_mat)) > 0L
  keep_row = rowSums(!is.na(resid_mat)) > 0L
  if (sum(keep_col) < 2L || sum(keep_row) < 2L) {
    return(fail("fewer than 2 usable units/periods after dropping all-NA"))
  }
  resid_mat = resid_mat[keep_row, keep_col, drop = FALSE]
  unit_ids  = unit_ids[keep_col]
  time_ids  = time_ids[keep_row]

  # Column-demean (unit means become `alpha`).
  alpha = colMeans(resid_mat, na.rm = TRUE)
  demeaned = sweep(resid_mat, 2, alpha, "-")

  n_t = nrow(demeaned); n_n = ncol(demeaned)
  r_use = max(1L, min(r_max, n_t - 1L, n_n - 1L))

  # Soft-impute: init missing cells at 0, iterate rank-r_use truncated SVD,
  # refill missing cells from the low-rank reconstruction (observed cells held
  # fixed), stop at <=25 iterations or relative Frobenius change < 1e-4.
  na_mask = is.na(demeaned)
  X = demeaned
  X[na_mask] = 0
  prev = X
  for (iter in seq_len(25L)) {
    sv = tryCatch(svd(X, nu = r_use, nv = r_use), error = function(e) NULL)
    if (is.null(sv)) return(fail(paste0("svd failed during soft-impute (iter=", iter, ")")))
    recon = sv$u %*% (sv$d[seq_len(r_use)] * t(sv$v))
    if (any(na_mask)) X[na_mask] = recon[na_mask]
    rel_change = norm(X - prev, type = "F") / (norm(prev, type = "F") + 1e-12)
    prev = X
    if (rel_change < 1e-4) break
  }

  # Final decomposition on the completed, demeaned matrix.
  sv = tryCatch(svd(X, nu = r_use, nv = r_use), error = function(e) NULL)
  if (is.null(sv) || length(sv$d) == 0L) return(fail("final svd failed or degenerate"))

  d = sv$d[seq_len(r_use)]
  total_var = sum(d^2)
  if (!is.finite(total_var) || total_var <= 0) {
    return(fail("degenerate (zero-variance) residual matrix"))
  }
  cumvar = cumsum(d^2) / total_var
  r = which(cumvar >= var_target)[1]
  if (is.na(r)) r = r_use
  r = max(2L, min(r, r_use))

  F_mat = sv$u[, seq_len(r), drop = FALSE] %*% diag(d[seq_len(r)], nrow = r)
  Lambda_mat = sv$v[, seq_len(r), drop = FALSE]
  rownames(F_mat) = time_ids
  rownames(Lambda_mat) = unit_ids
  colnames(F_mat) = paste0("f", seq_len(r))
  colnames(Lambda_mat) = paste0("f", seq_len(r))

  out = list(
    F = F_mat,
    Lambda = Lambda_mat,
    alpha = alpha,
    r = r,
    var_explained = cumvar[r],
    method = "postfit_svd_softimpute",
    computed_at = Sys.time()
  )

  sz = as.numeric(object.size(out))
  if (sz > max_bytes) {
    warning("[fect.train.factor_model] spec=", spec$id %||% "?",
            " factor_model exceeds max_bytes (", sz, " > ", max_bytes, "); dropping")
    return(NULL)
  }

  message("[fect.train.factor_model] spec=", spec$id %||% "?",
          " r=", r, " var_explained=", round(cumvar[r], 4),
          " dims F=", nrow(F_mat), "x", ncol(F_mat),
          " Lambda=", nrow(Lambda_mat), "x", ncol(Lambda_mat),
          " bytes=", sz)
  out
}

# -----------------------------------------------------------------------------
# DESIGN §7.6 task 008 / §7.7 (docs/model/DESIGN_sat_anchor_fect_integration.md):
# export the FROZEN components of the production fect fit as small tables, so
# Option D ("satellite projection through the frozen fect fit") can reassemble a
# counterfactual for a monitor that was never in the fit — WITHOUT pinning the
# multi-GB model object (ADR-0002 drops `bundle$model` before pin_write).
#
# Purely additive and read-only with respect to `m`: no second fect() call, no
# mutation of `m`, `att`, or any existing bundle field. Called from
# train_fect_bundle() above under CPPORTAL_FECT_FROZEN_EXPORT (default ON) and
# wrapped in tryCatch at the call site, so any failure here degrades to "no
# frozen_components element" and never affects production training.
#
# WHAT THE PRODUCTION FIT ACTUALLY IS (verified 2026-07-31 against fect 2.1.0
# and the live call in train_fect_bundle(): `use_metro_index = FALSE`,
# `method = "cfe"`, `force = "two-way"`):
#
#     y_it = mu + theta_i + tau_t + X_it'beta + eps_it
#
# and the fitted/counterfactual matrix satisfies that identity EXACTLY
# (max |mu + alpha_i + xi_t + X'beta - Y.ct| ~ 4e-16 on a synthetic fit; see
# job/model/fect/train/tests/test_frozen_components.R). `alpha` and `xi` are
# both mean-zero, so `mu` carries the grand level and MUST travel with them —
# hence it is exported alongside, even though §7.6's table lists only the four
# headline objects.
#
# SLOT MAP (fect 2.1.0, method="cfe", force="two-way"):
#   m$alpha   N x 1 matrix, unit FE          -> frozen_components$theta_unit$theta
#   m$xi      T x 1 matrix, time FE          -> frozen_components$tau_time$tau
#   m$beta    p x 1 matrix, covariate coefs  -> frozen_components$beta
#   m$mu      scalar grand intercept         -> frozen_components$mu
#   m$sigma2  residual variance (= m$sigma2.fect for the r = 0 cfe path)
#                                            -> frozen_components$sigma_eps = sqrt(.)
#   m$id      character unit ids aligned with rows of m$alpha
#   m$rawtime integer time ids aligned with rows of m$xi (see date mapping below)
#
# NAMING HAZARD: do NOT call any of this "alpha". `bundle$factor_model$alpha`
# (compute_factor_model() above, ADR-0025) is a residual-SVD COLUMN MEAN — a
# completely different object from the fect unit FE `m$alpha`. The frozen export
# therefore uses `theta_unit` / `tau_time` / `beta` / `sigma_eps` / `mu`.
#
# METRO FE — resolved, nothing to export. §7.7(4) left "which slot carries c_m"
# open. Answer: no slot, because the production fit has no metro FE to carry.
#   1. train_fect_bundle() calls get_fect(..., use_metro_index = FALSE), so
#      fect's `index` is 2-element and its `X.extra.FE` array is empty. The
#      metro-index path was dropped deliberately (see the "Plan (a)" comment at
#      the fit site) precisely because metro_id is unit-invariant.
#   2. Even if the 3rd index were restored, fect requires the extra FE to be
#      constant within unit ("A unit in different periods should have the same
#      group index"), so it is always nested in — and absorbed by — the unit FE
#      under force="two-way". Empirically a 3-index fit and a 2-index fit return
#      a bit-identical `alpha` (max abs diff 0), and `complex_fe_ub()` returns
#      no extra-FE coefficient slot at all.
# So the metro level lives INSIDE theta_unit$theta, which is exactly what Stage
# P needs (it projects theta_i for a dropped monitor, metro level included).
# `meta$metro_fe` records this rather than exporting an empty object.
compute_frozen_components = function(m, panel, spec,
                                     date_col = "date",
                                     unit_col = "fullaqsid",
                                     metro_col = "metro_id",
                                     max_bytes = 5e6) {
  fail = function(reason) {
    message("[fect.train.frozen] spec=", spec$id %||% "?", " skip: ", reason)
    NULL
  }

  if (is.null(m) || !is.list(m)) return(fail("no fit object"))

  # --- unit level (theta_i) --------------------------------------------------
  if (is.null(m$alpha)) return(fail("fit has no `alpha` slot (unit FE)"))
  theta_vec = as.numeric(m$alpha)
  unit_ids  = as.character(m$id)
  if (length(unit_ids) != length(theta_vec)) {
    return(fail(paste0("length(m$id)=", length(unit_ids),
                       " != length(m$alpha)=", length(theta_vec))))
  }
  theta_unit = tibble::tibble(unit_id = unit_ids, theta = theta_vec)
  names(theta_unit)[1] = unit_col

  # metro_id is a property of the monitor; attach it for convenience (Stage P
  # groups by metro). Unit-invariant by construction, so a unique() is safe.
  if (!is.null(panel) && all(c(unit_col, metro_col) %in% names(panel))) {
    unit_metro = as.data.frame(panel)[, c(unit_col, metro_col), drop = FALSE]
    unit_metro[[unit_col]] = as.character(unit_metro[[unit_col]])
    unit_metro = unique(unit_metro)
    if (nrow(unit_metro) == length(unique(unit_metro[[unit_col]]))) {
      theta_unit = dplyr::left_join(theta_unit, tibble::as_tibble(unit_metro),
                                    by = unit_col)
    } else {
      message("[fect.train.frozen] spec=", spec$id %||% "?",
              " metro_id is not unique per unit; omitting it from theta_unit")
    }
  }

  # --- time effects (tau_t) --------------------------------------------------
  if (is.null(m$xi)) return(fail("fit has no `xi` slot (time FE)"))
  tau_vec  = as.numeric(m$xi)
  time_ids = as.integer(m$rawtime)
  if (length(time_ids) != length(tau_vec)) {
    return(fail(paste0("length(m$rawtime)=", length(time_ids),
                       " != length(m$xi)=", length(tau_vec))))
  }
  # `time_id` is prepare_data_fect()'s 1-based index into the SORTED unique
  # character dates of the panel handed to get_fect() — reproduce that mapping
  # exactly so tau_time carries real calendar dates (Stage P needs monthly means
  # of tau). If the mapping does not line up, keep the ids and drop the dates
  # rather than emitting wrong ones.
  date_levels = if (!is.null(panel) && date_col %in% names(panel)) {
    sort(unique(as.character(panel[[date_col]])))
  } else {
    character(0)
  }
  date_map_ok = length(date_levels) > 0L &&
    all(is.finite(time_ids)) &&
    min(time_ids) >= 1L && max(time_ids) <= length(date_levels)
  tau_time = tibble::tibble(
    time_id = time_ids,
    date    = if (date_map_ok) as.Date(date_levels[time_ids]) else as.Date(NA),
    tau     = tau_vec
  )
  if (!date_map_ok) {
    message("[fect.train.frozen] spec=", spec$id %||% "?",
            " could not map time_id -> date (levels=", length(date_levels),
            "); tau_time$date left NA")
  }

  # --- covariate coefficients (beta) ----------------------------------------
  # Rownames come from fect; fall back to m$X (the covariate name vector) when
  # the matrix is unnamed. `D` (the treatment) is NOT in beta.
  beta_tbl = NULL
  if (!is.null(m$beta) && length(m$beta) > 0L) {
    beta_vec = as.numeric(m$beta)
    beta_nm  = rownames(m$beta)
    if (is.null(beta_nm) || length(beta_nm) != length(beta_vec)) {
      beta_nm = as.character(m$X %||% character(0))
    }
    if (length(beta_nm) != length(beta_vec)) {
      beta_nm = paste0("beta", seq_along(beta_vec))
      message("[fect.train.frozen] spec=", spec$id %||% "?",
              " could not name beta coefficients; using positional names")
    }
    beta_tbl = tibble::tibble(covariate = as.character(beta_nm), beta = beta_vec)
  }

  # --- residual scale --------------------------------------------------------
  # `sigma2` is the residual variance of the selected fit; `sigma2.fect` is the
  # same quantity from the r = 0 ("fect") fit. For the production cfe path they
  # coincide; record both so a future divergence is visible in the pin instead
  # of silently changing the exported sigma.
  sigma2      = suppressWarnings(as.numeric(m$sigma2      %||% NA_real_))[1]
  sigma2_fect = suppressWarnings(as.numeric(m$sigma2.fect %||% NA_real_))[1]
  if (!is.finite(sigma2) || sigma2 < 0) return(fail("m$sigma2 missing or not a non-negative scalar"))
  sigma_eps = sqrt(sigma2)

  out = list(
    theta_unit = theta_unit,
    tau_time   = tau_time,
    beta       = beta_tbl,
    sigma_eps  = sigma_eps,
    mu         = suppressWarnings(as.numeric(m$mu %||% NA_real_))[1],
    meta = list(
      exported_at = Sys.time(),
      spec_id     = spec$id %||% NA_character_,
      n_units     = nrow(theta_unit),
      n_dates     = nrow(tau_time),
      fect_method = as.character(m$method %||% NA_character_),
      force       = as.character(m$force  %||% NA_character_),
      # Which slot sigma_eps came from, and the sibling value for comparison.
      sigma2_source        = "sigma2",
      sigma2               = sigma2,
      sigma2_fect          = sigma2_fect,
      sigma2_equals_fect   = isTRUE(all.equal(sigma2, sigma2_fect)),
      date_map_ok = date_map_ok,
      # See the METRO FE block in the header comment above.
      metro_fe    = "absorbed_into_theta_unit",
      reconstruction = "y0_it = mu + theta_i + tau_t + X_it'beta",
      naming_note = "theta_unit$theta is fect's m$alpha (unit FE); unrelated to factor_model$alpha (residual-SVD column mean)",
      fect_version = tryCatch(as.character(utils::packageVersion("fect")),
                              error = function(e) NA_character_)
    )
  )

  sz = as.numeric(utils::object.size(out))
  if (sz > max_bytes) {
    warning("[fect.train.frozen] spec=", spec$id %||% "?",
            " frozen_components exceeds max_bytes (", sz, " > ", max_bytes, "); dropping")
    return(NULL)
  }

  message("[fect.train.frozen] spec=", spec$id %||% "?",
          " theta_unit=", nrow(theta_unit), " unit(s)",
          " tau_time=", nrow(tau_time), " date(s)",
          " beta=", if (is.null(beta_tbl)) 0L else nrow(beta_tbl), " coef(s)",
          " sigma_eps=", signif(sigma_eps, 5),
          " mu=", signif(out$mu, 5),
          " bytes=", sz)
  out
}

# =============================================================================
# OPTION D — Stage P / Stage F: "satellite projection through the FROZEN fect
# fit". Contract task 2026-07-31-010; spec in
# docs/model/DESIGN_sat_anchor_fect_integration.md §7 (as CORRECTED by §7.7),
# §10 (Tim's GO + tau backcast) and §11 (Tim: v1 runs WITHOUT weather).
#
# WHY THIS LIVES IN train/functions.R AND NOT IN A NEW FILE
# --------------------------------------------------------
# A sibling `stage_pf.R` would read more cleanly, but every file in
# job/model/fect/train/ is a Posit Connect bundle member whose checksum lives in
# manifest.json, which is only regenerated by `Rscript job/model/fect/train/setup.R`
# (rsconnect::writeManifest). A new source file that is not in the manifest is
# silently NOT uploaded, and the deployed job then fails at source() time in
# production only. train/functions.R is already a manifest member and is already
# where the sibling post-fit exports live (compute_factor_model, ADR-0025;
# compute_frozen_components, task 008), so Stage P/F goes here. If this block is
# ever split out, setup.R MUST be re-run in the same PR.
#
# WHAT THE M7 (shipped) AND D1 (this) COUNTERFACTUALS ARE, SIDE BY SIDE
# ---------------------------------------------------------------------
# For a DROPPED monitor i (zero untreated rows -> fect never fits theta_i):
#
#   M7 :  sqrt(yhat0_it) = sqrt(sat_i,M(t)) + gap_pred_i,M + cov_adj_it
#         level = satellite tile + hedonic siting-gap PREDICTION; the shared
#         time structure and beta come from a COMPANION feols fit, not fect.
#
#   D1 :  sqrt(yhat0_it) = mu + theta*_i + tau_t + (X_it - Xbar_i^obs)'beta
#         level theta*_i is ESTIMATED by projecting the monitor's own monthly
#         satellite series through the FROZEN fect structure (Stage P); mu,
#         tau_t, beta and sigma_eps all come from THE fect fit (task 008's
#         frozen_components). The gap model survives only as the satellite ->
#         monitor unit translation inside Stage P.
#
# That is the whole point of Option D: D1 is fect's own counterfactual with one
# unidentified parameter (the unit level) supplied from data the monitor really
# has, instead of a second estimator pasted on beside fect.
#
# STAGE P (monthly, per dropped monitor) — level-only per §7.7(2)
#   z_i,M = sqrt(sat_i,M) + gap_pred_i,M - mu - tau_bar_M   ~=   theta_i + u_i,M
#   theta*_i = robust (Huber) intercept of z_i,. ; SE = lag-truncated HAC.
#   NO unit seasonality is taken from the harmonic fit: the production model
#   (§7.7(1)) has NO month x unit or dow x unit CFE terms, so a FITTED monitor
#   gets no unit seasonality either, and giving a dropped monitor one would
#   re-introduce exactly the incohesion this design exists to remove. The
#   2-harmonic fit is kept ONLY as mis-fit diagnostic columns.
#   NO weather term (§11, Tim): W_bar'beta_W is omitted and its contribution is
#   absorbed into theta*_i, with Stage F entering covariates deviation-only so
#   nothing is double counted. The size of that omission is measured, not
#   assumed — see job/model/fect/diagnostics/run_d1_falsification.R (iii).
#
# STAGE F (daily, treated days only — the monitor reports AQ, so the covariate
# row exists)
#   sqrt(yhat0_it) = mu + theta*_i + tau_t + (X_it - Xbar_i^obs)'beta
#   varc_shared_md = bt( Var(theta*_i) + gap_pred_se_i^2 )   [day-persistent]
#   varc_indep_md  = bt( sigma_eps^2 )                       [day-independent]
#   back-transform via the same closed-form square rules as M7.
#
# EVERYTHING HERE IS ADDITIVE AND GATED. `CPPORTAL_FECT_D1_EXPORT` defaults OFF
# (promotion flips it); with the gate off nothing in this block runs, no
# attribute is attached anywhere, and training is byte-identical to today.
# =============================================================================

#' Is the Option D (D1) export enabled? Default OFF — promotion flips it.
#' DESIGN §7.5(3): D rows land BESIDE the M7 rows under DISTINCT type values, so
#' no consumer filtering today's types can double count M7 + D1 before Tim
#' promotes.
fect_d1_export_enabled = function() {
  tolower(Sys.getenv("CPPORTAL_FECT_D1_EXPORT", "0")) %in%
    c("1", "true", "t", "yes", "y")
}

# Row `type` values and the imp_method label. Distinct from the M7 types by
# construction (consumer-safety rule, DESIGN §11 closing paragraph).
FECT_D1_TYPE_UNIT      = "per_day_metro_unit_imputed_d1"
FECT_D1_TYPE_METRO_DAY = "per_day_metro_imputed_d1"
FECT_D1_IMP_METHOD     = "D1_sat_projected_fect"

# DEFAULT lambda for the §13 window family, in MONTHS of HALF-WIDTH (Inf = the
# static D1 level §12 falsified; 0 = the strictly contemporaneous month, i.e.
# M7's level rule / the D2-C-prime endpoint).
#
# Set from the PER-DAY twin sweep on the local all_priority/aq_daily_mean bundle,
# 2026-07-31 (diagnostics/run_d1_falsification.R --lambda-sweep; 546,905 fitted
# monitor-days). RMSE(lambda), c(M) on:
#     lambda:   0      1      6      12     24     60     Inf
#     RMSE:   0.4004 0.3016 0.2189 0.2095 0.2064 0.2054 0.2342
#     (M7 benchmark on the same monitor-days: 0.4629)
# The curve is U-shaped with a FLAT basin: the argmin is lambda = 60, but 24 is
# +0.5% and 12 is +2.0% — statistically indistinguishable. The default is
# therefore the MID-BASIN 24 months, not the literal argmin: at equal measured
# accuracy the shorter window carries less anchor staleness, which is the whole
# reason §13 made the level time-local, and it keeps the estimator honest at
# monitors whose gap relationship moves faster than the pooled c(M) curve.
# Moving between 12, 24 and 60 is a one-constant decision for Tim; moving to 0 or
# Inf is not — those are the two ends the sweep rejected. The whole D1/D2 path
# remains gated OFF by CPPORTAL_FECT_D1_EXPORT regardless.
FECT_D1_DEFAULT_LAMBDA_MONTHS = 24

# WINDOW ALIGNMENT for the same family. "trailing" (the default) makes the level
# EXOGENOUS: the window is [month(t) - lambda, month(t)], i.e. lambda months of
# lookback PLUS the current month, and never a month after the one being
# predicted. "centered" is the legacy |M - month(t)| <= lambda rule that produced
# the DESIGN section 14/16 numbers and is kept only so those remain reproducible.
#
# Tim's directive, 2026-07-31: "just make the window end with the current month
# ... it doesn't make sense to predict an event based on the future rather than
# the past." A centered window lets months AFTER the prediction month set the
# anchor level, which is a look-ahead an event-study estimate cannot defend --
# post-treatment months would be allowed to inform the counterfactual level of an
# earlier treated month. Trailing removes that channel by construction.
#
# NOTE the two alignments differ at lambda = Inf: centered Inf is ONE static
# level per unit over every month (the section 12 D1 level), while trailing Inf
# is an EXPANDING window (all months up to and including month(t)), which is a
# genuinely different, still-exogenous estimator.
#
# FECT_D1_DEFAULT_LAMBDA_MONTHS is deliberately UNCHANGED here: 24 came from the
# CENTERED sweep, and the trailing re-sweep (run separately) decides whether it
# needs revisiting.
FECT_D1_WINDOW_ALIGN = "trailing"

# ---------------------------------------------------------------------------
# ANCHOR METHOD SELECTOR (DESIGN section 37) -- M10 is the FINAL anchored model
# (Tim's ruling, 2026-08-03). One knob, CPPORTAL_FECT_ANCHOR_METHOD:
#
#   "M10"  (DEFAULT) the final anchored counterfactual:
#            trailing D(lambda* = 24) satellite anchor
#          + monotone shape constraint on the gap model's distance term,
#            INCLUDING the constrained monthly refinement (commit 7d4c808)
#          + siting_class FE (already the default gap covariate)
#          + anchor window frozen at T_f (tf_table.csv)
#          + donor-drift correction theta*(M) = frozen level - c_hat(M),
#            c_hat donor-only (commit 8ca840c; the sign SUBTRACTS).
#          Anchored rows are stamped FECT_M10_IMP_METHOD.
#
#   "M7"   LEGACY / DEPRECATED. The pre-M10 anchored path: no shape, no freeze,
#          no drift correction, rows stamped M7_sat_anchor_plus_hedonic_gap_
#          correction. Kept ONLY as the rollback switch -- M7 is condemned on
#          evidence (it is not exogenous; DESIGN section 37). Setting this
#          reproduces the pre-M10 numbers bit-for-bit.
#
# Note what is NOT in here: the charged-days ("excluding inactive days") cut is
# NOT part of the anchored estimator. It is the UNIVERSAL POOLING ESTIMAND
# applied to every method's rows -- fitted fect rows included -- at the pooling
# / serving stage (see fect_charged_days_only_filter() and its callers). It is
# first-class, not an arm flag.
#
# DIAGNOSTIC ESCAPE HATCH: the three underlying env flags remain individually
# usable. An EXPLICITLY SET CPPORTAL_FECT_M9_SHAPE / _M9_FREEZE /
# _M9_FREEZE_DRIFT always WINS over the anchor-method default, in either
# direction, so the decomposition arms (shape-only, freeze-only, ...) still run
# unchanged. The anchor method only supplies the DEFAULT for an unset flag.
# ---------------------------------------------------------------------------
FECT_ANCHOR_METHOD_DEFAULT = "M10"
FECT_ANCHOR_METHODS_KNOWN  = c("M10", "M7")

#' Resolved anchor method: "M10" (default) or "M7" (legacy).
#' Unknown values fall back to the default with a loud message.
fect_anchor_method = function() {
  v = toupper(trimws(Sys.getenv("CPPORTAL_FECT_ANCHOR_METHOD", "")))
  if (!nzchar(v)) return(FECT_ANCHOR_METHOD_DEFAULT)
  if (!(v %in% FECT_ANCHOR_METHODS_KNOWN)) {
    message("[fect.train.anchor] unknown CPPORTAL_FECT_ANCHOR_METHOD='", v,
            "'; falling back to ", FECT_ANCHOR_METHOD_DEFAULT,
            " (known: ", paste(FECT_ANCHOR_METHODS_KNOWN, collapse = "/"), ")")
    return(FECT_ANCHOR_METHOD_DEFAULT)
  }
  v
}

#' Is the resolved anchor method M10?
fect_anchor_is_m10 = function() identical(fect_anchor_method(), "M10")

#' TRUE when `env_name` is set to a non-empty value (i.e. the operator has an
#' explicit opinion that must beat the anchor-method default, in EITHER
#' direction -- `=0` is an explicit OFF, not "unset").
fect_env_flag_set = function(env_name) {
  nzchar(trimws(Sys.getenv(env_name, "")))
}

#' Truthiness of an env flag, ignoring whether it was set at all.
fect_env_flag_true = function(env_name) {
  tolower(trimws(Sys.getenv(env_name, ""))) %in%
    c("1", "true", "t", "yes", "y", "on")
}

#' Resolve one of the three M10 component flags. Explicit env wins; otherwise
#' the flag is ON iff the anchor method is M10.
fect_anchor_component_on = function(env_name) {
  if (fect_env_flag_set(env_name)) return(fect_env_flag_true(env_name))
  fect_anchor_is_m10()
}

# imp_method stamp for M10 anchored rows. Follows the M7 naming pattern
# (M7_sat_anchor_plus_hedonic_gap_correction) so the label alone says which
# estimator produced the cell. `basis` tagging (fitted / anchored) is UNCHANGED
# by the anchor method -- these rows stay basis = "anchored".
FECT_M10_IMP_METHOD = "M10_dlambda_shape_freeze_driftcorr"
FECT_M7_IMP_METHOD  = "M7_sat_anchor_plus_hedonic_gap_correction"

#' The imp_method label the anchored branch should stamp under the resolved
#' anchor method. NOTE: this is the label for the PROMOTED rows (Stage P/F under
#' M10); the legacy contemporaneous closure inside `impute_dropped_zone_atts()`
#' always stamps `FECT_M7_IMP_METHOD` because that is the math it computes.
fect_anchor_imp_method = function() {
  if (fect_anchor_is_m10()) FECT_M10_IMP_METHOD else FECT_M7_IMP_METHOD
}

#' Promote Stage P/F (Option D) rows to the PRODUCTION anchored types.
#'
#' Under `CPPORTAL_FECT_ANCHOR_METHOD = M10` the anchored counterfactual IS the
#' Stage P/F construction — trailing D(lambda* = 24) satellite anchor, monotone
#' shape gap model + siting FE, anchor frozen at T_f, donor-drift c_hat(M)
#' SUBTRACTED (commit 8ca840c). `fect_emit_d1_rows()` already computes exactly
#' that; before this function existed those rows only ever landed under the
#' diagnostic `*_d1` type names while production served M7 arithmetic wearing an
#' M10 label. This re-types them onto the production anchored types and stamps
#' `FECT_M10_IMP_METHOD`, so every downstream consumer (build_anchored_per_metro,
#' the /att + /window pools, Table 2) reads M10 numbers.
#'
#' @param d1 `fect_emit_d1_rows()` return value.
#' @return list(per_unit, per_metro_day) on the production types, or NULL.
fect_promote_d1_to_production = function(d1) {
  if (is.null(d1)) return(NULL)
  retype = function(df, type_new) {
    if (is.null(df) || nrow(df) == 0L) return(NULL)
    df$type = type_new
    df$imp_method = FECT_M10_IMP_METHOD
    df$basis = "anchored"
    df
  }
  pu = retype(d1$per_unit, "per_day_metro_unit_imputed")
  pm = retype(d1$per_metro_day, "per_day_metro_imputed")
  if (is.null(pm)) return(NULL)   # no metro-day rows => nothing to serve
  list(per_unit = pu, per_metro_day = pm)
}

# ---------------------------------------------------------------------------
# M9-FREEZE (DESIGN section 33c) — the anchor window may not cross T_f.
# ---------------------------------------------------------------------------
# (M9-F)  theta*_i(t) = Huber_k { z_iM - c(M) :
#                                 min(month(t), T_f) - w <= M <= min(month(t), T_f) }
#
# i.e. the trailing lambda = 24 rule is UNCHANGED for month(t) <= T_f and the
# level is FROZEN at theta*_i(T_f; w) for every day after. T_f is the last month
# fully inside the untreated regime (implementation basis: month(S_m) - 1).
#
# WHY. After a cordon starts charging, the satellite tile over an in-cordon
# monitor absorbs part of the treatment effect, so any window that reaches past
# T_f prices the counterfactual off partly-treated satellite months — the level
# eats a fraction omega of its own effect. Freezing removes that term by
# construction. It is NECESSARY, NOT SUFFICIENT (section 33c.5): the gate is a
# JOINT freeze+shape gate and the freeze-only arm is a decomposition
# diagnostic that must never be promoted alone.
#
# w = lambda = 24 (a 25-month window), TRAILING, UNIFORM ACROSS CITIES, so the
# freeze introduces ZERO tuned parameters. Trailing-24-at-T_f beat an expanding
# [first clean month, T_f] window on bounded worst case (NYC RMSE 0.070 vs
# 0.167); expanding is the single named sensitivity (--freeze-window expanding).
#
# Never-treated monitors, the twin pool and every G2 quantity have T_f = +Inf
# and are untouched. DEFAULT OFF: without CPPORTAL_FECT_M9_FREEZE=1 nothing in
# this block changes a single number.
FECT_M9_FREEZE_WINDOW_MONTHS = 24

# Which arm of FECT_D1_TF_TABLE to read. "primary" is the registered headline
# (Milan = pre-Ecopass 2007-12 per Tim's 2026-07-31 override); "sensitivity"
# picks the registered alternate where one exists (Milan Area C, 2011-12) and
# falls back to the primary row for every other metro.
FECT_M9_TF_ARM_DEFAULT = "primary"

#' Is M9-FREEZE on? An explicitly set CPPORTAL_FECT_M9_FREEZE wins
#' (=1/true/yes/on -> ON, anything else -> OFF); when the var is UNSET the
#' anchor method decides -- ON under M10 (the default), OFF under legacy M7.
fect_m9_freeze_enabled = function() {
  fect_anchor_component_on("CPPORTAL_FECT_M9_FREEZE")
}

# T_f registration, CHECKED-IN AS DATA (closed-world rule, section 33c.1 / M9-6).
# Values are DB-verified implementation dates from
# public.congestion_pricing_periods; the loader never queries the DB, so a run is
# reproducible from the bundle alone.
.FECT_M9_TF_TABLE_CACHE = new.env(parent = emptyenv())

#' Read `tf_table.csv` (the T_f registration) from the bundle or the repo.
#'
#' @return tibble(metro_id, metro_name, arm, tf_basis, freeze_month (Date),
#'   freezable) or NULL when the file is missing (freeze then degrades to a
#'   no-op with a message rather than erroring).
fect_m9_tf_table = function(path = NULL) {
  key = path %||% ".default"
  if (!is.null(.FECT_M9_TF_TABLE_CACHE[[key]])) return(.FECT_M9_TF_TABLE_CACHE[[key]])
  candidates = if (!is.null(path)) path else c(
    "tf_table.csv",                        # Connect: cwd = bundle root (train/)
    "job/model/fect/train/tf_table.csv",   # local: cwd = repo root
    "../train/tf_table.csv"                # diagnostics/ callers
  )
  p = candidates[file.exists(candidates)]
  if (!length(p)) {
    message("[fect.train.m9.freeze] tf_table.csv not found; no metro is freezable")
    return(NULL)
  }
  tb = tryCatch(utils::read.csv(p[1], stringsAsFactors = FALSE),
                error = function(e) NULL)
  if (is.null(tb) || !all(c("metro_id", "arm", "freeze_month") %in% names(tb))) {
    message("[fect.train.m9.freeze] tf_table.csv is unreadable or lacks ",
            "metro_id/arm/freeze_month; no metro is freezable")
    return(NULL)
  }
  out = tibble::tibble(
    metro_id     = suppressWarnings(as.integer(tb$metro_id)),
    metro_name   = as.character(tb$metro_name %||% NA_character_),
    arm          = as.character(tb$arm),
    tf_basis     = as.character(tb$tf_basis %||% NA_character_),
    freeze_month = .d1_month_floor(suppressWarnings(as.Date(tb$freeze_month))),
    freezable    = as.logical(tb$freezable %||% TRUE)
  )
  out$freezable[is.na(out$freezable)] = TRUE
  .FECT_M9_TF_TABLE_CACHE[[key]] = out
  out
}

#' Resolve one metro's registered freeze month.
#'
#' @param metro_id Metro id.
#' @param arm "primary" (default) or "sensitivity"; sensitivity falls back to
#'   the primary row when the metro registers no alternate.
#' @return list(freeze_month = Date or NA, tf_basis = chr, freezable = lgl).
#'   Metros absent from the table (never-treated, Madrid, Taipei) come back
#'   `freeze_month = NA` — i.e. T_f = +Inf, untouched, which is the correct
#'   default and never an error.
fect_m9_freeze_month = function(metro_id, arm = FECT_M9_TF_ARM_DEFAULT,
                                tf_table = NULL) {
  none = list(freeze_month = as.Date(NA), tf_basis = NA_character_, freezable = FALSE)
  mid = suppressWarnings(as.integer(metro_id))[1]
  if (!is.finite(mid)) return(none)
  tb = tf_table %||% fect_m9_tf_table()
  if (is.null(tb)) return(none)
  rows = tb[tb$metro_id %in% mid, , drop = FALSE]
  if (!nrow(rows)) return(none)
  pick = rows[rows$arm %in% arm, , drop = FALSE]
  if (!nrow(pick)) pick = rows[rows$arm %in% "primary", , drop = FALSE]
  if (!nrow(pick)) return(none)
  pick = pick[1, , drop = FALSE]
  if (!isTRUE(pick$freezable) || is.na(pick$freeze_month)) {
    return(list(freeze_month = as.Date(NA), tf_basis = pick$tf_basis,
                freezable = FALSE))
  }
  list(freeze_month = pick$freeze_month, tf_basis = pick$tf_basis, freezable = TRUE)
}

# -----------------------------------------------------------------------------
# CHARGED-DAYS-ONLY ATT FILTER (DESIGN section 33c.6, Tim ruling) -- DEFAULT
# OFF (CPPORTAL_FECT_CHARGED_DAYS_ONLY=1/true/yes/on). Models are fit on ALL
# days -- weekends (and Singapore Sundays) stay in the training data because
# they inform the dow/seasonal structure. This filter is a RESULTS-side /
# window-pooling cut only: it drops semi-treated days (days a cordon does not
# actually charge) BEFORE combine_att_window_d2b() pools a metro+window, per
# Tim: a non-charging day is "an extra confounder ... days that are only
# semi-treated," and pooling them alongside fully-charged days muddies the
# estimand. It touches NEITHER fect's fit, the gap model, c(M), nor T_f/freeze
# (section 33c) -- those continue to see every day, charged or not.
#
# This lives here (train/functions.R) rather than in app/v2/api/ ON PURPOSE:
# it is a SHARED helper the diagnostic/gate pooling layer uses today
# (run_d1_falsification.R) and the serving layer (app/v2/api/R/cross_city_att.R
# callers of combine_att_window_d2b) is meant to adopt VERBATIM at promotion
# time -- one map, one filter function, no reimplementation. No production
# code sources this file's charged-days pieces yet (section 33c.6: "ticketed
# separately when window-combiner work is next scheduled").
#
# Excluded weekdays are ISO %u codes (1=Mon ... 6=Sat, 7=Sun). NYC (metro_id
# 1) charges 24/7 and is deliberately ABSENT from this map -- absence means
# "exclude nothing," not "unregistered."
FECT_CHARGED_DAYS_EXCLUDED_WDAYS = list(
  `943` = c(6L, 7L),  # London CCZ/ULEZ      -- weekends not charged
  `949` = c(6L, 7L),  # Stockholm congestion tax -- weekends not charged
  `950` = c(6L, 7L),  # Milan Area C/Ecopass  -- weekends not charged
  `955` = 7L,         # Singapore ERP         -- Sundays not charged
  # Added Tim 2026-08-09, task #42. Gothenburg's trangselskatt is the SAME
  # statute family as Stockholm's (Lag 2004:629 om trangselskatt), extended to
  # Goteborg on 2013-01-01: charged Mon-Fri only, never Sat/Sun. Weekday grain
  # only -- the sub-daily hours, the public-holiday / day-before-holiday
  # exemption and the July exemption (which apply to 949 AND 957 alike) are
  # follow-ups for task #42; the mechanism is weekday-only today.
  `957` = c(6L, 7L)   # Gothenburg congestion tax -- weekends not charged
)

# -----------------------------------------------------------------------------
# EITHER/OR CHARGED DAYS (Tim ruling 2026-08-09) -- a metro can host MORE THAN
# ONE scheme, and the schemes have DIFFERENT weekday schedules.
# -----------------------------------------------------------------------------
# Tim, 2026-08-09, verbatim:
#
#   "it's an EITHER OR policy, not an AND/BOTH policy ... those monitors IN the
#    CCZ on weekends ARE treated during the ULEZ period because they are
#    affected by a traffic cordon."
#
# ESTIMAND (this is the definition every layer must implement):
#
#   treated(monitor, day) = 1  iff  at least ONE scheme whose zone covers the
#   monitor has an active period covering the day AND that scheme charges on
#   that weekday.
#
# The pre-2026-08-09 implementation applied ONE weekday map PER METRO
# (FECT_CHARGED_DAYS_EXCLUDED_WDAYS above). That is wrong for London: the CCZ
# charges Mon-Fri, but the ULEZ charges 7/7, so from 2019-04-08 a London
# monitor inside a ULEZ zone is charged on Saturday and Sunday. The per-metro
# map marked those days uncharged -- an AND/BOTH reading Tim has now overruled.
#
# GEOMETRY, VERIFIED AT SOURCE 2026-08-09 (public.monitors x public.zones,
# ST_Intersects, metro 943). Monitors inside the current CCZ core zone
# (LON-CCZ-CORE-2011, policy 42) that are ALSO inside the concurrently-active
# ULEZ zone:
#
#   ULEZ era                       n_ccz  n_ulez  ccz_in_ulez  ccz_NOT_in_ulez
#   LON-ULEZ-2019-CENTRAL (43)        34      29           29                5
#   LON-ULEZ-2021-INNER   (28)        34     147           34                0
#   LON-ULEZ-2023-WIDE    (44)        34     262           34                0
#
# So it is NOT a clean date cutover (CASE (b), not CASE (a)): during
# 2019-04-08..2021-10-24 five CCZ monitors sit OUTSIDE the central ULEZ zone
# and remain Mon-Fri-only. Genuine PER-PAIR logic is therefore required
# wherever monitor identity is available (the train side, which holds
# `zone_pairs`). Only three of those five carry any weekend panel rows in that
# window (ERG-MR8 123, ERG-MY7 234, ERG-SK8 128 = 485 monitor-days), which is
# what bounds the metro-grain approximation used by the serving layer below.
#
# HOW A PAIR GETS ITS SCHEDULE (resolution order):
#   1. policy_uid override in FECT_CHARGED_DAYS_SCHEME_EXCLUDED_WDAYS, else
#   2. system_type in FECT_CHARGED_DAYS_7_7_SYSTEM_TYPES -> charges every day,
#      else
#   3. the metro segment that contains the pair's WHOLE policy window, if
#      exactly one does (FECT_CHARGED_DAYS_METRO_SEGMENTS), else
#   4. the metro's cordon weekday map (FECT_CHARGED_DAYS_EXCLUDED_WDAYS).
#
# Step 3 exists for Bergen (958), whose two cordon eras run on different
# schedules but share one system_type and carry no policy_uid override -- the
# pair's own window is the only thing that separates them. It is a no-op
# everywhere else: London's CCZ periods straddle 2019-04-08 and fall through to
# step 4 unchanged, its ULEZ periods never reach step 3, and 949/950/955/957
# have a single open segment whose `excluded` equals their metro-map entry.
#
# Hours-of-day and public-holiday exemptions remain OUT OF SCOPE (task #42);
# the mechanism is weekday grain only.

#' Per-policy weekday overrides, keyed by `congestion_pricing_periods.policy_uid`.
#' Empty by default: a policy absent from this map resolves by system_type and
#' then by its metro's map. Add an entry only when one scheme inside a metro
#' departs from that metro's cordon schedule in a way system_type cannot express.
FECT_CHARGED_DAYS_SCHEME_EXCLUDED_WDAYS = list()

#' `system_type` values that charge SEVEN DAYS A WEEK. London's ULEZ operates
#' 24 hours a day, every day of the year except Christmas Day; at weekday grain
#' that is "exclude nothing". All three LON-ULEZ-* periods carry system_type
#' 'ULEZ', so this one entry covers them without hard-coding policy ids.
FECT_CHARGED_DAYS_7_7_SYSTEM_TYPES = c("ULEZ")

#' Excluded ISO weekdays for ONE (zone, period) pair.
#'
#' @param metro_id Metro id of the pair.
#' @param system_type `congestion_pricing_periods.system_type` (may be NA).
#' @param policy_uid `congestion_pricing_periods.policy_uid` (may be NA).
#' @param start_date,end_date The pair's own policy window (may be NA; an NA
#'   end means "open"). Used only by step 3 of the resolution order.
#' @return Integer vector of ISO weekdays (1=Mon..7=Sun) the pair does NOT
#'   charge; `integer(0)` means "charges every day".
fect_charged_days_pair_excluded_wdays = function(metro_id,
                                                 system_type = NA_character_,
                                                 policy_uid = NA_character_,
                                                 start_date = NA,
                                                 end_date = NA) {
  puid = as.character(policy_uid)[1]
  if (!is.na(puid) && nzchar(puid) &&
      !is.null(FECT_CHARGED_DAYS_SCHEME_EXCLUDED_WDAYS[[puid]])) {
    return(as.integer(FECT_CHARGED_DAYS_SCHEME_EXCLUDED_WDAYS[[puid]]))
  }
  st = as.character(system_type)[1]
  if (!is.na(st) && st %in% FECT_CHARGED_DAYS_7_7_SYSTEM_TYPES) return(integer(0))
  seg = fect_charged_days_segment_excluded_wdays(metro_id, start_date, end_date)
  if (!is.null(seg)) return(seg)
  fect_charged_days_excluded_wdays(metro_id)
}

#' The metro segment covering a pair's WHOLE policy window, if exactly one does.
#'
#' @return Integer vector of excluded ISO weekdays, or NULL meaning "no opinion,
#'   fall through to the date-blind metro map". NULL whenever the metro has no
#'   segments, the window start is unknown, or the window straddles a boundary
#'   -- London's CCZ periods run across 2019-04-08 and must keep resolving
#'   Mon-Fri from the metro map, not from whichever segment they touch first.
fect_charged_days_segment_excluded_wdays = function(metro_id, start_date,
                                                    end_date) {
  mid = suppressWarnings(as.integer(metro_id))[1]
  if (!is.finite(mid)) return(NULL)
  segs = FECT_CHARGED_DAYS_METRO_SEGMENTS[[as.character(mid)]]
  if (is.null(segs) || !length(segs)) return(NULL)
  s = suppressWarnings(as.Date(start_date))[1]
  if (is.na(s)) return(NULL)
  e = suppressWarnings(as.Date(end_date))[1]
  if (is.na(e)) e = as.Date("9999-12-31")
  hit = Filter(function(g) s >= g$from && e <= g$to, segs)
  if (length(hit) != 1L) return(NULL)
  as.integer(hit[[1]]$excluded)
}

#' METRO-GRAIN, DATE-AWARE schedule -- the either/or rule collapsed to the
#' coarsest grain that still respects it, for callers that see only
#' (metro_id, day) and cannot resolve which monitor sits in which zone.
#'
#' A segment says: over [from, to], the metro does not charge on `excluded`.
#' Segments are the OR of the metro's schemes: a weekday is excluded only if
#' NO scheme active in that window charges it. London therefore splits at
#' 2019-04-08, the day the first ULEZ period opened.
#'
#' APPROXIMATION, STATED LOUDLY: for metro 943 in 2019-04-08..2021-10-24 this
#' treats every London monitor as ULEZ-covered, but five CCZ monitors sat
#' outside the central ULEZ zone (see the table above). Three of them carry
#' weekend panel rows, 485 monitor-days in total, which this metro-grain form
#' counts as charged when they were not. That residual is accepted at the
#' serving/pooling grain and is ZERO on the train side, which uses per-pair
#' logic. Revisit if the pooling layer ever gains zone membership.
FECT_CHARGED_DAYS_METRO_SEGMENTS = list(
  `943` = list(
    # CCZ only -- Mon-Fri.
    list(from = as.Date("1900-01-01"), to = as.Date("2019-04-07"),
         excluded = c(6L, 7L)),
    # ULEZ live (central -> inner -> London-wide) -- 7/7 somewhere in the metro.
    list(from = as.Date("2019-04-08"), to = as.Date("9999-12-31"),
         excluded = integer(0))
  ),
  `949` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = c(6L, 7L))),
  `950` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = c(6L, 7L))),
  `955` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = 7L)),
  `957` = list(list(from = as.Date("1900-01-01"), to = as.Date("9999-12-31"),
                    excluded = c(6L, 7L))),
  `958` = list(
    # Bergen sentrum ring (period 65, zone 34) -- Mon-Fri only.
    list(from = as.Date("1900-01-01"), to = as.Date("2018-12-31"),
         excluded = c(6L, 7L)),
    # Bomringen full system (period 66, zone 35) -- AutoPASS-era 24/7 charging.
    # ASSUMPTION: no repo doc pins the weekday-only -> 7/7 cutover, so the
    # boundary is period 66's start; ADR-0029 lists Bergen's exact 2019
    # in-service date as an open provenance item.
    list(from = as.Date("2019-01-01"), to = as.Date("9999-12-31"),
         excluded = integer(0))
  )
)

#' Vectorised metro-grain "this day is NOT charged" predicate.
#'
#' @param metro_ids,dates Parallel vectors.
#' @return Logical vector, TRUE where the metro charges NOTHING that day.
#'   Unregistered metros (NYC, the 24/7 Nordic tolls) are always FALSE, and a
#'   non-finite metro id or unparseable date is always FALSE (never dropped,
#'   never flipped) -- the same NA posture as the pre-2026-08-09 code.
fect_charged_days_metro_uncharged = function(metro_ids, dates) {
  mids = suppressWarnings(as.integer(metro_ids))
  dts  = suppressWarnings(as.Date(dates))
  wday = as.integer(format(dts, "%u"))
  key  = as.character(mids)
  ok   = is.finite(mids) & !is.na(wday)
  out  = logical(length(mids))
  for (m in intersect(unique(key[ok]), names(FECT_CHARGED_DAYS_METRO_SEGMENTS))) {
    sel_m = ok & key == m
    if (!any(sel_m)) next
    for (s in FECT_CHARGED_DAYS_METRO_SEGMENTS[[m]]) {
      ex = as.integer(s$excluded)
      if (!length(ex)) next
      out[sel_m & dts >= s$from & dts <= s$to & wday %in% ex] = TRUE
    }
  }
  out
}

#' Fetch policy metadata (system_type / policy_uid) for a set of policy ids.
#'
#' Deliberately TRAIN-LOCAL rather than an extra column on
#' `fetch_zone_treated_pairs()`: that helper lives in the shared
#' job/model/functions.R, which is vendored into _bundled/model_functions.R and
#' read by other jobs. A tiny lookup here keeps the either/or change inside the
#' train layer, exactly as the 2026-08-08 flip did.
#'
#' @param db DBI connection; NULL returns the empty schema.
#' @param policy_ids Integer vector of `congestion_pricing_periods.id`.
#' @return Tibble(policy_id, policy_uid, system_type, metro_id).
fect_policy_schedules = function(db, policy_ids) {
  empty = tibble::tibble(policy_id = integer(), policy_uid = character(),
                         system_type = character(), metro_id = integer())
  pids = unique(suppressWarnings(as.integer(policy_ids)))
  pids = pids[is.finite(pids)]
  if (is.null(db) || !length(pids)) return(empty)
  arr = paste0("{", paste(pids, collapse = ","), "}")
  q = paste(
    "SELECT id AS policy_id, policy_uid, system_type, metro_id",
    "FROM public.congestion_pricing_periods WHERE id = ANY($1::bigint[])"
  )
  res = tryCatch(DBI::dbGetQuery(db, q, params = list(arr)),
                 error = function(e) {
                   message("[fect.train.charged] policy schedule lookup FAILED: ",
                           conditionMessage(e))
                   NULL
                 })
  if (is.null(res) || nrow(res) == 0L) return(empty)
  tibble::as_tibble(res) |>
    dplyr::mutate(policy_id   = as.integer(.data$policy_id),
                  policy_uid  = as.character(.data$policy_uid),
                  system_type = as.character(.data$system_type),
                  metro_id    = as.integer(.data$metro_id))
}

#' Is the charged-days-only ATT filter on? DEFAULT ON since 2026-08-03.
#'
#' Tim's ruling makes "excluding inactive days" the UNIVERSAL POOLING ESTIMAND
#' for every method and every basis -- the fitted fect rows included -- so it is
#' first-class, not an opt-in arm flag. Two cities' ATTs are only comparable if
#' both answer "effect on a charged day".
#'
#' CPPORTAL_FECT_CHARGED_DAYS_ONLY is now the DIAGNOSTIC ESCAPE HATCH: set it to
#' 0/false/no/off to restore the pre-ruling all-days pool. Callers can also pass
#' `enabled = FALSE` (the off-mode regression gates do).
fect_charged_days_only_enabled = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_FECT_CHARGED_DAYS_ONLY", "")))
  if (!nzchar(v)) return(TRUE)
  !(v %in% c("0", "false", "f", "no", "n", "off"))
}

#' Excluded ISO weekdays (1=Mon..7=Sun) for one metro. Metros absent from the
#' map (NYC, and any metro not yet registered) return `integer(0)` -- exclude
#' nothing, i.e. every day is charged/comparable.
#'
#' @param metro_id Metro id (scalar or vector; only the first element is used).
fect_charged_days_excluded_wdays = function(metro_id) {
  mid = suppressWarnings(as.integer(metro_id))[1]
  if (!is.finite(mid)) return(integer(0))
  key = as.character(mid)
  FECT_CHARGED_DAYS_EXCLUDED_WDAYS[[key]] %||% integer(0)
}

#' Drop semi-treated (non-charging) days from a per-monitor-day or per-metro-
#' day tibble before it is handed to `combine_att_window_d2b()`. DEFAULT OFF
#' (`enabled = NULL` reads `fect_charged_days_only_enabled()`); pass
#' `enabled = FALSE` to force a no-op regardless of the env var (used by the
#' off-mode regression gates).
#'
#' Rows whose metro has no entry in `FECT_CHARGED_DAYS_EXCLUDED_WDAYS` (e.g.
#' NYC, 24/7 tolling) are always kept -- this function only ever REMOVES rows,
#' never edits them, so a disabled call and an unregistered metro are both
#' exact no-ops.
#'
#' @param rows Tibble/data.frame with a metro id column and a Date column.
#' @param date_col Name of the Date column (default "day", matching the
#'   per_day_metro / per_day_metro_unit_imputed / D1 row shapes).
#' @param metro_col Name of the metro id column (default "metro_id").
#' @param enabled NULL (default, reads the env var) or an explicit TRUE/FALSE.
#' @return `rows` with semi-treated-day rows removed; unchanged (same object)
#'   when disabled, `rows` is NULL, or `rows` has zero rows.
fect_charged_days_only_filter = function(rows, date_col = "day",
                                          metro_col = "metro_id",
                                          enabled = NULL) {
  if (is.null(enabled)) enabled = fect_charged_days_only_enabled()
  if (!isTRUE(enabled)) return(rows)
  if (is.null(rows) || !nrow(rows)) return(rows)
  if (!all(c(date_col, metro_col) %in% names(rows))) return(rows)
  # EITHER/OR (Tim 2026-08-09): a day is charged if ANY scheme covering the
  # metro charges that weekday, so London weekends are KEPT from 2019-04-08
  # (ULEZ, 7/7) and dropped only before it (CCZ, Mon-Fri). This is the
  # metro-grain collapse of the rule -- see FECT_CHARGED_DAYS_METRO_SEGMENTS
  # for the 485-monitor-day residual it accepts. Still vectorised (one pass per
  # registered metro-segment) and still NA-safe: a row with a non-finite metro
  # id or an unparseable date is never dropped.
  drop = fect_charged_days_metro_uncharged(rows[[metro_col]], rows[[date_col]])
  rows[!drop, , drop = FALSE]
}

# -----------------------------------------------------------------------------
# UNCHARGED DAYS ARE UNTREATED IN THE FIT (Tim ruling 2026-08-08) -- DEFAULT ON
# (CPPORTAL_FECT_UNTREAT_UNCHARGED=0/false/no/off restores the old behavior).
# -----------------------------------------------------------------------------
# This REVERSES the earlier ruling encoded above ("models are fit on ALL days --
# weekends stay in the training data ... this filter is a RESULTS-side cut
# only"). Tim, 2026-08-08: "if there are entire days marked uncharged, then
# those should probably be left marked untreated. I'm changing my opinion. It is
# more precise and accurate."
#
# So a monitor-day that falls on one of its metro's NON-CHARGING weekdays is now
# `treated = FALSE` in the panel handed to `get_fect()`. The day is NOT dropped:
# the outcome and every covariate stay, so the row still informs the dow /
# seasonal / factor structure -- it simply stops claiming the cordon was
# charging that day, which it wasn't. Under the old encoding a London Sunday was
# a "treated" observation of an untreated regime, biasing the ATT toward zero.
#
# SCOPE -- what this does NOT change:
#   * `days_since_treatment` / `treatment_start_date` semantics are UNCHANGED.
#     add_treatment_zones() derives treatment_start_date from the POLICY window
#     (min start_date over the monitor's pairs), never from the `treated` flag,
#     so event time still counts from the first period start.
#   * The SERVING-side pooling cut (`fect_charged_days_only_filter()`, above) is
#     UNCHANGED and still runs: pooled ATTs remain charged-days-only. This
#     change is about what the ESTIMATOR SEES, not what gets reported.
#   * The schedule itself is NOT duplicated -- this reuses
#     `fect_charged_days_pair_excluded_wdays()` / FECT_CHARGED_DAYS_*.
#     There are two implementations of the charged-days rule (this flip and the
#     pooling filter); do not write a third.
#
# REFINED 2026-08-09 (Tim's either/or ruling -- see the block above): the flip
# is now PER (zone, period) PAIR, not per metro. A treated monitor-day stays
# treated if ANY covering pair charges that weekday, so a London monitor inside
# a ULEZ zone keeps its Saturdays and Sundays from 2019-04-08 while a CCZ-only
# monitor still loses them. Pre-2019 London weekends flip exactly as before.
# When `zone_pairs` / `policy_schedules` are unavailable the function degrades
# to the metro-grain date-aware segments, which still implements the ruling for
# London at the coarser grain.
#
# KNOWN, INTENDED SIDE EFFECT: flipping weekends makes treatment NON-ABSORBING
# (D = 1,1,1,1,1,0,0,1,1,...). `fect` supports this natively -- it computes
# `hasRevs` from the D matrix and only `method = "gsynth"` refuses reversals;
# `method = "cfe"` takes `hasRevs`/`T.off` and additionally emits switch-off
# dynamics (`att.off`, `time.off`). Treated monitors therefore gain
# within-post-period untreated observations, which CHANGES IDENTIFICATION (e.g.
# London CCZ monitors can become fittable off weekend days). Numbers will move
# at the next retrain; that is the point.

#' Is the "uncharged days are untreated" flip on? DEFAULT ON since 2026-08-08.
#'
#' CPPORTAL_FECT_UNTREAT_UNCHARGED is the kill switch: 0/false/f/no/n/off
#' restores the pre-2026-08-08 encoding (uncharged days marked TREATED).
fect_untreat_uncharged_enabled = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_FECT_UNTREAT_UNCHARGED", "")))
  if (!nzchar(v)) return(TRUE)
  !(v %in% c("0", "false", "f", "no", "n", "off"))
}

#' Is the D_unch X-covariate on? DEFAULT ON.
#'
#' `CPPORTAL_FECT_UNCH_DUMMY=0` (or false/f/no/n/off) disables. Call AFTER
#' `fect_untreat_uncharged_days()` so the dummy reflects the post-flip panel.
fect_unch_dummy_enabled = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_FECT_UNCH_DUMMY", "")))
  if (!nzchar(v)) return(TRUE)
  !(v %in% c("0", "false", "f", "no", "n", "off"))
}

#' Add D_unch (in-window untreated) to the panel and to spec$covariates (X).
#'
#' D_unch = 1 when `days_since_treatment >= 0` and `treated == FALSE` (after the
#' uncharged flip). Never a Z covariate; never an episode flag. Returns
#' `list(panel=, spec=)`.
fect_add_d_unch = function(panel, spec,
                           treated_col = "treated",
                           dst_col = "days_since_treatment") {
  if (!fect_unch_dummy_enabled()) {
    message("[fect.train.d_unch] DISABLED via CPPORTAL_FECT_UNCH_DUMMY")
    return(list(panel = panel, spec = spec))
  }
  if (is.null(panel) || !nrow(panel)) return(list(panel = panel, spec = spec))
  if (!all(c(treated_col, dst_col) %in% names(panel))) {
    message("[fect.train.d_unch] SKIP (missing ",
            paste(setdiff(c(treated_col, dst_col), names(panel)), collapse = ", "),
            ")")
    return(list(panel = panel, spec = spec))
  }
  dst = suppressWarnings(as.numeric(panel[[dst_col]]))
  panel$D_unch = as.integer(
    is.finite(dst) & dst >= 0 & !(panel[[treated_col]] %in% TRUE)
  )
  n_on = sum(panel$D_unch == 1L, na.rm = TRUE)
  message("[fect.train.d_unch] added D_unch to X; n_on=", n_on,
          " (", sprintf("%.2f", 100 * n_on / nrow(panel)), "% of rows); ",
          "CF ATT cells have D_unch=0 by construction")

  rhs = if (is.null(spec$covariates)) character(0) else all.vars(spec$covariates)
  if (!("D_unch" %in% rhs)) {
    rhs = c(rhs, "D_unch")
    spec$covariates = stats::as.formula(
      paste0("~ ", paste(rhs, collapse = " + "))
    )
  }
  list(panel = panel, spec = spec)
}

#' Mark non-charging monitor-days as UNTREATED in a training panel.
#'
#' Call immediately after `add_treatment_zones()` and before the dedupe /
#' `get_fect()`. Only ever flips TRUE -> FALSE; never drops a row, never adds
#' one, never touches `days_since_treatment`.
#'
#' The weekday is computed DIRECTLY on `date` with no timezone shift, which is
#' correct here: the panel's `date` is already METRO-LOCAL (it comes from the
#' `aq_local_date` basis in `fetch_panel()`), so "Saturday" means Saturday in
#' London / Stockholm / Milan / Singapore, not Saturday UTC.
#'
#' @param panel Training panel with `treated`, a date column, and a metro column.
#' @param spec_id Spec id, for the log lines only.
#' @param zone_pairs The (monitor, period) pairs that produced `treated` --
#'   `fetch_zone_treated_pairs()` / `fetch_metro_treated_pairs()` output
#'   (`metro_id`, `fullaqsid`, `policy_id`, `start_date`, `end_date`). Required
#'   for per-pair either/or; NULL falls back to metro-grain segments.
#' @param policy_schedules `fect_policy_schedules()` output (policy_id ->
#'   policy_uid / system_type). NULL falls back to metro-grain segments.
#' @param date_col,metro_col,unit_col,treated_col Column names.
#' @param enabled NULL (default, reads the env var) or explicit TRUE/FALSE.
#' @return `panel` with `treated` flipped to FALSE on non-charging days.
fect_untreat_uncharged_days = function(panel, spec_id = NA_character_,
                                       zone_pairs = NULL,
                                       policy_schedules = NULL,
                                       date_col = "date",
                                       metro_col = "metro_id",
                                       unit_col = "fullaqsid",
                                       treated_col = "treated",
                                       enabled = NULL) {
  if (is.null(enabled)) enabled = fect_untreat_uncharged_enabled()
  tag = paste0("[fect.train.charged] spec=", spec_id, " ")
  if (!isTRUE(enabled)) {
    message(tag, "DISABLED via CPPORTAL_FECT_UNTREAT_UNCHARGED",
            " -- uncharged days stay TREATED (pre-2026-08-08 encoding)")
    return(panel)
  }
  if (is.null(panel) || !nrow(panel)) return(panel)
  if (!all(c(date_col, metro_col, treated_col) %in% names(panel))) {
    message(tag, "SKIP (missing column(s): ",
            paste(setdiff(c(date_col, metro_col, treated_col), names(panel)),
                  collapse = ", "), ")")
    return(panel)
  }

  mids = suppressWarnings(as.integer(panel[[metro_col]]))
  dts  = as.Date(panel[[date_col]])
  # ISO weekday, 1=Mon .. 7=Sun -- the same numbering as
  # FECT_CHARGED_DAYS_EXCLUDED_WDAYS (and as format(date, "%u")).
  wday = lubridate::wday(dts, week_start = 1)
  is_tr = panel[[treated_col]] %in% TRUE
  ok    = is.finite(mids) & !is.na(wday)
  n_treated_before = sum(is_tr, na.rm = TRUE)

  # --- can we do the exact, per-pair either/or? -------------------------------
  pair_cols = c("fullaqsid", "policy_id", "start_date", "end_date")
  pair_mode = !is.null(zone_pairs) && nrow(zone_pairs) > 0L &&
    all(pair_cols %in% names(zone_pairs)) &&
    !is.null(policy_schedules) && nrow(policy_schedules) > 0L &&
    unit_col %in% names(panel)

  if (!pair_mode) {
    # Metro-grain fallback -- still date-aware, so London's post-2019 weekends
    # are correctly left TREATED; only the five CCZ-outside-central-ULEZ
    # monitors are approximated (see FECT_CHARGED_DAYS_METRO_SEGMENTS).
    message(tag, "pair schedules unavailable -- using METRO-GRAIN either/or",
            " segments (zone_pairs=", if (is.null(zone_pairs)) "NULL" else nrow(zone_pairs),
            ", policy_schedules=",
            if (is.null(policy_schedules)) "NULL" else nrow(policy_schedules), ")")
    unch = fect_charged_days_metro_uncharged(mids, dts)
    flip_total = 0L
    for (m in sort(unique(mids[ok & is_tr]))) {
      hit = ok & is_tr & mids == m & unch
      n_hit = sum(hit, na.rm = TRUE)
      if (n_hit == 0L) next
      panel[[treated_col]][hit] = FALSE
      flip_total = flip_total + as.integer(n_hit)
      message("[fect.train.charged] metro=", m, " flipped n=", n_hit,
              " monitor-days to untreated (metro-grain either/or segments)")
    }
    message(tag, "total flipped n=", flip_total,
            " of n_treated=", n_treated_before,
            " monitor-days; treatment is now NON-ABSORBING where flipped",
            " (fect handles reversals: hasRevs/T.off)")
    return(panel)
  }

  # --- exact per-pair either/or ----------------------------------------------
  # charged(monitor, day) = OR over covering pairs of "this pair charges that
  # weekday". Build one row per pair carrying its resolved excluded-weekday set.
  pairs = tibble::as_tibble(zone_pairs) |>
    dplyr::mutate(
      .u   = as.character(.data$fullaqsid),
      .pid = as.integer(.data$policy_id),
      .s   = as.Date(.data$start_date),
      .e   = dplyr::coalesce(as.Date(.data$end_date), as.Date("9999-12-31"))
    ) |>
    dplyr::left_join(
      policy_schedules |>
        dplyr::select(dplyr::all_of(c("policy_id", "policy_uid", "system_type"))) |>
        dplyr::mutate(.pid = as.integer(.data$policy_id)) |>
        dplyr::select(-dplyr::all_of("policy_id")),
      by = ".pid"
    )
  pair_metro = if (metro_col %in% names(pairs)) {
    suppressWarnings(as.integer(pairs[[metro_col]]))
  } else {
    rep(NA_integer_, nrow(pairs))
  }
  pair_excl = lapply(seq_len(nrow(pairs)), function(i) {
    fect_charged_days_pair_excluded_wdays(
      metro_id    = pair_metro[i],
      system_type = pairs$system_type[i],
      policy_uid  = pairs$policy_uid[i],
      start_date  = pairs$.s[i],
      end_date    = pairs$.e[i]
    )
  })

  # Log the resolved schedule once per (metro, policy) so the ruling is
  # auditable from the rendered job.html.
  sched_key = paste(pair_metro, pairs$policy_uid,
                    vapply(pair_excl, paste, character(1), collapse = ","),
                    sep = "|")
  for (k in unique(sched_key)) {
    i = which(sched_key == k)[1]
    ex = pair_excl[[i]]
    message("[fect.train.charged] schedule metro=", pair_metro[i],
            " policy=", pairs$policy_uid[i],
            " system_type=", pairs$system_type[i],
            " excluded_wdays=",
            if (length(ex)) paste(ex, collapse = ",") else "<none, charges 7/7>",
            " n_pairs=", length(which(sched_key == k)))
  }

  cand    = which(ok & is_tr)
  charged = logical(length(cand))
  has_pair = logical(length(cand))
  if (length(cand)) {
    unit_chr  = as.character(panel[[unit_col]])[cand]
    idx_by_u  = split(seq_along(cand), unit_chr)
    rows_by_u = split(seq_len(nrow(pairs)), pairs$.u)
    for (u in intersect(names(idx_by_u), names(rows_by_u))) {
      sel = idx_by_u[[u]]
      has_pair[sel] = TRUE
      du = dts[cand][sel]
      wu = wday[cand][sel]
      for (r in rows_by_u[[u]]) {
        inwin = !is.na(du) & du >= pairs$.s[r] & du <= pairs$.e[r]
        if (!any(inwin)) next
        ex = pair_excl[[r]]
        ch = inwin & !(wu %in% ex)
        if (any(ch)) charged[sel[ch]] = TRUE
      }
    }
  }

  # A treated row whose unit has NO pair at all should never be silently
  # untreated -- that would mean the panel and the pairs disagree. Leave it and
  # say so loudly.
  n_orphan = sum(!has_pair)
  if (n_orphan > 0L) {
    message(tag, "WARNING ", n_orphan, " treated monitor-days have NO matching ",
            "zone_pair (unit_col='", unit_col, "') -- left TREATED, not flipped")
  }

  flip_idx = cand[has_pair & !charged]
  flip_total = length(flip_idx)
  if (flip_total) panel[[treated_col]][flip_idx] = FALSE
  for (m in sort(unique(mids[flip_idx]))) {
    n_hit = sum(mids[flip_idx] == m, na.rm = TRUE)
    message("[fect.train.charged] metro=", m, " flipped n=", n_hit,
            " monitor-days to untreated (per-pair either/or: no covering",
            " scheme charges that weekday)")
  }
  message(tag, "total flipped n=", flip_total,
          " of n_treated=", n_treated_before,
          " monitor-days; treatment is now NON-ABSORBING where flipped",
          " (fect handles reversals: hasRevs/T.off)")
  panel
}

# Square back-transform (sqrt model scale -> native ug/m3). Identical closed
# form to the local closures inside impute_dropped_zone_atts() and to
# get_simeffects_fect(backtransform="square"): for X ~ N(mu, s), Y = X^2 =>
# E[Y] = mu^2 + s^2 and SD[Y] = s*sqrt(2 s^2 + 4 mu^2). Duplicated (not
# refactored out of M7) on purpose: touching the M7 closures would break the
# "gate off => byte-identical" guarantee this block is required to keep.
.d1_bt_mean_sq = function(mu, s) mu^2 + s^2
.d1_bt_sd_sq   = function(mu, s) s * sqrt(2 * s^2 + 4 * mu^2)

#' First-of-month Date for any date-ish vector.
.d1_month_floor = function(x) as.Date(format(as.Date(x), "%Y-%m-01"))

#' Integer month index (months since 1970-01) — makes CALENDAR gaps visible to
#' the HAC lag structure instead of pretending consecutive rows are adjacent.
.d1_month_index = function(ym) {
  d = as.Date(ym)
  as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m")) - 1L
}

#' Inverse of `.d1_month_index()` — first-of-month Date from a month index.
.d1_month_from_index = function(mi) {
  mi = as.integer(mi)
  as.Date(sprintf("%04d-%02d-01", mi %/% 12L, mi %% 12L + 1L))
}

#' Lag-truncated (Bartlett / Newey-West) HAC standard error of a LOCATION
#' estimate from its residuals.
#'
#' Why manual rather than `sandwich::NeweyWest`: our point estimate is a Huber
#' M-estimate, not the OLS mean, so `NeweyWest()` on `lm(z ~ 1)` would return
#' the HAC SE of a DIFFERENT estimator. The long-run-variance arithmetic is the
#' same either way, so it is done here on the Huber residuals. The OLS/sandwich
#' number is still computed as a cross-check when the package is installed (see
#' `stage_p_project_units`'s `theta_star_se_nw_ols` column).
#'
#' se^2 = LRV / n,  LRV = gamma_0 + 2 * sum_{l=1..L} (1 - l/(L+1)) * gamma_l
#' with gamma_l = (1/n) * sum over month pairs exactly l months apart of u_M
#' u_{M+l}. Pairs straddling a calendar gap simply do not exist and contribute
#' nothing, which shrinks gamma_l toward 0 — the conservative direction is the
#' other one, so a gap-heavy series can UNDER-state autocorrelation; the
#' diagnostic script reports n_months so that is visible.
#'
#' Bandwidth: Newey-West's classic rule of thumb L = floor(4 (n/100)^(2/9)),
#' floored at 1. A non-positive LRV (possible for any HAC estimator in finite
#' samples) falls back to the iid variance and says so in `$method`.
#'
#' @param u numeric residuals (z - theta_star)
#' @param month_index integer month indices aligned with `u` (see .d1_month_index)
#' @param lag optional integer override for L
#' @return list(se, lag, method, n, lrv, gamma0)
.d1_hac_se = function(u, month_index, lag = NULL) {
  u = as.numeric(u)
  month_index = as.integer(month_index)
  ok = is.finite(u) & is.finite(month_index)
  u = u[ok]; month_index = month_index[ok]
  n = length(u)
  if (n == 0L) {
    return(list(se = NA_real_, lag = NA_integer_, method = "none",
                n = 0L, lrv = NA_real_, gamma0 = NA_real_))
  }
  g0 = sum(u * u) / n
  if (n < 3L) {
    return(list(se = sqrt(g0 / n), lag = 0L, method = "iid_small_n",
                n = n, lrv = g0, gamma0 = g0))
  }
  L = if (is.null(lag)) max(1L, as.integer(floor(4 * (n / 100)^(2 / 9))))
      else max(0L, as.integer(lag))
  acc = 0
  if (L >= 1L) {
    for (l in seq_len(L)) {
      idx = match(month_index + l, month_index)
      keep = !is.na(idx)
      if (!any(keep)) next
      gl = sum(u[keep] * u[idx[keep]]) / n
      acc = acc + (1 - l / (L + 1)) * gl
    }
  }
  lrv = g0 + 2 * acc
  method = "bartlett_hac"
  if (!is.finite(lrv) || lrv <= 0) {
    lrv = g0
    method = "bartlett_hac_nonpositive_fallback_iid"
  }
  list(se = sqrt(lrv / n), lag = L, method = method, n = n,
       lrv = lrv, gamma0 = g0)
}

#' 2-harmonic seasonal fit of z on month-of-year — DIAGNOSTIC ONLY (§7.7(2)).
#' Never enters the counterfactual. A large amplitude/R2 means the monitor's
#' satellite-minus-frozen-structure residual has seasonal shape the two-way fect
#' model does not carry at the unit level, i.e. the level-only projection is
#' mis-specified for that monitor.
.d1_harmonic_diag = function(z, moy) {
  z = as.numeric(z); moy = as.integer(moy)
  ok = is.finite(z) & is.finite(moy)
  z = z[ok]; moy = moy[ok]
  n = length(z)
  out = list(n = n, r2 = NA_real_, amp1 = NA_real_, amp2 = NA_real_,
             ok = FALSE)
  if (n < 12L) return(out)
  X = cbind(
    s1 = sin(2 * pi * moy / 12), c1 = cos(2 * pi * moy / 12),
    s2 = sin(4 * pi * moy / 12), c2 = cos(4 * pi * moy / 12)
  )
  fit = tryCatch(stats::lm(z ~ X), error = function(e) NULL)
  if (is.null(fit)) return(out)
  cf = stats::coef(fit)
  gc = function(nm) { v = cf[[paste0("X", nm)]]; if (is.null(v) || !is.finite(v)) 0 else v }
  out$r2   = tryCatch(summary(fit)$r.squared, error = function(e) NA_real_)
  out$amp1 = sqrt(gc("s1")^2 + gc("c1")^2)
  out$amp2 = sqrt(gc("s2")^2 + gc("c2")^2)
  out$ok   = TRUE
  out
}

# -----------------------------------------------------------------------------
# tau BACKCAST (DESIGN §10, Tim 2026-07-31: "tau backcast IN v1")
# -----------------------------------------------------------------------------
#' Extend the frozen monthly time effect tau_bar_M back before the panel starts.
#'
#' The frozen tau_t only exists for dates in the fect panel (>= 2003-05 today),
#' but every cordon monitor has satellite months back to 1998-01. Without a
#' backcast, London's 62 true pre-CCZ satellite months are unusable in Stage P
#' — the binding constraint is tau availability, not satellite availability.
#'
#' Construction:
#'  1. Pool sqrt(sat) over OUT-OF-CORDON monitors into a monthly index. The pool
#'     is a TWO-WAY additive decomposition (monitor effect + month effect)
#'     solved by alternating projections, NOT a raw cross-monitor mean: a raw
#'     mean confounds a real month movement with a change in which monitors are
#'     in the pool. The month effects + grand mean are the index `s_index_M`.
#'  2. Calibrate the frozen tau_bar_M on s_index_M by OLS over the OVERLAP
#'     months (both observed). One slope + intercept, nothing per-monitor.
#'  3. Backcast tau_bar_M = a + b * s_index_M for months with an index but no
#'     frozen tau, and tag them `source = "backcast"`.
#'
#' Out-of-cordon is what makes step 2 honest: the calibration series is never
#' contaminated by a cordon's own effect, so the backcast carries no policy
#' signal into the pre-period.
#'
#' @param frozen_tau_monthly Either a monthly tibble (`year_month`, `tau_bar`)
#'   or the raw `frozen_components$tau_time` (`date`, `tau`), which is
#'   aggregated to months here.
#' @param sat_monthly_out_of_cordon Tibble (`fullaqsid`, `year_month`, value)
#'   where the value column is NATIVE satellite PM (`sat_value` / `sat` /
#'   `value`); restricted by the CALLER to out-of-cordon monitors.
#' @param min_overlap Minimum overlap months required to calibrate (default 24).
#' @param backcast_to Optional earliest month to emit (default: the earliest
#'   month the satellite index covers).
#' @return list(tau_bar_ext, calibration, sat_index) — or NULL when the inputs
#'   cannot support a calibration (never an error; Stage P then simply runs on
#'   the frozen months only).
compute_tau_backcast = function(frozen_tau_monthly,
                                sat_monthly_out_of_cordon,
                                min_overlap = 24L,
                                backcast_to = NULL) {
  bail = function(reason) {
    message("[fect.train.d1.tau] backcast skipped: ", reason)
    NULL
  }
  if (is.null(frozen_tau_monthly) || nrow(frozen_tau_monthly) == 0L) {
    return(bail("no frozen tau"))
  }

  # ---- frozen tau -> monthly means -----------------------------------------
  ft = tibble::as_tibble(frozen_tau_monthly)
  tau_m = if (all(c("year_month", "tau_bar") %in% names(ft))) {
    ft |>
      dplyr::transmute(year_month = .d1_month_floor(.data$year_month),
                       tau_bar    = as.numeric(.data$tau_bar)) |>
      dplyr::filter(is.finite(.data$tau_bar))
  } else if (all(c("date", "tau") %in% names(ft))) {
    ft |>
      dplyr::filter(!is.na(.data$date), is.finite(.data$tau)) |>
      dplyr::mutate(year_month = .d1_month_floor(.data$date)) |>
      dplyr::group_by(.data$year_month) |>
      dplyr::summarise(tau_bar = mean(as.numeric(.data$tau), na.rm = TRUE),
                       .groups = "drop")
  } else {
    return(bail("frozen tau has neither (year_month, tau_bar) nor (date, tau)"))
  }
  if (nrow(tau_m) == 0L) return(bail("frozen tau has no usable months"))

  # ---- satellite monthly index over out-of-cordon monitors ------------------
  if (is.null(sat_monthly_out_of_cordon) ||
      nrow(sat_monthly_out_of_cordon) == 0L) {
    return(bail("no out-of-cordon satellite months supplied"))
  }
  s = tibble::as_tibble(sat_monthly_out_of_cordon)
  val_col = intersect(c("sat_value", "sat", "value"), names(s))[1]
  if (is.na(val_col) || !("fullaqsid" %in% names(s)) ||
      !("year_month" %in% names(s))) {
    return(bail("satellite table needs fullaqsid, year_month and a value column"))
  }
  s = s |>
    dplyr::transmute(
      fullaqsid  = as.character(.data$fullaqsid),
      year_month = .d1_month_floor(.data$year_month),
      s          = sqrt(pmax(as.numeric(.data[[val_col]]), 0))
    ) |>
    dplyr::filter(is.finite(.data$s)) |>
    dplyr::group_by(.data$fullaqsid, .data$year_month) |>
    dplyr::summarise(s = mean(.data$s), .groups = "drop")
  if (dplyr::n_distinct(s$fullaqsid) < 2L) {
    return(bail("fewer than 2 out-of-cordon monitors in the satellite pool"))
  }

  # Two-way additive decomposition s_iM = grand + a_i + b_M + resid by
  # alternating projections. Balanced panels converge in one sweep; unbalanced
  # ones converge geometrically. 100 sweeps is far more than needed and costs
  # milliseconds at this size (~1e5 rows).
  grand = mean(s$s)
  uf = factor(s$fullaqsid)
  mf = factor(format(s$year_month))
  a = rep(0, nrow(s)); b = rep(0, nrow(s))
  for (it in seq_len(100L)) {
    b_new = stats::ave(s$s - grand - a, mf, FUN = mean)
    a_new = stats::ave(s$s - grand - b_new, uf, FUN = mean)
    delta = max(max(abs(b_new - b)), max(abs(a_new - a)))
    b = b_new; a = a_new
    if (!is.finite(delta) || delta < 1e-10) break
  }
  sat_index = tibble::tibble(year_month = s$year_month, b = b,
                             fullaqsid = s$fullaqsid) |>
    dplyr::group_by(.data$year_month) |>
    dplyr::summarise(s_index = grand + mean(.data$b),
                     n_units = dplyr::n_distinct(.data$fullaqsid),
                     .groups = "drop") |>
    dplyr::arrange(.data$year_month)

  # ---- calibrate tau_bar ~ s_index on the overlap ---------------------------
  ov = dplyr::inner_join(tau_m, sat_index, by = "year_month") |>
    dplyr::filter(is.finite(.data$tau_bar), is.finite(.data$s_index))
  if (nrow(ov) < min_overlap) {
    return(bail(paste0("overlap is ", nrow(ov), " months (< ", min_overlap, ")")))
  }
  fit = tryCatch(stats::lm(tau_bar ~ s_index, data = ov),
                 error = function(e) NULL)
  if (is.null(fit)) return(bail("calibration lm() failed"))
  sm = summary(fit)
  cf = stats::coef(fit)

  # ---- emit the extended series --------------------------------------------
  first_frozen = min(tau_m$year_month)
  need = sat_index |> dplyr::filter(.data$year_month < first_frozen)
  if (!is.null(backcast_to)) {
    need = need |> dplyr::filter(.data$year_month >= .d1_month_floor(backcast_to))
  }
  bc = if (nrow(need) == 0L) {
    tibble::tibble(year_month = as.Date(character()), tau_bar = numeric(),
                   tau_bar_se = numeric(), source = character())
  } else {
    pr = tryCatch(stats::predict(fit, newdata = need, se.fit = TRUE),
                  error = function(e) NULL)
    if (is.null(pr)) return(bail("predict() on the backcast months failed"))
    tibble::tibble(
      year_month = need$year_month,
      tau_bar    = as.numeric(pr$fit),
      # PREDICTION (not confidence) SE: a backcast tau is a draw of a new month,
      # so the residual scale belongs in it. This SE is reported, not folded
      # into Stage F's variance split — see the diagnostic script's header.
      tau_bar_se = sqrt(as.numeric(pr$se.fit)^2 + sm$sigma^2),
      source     = "backcast"
    )
  }

  tau_bar_ext = dplyr::bind_rows(
    bc,
    tau_m |> dplyr::mutate(tau_bar_se = 0, source = "frozen")
  ) |>
    dplyr::arrange(.data$year_month)

  calibration = list(
    n_overlap    = nrow(ov),
    overlap_from = min(ov$year_month),
    overlap_to   = max(ov$year_month),
    intercept    = unname(cf[["(Intercept)"]]),
    slope        = unname(cf[["s_index"]]),
    r_squared    = sm$r.squared,
    adj_r_squared = sm$adj.r.squared,
    sigma        = sm$sigma,
    n_backcast   = nrow(bc),
    backcast_from = if (nrow(bc)) min(bc$year_month) else as.Date(NA),
    backcast_to   = if (nrow(bc)) max(bc$year_month) else as.Date(NA),
    n_pool_units = dplyr::n_distinct(s$fullaqsid)
  )
  message(sprintf(
    paste0("[fect.train.d1.tau] calibration on %d overlap months (%s..%s): ",
           "tau_bar = %+.4f %+.4f * s_index, R2=%.3f sigma=%.4f; ",
           "backcast %d month(s) %s..%s from %d out-of-cordon monitor(s)"),
    calibration$n_overlap, format(calibration$overlap_from),
    format(calibration$overlap_to), calibration$intercept, calibration$slope,
    calibration$r_squared, calibration$sigma, calibration$n_backcast,
    format(calibration$backcast_from), format(calibration$backcast_to),
    calibration$n_pool_units
  ))

  list(tau_bar_ext = tau_bar_ext, calibration = calibration,
       sat_index = sat_index)
}

# -----------------------------------------------------------------------------
# STAGE P — per-unit monthly projection (DESIGN §7.3 as corrected by §7.7(2))
# -----------------------------------------------------------------------------
#' Build the Stage-P projection residual table `z` (shared by every level rule).
#'
#'   z_i,M = sqrt(sat_i,M) + gap_pred_i,M - mu - tau_bar_M
#'
#' Extracted VERBATIM out of `stage_p_project_units()` so the lambda-window
#' family (DESIGN §13) reads exactly the same z the D1 (lambda = Inf) estimator
#' reads — one construction, one set of joins, one set of degradation paths. The
#' arguments and the returned columns are unchanged from what Stage P built
#' inline before; `stage_p_project_units()` now calls this and is otherwise
#' identical (the lambda = Inf reproduction check in
#' train/tests/test_stage_pf.R pins that).
#'
#' @inheritParams stage_p_project_units
#' @return list(z, unit_tbl, mu) — or NULL (with a message) when a required
#'   input is missing/empty. `z` carries fullaqsid, year_month, sat_sqrt,
#'   tau_bar, tau_source, gap_pred, gap_pred_se, z, treat_start, period,
#'   month_index, moy.
.d1_build_z = function(units, sat_monthly, gap_pred_tbl, frozen, tau_bar_ext,
                       treat_start = NULL, .label = "stage_p") {
  bail = function(reason) {
    message("[fect.train.d1.", .label, "] skip: ", reason)
    NULL
  }
  unit_tbl = if (is.data.frame(units)) {
    tibble::as_tibble(units)
  } else {
    tibble::tibble(fullaqsid = as.character(units))
  }
  unit_tbl$fullaqsid = as.character(unit_tbl$fullaqsid)
  unit_tbl = unit_tbl |> dplyr::distinct(.data$fullaqsid, .keep_all = TRUE)
  if (nrow(unit_tbl) == 0L) return(bail("no units"))
  if (is.null(frozen) || !is.finite(suppressWarnings(as.numeric(frozen$mu %||% NA)))) {
    return(bail("frozen$mu missing — mu MUST travel with theta/tau (task 008)"))
  }
  mu = as.numeric(frozen$mu)
  if (is.null(sat_monthly) || nrow(sat_monthly) == 0L) {
    return(bail("no satellite months"))
  }
  if (is.null(tau_bar_ext) || nrow(tau_bar_ext) == 0L) {
    return(bail("no tau_bar series"))
  }

  sm = tibble::as_tibble(sat_monthly)
  val_col = intersect(c("sat_value", "sat", "value"), names(sm))[1]
  if (is.na(val_col)) return(bail("satellite table has no value column"))
  sm = sm |>
    dplyr::transmute(
      fullaqsid  = as.character(.data$fullaqsid),
      year_month = .d1_month_floor(.data$year_month),
      sat_sqrt   = sqrt(pmax(as.numeric(.data[[val_col]]), 0))
    ) |>
    dplyr::filter(.data$fullaqsid %in% unit_tbl$fullaqsid,
                  is.finite(.data$sat_sqrt)) |>
    dplyr::group_by(.data$fullaqsid, .data$year_month) |>
    dplyr::summarise(sat_sqrt = mean(.data$sat_sqrt), .groups = "drop")
  if (nrow(sm) == 0L) return(bail("no satellite months for the requested units"))

  tb = tibble::as_tibble(tau_bar_ext) |>
    dplyr::transmute(year_month = .d1_month_floor(.data$year_month),
                     tau_bar    = as.numeric(.data$tau_bar),
                     tau_source = as.character(.data$source %||% "frozen")) |>
    dplyr::filter(is.finite(.data$tau_bar)) |>
    dplyr::distinct(.data$year_month, .keep_all = TRUE)

  # Gap: monitor-level point + SE, optionally refined per month (mirrors the M7
  # coalesce in impute_unit_days — monthly point where available, monitor-level
  # fallback, and the SE always stays monitor-level / day-persistent).
  if (is.null(gap_pred_tbl) || nrow(gap_pred_tbl) == 0L) {
    return(bail("no gap predictions — the satellite level cannot be translated to the monitor"))
  }
  gp = tibble::as_tibble(gap_pred_tbl)
  gp$fullaqsid = as.character(gp$fullaqsid)
  gp_unit = gp |>
    dplyr::filter(is.finite(.data$gap_pred)) |>
    dplyr::group_by(.data$fullaqsid) |>
    dplyr::summarise(
      gap_pred    = mean(.data$gap_pred, na.rm = TRUE),
      gap_pred_se = mean(as.numeric(.data$gap_pred_se %||% NA_real_), na.rm = TRUE),
      .groups = "drop"
    )
  # F4 as AMENDED (Tim's directive 2026-07-31): `year_month` rows reach `gp` via
  # the monthly refinement, which the M9 arm DOES run — and now runs through the
  # constrained fitter, so what gets coalesced here obeys the same C1 cone as
  # h(d). The flag check stays so CPPORTAL_FECT_M9_MONTHLY_OFF=1 still yields
  # the monitor-level-only comparison arm at this mirror too.
  gp_month = if ("year_month" %in% names(gp) && fect_m9_monthly_refinement_enabled()) {
    gp |>
      dplyr::transmute(fullaqsid = .data$fullaqsid,
                       year_month = .d1_month_floor(.data$year_month),
                       gap_pred_m = as.numeric(.data$gap_pred)) |>
      dplyr::filter(is.finite(.data$gap_pred_m))
  } else {
    NULL
  }

  z = sm |>
    dplyr::inner_join(tb, by = "year_month") |>
    dplyr::inner_join(gp_unit, by = "fullaqsid")
  if (!is.null(gp_month)) {
    z = z |>
      dplyr::left_join(gp_month, by = c("fullaqsid", "year_month")) |>
      dplyr::mutate(gap_pred = dplyr::coalesce(.data$gap_pred_m, .data$gap_pred)) |>
      dplyr::select(-"gap_pred_m")
  }
  z = z |>
    dplyr::mutate(z = .data$sat_sqrt + .data$gap_pred - mu - .data$tau_bar) |>
    dplyr::filter(is.finite(.data$z))
  if (nrow(z) == 0L) return(bail("no (unit, month) cells survived the joins"))

  # pre/post tagging (§7.5(2))
  ts = if (!is.null(treat_start) && nrow(treat_start) > 0L) {
    tibble::as_tibble(treat_start) |>
      dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                       treat_start = as.Date(.data$treat_start)) |>
      dplyr::filter(!is.na(.data$treat_start)) |>
      dplyr::group_by(.data$fullaqsid) |>
      dplyr::summarise(treat_start = min(.data$treat_start), .groups = "drop")
  } else {
    NULL
  }
  if (!is.null(ts)) {
    z = z |> dplyr::left_join(ts, by = "fullaqsid")
  } else {
    z$treat_start = as.Date(NA)
  }
  z = z |>
    dplyr::mutate(
      period = dplyr::case_when(
        is.na(.data$treat_start)                                     ~ NA_character_,
        .data$year_month < .d1_month_floor(.data$treat_start)        ~ "pre",
        TRUE                                                         ~ "post"
      ),
      month_index = .d1_month_index(.data$year_month),
      moy         = as.integer(format(.data$year_month, "%m"))
    ) |>
    dplyr::arrange(.data$fullaqsid, .data$year_month)

  list(z = z, unit_tbl = unit_tbl, mu = mu)
}

#' Project each unit's monthly satellite series through the frozen fect fit to
#' estimate its unit level theta*_i.
#'
#'   z_i,M = sqrt(sat_i,M) + gap_pred_i,M - mu - tau_bar_M   ~=   theta_i + u_i,M
#'
#' LEVEL ONLY. See the block header for why no seasonality and no weather term
#' enter. The harmonic columns are diagnostics.
#'
#' This is the lambda = Inf member of the DESIGN §13 family: ONE level per unit,
#' estimated over every usable month. `stage_p_theta_star_lambda()` below is the
#' time-local generalisation; both read the same `.d1_build_z()` output.
#'
#' @param units Character vector of fullaqsid, or a tibble with `fullaqsid`
#'   (and optionally `metro_id`).
#' @param sat_monthly Tibble (`fullaqsid`, `year_month`, value) — value column
#'   named `sat_value` / `sat` / `value`, NATIVE units (the sqrt is taken here).
#'   Must cover the unobserved months too; see `fetch_sat_monitor_months()`.
#' @param gap_pred_tbl Tibble (`fullaqsid`, `gap_pred`, `gap_pred_se`) with an
#'   OPTIONAL `year_month` column carrying the R4 monthly refinement of the
#'   point correction (the SE stays monitor-level, exactly as in M7).
#' @param frozen `compute_frozen_components()` output — only `$mu` is used here.
#' @param tau_bar_ext Tibble (`year_month`, `tau_bar`) — from
#'   `compute_tau_backcast()`, or a plain monthly aggregation of
#'   `frozen$tau_time` when the backcast is unavailable.
#' @param treat_start Optional tibble (`fullaqsid`, `treat_start`) used to tag
#'   each month pre/post and to split theta* for the §7.5(2) stability test.
#' @param min_months Minimum usable months for a unit to get a theta* (default 12).
#' @param min_side Minimum months per side for the pre/post split (default 6).
#' @param hac_lag Optional HAC bandwidth override.
#' @return list(theta_star, z, meta) — NULL when no unit can be projected.
stage_p_project_units = function(units, sat_monthly, gap_pred_tbl, frozen,
                                 tau_bar_ext,
                                 treat_start = NULL,
                                 min_months = 12L,
                                 min_side = 6L,
                                 hac_lag = NULL,
                                 huber_k = 1.345) {
  bail = function(reason) {
    message("[fect.train.d1.stage_p] skip: ", reason)
    NULL
  }
  built = .d1_build_z(units, sat_monthly, gap_pred_tbl, frozen, tau_bar_ext,
                      treat_start = treat_start, .label = "stage_p")
  if (is.null(built)) return(NULL)
  z = built$z; unit_tbl = built$unit_tbl; mu = built$mu

  # ---- per-unit robust intercept + HAC SE ----------------------------------
  fit_one = function(d) {
    n = nrow(d)
    if (n < min_months) return(NULL)
    theta = .huber_location(d$z, k = huber_k)
    if (!is.finite(theta)) return(NULL)
    h = .d1_hac_se(d$z - theta, d$month_index, lag = hac_lag)
    hd = .d1_harmonic_diag(d$z, d$moy)
    side = function(p) {
      dd = d[!is.na(d$period) & d$period == p, , drop = FALSE]
      if (nrow(dd) < min_side) return(c(NA_real_, NA_real_, nrow(dd)))
      th = .huber_location(dd$z, k = huber_k)
      if (!is.finite(th)) return(c(NA_real_, NA_real_, nrow(dd)))
      hh = .d1_hac_se(dd$z - th, dd$month_index, lag = hac_lag)
      c(th, hh$se, nrow(dd))
    }
    pre  = side("pre"); post = side("post")
    # Cross-check only: HAC SE of the OLS intercept via sandwich, when present.
    nw = NA_real_
    if (requireNamespace("sandwich", quietly = TRUE) &&
        requireNamespace("lmtest", quietly = TRUE)) {
      nw = tryCatch({
        f = stats::lm(z ~ 1, data = d)
        sqrt(sandwich::NeweyWest(f, lag = h$lag, prewhite = FALSE)[1, 1])
      }, error = function(e) NA_real_)
    }
    tibble::tibble(
      fullaqsid           = d$fullaqsid[1],
      theta_star          = theta,
      theta_star_se       = h$se,
      theta_star_se_method = h$method,
      hac_lag             = h$lag,
      theta_star_se_nw_ols = nw,
      theta_star_mean     = mean(d$z),
      n_months            = n,
      n_pre               = as.integer(pre[3]),
      n_post              = as.integer(post[3]),
      n_months_tau_backcast = sum(d$tau_source == "backcast"),
      first_month         = min(d$year_month),
      last_month          = max(d$year_month),
      theta_star_pre      = pre[1],
      theta_star_pre_se   = pre[2],
      theta_star_post     = post[1],
      theta_star_post_se  = post[2],
      gap_pred            = d$gap_pred[1],
      gap_pred_se         = d$gap_pred_se[1],
      z_sd                = stats::sd(d$z),
      harm_r2             = hd$r2,
      harm_amp1           = hd$amp1,
      harm_amp2           = hd$amp2
    )
  }

  parts = lapply(split(z, z$fullaqsid), fit_one)
  parts = parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) {
    return(bail(paste0("no unit reached min_months=", min_months)))
  }
  theta_star = dplyr::bind_rows(parts)
  if ("metro_id" %in% names(unit_tbl)) {
    theta_star = theta_star |>
      dplyr::left_join(dplyr::distinct(unit_tbl, .data$fullaqsid, .keep_all = TRUE),
                       by = "fullaqsid")
  }

  message(sprintf(
    paste0("[fect.train.d1.stage_p] projected %d/%d unit(s); months/unit ",
           "median=%.0f min=%d max=%d; %d unit(s) use >=1 backcast tau month; ",
           "theta* range [%+.3f, %+.3f]; median HAC se=%.4f (lag median=%.0f)"),
    nrow(theta_star), nrow(unit_tbl),
    stats::median(theta_star$n_months), min(theta_star$n_months),
    max(theta_star$n_months), sum(theta_star$n_months_tau_backcast > 0L),
    min(theta_star$theta_star), max(theta_star$theta_star),
    stats::median(theta_star$theta_star_se, na.rm = TRUE),
    stats::median(theta_star$hac_lag, na.rm = TRUE)
  ))

  list(
    theta_star = theta_star,
    z = z,
    meta = list(
      mu = mu,
      n_units_requested = nrow(unit_tbl),
      n_units_projected = nrow(theta_star),
      min_months = min_months,
      min_side = min_side,
      huber_k = huber_k,
      weather = "omitted_v1_per_design_section_11",
      seasonality = "diagnostic_only_per_design_7_7_2"
    )
  )
}

# =============================================================================
# THE LAMBDA-WINDOW FAMILY (DESIGN §13, contract task 2026-07-31-012)
# =============================================================================
#
# §12 falsified the STATIC level (twin R2 = -1.97) and named the cause: the
# satellite<->monitor relationship is NON-STATIONARY (pre/post drift -0.57 at the
# OUT-of-cordon null). §13's resolution keeps Stage P but makes the level
# TIME-LOCAL and removes the part of the non-stationarity that is SHARED:
#
#   theta*_i(t; lambda) = robust level of { z_i,M - c(M) : |M - month(t)| <= lambda }
#
#   lambda = Inf     -> D1            (one level per unit; §12's falsified case)
#   lambda = 1 month -> D2 / C-prime  (contemporaneous anchor; M7's level rule
#                                      but with fect's beta / tau / sigma_eps)
#   1 < lambda < Inf -> new territory: anchor staleness vs monthly satellite noise
#
# c(M) is the SHARED drift curve: the cross-monitor robust location of the
# FITTED monitors' twin residuals r_i,M = z_i,M - (what the estimator is trying
# to recover). It is one curve for the whole panel — no per-unit parameter is
# added, so the cohesion argument that motivates Option D is untouched.
#
# WHAT c(M)'s TARGET MUST BE (the one judgement call, made explicit because it
# changes the numbers). Stage F predicts
#     sqrt(yhat0_it) = mu + theta*_i(t) + tau_t + (X_it - Xbar_i^obs)'beta
# against fect's own counterfactual
#     sqrt(Y.ct_it)  = mu + theta_i     + tau_t +  X_it'beta
# so the object theta* must recover is NOT theta_i but
#     Theta_i = theta_i + Xbar_i^obs'beta
# (plus, for any covariate EXCLUDED from Stage F's adjustment — sat_monthly_mean
# under ADR-0013 — that covariate's own contribution, which is month-varying).
# On the production fit Xbar_i'beta has mean -0.41 and sd 0.51 in sqrt units,
# i.e. BIGGER than sd(theta_i) = 0.30 — so scoring or centering against theta_i
# alone builds in a large constant and a real cross-monitor error. Hence
# `theta_target` here is a tibble the caller supplies, monitor-level or
# (monitor, month)-level, and the diagnostic harness passes Theta_i,M.
# =============================================================================

#' Per-monitor mean covariate contribution Xbar_i'beta on the model scale.
#'
#' The level Stage F needs theta* to carry (see the §13 block header): the
#' covariates enter Stage F as DEVIATIONS from the monitor's observed-period
#' mean, so the mean itself has to be inside the level. Covariates in `exclude`
#' (default `sat_monthly_mean`, ADR-0013) are left out because Stage F does not
#' adjust for them at all — their contribution is month-varying and belongs in a
#' month-varying target, not in this monitor-level constant.
#'
#' @param rows Panel/observed rows with `fullaqsid` and the covariate columns.
#' @param frozen `compute_frozen_components()` output (uses `$beta`).
#' @param x_vars Optional restriction of which betas to use.
#' @param exclude Covariates to leave out of the sum.
#' @return Tibble(fullaqsid, xbar_beta) — or NULL when no beta applies.
.d1_xbar_beta = function(rows, frozen, x_vars = NULL,
                         exclude = "sat_monthly_mean") {
  if (is.null(rows) || nrow(rows) == 0L) return(NULL)
  if (is.null(frozen$beta) || nrow(frozen$beta) == 0L) return(NULL)
  keep = as.character(frozen$beta$covariate)
  # x_vars may arrive as a one-sided FORMULA (spec$covariates). as.character()
  # on one returns c("~", "<whole RHS>"), which empties this intersect() and
  # silently drops the entire covariate-mean term from the drift target.
  # fect_cov_names() resolves either form; NULL still means "no restriction".
  if (!is.null(x_vars)) keep = intersect(keep, fect_cov_names(x_vars))
  keep = setdiff(keep, as.character(exclude))
  keep = intersect(keep, names(rows))
  if (!length(keep)) return(NULL)
  bh = stats::setNames(as.numeric(frozen$beta$beta),
                       as.character(frozen$beta$covariate))
  r = tibble::as_tibble(rows)
  r$fullaqsid = as.character(r$fullaqsid)
  r |>
    dplyr::group_by(.data$fullaqsid) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(keep),
                                   ~ mean(as.numeric(.x), na.rm = TRUE)),
                     .groups = "drop") |>
    dplyr::mutate(xbar_beta = as.numeric(
      as.matrix(dplyr::across(dplyr::all_of(keep))) %*% bh[keep])) |>
    dplyr::select("fullaqsid", "xbar_beta")
}

#' Shared satellite-vs-model drift curve c(M) (DESIGN §13).
#'
#' r_i,M = z_i,M - theta_target_i(,M); c(M) = robust cross-monitor location of
#' r_.,M, lightly smoothed. Subtracting c(M) from z removes the COMMON component
#' of §12's non-stationarity (the -0.57 pre/post drift was mostly shared: sd
#' 0.30 around it) without giving any unit a new parameter.
#'
#' SMOOTHING (`smooth`):
#'  * `"median3"` (default) — 3-month CENTERED running median over the calendar
#'    month index. Chosen over a wider or model-based smoother because the raw
#'    monthly curve is already an average over hundreds of monitors (its
#'    month-to-month noise is small), so the smoother only needs to suppress the
#'    occasional thin month; a 3-month median does that while preserving genuine
#'    level shifts, which a loess span would flatten. Gaps in the month index are
#'    respected — a neighbour 5 months away is not treated as adjacent.
#'  * `"loess"` — span-controlled loess on the month index, for sensitivity runs.
#'  * `"none"`  — c = c_raw.
#'
#' IN-SAMPLE-NESS: c(M) is estimated on the fitted monitors and (in the twin
#' test) applied to those same monitors, which is optimistic by O(1/n_units) per
#' month. With ~300 monitors per month the self-influence on any one monitor's
#' own correction is ~0.3% of the curve, so no leave-one-out is done; `n_units`
#' per month is returned so a thin month is visible.
#'
#' @param z `.d1_build_z()`'s `z` (needs fullaqsid, year_month, z).
#' @param theta_target Tibble with `fullaqsid`, `theta_target`, and OPTIONALLY
#'   `year_month` (month-varying target — use it when a covariate excluded from
#'   Stage F's adjustment moves the target within monitor).
#' @param smooth "median3" | "loess" | "none".
#' @param loess_span Span for `smooth = "loess"`.
#' @param huber_k Huber tuning constant for the cross-monitor location.
#' @param min_units Months with fewer contributing monitors get `c = NA` and are
#'   filled from the smoothed neighbours (never dropped silently).
#' @param exclude_cells M9-FREEZE c-hat decontamination (section 33c.3): tibble
#'   of `(fullaqsid, from_month)` and/or `(fullaqsid, year_month)` cells to drop
#'   from the shared pool — the TREATED in-cordon cells, never whole units. NULL
#'   (default) leaves the curve exactly as it is today.
#' @return list(curve, meta) — `curve` = tibble(year_month, month_index,
#'   n_units, c_raw, c); or NULL when it cannot be built.
compute_shared_drift_curve = function(z, theta_target,
                                      smooth = c("median3", "loess", "none"),
                                      loess_span = 0.3,
                                      huber_k = 1.345,
                                      min_units = 5L,
                                      exclude_cells = NULL) {
  smooth = match.arg(smooth)
  bail = function(reason) {
    message("[fect.train.d1.drift] skip: ", reason)
    NULL
  }
  if (is.null(z) || nrow(z) == 0L) return(bail("no z rows"))
  if (is.null(theta_target) || nrow(theta_target) == 0L) {
    return(bail("no theta_target"))
  }
  zz = tibble::as_tibble(z)
  if (!all(c("fullaqsid", "year_month", "z") %in% names(zz))) {
    return(bail("z needs fullaqsid, year_month and z"))
  }
  zz = zz |>
    dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                     year_month = .d1_month_floor(.data$year_month),
                     z = as.numeric(.data$z)) |>
    dplyr::filter(is.finite(.data$z))

  tt = tibble::as_tibble(theta_target)
  tt$fullaqsid = as.character(tt$fullaqsid)
  monthly_target = "year_month" %in% names(tt)
  tt = if (monthly_target) {
    tt |>
      dplyr::transmute(fullaqsid = .data$fullaqsid,
                       year_month = .d1_month_floor(.data$year_month),
                       theta_target = as.numeric(.data$theta_target)) |>
      dplyr::filter(is.finite(.data$theta_target))
  } else {
    tt |>
      dplyr::transmute(fullaqsid = .data$fullaqsid,
                       theta_target = as.numeric(.data$theta_target)) |>
      dplyr::filter(is.finite(.data$theta_target)) |>
      dplyr::distinct(.data$fullaqsid, .keep_all = TRUE)
  }
  r = if (monthly_target) {
    dplyr::inner_join(zz, tt, by = c("fullaqsid", "year_month"))
  } else {
    dplyr::inner_join(zz, tt, by = "fullaqsid")
  }
  if (nrow(r) == 0L) return(bail("no (unit, month) cell has both z and a target"))
  r$r = r$z - r$theta_target

  # ---- M9-FREEZE, c-hat decontamination (DESIGN section 33c.3) -------------
  # (C1)  { r_iM : i in fitted, and NOT ( i in-cordon AND M >= month(S_m(i)) ) }
  #
  # CELLS, NOT UNITS. Dropping the in-cordon units outright would remove them
  # from the PRE-treatment months too, where their residuals are legitimate and
  # where they are the only urban-core monitors in the pool — and because c(M) is
  # a CROSS-MONITOR location, changing the monitor mix month by month is itself a
  # source of level movement, the precise failure compute_tau_backcast() exists
  # to avoid. Two accepted shapes:
  #   * (fullaqsid, from_month) — drop every month >= from_month for that unit
  #     (the treated-cell rule; from_month = month(S_m)).
  #   * (fullaqsid, year_month) — drop exactly those cells.
  n_excluded = 0L
  if (!is.null(exclude_cells) && nrow(exclude_cells) > 0L) {
    ex = tibble::as_tibble(exclude_cells)
    ex$fullaqsid = as.character(ex$fullaqsid)
    drop = rep(FALSE, nrow(r))
    if ("from_month" %in% names(ex)) {
      exf = ex |>
        dplyr::transmute(fullaqsid = .data$fullaqsid,
                         .from_ = .d1_month_floor(.data$from_month)) |>
        dplyr::filter(!is.na(.data$.from_)) |>
        dplyr::group_by(.data$fullaqsid) |>
        dplyr::summarise(.from_ = min(.data$.from_), .groups = "drop")
      hit = exf$.from_[match(r$fullaqsid, exf$fullaqsid)]
      drop = drop | (!is.na(hit) & r$year_month >= hit)
    }
    if ("year_month" %in% names(ex)) {
      exc = ex |>
        dplyr::transmute(fullaqsid = .data$fullaqsid,
                         year_month = .d1_month_floor(.data$year_month)) |>
        dplyr::filter(!is.na(.data$year_month)) |>
        dplyr::distinct()
      drop = drop | (paste(r$fullaqsid, r$year_month) %in%
                       paste(exc$fullaqsid, exc$year_month))
    }
    n_excluded = sum(drop)
    if (n_excluded > 0L) {
      r = r[!drop, , drop = FALSE]
      message("[fect.train.d1.drift] c-hat decontamination: dropped ",
              n_excluded, " treated in-cordon (unit, month) cell(s) from the ",
              "shared drift pool")
    }
    if (nrow(r) == 0L) return(bail("decontamination removed every cell"))
  }

  curve = r |>
    dplyr::group_by(.data$year_month) |>
    dplyr::summarise(
      n_units = dplyr::n_distinct(.data$fullaqsid),
      c_raw   = .huber_location(.data$r, k = huber_k),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year_month) |>
    dplyr::mutate(month_index = .d1_month_index(.data$year_month))
  curve$c_raw[curve$n_units < min_units] = NA_real_
  if (all(!is.finite(curve$c_raw))) return(bail("every month is below min_units"))

  # ---- smoothing (gap-aware) -----------------------------------------------
  cs = curve$c_raw
  if (smooth == "median3") {
    idx = curve$month_index
    cs = vapply(seq_along(idx), function(j) {
      nb = which(abs(idx - idx[j]) <= 1L)
      v = curve$c_raw[nb]
      v = v[is.finite(v)]
      if (!length(v)) NA_real_ else stats::median(v)
    }, numeric(1))
  } else if (smooth == "loess") {
    ok = is.finite(curve$c_raw)
    fit = tryCatch(stats::loess(c_raw ~ month_index,
                                data = curve[ok, , drop = FALSE],
                                span = loess_span, degree = 1,
                                weights = curve$n_units[ok]),
                   error = function(e) NULL)
    if (is.null(fit)) {
      message("[fect.train.d1.drift] loess failed; falling back to c_raw")
    } else {
      cs = as.numeric(stats::predict(fit, newdata = curve))
    }
  }
  # Never leave a hole: months whose own estimate is missing inherit the nearest
  # smoothed month (constant extrapolation at the ends).
  if (any(!is.finite(cs))) {
    good = which(is.finite(cs))
    if (!length(good)) return(bail("smoothing produced no finite values"))
    cs[!is.finite(cs)] = cs[good[
      vapply(which(!is.finite(cs)),
             function(j) which.min(abs(curve$month_index[good] - curve$month_index[j])),
             integer(1))]]
  }
  curve$c = cs

  # ---- trend of the curve (§12 cause 2, quantified) ------------------------
  slope_per_decade = NA_real_
  fitl = tryCatch(stats::lm(c ~ month_index, data = curve), error = function(e) NULL)
  if (!is.null(fitl)) slope_per_decade = unname(stats::coef(fitl)[["month_index"]]) * 120

  meta = list(
    smooth = smooth, loess_span = loess_span, huber_k = huber_k,
    min_units = min_units,
    n_cells_excluded = n_excluded,
    decontaminated = n_excluded > 0L,
    target_grain = if (monthly_target) "unit_month" else "unit",
    n_months = nrow(curve),
    n_units  = dplyr::n_distinct(r$fullaqsid),
    first_month = min(curve$year_month), last_month = max(curve$year_month),
    c_range = range(curve$c),
    c_sd = stats::sd(curve$c),
    slope_per_decade = slope_per_decade,
    resid_sd_within_month = stats::sd(r$r - curve$c[match(r$year_month, curve$year_month)])
  )
  message(sprintf(
    paste0("[fect.train.d1.drift] c(M) over %d month(s) %s..%s from %d unit(s); ",
           "range [%+.3f, %+.3f] sd=%.3f trend=%+.3f/decade; ",
           "residual sd around c = %.3f (smooth=%s)"),
    meta$n_months, format(meta$first_month), format(meta$last_month),
    meta$n_units, meta$c_range[1], meta$c_range[2], meta$c_sd,
    meta$slope_per_decade, meta$resid_sd_within_month, smooth
  ))
  list(curve = curve, meta = meta)
}

#' Time-local unit level theta*_i(t; lambda) — the DESIGN §13 estimator.
#'
#' TRAILING (`align = "trailing"`, the DEFAULT — Tim's 2026-07-31 directive):
#'
#'   theta*_i(M0; lambda) = Huber location of
#'                          { z_i,M - c(M) : M0 - lambda <= M <= M0 }
#'
#' EXOGENEITY IS THE POINT. Every month that sets the level either PRECEDES or
#' COINCIDES WITH the month being predicted; no month after M0 ever enters. A
#' centered window would let post-treatment months inform the counterfactual
#' level of an earlier treated month — predicting an event from its own future —
#' and that is the look-ahead this default removes by construction. `lambda` is
#' therefore a LOOKBACK in months: the window spans lambda + 1 months, and
#' `lambda_months = 0` is the strictly contemporaneous single month (§13's D2 /
#' C-prime endpoint, the smallest window the harness sweeps). `lambda_months =
#' Inf` under trailing is an EXPANDING window — every month up to and including
#' M0 — which is NOT the same estimator as centered Inf.
#'
#' CENTERED (`align = "centered"`, LEGACY, kept only for reproducibility of the
#' DESIGN §14/§16 numbers):
#'
#'   theta*_i(M0; lambda) = Huber location of { z_i,M - c(M) : |M - M0| <= lambda }
#'
#' Here `lambda` is a HALF-WIDTH: the window spans 2*lambda + 1 months, and
#' `lambda_months = Inf` collapses to ONE level per unit over every month, i.e.
#' `stage_p_project_units()`'s D1 level (with c = 0 it is numerically identical;
#' the test pins that). The two alignments agree exactly at `lambda_months = 0`.
#'
#' SE. The level's own uncertainty shrinks with the window, so it cannot be the
#' D1 HAC number: se = s_i / sqrt(n_window), with s_i the monitor's robust sd
#' (1.4826 * MAD) of its c-corrected z around the LOCAL level — i.e. the scale of
#' exactly the noise the window averages over. For windows of >= 12 months the
#' lag-truncated HAC SE of the same residuals is also computed and the LARGER of
#' the two is used (monthly z is autocorrelated; taking the max never
#' under-states). Single-month windows get s_i itself (n = 1), which is the
#' honest statement that a one-month anchor carries one month of noise.
#'
#' @param z `.d1_build_z()`'s `z`.
#' @param lambda_months Width of the window in months (>= 0), or Inf. Under
#'   `align = "trailing"` this is the LOOKBACK (window = lambda + 1 months);
#'   under `align = "centered"` it is the HALF-WIDTH (window = 2*lambda + 1).
#' @param align `"trailing"` (default, EXOGENOUS: months <= the target month
#'   only) or `"centered"` (legacy `|M - M0| <= lambda`).
#' @param drift Optional tibble (`year_month`, `c`) from
#'   `compute_shared_drift_curve()`; months with no c get c = 0 (counted).
#' @param target_months Optional tibble (`fullaqsid`, `year_month`) listing the
#'   months to evaluate at (default: each unit's own z months). Months with an
#'   empty window are returned with `theta_star = NA` and `n_window = 0`.
#' @param min_window Minimum months in the window for a non-NA level (default 1).
#' @param huber_k Huber tuning constant.
#' @param hac_lag Optional HAC bandwidth override.
#' @param freeze_month M9-FREEZE (DESIGN section 33c): the registered `T_f` for
#'   this metro, as a Date (any day in the month) or NULL/NA for `T_f = +Inf`
#'   (never-treated units, the twin pool, every G2 quantity — untouched). With a
#'   finite `T_f` the window upper bound becomes `min(month(t), T_f)`, so no
#'   month after `T_f` ever enters any level, and the SE (`s_i` and the HAC
#'   residual set) is evaluated ONCE over months <= `T_f` and carried forward.
#' @param freeze_window_months `w` — window width used at frozen months; NULL
#'   means `w = lambda` (the registered rule: w = lambda = 24, zero new tuned
#'   parameters). `Inf` gives the registered EXPANDING sensitivity arm
#'   (`[first clean month, T_f]`).
#' @return list(theta_star, meta) — `theta_star` = tibble(fullaqsid, year_month,
#'   theta_star, theta_star_se, n_window, n_months_unit, se_method, gap_pred,
#'   gap_pred_se, ...); NULL when nothing is estimable.
stage_p_theta_star_lambda = function(z, lambda_months,
                                     align = c("trailing", "centered"),
                                     drift = NULL,
                                     target_months = NULL,
                                     min_window = 1L,
                                     huber_k = 1.345,
                                     hac_lag = NULL,
                                     freeze_month = NULL,
                                     freeze_window_months = NULL,
                                     freeze_drift = NULL) {
  bail = function(reason) {
    message("[fect.train.d1.lambda] skip: ", reason)
    NULL
  }
  align = tryCatch(match.arg(align), error = function(e) NA_character_)
  if (is.na(align)) return(bail("align must be 'trailing' or 'centered'"))
  trailing = identical(align, "trailing")
  if (is.null(z) || nrow(z) == 0L) return(bail("no z rows"))
  lam = suppressWarnings(as.numeric(lambda_months))[1]
  if (!is.finite(lam) && !identical(lam, Inf)) return(bail("lambda is not a number"))
  if (lam < 0) return(bail("lambda must be >= 0"))

  # ---- M9-FREEZE (section 33c) --------------------------------------------
  # T_f = NULL/NA is the untouched default (T_f = +Inf). A finite T_f clamps the
  # window's upper bound to min(month(t), T_f) — see the constant block for the
  # rule and why it exists.
  # `freeze_month` is EITHER a scalar Date (one metro's T_f, the common case)
  # OR a tibble(fullaqsid, freeze_month) for a pool that spans metros — the
  # per-unit resolution section 33c requires, since a mixed pool must never take
  # one city's T_f for another's monitors. Units absent from the map keep
  # T_f = +Inf.
  tf_map = NULL
  tf_date = as.Date(NA)
  if (is.data.frame(freeze_month)) {
    fm = tibble::as_tibble(freeze_month)
    if (all(c("fullaqsid", "freeze_month") %in% names(fm))) {
      fm = fm |>
        dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                         .tfmi = .d1_month_index(.d1_month_floor(
                           suppressWarnings(as.Date(.data$freeze_month))))) |>
        dplyr::filter(is.finite(.data$.tfmi)) |>
        dplyr::distinct(.data$fullaqsid, .keep_all = TRUE)
      if (nrow(fm) > 0L) {
        tf_map = stats::setNames(as.integer(fm$.tfmi), fm$fullaqsid)
      }
    } else {
      return(bail("freeze_month tibble needs fullaqsid and freeze_month"))
    }
  } else {
    tf_date = suppressWarnings(as.Date(freeze_month %||% NA))[1]
  }
  freeze_on = !is.na(tf_date) || !is.null(tf_map)
  tf_mi = if (!is.na(tf_date)) .d1_month_index(.d1_month_floor(tf_date)) else NA_integer_
  w_frozen = if (is.null(freeze_window_months)) lam else
    suppressWarnings(as.numeric(freeze_window_months))[1]
  if (freeze_on && (!is.finite(w_frozen) && !identical(w_frozen, Inf) || isTRUE(w_frozen < 0))) {
    return(bail("freeze_window_months must be >= 0 or Inf"))
  }

  # ---- M9-FREEZE-DRIFT (donor-drift correction) ----------------------------
  # The pure freeze holds the WHOLE anchor at T_f: theta*_frozen = huber_{M in
  # [T_f-w, T_f]}( z_i(M) - c(M) ), i.e. a frozen satellite level de-biased with
  # a frozen c. The unfrozen (M8) level at a post-T_f month M would be
  # huber( z_i(M') ) - c(M) over M' near M, so the freeze drops TWO terms:
  #   (a) the treated unit's own satellite movement since T_f  -- ENDOGENOUS
  #       after the policy starts; it must stay dropped ("you can't project the
  #       past with the future"), and
  #   (b) the movement of the SHARED sat-vs-model offset c(M) -- estimated on
  #       the fitted donor twins ONLY (and, with the freeze on, on their
  #       pre-treatment in-cordon cells removed; see compute_shared_drift_curve
  #       `exclude_cells`), so it carries no post-T_f information about the
  #       treated metro at all -- EXOGENOUS.
  # freeze_drift restores (b) and only (b):
  #     theta*_fd(M) = theta*_frozen(T_f) + [ c_window(T_f) - c(M) ]
  #                  = ( frozen satellite level ) - c(M)
  # so the anchor level stays frozen while the satellite->model calibration is
  # kept current with donor-only information. Note the SIGN: c enters the level
  # as `z - c`, so tracking the donor drift SUBTRACTS c(M) - c(T_f); adding it
  # would double the staleness rather than remove it.
  # Explicit argument wins, then an explicitly set env var, then the anchor
  # method (ON under M10, OFF under legacy M7).
  freeze_drift = if (is.null(freeze_drift)) {
    fect_anchor_component_on("CPPORTAL_FECT_M9_FREEZE_DRIFT")
  } else isTRUE(freeze_drift)

  # Only the CENTERED Inf case degenerates to a single static level per unit.
  # Trailing Inf is an expanding window and goes through the windowed path.
  static_level = is.infinite(lam) && !trailing

  zz = tibble::as_tibble(z)
  zz$fullaqsid = as.character(zz$fullaqsid)
  zz$year_month = .d1_month_floor(zz$year_month)
  if (!("month_index" %in% names(zz))) zz$month_index = .d1_month_index(zz$year_month)
  zz = zz |> dplyr::filter(is.finite(.data$z))
  if (nrow(zz) == 0L) return(bail("no finite z"))

  n_no_c = 0L
  if (!is.null(drift) && nrow(drift) > 0L) {
    dc = tibble::as_tibble(drift) |>
      dplyr::transmute(year_month = .d1_month_floor(.data$year_month),
                       .c = as.numeric(.data$c)) |>
      dplyr::filter(is.finite(.data$.c)) |>
      dplyr::distinct(.data$year_month, .keep_all = TRUE)
    zz = zz |> dplyr::left_join(dc, by = "year_month")
    miss = !is.finite(zz$.c)
    n_no_c = sum(miss)
    if (n_no_c > 0L) {
      # NEAREST-MONTH fill, never 0. z runs from the satellite record (1998) while
      # c(M) can only be estimated where fitted monitors report, so the earliest
      # months — exactly the pre-treatment months Option D exists to use — fall
      # outside the curve. Filling those with 0 would splice an UNCORRECTED month
      # into a window whose other months are corrected by ~0.5, i.e. manufacture a
      # step change in the level. Constant extrapolation from the nearest
      # estimated month is the conservative choice and is counted here.
      dc_mi = .d1_month_index(dc$year_month)
      zz$.c[miss] = dc$.c[vapply(.d1_month_index(zz$year_month[miss]),
                                 function(m) which.min(abs(dc_mi - m)), integer(1))]
    }
  } else {
    zz$.c = 0
  }
  zz$.zc = zz$z - zz$.c

  # Month -> c(M) lookup for the freeze-drift correction (nearest month, the
  # same constant-extrapolation rule the .c fill above uses). c(M) depends on
  # the month only, so one row per month_index is exact.
  cmap = zz |>
    dplyr::distinct(.data$month_index, .keep_all = FALSE) |>
    dplyr::arrange(.data$month_index)
  cmap$.c = zz$.c[match(cmap$month_index, zz$month_index)]
  cmap = cmap[is.finite(cmap$.c), , drop = FALSE]
  c_at = function(m) {
    if (!nrow(cmap)) return(rep(0, length(m)))
    cmap$.c[vapply(m, function(x) which.min(abs(cmap$month_index - x)), integer(1))]
  }
  n_freeze_drift = 0L

  # Evaluation grid: by default every month the unit has z for.
  tm = if (is.null(target_months)) {
    zz |> dplyr::distinct(.data$fullaqsid, .data$year_month)
  } else {
    tibble::as_tibble(target_months) |>
      dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                       year_month = .d1_month_floor(.data$year_month)) |>
      dplyr::distinct()
  }
  tm = tm |>
    dplyr::filter(.data$fullaqsid %in% unique(zz$fullaqsid)) |>
    dplyr::mutate(month_index = .d1_month_index(.data$year_month))
  if (nrow(tm) == 0L) return(bail("no target month belongs to a unit with z"))

  # Per-unit carry-alongs (the gap SE is monitor-level and day-persistent, as in
  # M7 and D1 — the window does not shrink it).
  if (!("gap_pred" %in% names(zz)))    zz$gap_pred = NA_real_
  if (!("gap_pred_se" %in% names(zz))) zz$gap_pred_se = NA_real_
  if (!("tau_source" %in% names(zz)))  zz$tau_source = NA_character_
  carry = zz |>
    dplyr::group_by(.data$fullaqsid) |>
    dplyr::summarise(
      gap_pred    = mean(.data$gap_pred, na.rm = TRUE),
      gap_pred_se = mean(.data$gap_pred_se, na.rm = TRUE),
      n_months_unit = dplyr::n(),
      first_month = min(.data$year_month), last_month = max(.data$year_month),
      n_months_tau_backcast = sum(.data$tau_source %in% "backcast"),
      .groups = "drop"
    )

  z_split  = split(zz, zz$fullaqsid)
  tm_split = split(tm, tm$fullaqsid)

  one_unit = function(uid) {
    d = z_split[[uid]]
    tg = tm_split[[uid]]
    if (is.null(d) || is.null(tg) || nrow(d) == 0L) return(NULL)
    # Per-unit T_f (mixed-metro pools); units off the map keep T_f = +Inf.
    tf_mi_u = if (!is.null(tf_map)) {
      # `[[` errors on an absent name; `[` returns NA, which IS the T_f = +Inf
      # default for a unit the registration does not cover.
      as.integer(unname(tf_map[uid]))
    } else tf_mi
    freeze_u = freeze_on && is.finite(tf_mi_u)
    d = d[order(d$month_index), , drop = FALSE]
    zc = d$.zc; mi = d$month_index

    lvl = numeric(nrow(tg)); nw = integer(nrow(tg))
    if (static_level) {
      one = .huber_location(zc, k = huber_k)
      lvl[] = one; nw[] = length(zc)
    } else {
      # Base subsetting on purpose (no dplyr masking of `mi`/`lam`). Trailing
      # closes the window AT the target month; centered extends lam months past
      # it. lam = Inf under trailing gives lo = 1 (an expanding window) because
      # month_index - Inf = -Inf and findInterval(-Inf, mi) = 0.
      #
      # M9-FREEZE: the closing month becomes min(month(t), T_f) and the width at
      # a frozen month becomes w. Months at or before T_f are BYTE-IDENTICAL to
      # the unfrozen rule (eff == month_index, width == lam), which is what makes
      # the pre-T_f half of the smoke test an equality test. Under the legacy
      # centered alignment the upper bound is clamped the same way, so no month
      # after T_f enters there either.
      eff = if (freeze_u) pmin(tg$month_index, tf_mi_u) else tg$month_index
      wid = if (freeze_u) ifelse(tg$month_index > tf_mi_u, w_frozen, lam) else
        rep(lam, nrow(tg))
      lo = findInterval(eff - wid - 1e-9, mi) + 1L
      hi_cap = if (freeze_u) tf_mi_u else Inf
      hi = if (trailing) findInterval(eff + 1e-9, mi) else
        findInterval(pmin(tg$month_index + lam, hi_cap) + 1e-9, mi)
      for (j in seq_len(nrow(tg))) {
        if (hi[j] < lo[j]) { lvl[j] = NA_real_; nw[j] = 0L; next }
        v = zc[lo[j]:hi[j]]
        nw[j] = length(v)
        lvl[j] = if (length(v) == 1L) v else .huber_location(v, k = huber_k)
      }
      # M9-FREEZE-DRIFT: at frozen months only, swap the window's (stale) c for
      # the target month's donor-estimated c. c_win is taken with the same
      # Huber location the level itself uses, so the swap is exact rather than
      # an approximation of the window average.
      if (freeze_u && freeze_drift) {
        frz = which(tg$month_index > tf_mi_u & is.finite(lvl))
        if (length(frz)) {
          cv = d$.c
          c_tgt = c_at(tg$month_index[frz])
          for (k2 in seq_along(frz)) {
            j = frz[k2]
            if (hi[j] < lo[j]) next
            w = cv[lo[j]:hi[j]]
            c_win = if (length(w) == 1L) w else .huber_location(w, k = huber_k)
            if (is.finite(c_win) && is.finite(c_tgt[k2]))
              lvl[j] = lvl[j] + (c_win - c_tgt[k2])
          }
          n_freeze_drift <<- n_freeze_drift + length(frz)
        }
      }
    }
    lvl[nw < min_window] = NA_real_

    # Local residual scale: z minus the level AT ITS OWN MONTH, so s_i measures
    # the noise the window is averaging, not the unit's long-run dispersion. A
    # month with no level of its own (not in `target_months`) contributes no
    # residual.
    lvl_at_own = if (static_level) rep(lvl[1], length(zc)) else
      lvl[match(mi, tg$month_index)]
    keep = is.finite(zc - lvl_at_own)
    # M9-FREEZE: "SEs are evaluated once, at T_f, and carried forward" (33c).
    # Post-T_f residuals are taken against a level that is deliberately stale, so
    # letting them into s_i / the HAC set would price anchor staleness as if it
    # were sampling noise. Staleness is measured separately (M9-9 placebo) and
    # enters as its own variance term, not through this back door.
    if (freeze_u) keep = keep & (mi <= tf_mi_u)
    res  = (zc - lvl_at_own)[keep]
    res_mi = mi[keep]
    s_i = if (length(res) >= 3L) 1.4826 * stats::mad(res) else
      if (length(res) >= 2L) stats::sd(res) else NA_real_
    if (!is.finite(s_i) || s_i <= 0) {
      s_i = if (length(zc) >= 2L) stats::sd(zc) else NA_real_
    }
    se = if (is.finite(s_i)) s_i / sqrt(pmax(nw, 1L)) else rep(NA_real_, length(nw))
    se_method = "local_scale_over_sqrt_n"
    if (length(res) >= 12L) {
      h = .d1_hac_se(res, res_mi, lag = hac_lag)
      # HAC is the long-run SE of a level estimated over the FULL series; scale
      # it back up to a window of n_window months before comparing, then take
      # the conservative maximum.
      if (is.finite(h$se)) {
        hac_win = h$se * sqrt(length(res) / pmax(nw, 1L))
        bigger = is.finite(hac_win) & is.finite(se) & hac_win > se
        if (any(bigger)) {
          se[bigger] = hac_win[bigger]
          se_method = "max_local_scale_and_hac"
        }
      }
    }
    out = tibble::tibble(
      fullaqsid = uid, year_month = tg$year_month,
      theta_star = lvl, theta_star_se = se, n_window = nw,
      theta_star_se_method = se_method, local_scale = s_i
    )
    # F10: the two M9-freeze columns are EMITTED ONLY WHEN THE FREEZE FLAG IS
    # ON. Attaching them unconditionally changed the theta_star schema on the
    # production (flag-off) path — a silent contract change for every consumer
    # that binds or column-checks this table, which is exactly what "default
    # OFF means byte-identical" is supposed to rule out.
    # Emission (33c.4): with the flag on, every M9 row carries its freeze month
    # so the serving layer can label the ribbon. NA = T_f is +Inf for this unit.
    if (isTRUE(freeze_on)) {
      out$theta_star_freeze_month =
        if (freeze_u) .d1_month_from_index(tf_mi_u) else as.Date(NA)
      out$theta_star_frozen =
        if (freeze_u) tg$month_index > tf_mi_u else rep(FALSE, nrow(tg))
    }
    out
  }

  parts = lapply(names(tm_split), one_unit)
  parts = parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) return(bail("no unit produced a level"))
  if (freeze_on && freeze_drift) {
    message("[fect.train.d1.lambda] M9-FREEZE-DRIFT: donor c(M) swapped in at ",
            n_freeze_drift, " frozen (unit, month) level(s) ",
            "(anchor level stays frozen at T_f; calibration tracks the ",
            "fitted-twin drift curve)")
  }
  theta_star = dplyr::bind_rows(parts) |> dplyr::left_join(carry, by = "fullaqsid")

  ok = is.finite(theta_star$theta_star)
  message(sprintf(
    paste0("[fect.train.d1.lambda] lambda=%s align=%s: %d (unit, month) level(s) ",
           "over %d unit(s); window months median=%.0f (min %d, max %d); theta* ",
           "range [%+.3f, %+.3f]; median se=%.4f; %d (unit, month) cell(s) took ",
           "a nearest-month c(M)"),
    if (is.infinite(lam)) "Inf" else format(lam), align,
    sum(ok), dplyr::n_distinct(theta_star$fullaqsid),
    stats::median(theta_star$n_window[ok]), min(theta_star$n_window[ok]),
    max(theta_star$n_window[ok]),
    min(theta_star$theta_star[ok]), max(theta_star$theta_star[ok]),
    stats::median(theta_star$theta_star_se[ok], na.rm = TRUE), n_no_c
  ))

  list(
    theta_star = theta_star,
    meta = list(
      lambda_months = lam,
      align = align,
      window_rule = if (freeze_on && trailing)
        "min(month(t), T_f) - w <= M <= min(month(t), T_f)"
        else if (trailing) "month(t) - lambda <= M <= month(t)"
        else if (freeze_on) "abs(M - month(t)) <= lambda, capped at T_f"
        else "abs(M - month(t)) <= lambda",
      exogenous = trailing,
      freeze_on = freeze_on,
      freeze_month = if (!is.na(tf_date)) .d1_month_floor(tf_date) else as.Date(NA),
      freeze_month_per_unit = !is.null(tf_map),
      n_units_frozen = if (is.null(tf_map)) NA_integer_ else length(tf_map),
      freeze_window_months = if (freeze_on) w_frozen else NA_real_,
      n_cells_frozen = sum(theta_star$theta_star_frozen %in% TRUE),
      drift_applied = !is.null(drift) && nrow(drift) > 0L,
      n_z_months_without_c = n_no_c,
      min_window = min_window,
      huber_k = huber_k,
      n_units = dplyr::n_distinct(theta_star$fullaqsid),
      n_cells = nrow(theta_star),
      weather = "omitted_v1_per_design_section_11",
      seasonality = "diagnostic_only_per_design_7_7_2"
    )
  )
}

# -----------------------------------------------------------------------------
# STAGE F — daily counterfactual on treated days (DESIGN §7.4, eq. (5'))
# -----------------------------------------------------------------------------
#' Reassemble fect's own counterfactual with the Stage-P unit level.
#'
#'   sqrt(yhat0_it) = mu + theta*_i + tau_t + (X_it - Xbar_i^obs)'beta
#'
#' COVARIATE CONVENTION (documented because it is the one judgement call here):
#' every covariate that carries a frozen beta enters DEVIATION-ONLY — weather
#' AND bg3 — against `Xbar_i^obs`, the monitor's mean of that covariate over its
#' OBSERVED rows, i.e. the rows passed in `rows` (its treated reporting days),
#' unless the caller supplies `xbar_tbl`. Rationale: Stage P omitted the
#' covariate term entirely (§11), so theta*_i already contains the monitor's
#' mean covariate contribution; re-adding a LEVEL here would double count it,
#' while the deviation carries the genuine day-to-day covariate signal fect
#' estimated. `sat_monthly_mean` is additionally excluded (ADR-0013) because it
#' is the anchor base — the satellite already set the level through Stage P.
#'
#' The consequence to be honest about: Xbar_i^obs is a POST-treatment mean for a
#' dropped monitor (those are the only days it has), so a covariate whose level
#' shifted with the policy has its shift absorbed into theta*_i rather than
#' attributed to the policy. That is the same assumption M7's within-monitor
#' centering already makes, and it is measured end-to-end by the twin-projection
#' falsification, not assumed away.
#'
#' @param rows Tibble of TREATED monitor-days: `metro_id`, `fullaqsid`, `date`,
#'   the model-scale outcome (`.y_model` by default) and the covariate columns.
#' @param theta_star_tbl `stage_p_project_units()$theta_star` (one level per
#'   unit) OR `stage_p_theta_star_lambda()$theta_star` (one level per (unit,
#'   month) — DESIGN §13). The presence of a `year_month` column is what selects
#'   the time-local path; rows with `year_month = NA` act as the unit-level
#'   fallback for months the window could not cover.
#' @param frozen `compute_frozen_components()` output (mu, tau_time, beta, sigma_eps).
#' @param x_vars Covariate columns to consider (default: everything with a beta).
#' @param xbar_tbl Optional (`fullaqsid`, one column per covariate) override for
#'   the observed-period means.
#' @return list(per_unit, per_metro_day, meta) — NULL when nothing is emittable.
stage_f_counterfactual = function(rows, theta_star_tbl, frozen,
                                  x_vars = NULL,
                                  exclude_covariates = "sat_monthly_mean",
                                  xbar_tbl = NULL,
                                  outcome_col = ".y_model",
                                  date_col = "date") {
  bail = function(reason) {
    message("[fect.train.d1.stage_f] skip: ", reason)
    NULL
  }
  if (is.null(rows) || nrow(rows) == 0L) return(bail("no treated rows"))
  if (is.null(theta_star_tbl) || nrow(theta_star_tbl) == 0L) {
    return(bail("no theta* table"))
  }
  if (is.null(frozen)) return(bail("no frozen components"))
  mu = suppressWarnings(as.numeric(frozen$mu %||% NA_real_))[1]
  if (!is.finite(mu)) return(bail("frozen$mu is not finite"))
  sigma_eps = suppressWarnings(as.numeric(frozen$sigma_eps %||% NA_real_))[1]
  if (!is.finite(sigma_eps)) sigma_eps = 0

  r = tibble::as_tibble(rows)
  for (nm in c("metro_id", "fullaqsid", date_col, outcome_col)) {
    if (!nm %in% names(r)) return(bail(paste0("rows lacks column `", nm, "`")))
  }
  r$fullaqsid = as.character(r$fullaqsid)
  r$.date_ = as.Date(r[[date_col]])
  r$.y_ = as.numeric(r[[outcome_col]])
  r = r |> dplyr::filter(is.finite(.data$.y_), !is.na(.data$.date_))
  if (nrow(r) == 0L) return(bail("no rows with a finite outcome"))

  # ---- beta ----------------------------------------------------------------
  beta_hat = list()
  if (!is.null(frozen$beta) && nrow(frozen$beta) > 0L) {
    bt = frozen$beta
    keep = as.character(bt$covariate)
    # Formula-safe: see fect_cov_names(). as.character() on the one-sided
    # covariates formula emptied this intersect(), leaving `beta_hat` empty and
    # Stage F's covariate adjustment identically zero.
    if (!is.null(x_vars)) keep = intersect(keep, fect_cov_names(x_vars))
    keep = setdiff(keep, as.character(exclude_covariates))
    keep = intersect(keep, names(r))
    for (k in keep) {
      v = suppressWarnings(as.numeric(bt$beta[match(k, as.character(bt$covariate))]))
      if (is.finite(v)) beta_hat[[k]] = v
    }
  }
  cov_keys = names(beta_hat)

  # ---- observed-period means (see the convention note above) ---------------
  if (length(cov_keys) > 0L) {
    if (is.null(xbar_tbl)) {
      xb = r |>
        dplyr::group_by(.data$fullaqsid) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(cov_keys),
                                       ~ mean(.x, na.rm = TRUE),
                                       .names = "xbar_{.col}"),
                         .groups = "drop")
    } else {
      xb = tibble::as_tibble(xbar_tbl)
      xb$fullaqsid = as.character(xb$fullaqsid)
      have = intersect(cov_keys, names(xb))
      xb = xb |>
        dplyr::select(dplyr::all_of(c("fullaqsid", have))) |>
        dplyr::rename_with(~ paste0("xbar_", .x), dplyr::all_of(have))
    }
    r = r |> dplyr::left_join(xb, by = "fullaqsid")
  }
  cov_adj = rep(0, nrow(r))
  for (k in cov_keys) {
    xbk = paste0("xbar_", k)
    if (!xbk %in% names(r)) next
    d = as.numeric(r[[k]]) - as.numeric(r[[xbk]])
    d[!is.finite(d)] = 0
    cov_adj = cov_adj + d * beta_hat[[k]]
  }
  r$cov_adj = cov_adj

  # ---- tau_t ---------------------------------------------------------------
  if (is.null(frozen$tau_time) || nrow(frozen$tau_time) == 0L) {
    return(bail("frozen$tau_time is empty"))
  }
  tt = frozen$tau_time |>
    dplyr::filter(!is.na(.data$date)) |>
    dplyr::transmute(.date_ = as.Date(.data$date), tau = as.numeric(.data$tau)) |>
    dplyr::filter(is.finite(.data$tau)) |>
    dplyr::distinct(.data$.date_, .keep_all = TRUE)
  n_before = nrow(r)
  r = r |> dplyr::inner_join(tt, by = ".date_")
  n_no_tau = n_before - nrow(r)
  if (nrow(r) == 0L) {
    return(bail("no treated day has a frozen tau_t (date mapping failed?)"))
  }
  if (n_no_tau > 0L) {
    message("[fect.train.d1.stage_f] dropped ", n_no_tau,
            " treated row(s) with no frozen tau_t (date outside the fit window)")
  }

  # ---- theta* --------------------------------------------------------------
  # TIME-LOCAL LEVELS (DESIGN §13): when `theta_star_tbl` carries a `year_month`
  # column the level is joined per (unit, MONTH of the day) — that is the ONLY
  # difference between the lambda-window family and D1 inside Stage F. Rows
  # whose month has no level fall back to the unit-level row the table may also
  # carry (year_month NA); with neither, the row is dropped rather than priced
  # off some other month's anchor.
  th_in = tibble::as_tibble(theta_star_tbl)
  th_in$fullaqsid = as.character(th_in$fullaqsid)
  time_local = "year_month" %in% names(th_in)
  n_no_level = 0L
  if (time_local) {
    th_m = th_in |>
      dplyr::filter(!is.na(.data$year_month)) |>
      dplyr::transmute(fullaqsid = .data$fullaqsid,
                       .ym_ = .d1_month_floor(.data$year_month),
                       theta_star = as.numeric(.data$theta_star),
                       theta_star_se = as.numeric(.data$theta_star_se),
                       gap_pred_se = as.numeric(.data$gap_pred_se)) |>
      dplyr::filter(is.finite(.data$theta_star)) |>
      dplyr::distinct(.data$fullaqsid, .data$.ym_, .keep_all = TRUE)
    th_u = th_in |>
      dplyr::filter(is.na(.data$year_month)) |>
      dplyr::transmute(fullaqsid = .data$fullaqsid,
                       theta_star_u = as.numeric(.data$theta_star),
                       theta_star_se_u = as.numeric(.data$theta_star_se),
                       gap_pred_se_u = as.numeric(.data$gap_pred_se)) |>
      dplyr::filter(is.finite(.data$theta_star_u)) |>
      dplyr::distinct(.data$fullaqsid, .keep_all = TRUE)
    r$.ym_ = .d1_month_floor(r$.date_)
    r = r |>
      dplyr::left_join(th_m, by = c("fullaqsid", ".ym_")) |>
      dplyr::left_join(th_u, by = "fullaqsid") |>
      dplyr::mutate(
        theta_star    = dplyr::coalesce(.data$theta_star, .data$theta_star_u),
        theta_star_se = dplyr::coalesce(.data$theta_star_se, .data$theta_star_se_u),
        gap_pred_se   = dplyr::coalesce(.data$gap_pred_se, .data$gap_pred_se_u)
      ) |>
      dplyr::select(-".ym_", -"theta_star_u", -"theta_star_se_u", -"gap_pred_se_u")
    n_no_level = sum(!is.finite(r$theta_star))
    r = r |> dplyr::filter(is.finite(.data$theta_star))
    if (n_no_level > 0L) {
      message("[fect.train.d1.stage_f] dropped ", n_no_level,
              " treated row(s) whose month has no time-local level")
    }
  } else {
    th = th_in |>
      dplyr::transmute(fullaqsid = .data$fullaqsid,
                       theta_star = as.numeric(.data$theta_star),
                       theta_star_se = as.numeric(.data$theta_star_se),
                       gap_pred_se = as.numeric(.data$gap_pred_se))
    r = r |> dplyr::inner_join(th, by = "fullaqsid") |>
      dplyr::filter(is.finite(.data$theta_star))
  }
  if (nrow(r) == 0L) return(bail("no treated row belongs to a projected unit"))

  # ---- counterfactual + D2b variance split ---------------------------------
  per_unit_raw = r |>
    dplyr::mutate(
      .yhat0_m = mu + .data$theta_star + .data$tau + .data$cov_adj,
      .yhat1_m = .data$.y_,
      # SHARED (day-persistent, one estimate per monitor): the projected level's
      # own uncertainty plus the satellite->monitor gap prediction uncertainty.
      .se0_m_shared = sqrt(
        dplyr::coalesce(.data$theta_star_se, 0)^2 +
        dplyr::coalesce(.data$gap_pred_se, 0)^2
      ),
      # INDEPENDENT (across days): fect's own residual scale. Unlike M7 there is
      # no control mean here, so nothing /n_ctrl-shrinks it.
      .se0_m_indep = sigma_eps,
      .se0_m = sqrt(.data$.se0_m_shared^2 + .data$.se0_m_indep^2),
      yhat0  = .d1_bt_mean_sq(.data$.yhat0_m, .data$.se0_m),
      yhat1  = .data$.yhat1_m^2,
      att    = .data$yhat1 - .data$yhat0,
      se_att = .d1_bt_sd_sq(.data$.yhat0_m, .data$.se0_m),
      se_att_shared_sq = .d1_bt_sd_sq(.data$.yhat0_m, .data$.se0_m_shared)^2,
      se_att_indep_sq  = .d1_bt_sd_sq(.data$.yhat0_m, .data$.se0_m_indep)^2,
      imp_method = FECT_D1_IMP_METHOD,
      # Covariates are centered on the monitor's own observed-period mean, the
      # same provenance label M7 uses for its control-free rows.
      centering = "within",
      n_ctrl = 0L,
      date = .data$.date_
    ) |>
    dplyr::filter(is.finite(.data$.yhat0_m))
  if (nrow(per_unit_raw) == 0L) return(bail("every counterfactual was non-finite"))

  # ---- per-monitor-day rows (mirrors the M7 `imputed_unit` layout) ---------
  per_unit = per_unit_raw |>
    dplyr::mutate(
      day        = .data$date,
      month      = lubridate::floor_date(.data$date, "month"),
      type       = FECT_D1_TYPE_UNIT,
      basis      = "anchored",
      yhatse1    = 0,
      yhatse0    = .data$se_att,
      t          = .data$att / .data$se_att,
      df         = NA_real_,
      p_value    = 2 * stats::pnorm(-abs(.data$t)),
      stars      = dplyr::case_when(
        !is.finite(.data$p_value) ~ NA_character_,
        .data$p_value < 0.001     ~ "***",
        .data$p_value < 0.01      ~ "**",
        .data$p_value < 0.05      ~ "*",
        TRUE                      ~ ""
      ),
      pct_change = NA_real_,
      n_effects  = 1L,
      n_units    = 1L,
      varc_shared_md = .data$se_att_shared_sq,
      varc_indep_md  = .data$se_att_indep_sq,
      # No leave-out calibration band for D1 in v1: the falsification is the
      # twin-projection test (§7.5(1)), reported separately rather than baked
      # into every row as a global constant.
      cal_bias = NA_real_, cal_q05 = NA_real_, cal_q95 = NA_real_
    ) |>
    dplyr::select(dplyr::any_of(c(
      "yhat1", "yhatse1", "yhat0", "yhatse0", "att", "se_att", "t", "df",
      "p_value", "stars", "pct_change", "n_effects", "type", "basis",
      "metro_id", "month", "fullaqsid", "day", "n_units", "n_ctrl",
      "imp_method", "centering", "cal_bias", "cal_q05", "cal_q95",
      "varc_shared_md", "varc_indep_md"
    )))

  # ---- (metro, day) rows (mirrors the M7 `imputed_agg` layout) -------------
  per_metro_day = per_unit_raw |>
    dplyr::group_by(.data$metro_id, .data$date) |>
    dplyr::summarise(
      yhat1   = mean(.data$yhat1, na.rm = TRUE),
      yhat0   = mean(.data$yhat0, na.rm = TRUE),
      att     = mean(.data$att,   na.rm = TRUE),
      se_att  = mean(.data$se_att, na.rm = TRUE),
      varc_shared_md = mean(.data$se_att_shared_sq, na.rm = TRUE),
      varc_indep_md  = mean(.data$se_att_indep_sq,  na.rm = TRUE),
      n_units = dplyr::n_distinct(.data$fullaqsid),
      n_ctrl  = 0,
      imp_method = paste(sort(unique(.data$imp_method)), collapse = "+"),
      centering  = paste(sort(unique(.data$centering)), collapse = "+"),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      day       = .data$date,
      month     = lubridate::floor_date(.data$date, "month"),
      type      = FECT_D1_TYPE_METRO_DAY,
      fullaqsid = NA_character_,
      yhatse1   = 0,
      yhatse0   = .data$se_att,
      se_att    = dplyr::if_else(is.finite(.data$se_att) & .data$se_att > 0,
                                 .data$se_att, NA_real_),
      t         = .data$att / .data$se_att,
      df        = NA_real_,
      p_value   = 2 * stats::pnorm(-abs(.data$t)),
      stars     = dplyr::case_when(
        !is.finite(.data$p_value) ~ NA_character_,
        .data$p_value < 0.001     ~ "***",
        .data$p_value < 0.01      ~ "**",
        .data$p_value < 0.05      ~ "*",
        TRUE                      ~ ""
      ),
      pct_change = NA_real_,
      n_effects = .data$n_units,
      cal_bias = NA_real_, cal_q05 = NA_real_, cal_q95 = NA_real_
    ) |>
    dplyr::select(dplyr::any_of(c(
      "yhat1", "yhatse1", "yhat0", "yhatse0", "att", "se_att", "t", "df",
      "p_value", "stars", "pct_change", "n_effects", "type", "metro_id",
      "month", "fullaqsid", "day", "n_units", "n_ctrl", "imp_method",
      "centering", "cal_bias", "cal_q05", "cal_q95",
      "varc_shared_md", "varc_indep_md"
    )))

  message(sprintf(
    paste0("[fect.train.d1.stage_f] %d %s row(s) over %d monitor(s) -> ",
           "%d %s row(s) over %d metro(s); covariates(dev-only)=%s; ",
           "sigma_eps=%.4f mu=%.4f"),
    nrow(per_unit), FECT_D1_TYPE_UNIT, dplyr::n_distinct(per_unit$fullaqsid),
    nrow(per_metro_day), FECT_D1_TYPE_METRO_DAY,
    dplyr::n_distinct(per_metro_day$metro_id),
    if (length(cov_keys)) paste(cov_keys, collapse = "+") else "(none)",
    sigma_eps, mu
  ))

  list(
    per_unit = per_unit,
    per_metro_day = per_metro_day,
    meta = list(
      mu = mu, sigma_eps = sigma_eps,
      covariates_deviation_only = cov_keys,
      covariates_excluded = as.character(exclude_covariates),
      xbar_convention = if (is.null(xbar_tbl))
        "monitor mean over the treated rows supplied (observed period)"
        else "caller-supplied xbar_tbl",
      n_rows_dropped_no_tau = n_no_tau,
      level_grain = if (time_local) "unit_month_time_local" else "unit_static",
      n_rows_dropped_no_level = n_no_level
    )
  )
}

# -----------------------------------------------------------------------------
# Monitor-MONTH satellite series — the Stage P input, and the ONE data
# dependency the design flagged as its biggest availability risk.
# -----------------------------------------------------------------------------
#' Fetch the monthly satellite series per monitor from `public.v_sat_monitor_month`.
#'
#' VERIFIED 2026-07-31 (read-only): this view is NOT panel-gated. It is built on
#' `public.sat_monitors` (ACAG source 15, monthly, `buffer_km = 0`), which holds
#' 301,542 monitor-month rows for 932 monitors covering 1998-01..2024-12 with the
#' view's CAMS-adjusted / carry-forward tail extending to the current month. Rows
#' exist for monitors that have NEVER reported AQ (e.g. 360610010, 702NEA000003
#' both have all 324 ACAG months and no `model_panel_daily` AQ row at all), and
#' every cordon monitor checked — ERG-VS1, ERG-BL0, ERG-MY7, ERG-KC1/KC2,
#' SWE_SE0022A/0027A/157992, ITA_IT1016A, SGNEA-SOUTH, 36061NY08454 — carries the
#' full 324-month history. So Stage P's projection window is set by the satellite
#' record (1998-01), not by when the monitor started reporting. That is what makes
#' London's pre-CCZ months (satellite 1998-01 vs panel 2003-05) and Stockholm's /
#' Milan's pre-policy months real, estimable data rather than an assumption.
#'
#' `= ANY($n::text[])` with an explicit array literal, never a bare vector param
#' (RPostgres binds a vector to a scalar and produces "malformed array literal").
#'
#' ---------------------------------------------------------------------------
#' STATEMENT TIMEOUT — the 2026-08-09 production degrade. READ THIS.
#' ---------------------------------------------------------------------------
#' `connect_db()` (`job/model/functions.R`) sets no `statement_timeout`, so the
#' train job inherits the database default, which on this Supabase instance is
#' **2 min** for the `postgres` role. `v_sat_monitor_month` is NOT cheap: its
#' `acag` / `cams` / `gap_filled` CTEs are each referenced more than once, so
#' Postgres materialises them and scans the whole of `public.sat_monitors`
#' (309k rows / 123 MB) plus a `generate_series` carry-forward tail and a final
#' `DISTINCT ON` sort **on every call, whatever the `fullaqsid` filter is**.
#' Measured 2026-08-09 off-peak: ~13 s filtered, ~31 s cold unfiltered. Under
#' the nightly's own load it crossed 120 s and the server cancelled it.
#'
#' The consequence was silent and expensive. The fetch returned NULL, so
#' `fect_emit_d1_rows()` bailed with "no monthly satellite series", Stage P/F
#' emitted nothing, and the truthful-degrade guard in `train_fect_bundle()`
#' fell all the way back to the **condemned M7** contemporaneous anchor — for
#' every spec in the 2026-08-09 07:53Z render, which still exited 0. Nothing in
#' the run failed; it just quietly stopped being M10.
#'
#' Two defences, both here:
#'   1. Raise `statement_timeout` for THIS query only (session-scoped, restored
#'      afterwards), configurable via `CPPORTAL_FECT_SAT_TIMEOUT_MS`.
#'   2. Fetch the whole (pollutant, buffer_km) series ONCE per process and
#'      filter `units` in R. The view's cost is dominated by those materialised
#'      CTEs, not by the row count returned, and the panel's unit set is very
#'      nearly the whole table anyway (970 of 970 PM2.5 monitors on the
#'      2026-08-09 panel), so the SQL-side unit filter bought nothing while
#'      each of the 22 specs paid the full view cost again.
#' Neither changes a single returned value: the R-side filter on `fullaqsid` is
#' exactly the predicate the SQL used to carry.
.FECT_SAT_MONTH_CACHE = new.env(parent = emptyenv())

#' Session-scoped `statement_timeout` bump for one expensive read.
#' Returns the previous value so the caller can restore it. Fail-soft: a role
#' that may not `SET` simply keeps whatever it had.
.fect_with_stmt_timeout = function(db, ms) {
  prev = tryCatch(DBI::dbGetQuery(db, "SHOW statement_timeout")[[1]][1],
                  error = function(e) NA_character_)
  ok = tryCatch({
    DBI::dbExecute(db, sprintf("SET statement_timeout = %d", as.integer(ms)))
    TRUE
  }, error = function(e) {
    message("[fect.train.d1.sat] could not raise statement_timeout (",
            conditionMessage(e), ") — running under the inherited ", prev)
    FALSE
  })
  if (ok) message("[fect.train.d1.sat] statement_timeout ", prev, " -> ",
                  as.integer(ms), "ms for the monthly satellite fetch")
  list(prev = prev, ok = ok)
}

.fect_restore_stmt_timeout = function(db, st) {
  if (!isTRUE(st$ok)) return(invisible(NULL))
  tryCatch(DBI::dbExecute(db, if (is.na(st$prev) || !nzchar(st$prev))
    "SET statement_timeout = DEFAULT" else
      sprintf("SET statement_timeout = '%s'", st$prev)),
    error = function(e) NULL)
  invisible(NULL)
}

fetch_sat_monitor_months = function(db, spec, units = NULL, pollutant = NULL,
                                    buffer_km = 0) {
  if (is.null(db)) return(NULL)
  poll = as.character(pollutant %||% spec$pollutant %||% "PM2.5")
  ids = if (!is.null(units) && length(units) > 0L)
    unique(as.character(units)) else NULL

  # ---- one full fetch per (pollutant, buffer_km) per process ----------------
  ckey = paste0(poll, "|", as.numeric(buffer_km))
  out = .FECT_SAT_MONTH_CACHE[[ckey]]
  if (!is.null(out)) {
    message("[fect.train.d1.sat] cache hit for pollutant=", poll,
            " buffer_km=", buffer_km, " (", nrow(out), " row(s))")
  } else {
    q = paste0(
      "SELECT fullaqsid, metro_id, make_date(year, month, 1) AS year_month, ",
      "       value AS sat_value, method AS sat_method ",
      "FROM public.v_sat_monitor_month ",
      "WHERE pollutant = $1 AND buffer_km = $2 AND value IS NOT NULL"
    )
    params = list(poll, as.numeric(buffer_km))
    tmo = suppressWarnings(as.integer(Sys.getenv(
      "CPPORTAL_FECT_SAT_TIMEOUT_MS", "900000")))
    if (!is.finite(tmo) || is.na(tmo) || tmo <= 0L) tmo = 900000L
    st = .fect_with_stmt_timeout(db, tmo)
    on.exit(.fect_restore_stmt_timeout(db, st), add = TRUE)
    run = function() {
      t0 = Sys.time()
      r = DBI::dbGetQuery(db, q, params = params)
      message(sprintf("[fect.train.d1.sat] view read took %.1f s",
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      r
    }
    out = tryCatch(run(), error = function(e) {
      message("[fect.train.d1.sat] monthly satellite fetch failed: ",
              conditionMessage(e), " — retrying once")
      tryCatch(run(), error = function(e2) {
        message("[fect.train.d1.sat] retry also failed: ", conditionMessage(e2))
        NULL
      })
    })
    if (!is.null(out) && nrow(out) > 0L) .FECT_SAT_MONTH_CACHE[[ckey]] = out
  }

  if (is.null(out) || nrow(out) == 0L) {
    message("[fect.train.d1.sat] no rows from v_sat_monitor_month (pollutant=",
            poll, ")")
    return(NULL)
  }
  # R-side unit filter — byte-identical to the old `fullaqsid = ANY($3::text[])`.
  if (!is.null(ids)) out = out[as.character(out$fullaqsid) %in% ids, , drop = FALSE]
  if (nrow(out) == 0L) {
    message("[fect.train.d1.sat] no rows from v_sat_monitor_month for the ",
            length(ids), " requested unit(s) (pollutant=", poll, ")")
    return(NULL)
  }
  out = tibble::as_tibble(out) |>
    dplyr::mutate(fullaqsid = as.character(.data$fullaqsid),
                  metro_id = suppressWarnings(as.integer(.data$metro_id)),
                  year_month = as.Date(.data$year_month),
                  sat_value = as.numeric(.data$sat_value))
  message(sprintf(
    "[fect.train.d1.sat] %d monitor-month row(s), %d monitor(s), %s..%s",
    nrow(out), dplyr::n_distinct(out$fullaqsid),
    format(min(out$year_month)), format(max(out$year_month))
  ))
  out
}

# -----------------------------------------------------------------------------
# Orchestrator — the single call site in train_fect_bundle()
# -----------------------------------------------------------------------------
#' Run Stage P + Stage F and return the D1 rows to append to `bundle$att`.
#'
#' Fail-soft by construction: every step that can fail returns NULL with a
#' message, and the caller wraps the whole thing in tryCatch. With
#' `CPPORTAL_FECT_D1_EXPORT` unset (the default) this returns NULL immediately
#' and nothing else in this file executes.
#'
#' @param imputed The `impute_dropped_zone_atts()` return value; its (gated)
#'   `gap_pred` / `gap_pred_monthly` / `gap_pred_fitted` / `dropped_units`
#'   attributes carry the M7 gap model forward so Stage P does not refit it.
#' @param lambda_months DESIGN §13 window half-width in months for the unit
#'   level; `Inf` is the static D1 level of §12. Default
#'   `FECT_D1_DEFAULT_LAMBDA_MONTHS` (see that constant for the sweep that set
#'   it). A finite lambda makes the level TIME-LOCAL, which Stage F consumes via
#'   the `year_month` column on the theta* table.
#' @param align Window alignment for that level: `"trailing"` (default, from
#'   `FECT_D1_WINDOW_ALIGN` — EXOGENOUS, months <= the predicted month only) or
#'   `"centered"` (legacy). See `stage_p_theta_star_lambda()`.
#' @param drift_curve Optional tibble (`year_month`, `c`) — the SHARED
#'   satellite-vs-model drift curve subtracted from z (§13). Supply
#'   `compute_shared_drift_curve()$curve` to override; leave NULL to have it
#'   estimated here from the FITTED monitors' twin residuals, which needs gap
#'   predictions for the fitted units (`attr(imputed, "gap_pred_fitted")`, or the
#'   `gap_pred_fitted` argument). With neither, the family runs UNCORRECTED and
#'   says so — the level is still time-local, only the shared drift stays in.
#' @param gap_pred_fitted Optional tibble (`fullaqsid`, `gap_pred`,
#'   `gap_pred_se`, optional `year_month`) of gap predictions for the FITTED
#'   monitors, used only to estimate `drift_curve`.
fect_emit_d1_rows = function(m, panel, spec, db,
                             imputed = NULL,
                             zone_pairs = NULL,
                             zone_monitor_ids = NULL,
                             frozen = NULL,
                             lambda_months = FECT_D1_DEFAULT_LAMBDA_MONTHS,
                             align = FECT_D1_WINDOW_ALIGN,
                             drift_curve = NULL,
                             gap_pred_fitted = NULL,
                             freeze = NULL,
                             freeze_arm = FECT_M9_TF_ARM_DEFAULT,
                             freeze_window_months = NULL,
                             force = FALSE) {
  # `force = TRUE` is the M10 PRODUCTION path: under CPPORTAL_FECT_ANCHOR_METHOD
  # = M10 the Stage P/F construction IS the anchored counterfactual, so it must
  # run whether or not the diagnostic export gate is on.
  if (!isTRUE(force) && !fect_d1_export_enabled()) return(NULL)
  bail = function(reason) {
    message("[fect.train.d1] skip: ", reason)
    NULL
  }
  message("[fect.train.d1] ", if (isTRUE(force)) "M10 production path" else
            "CPPORTAL_FECT_D1_EXPORT is ON",
          " — building Option D Stage P/F rows (types ", FECT_D1_TYPE_UNIT,
          " / ", FECT_D1_TYPE_METRO_DAY, ")")

  if (is.null(m) || is.null(panel) || nrow(panel) == 0L) return(bail("no fit/panel"))
  .dt0 = Sys.time(); .dlast = .dt0
  .dlap = function(stage) {
    now = Sys.time()
    message(sprintf("[fect.d1.timing] stage=%s stage_s=%.1f total_s=%.1f", stage,
                    as.numeric(difftime(now, .dlast, units = "secs")),
                    as.numeric(difftime(now, .dt0, units = "secs"))))
    .dlast <<- now
    invisible(NULL)
  }
  frozen = frozen %||% compute_frozen_components(m, panel, spec)
  .dlap("compute_frozen_components")
  if (is.null(frozen)) return(bail("frozen components unavailable"))

  # ---- model-scale response -------------------------------------------------
  # Stage F filters on `.y_model`, but that column is NOT part of the panel the
  # train hands us: `impute_dropped_zone_atts()` derives it on its own LOCAL
  # copy (see the `panel$.y_model = ...` line there), and the D-arm runner
  # derives it before calling us. The in-train call site passes the raw panel,
  # so Stage F died with "In argument: `is.finite(.data$.y_model)`" AFTER Stage
  # P had done all its work — the last seam in the M10 production path
  # (2026-08-03; the two earlier ones were the export gate and the deploy
  # manifest). Derive it here from the spec's own outcome formula so this
  # function is self-sufficient whichever caller invokes it.
  if (!".y_model" %in% names(panel)) {
    lhs_expr_d1 = if (!is.null(spec$outcome)) spec$outcome[[2L]] else NULL
    if (is.null(lhs_expr_d1))
      return(bail("panel lacks .y_model and spec$outcome is NULL — cannot derive the model-scale response"))
    panel$.y_model = suppressWarnings(
      as.numeric(eval(lhs_expr_d1, envir = panel, enclos = baseenv())))
    if (!any(is.finite(panel$.y_model)))
      return(bail("derived .y_model is entirely non-finite"))
    message("[fect.train.d1] derived .y_model from spec$outcome (",
            sum(is.finite(panel$.y_model)), " finite of ", nrow(panel), " rows)")
  }

  # ---- unit sets ------------------------------------------------------------
  fitted_units = as.character(m$id)
  zone_units = if (!is.null(zone_pairs) && nrow(zone_pairs) > 0L) {
    unique(as.character(zone_pairs$fullaqsid))
  } else {
    character(0)
  }
  dropped_units = attr(imputed, "dropped_units") %||% setdiff(zone_units, fitted_units)
  dropped_units = as.character(dropped_units)
  if (length(dropped_units) == 0L) return(bail("no dropped units to project"))

  # Out-of-CORDON monitors carry the tau backcast pool. Same exclusion rule the
  # M7 gap model uses (Option A): exclude in-zone monitors, keep everything else.
  in_cordon = unique(c(as.character(zone_monitor_ids %||% character(0)), zone_units))
  panel_units = unique(as.character(panel$fullaqsid))
  out_of_cordon = setdiff(panel_units, in_cordon)
  if (length(out_of_cordon) == 0L) return(bail("no out-of-cordon monitors for the tau pool"))

  # ---- M9-FREEZE resolution (DESIGN section 33c) — DEFAULT OFF -------------
  # `freeze = NULL` defers to CPPORTAL_FECT_M9_FREEZE; TRUE/FALSE overrides it
  # (the diagnostics runner passes an explicit value). A metro with no registered
  # T_f — never-treated, Madrid, Taipei, and Singapore, which is registered as
  # NOT FREEZABLE because the ALS predates the satellite record — comes back
  # freeze_month = NA and runs exactly as it does today.
  #
  # PER-UNIT T_f, NOT PER-PANEL (fixed 2026-08-03). The TRAINING panel spans
  # every metro in the fit (~20), so `panel$metro_id` is NOT a scalar. The
  # previous `unique(panel$metro_id)[1]` took the FIRST metro's T_f and applied
  # it to EVERY metro: the log read "metro 1 T_f=2024-12-01" (NYC's) and Milan
  # (registered T_f = 2007-12), Singapore and the Nordics were all frozen at a
  # month a decade+ after their own implementation. That produced anchored
  # per_metro ATTs with the wrong magnitude and, for Milan, the wrong SIGN
  # (+0.6925 where the runner-based reference is -3.6893). Resolve T_f per unit
  # from the unit's OWN metro — exactly what run_d1_falsification.R does
  # (`tf_drop` / `tf_c`), and what stage_p_theta_star_lambda() has always
  # accepted via tibble(fullaqsid, freeze_month). A genuinely single-metro panel
  # keeps the scalar path, bit-for-bit.
  m9_freeze = if (is.null(freeze)) fect_m9_freeze_enabled() else isTRUE(freeze)
  panel_metro_map = tibble::tibble(
    fullaqsid = as.character(panel$fullaqsid),
    metro_id  = suppressWarnings(as.integer(panel$metro_id))) |>
    dplyr::filter(!is.na(.data$metro_id)) |>
    dplyr::distinct(.data$fullaqsid, .keep_all = TRUE)
  metro_ids_all = sort(unique(panel_metro_map$metro_id))
  multi_metro = length(metro_ids_all) > 1L
  metro_id_this = if (length(metro_ids_all)) metro_ids_all[1] else NA_integer_
  tf = list(freeze_month = as.Date(NA), tf_basis = NA_character_, freezable = FALSE)
  tf_by_metro = NULL   # tibble(metro_id, freeze_month, tf_basis)
  tf_unit = NULL       # tibble(fullaqsid, metro_id, freeze_month) — frozen units only
  if (m9_freeze) {
    tf_list = lapply(metro_ids_all, function(k)
      fect_m9_freeze_month(k, arm = freeze_arm))
    tf_by_metro = tibble::tibble(
      metro_id     = metro_ids_all,
      freeze_month = as.Date(vapply(tf_list, function(r)
        if (is.na(r$freeze_month)) NA_real_ else as.numeric(r$freeze_month),
        numeric(1)), origin = "1970-01-01"),
      tf_basis     = vapply(tf_list, function(r)
        as.character(r$tf_basis %||% NA_character_)[1], character(1)))
    tf = if (length(tf_list)) tf_list[[1]] else tf
    tf_unit = panel_metro_map |>
      dplyr::left_join(tf_by_metro |> dplyr::select("metro_id", "freeze_month"),
                       by = "metro_id") |>
      dplyr::filter(!is.na(.data$freeze_month))
    unfrozen_metros = tf_by_metro$metro_id[is.na(tf_by_metro$freeze_month)]
    if (nrow(tf_unit) == 0L) {
      message("[fect.train.d1] M9-FREEZE is ON but NO metro in the panel (",
              paste(metro_ids_all, collapse = ","), ") has a registered T_f ",
              "(arm=", freeze_arm, ") — running UNFROZEN (T_f = +Inf)")
    } else {
      message("[fect.train.d1] M9-FREEZE ON: ", nrow(tf_unit), " unit(s) across ",
              dplyr::n_distinct(tf_unit$metro_id), " metro(s) frozen; ",
              dplyr::n_distinct(tf_unit$freeze_month), " distinct T_f = ",
              paste(format(sort(unique(tf_unit$freeze_month))), collapse = ", "),
              "; UNFROZEN metro(s) = ",
              if (!length(unfrozen_metros)) "none" else
                paste(unfrozen_metros, collapse = ","),
              "; arm=", freeze_arm, " w=",
              if (is.null(freeze_window_months)) "lambda" else
                format(freeze_window_months))
      for (i in seq_len(nrow(tf_by_metro))) {
        message("[fect.train.d1]   T_f metro ", tf_by_metro$metro_id[i], " = ",
                if (is.na(tf_by_metro$freeze_month[i])) "none (UNFROZEN)" else
                  paste0(format(tf_by_metro$freeze_month[i]), " basis=",
                         tf_by_metro$tf_basis[i]))
      }
    }
  }
  # `freeze_per_unit` drives the multi-metro path; single-metro keeps the scalar
  # so nothing about a one-city fit changes.
  freeze_per_unit = m9_freeze && multi_metro &&
    !is.null(tf_unit) && nrow(tf_unit) > 0L
  tf_month = if (multi_metro) as.Date(NA) else tf$freeze_month
  freeze_active = m9_freeze &&
    ((!multi_metro && !is.na(tf_month)) || freeze_per_unit)
  # ONE lookup every downstream freeze site keys off: fullaqsid -> that unit's
  # own T_f. Single-metro fills it with the scalar (identical behaviour); a
  # metro with no registered T_f contributes no rows, so its units stay
  # UNFROZEN individually rather than dragging the whole panel unfrozen.
  tf_lookup = if (freeze_per_unit) {
    tf_unit |> dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                                freeze_month = as.Date(.data$freeze_month))
  } else if (freeze_active) {
    panel_metro_map |> dplyr::transmute(fullaqsid = .data$fullaqsid,
                                        freeze_month = as.Date(tf_month))
  } else NULL

  # ---- satellite ------------------------------------------------------------
  sat = fetch_sat_monitor_months(db, spec, units = c(dropped_units, out_of_cordon))
  .dlap("fetch_sat_monitor_months")
  if (is.null(sat)) return(bail("no monthly satellite series"))
  sat_ooc = sat |> dplyr::filter(.data$fullaqsid %in% out_of_cordon)
  sat_drp = sat |> dplyr::filter(.data$fullaqsid %in% dropped_units)
  if (nrow(sat_drp) == 0L) {
    return(bail("satellite has no months for ANY dropped unit — Stage P cannot run"))
  }

  # ---- tau backcast (§10) ---------------------------------------------------
  tau_bc = tryCatch(compute_tau_backcast(frozen$tau_time, sat_ooc),
                    error = function(e) {
                      message("[fect.train.d1] tau backcast errored: ",
                              conditionMessage(e)); NULL
                    })
  tau_bar_ext = if (!is.null(tau_bc)) {
    tau_bc$tau_bar_ext
  } else {
    # Frozen months only — Stage P still runs, on a shorter window.
    message("[fect.train.d1] falling back to frozen tau months only (no backcast)")
    frozen$tau_time |>
      dplyr::filter(!is.na(.data$date), is.finite(.data$tau)) |>
      dplyr::mutate(year_month = .d1_month_floor(.data$date)) |>
      dplyr::group_by(.data$year_month) |>
      dplyr::summarise(tau_bar = mean(.data$tau), .groups = "drop") |>
      dplyr::mutate(tau_bar_se = 0, source = "frozen")
  }

  # ---- gap predictions (reused from the M7 machinery) ----------------------
  gap_pred_tbl = attr(imputed, "gap_pred")
  gap_pred_mm  = attr(imputed, "gap_pred_monthly")
  if (is.null(gap_pred_tbl) || nrow(gap_pred_tbl) == 0L) {
    return(bail(paste0("no gap predictions available (M7 gap model unavailable, ",
                       "or CPPORTAL_FECT_D1_EXPORT was flipped on after the ",
                       "imputation ran)")))
  }
  # F4 as AMENDED (Tim's directive 2026-07-31): `gap_pred_monthly` IS produced
  # on the M9 arm — by the constrained monthly fitter — so this branch is live
  # and the D1 export carries the shape-consistent monthly correction with the
  # constrained monitor-level h(d) as fallback. The explicit flag check is what
  # CPPORTAL_FECT_M9_MONTHLY_OFF=1 flips for the comparison arm.
  if (!is.null(gap_pred_mm) && nrow(gap_pred_mm) > 0L && ".ym" %in% names(gap_pred_mm) &&
      fect_m9_monthly_refinement_enabled()) {
    gap_month = gap_pred_mm |>
      dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                       year_month = as.Date(paste0(.data$.ym, "-01")),
                       gap_pred = as.numeric(.data$gap_pred_m))
    gap_in = dplyr::bind_rows(
      tibble::as_tibble(gap_pred_tbl),
      gap_month
    )
  } else {
    gap_in = tibble::as_tibble(gap_pred_tbl)
  }

  # ---- treatment starts for the pre/post tagging ---------------------------
  treat_start = if (!is.null(zone_pairs) && nrow(zone_pairs) > 0L &&
                    "start_date" %in% names(zone_pairs)) {
    tibble::as_tibble(zone_pairs) |>
      dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                       treat_start = as.Date(.data$start_date))
  } else {
    NULL
  }

  unit_tbl = panel |>
    dplyr::filter(.data$fullaqsid %in% dropped_units) |>
    dplyr::distinct(.data$fullaqsid, .data$metro_id)
  if (nrow(unit_tbl) == 0L) {
    unit_tbl = sat_drp |> dplyr::distinct(.data$fullaqsid, .data$metro_id)
  }

  sp = stage_p_project_units(unit_tbl, sat_drp, gap_in, frozen, tau_bar_ext,
                             treat_start = treat_start)
  if (is.null(sp)) return(bail("Stage P produced no theta*"))

  # ---- shared drift curve c(M) (DESIGN §13) ---------------------------------
  # Estimated on the FITTED monitors (they are the only ones with a fect truth
  # to residualise against) and applied to the DROPPED ones. Needs gap
  # predictions for the fitted set; when they are unavailable the family still
  # runs, uncorrected, and the meta says so.
  drift_meta = list(source = "none")
  if (is.null(drift_curve)) {
    gpf = gap_pred_fitted %||% attr(imputed, "gap_pred_fitted")
    if (!is.null(gpf) && nrow(gpf) > 0L && !is.null(frozen$theta_unit)) {
      sat_fit = sat |> dplyr::filter(.data$fullaqsid %in% as.character(gpf$fullaqsid))
      zb = if (nrow(sat_fit) == 0L) NULL else
        .d1_build_z(unique(as.character(gpf$fullaqsid)), sat_fit, gpf, frozen,
                    tau_bar_ext, .label = "drift")
      if (!is.null(zb)) {
        # Target = theta_i + the monitor's mean covariate contribution, because
        # Stage F re-adds covariates DEVIATION-ONLY (see the §13 block header).
        # spec$covariates is a one-sided formula; .d1_xbar_beta() resolves it
        # through fect_cov_names(). Passing it raw used to empty that
        # function's intersect(), so it returned NULL and the drift target
        # silently lost its covariate-mean term (Xbar_i'beta).
        xb = .d1_xbar_beta(panel, frozen, x_vars = spec$covariates,
                           exclude = "sat_monthly_mean")
        tgt = frozen$theta_unit |>
          dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                           theta_target = as.numeric(.data$theta))
        if (!is.null(xb)) {
          tgt = tgt |>
            dplyr::left_join(xb, by = "fullaqsid") |>
            dplyr::mutate(theta_target = .data$theta_target +
                            dplyr::coalesce(.data$xbar_beta, 0)) |>
            dplyr::select(-"xbar_beta")
        }
        # c-hat decontamination (33c.3): the FITTED in-cordon monitors' TREATED
        # cells leave the shared pool from month(S_m) on. Their pre-treatment
        # cells stay — they are the only urban-core monitors in it.
        ex_cells = NULL
        if (freeze_active) {
          s_m = if (!is.null(zone_pairs) && nrow(zone_pairs) > 0L &&
                    "start_date" %in% names(zone_pairs)) {
            tibble::as_tibble(zone_pairs) |>
              dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                               from_month = .d1_month_floor(as.Date(.data$start_date))) |>
              dplyr::filter(!is.na(.data$from_month))
          } else {
            tibble::tibble(fullaqsid = character(0), from_month = as.Date(character(0)))
          }
          # F9 (corrected 2026-07-31 — the previous comment claimed this was
          # "month(S_m) by definition", which is FALSE for Milan). In-cordon
          # monitors with no start_date of their own fall back to the metro's
          # registered T_f + 1 month. That EQUALS month(S_m) only when T_f is the
          # month before the studied cordon's own start. It is NOT equal under
          # Tim's 2026-07-31 Milan override: Milan's PRIMARY arm registers
          # T_f = 2007-12 (pre-Ecopass, tf_table.csv implementation_date
          # 2008-01-02) to buy the "vs no pricing" estimand, while the studied
          # cordon in zone_pairs is Area C (2012-01-16). So for a Milan in-cordon
          # monitor on this fallback the exclusion starts 2008-01, four years
          # BEFORE month(S_m) = 2012-01, and the Ecopass era is dropped from the
          # shared pool too. That is CONSERVATIVE and consistent with the arm's
          # estimand (a satellite tile over Ecopass-era Milan is already
          # partly treated for "vs no pricing"), not an accident — but it is a
          # deliberate over-exclusion, not an identity. Monitors that DO carry a
          # start_date use it and are unaffected.
          # PER UNIT (2026-08-03): the T_f+1 fallback is each unit's OWN metro's
          # T_f, from `tf_lookup`. A unit whose metro registers no T_f gets no
          # fallback exclusion (unfrozen for that unit) instead of inheriting
          # some other city's month.
          need = setdiff(intersect(in_cordon, as.character(zb$z$fullaqsid)),
                         s_m$fullaqsid)
          need_tbl = tibble::tibble(fullaqsid = as.character(need)) |>
            dplyr::inner_join(tf_lookup, by = "fullaqsid") |>
            dplyr::transmute(
              fullaqsid  = .data$fullaqsid,
              # first-of-month + 32 days always lands in the NEXT month
              from_month = .d1_month_floor(.d1_month_floor(.data$freeze_month) + 32))
          ex_cells = dplyr::bind_rows(
            s_m |> dplyr::filter(.data$fullaqsid %in% as.character(zb$z$fullaqsid)),
            need_tbl
          )
          if (nrow(ex_cells) == 0L) ex_cells = NULL
        }
        dc = compute_shared_drift_curve(zb$z, tgt, exclude_cells = ex_cells)
        .dlap("compute_shared_drift_curve")
        if (!is.null(dc)) {
          drift_curve = dc$curve
          drift_meta = c(list(source = "estimated_from_fitted_monitors"), dc$meta)
        }
      }
    }
    if (is.null(drift_curve)) {
      message("[fect.train.d1] no shared drift curve c(M): running the ",
              "lambda-window level UNCORRECTED (supply drift_curve= or ",
              "gap_pred_fitted= to enable it)")
    }
  } else {
    drift_meta = list(source = "caller_supplied", n_months = nrow(drift_curve))
  }

  # ---- Stage F rows ---------------------------------------------------------
  # `spec$covariates` is a one-sided FORMULA — resolve it to names with
  # fect_cov_names(). This used to call as.character(), which yields
  # c("~", "<the whole RHS as one string>"); every intersect() below then came
  # back empty, so `keep_cols` carried no covariate columns, stage_f's beta set
  # was empty, and Stage F ran with cov_adj IDENTICALLY ZERO. Silent, and it
  # moved the published anchored numbers. `x_vars` flows on to the freeze block
  # and to stage_f_counterfactual(), so fixing it here fixes all three.
  x_vars = fect_cov_names(spec$covariates)
  x_keep = intersect(x_vars, names(panel))
  if (!length(x_keep)) {
    message("[fect.train.d1] WARNING: Stage F running with cov_adj=0 — no ",
            "covariate columns resolved (spec covariates: [",
            paste(x_vars, collapse = ", "), "]; panel columns: [",
            paste(utils::head(names(panel), 12L), collapse = ", "), "])")
  }
  keep_cols = unique(c("metro_id", "fullaqsid", "date", ".y_model", x_keep))
  rows = panel |>
    dplyr::filter(.data$fullaqsid %in% sp$theta_star$fullaqsid,
                  .data$treated %in% TRUE,
                  is.finite(.data$.y_model)) |>
    dplyr::select(dplyr::all_of(keep_cols))
  if (nrow(rows) == 0L) return(bail("projected units have no treated panel rows"))

  # ---- level: static (lambda = Inf) or time-local (DESIGN §13) --------------
  # Only CENTERED lambda = Inf is the static one-level-per-unit case. Trailing
  # Inf is an EXPANDING window (months <= the predicted month), so it takes the
  # time-local path like any finite lambda.
  lam = suppressWarnings(as.numeric(lambda_months))[1]
  static_level = identical(lam, Inf) && identical(align, "centered")
  spl = NULL
  theta_for_f = sp$theta_star
  if (!static_level) {
    spl = stage_p_theta_star_lambda(
      sp$z, lambda_months = lam, align = align, drift = drift_curve,
      target_months = rows |>
        dplyr::transmute(fullaqsid = as.character(.data$fullaqsid),
                         year_month = .d1_month_floor(.data$date)) |>
        dplyr::distinct(),
      # Scalar for a single-metro panel; tibble(fullaqsid, freeze_month) when
      # the pool spans metros — Stage P has always supported both, and the
      # per-unit form is the only correct one for the training panel.
      freeze_month = if (!freeze_active) NULL else
        if (freeze_per_unit) tf_lookup else tf_month,
      freeze_window_months = freeze_window_months
    )
    if (is.null(spl)) return(bail("the lambda-window level produced nothing"))
    # Unit-level fallback rows (year_month NA) so a treated month the satellite
    # window cannot cover still gets the static level rather than being dropped.
    theta_for_f = dplyr::bind_rows(
      spl$theta_star |>
        dplyr::select(dplyr::any_of(c("fullaqsid", "year_month", "theta_star",
                                      "theta_star_se", "gap_pred_se"))),
      sp$theta_star |>
        dplyr::transmute(fullaqsid = .data$fullaqsid,
                         year_month = as.Date(NA),
                         theta_star = .data$theta_star,
                         theta_star_se = .data$theta_star_se,
                         gap_pred_se = .data$gap_pred_se)
    )
  } else if (!is.null(drift_curve)) {
    # lambda = Inf WITH a drift curve is still a different estimator from D1:
    # apply c(M) to z and re-take the single level.
    spl = stage_p_theta_star_lambda(sp$z, lambda_months = Inf, align = "centered",
                                    drift = drift_curve)
    if (!is.null(spl)) {
      theta_for_f = spl$theta_star |>
        dplyr::distinct(.data$fullaqsid, .keep_all = TRUE) |>
        dplyr::select(dplyr::any_of(c("fullaqsid", "theta_star",
                                      "theta_star_se", "gap_pred_se")))
    }
  }

  # ---- frozen-window deviation references (DESIGN section 33c.4) ------------
  # Stage F re-adds covariates DEVIATION-ONLY, so the reference mean sets the
  # level the deviations are measured from. With the anchor frozen at T_f, an
  # observed-PERIOD (post-treatment) reference is both treatment-touched and
  # inconsistent with that level: W_bar and bg3_bar would be averaged over days
  # the cordon was charging while theta* describes the untreated regime. One
  # reference window, one level, no mismatch — X_bar_i over the SAME
  # [T_f - w, T_f] months that set theta*(T_f). Units with no panel coverage in
  # that window (London: the panel starts 2003-05, T_f = 2003-01) keep the
  # observed-period mean for those covariates and are COUNTED, never silently
  # switched.
  xbar_tbl = NULL
  ref_meta = list(reference = "observed_period")
  # x_vars is already resolved to NAMES by fect_cov_names() above — do not
  # re-wrap it in as.character() (see the note at the x_vars assignment).
  if (freeze_active && length(x_vars)) {
    keep_x = setdiff(x_keep, "sat_monthly_mean")
    if (length(keep_x)) {
      w_ref = if (is.null(freeze_window_months)) lam else
        suppressWarnings(as.numeric(freeze_window_months))[1]
      # PER UNIT (2026-08-03): one [T_f - w, T_f] window per unit, keyed on that
      # unit's OWN T_f. Computed once per DISTINCT T_f with the same exact
      # calendar-month arithmetic as before, so a single-metro panel gets
      # byte-identical bounds.
      unit_ids = unique(as.character(rows$fullaqsid))
      tf_x = tf_lookup |> dplyr::filter(.data$fullaqsid %in% unit_ids)
      tf_vals = sort(unique(tf_x$freeze_month))
      win = tibble::tibble(
        freeze_month = tf_vals,
        .lo = if (is.finite(w_ref))
          as.Date(vapply(tf_vals, function(d) as.numeric(.d1_month_floor(
            seq(d, by = paste0("-", w_ref, " months"), length.out = 2)[2])),
            numeric(1)), origin = "1970-01-01")
        else rep(as.Date("1900-01-01"), length(tf_vals)),
        .hi = as.Date(vapply(tf_vals, function(d) as.numeric(
          seq(d, by = "month", length.out = 2)[2] - 1), numeric(1)),
          origin = "1970-01-01"))
      lo_m = if (nrow(win)) min(win$.lo) else as.Date(NA)
      hi_d = if (nrow(win)) max(win$.hi) else as.Date(NA)
      pw = panel |>
        dplyr::mutate(fullaqsid = as.character(.data$fullaqsid)) |>
        dplyr::filter(.data$fullaqsid %in% unit_ids) |>
        dplyr::inner_join(tf_x, by = "fullaqsid") |>
        dplyr::inner_join(win, by = "freeze_month") |>
        dplyr::filter(as.Date(.data$date) >= .data$.lo,
                      as.Date(.data$date) <= .data$.hi) |>
        dplyr::select(-".lo", -".hi", -"freeze_month")
      fb = rows |>
        dplyr::mutate(fullaqsid = as.character(.data$fullaqsid)) |>
        dplyr::group_by(.data$fullaqsid) |>
        dplyr::summarise(dplyr::across(dplyr::all_of(keep_x),
                                       ~ mean(.x, na.rm = TRUE)), .groups = "drop")
      if (nrow(pw) > 0L) {
        fz = pw |>
          dplyr::group_by(.data$fullaqsid) |>
          dplyr::summarise(dplyr::across(dplyr::all_of(keep_x),
                                         ~ mean(.x, na.rm = TRUE)),
                           .n_ref_days = dplyr::n(), .groups = "drop")
        xbar_tbl = fb |>
          dplyr::left_join(fz, by = "fullaqsid", suffix = c(".obs", ".frz"))
        n_fallback = 0L
        for (k in keep_x) {
          frz = xbar_tbl[[paste0(k, ".frz")]]
          obs = xbar_tbl[[paste0(k, ".obs")]]
          n_fallback = n_fallback + sum(!is.finite(frz))
          xbar_tbl[[k]] = ifelse(is.finite(frz), frz, obs)
        }
        xbar_tbl = xbar_tbl |>
          dplyr::select(dplyr::all_of(c("fullaqsid", keep_x)))
        ref_meta = list(reference = "frozen_window", window_lo = lo_m,
                        window_hi = hi_d, covariates = keep_x,
                        n_distinct_tf = nrow(win),
                        per_unit_windows = freeze_per_unit,
                        n_unit_covariate_fallbacks = n_fallback,
                        n_units_with_frozen_days = nrow(fz))
        message("[fect.train.d1] Stage F deviation references taken over ",
                nrow(win), " per-unit FROZEN window(s) spanning ",
                format(lo_m), "..", format(hi_d), " for ",
                nrow(fz), " unit(s); ", n_fallback,
                " (unit, covariate) reference(s) fell back to the observed period")
      } else {
        ref_meta = list(reference = "observed_period_no_frozen_coverage",
                        window_lo = lo_m, window_hi = hi_d)
        message("[fect.train.d1] no panel days inside the frozen window ",
                format(lo_m), "..", format(hi_d),
                " — Stage F keeps the observed-period references")
      }
    }
  }

  .dlap("stage_p_total")
  sf = stage_f_counterfactual(rows, theta_for_f, frozen, x_vars = x_vars,
                              xbar_tbl = xbar_tbl)
  if (is.null(sf)) return(bail("Stage F produced no rows"))

  list(
    per_unit = sf$per_unit,
    per_metro_day = sf$per_metro_day,
    diagnostics = list(
      tau_backcast = if (is.null(tau_bc)) NULL else tau_bc$calibration,
      stage_p = sp$meta,
      stage_p_lambda = if (is.null(spl)) NULL else spl$meta,
      stage_f = sf$meta,
      lambda_months = lam,
      window_align = align,
      # M9-FREEZE emission (33c.4) — the serving layer labels the ribbon off
      # these; `imp_method` for a joint freeze+shape run is
      # "M9_frozen_sat_anchor_plus_shape_gap".
      m9_freeze = m9_freeze,
      theta_star_freeze_month = tf_month,
      # Per-unit T_f (multi-metro panel): the scalar above is NA on purpose —
      # there is no single T_f — and `tf_by_metro` carries the real map.
      theta_star_freeze_per_unit = freeze_per_unit,
      tf_by_metro = tf_by_metro,
      n_units_frozen = if (is.null(tf_unit)) 0L else nrow(tf_unit),
      n_distinct_tf = if (is.null(tf_unit)) 0L else
        dplyr::n_distinct(tf_unit$freeze_month),
      tf_basis = if (freeze_per_unit) "per_unit" else tf$tf_basis,
      tf_arm = freeze_arm,
      freeze_active = freeze_active,
      freeze_window_months = freeze_window_months %||% lam,
      stage_f_reference = ref_meta,
      drift = drift_meta,
      theta_star = sp$theta_star,
      theta_star_lambda = if (is.null(spl)) NULL else spl$theta_star,
      n_dropped_units = length(dropped_units),
      n_out_of_cordon = length(out_of_cordon)
    )
  )
}

# Pins / Connect user_metadata: each value must be length-1 (JSON scalars).
fect_scalarize_pin_metadata = function(md) {
  out = list()
  for (nm in names(md)) {
    v = md[[nm]]
    if (is.null(v)) {
      out[[nm]] = NA_character_
      next
    }
    if (length(v) == 1L && !is.list(v)) {
      out[[nm]] = v
      next
    }
    if (length(v) == 1L && is.list(v)) {
      out[[nm]] = v[[1]]
      next
    }
    out[[nm]] = paste(as.character(unlist(v)), collapse = ",")
  }
  out
}

# =============================================================================
# Metro-models pooling: e_itz / e_itm / e_it_adj + harmonized super-pin
# (see job/model/HANDOFF_METRO_MODELS.md). These read the per-monitor-day grain
# (`type == "per_day_metro_unit"`) that train_fect_bundle now persists for both
# zone and metro scopes, pool them, and write a model-object-free super-pin.
# =============================================================================

#' Inverse-variance aggregate of per-cell effects to an ATT over `group_cols`.
#'
#' ATT = sum(w*effect)/sum(w), w = 1/se^2; SE = sqrt(1/sum(w)). Cells with a
#' missing/non-positive SE get an equal (unit) weight so they still contribute a
#' point estimate. Matches the appendix's stated inverse-variance combination —
#' no bootstrap. `type` labels the resulting rows.
fect_iv_aggregate = function(df, group_cols = character(0), type = "overall") {
  if (is.null(df) || nrow(df) == 0L) return(tibble::tibble())
  g = if (length(group_cols)) dplyr::group_by(df, dplyr::across(dplyr::all_of(group_cols))) else df
  out = g |>
    dplyr::summarise(
      .w = sum(dplyr::if_else(is.finite(.data$se) & .data$se > 0,
                              1 / (.data$se^2), 1), na.rm = TRUE),
      att = sum(dplyr::if_else(is.finite(.data$se) & .data$se > 0,
                               .data$effect / (.data$se^2), .data$effect),
                na.rm = TRUE),
      n_effects = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      att = .data$att / .data$.w,
      se_att = sqrt(1 / .data$.w),
      type = type
    ) |>
    dplyr::select(-".w")
  out
}

#' Build the pooled per-monitor-day effect table `e_it_adj`.
#'
#' `e_itz` = zone bundle's `per_day_metro_unit` rows (in-zone treated
#' monitor-days); `e_itm` = metro bundle's (all in-metro treated monitor-days).
#' `e_it_adj` = all `e_itz` rows + `e_itm` rows for monitors NOT present in
#' `e_itz` — i.e. prefer the zone estimate where we have it, else the metro
#' estimate. Each row is labeled `in_zone` (true geographic membership from
#' `fetch_zone_treated_pairs`, independent of which monitors fect fit) and
#' `source_model` (`"zone"`|`"metro"`), and joined to monitor lat/lon so an ATT
#' for any geographic subset over any window is a downstream filter + aggregate.
#'
#' @return Tibble (metro_id, fullaqsid, day, effect, se, n_effects,
#'   source_model, in_zone, latitude, longitude) or NULL if neither model
#'   produced unit-grain rows.
build_e_it_adj = function(db, zone_bundle, metro_bundle, spec) {
  pdmu = function(b) {
    if (is.null(b) || is.null(b$att) || nrow(b$att) == 0L) return(NULL)
    a = b$att |> dplyr::filter(.data$type == "per_day_metro_unit")
    if (nrow(a) == 0L) return(NULL)
    a
  }
  e_itz = pdmu(zone_bundle)
  e_itm = pdmu(metro_bundle)
  if (is.null(e_itz) && is.null(e_itm)) {
    message("[fect.train.adj] no per_day_metro_unit rows in either bundle; skipping e_it_adj")
    return(NULL)
  }

  norm = function(a, source_model) {
    a |>
      dplyr::transmute(
        # metro_id in $att is bit64::integer64; plain as.integer() reinterprets
        # the bit pattern (the ~e-324 trap), so round-trip via character.
        metro_id     = as.integer(as.character(.data$metro_id)),
        fullaqsid    = as.character(.data$fullaqsid),
        day          = as.Date(.data$day),
        effect       = .data$att,
        se           = .data$se_att,
        n_effects    = .data$n_effects %||% 1L,
        source_model = source_model
      )
  }
  zpart   = if (!is.null(e_itz)) norm(e_itz, "zone") else NULL
  covered = if (!is.null(zpart)) unique(zpart$fullaqsid) else character(0)
  mpart   = if (!is.null(e_itm)) {
    norm(e_itm, "metro") |> dplyr::filter(!(.data$fullaqsid %in% covered))
  } else NULL

  adj = dplyr::bind_rows(zpart, mpart)
  if (nrow(adj) == 0L) return(NULL)

  # True in-zone membership (not "did fect fit it") for the in_zone label.
  zone_pairs = fetch_zone_treated_pairs(db, metro_ids = spec$metro_ids,
                                        system_types = spec$system_types)
  zone_units = unique(as.character(zone_pairs$fullaqsid))
  adj = adj |> dplyr::mutate(in_zone = .data$fullaqsid %in% zone_units)

  # Geometry for arbitrary geographic subsets (handoff §3 "one-stop shop").
  ids = unique(adj$fullaqsid); ids = ids[!is.na(ids)]
  if (length(ids) > 0L && !is.null(db)) {
    # Postgres text-array literal: bare comma-joined values (NOT shQuote'd —
    # single quotes would become literal characters and match nothing). Safe
    # because fullaqsids are alphanumeric/underscore. Mirrors the array
    # construction in fetch_zone_treated_pairs().
    arr = paste0("{", paste(ids, collapse = ","), "}")
    geo = tryCatch(
      DBI::dbGetQuery(db,
        "SELECT fullaqsid, latitude, longitude FROM public.monitors
         WHERE fullaqsid = ANY($1::text[])", params = list(arr)),
      error = function(e) { message("[fect.train.adj] geom join failed: ",
                                    conditionMessage(e)); NULL })
    if (!is.null(geo) && nrow(geo) > 0L) {
      geo = geo |>
        dplyr::group_by(.data$fullaqsid) |>
        dplyr::summarise(latitude = dplyr::first(.data$latitude),
                         longitude = dplyr::first(.data$longitude),
                         .groups = "drop")
      adj = adj |> dplyr::left_join(geo, by = "fullaqsid")
    }
  }

  message(sprintf(
    "[fect.train.adj] e_it_adj rows=%d (zone=%d metro=%d) monitors=%d in_zone=%d",
    nrow(adj), sum(adj$source_model == "zone"), sum(adj$source_model == "metro"),
    dplyr::n_distinct(adj$fullaqsid), dplyr::n_distinct(adj$fullaqsid[adj$in_zone])))
  adj |> dplyr::arrange(.data$metro_id, .data$fullaqsid, .data$day)
}

#' Assemble the harmonized super-pin object for one (sample, outcome) pair.
#'
#' Contains NO model objects: both specs' metadata + panel_stats + diagnostics,
#' the pooled `e_it_adj` per-monitor-day table, and harmonized aggregated ATTs
#' (overall / per_metro / per_day_metro) for zone, metro, AND adjusted. This is
#' the QoI store the dashboard + paper read going forward. The adjusted ATTs are
#' inverse-variance aggregates of `e_it_adj` (so an arbitrary geographic subset
#' is the same op over a row filter).
build_super_pin_bundle = function(zone_bundle, metro_bundle, e_it_adj,
                                  sample_id, outcome_var) {
  pin_name = paste0("fect_pair_", sample_id, "_", outcome_var)
  # Harmonized aggregated ATTs from each model's already-computed $att, tagged
  # with `scope`, plus the adjusted aggregates from e_it_adj.
  tag_scope = function(att, scope) {
    if (is.null(att) || nrow(att) == 0L) return(tibble::tibble())
    att |>
      dplyr::filter(.data$type %in% c("overall", "per_metro", "per_day_metro")) |>
      dplyr::mutate(scope = scope)
  }
  adj_atts = if (!is.null(e_it_adj) && nrow(e_it_adj) > 0L) {
    dplyr::bind_rows(
      fect_iv_aggregate(e_it_adj, character(0), "overall"),
      fect_iv_aggregate(e_it_adj, "metro_id", "per_metro"),
      fect_iv_aggregate(e_it_adj, c("metro_id", "day"), "per_day_metro")
    ) |> dplyr::mutate(scope = "adjusted")
  } else {
    tibble::tibble()
  }
  att_harmonized = dplyr::bind_rows(
    tag_scope(zone_bundle$att,  "zone"),
    tag_scope(metro_bundle$att, "metro"),
    adj_atts
  )

  list(
    pin_name     = pin_name,
    model_family = "fect_pair",
    status       = "fitted",
    trained_at   = Sys.time(),
    sample_id    = sample_id,
    outcome_var  = outcome_var,
    # No `model` objects — this is the lean QoI store.
    e_it_adj     = e_it_adj,
    att          = att_harmonized,
    zone = list(
      spec        = zone_bundle$spec,
      panel_stats = zone_bundle$panel_stats,
      diagnostics = zone_bundle$diagnostics,
      ui_metadata = zone_bundle$ui_metadata
    ),
    metro = list(
      spec        = metro_bundle$spec,
      panel_stats = metro_bundle$panel_stats,
      diagnostics = metro_bundle$diagnostics,
      ui_metadata = metro_bundle$ui_metadata
    ),
    ui_metadata = list(
      model_family = "fect_pair",
      model_type   = "synth_pair",
      outcome_var  = outcome_var,
      sample_id    = sample_id,
      group_label  = paste0("FECT pair ", sample_id, " (", outcome_var, ")"),
      n_adj_rows   = if (is.null(e_it_adj)) 0L else nrow(e_it_adj)
    )
  )
}

pick_board = function(local_path = "app/v1/data/pins") {
  if (nzchar(Sys.getenv("CONNECT_SERVER")) && nzchar(Sys.getenv("CONNECT_API_KEY"))) {
    message("[fect.train.board] using board_connect (server=", Sys.getenv("CONNECT_SERVER"), ")")
    pins::board_connect()
  } else {
    if (!dir.exists(local_path)) {
      dir.create(local_path, recursive = TRUE, showWarnings = FALSE)
    }
    message("[fect.train.board] using board_folder path=",
            normalizePath(local_path, winslash = "/", mustWork = FALSE))
    pins::board_folder(local_path, versioned = TRUE)
  }
}

# Legacy short pin name for the aq_daily_mean spec — kept so existing app
# loaders (`fect_us_priority`, `fect_all_priority`) keep resolving.
fect_legacy_alias_pin = function(spec) {
  if (!identical(spec$outcome_var, "aq_daily_mean")) return(NULL)
  paste0("fect_", spec$sample_id)
}

write_bundle = function(board, bundle, spec) {
  if (is.null(bundle)) {
    message("[fect.train.write] spec=", spec$id, " SKIP (bundle is NULL)")
    return(invisible(NULL))
  }
  # ADR-0002: `status` no longer depends on presence of bundle$model — the
  # model object is dropped at write time by default, and status is set
  # authoritatively at fit time.
  status = bundle$status %||% "pending_observation"
  is_placeholder = !identical(status, "fitted")

  message("[fect.train.write] spec=", spec$id,
          " pin=", spec$pin_name,
          " status=", status)

  # Metadata (gof/tests) is built at fit time (see fit path ~line 2370) and
  # stored on the bundle. Here we only fall back to recomputing it from
  # bundle$model if it's absent AND the model was retained — the ADR-0002 path
  # drops the model, so the fallback simply won't fire and gof_tbl/tests_tbl
  # come from bundle$gof_tbl / bundle$tests_tbl directly.
  gof_tbl = NULL
  tests_tbl = NULL
  if (!is_placeholder && !is.null(bundle$model)) {
    gof_tbl = bundle$gof_tbl %||% tryCatch(
      get_gof_fect(bundle$model, rmse_on = "control"),
      error = function(e) NULL
    )
    tests_tbl = bundle$tests_tbl %||% tryCatch(
      summarize_fect_tests(bundle$model),
      error = function(e) NULL
    )
  } else {
    gof_tbl   = bundle$gof_tbl
    tests_tbl = bundle$tests_tbl
  }

  # ADR-0002: DROP THE MODEL OBJECT before pinning. Each fit is ~1–3 GB (the
  # raw fect list), and Connect keeps every pin version until pruned — this
  # was refilling disk in a few days. The serving path does not read
  # bundle$model (see app/v2/api/R/data_fetch.R:1230); metadata derivatives
  # (gof_tbl, tests_tbl, ui_metadata, panel_stats) are already stamped above.
  # Keep the model in the bundle by setting CPPORTAL_FECT_KEEP_MODEL=1 (for
  # rare local debugging that still wants the raw fit).
  .keep_model <- tolower(Sys.getenv("CPPORTAL_FECT_KEEP_MODEL", "0")) %in%
                 c("1","true","t","yes","y")
  if (!.keep_model && !is.null(bundle$model)) {
    message("[fect.train.write] spec=", spec$id, " ADR-0002 dropping bundle$model before pin")
    bundle$model <- NULL
  }
  # VIF (covariate-only collinearity) — survives even if fit failed and the
  # bundle is a placeholder, but pull defensively in case the field is missing.
  vif_tbl_here   = bundle$vif_tbl
  vif_max_excl_z = bundle$diagnostics$covariate_vif_max_excl_z %||% NA_real_
  vif_max_all    = bundle$diagnostics$covariate_vif_max_with_all %||% NA_real_
  vif_per_cov_csv = if (!is.null(vif_tbl_here) && nrow(vif_tbl_here) > 0L) {
    paste(
      paste0(vif_tbl_here$covariate, "=",
             ifelse(is.na(vif_tbl_here$vif), "NA",
                    as.character(round(vif_tbl_here$vif, 3)))),
      collapse = ","
    )
  } else {
    NA_character_
  }

  imp_diag = bundle$diagnostics$imputation

  meta_extra = c(
    fect_pin_metadata_gof(gof_tbl),
    fect_pin_metadata_tests(tests_tbl),
    list(
      covariate_vif_max         = as.numeric(vif_max_excl_z),
      covariate_vif_max_all     = as.numeric(vif_max_all),
      covariate_vif_n           = if (is.null(vif_tbl_here)) NA_integer_
                                  else as.integer(nrow(vif_tbl_here)),
      covariate_vif_per_cov_csv = as.character(vif_per_cov_csv),
      # Z-covariate (distance-to-road siting control) inclusion provenance.
      # `dist_included` is TRUE for every model UNLESS including the Z covariate
      # made the cfe solver singular, in which case it was dropped for that spec
      # only (see get_fect() Z singular-matrix fallback).
      dist_included             = isTRUE(bundle$diagnostics$dist_included),
      z_covariates_used_csv     = {
        zu = bundle$diagnostics$z_covariates_used %||% character(0)
        if (length(zu)) paste(zu, collapse = ",") else NA_character_
      },
      z_dropped_singular_csv    = {
        zd = bundle$diagnostics$z_dropped_singular %||% character(0)
        if (length(zd)) paste(zd, collapse = ",") else NA_character_
      },
      # Plan B (M2) imputation provenance — see impute_dropped_zone_atts()
      # and appendix.md §A.6. NA when no units needed imputation.
      imputation_method          = as.character(imp_diag$method %||% NA_character_),
      imputation_alpha_model_r2  = as.numeric(imp_diag$alpha_model_r2 %||% NA_real_),
      imputation_sat_anchor      = isTRUE(imp_diag$sat_anchor),
      imputation_n_dropped_units = as.integer(imp_diag$n_dropped_units %||% NA_integer_),
      imputation_n_imputed_units = as.integer(imp_diag$n_imputed_units %||% NA_integer_),
      # Leave-out calibration: measured error of the imputation procedure on
      # the fitted-zone testbed (appendix A.7.3 "empirical bias bands").
      imputation_cal_n           = as.integer(imp_diag$cal_n %||% NA_integer_),
      imputation_cal_bias        = as.numeric(imp_diag$cal_bias %||% NA_real_),
      imputation_cal_mae         = as.numeric(imp_diag$cal_mae %||% NA_real_),
      imputation_cal_q05         = as.numeric(imp_diag$cal_q05 %||% NA_real_),
      imputation_cal_q95         = as.numeric(imp_diag$cal_q95 %||% NA_real_)
    )
  )

  ps = bundle$panel_stats %||% list()
  # Posit Connect pin user_metadata must be JSON-friendly scalars (no vectors/lists).
  metadata = utils::modifyList(
    list(
      spec_id = spec$id,
      sample_id = as.character(spec$sample_id %||% NA_character_),
      outcome_var = as.character(spec$outcome_var %||% NA_character_),
      outcome_kind = as.character(spec$outcome_kind %||% NA_character_),
      treatment_scope = as.character(spec$treatment_scope %||% "zone"),
      fect_method = as.character(spec$fect_method %||% NA_character_),
      system_types_csv = paste(spec$system_types %||% character(0), collapse = ","),
      model_family = "fect",
      model_type = "synth",
      status = status,
      status_reason = as.character(bundle$status_reason %||% NA_character_),
      group_label = as.character(spec$label %||% spec$id),
      model_label = as.character(spec$name %||% spec$id),
      control_metro_ids_csv = paste(
        sort(unique(as.integer(spec$metro_ids))),
        collapse = ","
      ),
      n_metro_ids_declared = length(spec$metro_ids),
      pollutant = as.character(spec$pollutant),
      date_from = format(spec$date_from),
      date_to = format(spec$date_to),
      min_date = if (is.null(spec$min_date) || is.na(spec$min_date)) {
        NA_character_
      } else {
        format(as.Date(spec$min_date))
      },
      trained_at = format(bundle$trained_at, "%Y-%m-%d %H:%M:%S"),
      panel_n_rows = as.integer(ps$n_rows %||% NA_integer_),
      panel_n_monitors = as.integer(ps$n_monitors %||% NA_integer_),
      panel_n_metros = as.integer(ps$n_metros %||% NA_integer_),
      panel_n_dates = as.integer(ps$n_dates %||% NA_integer_),
      panel_n_treated = as.integer(ps$n_treated %||% NA_integer_),
      panel_n_treated_obs = as.integer(ps$n_treated_obs %||% NA_integer_),
      panel_n_treated_units = as.integer(ps$n_treated_units %||% NA_integer_),
      panel_n_control = as.integer(ps$n_control %||% NA_integer_),
      panel_n_zone_pairs = as.integer(ps$n_zone_pairs %||% NA_integer_),
      panel_date_min = if (is.null(ps$date_min) || is.na(ps$date_min)) {
        NA_character_
      } else {
        format(as.Date(ps$date_min))
      },
      panel_date_max = if (is.null(ps$date_max) || is.na(ps$date_max)) {
        NA_character_
      } else {
        format(as.Date(ps$date_max))
      }
    ),
    meta_extra
  )
  metadata = fect_scalarize_pin_metadata(metadata)

  pin_tags = c("cpportal", "fect", spec$id,
               spec$sample_id %||% character(0),
               spec$outcome_kind %||% character(0),
               status)

  title_suffix = if (is_placeholder) " [PLACEHOLDER]" else ""
  pin_description = if (is_placeholder) {
    paste0(spec$description,
           " STATUS=PLACEHOLDER (", bundle$status_reason %||% status,
           "); pipeline ready -- next training run picks up real fit once",
           " upstream observations land.")
  } else {
    spec$description
  }

  pins::pin_write(
    board,
    x = bundle,
    name = spec$pin_name,
    type = "rds",
    title = paste0("CP Portal FECT - ", spec$name, title_suffix),
    description = pin_description,
    metadata = metadata,
    tags = pin_tags
  )

  # Legacy alias pin (`fect_us_priority`, `fect_all_priority`) only mirrors the
  # canonical aq_daily_mean bundle, and only when that bundle is fitted -- we
  # don't want a placeholder to clobber a previously-good legacy pin.
  alias = fect_legacy_alias_pin(spec)
  if (!is.null(alias)) {
    if (is_placeholder) {
      message("[fect.train.write] spec=", spec$id,
              " legacy alias pin=", alias,
              " SKIPPED (canonical bundle is a placeholder)")
    } else {
      message("[fect.train.write] spec=", spec$id, " legacy alias pin=", alias)
      alias_metadata = utils::modifyList(metadata,
                                         list(legacy_alias_for = spec$pin_name))
      alias_metadata = fect_scalarize_pin_metadata(alias_metadata)
      pins::pin_write(
        board,
        x = bundle,
        name = alias,
        type = "rds",
        title = paste0("CP Portal FECT - ", spec$name, " (legacy alias)"),
        description = paste0(spec$description,
                             " Legacy short pin name; alias of ", spec$pin_name, "."),
        metadata = alias_metadata,
        tags = c(pin_tags, "legacy_alias")
      )
    }
  }

  invisible(NULL)
}

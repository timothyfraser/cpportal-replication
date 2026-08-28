# functions_fect.R
#
# fect::fect (method = "cfe") helpers. Source after job/model/did/functions_did.R
# so get_att_did / summarize_att_core_did are available for get_qis_fect().
#
# References: job/model/fect/functions_fect_references.bib (LWX2024).

library(dplyr)
library(tidyr)
library(gtools)
library(lubridate)
library(tibble)

if (!exists("default_outcome_aq", mode = "function")) {
  default_outcome_aq = function() stats::as.formula("~ sqrt(aq_daily_mean)")
}

#' Default traffic outcome (identity transform — speeds are bounded, not log-skewed).
#' @return One-sided formula `~ traffic_daily_mean`.
#' @export
default_outcome_traffic = function() stats::as.formula("~ traffic_daily_mean")

#' Bootstrap replicates for **smoke** checks (`se = TRUE`); see `README_modeling.md`.
#' @keywords internal
FECT_NBOOTS_SMOKE = 10L

#' Bootstrap replicates for **production** / full runs (`se = TRUE`); see `README_modeling.md`.
#' @keywords internal
FECT_NBOOTS_PRODUCTION = 1000L

#' Default `cores` when `parallel = TRUE` for fect bootstrap; see `README_modeling.md`.
#' @keywords internal
FECT_CORES_DEFAULT = 8L

#' @keywords internal
assert_required_columns_fect = function(data, required, fn_name) {
  missing = setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      paste0(fn_name, "() missing required columns: ", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }
}

#' @keywords internal
extract_response_lhs_fect = function(outcome) {
  if (!inherits(outcome, "formula")) {
    stop("outcome must be a formula, e.g. ~ aq_daily_mean", call. = FALSE)
  }
  if (length(outcome) < 2) {
    stop("outcome must be one-sided: ~ response_expression", call. = FALSE)
  }
  outcome[[2]]
}

#' @keywords internal
infer_inverse_fn_fect = function(lhs_expr, response_inverse = NULL) {
  if (!is.null(response_inverse)) {
    if (!is.function(response_inverse)) {
      stop("response_inverse must be NULL or a function(z)", call. = FALSE)
    }
    return(response_inverse)
  }
  if (is.symbol(lhs_expr)) {
    return(identity)
  }
  if (!is.call(lhs_expr)) {
    stop("Unsupported response expression; pass response_inverse = function(z) ...", call. = FALSE)
  }
  fn = as.character(lhs_expr[[1]])
  if (fn == "sqrt") {
    return(function(z) z^2)
  }
  if (fn == "log") {
    return(exp)
  }
  if (fn == "log10") {
    return(function(z) 10^z)
  }
  if (fn == "log1p") {
    return(expm1)
  }
  stop(
    paste0("Could not infer inverse for ", deparse(lhs_expr)),
    call. = FALSE
  )
}

#' Default **time-varying** covariates (RHS `X`) for `model_panel_daily`
#'
#' **fect terminology:** variables that do not change over time **within** each
#' monitor (e.g. road distance) are rejected as ordinary formula terms under
#' `method = \"cfe\"` with an error like *unit-invariant* — meaning
#' *time-invariant within unit*, not "the same for every unit". Pass those
#' columns via [default_z_covariates_fect()] and the `z_covariates` argument to
#' [get_fect()] so they enter the **`Z`** (time-invariant) matrix.
#'
#' @return One-sided formula (population is yearly stepwise in the panel).
#' @keywords internal
default_covariates_fect = function() {
  stats::as.formula(paste0(
    "~ temp_daily_mean + rhum_daily_mean + precip_daily_mean + ",
    "ws_daily_mean + wd_daily_mean_deg + barpr_daily_mean + srad_daily_mean + ",
    "cloud_daily_mean + population + bg3"
  ))
}

#' Default time-invariant covariates for the **`Z`** matrix (`method = \"cfe\"`)
#'
#' Road distance is fixed per monitor; it varies across units but not over
#' dates for a given `fullaqsid`.
#'
#' @return Character vector of column names passed to `fect::fect(..., Z = )`.
#' @keywords internal
default_z_covariates_fect = function() {
  c("dist_km_motorway_trunk_primary_secondary")
}

#' Default time-varying covariates for traffic-outcome FECT models
#'
#' Smaller covariate set than [default_covariates_fect()]: weather drivers of
#' congestion (`temp`, `rhum`) plus `population` and the time-invariant
#' road-distance proxy. For `method = "mc"`, time-invariant columns are kept
#' here (not in `Z =`) and absorbed by unit FE — the matrix-completion estimator
#' does not partial out time-invariant `Z` separately.
#'
#' @return One-sided formula.
#' @export
default_covariates_traffic = function() {
  stats::as.formula(
    "~ temp_daily_mean + rhum_daily_mean + population + dist_km_motorway_trunk_primary_secondary"
  )
}

#' Prepare panel for [get_fect()]
#'
#' Builds `D` (0/1), integer `time_id` from calendar `date`, optional imputation
#' for `barpr_daily_mean` and `srad_daily_mean` (metro-month medians).
#'
#' @param data Panel tibble.
#' @param treated_col Binary treatment column name.
#' @param date_col Date column name.
#' @param unit_col Monitor id column (default `fullaqsid`).
#' @param impute_barpr_srad If TRUE, impute missing barometric pressure and
#'   shortwave radiation with metro-month medians.
#' @return Data frame with `D`, `time_id`, and date levels in `attr(..., "fect_date_levels")`.
prepare_data_fect = function(
    data,
    treated_col = "treated",
    date_col = "date",
    unit_col = "fullaqsid",
    impute_barpr_srad = TRUE
) {
  assert_required_columns_fect(
    data,
    c(treated_col, date_col, unit_col),
    "prepare_data_fect"
  )
  out = data |>
    mutate(
      D = as.integer(as.logical(.data[[treated_col]])),
      .date_lbl = as.character(.data[[date_col]])
    )

  date_levels = sort(unique(out$.date_lbl))
  out = out |>
    mutate(time_id = match(.data$.date_lbl, date_levels))

  if (impute_barpr_srad) {
    for (col in c("barpr_daily_mean", "srad_daily_mean")) {
      if (col %in% names(out) && anyNA(out[[col]])) {
        med = stats::median(out[[col]], na.rm = TRUE)
        if (is.finite(med)) {
          out[[col]] = ifelse(is.na(out[[col]]), med, out[[col]])
        }
      }
    }
  }

  out = out |> select(-.date_lbl)
  attr(out, "fect_date_levels") = date_levels
  out
}

#' @keywords internal
build_fect_formula = function(covariate_vars) {
  rhs = c("D", covariate_vars)
  stats::reformulate(termlabels = rhs, response = ".y_fect")
}

#' Fit `fect::fect` with `method = "cfe"`
#'
#' **Requires** package `fect` on the path (GitHub: `xuyiqing/fect`).
#'
#' @param data Panel tibble after [add_treatment()] etc.
#' @param outcome One-sided formula for outcome; default [default_outcome_aq()]
#'   (`~ sqrt(aq_daily_mean)`). Pass `~ aq_daily_mean` for identity on native units.
#' @param covariates One-sided formula of **time-varying** RHS controls; default
#'   [default_covariates_fect()]. Time-invariant-within-unit columns (e.g.
#'   road distance) belong in `z_covariates`, not here.
#' @param z_covariates Character vector of column names for **time-invariant**
#'   covariates, passed to `fect::fect(..., Z = )` under `method = \"cfe\"`.
#'   Default [default_z_covariates_fect()]. Use `character(0)` to omit `Z`
#'   (only if you drop those columns from `data` or know they are not needed).
#' @param treated_col Treatment column (logical/integer); mapped to `D`.
#' @param date_col Calendar date column.
#' @param unit_col Unit column (monitor id).
#' @param metro_col Metro grouping for third `index` element with `cfe`.
#' @param use_metro_index If FALSE, use two-way `index` only (fallback).
#' @param response_inverse Optional inverse transform (see DiD helpers).
#' @param na.rm Passed to `fect::fect`. Default `NULL` resolves method-aware:
#'   `TRUE` for `cfe` (drops NA rows the way the CFE estimator expects) and
#'   `FALSE` for `mc` (matrix completion treats NA Y as missing entries to
#'   impute -- stripping them defeats the estimator's purpose). Pass an
#'   explicit logical to override.
#' @param se,parallel,cores,nboots Passed to `fect::fect`. When `se = TRUE`, use
#'   **`vartype = "bootstrap"`** (default), **`nboots = 1000`**, **`cores = 8`**
#'   for production; **`nboots = 10`** ([FECT_NBOOTS_SMOKE]) for smoke tests only—see
#'   `README_modeling.md` and [FECT_NBOOTS_PRODUCTION].
#' @param vartype Passed to `fect::fect` when `se = TRUE` (`"bootstrap"` or `"jackknife"`).
#' @param method One of `"cfe"` (default; complex fixed effects, requires
#'   pre-treatment rows for treated units) or `"mc"` (matrix completion;
#'   borrows cross-sectional structure when pre-treatment is sparse). For
#'   `"mc"` the third metro index dimension and the `Z =` argument are not
#'   used: time-invariant covariates can sit in `covariates` and will be
#'   absorbed by unit FE.
#' @param mc_lambda Optional `lambda` for `method = "mc"`; default `NULL`
#'   triggers cross-validation via `CV = TRUE`.
#' @param ... Additional arguments to `fect::fect`.
#' @return `fect` object with attribute `fect_meta`.
#' @export
get_fect = function(
    data,
    outcome = default_outcome_aq(),
    covariates = default_covariates_fect(),
    z_covariates = default_z_covariates_fect(),
    treated_col = "treated",
    date_col = "date",
    unit_col = "fullaqsid",
    metro_col = "metro_id",
    use_metro_index = TRUE,
    response_inverse = NULL,
    na.rm = NULL,
    se = FALSE,
    parallel = TRUE,
    cores = FECT_CORES_DEFAULT,
    nboots = FECT_NBOOTS_PRODUCTION,
    vartype = "bootstrap",
    method = c("cfe", "mc"),
    mc_lambda = NULL,
    ...
) {
  if (!requireNamespace("fect", quietly = TRUE)) {
    stop("Install package 'fect', e.g. devtools::install_github('xuyiqing/fect')", call. = FALSE)
  }
  method = match.arg(method)
  if (is.null(na.rm)) {
    na.rm = !identical(method, "mc")
  }

  lhs_expr = extract_response_lhs_fect(outcome)
  outcome_vars = all.vars(lhs_expr)
  covariate_vars = if (is.null(covariates)) {
    character(0)
  } else {
    all.vars(covariates)
  }

  # MC does not partial out time-invariant Z separately; fold it into time-varying covariates.
  z_vars = if (length(z_covariates) == 0L) {
    character(0)
  } else {
    unique(as.character(z_covariates))
  }

  if (identical(method, "mc") && length(z_vars) > 0L) {
    covariate_vars = unique(c(covariate_vars, z_vars))
    z_vars = character(0)
  } else {
    covariate_vars = setdiff(covariate_vars, z_vars)
  }

  assert_required_columns_fect(
    data,
    unique(c(
      outcome_vars,
      covariate_vars,
      z_vars,
      treated_col,
      date_col,
      unit_col,
      metro_col
    )),
    "get_fect"
  )

  inverse_fn = infer_inverse_fn_fect(lhs_expr, response_inverse)

  prep = prepare_data_fect(
    data = data,
    treated_col = treated_col,
    date_col = date_col,
    unit_col = unit_col,
    impute_barpr_srad = TRUE
  )

  prep = prep |>
    mutate(.y_fect = as.numeric(eval(lhs_expr, envir = prep, enclos = baseenv())))

  form = build_fect_formula(covariate_vars)
  fit_data = prep |> as.data.frame()

  index2 = c(unit_col, "time_id")
  index3 = c(unit_col, "time_id", metro_col)

  # MC does not support a 3rd metro index — force two-way only.
  use_metro_index_eff = isTRUE(use_metro_index) && identical(method, "cfe")

  # vartype = "analytic": skip bootstrap; compute two-way residual SEs post-hoc
  analytic_se = isTRUE(se) && identical(vartype, "analytic")
  if (analytic_se) se = FALSE

  fect_call = function(index_arg, z_use = z_vars) {
    args = list(
      formula = form,
      data = fit_data,
      index = index_arg,
      method = method,
      force = "two-way",
      na.rm = na.rm,
      se = se,
      parallel = parallel,
      cores = cores,
      nboots = nboots
    )
    if (isTRUE(se)) {
      args$vartype = vartype
    }
    if (identical(method, "cfe") && length(z_use) > 0L) {
      args$Z = z_use
    }
    if (identical(method, "mc")) {
      if (is.null(mc_lambda)) {
        args$CV = TRUE
      } else {
        args$lambda = mc_lambda
        args$CV = FALSE
      }
    }
    do.call(fect::fect, c(args, list(...)))
  }

  # Z-covariate singular-matrix fallback. Time-invariant Z covariates (e.g.
  # `dist_km_...` distance-to-road) enter the cfe estimator through its Z/gamma
  # structure as time-interacted terms (dist_i * gamma_t; see job/model/fect/
  # CLAUDE.md). When the per-period control cross-section is too thin to
  # identify those gamma_t — observed for metro-scope peak/off-peak-window AQ
  # specs, where controls are only untreated-metro monitors — the solver's
  # `inv_sympd()` throws "matrix is singular or not positive definite" and NO
  # model is produced. Default behaviour is to INCLUDE Z (it is an important
  # siting control); only on that specific singular-matrix failure do we retry
  # WITHOUT Z and record which covariates were dropped, so daily/overnight and
  # all zone specs (which identify Z fine) are byte-for-byte unchanged.
  z_dropped_singular = character(0)
  is_singular_err = function(e) {
    grepl("singular|not positive definite|inv_sympd", conditionMessage(e),
          ignore.case = TRUE)
  }
  run_fit = function(index_arg) {
    if (!(identical(method, "cfe") && length(z_vars) > 0L)) {
      return(fect_call(index_arg))
    }
    tryCatch(
      fect_call(index_arg, z_use = z_vars),
      error = function(e) {
        if (is_singular_err(e)) {
          message(
            "get_fect: Z covariate(s) [", paste(z_vars, collapse = ", "),
            "] make the cfe solver singular (", conditionMessage(e),
            "); retrying WITHOUT Z for this spec (identification too thin)."
          )
          z_dropped_singular <<- z_vars
          fect_call(index_arg, z_use = character(0))
        } else {
          stop(e)
        }
      }
    )
  }

  t_fit = Sys.time()
  fit = if (use_metro_index_eff) {
    tryCatch(
      run_fit(index3),
      error = function(e) {
        message(
          "get_fect: ", method, " with metro index failed (",
          conditionMessage(e),
          "); retrying two-way index only."
        )
        run_fit(index2)
      }
    )
  } else {
    run_fit(index2)
  }
  message(sprintf(
    "[fect.timing] get_fect fect::fect(method=%s) fit_s=%.1f n_rows=%d",
    method, as.numeric(difftime(Sys.time(), t_fit, units = "secs")),
    nrow(fit_data)
  ))

  n_idx = tryCatch(length(fit$index), error = function(e) 2L)
  index_used = if (use_metro_index_eff && n_idx >= 3L) index3 else index2

  meta = list(
    outcome_col = outcome_vars[[1]],
    response_lhs = lhs_expr,
    inverse_fn = inverse_fn,
    covariate_vars = covariate_vars,
    z_covariate_vars = z_vars,
    # What actually entered the fit: identical to z_covariate_vars unless the
    # singular-matrix fallback dropped Z for this spec (then character(0)).
    z_covariate_vars_used = if (length(z_dropped_singular)) character(0) else z_vars,
    z_dropped_singular = z_dropped_singular,
    treated_col = treated_col,
    date_col = date_col,
    unit_col = unit_col,
    metro_col = metro_col,
    formula = form,
    index_used = index_used,
    data_fit = prep,
    lhs_expr = lhs_expr,
    fect_method = method
  )

  sigma2 = fit$sigma2
  if (is.null(sigma2) || !is.finite(sigma2)) {
    sigma2 = stats::var(prep$.y_fect, na.rm = TRUE)
  }
  meta$sigma2 = sigma2

  meta$se_method = if (analytic_se) "twoway_resid" else if (isTRUE(se)) vartype else "none"
  attr(fit, "fect_meta") = meta

  if (analytic_se) {
    t_se = Sys.time()
    analytic_se_vec = fect_pred_se(fit, newdata = NULL, method = "twoway_resid")
    meta$analytic_se_tbl = tibble::tibble(
      unit_id  = as.character(meta$data_fit[[unit_col]]),
      time_id  = as.integer(meta$data_fit$time_id),
      se_analytic = analytic_se_vec
    )
    attr(fit, "fect_meta") = meta
    message(sprintf(
      "[fect.timing] get_fect analytic SE (fect_pred_se twoway_resid) se_s=%.1f",
      as.numeric(difftime(Sys.time(), t_se, units = "secs"))
    ))
  }

  fit
}

#' Placebo pre-trend diagnostic (fect §2.3.1)
#'
#' Refits with `placeboTest = TRUE` and the given `placebo.period` (relative
#' pre-treatment periods, e.g. `c(-2, 0)`). Observations in that range are
#' excluded from fitting; the test checks whether estimated ATT in that window
#' differs from zero. See
#' [fect Ch. 2 §2.3.1](https://yiqingxu.org/packages/fect/02-fect.html).
#'
#' **Note:** This is a **second** fit (after your main [get_fect()]). Defaults
#' to `se = TRUE` so uncertainty is available for the diagnostic; use
#' `se = FALSE` only for a quick wiring check.
#'
#' @param placebo.period Length-2 integer vector, e.g. `c(-2, 0)`.
#' @inheritParams get_fect
#' @param ... Additional arguments passed to `fect::fect` (e.g. `min.T0`, `seed`).
#' @return Named list: `fit` (fect object with `fect_meta$placebo.period` set)
#'   and `summary` (one-row tibble from [summarize_fect_placebo()]).
#' @export
get_fect_placebo = function(
    placebo.period,
    data,
    outcome = default_outcome_aq(),
    covariates = default_covariates_fect(),
    z_covariates = default_z_covariates_fect(),
    treated_col = "treated",
    date_col = "date",
    unit_col = "fullaqsid",
    metro_col = "metro_id",
    use_metro_index = TRUE,
    response_inverse = NULL,
    na.rm = NULL,
    se = TRUE,
    parallel = TRUE,
    cores = FECT_CORES_DEFAULT,
    nboots = FECT_NBOOTS_PRODUCTION,
    vartype = "bootstrap",
    method = c("cfe", "mc"),
    mc_lambda = NULL,
    ...
) {
  method = match.arg(method)
  if (is.null(na.rm)) {
    na.rm = !identical(method, "mc")
  }
  if (length(placebo.period) != 2L) {
    stop(
      "placebo.period must be length 2 (e.g. c(-2, 0)); see fect §2.3.1",
      call. = FALSE
    )
  }
  fit = get_fect(
    data = data,
    outcome = outcome,
    covariates = covariates,
    z_covariates = z_covariates,
    treated_col = treated_col,
    date_col = date_col,
    unit_col = unit_col,
    metro_col = metro_col,
    use_metro_index = use_metro_index,
    response_inverse = response_inverse,
    na.rm = na.rm,
    se = se,
    parallel = parallel,
    cores = cores,
    nboots = nboots,
    vartype = vartype,
    method = method,
    mc_lambda = mc_lambda,
    placeboTest = TRUE,
    placebo.period = placebo.period,
    ...
  )
  attr(fit, "fect_placebo") = list(placebo.period = unname(as.integer(placebo.period)))
  meta = attr(fit, "fect_meta")
  if (!is.null(meta)) {
    meta$placebo.period = unname(as.integer(placebo.period))
    attr(fit, "fect_meta") = meta
  }
  list(
    fit = fit,
    summary = summarize_fect_placebo(fit)
  )
}

#' Summarize placebo run (one-row tibble for logs)
#'
#' Combines [summarize_fect_tests()] with placebo period metadata when present.
#'
#' @param fit A `fect` object, ideally from [get_fect_placebo()].
#' @return Single-row tibble.
#' @export
summarize_fect_placebo = function(fit) {
  if (!inherits(fit, "fect")) {
    stop("summarize_fect_placebo() needs a fect object", call. = FALSE)
  }
  meta = attr(fit, "fect_meta")
  pb = attr(fit, "fect_placebo")
  pp_vec = if (!is.null(meta$placebo.period)) {
    meta$placebo.period
  } else if (!is.null(pb$placebo.period)) {
    pb$placebo.period
  } else {
    NULL
  }
  pp_lbl = if (!is.null(pp_vec) && length(pp_vec) == 2L) {
    paste(pp_vec[1], pp_vec[2], sep = " to ")
  } else {
    NA_character_
  }
  st = summarize_fect_tests(fit)
  tibble::tibble(
    placebo_period = pp_lbl,
    pretrend_f_p = st$pretrend_f_p,
    equiv_p = st$equiv_p,
    loo_test_present = st$loo_test_present,
    manual = "fect §2.3.1 — https://yiqingxu.org/packages/fect/02-fect.html"
  )
}

#' Validation helper: placebo summary for a fect fit
#'
#' Calls [summarize_fect_placebo()]. If `fit` was not produced by
#' [get_fect_placebo()], metadata may be incomplete but `test.out` is still
#' summarized when present.
#'
#' @param fit A `fect` object (with or without placebo attributes).
#' @return Same as [summarize_fect_placebo()].
#' @export
validate_fect_placebo = function(fit) {
  summarize_fect_placebo(fit)
}

#' @keywords internal
vif_max_pooled_covariates_fect = function(meta) {
  if (!requireNamespace("car", quietly = TRUE)) {
    return(list(vifmax = NA_real_, vif_scope = "car not installed"))
  }
  vars = unique(c(meta$covariate_vars, meta$z_covariate_vars))
  if (length(vars) < 2L) {
    return(list(vifmax = NA_real_, vif_scope = "fewer than 2 covariates"))
  }
  df = meta$data_fit
  need = c(".y_fect", vars)
  if (!all(need %in% names(df))) {
    return(list(vifmax = NA_real_, vif_scope = "missing columns"))
  }
  df = df[stats::complete.cases(df[need]), , drop = FALSE]
  if (nrow(df) < length(vars) + 2L) {
    return(list(vifmax = NA_real_, vif_scope = "insufficient complete rows"))
  }
  rhs = paste(vars, collapse = " + ")
  f = stats::as.formula(paste0(".y_fect ~ ", rhs))
  m = tryCatch(stats::lm(f, data = df), error = function(e) NULL)
  if (is.null(m)) {
    return(list(vifmax = NA_real_, vif_scope = "lm failed"))
  }
  v = tryCatch(car::vif(m), error = function(e) NULL)
  if (is.null(v)) {
    return(list(vifmax = NA_real_, vif_scope = "vif failed"))
  }
  if (is.matrix(v)) {
    vmax = max(v[, 1], na.rm = TRUE)
  } else {
    vmax = max(as.numeric(v), na.rm = TRUE)
  }
  list(vifmax = vmax, vif_scope = "pooled_lm_covariates_not_twfe_cfe")
}

#' Goodness-of-fit summary for a [get_fect()] fit
#'
#' Aligns with [get_gof_did()] where possible. **R-squared** is a pseudo-R² on
#' **control** rows (`D == 0`) comparing observed `.y_fect` to counterfactual
#' `yhat0`, or `NA` if undefined. **VIF** is max VIF from a **pooled** `lm` of
#' covariates only—not TWFE/CFE structural VIF (see `vif_scope` column).
#'
#' @param fit Object from [get_fect()].
#' @param grid Optional output of [get_grid_fect()]; built if `NULL`.
#' @param rmse_on `"control"` (default) or `"all"` rows for RMSE/MAE/pseudo-r².
#' @return Single-row tibble.
#' @export
get_gof_fect = function(fit, grid = NULL, rmse_on = c("control", "all")) {
  if (!inherits(fit, "fect")) {
    stop("get_gof_fect() requires a fect fit", call. = FALSE)
  }
  meta = attr(fit, "fect_meta")
  if (is.null(meta)) {
    stop("get_gof_fect() requires fect_meta from get_fect()", call. = FALSE)
  }
  rmse_on = match.arg(rmse_on)
  if (is.null(grid)) {
    grid = get_grid_fect(fit, meta$data_fit)
  }

  y = grid$.y_fect
  yhat = grid$yhat0
  d = grid$D
  w = if (rmse_on == "control") {
    d %in% 0L
  } else {
    rep(TRUE, nrow(grid))
  }
  w = w & is.finite(y) & is.finite(yhat)

  rsq = NA_real_
  rmse = NA_real_
  mae = NA_real_
  ymin = NA_real_
  ymax = NA_real_
  rng = NA_real_
  mvr = NA_real_
  rmvr = NA_real_

  if (any(w)) {
    yw = y[w]
    e = yw - yhat[w]
    rmse = sqrt(mean(e^2))
    mae = mean(abs(e))
    ymin = min(yw, na.rm = TRUE)
    ymax = max(yw, na.rm = TRUE)
    rng = ymax - ymin
    if (is.finite(rng) && rng > 0) {
      mvr = mae / rng
      rmvr = rmse / rng
    }
    ym = mean(yw, na.rm = TRUE)
    sst = sum((yw - ym)^2)
    if (is.finite(sst) && sst > 0) {
      rsq = 1 - sum(e^2) / sst
    }
  }

  nobs = sum(is.finite(y))
  tr = sum(d %in% 1L, na.rm = TRUE)
  ct = sum(d %in% 0L, na.rm = TRUE)

  vif = vif_max_pooled_covariates_fect(meta)

  s2 = as.numeric(meta$sigma2)[1]
  sigma = if (is.finite(s2) && s2 >= 0) sqrt(s2) else NA_real_

  rfit = tryCatch(fit$r.squared, error = function(e) NA_real_)
  if (length(rfit) != 1L || !is.finite(rfit)) {
    rfit = NA_real_
  }

  tibble::tibble(
    rsq = rsq,
    r_squared_fect = rfit,
    sigma = sigma,
    statistic = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    nobs = as.integer(nobs),
    vifmax = vif$vifmax,
    vif_scope = vif$vif_scope,
    ymin = ymin,
    ymax = ymax,
    range = rng,
    rmse = rmse,
    mae = mae,
    maevsrange = mvr,
    rmsevsrange = rmvr,
    tr = as.integer(tr),
    ct = as.integer(ct),
    rmse_on = rmse_on
  )
}

#' Flatten pretrend / equivalence p-values from `fect` `test.out` when possible
#'
#' @param fit A `fect` object.
#' @return Single-row tibble (best-effort; `NA` if structure unknown).
#' @export
summarize_fect_tests = function(fit) {
  pretrend_f_p = NA_real_
  equiv_p = NA_real_
  to = tryCatch(fit$test.out, error = function(e) NULL)
  if (!is.null(to) && is.list(to)) {
    if (!is.null(to$F.p)) {
      pretrend_f_p = suppressWarnings(as.numeric(to$F.p)[1])
    }
    if (!is.null(to$equiv.p)) {
      equiv_p = suppressWarnings(as.numeric(to$equiv.p)[1])
    }
    if (is.na(pretrend_f_p) || is.na(equiv_p)) {
      ul = unlist(to, recursive = TRUE, use.names = TRUE)
      nm = names(ul)
      if (!is.null(nm)) {
        if (is.na(pretrend_f_p)) {
          ix = grep("F\\.p", nm, ignore.case = TRUE)
          if (length(ix)) {
            pretrend_f_p = suppressWarnings(as.numeric(ul[ix[1]]))
          }
        }
        if (is.na(equiv_p)) {
          ix2 = grep("equiv", nm, ignore.case = TRUE)
          if (length(ix2)) {
            equiv_p = suppressWarnings(max(as.numeric(ul[ix2]), na.rm = TRUE))
            if (!is.finite(equiv_p)) {
              equiv_p = NA_real_
            }
          }
        }
      }
    }
  }

  loo_ok = FALSE
  lto = tryCatch(fit$loo.test.out, error = function(e) NULL)
  if (!is.null(lto) && length(lto) > 0) {
    loo_ok = TRUE
  }

  tibble::tibble(
    pretrend_f_p = pretrend_f_p,
    equiv_p = equiv_p,
    loo_test_present = loo_ok
  )
}

#' Method-specific diagnostics (factor loadings only for factor estimators)
#'
#' For `cfe` / `fe` / `polynomial`, factor loadings are not applicable. For `ife` /
#' `mc` / `gsynth`, reports dimensions of `lambda` / `factor` when present.
#'
#' @param fit A `fect` object.
#' @return Single-row tibble.
#' @export
diagnose_fect_method = function(fit) {
  if (!inherits(fit, "fect")) {
    stop("diagnose_fect_method() needs a fect object", call. = FALSE)
  }
  method = tryCatch(as.character(fit$method)[1], error = function(e) NA_character_)
  if (is.na(method) || !nzchar(method)) {
    method = "unknown"
  }
  fac_ok = method %in% c("ife", "mc", "gsynth")
  r = tryCatch(fit$r, error = function(e) NA_real_)
  ld = tryCatch(dim(fit$lambda), error = function(e) NULL)
  fd = tryCatch(dim(fit$factor), error = function(e) NULL)
  tibble::tibble(
    method = method,
    factor_estimator = fac_ok,
    r = r,
    lambda_nrow = if (!is.null(ld)) ld[1] else NA_integer_,
    lambda_ncol = if (!is.null(ld)) ld[2] else NA_integer_,
    factor_nrow = if (!is.null(fd)) fd[1] else NA_integer_,
    factor_ncol = if (!is.null(fd)) fd[2] else NA_integer_
  )
}

#' @keywords internal
#' Add `time_id`, `.y_fect`, and `D` when `data` is raw panel rows (e.g. same
#' tibble passed to [get_fect()] before internal prep). Matches [prepare_data_fect()]
#' / fit-time outcome evaluation.
augment_data_fect_for_grid = function(meta, data) {
  date_col = meta$date_col
  unit_col = meta$unit_col
  treated_col = meta$treated_col
  assert_required_columns_fect(
    data,
    unique(c(date_col, unit_col, treated_col)),
    "augment_data_fect_for_grid"
  )
  date_levels = attr(meta$data_fit, "fect_date_levels")
  if (is.null(date_levels)) {
    date_levels = sort(unique(as.character(meta$data_fit[[date_col]])))
  }
  out = data
  need_tid = !"time_id" %in% names(out) || all(is.na(out$time_id))
  if (need_tid) {
    out$time_id = match(as.character(out[[date_col]]), date_levels)
  }
  if (!".y_fect" %in% names(out)) {
    out$.y_fect = suppressWarnings(
      as.numeric(eval(meta$lhs_expr, envir = out, enclos = baseenv()))
    )
  }
  if (!"D" %in% names(out) && treated_col %in% names(out)) {
    out$D = as.integer(as.logical(out[[treated_col]]))
  }
  out
}

#' Observation-level prediction SEs for `fect` CFE fits (analytic / fast path)
#'
#' Two-way residual variance decomposition on untreated observations. This is
#' the analytic alternative to `vartype = "bootstrap"` — called automatically
#' by [get_fect()] when `se = TRUE, vartype = "analytic"`, and also available
#' as a standalone post-fit helper.
#'
#' **Method `"twoway_resid"`**: fits
#' `E[ε²_it] ~ 1 + (1|unit) + (1|time)` on untreated residuals via
#' `lme4::lmer` (preferred — random effects shrink sparse cells toward the grand
#' mean). Falls back to `lm` if `lme4` is not installed (can predict negative
#' variances for unrepresented cells; those are floored to 0).
#'
#' **Caveats:** ignores parameter uncertainty (hat matrix term, negligible at
#' large N); assumes residual variance is stationary across treated/untreated
#' regimes (reasonable under parallel trends); does not account for
#' within-monitor temporal autocorrelation.
#'
#' @param fit Object from [get_fect()].
#' @param newdata Optional panel rows for prediction; defaults to
#'   `fect_meta$data_fit`. Must contain the `unit_col` and a `time_id` column
#'   (added automatically if raw panel rows are passed).
#' @param method `"twoway_resid"` (default) or `"constant"` (returns
#'   `sqrt(sigma2)` uniformly — the same proxy as `se = FALSE`).
#' @return Numeric vector of length `nrow(newdata)` — SEs on the **model
#'   (transformed) scale**, matching `yhat0` in [get_grid_fect()].
#' @export
fect_pred_se = function(fit, newdata = NULL, method = c("twoway_resid", "constant")) {
  method = match.arg(method)
  if (!inherits(fit, "fect")) {
    stop("fect_pred_se() requires a fect fit", call. = FALSE)
  }
  meta = attr(fit, "fect_meta")
  if (is.null(meta)) {
    stop("fect_pred_se() requires fect_meta from get_fect()", call. = FALSE)
  }

  sigma_const = max(sqrt(meta$sigma2), 1e-6)
  unit_col = meta$unit_col

  target_data = if (is.null(newdata)) {
    meta$data_fit
  } else {
    augment_data_fect_for_grid(meta, newdata)
  }
  n_target = nrow(target_data)

  if (method == "constant") {
    return(rep(sigma_const, n_target))
  }

  # --- method == "twoway_resid" -------------------------------------------
  t_build = Sys.time()
  id_vec   = as.character(fit$id)
  time_vec = as.integer(fit$rawtime)
  Ydat = fit$Y.dat
  Yct  = fit$Y.ct

  n_time  = length(time_vec)
  n_units = length(id_vec)

  # Expand fect matrices [n_time × n_units] to long format (observed cells only)
  t_idx = rep(seq_len(n_time),  times = n_units)
  u_idx = rep(seq_len(n_units), each  = n_time)

  resid_long = tibble::tibble(
    unit_id = id_vec[u_idx],
    time_id = time_vec[t_idx],
    ydat    = Ydat[cbind(t_idx, u_idx)],
    yct     = Yct[cbind(t_idx, u_idx)]
  ) |>
    dplyr::filter(is.finite(.data$ydat), is.finite(.data$yct)) |>
    dplyr::mutate(resid = .data$ydat - .data$yct)

  # Keep only untreated obs via D from meta$data_fit
  d_lookup = meta$data_fit |>
    dplyr::select(dplyr::all_of(c(unit_col, "time_id", "D"))) |>
    dplyr::mutate(
      unit_id = as.character(.data[[unit_col]]),
      time_id = as.integer(.data$time_id)
    ) |>
    dplyr::select(unit_id, time_id, D)

  resid_long = resid_long |>
    dplyr::left_join(d_lookup, by = c("unit_id", "time_id")) |>
    dplyr::filter(.data$D %in% 0L, is.finite(.data$resid)) |>
    dplyr::mutate(resid2 = .data$resid^2)

  if (nrow(resid_long) < 10L) {
    warning(
      "fect_pred_se: fewer than 10 untreated residuals; returning constant SE.",
      call. = FALSE
    )
    return(rep(sigma_const, n_target))
  }
  message(sprintf(
    "[fect.timing] fect_pred_se resid_long build_s=%.1f n=%d n_units=%d n_times=%d",
    as.numeric(difftime(Sys.time(), t_build, units = "secs")),
    nrow(resid_long),
    dplyr::n_distinct(resid_long$unit_id),
    dplyr::n_distinct(resid_long$time_id)
  ))

  # Fit two-way variance model on squared residuals
  var_model = NULL
  use_lme4  = requireNamespace("lme4", quietly = TRUE)

  if (use_lme4) {
    t_lmer = Sys.time()
    lmer_err = NULL
    var_model = tryCatch(
      lme4::lmer(
        resid2 ~ 1 + (1 | unit_id) + (1 | time_id),
        data    = resid_long,
        control = lme4::lmerControl(optimizer = "bobyqa")
      ),
      error = function(e) { lmer_err <<- conditionMessage(e); NULL }
    )
    message(sprintf(
      "[fect.timing] fect_pred_se var_model=lme4 fit_s=%.1f ok=%s%s",
      as.numeric(difftime(Sys.time(), t_lmer, units = "secs")),
      !is.null(var_model),
      if (is.null(var_model)) paste0(" err=", substr(if (is.null(lmer_err)) "?" else lmer_err, 1, 120)) else ""
    ))
    if (is.null(var_model)) use_lme4 = FALSE
  }

  if (is.null(var_model)) {
    # lm fallback: no shrinkage; new levels → NA → replaced with global mean.
    # CAUTION: factor(time_id) can have thousands of levels — this lm builds a
    # dense n × (levels) model matrix and can dwarf the fect fit itself; the
    # timing line below makes that visible.
    t_lm = Sys.time()
    var_model = tryCatch(
      stats::lm(resid2 ~ factor(unit_id) + factor(time_id), data = resid_long),
      error = function(e) NULL
    )
    message(sprintf(
      "[fect.timing] fect_pred_se var_model=lm-fallback fit_s=%.1f ok=%s",
      as.numeric(difftime(Sys.time(), t_lm, units = "secs")),
      !is.null(var_model)
    ))
  }

  if (is.null(var_model)) {
    warning("fect_pred_se: variance model failed; returning constant SE.", call. = FALSE)
    return(rep(sigma_const, n_target))
  }

  pred_df = tibble::tibble(
    unit_id = as.character(target_data[[unit_col]]),
    time_id = as.integer(target_data$time_id)
  )
  global_mean_var = mean(resid_long$resid2, na.rm = TRUE)

  t_pred = Sys.time()
  sigma2_pred = if (use_lme4) {
    tryCatch(
      stats::predict(var_model, newdata = pred_df, allow.new.levels = TRUE),
      error = function(e) rep(global_mean_var, n_target)
    )
  } else {
    raw = suppressWarnings(
      tryCatch(
        stats::predict(var_model, newdata = pred_df),
        error = function(e) rep(NA_real_, n_target)
      )
    )
    dplyr::if_else(is.finite(raw), raw, global_mean_var)
  }
  message(sprintf(
    "[fect.timing] fect_pred_se predict_s=%.1f n_target=%d path=%s",
    as.numeric(difftime(Sys.time(), t_pred, units = "secs")),
    n_target, if (use_lme4) "lme4" else "lm-fallback"
  ))

  sqrt(pmax(sigma2_pred, 0))
}

#' Map fect matrices to observation-level yhat (model scale)
#'
#' @param fit Object from [get_fect()].
#' @param data Panel rows: defaults to `fect_meta$data_fit`. If you pass the
#'   **original** analysis `data` (same columns as for [get_fect()], without
#'   `time_id` / `.y_fect`), those columns are **added** automatically via
#'   [augment_data_fect_for_grid()].
#' @details `se0`/`se1` are observation-specific when the fit was produced with
#'   `get_fect(..., se = TRUE, vartype = "analytic")` (two-way residual variance
#'   decomposition via [fect_pred_se()]). Otherwise they are a constant
#'   `sqrt(sigma2)` proxy. `se1 = 0` for rows where the factual outcome is observed.
#' @return `data` with `yhat0`, `yhat1`, `se0`, `se1`, `id`, and effect columns.
#' @export
get_grid_fect = function(fit, data = NULL) {
  if (!inherits(fit, "fect")) {
    stop("get_grid_fect() requires a fect fit", call. = FALSE)
  }
  meta = attr(fit, "fect_meta")
  if (is.null(meta)) {
    stop("get_grid_fect() requires fect_meta from get_fect()", call. = FALSE)
  }
  if (is.null(data)) {
    data = meta$data_fit
  } else {
    data = augment_data_fect_for_grid(meta, data)
  }

  id_vec = as.character(fit$id)
  time_vec = as.character(fit$rawtime)
  Yct = fit$Y.ct
  Ydt = fit$Y.dat

  se_const = max(sqrt(meta$sigma2), 1e-6)

  ti = match(as.character(data$time_id), time_vec)
  ni = match(as.character(data[[meta$unit_col]]), id_vec)

  ok = is.finite(ti) & is.finite(ni)
  yhat0 = rep(NA_real_, nrow(data))
  yhat1 = rep(NA_real_, nrow(data))
  yhat0[ok] = Yct[cbind(ti[ok], ni[ok])]
  yhat1[ok] = Ydt[cbind(ti[ok], ni[ok])]

  # Use observation-level SEs from analytic path when available; constant proxy otherwise
  se_vec = if (!is.null(meta$analytic_se_tbl)) {
    se_tbl    = meta$analytic_se_tbl
    unit_keys = as.character(data[[meta$unit_col]])
    time_keys = as.integer(data$time_id)
    row_key   = paste(unit_keys, time_keys, sep = "\x01")
    tbl_key   = paste(se_tbl$unit_id, se_tbl$time_id, sep = "\x01")
    idx       = match(row_key, tbl_key)
    se_matched = se_tbl$se_analytic[idx]
    dplyr::if_else(is.finite(se_matched), se_matched, se_const)
  } else {
    rep(se_const, nrow(data))
  }

  grid = data |>
    mutate(
      yhat0 = yhat0,
      yhat1 = yhat1,
      se0 = se_vec,
      se1 = se_vec,
      id = dplyr::row_number()
    )

  obs_ok = is.finite(grid$.y_fect)
  grid = grid |>
    mutate(
      yhat1 = if_else(obs_ok, .data$.y_fect, .data$yhat1),
      se1 = if_else(obs_ok, 0, .data$se1)
    )

  grid
}

#' Row-level effects compatible with [get_att_fect()] (phase 1: analytic SE proxy)
#'
#' Uses `sqrt(sigma2)`-based `sediff` so [get_qis_fect()] can call [get_att_fect()].
#' Refit with `get_fect(..., se = TRUE, vartype = \"bootstrap\", nboots = ...)` for
#' fect’s own bootstrap/jackknife uncertainty on `est.att` etc.; downstream `se_att`
#' here remains **proxy-based** unless extended to map fect SEs into the grid.
#'
#' @export
#' This is the **back-transform** stage: it takes each prediction's `yhat`/`se`
#' (already computed on the model/sqrt scale — the SEs come from the upstream
#' **analytic** two-way residual method in [get_fect()], `vartype = "analytic"`,
#' which is a separate choice from bootstrap and is NOT touched here) and maps
#' them to native units (e.g. µg/m³).
#'
#' @param backtransform How to map `yhat`/`se` from the model (sqrt) scale to
#'   native units. Named by the **transform applied** (so adding a model on a
#'   different scale just adds a value — e.g. a future log-outcome model would
#'   add `"exp"` that draws and exponentiates):
#'   * `"square_mc"` (default, **the honest method**) — for each cell, draw
#'     `n` samples from `N(yhat, se)` on the sqrt scale, apply `inverse_fn`
#'     (square) to each, then take `mean`/`sd` to get native-scale yhat/se.
#'     Literal Monte-Carlo propagation of error. This is the original method.
#'   * `"square_moments"` — uses the **exact** closed-form moments of `Y = X²`
#'     for `X ~ N(μ, s)`: `E[Y] = μ² + s²`, `SD[Y] = s · sqrt(2s² + 4μ²)`.
#'     These are what `n → ∞` Monte-Carlo draws converge to — same answer at
#'     infinity, no draws, scales to 200k+ treated cells. Opt-in performance
#'     escape hatch (use when `n` MC draws is too slow). For non-square /
#'     non-identity inverses, falls through to literal `n` draws.
#'   * `"square"` — **deprecated** alias for `"square_moments"` (issues a
#'     one-time warning). Kept for back-compat with existing scripts that
#'     hard-code the old name.
#'   * `"point"` — legacy/incorrect proxy that applies `inverse_fn` to the
#'     **point** estimate and keeps the sqrt-scale SE (biased point, missing
#'     the +sigma^2 Jensen term; scale-mismatched SE). Kept only to reproduce
#'     pre-2026-06-13 pin values.
#'   NOTE: this is the **back-transform** stage and is independent of the
#'   **SE method** ("analytic" vs "bootstrap"), which is decided upstream in
#'   [get_fect()] (`vartype`) and is untouched here.
#' @param n Monte Carlo draws for `"square_mc"` and for the literal-draw
#'   fallback (non-square inverse).
get_simeffects_fect = function(
    grid,
    fit,
    start = NULL,
    end = NULL,
    date_col = "date",
    treated_col = "treated",
    backtransform = c("square_moments", "square_mc", "square", "point"),
    n = 1000L
) {
  backtransform = match.arg(backtransform)
  if (identical(backtransform, "square")) {
    warning(
      "backtransform = \"square\" is deprecated; use \"square_moments\" for the exact closed-form moments, or \"square_mc\" for literal Monte-Carlo draws (now the default).",
      call. = FALSE
    )
    backtransform = "square_moments"
  }
  meta = attr(fit, "fect_meta")
  inverse_fn = if (!is.null(meta)) meta$inverse_fn else identity
  sigma = max(sqrt(meta$sigma2), 1e-6)

  date_vals = as.Date(grid[[date_col]])
  start_date = if (is.null(start)) min(date_vals, na.rm = TRUE) else as.Date(start)
  end_date = if (is.null(end)) max(date_vals, na.rm = TRUE) else as.Date(end)

  base = grid |>
    mutate(
      date_filter = as.Date(.data[[date_col]]),
      # Preserve observation-level se0 from get_grid_fect; fall back to sigma if missing
      se0 = dplyr::if_else(is.finite(.data$se0) & .data$se0 > 0, .data$se0, sigma),
      se1 = dplyr::if_else(is.finite(.data$se1) & .data$se1 >= 0, .data$se1, 0)
    ) |>
    filter(
      .data$date_filter >= start_date,
      .data$date_filter <= end_date,
      .data[[treated_col]] %in% TRUE,
      is.finite(.data$yhat0), is.finite(.data$yhat1)
    ) |>
    select(-date_filter)

  if (identical(backtransform, "point")) {
    # LEGACY/incorrect: square the point estimate, keep the sqrt-scale SE.
    # Biased point + scale-mismatched SE; only to reproduce old pin values.
    return(
      base |>
        mutate(
          yhat0 = inverse_fn(.data$yhat0),
          yhat1 = inverse_fn(.data$yhat1),
          diff  = .data$yhat1 - .data$yhat0,
          sediff = sqrt(.data$se0^2 + .data$se1^2)
        ) |>
        filter(is.finite(.data$diff))
    )
  }

  # Detect the inverse so the common square/identity cases can use exact
  # closed-form moments when the caller asks for them; everything else falls
  # through to the literal-MC path below. (A future "exp" for log outcomes
  # would be a sibling branch here.)
  tv = suppressWarnings(inverse_fn(c(0, 2, -2, 3)))
  is_square   = isTRUE(all.equal(tv, c(0, 4, 4, 9)))
  is_identity = isTRUE(all.equal(tv, c(0, 2, -2, 3)))

  # square_moments: exact closed-form when the inverse is square/identity.
  # square_mc and any non-square inverse skip this and use MC below.
  if (identical(backtransform, "square_moments") && (is_square || is_identity)) {
    # X ~ N(mu, s);  Y = inverse_fn(X).
    #   identity:  E[Y] = mu,            SD[Y] = s
    #   square:    E[Y] = mu^2 + s^2,    SD[Y] = s * sqrt(2 s^2 + 4 mu^2)
    bt_mean = function(mu, s) if (is_square) mu^2 + s^2 else mu
    bt_sd   = function(mu, s) if (is_square) s * sqrt(2 * s^2 + 4 * mu^2) else s
    return(
      base |>
        mutate(
          .y0n  = bt_mean(.data$yhat0, .data$se0),
          .y1n  = bt_mean(.data$yhat1, .data$se1),
          .s0n  = bt_sd(.data$yhat0, .data$se0),
          .s1n  = bt_sd(.data$yhat1, .data$se1),
          yhat0 = .data$.y0n,
          yhat1 = .data$.y1n,
          se0   = .data$.s0n,
          se1   = .data$.s1n,
          diff   = .data$yhat1 - .data$yhat0,
          sediff = sqrt(.data$se1^2 + .data$se0^2)
        ) |>
        select(-".y0n", -".y1n", -".s0n", -".s1n") |>
        filter(is.finite(.data$diff))
    )
  }

  # Literal Monte Carlo: draw `n` samples on the model (sqrt) scale, apply
  # `inverse_fn` to each, summarize native. Handles two cases:
  #   - backtransform = "square_mc" (the honest default — the original method)
  #   - any non-square / non-identity inverse under "square_moments"
  # Mirrors get_simeffects_did().
  keep_cols = intersect(c("id", date_col, treated_col, "fullaqsid", "metro_id"),
                        names(base))
  base |>
    group_by(.data$id) |>
    reframe(
      ysim1_t = stats::rnorm(n = n, mean = .data$yhat1, sd = pmax(.data$se1, 0)),
      ysim0_t = stats::rnorm(n = n, mean = .data$yhat0, sd = pmax(.data$se0, 0))
    ) |>
    mutate(
      ysim1 = inverse_fn(.data$ysim1_t),
      ysim0 = inverse_fn(.data$ysim0_t),
      diff  = .data$ysim1 - .data$ysim0
    ) |>
    group_by(.data$id) |>
    summarize(
      sediff = stats::sd(.data$diff),
      diff   = mean(.data$diff),
      yhat1  = mean(.data$ysim1),
      yhat0  = mean(.data$ysim0),
      se1    = stats::sd(.data$ysim1),
      se0    = stats::sd(.data$ysim0),
      .groups = "drop"
    ) |>
    left_join(base |> select(dplyr::all_of(keep_cols)), by = "id") |>
    filter(is.finite(.data$diff))
}

#' ATT aggregates mirroring [get_qis_did()]
#'
#' Pools per-cell effects (from [get_simeffects_fect()]) at each requested
#' grain via [get_att_fect()]. See §4 S4 in
#' `docs/model/README_fect_uncertainty.md` for the pooling math and the
#' `weighting` toggle.
#'
#' @param data `NULL` uses the fitted `data_fit` grid. Otherwise pass the same
#'   panel you used for [get_fect()] (e.g. `data` with `date`, `fullaqsid`,
#'   `treated`); [get_grid_fect()] adds `time_id`, `.y_fect`, and `D` as needed.
#' @param weighting Forwarded to [get_att_fect()]. `"unweighted"` (default,
#'   matches paper) or `"ivw"`.
#'
#' @export
get_qis_fect = function(
    fit,
    data = NULL,
    start = NULL,
    end = NULL,
    date_col = "date",
    unit_col = "fullaqsid",
    metro_col = "metro_id",
    aggregates = c("per_metro", "per_metro_month", "overall"),
    weighting = c("unweighted", "ivw")
) {
  weighting = match.arg(weighting)

  valid_aggregates = c(
    "overall", "per_metro", "per_metro_month", "per_month",
    "per_monitor", "per_week", "per_week_metro", "per_day_metro",
    "per_day_metro_unit"
  )
  if (!all(aggregates %in% valid_aggregates)) {
    stop("Invalid aggregates in get_qis_fect()", call. = FALSE)
  }

  .qt0 = Sys.time(); .qlast = .qt0
  .qlap = function(stage) {
    now = Sys.time()
    message(sprintf("[fect.qis.timing] stage=%s stage_s=%.1f total_s=%.1f", stage,
                    as.numeric(difftime(now, .qlast, units = "secs")),
                    as.numeric(difftime(now, .qt0, units = "secs"))))
    .qlast <<- now
    invisible(NULL)
  }
  grid = get_grid_fect(fit, data)
  .qlap("get_grid_fect")
  sims = get_simeffects_fect(
    grid = grid,
    fit = fit,
    start = start,
    end = end,
    date_col = date_col
  )

  .qlap("get_simeffects_fect")
  stats = list()

  if ("overall" %in% aggregates) {
    stats = append(stats, list(sims |> get_att_fect(weighting = weighting) |> mutate(type = "overall")))
  }
  .qlap("agg_overall")
  if ("per_metro" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          group_by(.data[[metro_col]]) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_metro")
      )
    )
  }
  .qlap("agg_per_metro")
  if ("per_metro_month" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(month = lubridate::floor_date(.data[[date_col]], unit = "month")) |>
          group_by(.data[[metro_col]], month) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_metro_month")
      )
    )
  }
  .qlap("agg_per_metro_month")
  if ("per_month" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(month = lubridate::floor_date(.data[[date_col]], unit = "month")) |>
          group_by(month) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_month")
      )
    )
  }
  .qlap("agg_per_month")
  if ("per_monitor" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          group_by(.data[[metro_col]], .data[[unit_col]]) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_monitor")
      )
    )
  }
  .qlap("agg_per_monitor")
  if ("per_day_metro" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(day = as.Date(.data[[date_col]])) |>
          group_by(.data[[metro_col]], day) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_day_metro")
      )
    )
  }
  .qlap("agg_per_day_metro")
  if ("per_day_metro_unit" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(day = as.Date(.data[[date_col]])) |>
          group_by(.data[[metro_col]], .data[[unit_col]], day) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_day_metro_unit")
      )
    )
  }
  .qlap("agg_per_day_metro_unit")
  if ("per_week" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(week = lubridate::week(.data[[date_col]]), year = lubridate::year(.data[[date_col]])) |>
          group_by(week, year) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_week")
      )
    )
  }
  .qlap("agg_per_week")
  if ("per_week_metro" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(week = lubridate::week(.data[[date_col]]), year = lubridate::year(.data[[date_col]])) |>
          group_by(.data[[metro_col]], week, year) |>
          get_att_fect(weighting = weighting) |>
          mutate(type = "per_week_metro")
      )
    )
  }

  .qlap("agg_per_week_metro")
  out = dplyr::bind_rows(stats)
  .qlap("bind_rows")
  out
}

#' Long-format effects (treated rows), mirroring [get_long_effects_did()]
#'
#' @export
get_long_effects_fect = function(
    grid,
    fit,
    start = NULL,
    end = NULL,
    date_col = "date",
    unit_col = "fullaqsid",
    treated_col = "treated"
) {
  sims = get_simeffects_fect(
    grid = grid,
    fit = fit,
    start = start,
    end = end,
    date_col = date_col,
    treated_col = treated_col
  )

  sims |>
    select(any_of(c("metro_id", unit_col, date_col, "yhat0", "se0", "yhat1", "se1", "diff", "sediff"))) |>
    pivot_longer(
      cols = c(yhat0, yhat1, diff),
      names_to = "outcome_type",
      values_to = "estimate"
    ) |>
    mutate(
      se = case_when(
        .data$outcome_type == "yhat0" ~ .data$se0,
        .data$outcome_type == "yhat1" ~ .data$se1,
        .data$outcome_type == "diff" ~ .data$sediff,
        TRUE ~ NA_real_
      ),
      outcome_type = recode(.data$outcome_type, diff = "effect")
    ) |>
    select(any_of(c("metro_id", unit_col, date_col)), outcome_type, estimate, se) |>
    arrange(.data[[unit_col]], .data[[date_col]], outcome_type)
}

#' Summarize fect diagnostics (goodness of fit / identification)
#'
#' @param fit Output of [get_fect()].
#' @param full If TRUE, note placebo/carryover/loo options (not run automatically).
#' @return List with `sigma2`, `test_out`, `loo_test_out`, `tests_tbl` from
#'   [summarize_fect_tests()], `method_tbl` from [diagnose_fect_method()], `gof`
#'   from [get_gof_fect()] when `include_gof = TRUE`.
#' @param include_gof If TRUE, run [get_gof_fect()] (adds one-row tibble `gof`).
#' @export
validate_fect_fit = function(fit, full = FALSE, include_gof = TRUE) {
  if (!inherits(fit, "fect")) {
    stop("validate_fect_fit() needs a fect object", call. = FALSE)
  }
  out = list(
    sigma2 = fit$sigma2,
    test_out = fit$test.out,
    loo_test_out = fit$loo.test.out,
    tests_tbl = summarize_fect_tests(fit),
    method_tbl = diagnose_fect_method(fit)
  )
  if (isTRUE(include_gof)) {
    out$gof = get_gof_fect(fit)
  }
  if (full) {
    out$notes = paste(
      "Optional: [get_fect_placebo()] for placeboTest + placebo.period (fect §2.3.1);",
      "carryoverTest + carryover.period; loo = TRUE (slow); see ?fect and Liu, Wang, Xu (2024)."
    )
  }
  out
}

#' Stabilize SEs by replacing zeros/negatives with the 10th percentile of positives.
#'
#' Local helper for [get_att_fect()] so the pooling pipeline does not need to
#' source `_did` utilities. Mirrors `stabilize_se_did()` exactly.
stabilize_se_fect = function(x) {
  positive = x[is.finite(x) & x > 0]
  fallback = if (length(positive) > 0) {
    as.numeric(stats::quantile(positive, probs = 0.1, na.rm = TRUE, names = FALSE))
  } else {
    1
  }
  if_else(is.finite(x) & x > 0, x, fallback)
}

#' Pool per-cell effects into one ATT (fect-native; see §4 S4 in
#' `docs/model/README_fect_uncertainty.md`).
#'
#' S4 is a **pooling** problem, not a confidence-SE problem: we combine `n`
#' already-computed per-cell effects, each with its own predictive SE, into one
#' aggregate `att`/`se_att` by propagating error through a weighted average.
#'
#' Two rules — both correct under independence between cells:
#'   * `weighting = "unweighted"` (current default; matches paper):
#'       `att    = mean(diff)`
#'       `se_att = sqrt(sum(sediff^2)) / n()`
#'   * `weighting = "ivw"` — precision-weighted:
#'       `att    = sum(diff / sediff^2) / sum(1 / sediff^2)`
#'       `se_att = sqrt(1 / sum(1 / sediff^2))`
#'
#' Levels (`yhat0`/`yhat1`) are always IVW; this matches the longstanding DiD
#' pipeline and is unaffected by the diff weighting.
#'
#' **Independence caveat.** The pooling SE assumes per-cell `diff_i` are
#' independent measurements. Strictly they aren't (they share a fitted model);
#' at large training-N the cross-cell covariance from shared parameters is
#' small relative to per-cell residual noise.
summarize_att_core_fect = function(effects, weighting = c("unweighted", "ivw")) {
  weighting = match.arg(weighting)

  cleaned = effects |>
    filter(
      is.finite(.data$diff),
      is.finite(.data$sediff),
      is.finite(.data$yhat1),
      is.finite(.data$yhat0),
      is.finite(.data$se1),
      is.finite(.data$se0)
    ) |>
    mutate(
      se1    = stabilize_se_fect(.data$se1),
      se0    = stabilize_se_fect(.data$se0),
      sediff = stabilize_se_fect(.data$sediff)
    )

  n_effects = nrow(cleaned)
  if (n_effects == 0) {
    return(
      tibble(
        yhat1 = NA_real_,
        yhatse1 = NA_real_,
        yhat0 = NA_real_,
        yhatse0 = NA_real_,
        att = NA_real_,
        se_att = NA_real_,
        t = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        stars = "",
        pct_change = NA_real_,
        n_effects = 0L,
        weighting = weighting
      )
    )
  }

  # Levels: always IVW (unchanged behavior, matches paper).
  out_levels = cleaned |>
    summarize(
      yhat1   = sum(.data$yhat1 / .data$se1^2) / sum(1 / .data$se1^2),
      yhatse1 = sqrt(1 / sum(1 / .data$se1^2)),
      yhat0   = sum(.data$yhat0 / .data$se0^2) / sum(1 / .data$se0^2),
      yhatse0 = sqrt(1 / sum(1 / .data$se0^2)),
      .groups = "drop"
    )

  # Diff + se_att: toggle by `weighting`.
  if (identical(weighting, "unweighted")) {
    out_diff = cleaned |>
      summarize(
        att    = mean(.data$diff),
        se_att = sqrt(sum(.data$sediff^2)) / n(),
        .groups = "drop"
      )
  } else { # "ivw"
    out_diff = cleaned |>
      summarize(
        att    = sum(.data$diff / .data$sediff^2) / sum(1 / .data$sediff^2),
        se_att = sqrt(1 / sum(1 / .data$sediff^2)),
        .groups = "drop"
      )
  }

  dplyr::bind_cols(out_levels, out_diff) |>
    mutate(
      t = if_else(.data$se_att > 0, .data$att / .data$se_att, NA_real_),
      df = pmax(n_effects - 1, 1),
      p_value = if_else(
        is.finite(.data$t),
        2 * (1 - stats::pt(q = abs(.data$t), df = .data$df)),
        NA_real_
      ),
      stars = if_else(is.finite(.data$p_value), gtools::stars.pval(.data$p_value), ""),
      pct_change = if_else(abs(.data$yhat0) > 0, (.data$att / .data$yhat0) * 100, NA_real_),
      n_effects = as.integer(n_effects),
      weighting = weighting
    )
}

#' Pool per-cell effects into an ATT (fect-native).
#'
#' Replaces the previous no-op wrapper around `get_att_did()`. Exposes a
#' `weighting=` toggle and handles grouped tibbles the same way as the DiD
#' aggregator. See [summarize_att_core_fect()] for the math.
#'
#' @param effects Tibble with `diff`, `sediff`, `yhat1`, `se1`, `yhat0`, `se0`,
#'   optionally grouped. Typically the output of [get_simeffects_fect()] or
#'   one of the grouped frames inside [get_qis_fect()].
#' @param weighting `"unweighted"` (default, matches paper) or `"ivw"`.
#' @export
get_att_fect = function(effects, weighting = c("unweighted", "ivw")) {
  weighting = match.arg(weighting)

  assert_required_columns_fect(
    data = effects,
    required = c("diff", "sediff", "yhat1", "se1", "yhat0", "se0"),
    fn_name = "get_att_fect"
  )

  if (dplyr::is_grouped_df(effects)) {
    return(summarize_att_grouped_fect(effects, weighting = weighting))
  }

  summarize_att_core_fect(effects, weighting = weighting)
}

#' Vectorized grouped ATT pooling (replaces the per-group `group_modify`)
#'
#' `group_modify(~summarize_att_core_fect(.x, ...))` calls an R closure and
#' allocates a tibble ONCE PER GROUP. At the `per_day_metro_unit` grain that is
#' ~145k groups and cost 250.6 s/spec; `per_day_metro` cost 91.3 s — together
#' 98% of `qis_aggregates`, and linear in group COUNT rather than data volume
#' (see `docs/model/TIMING.md`). Every quantity in
#' [summarize_att_core_fect()] is a `sum()`, `mean()` or `n()` reduction, so the
#' whole thing collapses to one grouped `summarize()`.
#'
#' Behavioural parity notes (the two places a naive rewrite would drift):
#'   * A group whose rows are ALL non-finite is dropped by `summarize()` but
#'     `group_modify()` emitted an all-NA row with `n_effects = 0L`. The group
#'     keys are captured up front and re-joined so those rows survive.
#'   * `group_modify()` returns groups in sorted key order and keeps the result
#'     grouped. Both are reproduced here.
#'
#' @keywords internal
summarize_att_grouped_fect = function(effects, weighting = c("unweighted", "ivw")) {
  weighting = match.arg(weighting)
  gvars = dplyr::group_vars(effects)

  keys = effects |>
    dplyr::ungroup() |>
    dplyr::distinct(dplyr::across(dplyr::all_of(gvars))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(gvars)))

  cleaned = effects |>
    dplyr::ungroup() |>
    filter(
      is.finite(.data$diff),
      is.finite(.data$sediff),
      is.finite(.data$yhat1),
      is.finite(.data$yhat0),
      is.finite(.data$se1),
      is.finite(.data$se0)
    ) |>
    mutate(
      se1    = stabilize_se_fect(.data$se1),
      se0    = stabilize_se_fect(.data$se0),
      sediff = stabilize_se_fect(.data$sediff)
    )

  agg = cleaned |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gvars))) |>
    summarize(
      yhat1   = sum(.data$yhat1 / .data$se1^2) / sum(1 / .data$se1^2),
      yhatse1 = sqrt(1 / sum(1 / .data$se1^2)),
      yhat0   = sum(.data$yhat0 / .data$se0^2) / sum(1 / .data$se0^2),
      yhatse0 = sqrt(1 / sum(1 / .data$se0^2)),
      att = if (identical(weighting, "unweighted")) {
        mean(.data$diff)
      } else {
        sum(.data$diff / .data$sediff^2) / sum(1 / .data$sediff^2)
      },
      se_att = if (identical(weighting, "unweighted")) {
        sqrt(sum(.data$sediff^2)) / dplyr::n()
      } else {
        sqrt(1 / sum(1 / .data$sediff^2))
      },
      n_effects = dplyr::n(),
      .groups = "drop"
    )

  keys |>
    dplyr::left_join(agg, by = gvars) |>
    mutate(
      n_effects = as.integer(dplyr::coalesce(.data$n_effects, 0L)),
      t = if_else(!is.na(.data$se_att) & .data$se_att > 0,
                  .data$att / .data$se_att, NA_real_),
      df = if_else(.data$n_effects > 0L,
                   as.numeric(pmax(.data$n_effects - 1L, 1L)), NA_real_),
      p_value = if_else(
        is.finite(.data$t),
        2 * (1 - stats::pt(q = abs(.data$t), df = .data$df)),
        NA_real_
      ),
      stars = if_else(is.finite(.data$p_value),
                      gtools::stars.pval(.data$p_value), ""),
      pct_change = if_else(!is.na(.data$yhat0) & abs(.data$yhat0) > 0,
                           (.data$att / .data$yhat0) * 100, NA_real_),
      weighting = weighting
    ) |>
    dplyr::relocate(dplyr::all_of(gvars)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(gvars)))
}

#' Compare ATTs on two dates (fect)
#'
#' Thin wrapper around [compare_att_two_dates_did()] for tibbles from [get_qis_fect()].
#' Arguments are passed through unchanged.
#'
#' **Audit verdict (ADR-0011, 2026-06-30):** correct as a wrapper under the
#' pooling framing. Each per-date ATT comes from a pool over disjoint cells
#' (different days), so the per-date estimates are independent; the DiD
#' compare function combines variances as `Var(att2 − att1) = se1² + se2²`
#' with Welch–Satterthwaite df, which is correct under that independence.
#' The `weighting=` choice made upstream in `get_att_fect` flows through
#' unchanged. No further changes needed.
#'
#' @export
compare_att_two_dates_fect = function(
    att_df,
    date1,
    date2,
    id_cols,
    day_col = "day",
    n_draws = 1000L,
    type = NULL,
    method = c("mc", "analytic"),
    warn_incomplete = TRUE
) {
  if (!exists("compare_att_two_dates_did", mode = "function")) {
    stop("compare_att_two_dates_fect() requires job/model/did/functions_did.R sourced first", call. = FALSE)
  }
  compare_att_two_dates_did(
    att_df = att_df,
    date1 = date1,
    date2 = date2,
    id_cols = id_cols,
    day_col = day_col,
    n_draws = n_draws,
    type = type,
    method = method,
    warn_incomplete = warn_incomplete
  )
}

#' Compare ATTs on many dates (fect)
#'
#' Thin wrapper around [compare_att_many_dates_did()] for tibbles from [get_qis_fect()].
#' Arguments are passed through unchanged.
#'
#' **Audit verdict (ADR-0011, 2026-06-30):** correct as a wrapper under the
#' pooling framing. The WLS slope estimand uses `1/se_att²` precision weights
#' across dates — the right WLS for combining independent per-date estimates.
#' Reference and omnibus estimands are also correct under per-date
#' independence. The `weighting=` choice made upstream in `get_att_fect`
#' flows through unchanged. No further changes needed.
#'
#' @export
compare_att_many_dates_fect = function(
    att_df,
    dates,
    id_cols,
    day_col = "day",
    estimand = c("reference", "slope", "omnibus"),
    reference_date = NULL,
    time = NULL,
    type = NULL,
    method = c("mc", "analytic"),
    n_draws = 1000L,
    variance_combine = c("rubin", "analytic", "mc"),
    warn_incomplete = TRUE
) {
  if (!exists("compare_att_many_dates_did", mode = "function")) {
    stop("compare_att_many_dates_fect() requires job/model/did/functions_did.R sourced first", call. = FALSE)
  }
  compare_att_many_dates_did(
    att_df = att_df,
    dates = dates,
    id_cols = id_cols,
    day_col = day_col,
    estimand = estimand,
    reference_date = reference_date,
    time = time,
    type = type,
    method = method,
    n_draws = n_draws,
    variance_combine = variance_combine,
    warn_incomplete = warn_incomplete
  )
}

#' Prediction grid alias for fect
#'
#' Alias for [get_grid_fect()].
#'
#' @export
get_yhat_fect = function(fit, data = NULL) {
  get_grid_fect(fit, data)
}

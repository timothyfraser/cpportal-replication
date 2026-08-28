# functions_did.R
#
# Daily-level DiD helpers adapted from cpportal_transport and nyc_congestion_rep.
#
# BibTeX keys for methods cited in roxygen blocks below are listed in
# `job/model/did/functions_did_references.bib` (Welch--Satterthwaite,
# Rubin combining rules, Wald tests, WLS/GLS, bootstrap-style Monte Carlo tests).

library(dplyr)
library(tidyr)
library(broom)
library(gtools)
library(lubridate)

if (!exists("default_outcome_aq", mode = "function")) {
  default_outcome_aq = function() stats::as.formula("~ sqrt(aq_daily_mean)")
}

#' Validate required columns for DiD helpers
#'
#' Stops with an explicit message if any name in `required` is missing from
#' `names(data)`.
#'
#' @param data A data frame or data-frame-like object.
#' @param required Character vector of required column names.
#' @param fn_name Function name for informative error messages.
#' @return Called for its side effect of validation; invisibly `NULL` on success.
#' @keywords internal
assert_required_columns_did = function(data, required, fn_name) {
  missing = setdiff(required, names(data))
  if (length(missing) > 0) {
    stop(
      paste0(
        fn_name, "() missing required columns: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

#' Extract LHS expression from a one-sided outcome formula
#'
#' @param outcome A formula such as `~ sqrt(aq_daily_mean)`.
#' @return Language object (expression) for the response.
#' @keywords internal
extract_response_lhs_did = function(outcome) {
  if (!inherits(outcome, "formula")) {
    stop("outcome must be a formula, e.g. ~ sqrt(aq_daily_mean)", call. = FALSE)
  }
  if (length(outcome) < 2) {
    stop("outcome must be one-sided: ~ response_expression", call. = FALSE)
  }
  outcome[[2]]
}

#' Infer inverse transform from response LHS, or use user function
#'
#' Maps model-scale (transformed) values to native outcome scale for
#' [get_simeffects_did()] and downstream simulation.
#'
#' @param lhs_expr Response expression from [extract_response_lhs_did()].
#' @param response_inverse `NULL`, or a function `f(z)` for `z` on the transformed scale.
#' @return A function mapping transformed scale to native scale.
#' @keywords internal
infer_inverse_fn_did = function(lhs_expr, response_inverse = NULL) {
  if (!is.null(response_inverse)) {
    if (!is.function(response_inverse)) {
      stop("response_inverse must be NULL or a function(z) on the transformed scale", call. = FALSE)
    }
    return(response_inverse)
  }

  if (is.symbol(lhs_expr)) {
    return(identity)
  }

  if (!is.call(lhs_expr)) {
    stop(
      "Unsupported response expression; use response_inverse = function(z) ...",
      call. = FALSE
    )
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
    paste0(
      "Could not infer inverse for response ", deparse(lhs_expr),
      ". Supported: bare column, sqrt(), log(), log10(), log1p(), or pass response_inverse."
    ),
    call. = FALSE
  )
}

#' Build full \code{lm} formula for the DiD specification
#'
#' Constructs `response ~ did_treated + I(did_treated * did_days_since) +
#' did_date_fe + did_unit_fe` plus optional covariate terms.
#'
#' @param lhs_expr Response expression (language).
#' @param covariates `NULL` or a one-sided formula of extra RHS terms.
#' @return A two-sided [stats::formula()] suitable for [stats::lm()].
#' @keywords internal
build_lm_formula_did = function(lhs_expr, covariates = NULL) {
  termlabels = c(
    "did_treated",
    "I(did_treated * did_days_since)",
    "did_date_fe",
    "did_unit_fe"
  )

  if (!is.null(covariates)) {
    if (!inherits(covariates, "formula")) {
      stop("covariates must be NULL or a one-sided formula, e.g. ~ poly(x, 2)", call. = FALSE)
    }
    cov_terms = attr(stats::terms(covariates), "term.labels")
    if (length(cov_terms) > 0) {
      termlabels = c(termlabels, cov_terms)
    }
  }

  stats::reformulate(
    termlabels = termlabels,
    response = paste(deparse(lhs_expr), collapse = " ")
  )
}

#' Stabilize standard errors for inverse-variance weighting
#'
#' Replaces non-finite and non-positive values with a low positive fallback
#' (10th percentile of positive finite values, or 1) so division by `se^2` in
#' WLS-style steps remains stable.
#'
#' @param x Numeric vector of standard errors or scales.
#' @return Numeric vector of the same length as `x`.
#' @keywords internal
stabilize_se_did = function(x) {
  positive = x[is.finite(x) & x > 0]
  fallback = if (length(positive) > 0) {
    as.numeric(stats::quantile(positive, probs = 0.1, na.rm = TRUE, names = FALSE))
  } else {
    1
  }
  if_else(is.finite(x) & x > 0, x, fallback)
}

#' Predict from a fitted \code{lm} with standard errors
#'
#' Wraps [stats::predict()] with `se.fit = TRUE`. On failure, falls back to
#' prediction without `se.fit` and uses `summary(m)$sigma` as a constant
#' uncertainty scale for `se.fit`.
#'
#' @param m A fitted model from [stats::lm()].
#' @param newdata Passed to [stats::predict()].
#' @return A tibble with columns `fit` and `se.fit`.
#' @keywords internal
predict_with_se_did = function(m, newdata) {
  out = tryCatch(
    stats::predict(object = m, newdata = newdata, se.fit = TRUE),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(as_tibble(out) |> select(fit, se.fit))
  }

  fit_vals = tryCatch(
    stats::predict(object = m, newdata = newdata),
    error = function(e) NULL
  )
  sigma_fallback = summary(m)$sigma

  if (is.null(fit_vals)) {
    fit_vals = rep(NA_real_, nrow(newdata))
  }

  tibble(
    fit = as.numeric(fit_vals),
    se.fit = rep(sigma_fallback, length(fit_vals))
  )
}

summarize_att_core_did = function(effects) {
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
      se1 = stabilize_se_did(.data$se1),
      se0 = stabilize_se_did(.data$se0)
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
        n_effects = 0L
      )
    )
  }

  out = cleaned |>
    summarize(
      yhat1 = sum(.data$yhat1 / .data$se1^2) / sum(1 / .data$se1^2),
      yhatse1 = sqrt(1 / sum(1 / .data$se1^2)),
      yhat0 = sum(.data$yhat0 / .data$se0^2) / sum(1 / .data$se0^2),
      yhatse0 = sqrt(1 / sum(1 / .data$se0^2)),
      att = mean(.data$diff),
      se_att = sqrt(sum(.data$sediff^2)) / n(),
      .groups = "drop"
    )

  out = out |>
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
      n_effects = as.integer(n_effects)
    )

  out
}

#' Tidy model output with significance stars
#'
#' @param m A fitted model object.
#' @return Tibble of tidy coefficients with a `stars` column from [gtools::stars.pval()].
#' @keywords internal
tidier_did = function(m) {
  m |>
    broom::tidy() |>
    rename(se = "std.error", p_value = "p.value") |>
    mutate(stars = gtools::stars.pval(.data$p_value))
}

#' Goodness-of-fit summary for a fitted DiD model
#'
#' Combines [broom::glance()] output with simple scale and range diagnostics.
#' When `m$did_meta$inverse_fn` is not [identity], also reports **`rmse_native`**,
#' **`mae_native`**, etc., using the same inverse as [get_simeffects_did()] (native
#' outcome units, e.g. µg/m³ for `sqrt(aq_daily_mean)`).
#'
#' @param m A fitted model object.
#' @param treated_col Name of treated column in model frame.
#' @return Single-row tibble.
#' @keywords internal
get_gof_did = function(m, treated_col = "did_treated") {
  gof = m |>
    broom::glance() |>
    rename(rsq = "r.squared", p_value = "p.value") |>
    select(rsq, sigma, statistic, p_value, df, nobs)

  y_obs = m$model[[1]]
  treated_values = m$model[[treated_col]]

  ymin = min(y_obs, na.rm = TRUE)
  ymax = max(y_obs, na.rm = TRUE)
  rng = ymax - ymin
  rmse = sqrt(mean((y_obs - m$fitted.values)^2))
  mae = mean(abs(y_obs - m$fitted.values))

  inverse_fn = identity
  if (!is.null(m$did_meta) && !is.null(m$did_meta$inverse_fn)) {
    inverse_fn = m$did_meta$inverse_fn
  }

  rmse_native = NA_real_
  mae_native = NA_real_
  ymin_native = NA_real_
  ymax_native = NA_real_
  range_native = NA_real_
  maevsrange_native = NA_real_
  if (!identical(inverse_fn, identity)) {
    y_obs_n = inverse_fn(y_obs)
    yhat_n = inverse_fn(m$fitted.values)
    rmse_native = sqrt(mean((y_obs_n - yhat_n)^2, na.rm = TRUE))
    mae_native = mean(abs(y_obs_n - yhat_n), na.rm = TRUE)
    ymin_native = min(y_obs_n, na.rm = TRUE)
    ymax_native = max(y_obs_n, na.rm = TRUE)
    range_native = ymax_native - ymin_native
    if (is.finite(range_native) && range_native > 0) {
      maevsrange_native = mae_native / range_native
    }
  }

  extra = tibble(
    ymin = ymin,
    ymax = ymax,
    range = rng,
    rmse = rmse,
    mae = mae,
    maevsrange = if (is.finite(rng) && rng > 0) mae / rng else NA_real_,
    rmse_native = rmse_native,
    mae_native = mae_native,
    ymin_native = ymin_native,
    ymax_native = ymax_native,
    range_native = range_native,
    maevsrange_native = maevsrange_native,
    tr = sum(treated_values == 1, na.rm = TRUE),
    ct = sum(treated_values == 0, na.rm = TRUE)
  )

  bind_cols(gof, extra)
}

#' Fit a daily-level DiD model with date and monitor fixed effects.
#'
#' **Model stage:** `lm` is fit on the **transformed** response when `outcome`
#' uses a function of a column (e.g. `sqrt(aq_daily_mean)`). `predict(...,
#' se.fit = TRUE)` standard errors are on that **transformed** scale.
#'
#' @param data Panel data containing outcome, treatment, time, unit, and any
#'   covariate columns referenced by `covariates`.
#' @param outcome One-sided formula for the response, e.g. `~ aq_daily_mean`,
#'   `~ sqrt(aq_daily_mean)`, `~ log(aq_daily_mean)`. Defaults to
#'   [default_outcome_aq()] (`~ sqrt(aq_daily_mean)`); pass `~ aq_daily_mean` for identity.
#' @param covariates Optional one-sided formula of extra RHS terms from `data`,
#'   e.g. `~ poly(temp_daily_mean, 2)`.
#' @param response_inverse Optional `function(z)` mapping model-scale `z` to
#'   native outcome scale. If `NULL`, inverse is inferred for bare symbol,
#'   `sqrt`, `log`, `log10`, `log1p`.
#' @param treated_col Binary or logical treatment indicator.
#' @param days_since_col Days since treatment start (NA coerced to 0).
#' @param date_col Date column.
#' @param unit_col Unit/monitor column.
#' @param metro_col Optional; stored in `did_meta` for downstream grouping.
#' @return Fitted `lm` object with `did_meta` including `response_lhs`,
#'   `inverse_fn`, and `outcome_vars`.
#' @seealso [get_yhat_did()], [get_simeffects_did()], [get_qis_did()] for the pipeline
#'   after fitting.
get_model_did = function(
    data,
    outcome = default_outcome_aq(),
    covariates = NULL,
    response_inverse = NULL,
    treated_col = "treated",
    days_since_col = "days_since_treatment",
    date_col = "date",
    unit_col = "fullaqsid",
    metro_col = NULL
) {
  lhs_expr = extract_response_lhs_did(outcome)
  outcome_vars = all.vars(lhs_expr)
  covariate_vars = if (is.null(covariates)) {
    character(0)
  } else {
    all.vars(covariates)
  }

  assert_required_columns_did(
    data = data,
    required = unique(c(
      outcome_vars,
      covariate_vars,
      treated_col,
      days_since_col,
      date_col,
      unit_col
    )),
    fn_name = "get_model_did"
  )

  model_data = data |>
    mutate(
      did_treated = as.integer(.data[[treated_col]]),
      did_days_since = coalesce(as.numeric(.data[[days_since_col]]), 0),
      did_date_fe = factor(.data[[date_col]], levels = sort(unique(.data[[date_col]]))),
      did_unit_fe = factor(.data[[unit_col]], levels = sort(unique(.data[[unit_col]])))
    )

  formula = build_lm_formula_did(lhs_expr, covariates)

  model = stats::lm(formula = formula, data = model_data)
  model$xlevels$did_date_fe = levels(model_data$did_date_fe)
  model$xlevels$did_unit_fe = levels(model_data$did_unit_fe)

  inverse_fn = infer_inverse_fn_did(lhs_expr, response_inverse)

  model$did_meta = list(
    outcome_col = if (length(outcome_vars) == 1L) outcome_vars[[1]] else NA_character_,
    response_lhs = lhs_expr,
    inverse_fn = inverse_fn,
    outcome_vars = outcome_vars,
    covariate_vars = covariate_vars,
    treated_col = treated_col,
    days_since_col = days_since_col,
    date_col = date_col,
    unit_col = unit_col,
    metro_col = metro_col,
    date_levels = levels(model_data$did_date_fe),
    unit_levels = levels(model_data$did_unit_fe)
  )
  model
}

#' Generate fitted and counterfactual predictions from a DiD model.
#'
#' **yhat stage:** `yhat0`/`yhat1`/`se0`/`se1` are on the **model (transformed)
#' scale**. For factual rows where the transformed response can be evaluated
#' from observed columns, `yhat1` is replaced by that value and `se1` is set
#' to 0 (NYC-style `useobs` path).
#'
#' @param m A fitted model from get_model_did.
#' @param data Input panel data (must include columns for covariates if used).
#' @param treated_col Name of treatment indicator column (overridden by
#'   `m$did_meta$treated_col` when present).
#' @param days_since_col Name of days-since-treatment column (overridden by
#'   `m$did_meta$days_since_col` when present).
#' @param outcome_col Legacy: bare outcome column for factual imputation when
#'   `m$did_meta$response_lhs` is missing (older fits). Otherwise ignored.
#' @return Input data with yhat0, se0, yhat1, se1, and id columns.
get_yhat_did = function(
    m,
    data,
    treated_col = NULL,
    days_since_col = NULL,
    outcome_col = NULL
) {
  meta = m$did_meta
  if (is.null(meta)) {
    stop("get_yhat_did() requires m$did_meta from get_model_did()", call. = FALSE)
  }

  if (!is.null(meta$treated_col)) {
    treated_col = meta$treated_col
  }
  if (is.null(treated_col)) {
    treated_col = "treated"
  }
  if (!is.null(meta$days_since_col)) {
    days_since_col = meta$days_since_col
  }
  if (is.null(days_since_col)) {
    days_since_col = "days_since_treatment"
  }

  req_base = c(treated_col, days_since_col)
  assert_required_columns_did(data, req_base, "get_yhat_did")

  data_model_terms = data |>
    mutate(
      did_treated = as.integer(.data[[treated_col]]),
      did_days_since = coalesce(as.numeric(.data[[days_since_col]]), 0)
    )

  if (!is.null(meta)) {
    data_model_terms = data_model_terms |>
      mutate(
        did_date_fe = factor(
          .data[[meta$date_col]],
          levels = meta$date_levels
        ),
        did_unit_fe = factor(
          .data[[meta$unit_col]],
          levels = meta$unit_levels
        )
      )
  }

  yhat0 = predict_with_se_did(
    m = m,
    newdata = data_model_terms |>
      mutate(did_treated = 0)
  ) |>
    select(yhat0 = fit, se0 = se.fit)

  yhat1 = predict_with_se_did(
    m = m,
    newdata = data_model_terms
  ) |>
    select(yhat1 = fit, se1 = se.fit)

  grid = bind_cols(data, yhat0, yhat1) |>
    mutate(id = row_number())

  if (!is.null(meta$response_lhs)) {
    assert_required_columns_did(
      grid,
      meta$outcome_vars,
      "get_yhat_did"
    )
    complete_obs = stats::complete.cases(grid[meta$outcome_vars])
    obs_trans = as.numeric(
      as.vector(eval(meta$response_lhs, envir = grid, enclos = baseenv()))
    )
    obs_available = complete_obs & !is.na(obs_trans)
  } else {
    oc = if (is.null(outcome_col)) "aq_daily_mean" else outcome_col
    assert_required_columns_did(grid, oc, "get_yhat_did")
    obs_trans = grid[[oc]]
    obs_available = !is.na(obs_trans)
  }

  grid = grid |>
    mutate(
      yhat1 = if_else(obs_available, obs_trans, .data$yhat1),
      se1 = if_else(obs_available, 0, .data$se1)
    )

  grid
}

#' Simulate treatment effects with Monte Carlo uncertainty propagation.
#'
#' **Simulation stages (contract):**
#' (1) `yhat1`/`yhat0`/`se1`/`se0` from `grid` are on the **transformed**
#' model scale. (2) For each `id`, draw `ysim1_t`, `ysim0_t` ~ Normal on that
#' scale. (3) Apply `m$did_meta$inverse_fn` to each draw to obtain **native**
#' scale `ysim1`, `ysim0`. (4) Summaries (`diff`, `sediff`, means, SDs) use
#' **native**-scale draws only. `get_qis_did` / `get_long_effects_did` consume
#' these native-scale summaries.
#'
#' @param grid Output from get_yhat_did (predictions on transformed scale).
#' @param m Fitted model from `get_model_did`; supplies `inverse_fn`. If
#'   `NULL`, inverse defaults to `identity` (identity response).
#' @param start Optional start date filter.
#' @param end Optional end date filter.
#' @param n Number of Monte Carlo draws per id.
#' @param date_col Date column name.
#' @param treated_col Treated column name.
#' @return Row-level simulated effects with `diff` and `sediff` on **native** scale.
#' @seealso [get_att_did()], [get_qis_did()] for aggregation.
get_simeffects_did = function(
    grid,
    m = NULL,
    start = NULL,
    end = NULL,
    n = 10000,
    date_col = "date",
    treated_col = "treated"
) {
  inverse_fn = identity
  if (!is.null(m) && !is.null(m$did_meta) && !is.null(m$did_meta$inverse_fn)) {
    inverse_fn = m$did_meta$inverse_fn
  }

  assert_required_columns_did(
    data = grid,
    required = c("id", date_col, treated_col, "yhat0", "se0", "yhat1", "se1"),
    fn_name = "get_simeffects_did"
  )

  date_vals = as.Date(grid[[date_col]])
  start_date = if (is.null(start)) min(date_vals, na.rm = TRUE) else as.Date(start)
  end_date = if (is.null(end)) max(date_vals, na.rm = TRUE) else as.Date(end)

  grid_valid = grid |>
    mutate(date_filter = as.Date(.data[[date_col]])) |>
    filter(.data$date_filter >= start_date, .data$date_filter <= end_date) |>
    filter(
      is.finite(.data$yhat0), is.finite(.data$yhat1),
      is.finite(.data$se0), is.finite(.data$se1)
    ) |>
    select(-date_filter)

  if (nrow(grid_valid) == 0) {
    return(
      tibble(
        id = integer(),
        sediff = double(),
        diff = double(),
        yhat1 = double(),
        yhat0 = double(),
        se1 = double(),
        se0 = double()
      ) |>
        left_join(
          grid |>
            select(any_of(c("id", date_col, treated_col, "fullaqsid", "metro_id"))),
          by = "id"
        )
    )
  }

  # Draw on transformed scale; inverse each draw; summarize on native scale.
  grid_valid |>
    group_by(id) |>
    reframe(
      ysim1_t = stats::rnorm(n = n, mean = .data$yhat1, sd = pmax(.data$se1, 0)),
      ysim0_t = stats::rnorm(n = n, mean = .data$yhat0, sd = pmax(.data$se0, 0))
    ) |>
    mutate(
      ysim1 = inverse_fn(.data$ysim1_t),
      ysim0 = inverse_fn(.data$ysim0_t),
      diff = .data$ysim1 - .data$ysim0
    ) |>
    group_by(id) |>
    summarize(
      sediff = sd(.data$diff),
      diff = mean(.data$diff),
      yhat1 = mean(.data$ysim1),
      yhat0 = mean(.data$ysim0),
      se1 = sd(.data$ysim1),
      se0 = sd(.data$ysim0),
      .groups = "drop"
    ) |>
    left_join(
      grid |>
        select(any_of(c("id", date_col, treated_col, "fullaqsid", "metro_id"))),
      by = "id"
    )
}

#' Compute ATT summary from row-level effects.
#' @param effects Output from get_simeffects_did, optionally grouped.
#' @return ATT summary with weighted means and inferential statistics.
get_att_did = function(effects) {
  assert_required_columns_did(
    data = effects,
    required = c("diff", "sediff", "yhat1", "se1", "yhat0", "se0"),
    fn_name = "get_att_did"
  )

  if (dplyr::is_grouped_df(effects)) {
    return(effects |> group_modify(~summarize_att_core_did(.x)))
  }

  summarize_att_core_did(effects)
}

#' Produce ATT estimates at overall and grouped levels.
#'
#' **Aggregation stage:** `get_qis_did` builds on `get_simeffects_did`, which
#' already reports means/SDs on the **native** outcome scale after inverse
#' transform of Monte Carlo draws. ATT / SE logic is therefore in native units.
#'
#' @param m A fitted DiD model.
#' @param data Input panel data.
#' @param start Optional start date for effect window.
#' @param end Optional end date for effect window.
#' @param n Number of Monte Carlo draws.
#' @param date_col Date column.
#' @param unit_col Unit column.
#' @param metro_col Metro ID column.
#' @param aggregates Which aggregates to return:
#'   overall, per_metro, per_metro_month, per_month, per_monitor, per_week,
#'   per_week_metro, per_day_metro, per_day_metro_unit.
#' @param outcome_col Legacy: passed to `get_yhat_did` only when `m$did_meta`
#'   lacks `response_lhs` (older fits).
#' @return Long tibble with `type` and grouped ATT summaries.
#' @seealso [compare_att_two_dates_did()], [compare_att_many_dates_did()] for
#'   comparisons across calendar days.
get_qis_did = function(
    m,
    data,
    start = NULL,
    end = NULL,
    n = 10000,
    date_col = "date",
    unit_col = "fullaqsid",
    metro_col = "metro_id",
    aggregates = c("per_metro", "per_metro_month", "overall"),
    outcome_col = NULL
) {
  valid_aggregates = c(
    "overall", "per_metro", "per_metro_month", "per_month",
    "per_monitor", "per_week", "per_week_metro", "per_day_metro",
    "per_day_metro_unit"
  )

  if (!all(aggregates %in% valid_aggregates)) {
    stop(
      paste0(
        "get_qis_did() invalid aggregates: ",
        paste(setdiff(aggregates, valid_aggregates), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  grid = get_yhat_did(
    m = m,
    data = data,
    outcome_col = outcome_col
  )
  sims = get_simeffects_did(
    grid = grid,
    m = m,
    start = start,
    end = end,
    n = n,
    date_col = date_col
  )

  stats = list()

  if ("overall" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          get_att_did() |>
          mutate(type = "overall")
      )
    )
  }

  if ("per_metro" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          group_by(.data[[metro_col]]) |>
          get_att_did() |>
          mutate(type = "per_metro")
      )
    )
  }

  if ("per_metro_month" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(month = lubridate::floor_date(.data[[date_col]], unit = "month")) |>
          group_by(.data[[metro_col]], month) |>
          get_att_did() |>
          mutate(type = "per_metro_month")
      )
    )
  }

  if ("per_month" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(month = lubridate::floor_date(.data[[date_col]], unit = "month")) |>
          group_by(month) |>
          get_att_did() |>
          mutate(type = "per_month")
      )
    )
  }

  if ("per_monitor" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          group_by(.data[[metro_col]], .data[[unit_col]]) |>
          get_att_did() |>
          mutate(type = "per_monitor")
      )
    )
  }

  if ("per_week" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(week = lubridate::floor_date(.data[[date_col]], unit = "week")) |>
          group_by(week) |>
          get_att_did() |>
          mutate(type = "per_week")
      )
    )
  }

  if ("per_week_metro" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(week = lubridate::floor_date(.data[[date_col]], unit = "week")) |>
          group_by(.data[[metro_col]], week) |>
          get_att_did() |>
          mutate(type = "per_week_metro")
      )
    )
  }

  if ("per_day_metro" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(day = as.Date(.data[[date_col]])) |>
          group_by(.data[[metro_col]], day) |>
          get_att_did() |>
          mutate(type = "per_day_metro")
      )
    )
  }

  if ("per_day_metro_unit" %in% aggregates) {
    stats = append(
      stats,
      list(
        sims |>
          mutate(day = as.Date(.data[[date_col]])) |>
          group_by(.data[[metro_col]], .data[[unit_col]], day) |>
          get_att_did() |>
          mutate(type = "per_day_metro_unit")
      )
    )
  }

  output = bind_rows(stats) %>% ungroup()
  return(output)
}

#' Compare aggregated ATTs between two calendar dates
#'
#' Takes rows from [get_qis_did()] (e.g. `type == "per_day_metro"` or
#' `"per_day_metro_unit"`) and estimates the distribution of
#' `ATT(date2) - ATT(date1)` per `id_cols` group.
#'
#' **Uncertainty:** Each date's `att` and `se_att` already summarize upstream
#' simulation; `se_att` is wider when fewer micro-rows enter that aggregate.
#' Under **independence** of the two daily estimates, the contrast variance is
#' `se_att_1^2 + se_att_2^2` (sum of variances for independent mean estimators).
#' The `df` column from [summarize_att_core_did()] is for the within-date t-test
#' only; when both per-date `df` columns are present, the analytic p-value uses
#' **Welch--Satterthwaite** degrees of freedom for the two-sample
#' Behrens--Fisher-style contrast (Welch 1947; see `functions_did_references.bib`).
#' The Monte Carlo path draws `ATT_d \sim N(\texttt{att}_d, \texttt{se\_att}_d^2)`
#' independently per date and forms the contrast; the empirical two-sided
#' p-value is a **simple Monte Carlo test** (Davison and Hinkley 1997, Ch. 4;
#' bib key `davison1997bootstrap`).
#'
#' @param att_df Tibble from `get_qis_did()` (optionally pre-filtered).
#' @param date1,date2 Dates to compare (`Date` or coercible).
#' @param id_cols Character vector of key columns to keep (e.g. `metro_id` or
#'   `c("metro_id", "fullaqsid")`). All must exist in `att_df`.
#' @param day_col Name of the calendar column (`day` for `per_day_*` types).
#' @param n_draws Monte Carlo draws per group when `method = "mc"`.
#' @param type If non-`NULL` and `att_df` has a `type` column, filter to this
#'   value.
#' @param method `"mc"` matches the workflow: draw `ATT ~ N(att, se_att^2)` per
#'   date then differ; `"analytic"` uses `delta = att2 - att1` and
#'   `sqrt(se1^2 + se2^2)`. Analytic p-values use a Welch-style degrees of
#'   freedom when both per-date `df` columns are available, otherwise a
#'   normal approximation. Monte Carlo p-values are empirical two-sided tail
#'   probabilities from the simulated contrast draws.
#' @param warn_incomplete If `TRUE`, warn when groups lack both dates.
#' @return Tibble with `id_cols`, `delta`, `delta_se`, `p_value`, `method`,
#'   `n_draws` (`NA` for analytic). Optional `n_effects_date1`,
#'   `n_effects_date2`, `df_date1`, `df_date2` when those columns were present
#'   in `att_df`.
#' @references
#' Welch B. L. (1947). The generalization of "Student's" problem when several
#' different population variances are involved. \emph{Biometrika} 34(1/2), 28--35.
#' \doi{10.1093/biomet/34.1-2.28}
#'
#' Davison A. C., Hinkley D. V. (1997). \emph{Bootstrap Methods and Their Application.}
#' Cambridge University Press. (Monte Carlo / resampling tests.)
#'
#' @seealso [compare_att_many_dates_did()] for three or more days.
compare_att_two_dates_did = function(
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
  method = match.arg(method)
  date1 = as.Date(date1)
  date2 = as.Date(date2)
  if (date1 == date2) {
    stop("compare_att_two_dates_did(): date1 and date2 must differ", call. = FALSE)
  }

  assert_required_columns_did(
    att_df,
    c(id_cols, day_col, "att", "se_att"),
    "compare_att_two_dates_did"
  )

  d = dplyr::as_tibble(att_df)
  if (!is.null(type) && "type" %in% names(d)) {
    d = dplyr::filter(d, .data$type == type)
  }

  if (nrow(d) == 0) {
    stop("compare_att_two_dates_did(): no rows left after optional type filter", call. = FALSE)
  }

  d[[day_col]] = as.Date(d[[day_col]])

  d = d |>
    dplyr::filter(.data[[day_col]] %in% c(date1, date2)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(id_cols, day_col))), .keep_all = TRUE)

  n_group_total = d |>
    dplyr::distinct(dplyr::across(dplyr::all_of(id_cols))) |>
    nrow()

  ok = d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarize(
      both = all(c(date1, date2) %in% .data[[day_col]]),
      .groups = "drop"
    )

  n_drop = sum(!ok$both)
  if (warn_incomplete && n_drop > 0) {
    warning(
      "compare_att_two_dates_did(): dropped ", n_drop,
      " group(s) missing one of the two dates (kept ",
      sum(ok$both), " of ", n_group_total, " distinct groups in the two-date window).",
      call. = FALSE
    )
  }

  d = d |>
    dplyr::semi_join(dplyr::filter(ok, .data$both), by = id_cols)

  if (nrow(d) == 0) {
    stop(
      "compare_att_two_dates_did(): no groups with both dates; check id_cols and day_col",
      call. = FALSE
    )
  }

  d = d |>
    dplyr::select(
      dplyr::all_of(id_cols),
      dplyr::all_of(day_col),
      "att",
      "se_att",
      dplyr::any_of(c("n_effects", "df"))
    )

  values_from = c("att", "se_att")
  if ("n_effects" %in% names(d)) {
    values_from = c(values_from, "n_effects")
  }
  if ("df" %in% names(d)) {
    values_from = c(values_from, "df")
  }

  wide = d |>
    dplyr::mutate(
      scenario = factor(
        .data[[day_col]],
        levels = c(date1, date2),
        labels = c("date1", "date2")
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from = scenario,
      values_from = dplyr::all_of(values_from)
    )

  if (method == "analytic") {
    analytic = wide |>
      dplyr::mutate(
        delta = .data$att_date2 - .data$att_date1,
        delta_se = sqrt(pmax(.data$se_att_date1, 0)^2 + pmax(.data$se_att_date2, 0)^2),
        test_stat = dplyr::if_else(.data$delta_se > 0, .data$delta / .data$delta_se, NA_real_)
      )

    if (all(c("df_date1", "df_date2") %in% names(analytic))) {
      analytic = analytic |>
        dplyr::mutate(
          df_welch = dplyr::if_else(
            is.finite(.data$df_date1) && is.finite(.data$df_date2) &&
              .data$df_date1 > 0 && .data$df_date2 > 0 &&
              .data$delta_se > 0,
            (.data$se_att_date1^2 + .data$se_att_date2^2)^2 /
              (
                ((.data$se_att_date1^2)^2 / .data$df_date1) +
                  ((.data$se_att_date2^2)^2 / .data$df_date2)
              ),
            NA_real_
          ),
          p_value = dplyr::case_when(
            !is.finite(.data$test_stat) ~ NA_real_,
            is.finite(.data$df_welch) ~ 2 * (1 - stats::pt(q = abs(.data$test_stat), df = .data$df_welch)),
            TRUE ~ 2 * (1 - stats::pnorm(q = abs(.data$test_stat)))
          )
        )
    } else {
      analytic = analytic |>
        dplyr::mutate(
          p_value = dplyr::if_else(
            is.finite(.data$test_stat),
            2 * (1 - stats::pnorm(q = abs(.data$test_stat))),
            NA_real_
          )
        )
    }

    return(
      analytic |>
        dplyr::mutate(method = "analytic", n_draws = NA_integer_) |>
        dplyr::select(
          dplyr::all_of(id_cols),
          "delta",
          "delta_se",
          "p_value",
          "method",
          "n_draws",
          dplyr::any_of(
            c(
              "n_effects_date1", "n_effects_date2",
              "df_date1", "df_date2"
            )
          )
        )
    )
  }

  wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::reframe(
      diff = stats::rnorm(n_draws, mean = .data$att_date2, sd = pmax(.data$se_att_date2, 0)) -
        stats::rnorm(n_draws, mean = .data$att_date1, sd = pmax(.data$se_att_date1, 0))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarize(
      delta = mean(.data$diff, na.rm = TRUE),
      delta_se = stats::sd(.data$diff, na.rm = TRUE),
      p_value = 2 * pmin(
        (sum(.data$diff <= 0, na.rm = TRUE) + 1) / (sum(is.finite(.data$diff)) + 1),
        (sum(.data$diff >= 0, na.rm = TRUE) + 1) / (sum(is.finite(.data$diff)) + 1)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(method = "mc", n_draws = as.integer(n_draws))
}

#' Weighted least squares for a line through uncertain daily ATTs
#'
#' Fits \eqn{y_j = \beta_0 + \beta_1 x_j} with weights \eqn{w_j = 1/\mathrm{se}_j^2}.
#' This is the **Aitken** / GLS solution when \eqn{\mathrm{Var}(y) = \mathrm{diag}(\mathrm{se}^2)}
#' is known (Aitken 1935; see `functions_did_references.bib`).
#'
#' The returned covariance is the standard
#' \eqn{\mathrm{Var}(\hat\beta) = (X'WX)^{-1} X' W \Sigma W X (X'WX)^{-1}}
#' with \eqn{W = \mathrm{diag}(w)} and \eqn{\Sigma = \mathrm{diag}(\mathrm{se}^2)}.
#'
#' @param y Numeric vector of responses (e.g. ATT point estimates per day).
#' @param se Numeric vector of standard errors for \eqn{y_j} (homoskedastic on the
#'   linear scale in the sense of known diagonal \eqn{\Sigma}).
#' @param x Numeric covariate (e.g. days since first date).
#' @return list with `beta` (length 2), `var_beta` (2x2 matrix), and `x`.
#' @references
#' Aitken A. C. (1935). On least squares and linear combination of observations.
#' \emph{Proceedings of the Royal Society of Edinburgh} 55, 42--48.
#' \doi{10.1017/S0370164600014346}
#' @keywords internal
wls_intercept_slope_var_did = function(y, se, x) {
  J = length(y)
  if (length(se) != J || length(x) != J) {
    stop("wls_intercept_slope_var_did(): y, se, x must have equal length", call. = FALSE)
  }
  w = 1 / pmax(se^2, .Machine$double.eps)
  X = cbind(1, x)
  XtW = crossprod(X, w * X)
  XtWy = crossprod(X, w * y)
  beta = as.numeric(solve(XtW, XtWy))
  XtW_inv = solve(XtW)
  # Var(y) = diag(se^2); Var(beta) = (X'WX)^{-1} X' W Var(y) W X (X'WX)^{-1}
  D = w^2 * se^2
  Z = sweep(X, 1, sqrt(D), `*`)
  mid = crossprod(Z)
  var_beta = XtW_inv %*% mid %*% XtW_inv
  list(beta = beta, var_beta = var_beta, x = x)
}

#' Wald chi-square for equality of J independent means (known variances)
#'
#' Tests \eqn{H_0: \mu_1 = \cdots = \mu_J} with \eqn{\hat\mu_j} independent and
#' \eqn{\hat\mu_j \sim N(\mu_j, \mathrm{se}_j^2)} approximately. Uses the
#' \eqn{(J-1)}-vector of contrasts \eqn{\mu_j - \mu_J} with covariance
#' \eqn{C \Sigma C'} where \eqn{C} rows are \eqn{(e_j - e_J)'} and
#' \eqn{\Sigma = \mathrm{diag}(\mathrm{se}^2)}. The statistic
#' \eqn{Q = (C\hat\mu)' (C \Sigma C')^{-1} (C\hat\mu)} is asymptotically
#' \eqn{\chi^2_{J-1}} under \eqn{H_0} (Wald 1943; standard linear hypothesis
#' theory; Casella and Berger 2002, Ch. 10); see `functions_did_references.bib`.
#'
#' If \eqn{C \Sigma C'} is singular, a generalized inverse \eqn{(C \Sigma C')^{-}}
#' is used (via [MASS::ginv()]).
#'
#' @param att Numeric vector of length \eqn{J} (point estimates).
#' @param se Numeric vector of length \eqn{J} (standard errors).
#' @return list with `statistic` (scalar \eqn{Q}) and `df` (\eqn{J-1}).
#' @references
#' Wald A. (1943). Tests of statistical hypotheses concerning several parameters
#' when the number of observations is large. \emph{Transactions of the American Mathematical Society}
#' 54(3), 426--482. \doi{10.1090/S0002-9947-1943-0010241-8}
#'
#' Casella G., Berger R. L. (2002). \emph{Statistical Inference}, 2nd ed. Duxbury.
#' @keywords internal
wald_equal_means_chisq_did = function(att, se) {
  J = length(att)
  if (length(se) != J) {
    stop("wald_equal_means_chisq_did(): att and se must have equal length", call. = FALSE)
  }
  if (J < 2) {
    stop("wald_equal_means_chisq_did(): need at least two days", call. = FALSE)
  }
  se = pmax(se, sqrt(.Machine$double.eps))
  Sigma = diag(se^2, nrow = J, ncol = J)
  C = matrix(0, nrow = J - 1L, ncol = J)
  for (j in seq_len(J - 1L)) {
    C[j, j] = 1
    C[j, J] = -1
  }
  ca = as.numeric(C %*% att)
  M = C %*% Sigma %*% t(C)
  q = tryCatch(
    as.numeric(t(ca) %*% solve(M, ca)),
    error = function(e) NA_real_
  )
  if (!is.finite(q)) {
    q = tryCatch(
      as.numeric(t(ca) %*% MASS::ginv(M) %*% ca),
      error = function(e) NA_real_
    )
  }
  list(statistic = q, df = J - 1L)
}

#' Compare daily aggregated ATTs across two or more calendar days
#'
#' Companion to [compare_att_two_dates_did()] with three estimands. Statistical
#' ideas and BibTeX keys in `functions_did_references.bib` are cross-referenced
#' below.
#'
#' **Why these are valid (brief):**
#' \describe{
#'   \item{Reference contrasts}{Each contrast \eqn{\Delta = \mathrm{ATT}(t) -
#'     \mathrm{ATT}(t_\mathrm{ref})} is a linear combination of independent
#'     summaries; under independence of the daily inputs, \eqn{\mathrm{Var}(\Delta) =
#'     \mathrm{se}_t^2 + \mathrm{se}_\mathrm{ref}^2}. The analytic p-value is a
#'     two-sided normal tail test; MC uses the same simulation-as-test principle
#'     as [compare_att_two_dates_did()] (Davison and Hinkley 1997).}
#'   \item{Slope (WLS)}{Treating daily \eqn{\mathrm{ATT}_j} as heteroskedastic
#'     Normal with known diagonal variance is the standard WLS/GLS setting
#'     (Aitken 1935). \eqn{\mathrm{Var}(\hat\beta)} follows from linear estimation
#'     with known \eqn{\Sigma} (Casella and Berger 2002). When `method = "mc"`,
#'     each replicate draws a full vector \eqn{\tilde y \sim N(\texttt{att},
#'     \mathrm{diag}(\texttt{se\_att}^2))}, refits WLS, and stores the diagonal
#'     of the **within-replicate** WLS covariance \eqn{W^{(b)}}. Rubin's
#'     **combining rule** for total variance,
#'     \eqn{T = \bar W + (1 + 1/m) B}, where \eqn{\bar W} is the mean of
#'     \eqn{W^{(b)}} across \eqn{m} replicates and \eqn{B} is the between-replicate
#'     variance of \eqn{\hat\beta^{(b)}}, is the standard multiple-imputation
#'     pooling formula (Rubin 1987; Little and Rubin 2002, Sec. 10.2). It
#'     separates **within-replicate** (WLS, given a draw) and **between-replicate**
#'     (simulation of the latent daily ATTs) components. For scalar coefficients,
#'     this aligns with the **law of total variance** when the two sources are
#'     interpreted analogously to imputation uncertainty (Casella and Berger 2002,
#'     Ch. 4). If \eqn{\bar W \approx B} asymptotically for linear \eqn{\hat\beta},
#'     Rubin's \eqn{T} can be conservative; use `variance_combine = "analytic"` or
#'     `"mc"` to compare.}
#'   \item{Omnibus}{The Wald statistic for \eqn{H_0: \mu_1 = \cdots = \mu_J} with
#'     known heteroskedasticity is a quadratic form in \eqn{(J-1)} contrasts
#'     (Wald 1943). The implementation contrasts each day to the **last** sorted
#'     date in `dates`. Analytic p-values use \eqn{\chi^2_{J-1}}; `method = "mc"`
#'     uses a Monte Carlo p-value by comparing the observed statistic to draws
#'     from the same quadratic form under \eqn{N(\texttt{att}, \mathrm{diag}(\texttt{se\_att}^2))}.}
#' }
#'
#' **Limitation:** Daily rows are treated as **independent**; joint uncertainty
#' across days from the same underlying simulation draws is not modeled (see
#' pipeline notes in [compare_att_two_dates_did()]).
#'
#' @param att_df Tibble from [get_qis_did()] (optionally pre-filtered).
#' @param dates Vector of calendar dates (`Date` or coercible); length \eqn{\geq 2}.
#' @param id_cols Key columns (e.g. `metro_id` or `c("metro_id", "fullaqsid")`).
#' @param day_col Calendar column (`day` for `per_day_*` types).
#' @param estimand One of `"reference"`, `"slope"`, `"omnibus"`.
#' @param reference_date Required when `estimand == "reference"`; must be one of `dates`.
#' @param time Optional numeric vector of length `length(dates)` for the slope
#'   covariate (e.g. custom spacing). If `NULL`, uses days since `min(dates)`.
#' @param type If non-`NULL` and `att_df` has a `type` column, filter to this value.
#' @param method `"mc"` or `"analytic"`.
#' @param n_draws Monte Carlo replicates when `method = "mc"`.
#' @param variance_combine For `estimand == "slope"` only: `"rubin"` (default),
#'   `"analytic"`, or `"mc"`. `"rubin"` uses \eqn{\bar W + (1+1/m) B} (Rubin 1987).
#'   `"analytic"` uses only the plug-in WLS variance at \eqn{(\texttt{att}, \texttt{se\_att})}.
#'   `"mc"` uses only the sample SD of \eqn{\hat\beta^{(b)}} across draws.
#' @param warn_incomplete If `TRUE`, warn when groups lack all `dates`.
#' @return For `"reference"`, a long tibble with one row per group and
#'   non-reference day. For `"slope"`, a long tibble with rows `intercept` and
#'   `slope`. For `"omnibus"`, one row per group with `statistic`, `df`, `p_value`.
#' @references
#' Aitken A. C. (1935). On least squares and linear combination of observations.
#' \emph{Proceedings of the Royal Society of Edinburgh} 55, 42--48.
#' \doi{10.1017/S0370164600014346}
#'
#' Wald A. (1943). Tests of statistical hypotheses concerning several parameters
#' when the number of observations is large. \emph{Transactions of the American Mathematical Society}
#' 54(3), 426--482. \doi{10.1090/S0002-9947-1943-0010241-8}
#'
#' Rubin D. B. (1987). \emph{Multiple Imputation for Nonresponse in Surveys.}
#' John Wiley & Sons. (Combining rules \eqn{T = \bar W + (1+1/m) B}.)
#'
#' Little R. J. A., Rubin D. B. (2002). \emph{Statistical Analysis with Missing Data},
#' 2nd ed. Wiley. (Pooling rules and theory.)
#'
#' Davison A. C., Hinkley D. V. (1997). \emph{Bootstrap Methods and Their Application.}
#' Cambridge University Press.
#'
#' Casella G., Berger R. L. (2002). \emph{Statistical Inference}, 2nd ed. Duxbury.
#' (Law of total variance; linear models.)
#'
#' @seealso [compare_att_two_dates_did()], [wald_equal_means_chisq_did()],
#'   [wls_intercept_slope_var_did()].
compare_att_many_dates_did = function(
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
  estimand = match.arg(estimand)
  method = match.arg(method)
  variance_combine = match.arg(variance_combine)

  assert_required_columns_did(
    att_df,
    c(id_cols, day_col, "att", "se_att"),
    "compare_att_many_dates_did"
  )

  dates = sort(unique(as.Date(dates)))
  if (length(dates) < 2L) {
    stop("compare_att_many_dates_did(): need at least two distinct dates", call. = FALSE)
  }

  if (estimand == "reference") {
    if (is.null(reference_date)) {
      stop("compare_att_many_dates_did(): reference_date is required when estimand = \"reference\"", call. = FALSE)
    }
    reference_date = as.Date(reference_date)
    if (!reference_date %in% dates) {
      stop("compare_att_many_dates_did(): reference_date must be in dates", call. = FALSE)
    }
  }

  d = dplyr::as_tibble(att_df)
  if (!is.null(type) && "type" %in% names(d)) {
    d = dplyr::filter(d, .data$type == type)
  }

  if (nrow(d) == 0) {
    stop("compare_att_many_dates_did(): no rows left after optional type filter", call. = FALSE)
  }

  d[[day_col]] = as.Date(d[[day_col]])

  d = d |>
    dplyr::filter(.data[[day_col]] %in% dates) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(id_cols, day_col))), .keep_all = TRUE)

  complete_args = stats::setNames(list(dates), day_col)
  d = d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    tidyr::complete(!!!complete_args) |>
    dplyr::ungroup()

  ok = d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarize(
      complete = !any(is.na(.data$att) | is.na(.data$se_att)),
      .groups = "drop"
    )

  n_group_total = nrow(ok)
  n_drop = sum(!ok$complete)
  if (warn_incomplete && n_drop > 0) {
    warning(
      "compare_att_many_dates_did(): dropped ", n_drop,
      " group(s) missing at least one date (kept ",
      sum(ok$complete), " of ", n_group_total, ").",
      call. = FALSE
    )
  }

  d = d |>
    dplyr::semi_join(dplyr::filter(ok, .data$complete), by = id_cols)

  if (nrow(d) == 0) {
    stop(
      "compare_att_many_dates_did(): no groups with all dates; check id_cols and day_col",
      call. = FALSE
    )
  }

  d = d |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(id_cols, day_col))))

  if (!is.null(time)) {
    if (length(time) != length(dates)) {
      stop("compare_att_many_dates_did(): time must have length equal to length(dates)", call. = FALSE)
    }
    time_by_day = stats::setNames(as.numeric(time), as.character(dates))
  } else {
    time_by_day = stats::setNames(
      as.numeric(dates - min(dates), units = "days"),
      as.character(dates)
    )
  }

  groups = d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::group_split(.keep = TRUE)

  if (estimand == "reference") {
    other_dates = dates[dates != reference_date]
    out = vector("list", length(groups))
    for (gi in seq_along(groups)) {
      g = groups[[gi]]
      att = g$att
      se = pmax(g$se_att, 0)
      days = g[[day_col]]
      i_ref = which(days == reference_date)
      att_r = att[i_ref]
      se_r = se[i_ref]
      rows = vector("list", length(other_dates))
      for (ti in seq_along(other_dates)) {
        td = other_dates[ti]
        i_t = which(days == td)
        att_t = att[i_t]
        se_t = se[i_t]
        delta = att_t - att_r
        delta_se = sqrt(se_t^2 + se_r^2)
        z = if (delta_se > 0) abs(delta / delta_se) else NA_real_
        if (method == "analytic") {
          p_val = if (is.finite(z)) 2 * (1 - stats::pnorm(z)) else NA_real_
        } else {
          diff = stats::rnorm(n_draws, mean = att_t, sd = se_t) -
            stats::rnorm(n_draws, mean = att_r, sd = se_r)
          p_val = 2 * pmin(
            (sum(diff <= 0, na.rm = TRUE) + 1) / (sum(is.finite(diff)) + 1),
            (sum(diff >= 0, na.rm = TRUE) + 1) / (sum(is.finite(diff)) + 1)
          )
        }
        key = dplyr::slice(g, 1L) |> dplyr::select(dplyr::all_of(id_cols))
        rows[[ti]] = dplyr::bind_cols(
          key,
          tibble::tibble(
            compare_date = td,
            reference_date = reference_date,
            delta = delta,
            delta_se = if (method == "analytic") delta_se else stats::sd(diff, na.rm = TRUE),
            p_value = p_val,
            method = method,
            n_draws = if (method == "analytic") NA_integer_ else as.integer(n_draws),
            estimand = "reference"
          )
        )
      }
      out[[gi]] = dplyr::bind_rows(rows)
    }
    return(dplyr::bind_rows(out))
  }

  if (estimand == "omnibus") {
    out = vector("list", length(groups))
    for (gi in seq_along(groups)) {
      g = groups[[gi]]
      att = g$att
      se = pmax(g$se_att, 0)
      wald = wald_equal_means_chisq_did(att, se)
      Q_obs = wald$statistic
      df = wald$df
      key = dplyr::slice(g, 1L) |> dplyr::select(dplyr::all_of(id_cols))
      if (method == "analytic") {
        p_val = if (is.finite(Q_obs) && is.finite(df) && df > 0) {
          stats::pchisq(Q_obs, df = df, lower.tail = FALSE)
        } else {
          NA_real_
        }
        out[[gi]] = dplyr::bind_cols(
          key,
          tibble::tibble(
            statistic = Q_obs,
            df = as.numeric(df),
            p_value = p_val,
            method = "analytic",
            n_draws = NA_integer_,
            estimand = "omnibus"
          )
        )
      } else {
        Q_sim = replicate(
          n_draws,
          {
            y = stats::rnorm(length(att), mean = att, sd = se)
            wald_equal_means_chisq_did(y, se)$statistic
          }
        )
        p_val = (sum(Q_sim >= Q_obs, na.rm = TRUE) + 1) / (sum(is.finite(Q_sim)) + 1)
        out[[gi]] = dplyr::bind_cols(
          key,
          tibble::tibble(
            statistic = Q_obs,
            df = as.numeric(df),
            p_value = p_val,
            method = "mc",
            n_draws = as.integer(n_draws),
            estimand = "omnibus"
          )
        )
      }
    }
    return(dplyr::bind_rows(out))
  }

  # estimand == "slope"
  out = vector("list", length(groups))
  for (gi in seq_along(groups)) {
    g = groups[[gi]]
    att = g$att
    se = pmax(g$se_att, 0)
    x = as.numeric(time_by_day[as.character(g[[day_col]])])
    fit_point = wls_intercept_slope_var_did(att, se, x)
    beta_hat = fit_point$beta
    var_analytic = diag(fit_point$var_beta)

    if (method == "analytic") {
      z0 = if (var_analytic[1] > 0) abs(beta_hat[1] / sqrt(var_analytic[1])) else NA_real_
      z1 = if (var_analytic[2] > 0) abs(beta_hat[2] / sqrt(var_analytic[2])) else NA_real_
      p0 = if (is.finite(z0)) 2 * (1 - stats::pnorm(z0)) else NA_real_
      p1 = if (is.finite(z1)) 2 * (1 - stats::pnorm(z1)) else NA_real_
      key = dplyr::slice(g, 1L) |> dplyr::select(dplyr::all_of(id_cols))
      out[[gi]] = dplyr::bind_rows(
        dplyr::bind_cols(
          key,
          tibble::tibble(
            term = "intercept",
            estimate = beta_hat[1],
            delta_se = sqrt(var_analytic[1]),
            delta_se_within = sqrt(var_analytic[1]),
            delta_se_between = NA_real_,
            p_value = p0,
            method = "analytic",
            n_draws = NA_integer_,
            variance_combine = NA_character_,
            estimand = "slope"
          )
        ),
        dplyr::bind_cols(
          key,
          tibble::tibble(
            term = "slope",
            estimate = beta_hat[2],
            delta_se = sqrt(var_analytic[2]),
            delta_se_within = sqrt(var_analytic[2]),
            delta_se_between = NA_real_,
            p_value = p1,
            method = "analytic",
            n_draws = NA_integer_,
            variance_combine = NA_character_,
            estimand = "slope"
          )
        )
      )
      next
    }

    # method == "mc": draws of ATT vector, WLS per draw; Rubin combine W and B
    B = n_draws
    beta_draws = matrix(NA_real_, nrow = B, ncol = 2L)
    W_rows = matrix(NA_real_, nrow = B, ncol = 2L)
    for (b in seq_len(B)) {
      y_b = stats::rnorm(length(att), mean = att, sd = se)
      fit_b = wls_intercept_slope_var_did(y_b, se, x)
      beta_draws[b, ] = fit_b$beta
      W_rows[b, ] = diag(fit_b$var_beta)
    }
    W_bar = colMeans(W_rows, na.rm = TRUE)
    B_between = apply(beta_draws, 2, stats::var, na.rm = TRUE)

    se_rubin = sqrt(pmax(0, W_bar + (1 + 1 / B) * B_between))
    se_mc = apply(beta_draws, 2, stats::sd, na.rm = TRUE)
    se_analytic_vec = sqrt(var_analytic)

    pick_se = switch(
      variance_combine,
      rubin = se_rubin,
      analytic = se_analytic_vec,
      mc = se_mc
    )

    z0 = if (pick_se[1] > 0) abs(beta_hat[1] / pick_se[1]) else NA_real_
    z1 = if (pick_se[2] > 0) abs(beta_hat[2] / pick_se[2]) else NA_real_
    p0 = if (is.finite(z0)) 2 * (1 - stats::pnorm(z0)) else NA_real_
    p1 = if (is.finite(z1)) 2 * (1 - stats::pnorm(z1)) else NA_real_

    key = dplyr::slice(g, 1L) |> dplyr::select(dplyr::all_of(id_cols))
    out[[gi]] = dplyr::bind_rows(
      dplyr::bind_cols(
        key,
        tibble::tibble(
          term = "intercept",
          estimate = beta_hat[1],
          delta_se = pick_se[1],
          delta_se_within = sqrt(W_bar[1]),
          delta_se_between = sqrt(B_between[1]),
          p_value = p0,
          method = "mc",
          n_draws = as.integer(n_draws),
          variance_combine = variance_combine,
          estimand = "slope"
        )
      ),
      dplyr::bind_cols(
        key,
        tibble::tibble(
          term = "slope",
          estimate = beta_hat[2],
          delta_se = pick_se[2],
          delta_se_within = sqrt(W_bar[2]),
          delta_se_between = sqrt(B_between[2]),
          p_value = p1,
          method = "mc",
          n_draws = as.integer(n_draws),
          variance_combine = variance_combine,
          estimand = "slope"
        )
      )
    )
  }

  dplyr::bind_rows(out)
}

#' Build long-format counterfactual/treated/effect table
#'
#' Reshapes simulated summaries from [get_simeffects_did()] into long format with
#' `outcome_type` (`yhat0`, `yhat1`, `effect`) and matching `se`.
#'
#' @param grid Output from [get_yhat_did()].
#' @param m Fitted model; passed to [get_simeffects_did()] for inverse transform.
#' @param start Optional start date.
#' @param end Optional end date.
#' @param n Number of Monte Carlo draws.
#' @param date_col Date column.
#' @param unit_col Unit column.
#' @param treated_col Treated column.
#' @return Long table with `outcome_type`, `estimate`, and `se`.
#' @seealso [get_simeffects_did()], [get_qis_did()].
get_long_effects_did = function(
    grid,
    m = NULL,
    start = NULL,
    end = NULL,
    n = 10000,
    date_col = "date",
    unit_col = "fullaqsid",
    treated_col = "treated"
) {
  sims = get_simeffects_did(
    grid = grid,
    m = m,
    start = start,
    end = end,
    n = n,
    date_col = date_col,
    treated_col = treated_col
  )

  sims |>
    filter(.data[[treated_col]] %in% TRUE) |>
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

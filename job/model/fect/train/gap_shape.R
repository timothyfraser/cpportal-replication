# job/model/fect/train/gap_shape.R -------------------------------------------
# M9-SHAPE: shape-constrained hedonic GAP model (DESIGN section 33b).
#
# WHAT THIS IS
#   The shipped gap model is
#       gap_i = mu + beta_d*dist_road_i + beta_b*bg3_mean_i
#               + beta_p*log1p(pop_mean_i) + gamma_{m(i)} + delta_{c(i)} + e_i
#   fit by stats::lm() on OUT-OF-ZONE monitors only (train/functions.R ~:1504).
#   M9-shape replaces the LINEAR dist_road term with a rank-reduced cubic
#   regression spline h() on the axis  s = log(dist_road + 0.01)  and solves
#   ONE quadratic program subject to
#     (C1) monotone non-increasing h:  h'(s) <= 0 everywhere;
#     (C3) convexity h'' >= 0 -- pre-registered VARIANT, not headline, off by
#          default (spec$convex).
#
#   REGISTERED DESIGN CALL (F6, 2026-07-31, section 33b): the (C2) siting-class
#   ORDER CONE is NOT in this round. Section 33b says the class-order cone
#   "ships with the within-metro deviations u_mc or not at all"; the
#   NOT-AT-ALL branch is taken, so the gate rests on the monotone-distance
#   lever ALONE. The global-dummy cone that briefly lived here was an
#   unspecced third variant (it constrains BETWEEN-metro contrast-coded
#   dummies, not the within-metro ordering the design registered) and is gone.
#   Full C2 + u_mc is the M9-3 follow-up. NOTE: the siting-class FIXED EFFECT
#   itself is untouched -- it is ADR-0026 production behaviour and enters via
#   `spec$parametric`, not via any cone.
#
# PARAMETERISATION (round-3 fix, 2026-07-31 -- read before touching the design)
#   The constrained block is the ?pcls monotone idiom: an UNABSORBED `cr` basis
#   (absorb.cons = FALSE, so the coefficients are the spline's values at the
#   knots, which is exactly what mono.con() constrains) and NO separate
#   intercept -- the smooth carries the level. The parametric factors keep
#   ordinary contrast coding and the "(Intercept)" column is dropped after
#   model.matrix(). The earlier cut (separate intercept + column-centred spline)
#   was RANK DEFICIENT: centring removes each column's sample mean, not the
#   constant FUNCTION from the smooth's span, so pcls() solved on a cancelling
#   manifold (max|coef| ~ 1e12, one CV fold at |pred| ~ 1e82 out of sample).
#   Dropping the intercept is NECESSARY BUT NOT SUFFICIENT on this table: the
#   parametric block spans the constant on its own, because factor(siting_class)
#   is aliased with factor(metro_id) (US metros are all `network_default`,
#   European ones all background/industrial/traffic). lm() pivots that column
#   out and reports NA; pcls() does not, so the fitter now RANK-PRUNES the
#   parametric block against the smooth (same column space, columns dropped are
#   recorded in fit$par_dropped) before solving.
#   INVARIANT: [parametric | smooth] is FULL COLUMN RANK. The fitter asserts it
#   with qr() and the ill-conditioning tripwire below stays wired.
#
#   Solver is mgcv ONLY (smoothCon + mono.con + pcls), per section 33b: mgcv is
#   an R *recommended* package, so this is zero manifest churn on the train
#   bundle. `scam` is NOT installed on this machine (checked 2026-07-31) and is
#   rejected by the design anyway.
#
# WHY IT RAISES IN-ZONE PREDICTIONS (section 33b, sign chain)
#   gap under-predicted in-zone -> z too low -> theta* too low -> (theta*-Theta)
#   < 0 -> counterfactual too low -> ATT biased POSITIVE. Raising in-zone gap
#   moves ATT DOWN toward fect's negative truth. Per A1 the LEVER is the basis
#   (an 8-df smooth can bend below d = 0.03 km where a linear term cannot); the
#   monotone cone is a GUARD that stops the six NYCCAS near-road monitors from
#   driving the basis somewhere ridiculous.
#
# KNOTS (amendment A2, pre-registered)
#   INTERIOR knots at quantiles of the OUT-OF-ZONE s distribution; BOUNDARY
#   knots at min/max of the UNION of out-of-zone and in-zone s. Placing design
#   points using the COVARIATE distribution of the targets touches no in-zone
#   outcome -- it is not selection-on-the-answer.
#
# FLAG (default OFF -- no production change)
#   CPPORTAL_FECT_M9_SHAPE=1   (alias: CPPORTAL_FECT_GAP_SHAPE=1/on)
#   With the flag off, cpportal_gap_fit() returns stats::lm() and the pipeline
#   is byte-identical to today.
#   CPPORTAL_FECT_M9_SHAPE_BOOT=<B>  monitor-level bootstrap replicates for
#   se.fit. F5: when the shape flag is ON the DEFAULT is the design headline
#   B = 400 (the bootstrap IS the gate-arm SE); the env var only overrides it.
#   With the shape flag off the default stays 0. The plug-in active-set SE is
#   the design's CROSS-CHECK, never the headline.
#   G-S6: se_boot < se_plugin is a pre-registered VIOLATION (a
#   boundary-constrained plug-in must UNDERSTATE). It is now reported as a
#   warning + `gs6_violation` on the predict() return, NOT silently pmax()ed
#   away.
#
# BOTH GRAINS ARE CONSTRAINED (M9-r3, Tim's standing directive 2026-07-31)
#   F4's earlier resolution DISABLED the monitor-MONTH refinement on the M9 arm
#   so the constrained monitor-level h(d) could carry end to end. Tim overruled:
#   "monitor month is the more correct route ... use appropriate controls for
#   the heteroskedasticity but don't throw away large sample size for no reason.
#   This is a time series cross-sectional problem - don't throw away the time
#   series." So the refinement is RETAINED and CONSTRAINED instead: the monthly
#   model runs through this same fitter, with the same monotone I-spline basis
#   and the same C1 cone on dist_road, and its monthly terms (factor(moy),
#   rh_mm, t2_core_dist_km) sit in the FREE parametric block. The controls for
#   the heteroskedasticity/leverage Tim asked for are the WEIGHTS (see
#   .gap_weights_vec). CPPORTAL_FECT_M9_MONTHLY_OFF=1 keeps the old
#   refinement-off behaviour available as an explicit DIAGNOSTIC comparison arm.
#
# LOCKSTEP (section 33b): gap_rhs lives in train/functions.R,
#   diagnostics/run_d1_falsification.R and diagnostics/validate_m7_from_panel.R.
#   cpportal_gap_spec() is the single source of the shape spec; every caller
#   must build it from the same inputs and compare cpportal_gap_spec_digest().
# ---------------------------------------------------------------------------

# --- flag ------------------------------------------------------------------

# The shape constraint is a COMPONENT OF M10 (DESIGN section 37, Tim's final
# ruling 2026-08-03), so an UNSET flag now defaults to the anchor method:
# ON under CPPORTAL_FECT_ANCHOR_METHOD=M10 (the default), OFF under the legacy
# M7 rollback. An EXPLICITLY set CPPORTAL_FECT_M9_SHAPE (or its
# CPPORTAL_FECT_GAP_SHAPE alias) still wins in either direction, which is what
# keeps the decomposition arms (shape-only / no-shape) runnable.
# Deliberately self-contained: gap_shape.R is sourced standalone by the
# diagnostics scripts, so it reads the env var itself rather than depending on
# train/functions.R being loaded.
cpportal_gap_shape_enabled = function() {
  v = Sys.getenv("CPPORTAL_FECT_M9_SHAPE", "")
  if (!nzchar(trimws(v))) v = Sys.getenv("CPPORTAL_FECT_GAP_SHAPE", "")
  if (nzchar(trimws(v))) {
    return(tolower(trimws(v)) %in% c("1", "true", "t", "yes", "y", "on"))
  }
  am = toupper(trimws(Sys.getenv("CPPORTAL_FECT_ANCHOR_METHOD", "")))
  # Unset or unrecognised => the M10 default; only an explicit M7 turns it off.
  !identical(am, "M7")
}

# F5: the gate arm's SE is the monitor-level bootstrap. When the shape flag is
# on and CPPORTAL_FECT_M9_SHAPE_BOOT is unset, default to the design headline
# B = 400 rather than to 0 (which silently demoted the cross-check plug-in SE
# to headline). `enabled` lets a caller that fits with an explicit shape=TRUE
# (e.g. smoke_gap_shape.R) pick the arm without touching the environment.
# B = 100 since 2026-08-04, down from 400. MEASURED, not guessed: the whole
# error-vs-B curve was read off a single B=400 run on the real production gap
# frame by subsampling its (iid) replicates. Cost is linear in B at 2.339 s per
# replicate.
#
#     B    rel.SD of the SE   1/sqrt(2B)   median bias vs B=400   secs
#    25         13.9%           14.1%            -1.4%             58
#    50          9.7%           10.0%            -0.6%            117
#   100          6.1%            7.1%            -0.2%            234   <- here
#   150          4.4%            5.8%            -0.5%            351
#   200          3.5%            5.0%            -0.1%            468
#   400            --            3.5%              0              936
#
# Read that as: 400 -> 100 moves the SE's own relative wobble from ~3.5% to
# ~6.1% and costs 234 s instead of 936 s. Three reasons that is a cheap trade:
#   * It is error on the STANDARD ERROR. The bootstrap yields no point estimate,
#     so no published ATT moves.
#   * Bias is -0.2%. Noisier, not shifted -- it does not systematically flatter
#     or inflate a CI.
#   * 6% on one variance component is far inside the gap model's own
#     specification uncertainty.
# (The B >= 200 rows understate rel.SD: subsets drawn from the same 400-replicate
# pool share replicates with the reference. The small-B end, which is what this
# decision rests on, tracks theory closely.)
#
# `min_rep = max(2, ceiling(0.125 * B))` scales with B, so the
# too-few-replicates fallback keeps the same proportional threshold and the same
# rows qualify. WATCH ON THE NEXT RUN: a noisier bootstrap SE dips below the
# plug-in SE slightly more often, so a modest rise in G-S6 violation warnings is
# expected and is not by itself evidence of a problem.
#
# Full analysis: docs/model/TIMING.md. Override with CPPORTAL_FECT_M9_SHAPE_BOOT.
CPPORTAL_GAP_SHAPE_BOOT_DEFAULT = 100L

# --- PARTIALLY POOLED METRO INTERCEPT (2026-08-04) --------------------------
#
# THE DEFECT. `gap_rhs` carries `factor(metro_id)` -- a metro FIXED effect fit
# on OUT-OF-CORDON donor monitors. Donor counts are wildly unbalanced: several
# treated metros contribute one donor or none at all. A metro with k_m = 1
# donor has an alpha identified by a single monitor; a metro with k_m = 0 has no
# alpha at all, yet its in-zone monitors still need gap predictions.
#
# Two things break as a result:
#   (1) IDENTIFICATION. k_m = 0 => alpha_m is not estimable, so the FE model
#       simply cannot score that metro. k_m = 1 => alpha_m IS the single donor's
#       residual, an estimate with no degrees of freedom and a spuriously TIGHT
#       implied interval (the FE machinery reports the sampling SE of a mean of
#       one, conditional on the level being real).
#   (2) THE BOOTSTRAP. `.gap_shape_boot_se()` resamples monitors STRATIFIED
#       WITHIN metro, so it cannot invent a level -- but for k_m = 1 every
#       replicate draws the same row, that metro's dummy is collinear with the
#       rest of the design, the constrained `pcls` solve RANK-PRUNES the column
#       (fit$par_dropped), and scoring newdata that contains the metro then
#       errors `factor(metro_id) has new levels ...`. DETERMINISTIC: 400 of 400
#       replicates fail, se_boot is NA, and predict() silently degrades to the
#       plug-in active-set cross-check SE as the headline.
#
# THE FIX. Treat the metro intercept as a RANDOM effect, alpha_m ~ N(0, tau^2),
# and pool partially instead of estimating k_m free levels:
#
#   point:      alpha_hat_m = [k_m/sigma^2] / [k_m/sigma^2 + 1/tau^2] * rbar_m
#                           = k_m tau^2 / (k_m tau^2 + sigma^2) * rbar_m
#   prediction: nu_hat_m^2  = tau^2 sigma^2 / (k_m tau^2 + sigma^2)
#
# k_m = 0 gives alpha_hat = 0 (the grand mean -- the honest answer for a metro
# with no donors) and nu^2 = tau^2 (the full prior spread, i.e. maximally wide).
# k_m -> Inf gives back the FE estimate with nu^2 -> sigma^2/k_m. So the change
# is a strict generalisation: adequately-donated metros are essentially
# unaffected, thin ones get shrunk toward zero and get HONESTLY WIDE intervals
# instead of spuriously tight or missing ones.
#
# WHY NOT lme4. The shape term must remain a MONOTONE-CONSTRAINED `pcls` solve
# (the C1 cone is the registered estimator; dropping it to obtain a mixed model
# would change the estimand). lmer cannot carry an inequality-constrained
# smooth. The correct composition, and the one taken here, is BACKFITTING:
# alternate (a) the constrained QP on the metro-adjusted response y - alpha, and
# (b) a closed-form one-way random-effects update of alpha from the residuals.
# Both steps decrease the same penalised criterion and it converges in a handful
# of sweeps. tau^2 and sigma^2 come from METHOD OF MOMENTS on the residual
# cluster means (the standard one-way random-effects / James-Stein estimator),
# using only metros with ADEQUATE donor count -- a metro with one donor supplies
# no information about the between-metro spread.
#
# GRAIN. sigma^2 here is the variance of a metro's mean over its DONOR
# MONITORS, so both moments are computed on MONITOR-level residual means
# (`pool_cluster`, default "fullaqsid") rather than on rows. That matters for
# the monitor-MONTH refinement, where a single donor contributes hundreds of
# rows but still only one independent cluster; using rows would make k_m huge
# and pool almost nothing.
#
# FLAGS
#   CPPORTAL_FECT_GAP_POOL=0/1     force off/on (default: ON whenever the shape
#                                  arm is on, i.e. under M10)
#   CPPORTAL_FECT_GAP_MIN_DONORS=K K, the adequate-donor threshold (default 3).
#                                  Metros with k_m >= K keep the nonparametric
#                                  within-metro resample in the bootstrap and
#                                  are the ones tau^2 is estimated from; metros
#                                  with k_m < K hold their rows FIXED and draw
#                                  alpha_m* ~ N(alpha_hat_m, nu_hat_m^2) instead
#                                  (the HYBRID bootstrap).
#
# WHICH VALUES OF K ACTUALLY DO ANYTHING (checked 2026-08-04 against the gap
# training frame, n=373 monitors over 20 metros). K enters in exactly TWO
# places, both THRESHOLD comparisons against the per-metro donor count k_m:
#     adequate set   k_m >= K   (metros tau^2 is estimated from; .gap_pool_update)
#     small set      k_m <  K   (metros that draw alpha* instead of resampling)
# so K changes behaviour only when it crosses an OBSERVED k_m. Observed:
#     NYC 81, London 64, Oslo 8, Milan 4, Singapore 4, Stockholm 3,
#     Gothenburg 1, Bergen 1; controls 4..43, no singletons
# -- there is NO metro with k_m = 2. Therefore:
#     K=2 and K=3 are IDENTICAL. Both put exactly {Gothenburg, Bergen} (k=1) in
#       the small set and everything else in the adequate set. K=2 is not a
#       sensitivity test, it is the same run.
#     K=4 is the FIRST value that changes anything: it moves Stockholm (k=3)
#       out of the adequate set and onto the parametric draw.
#     K=5 additionally takes Milan and Singapore (k=4) and every k=4 control.
# Do not spend a retrain on a K that does not cross a k_m.
#
# The flag-OFF / lm() fallback path is untouched: it still fits the metro FIXED
# effect exactly as production does, so the M7 rollback stays byte-identical.
CPPORTAL_GAP_POOL_MIN_DONORS_DEFAULT = 3L
CPPORTAL_GAP_POOL_TERM = "factor(metro_id)"
CPPORTAL_GAP_POOL_VAR = "metro_id"
CPPORTAL_GAP_POOL_CLUSTER = "fullaqsid"

cpportal_gap_pool_enabled = function(shape = NULL) {
  v = trimws(Sys.getenv("CPPORTAL_FECT_GAP_POOL", ""))
  if (nzchar(v)) return(tolower(v) %in% c("1", "true", "t", "yes", "y", "on"))
  if (is.null(shape)) cpportal_gap_shape_enabled() else isTRUE(shape)
}

#' JOINT (ridge-in-pcls) vs BACKFIT for the pooled metro intercept.
#'
#' A Gaussian random intercept IS an L2 penalty: alpha_m ~ N(0, tau^2) with
#' residual variance sigma^2 is exactly a ridge with lambda = sigma^2/tau^2.
#' So the pooled level does not need a separate alternating step at all -- the
#' metro dummies can go straight into the SAME constrained QP as a second
#' penalty block, and pcls solves both blocks at once.
#'
#' WHY THIS MATTERS HERE. The backfit alternates between two blocks whose third
#' canonical correlation is 0.8464, so it contracts at ~0.716 per sweep
#' (measured: 0.68) and never reaches its fixed point inside the 12-sweep cap --
#' the non-convergence warning fires on EVERY pooled fit. That is not a tuning
#' problem, it is the convergence rate of backfitting between near-collinear
#' blocks. Solving jointly removes the iteration over coefficients entirely;
#' only the SCALAR variance ratio lambda is iterated, and a scalar sequence
#' converges in a handful of steps.
#'
#' It also fixes identification for free. The constant lives in the UNPENALISED
#' smooth and alpha is penalised, so the penalised optimum puts the level in the
#' smooth and keeps alpha minimal -- mean-zero pooled intercepts by
#' construction, which is what alpha_m ~ N(0, tau^2) asserted all along.
#'
#' CPPORTAL_FECT_GAP_POOL_JOINT=0 falls back to the backfit (kept so the two can
#' be run against each other on one seed).
cpportal_gap_pool_joint = function() {
  v = trimws(Sys.getenv("CPPORTAL_FECT_GAP_POOL_JOINT", ""))
  if (!nzchar(v)) return(TRUE)
  tolower(v) %in% c("1", "true", "t", "yes", "y", "on")
}

cpportal_gap_pool_min_donors = function() {
  raw = trimws(Sys.getenv("CPPORTAL_FECT_GAP_MIN_DONORS", ""))
  if (!nzchar(raw)) return(CPPORTAL_GAP_POOL_MIN_DONORS_DEFAULT)
  v = suppressWarnings(as.integer(raw))
  if (is.na(v) || v < 1L) CPPORTAL_GAP_POOL_MIN_DONORS_DEFAULT else v
}

# Both lambda-uncertainty repairs on the joint path (edf convention + the
# delta-method Vc term). ON by default whenever the joint solve is on; set
# CPPORTAL_FECT_GAP_JOINT_VCORR=0 to reproduce the old conditional-Vp behaviour.
cpportal_gap_joint_vcorr = function() {
  v = trimws(Sys.getenv("CPPORTAL_FECT_GAP_JOINT_VCORR", ""))
  if (!nzchar(v)) return(TRUE)
  !(tolower(v) %in% c("0", "false", "f", "no", "n"))
}

cpportal_gap_shape_boot_B = function(enabled = NULL) {
  raw = trimws(Sys.getenv("CPPORTAL_FECT_M9_SHAPE_BOOT", ""))
  if (!nzchar(raw)) {
    on = if (is.null(enabled)) cpportal_gap_shape_enabled() else isTRUE(enabled)
    return(if (on) CPPORTAL_GAP_SHAPE_BOOT_DEFAULT else 0L)
  }
  v = suppressWarnings(as.integer(raw))
  if (is.na(v) || v < 0L) 0L else v
}

# --- F7: LOUD degradation ----------------------------------------------------
# Every lm() fallback (headline OR a CV fold) is a silent estimator swap: the
# fold stops being the constrained estimator and sigma_gap quietly becomes a
# blend. Count them in a package-local environment so the count survives the
# fold loop, and stamp the count onto every fit object so a consumer holding
# only the fit can still see it.
.CPPORTAL_GAP_STATE = new.env(parent = emptyenv())
.CPPORTAL_GAP_STATE$n_fallback = 0L
.CPPORTAL_GAP_STATE$reasons = character(0)

cpportal_gap_fallback_reset = function() {
  .CPPORTAL_GAP_STATE$n_fallback = 0L
  .CPPORTAL_GAP_STATE$reasons = character(0)
  invisible(TRUE)
}

cpportal_gap_fallback_count = function() .CPPORTAL_GAP_STATE$n_fallback

cpportal_gap_fallback_reasons = function() .CPPORTAL_GAP_STATE$reasons

.cpportal_gap_fallback_note = function(reason, where = "headline") {
  .CPPORTAL_GAP_STATE$n_fallback = .CPPORTAL_GAP_STATE$n_fallback + 1L
  .CPPORTAL_GAP_STATE$reasons = c(.CPPORTAL_GAP_STATE$reasons,
                                  sprintf("%s: %s", where, reason))
  warning(sprintf(paste0("[M9-shape] DEGRADED (#%d): constrained fit failed at ",
                         "%s (%s); this fit is an UNCONSTRAINED lm() -- the ",
                         "gate arm is no longer the registered estimator"),
                  .CPPORTAL_GAP_STATE$n_fallback, where, reason),
          call. = FALSE, immediate. = TRUE)
  invisible(.CPPORTAL_GAP_STATE$n_fallback)
}

# Road-exposure chain. RETAINED as documentation of the registered ordering for
# the M9-3 follow-up (C2 + within-metro deviations u_mc); it is NOT used to
# build any constraint this round -- see the F6 note in the header.
CPPORTAL_GAP_CLASS_CHAIN = c("kerbside", "traffic", "suburban",
                             "background", "regional")

# --- spec ------------------------------------------------------------------

#' Build the M9-shape spec (basis + axis + knots + constraint set + penalty).
#'
#' @param gap_rhs character vector of rhs terms as built in functions.R
#' @param train   the gap training frame (out-of-zone monitors)
#' @param target_dist_km numeric vector of dist_road for the PREDICTION targets
#'   (in-zone monitors). Used ONLY for boundary knots (amendment A2).
#' @param k number of spline basis functions (design: 8)
cpportal_gap_spec = function(gap_rhs, train, target_dist_km = NULL,
                             k = 8L, dist_var = "dist_road", offset = 0.01,
                             convex = FALSE, class_var = "siting_class",
                             pool = NULL) {
  stopifnot(is.character(gap_rhs))
  # Partially pooled metro intercept: pull factor(metro_id) OUT of the free
  # parametric block so it is never a set of estimated dummy columns. It comes
  # back as a shrunken random effect in the backfitting loop (see
  # .gap_pool_update / the header block). When pooling is off the term stays in
  # `parametric` and the estimator is the old metro FIXED effect, unchanged.
  pool_on = if (is.null(pool)) cpportal_gap_pool_enabled() else isTRUE(pool)
  pool_term = if (pool_on && CPPORTAL_GAP_POOL_TERM %in% gap_rhs)
    CPPORTAL_GAP_POOL_TERM else NULL
  has_dist = dist_var %in% gap_rhs
  s_tr = if (has_dist && dist_var %in% names(train)) {
    log(as.numeric(train[[dist_var]]) + offset)
  } else numeric(0)
  s_tr = s_tr[is.finite(s_tr)]
  s_tg = if (length(target_dist_km)) log(as.numeric(target_dist_km) + offset) else numeric(0)
  s_tg = s_tg[is.finite(s_tg)]

  knots = NULL
  if (length(s_tr) >= k) {
    # interior at OUT-OF-ZONE quantiles; boundary at the UNION min/max (A2)
    interior = stats::quantile(s_tr, probs = seq(0, 1, length.out = k)[-c(1, k)],
                               names = FALSE, type = 7)
    lo = min(c(s_tr, s_tg))
    hi = max(c(s_tr, s_tg))
    knots = sort(unique(c(lo, interior, hi)))
    # a cr basis of rank k needs exactly k knots; pad/trim defensively
    if (length(knots) != k) {
      knots = seq(lo, hi, length.out = k)
    }
  }

  spec = list(
    version      = "M9-shape-1",
    basis        = "cr",
    k            = as.integer(k),
    axis         = sprintf("log(%s + %s)", dist_var, format(offset)),
    dist_var     = dist_var,
    offset       = offset,
    knots        = knots,
    # F6 (registered): monotone-distance lever ALONE. No C2 cone this round.
    constraints  = c("C1_monotone_nonincreasing",
                     if (isTRUE(convex)) "C3_convex"),
    convex       = isTRUE(convex),
    class_var    = class_var,
    class_chain  = CPPORTAL_GAP_CLASS_CHAIN,
    parametric   = setdiff(gap_rhs, c(dist_var, pool_term)),
    has_dist     = has_dist,
    # Partially pooled metro intercept (NULL => old fixed-effect behaviour)
    pool_term    = pool_term,
    pool_var     = if (is.null(pool_term)) NULL else CPPORTAL_GAP_POOL_VAR,
    pool_cluster = if (is.null(pool_term)) NULL else CPPORTAL_GAP_POOL_CLUSTER,
    pool_min_donors = if (is.null(pool_term)) NULL else cpportal_gap_pool_min_donors()
  )
  spec$digest = cpportal_gap_spec_digest(spec)
  spec
}

cpportal_gap_spec_digest = function(spec) {
  spec$digest = NULL
  txt = paste(utils::capture.output(utils::str(spec, max.level = 3L,
                                               digits.d = 8L)),
              collapse = "\n")
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(txt, algo = "md5"))
  }
  # dependency-free fallback checksum (deterministic, adequate for lockstep)
  iv = utf8ToInt(txt)
  sprintf("fnv-%08x", sum(as.numeric(iv) * seq_along(iv)) %% 2^31)
}

#' Lockstep guard: stop() unless the caller's spec matches `expected`.
cpportal_gap_spec_assert = function(spec, expected, where = "gap spec") {
  if (is.null(expected)) return(invisible(TRUE))
  got = if (is.null(spec$digest)) cpportal_gap_spec_digest(spec) else spec$digest
  if (!identical(got, expected)) {
    stop(sprintf("[M9-shape] %s: gap spec digest mismatch (got %s, expected %s) - the three gap_rhs copies are OUT OF LOCKSTEP",
                 where, got, expected), call. = FALSE)
  }
  invisible(TRUE)
}

# --- design construction ---------------------------------------------------

.gap_parametric_formula = function(fml, spec) {
  lhs = as.character(fml)[2]
  rhs = spec$parametric
  rhs = rhs[nzchar(rhs)]
  if (!length(rhs)) rhs = "1"
  stats::as.formula(paste0(lhs, " ~ ", paste(rhs, collapse = " + ")))
}

# RESPONSE-FREE twin of the above, used at PREDICT time. predict_gap_units()
# scores `mon_obs`, which has no `gap` column, so a formula that still carries
# the response makes model.frame() evaluate `gap` — it either errors ("object
# 'gap' not found") or, worse, silently picks a stray `gap` up out of the
# formula's environment. predict.lm() avoids this with delete.response(); this
# is the same guard.
.gap_parametric_formula_rhs = function(spec) {
  rhs = spec$parametric
  rhs = rhs[nzchar(rhs)]
  if (!length(rhs)) rhs = "1"
  stats::as.formula(paste0("~ ", paste(rhs, collapse = " + ")))
}

# Parametric design matrix. THE INTERCEPT COLUMN IS DROPPED (`drop_intercept`),
# and the spline block is left UNCENTRED, so the smooth carries the level.
#
# WHY (round-2 blocking defect, fixed 2026-07-31): the previous cut kept a
# separate intercept and column-centred the spline block. Centring removes each
# basis column's sample MEAN but NOT the constant FUNCTION from the smooth's
# SPAN -- a `cr` basis built with absorb.cons = FALSE reproduces any constant
# exactly (all knot coefficients equal), and subtracting a constant vector from
# each column leaves that direction inside the column space. So
# [intercept | centred spline] stayed rank deficient, pcls() returned an
# arbitrary point on a cancelling manifold (max|coef| ~ 1e12 on every fold, one
# fold at |pred| ~ 1e82 out of sample), and the coefficients, plug-in SEs and
# out-of-sample predictions were not unique.
#
# THE STANDARD mgcv APPROACH, and the one taken here, is the ?pcls monotone
# idiom: keep the smooth in its UNABSORBED knot-value parameterisation (which
# mono.con requires -- its constraints are differences of consecutive knot
# values) and give it the level by dropping the separate intercept. The
# parametric factors keep their usual CONTRAST coding (model.matrix is built
# with the intercept and the column is removed afterwards, NOT by writing `- 1`
# into the formula, which would switch the first factor to full dummy coding
# and re-introduce the constant). Result: exactly one constant direction in the
# design, unique solve.
.gap_par_mm = function(fml_par, data, xlev = NULL, drop_intercept = FALSE) {
  mf = stats::model.frame(fml_par, data = data, na.action = stats::na.pass,
                          xlev = xlev)
  tt = stats::terms(mf)
  X = stats::model.matrix(tt, mf)
  if (isTRUE(drop_intercept)) {
    keep = colnames(X) != "(Intercept)"
    X = X[, keep, drop = FALSE]
  }
  list(X = X, terms = tt, xlevels = stats::.getXlevels(tt, mf))
}

# F6 (registered, 2026-07-31): `.gap_class_con()` -- which built (C2) as an
# order cone on the CONTRAST-CODED global class dummies -- has been REMOVED.
# Section 33b registered the class-order cone as a WITHIN-metro statement that
# ships together with the deviations u_mc; a cone on global dummies is a
# different, unspecced estimator. The not-at-all branch is taken; C2 + u_mc is
# the M9-3 follow-up task. The class FIXED EFFECT is unaffected.

# --- fitter ----------------------------------------------------------------

#' Fit the gap model: stats::lm() when the flag is off, constrained QP when on.
#'
#' Returns either an `lm` or an object of class `cpportal_gap_shape_fit`, which
#' exposes `$xlevels` and a `predict()` method with `se.fit = TRUE`, so it is a
#' drop-in for every downstream consumer in train/functions.R.
#' @param where free-text label ("headline", "cv_fold_3", ...) used in the F7
#'   degradation warning so a fold failure is distinguishable from a headline
#'   failure in the log.
#' @param weights optional numeric vector of observation weights, length
#'   nrow(data). Passed to pcls() as G$w (mgcv minimises
#'   || W^0.5 (Xp - y) ||^2 + sum_i lambda_i p'S_i p, W = diag(w)), and to
#'   stats::lm() on the flag-off / fallback branch. This is a REAL weighted
#'   solve -- no row duplication, which would corrupt both the penalty scale
#'   and the residual df.
cpportal_gap_fit = function(formula, data, spec = NULL, shape = NULL,
                            where = "headline", weights = NULL) {
  use_shape = if (is.null(shape)) cpportal_gap_shape_enabled() else isTRUE(shape)
  w = .gap_weights_vec(weights, nrow(data))
  if (!use_shape || is.null(spec) || !isTRUE(spec$has_dist)) {
    # Flag OFF: byte-identical to production. No counter, no warning -- an
    # unconstrained lm() is the INTENDED estimator here, not a degradation.
    return(.gap_lm(formula, data, w))
  }
  out = tryCatch(cpportal_gap_fit_shaped(formula, data, spec, weights = w),
                 error = function(e) {
                   .cpportal_gap_fallback_note(conditionMessage(e), where)
                   NULL
                 })
  if (!is.null(out)) {
    out$n_fallback = cpportal_gap_fallback_count()
    return(out)
  }
  out = .gap_lm(formula, data, w)
  attr(out, "cpportal_gap_fallback") = TRUE
  attr(out, "cpportal_gap_n_fallback") = cpportal_gap_fallback_count()
  out
}

#' Fallback count carried by ANY gap fit (shape fit or degraded lm).
cpportal_gap_fit_n_fallback = function(fit) {
  if (inherits(fit, "cpportal_gap_shape_fit")) return(fit$n_fallback %||% 0L)
  n = attr(fit, "cpportal_gap_n_fallback")
  if (is.null(n)) 0L else as.integer(n)
}

if (!exists("%||%", mode = "function")) {
  `%||%` = function(a, b) if (is.null(a)) b else a
}

# --- weights (M9-r3, Tim's standing directive 2026-07-31) -------------------
# The monitor-MONTH refinement is the more correct grain and it STAYS -- "don't
# throw away the time series". What it needs is the right variance/leverage
# treatment, which means a genuinely WEIGHTED constrained solve:
#   * reliability weight  n_days_mm  (a cell built from 28 days is not a cell
#     built from 15) -- heteroskedasticity of the cell mean; and
#   * cluster-balance weight 1/m_i  (m_i = that monitor's month count) so a
#     monitor with 300 months does not supply 20x the leverage on the STATIC
#     block -- h(d), bg3, pop, siting -- that a 15-month monitor does.
# mgcv::pcls takes these directly on the prepared G object (G$w, objective
# ||W^0.5(Xp - y)||^2). Row duplication would have been the wrong instrument:
# it can only express integer weights, and it inflates n, hence df.residual and
# the penalty-to-likelihood ratio.
.gap_weights_vec = function(weights, n) {
  if (is.null(weights)) return(NULL)
  w = as.numeric(weights)
  if (length(w) == 1L) w = rep(w, n)
  if (length(w) != n) stop("weights length (", length(w), ") != nrow(data) (", n, ")")
  if (any(!is.finite(w)) || any(w < 0)) stop("weights must be finite and non-negative")
  if (sum(w) <= 0) stop("weights are all zero")
  w
}

.gap_lm = function(formula, data, w) {
  if (is.null(w)) return(stats::lm(formula, data = data))
  # NSE, carefully. `lm()` does NOT evaluate `weights` here: it splices the
  # unevaluated expression into a `model.frame()` call, and model.frame.default
  # evaluates it with `eval(extras, data, environment(formula))`. So the
  # expression is looked up (1) among the COLUMNS of `data`, then (2) in the
  # formula's environment -- which is the CALLER's frame, not this function's.
  #
  # That is what broke round 3: `weights = data[[".gap_w"]]` needs a binding
  # called `data`, and in the production caller (train/functions.R, where the
  # gap frame is named `gap_mm_train`) no such binding exists, so the lookup
  # walked out to `utils::data` and R raised
  #   "object of type 'closure' is not subsettable"
  # -- killing the monitor-month refinement on the FLAGS-OFF arm. A bare
  # `weights = w` fails the same way (no `w` in the caller's frame).
  #
  # The fix uses step (1) instead of step (2): put the weights in `data` as a
  # plain column and name that column with a BARE SYMBOL, which model.frame
  # resolves from the data mask regardless of what the caller's frame holds.
  # The call is built and evaluated in THIS frame so `data` is this local.
  data[[".gap_w"]] = w
  eval(as.call(list(quote(stats::lm), formula = formula, data = quote(data),
                    weights = quote(.gap_w))),
       envir = environment())
}

# --- partially pooled metro intercept: the closed-form backfitting step ------
#
# ONE SWEEP of the random-intercept update, given the current mean-function
# residuals r_i = y_i - x_i'beta (NOT y - x'beta - alpha; alpha is what we are
# re-deriving). Returns alpha_hat, nu2, k, tau2, sigma2.
#
# Moments are taken at the CLUSTER (monitor) grain, because that is the level at
# which the gap residual is independent: a donor monitor contributing 300
# monitor-months is still ONE draw from the metro's siting distribution. Using
# rows would inflate k_m by ~300x on the monthly frame and pool essentially
# nothing.
#
#   c_j    = weighted mean residual of cluster j
#   rbar_m = mean of c_j over the k_m clusters in metro m
#   sigma^2 = pooled within-metro variance of c_j about rbar_m,
#             sum_m sum_j (c_j - rbar_m)^2 / sum_m (k_m - 1)     [metros k_m>=2]
#   tau^2   = MoM on the ADEQUATE metros (k_m >= min_donors):
#             (|A|/(|A|-1)) * mean_A(rbar_m^2) - mean_A(sigma^2 / k_m), floored at 0
#             The |A|/(|A|-1) factor corrects for rbar_m being a deviation from
#             an implicitly estimated grand mean (the smooth carries the level),
#             which otherwise biases the between-metro spread DOWN.
#   alpha_hat_m = k_m tau^2 / (k_m tau^2 + sigma^2) * rbar_m     [BLUP shrinkage]
#   nu2_m       = tau^2 sigma^2 / (k_m tau^2 + sigma^2)          [pred variance]
#
# A metro absent from training has k_m = 0 => alpha_hat = 0, nu2 = tau^2. That
# is handled by the ACCESSOR (.gap_pool_alpha), not here.
#' @param codes optional cached grouping built by `.gap_pool_codes()`. The
#'   (metro, monitor) grouping is FIXED for the life of a fit, but the backfit
#'   sweep calls this once per iteration on a new residual vector -- rebuilding
#'   the paste key + two tapply()s every sweep was 8.6% of the bootstrap's
#'   runtime for no new information. The cache is used only when the finiteness
#'   filter selects every row (the normal case); otherwise the grouping is
#'   rebuilt on the surviving subset exactly as before.
.gap_pool_codes = function(grp, clust, n) {
  grp = as.character(grp)
  clust = if (is.null(clust)) as.character(seq_len(n)) else as.character(clust)
  f = factor(paste(grp, clust, sep = "\r"))
  m_j = sub("\r.*$", "", levels(f))
  list(f = f, m_j = m_j, mf = factor(m_j))
}

.gap_pool_update = function(r, w, grp, clust, min_donors, codes = NULL) {
  ok = is.finite(r) & is.finite(w) & w > 0 & !is.na(grp)
  if (!is.null(codes) && !all(ok)) codes = NULL
  if (!all(ok)) { r = r[ok]; w = w[ok]; grp = as.character(grp)[ok] }
  if (is.null(codes)) {
    clust = if (is.null(clust)) seq_along(r) else as.character(clust)[ok]
  }
  if (!length(r)) return(NULL)
  # cluster means (weighted), and each cluster's metro. rowsum() on a factor
  # groups in level (i.e. sorted) order -- byte-identical grouping to the
  # tapply()/split() pair this replaced, at a fraction of the cost.
  if (is.null(codes)) codes = .gap_pool_codes(grp, clust, length(r))
  f = codes$f; m_j = codes$m_j; mf = codes$mf
  sw = as.numeric(rowsum(w, f))
  swr = as.numeric(rowsum(w * r, f))
  c_j = swr / sw
  mi = as.integer(mf)
  k = as.integer(tabulate(mi, nbins = nlevels(mf)))
  rbar = as.numeric(rowsum(c_j, mf)) / k
  names(k) = levels(mf); names(rbar) = levels(mf)
  # IDENTIFICATION. alpha_m ~ N(0, tau^2) says the metro levels are deviations
  # about a common mean, and that common mean is ALREADY carried by the smooth
  # (the `cr` basis is unabsorbed, so it spans the constant). Without imposing
  # sum(alpha) = 0 the two blocks fight over the constant direction: the
  # backfit still converges, but LINEARLY and slowly (observed: 12+ sweeps with
  # alpha still moving in the 3rd decimal), because the blocks are nearly
  # collinear. Centring the metro means makes them orthogonal in that direction
  # and the sweep converges in ~2-3. It is the standard sum-to-zero convention,
  # not a numerical hack -- and it is what makes alpha_hat = 0 mean "the grand
  # mean" for a zero-donor metro.
  # sigma^2: pooled within-metro dispersion of cluster means. Computed on the
  # UNCENTRED means (the centring below is a global shift, so it cancels here --
  # but the order matters for readers).
  ss = sum((c_j - rbar[mi])^2)
  rbar = rbar - mean(rbar)
  df = sum(pmax(k - 1L, 0L))
  sigma2 = if (df > 0L) ss / df else NA_real_
  # tau^2: MoM over ADEQUATE metros only. A one-donor metro's rbar_m carries no
  # information about the between-metro spread (its sampling variance is the
  # whole of sigma^2), so including it would inflate tau^2 by exactly the noise
  # we are trying to shrink away.
  adq = names(k)[k >= min_donors]
  tau2 = NA_real_
  if (length(adq) >= 2L && is.finite(sigma2)) {
    nA = length(adq)
    tau2 = max(0, (nA / (nA - 1)) * mean(rbar[adq]^2) - mean(sigma2 / k[adq]))
  }
  if (!is.finite(sigma2) || !is.finite(tau2)) return(NULL)
  denom = k * tau2 + sigma2
  alpha = ifelse(denom > 0, (k * tau2 / denom) * rbar, 0)
  nu2 = ifelse(denom > 0, tau2 * sigma2 / denom, 0)
  names(alpha) = names(k); names(nu2) = names(k)
  list(alpha = alpha, nu2 = nu2, k = k, rbar = rbar, tau2 = tau2,
       sigma2 = sigma2, min_donors = min_donors, adequate = adq)
}

# alpha (and nu^2) for arbitrary metro ids. UNKNOWN metro => k = 0 => the
# grand-mean answer alpha = 0 with the FULL prior variance nu^2 = tau^2. That is
# what makes a zero-donor metro scorable at all, and honestly wide when it is.
.gap_pool_alpha = function(pool, ids, override = NULL) {
  if (is.null(pool)) return(list(alpha = 0, nu2 = 0))
  ids = as.character(ids)
  a = pool$alpha
  # `override` MERGES over the estimated levels (it does not replace them): the
  # hybrid bootstrap draws alpha_m* only for the small-k metros and for metros
  # absent from training, and every other metro keeps its resample-derived BLUP.
  if (!is.null(override) && length(override)) {
    a = c(a[setdiff(names(a), names(override))], override)
  }
  al = unname(a[match(ids, names(a))]); al[is.na(al)] = 0
  nu = unname(pool$nu2[match(ids, names(pool$nu2))])
  nu[is.na(nu)] = pool$tau2
  list(alpha = as.numeric(al), nu2 = as.numeric(nu))
}

.gap_pool_k = function(pool, ids) {
  if (is.null(pool)) return(rep(NA_integer_, length(ids)))
  kk = unname(pool$k[match(as.character(ids), names(pool$k))])
  kk[is.na(kk)] = 0L
  as.integer(kk)
}

#' @param pool_alpha_start named alpha_m to START the backfit sweep from
#'   (default: all zero). The sweep's fixed point does not depend on where it
#'   starts, but its COST does: a bootstrap replicate's alpha sits within a
#'   bootstrap SE of the headline fit's alpha, so starting there removes most of
#'   the sweeps. Supplied by `.gap_shape_boot_se()`; never by the headline fit.
# --- JOINT solve: the pooled intercept as a ridge block inside the QP --------
#
# Minimises, over (beta, alpha) TOGETHER,
#
#   || W^0.5 (y - X beta - Z alpha) ||^2  +  sp * beta' S beta  +  lambda ||alpha||^2
#     subject to  Ain beta >= bin              (the C1 monotone cone, untouched)
#
# with lambda = sigma^2 / tau^2. That is the penalised-likelihood form of the
# one-way random intercept, so `alpha` comes out as the BLUP -- but conditioned
# on the smooth and the parametric block rather than on marginal residuals,
# which is what the backfit could only approximate.
#
# `Z` carries FULL dummy coding (no reference level). The design is then rank
# deficient by one against the smooth's constant, and that is FINE and in fact
# the point: the constant is unpenalised in the smooth and penalised in alpha,
# so the penalised problem has a unique solution which puts the level in the
# smooth. No sum-to-zero constraint, no gauge choice.
#
# Only lambda iterates. sigma^2/tau^2 come from the SAME method-of-moments
# machinery the backfit used (`.gap_pool_update` on the alpha-removed residual),
# so the variance-component ESTIMATOR is unchanged -- only how the coefficients
# are obtained changes.
#
# @param fixed_alpha named alpha imposed by a hybrid bootstrap draw. Imposed
#   levels are moved to an OFFSET and their columns dropped from Z, so they are
#   held exactly rather than re-shrunk.
.gap_pool_joint_fit = function(X, y, w, Ain, bin, p0, S, sp, npar, nsp,
                               grp_v, clu_v, min_don, codes, n,
                               fixed_alpha = NULL, lambda_start = NULL,
                               maxit = 20L, tol = 1e-4) {
  mlev = sort(unique(grp_v[!is.na(grp_v)]))
  fixed = if (is.null(fixed_alpha)) character(0) else
    intersect(mlev, names(fixed_alpha))
  free = setdiff(mlev, fixed)
  # imposed levels -> offset
  off_vec = rep(0, n)
  if (length(fixed)) {
    h = match(grp_v, names(fixed_alpha))
    hh = !is.na(h) & grp_v %in% fixed
    off_vec[hh] = as.numeric(fixed_alpha)[h[hh]]
  }
  if (!length(free)) return(NULL)
  Z = matrix(0, n, length(free))
  jz = match(grp_v, free); okz = !is.na(jz)
  Z[cbind(which(okz), jz[okz])] = 1
  nz = ncol(Z)

  Xa = cbind(X, Z)
  Aa = cbind(Ain, matrix(0, nrow(Ain), nz))
  p0a = c(p0, rep(0, nz))          # slack unchanged: Aa %*% p0a == Ain %*% p0
  ya = y - off_vec
  Sa = list(S[[1]], diag(nz))
  offs = c(npar, npar + nsp)
  nmain = npar + nsp

  lambda = if (!is.null(lambda_start) && is.finite(lambda_start) &&
                lambda_start > 0) lambda_start else 1
  pa = NULL; pu = NULL; it_used = 0L; converged = FALSE
  for (it in seq_len(maxit)) {
    it_used = it
    pa = mgcv::pcls(list(X = Xa, p = p0a, y = ya, w = w, Ain = Aa, bin = bin,
                         C = matrix(0, 0, 0), S = Sa, off = offs,
                         sp = c(sp, lambda)))
    if (!all(is.finite(pa))) return(NULL)
    # variance components on the residual with the metro level REMOVED -- the
    # same quantity, and the same estimator, the backfit fed to .gap_pool_update
    r_marg = ya - as.numeric(X %*% pa[seq_len(nmain)])
    pu = .gap_pool_update(r_marg, w, grp_v, clu_v, min_don, codes = codes)
    if (is.null(pu)) return(NULL)
    lam_new = pu$sigma2 / max(pu$tau2, 1e-12)
    if (!is.finite(lam_new) || lam_new <= 0) return(NULL)
    rel = abs(log(lam_new / lambda))
    lambda = lam_new
    if (rel < tol) { converged = TRUE; break }
  }
  # one final solve at the converged lambda so coefficients and variance
  # components describe the SAME fit
  pa = mgcv::pcls(list(X = Xa, p = p0a, y = ya, w = w, Ain = Aa, bin = bin,
                       C = matrix(0, 0, 0), S = Sa, off = offs,
                       sp = c(sp, lambda)))
  if (!all(is.finite(pa))) return(NULL)
  al = pa[nmain + seq_len(nz)]; names(al) = free
  # the fit's alpha: JOINT BLUP for the free metros, imposed draw for the rest
  alpha_all = al
  if (length(fixed)) {
    alpha_all = c(alpha_all,
                  stats::setNames(as.numeric(fixed_alpha)[match(fixed, names(fixed_alpha))],
                                  fixed))
  }
  pu$alpha = alpha_all[match(names(pu$k), names(alpha_all))]
  pu$alpha[is.na(pu$alpha)] = 0
  names(pu$alpha) = names(pu$k)
  pu$lambda = lambda; pu$n_iter = it_used; pu$converged = converged
  pu$joint = TRUE
  list(p = pa[seq_len(nmain)], alpha_i = as.numeric(Z %*% al) + off_vec,
       pool = pu, Xa = Xa, Aa = Aa, pa = pa, nz = nz, lambda = lambda,
       n_iter = it_used, converged = converged)
}

cpportal_gap_fit_shaped = function(formula, data, spec, weights = NULL,
                                   pool_alpha_override = NULL,
                                   pool_alpha_start = NULL,
                                   pool_lambda_start = NULL) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    stop("mgcv unavailable")
  }
  lhs = as.character(formula)[2]
  y = as.numeric(data[[lhs]])
  d = as.numeric(data[[spec$dist_var]])
  s = log(d + spec$offset)
  w_all = .gap_weights_vec(weights, nrow(data))
  ok = is.finite(y) & is.finite(s)
  if (!is.null(w_all)) ok = ok & is.finite(w_all) & w_all > 0
  data = data[ok, , drop = FALSE]; y = y[ok]; s = s[ok]
  w = if (is.null(w_all)) rep(1, length(y)) else w_all[ok]
  # Scale-free: pcls's penalty trades off against the WEIGHTED RSS, so the
  # smoothing parameter borrowed from the unconstrained gam below is only
  # comparable if the weights average to one. Normalising changes nothing about
  # the RELATIVE weighting (the point of the exercise) and keeps sigma^2 on the
  # response's scale.
  w = w / mean(w)
  n = length(y)
  if (n < 30L) stop("too few rows for a constrained fit (", n, ")")

  # Intercept DROPPED here: the unabsorbed cr smooth carries the level (see
  # .gap_par_mm). This is the re-parameterisation that makes the constrained
  # solve unique.
  fml_par = .gap_parametric_formula(formula, spec)
  par = .gap_par_mm(fml_par, data, drop_intercept = TRUE)
  Xp = par$X
  npar = ncol(Xp)

  # --- spline block (mgcv smoothCon, cr basis, pre-registered knots) --------
  # absorb.cons = FALSE: coefficients ARE the spline's values at the knots,
  # which is what mono.con() constrains. NOT column-centred -- the constant
  # function must stay in this block, and only in this block.
  dat_s = data.frame(.s = s)
  knots = spec$knots
  if (is.null(knots)) knots = seq(min(s), max(s), length.out = spec$k)
  sm = mgcv::smoothCon(mgcv::s(.s, k = spec$k, bs = spec$basis),
                       data = dat_s, knots = list(.s = knots),
                       absorb.cons = FALSE, scale.penalty = TRUE)[[1]]
  Xs = sm$X
  nsp = ncol(Xs)

  # --- rank pruning of the PARAMETRIC block (what lm() does with NA coefs) ---
  # Dropping the intercept is necessary but NOT sufficient. On the canonical gap
  # table the parametric block STILL spans the constant on its own:
  # factor(siting_class) is aliased with factor(metro_id) (the US metros are
  # entirely `network_default`, the European ones entirely
  # background/industrial/traffic), so rank(cbind(1, Xp)) == rank(Xp) == 20.
  # stats::lm() copes by pivoting the aliased column out and reporting NA for
  # it; pcls() has no such machinery and silently solves on the cancelling
  # manifold instead -- which is what kept max|coef| at ~1e12 after the
  # intercept was removed.
  # So: pivot the SMOOTH block FIRST (it must survive whole -- mono.con's
  # constraints are indexed against its 8 knot coefficients) and keep only the
  # parametric columns that are linearly independent given it. Identical column
  # space, unique solve, and the dropped names are recorded on the fit.
  keep_par = seq_len(npar)
  par_dropped = character(0)
  if (npar > 0L) {
    M = cbind(Xs, Xp)
    qrM = qr(M)
    if (qrM$rank < ncol(M)) {
      kept = sort(qrM$pivot[seq_len(qrM$rank)])
      if (!all(seq_len(nsp) %in% kept)) {
        stop("rank pruning would drop a SMOOTH basis column (the spline block ",
             "is itself rank deficient); mono.con constraints would be ",
             "mis-indexed")
      }
      keep_par = kept[kept > nsp] - nsp
      par_dropped = setdiff(colnames(Xp), colnames(Xp)[keep_par])
    }
  }
  Xp = Xp[, keep_par, drop = FALSE]
  npar = ncol(Xp)
  par_cols = colnames(Xp)
  X = cbind(Xp, Xs)
  # Hard invariant: the constrained design must be FULL COLUMN RANK. Anything
  # else means pcls() is picking an arbitrary point on a cancelling manifold.
  # Fail loudly (-> counted lm() fallback) rather than return a non-unique fit.
  if (qr(X)$rank < ncol(X)) {
    stop(sprintf(paste0("constrained design is rank deficient (rank %d < %d ",
                        "columns) after intercept drop and parametric rank ",
                        "pruning"), qr(X)$rank, ncol(X)))
  }

  # --- constraints ---------------------------------------------------------
  # (C1) monotone non-increasing: mono.con() on the cr knot sequence
  mc = mgcv::mono.con(sm$xp, up = FALSE)
  A1 = cbind(matrix(0, nrow(mc$A), npar), mc$A)
  b1 = mc$b
  # (C3) convexity variant: second differences of the knot values >= 0
  A3 = NULL; b3 = NULL
  if (isTRUE(spec$convex) && nsp >= 3L) {
    D2 = matrix(0, nsp - 2L, nsp)
    for (j in seq_len(nsp - 2L)) D2[j, j:(j + 2L)] = c(1, -2, 1)
    A3 = cbind(matrix(0, nrow(D2), npar), D2)
    b3 = rep(0, nrow(D2))
  }
  # (C2) class order cone: NOT BUILT (F6 registered call -- see header).
  Ain = rbind(A1, A3)
  bin = c(b1, b3)

  # --- feasible strictly-interior start ------------------------------------
  # pcls requires a STRICTLY feasible start. Knots are unevenly spaced (they
  # are out-of-zone quantiles), so mono.con's rows are not satisfied by a
  # constant per-coefficient decrement: build the start as a decreasing linear
  # function OF THE KNOT POSITIONS and escalate the slope until every row of
  # Ain is strictly slack.
  xp = as.numeric(sm$xp)
  rng = max(diff(range(xp)), 1e-6)
  ybar_w = stats::weighted.mean(y, w)
  sd_w = sqrt(stats::weighted.mean((y - ybar_w)^2, w))
  step0 = max(1e-3, sd_w / rng)
  p0 = rep(0, npar + nsp)
  feasible = FALSE
  for (it in 0:24) {
    step = step0 * 2^it
    p0[] = 0
    p0[npar + seq_len(nsp)] = ybar_w + (max(xp) - xp) * step
    if (isTRUE(spec$convex)) {
      # convex AND decreasing: quadratic in (max(xp) - xp)
      p0[npar + seq_len(nsp)] = ybar_w + ((max(xp) - xp) * step)^2 / rng
    }
    if (min(as.numeric(Ain %*% p0) - bin) > 1e-8) { feasible = TRUE; break }
  }
  if (!feasible) stop("could not build a strictly feasible start for pcls")

  # --- smoothing parameter: borrow the UNCONSTRAINED gam's sp (?pcls idiom) -
  # NO warm start here. Seeding mgcv's outer iteration via `in.out` was tried
  # and REVERTED (2026-08-04): it needs a `scale` seed as well as an `sp` seed,
  # and a wrong scale (var(y) is the wrong order of magnitude for a WEIGHTED
  # REML residual variance) walks the Newton search somewhere bad -- 1 replicate
  # in 100 came back with an sp that made pcls return NaN, and the seeded search
  # was a net SLOWDOWN besides (B=100: 101.6s seeded vs 83.2s cold).
  sp = tryCatch({
    gdat = data.frame(.y = y, .s = s, .w = w)
    g_un = mgcv::gam(.y ~ s(.s, k = spec$k, bs = spec$basis),
                     data = gdat, weights = .w,
                     knots = list(.s = knots), method = "REML")
    as.numeric(g_un$sp[1])
  }, error = function(e) 1)
  if (!is.finite(sp) || sp <= 0) sp = 1

  S = list(sm$S[[1]])

  # --- BACKFITTING: constrained QP  <->  pooled metro intercept -------------
  # The C1 cone is an INEQUALITY on the smooth's knot coefficients, so the mean
  # function must stay a `pcls` solve; lme4 cannot carry it. So alternate:
  #   (a) solve the constrained QP on the metro-ADJUSTED response y - alpha;
  #   (b) re-derive alpha in closed form from the resulting residuals
  #       (.gap_pool_update -- one-way random effects, BLUP shrinkage).
  # Each step is the exact minimiser of the same penalised weighted criterion
  # given the other block, so the sweep is monotone and converges in a handful
  # of iterations (observed: 3-5 to 1e-10). With pooling off this collapses to
  # the single unmodified pcls() solve the M7 arm has always done.
  #
  # G$w carries the weights into the QP itself (?pcls: min ||W^0.5(Xp-y)||^2 +
  # sum lambda_i p'S_i p). The constraint block Ain/bin is on the COEFFICIENTS,
  # so it is untouched by weighting -- C1 still binds h'(s) <= 0 everywhere.
  pool_on = !is.null(spec$pool_term) &&
    !is.null(spec$pool_var) && spec$pool_var %in% names(data)
  grp_v = if (pool_on) as.character(data[[spec$pool_var]]) else NULL
  clu_v = if (pool_on && !is.null(spec$pool_cluster) &&
              spec$pool_cluster %in% names(data))
    as.character(data[[spec$pool_cluster]]) else NULL
  min_don = spec$pool_min_donors %||% CPPORTAL_GAP_POOL_MIN_DONORS_DEFAULT
  pool = NULL
  alpha_i = rep(0, n)
  n_sweep = 0L
  p = NULL
  # RELATIVE tolerance on the response's own scale. An absolute 1e-10 makes the
  # sweep run to the iteration cap chasing float noise -- and the sweep is not
  # free: the bootstrap pays it B times on BOTH stages, so an unnecessary cap of
  # 25 would multiply the stage-2 refit cost by ~5x for no change in any
  # reported digit.
  # 1e-6 of the response SD. MEASURED on the real gap table: |dalpha| plateaus
  # at ~2.7e-8 -- float noise on alphas of magnitude 0.1-0.7, i.e. seven
  # significant figures -- so a 1e-7 threshold never tripped and the
  # non-convergence warning below fired on EVERY fit (thousands of times across
  # a 400-replicate two-stage bootstrap). 1e-6 stops one or two sweeps earlier
  # at a difference of <1e-6 in alpha, immaterial to every reported digit, and
  # keeps the warning meaningful when it does fire.
  pool_tol = 1e-6 * max(sqrt(stats::weighted.mean((y - stats::weighted.mean(y, w))^2, w)), 1e-8)
  pool_maxit = 12L
  # WARM START. The fixed point does not depend on the starting alpha; the
  # number of sweeps to reach it does. A bootstrap replicate starts from the
  # headline alpha (see `pool_alpha_start`), which is within one bootstrap SE of
  # its own answer.
  if (pool_on && !is.null(pool_alpha_start) && length(pool_alpha_start)) {
    h0 = match(grp_v, names(pool_alpha_start))
    a0 = as.numeric(pool_alpha_start)[h0]; a0[is.na(a0)] = 0
    alpha_i = a0
  }
  # The (metro, monitor) grouping is constant for the life of this fit.
  pool_codes = if (pool_on) .gap_pool_codes(grp_v, clu_v, n) else NULL
  # AITKEN ACCELERATION. MEASURED on the monitor-MONTH stage: the sweep is
  # LINEARLY convergent with ratio rho ~= 0.68 (deltas 6.3e-1, 2.2e-1, 1.3e-1,
  # 8.3e-2, ... 3.8e-3 at sweep 12), so a 12-sweep cap does NOT reach the fixed
  # point -- it stops ~1.2e-2 short of it and the non-convergence warning fires
  # on every fit. Because the error contracts by a near-constant factor, three
  # successive iterates identify rho and the limit in closed form:
  #   a* ~= a_n + rho/(1-rho) * (a_n - a_{n-1}).
  # That turns a linear iteration into a near-quadratic one: the same fixed
  # point, reached in ~4 sweeps instead of >30, and the bootstrap pays the pcls
  # solve 4 times per stage per replicate instead of 12.
  d_prev = NULL
  # --- JOINT PATH (default): one constrained solve, no coefficient sweep ------
  joint = NULL
  if (pool_on && cpportal_gap_pool_joint()) {
    joint = .gap_pool_joint_fit(X, y, w, Ain, bin, p0, S, sp, npar, nsp,
                                grp_v, clu_v, min_don, pool_codes, n,
                                fixed_alpha = pool_alpha_override,
                                lambda_start = pool_lambda_start)
    if (!is.null(joint)) {
      p = joint$p; alpha_i = joint$alpha_i; pool = joint$pool
      n_sweep = joint$n_iter
      if (!joint$converged) {
        warning(sprintf(paste0("[M9-shape] pooled variance ratio lambda did NOT ",
                "converge in %d outer steps; the fit is the last step"), 20L),
                call. = FALSE, immediate. = TRUE)
      }
    } else {
      # The joint solve is not a silent option: if it cannot be formed (no free
      # metros, unidentified tau^2, non-finite QP) fall through to the backfit
      # rather than to a fabricated level.
      .cpportal_gap_fallback_note("joint pooled solve failed; backfit used",
                                  "gap_pool_joint")
    }
  }
  for (sweep in seq_len(if (pool_on && is.null(joint)) pool_maxit else 0L)) {
    n_sweep = sweep
    # pcls needs a STRICTLY feasible start. The previous sweep's solution is
    # feasible but may sit ON the cone's boundary, so nudge it toward the
    # strictly-interior p0: slack is affine in p, so the convex combination has
    # slack >= t * slack(p0) > 0.
    p_qp = if (is.null(p) || !all(is.finite(p))) p0 else 0.99 * p + 0.01 * p0
    G = list(X = X, p = p_qp, y = y - alpha_i, w = w,
             Ain = Ain, bin = bin, C = matrix(0, 0, 0),
             S = S, off = npar, sp = sp)
    p = mgcv::pcls(G)
    # A warm start is an optimisation, never a licence to fail: if the QP came
    # back non-finite from the previous sweep's solution, re-solve from the
    # cold, strictly-interior p0 before treating the replicate as degenerate.
    if (!all(is.finite(p)) && !identical(p_qp, p0)) {
      G$p = p0
      p = mgcv::pcls(G)
    }
    if (!pool_on || !all(is.finite(p))) break
    pu = .gap_pool_update(y - as.numeric(X %*% p), w, grp_v, clu_v, min_don,
                          codes = pool_codes)
    if (is.null(pu)) {
      # Not enough structure to identify tau^2 (fewer than two adequate metros,
      # or no metro with >= 2 donors). Degrade to NO metro adjustment rather
      # than to a fabricated one, and say so on the fit.
      pool = NULL; alpha_i[] = 0; pool_on = FALSE
      p = mgcv::pcls(list(X = X, p = p0, y = y, w = w, Ain = Ain, bin = bin,
                          C = matrix(0, 0, 0), S = S, off = npar, sp = sp))
      break
    }
    # HYBRID BOOTSTRAP hook: a replicate may supply a DRAWN alpha for the
    # small-k metros (whose rows are held fixed, so the resample carries no
    # information about them). Those values are imposed, not re-estimated;
    # every other metro still updates from the resampled residuals.
    if (!is.null(pool_alpha_override) && length(pool_alpha_override)) {
      hit = match(names(pu$alpha), names(pool_alpha_override))
      has = !is.na(hit)
      if (any(has)) pu$alpha[has] = as.numeric(pool_alpha_override)[hit[has]]
    }
    a_raw = .gap_pool_alpha(pu, grp_v)$alpha
    d_cur = a_raw - alpha_i
    delta = max(abs(d_cur))
    pool = pu
    if (delta < pool_tol) { alpha_i = a_raw; break }
    a_new = a_raw
    if (!is.null(d_prev)) {
      den = sum(d_prev * d_prev)
      rho = if (den > 0) sum(d_cur * d_prev) / den else NA_real_
      # RESTART after every extrapolation (Steffensen's pattern): the increment
      # that FOLLOWS an extrapolated step is not a step of the plain linear
      # iteration, so using it to estimate rho poisons the next extrapolation
      # (observed: delta jumping back up an order of magnitude at sweep 9).
      d_prev = NULL
      if (is.finite(rho) && rho > 0.05 && rho < 0.995) {
        a_new = a_raw + (rho / (1 - rho)) * d_cur
        # An overridden metro's level is IMPOSED by the replicate's draw, not
        # estimated, so it must never be extrapolated away from that draw.
        if (!is.null(pool_alpha_override) && length(pool_alpha_override)) {
          hit = match(grp_v, names(pool_alpha_override))
          has = !is.na(hit)
          if (any(has)) a_new[has] = as.numeric(pool_alpha_override)[hit[has]]
        }
      }
    }
    if (identical(a_new, a_raw)) d_prev = d_cur
    alpha_i = a_new
  }
  if (is.null(p)) {
    # pooling off, or the joint solve declined: the single unmodified pcls solve
    p = mgcv::pcls(list(X = X, p = p0, y = y - alpha_i, w = w, Ain = Ain,
                        bin = bin, C = matrix(0, 0, 0), S = S, off = npar,
                        sp = sp))
    n_sweep = max(n_sweep, 1L)
  }
  if (pool_on && is.null(joint) && n_sweep >= pool_maxit) {
    warning(sprintf(paste0("[M9-shape] pooled metro intercept did NOT converge ",
            "in %d backfit sweeps (last |dalpha| above %.3g); the fit is the ",
            "last sweep, not a fixed point"), pool_maxit, pool_tol),
            call. = FALSE, immediate. = TRUE)
  }
  if (!is.null(pool)) {
    pool$override = pool_alpha_override
    pool$n_sweep = n_sweep
  }

  # --- F7: numerical-degeneracy detector -----------------------------------
  # The rank deficiency this detector was built for (round-2 blocker: separate
  # intercept + column-centred spline => cancelling manifold, max|coef| ~ 1e12
  # on every fold, |pred| ~ 1e82 out of sample on fold 3) is FIXED above by the
  # ?pcls re-parameterisation. The instrumentation STAYS as a standing tripwire:
  # it must fire ZERO times on the canonical gap table, and any future basis /
  # parametric change that re-creates a near-singular design gets caught loudly
  # (warn) or converted into a counted lm() fallback (error) instead of
  # returning a silently-garbage fit.
  y_scale = max(sd_w, abs(ybar_w), 1e-8)
  coef_scale = max(abs(p))
  degenerate = !all(is.finite(p)) || coef_scale > 1e6 * y_scale
  if (!all(is.finite(p)) || coef_scale > 1e20 * y_scale) {
    stop(sprintf(paste0("constrained solution is numerically degenerate ",
                        "(max|coef| = %.3g vs response scale %.3g); the ",
                        "[intercept | centred spline] design is rank ",
                        "deficient"), coef_scale, y_scale))
  }
  if (degenerate) {
    warning(sprintf(paste0("[M9-shape] ILL-CONDITIONED fit: max|coef| = %.3g ",
                           "vs response scale %.3g. In-sample fit is exact but ",
                           "the coefficient vector is NOT unique (rank-deficient ",
                           "[intercept | centred spline] design). Treat ",
                           "out-of-sample predictions and the plug-in SE with ",
                           "suspicion."), coef_scale, y_scale),
            call. = FALSE, immediate. = TRUE)
  }

  fit = as.numeric(X %*% p) + alpha_i
  resid = y - fit
  # WEIGHTED residual sum of squares -- the quantity pcls actually minimised.
  rss = sum(w * resid^2)
  # active set at the optimum -> effective df and the restricted covariance
  slack = as.numeric(Ain %*% p) - bin
  # tolerance scaled by the SLACK scale (the constraint rows are differences of
  # knot values, so their natural scale is the response's, not |p|'s).
  active = which(slack < 1e-6 * max(1, max(abs(slack))))
  # The pooled intercepts cost EFFECTIVE, not integer, df: a shrunken level
  # spends k_m tau^2 / (k_m tau^2 + sigma^2) of a parameter, which is ~1 for a
  # well-donated metro and ~0 for a one-donor one. Counting them as full dummies
  # (the FE convention) would over-penalise exactly the thin metros this change
  # exists to protect.
  # EFFECTIVE df. On the backfit path the pooled levels' df was the closed-form
  # shrinkage sum. On the joint path the ridge block is IN the penalised design,
  # so the honest quantity is the trace of the penalised hat matrix restricted
  # to the metro columns -- same idea, but it now accounts for the metro block
  # competing with the smooth and the parametric block rather than assuming it
  # is orthogonal to them.
  if (is.null(joint)) {
    edf_pool = if (is.null(pool)) 0 else
      sum(pool$k * pool$tau2 / (pool$k * pool$tau2 + pool$sigma2))
    edf_raw = ncol(X) - length(active) + edf_pool
  } else {
    Pen0 = matrix(0, ncol(joint$Xa), ncol(joint$Xa))
    Pen0[npar + seq_len(nsp), npar + seq_len(nsp)] = sp * S[[1]]
    zi0 = npar + nsp + seq_len(joint$nz)
    diag(Pen0)[zi0] = diag(Pen0)[zi0] + joint$lambda
    XtWX0 = crossprod(joint$Xa * sqrt(w))
    H = tryCatch(solve(XtWX0 + Pen0, XtWX0),
                 error = function(e) MASS_ginv(XtWX0 + Pen0) %*% XtWX0)
    edf_pool_joint = sum(diag(H)[zi0])
    # REPAIR 2 (lambda-uncertainty work, 2026-08-04). The joint trace is taken
    # AT THE CONVERGED lambda, i.e. it treats a quantity estimated from the same
    # data as known. That buys spurious residual df exactly where the metro
    # block is thin: measured edf_pool 18.081 (backfit) -> 16.067 (joint), which
    # lowers sigma2 = rss/df_res and therefore BOTH V and nu2 -- the mechanism
    # behind the -19.3% SE drop on metro 950 and the 14/35 -> 29/35 G-S6 blowup.
    # The PR claims "same estimator, different solver", so hold the backfit's
    # closed-form edf_pool convention and keep sigma2 comparable.
    edf_pool_cf = if (is.null(pool)) NA_real_ else
      sum(pool$k * pool$tau2 / (pool$k * pool$tau2 + pool$sigma2))
    edf_pool = if (cpportal_gap_joint_vcorr() && is.finite(edf_pool_cf))
      edf_pool_cf else edf_pool_joint
    edf_raw = sum(diag(H)) - length(active) - edf_pool_joint + edf_pool
  }
  df_res = max(n - edf_raw, 1)
  sigma2 = rss / df_res

  # COVARIANCE. On the joint path the design is [X | Z] and the penalty carries
  # BOTH blocks -- sp*S on the smooth and lambda*I on the metro levels. Ignoring
  # the ridge block here would treat the pooled intercepts as if they had been
  # estimated freely and overstate everything downstream of it.
  Xc = if (is.null(joint)) X else joint$Xa
  Ac = if (is.null(joint)) Ain else joint$Aa
  Pen = matrix(0, ncol(Xc), ncol(Xc))
  Pen[npar + seq_len(nsp), npar + seq_len(nsp)] = sp * S[[1]]
  if (!is.null(joint)) {
    zi = npar + nsp + seq_len(joint$nz)
    diag(Pen)[zi] = diag(Pen)[zi] + joint$lambda
  }
  # X'WX, not X'X: the covariance of a weighted least-squares solve.
  XtWX = crossprod(Xc * sqrt(w))
  XtX = XtWX + Pen
  M = tryCatch(solve(XtX), error = function(e) MASS_ginv(XtX))
  V = sigma2 * M
  # REPAIR 1 (2026-08-04). sigma2 * M is mgcv's Vp -- the Bayesian covariance
  # CONDITIONAL ON the smoothing/ridge parameters. On the joint path lambda is
  # not fixed a priori: it is estimated by fixed-point iteration on the same
  # data (lam_new = sigma2/tau2), so conditioning on it discards a real source
  # of uncertainty. That is the objection that stopped this PR.
  #
  # Wood/Pya/Safken's Vc needs a REML Hessian for lambda, which a constrained
  # pcls fit does not produce. First-order delta method instead, which is
  # closed-form and costs nothing:
  #     beta(lambda) = (X'WX + S + lambda*J)^-1 X'Wy
  #     d beta / d log lambda = -lambda * M %*% (J %*% beta)
  #     Var_lambda(beta) ~= Var(log lambda_hat) * g g',   g = lambda * M J beta
  # (valid at a fixed active set, where the constrained solve is linear).
  #
  # Var(log lambda_hat): lambda = sigma2 / tau2, so
  #     Var(log lambda) = Var(log sigma2) + Var(log tau2)
  #                     ~= 2/df_res + 2/(nz - 1)
  # -- chi-square plug-ins for the two variance components. The metro term
  # dominates whenever the pool is thin, which is precisely where the
  # conditional SE was most anti-conservative.
  if (!is.null(joint) && cpportal_gap_joint_vcorr()) {
    zi_v = npar + nsp + seq_len(joint$nz)
    Jb = numeric(ncol(Xc))
    Jb[zi_v] = as.numeric(joint$pa)[zi_v]
    g = as.numeric(joint$lambda * (M %*% Jb))
    var_log_lambda = 2 / max(df_res, 1) + 2 / max(joint$nz - 1, 1)
    if (all(is.finite(g)) && is.finite(var_log_lambda)) {
      V = V + var_log_lambda * tcrossprod(g)
      message(sprintf(paste0("[M9-shape] joint V: lambda-uncertainty term ON ",
                             "(lambda=%.4g, nz=%d, var_log_lambda=%.4g, ",
                             "mean inflation of sqrt(diag V) = %.1f%%)"),
                      joint$lambda, joint$nz, var_log_lambda,
                      100 * (mean(sqrt(diag(V)) /
                             pmax(sqrt(diag(sigma2 * M)), 1e-12)) - 1)))
    }
  }
  if (length(active)) {
    Aa = Ac[active, , drop = FALSE]
    AVA = Aa %*% V %*% t(Aa)
    Ainv = tryCatch(solve(AVA), error = function(e) MASS_ginv(AVA))
    V = V - V %*% t(Aa) %*% Ainv %*% Aa %*% V
  }
  # Downstream consumers score `.gap_shape_design(fit) %*% coef + alpha`, so the
  # vcov they need is the MAIN block's -- marginal over the metro levels, which
  # is what the corresponding sub-matrix of the joint V already is.
  V_full = V
  if (!is.null(joint)) V = V[seq_len(npar + nsp), seq_len(npar + nsp), drop = FALSE]

  tss = sum(w * (y - ybar_w)^2)
  # NAME the coefficients ([parametric | spline knots]) — pcls returns a bare
  # vector, and consumers that read a single term off the fit by name (e.g. the
  # siting-class premiums the appendix cites) would otherwise get NA and not
  # know why.
  names(p) = c(par_cols, sprintf("h(%s).knot%02d", spec$dist_var, seq_len(nsp)))
  structure(list(
    coefficients = p,
    fitted.values = fit,
    residuals = resid,
    sigma = sqrt(sigma2),
    r.squared = 1 - rss / tss,
    df.residual = df_res,
    vcov = V,
    spec = spec, sm = sm, knots = knots, sp = sp,
    # Partially pooled metro intercept (NULL when pooling is off / unidentified)
    pool = pool, pool_alpha_i = alpha_i, edf_pool = edf_pool,
    pool_joint = !is.null(joint),
    pool_lambda = if (is.null(joint)) NULL else joint$lambda,
    vcov_full = if (is.null(joint)) NULL else V_full,
    # NO xs_center: the spline block is deliberately UNCENTRED (it carries the
    # level). A non-NULL xs_center here would be the round-2 defect returning.
    xs_center = NULL, drop_intercept = TRUE,
    par_cols = par_cols, par_dropped = par_dropped,
    npar = npar, nsp = nsp,
    weights = w, weighted = !is.null(w_all),
    # The fit's OWN shape state. Reaching this line means the constrained solve
    # ran, whether it was the env flag or an explicit `shape = TRUE` that chose
    # it. predict() reads this instead of re-consulting the environment, so an
    # explicit-TRUE fit still gets the bootstrap default when the flag is off.
    shape_enabled = TRUE,
    # Mutable slot so a bootstrap run at predict() time can record failed
    # replicates back onto the fit the caller is holding (R copies the list,
    # but an environment is a reference).
    boot_state = local({ e = new.env(parent = emptyenv())
                         e$n_boot_failed = 0L; e$boot_B = 0L; e }),
    terms_par = par$terms, xlevels = par$xlevels,
    fml_par = fml_par, fml_par_pred = .gap_parametric_formula_rhs(spec),
    formula = formula,
    Ain = Ain, bin = bin, active = active,
    n_active = length(active),
    active_labels = .gap_active_labels(active, nrow(A1)),
    active_slack = slack[active], min_slack = min(slack),
    # WHERE the C1 cone binds, on the km axis the design speaks in.
    active_dist_km = .gap_active_dist(active, nrow(A1), xp, spec$offset),
    n_fallback = cpportal_gap_fallback_count(),
    coef_scale = coef_scale, ill_conditioned = degenerate,
    train = data, lhs = lhs
  ), class = "cpportal_gap_shape_fit")
}

# mono.con() emits exactly FOUR rows per knot interval (mgcv builds sufficient
# conditions for monotonicity of the cubic ON the interval, not just at the
# knots), so constraint row r belongs to interval ceiling(r/4) = [xp_j, xp_{j+1}].
# Map that back to km so "the cone binds" is a statement about distance to road,
# not about an opaque row index. Returns NULL when the row count does not match
# the 4-per-interval layout (defensive: mono.con gains rows if lower/upper are
# ever supplied).
.gap_active_dist = function(active, n1, xp, offset) {
  nk = length(xp)
  if (!length(active) || n1 != 4L * (nk - 1L)) return(NULL)
  rows = active[active <= n1]
  if (!length(rows)) return(NULL)
  j = sort(unique(as.integer(ceiling(rows / 4))))
  data.frame(interval = j,
             lo_km = exp(xp[j]) - offset,
             hi_km = exp(xp[j + 1L]) - offset,
             stringsAsFactors = FALSE)
}

# Rows 1..n1 are (C1) monotone; anything after is (C3) convex. No C2 block.
.gap_active_labels = function(active, n1) {
  if (!length(active)) return(character(0))
  vapply(active, function(a) {
    if (a <= n1) sprintf("C1_monotone[%d]", a) else sprintf("C3_convex[%d]", a - n1)
  }, character(1))
}

MASS_ginv = function(A) {
  sv = svd(A)
  pos = sv$d > max(sv$d) * 1e-10
  sv$v[, pos, drop = FALSE] %*% (t(sv$u[, pos, drop = FALSE]) / sv$d[pos])
}

# --- prediction ------------------------------------------------------------

.gap_shape_design = function(object, newdata) {
  fml_pred = if (is.null(object$fml_par_pred))
    .gap_parametric_formula_rhs(object$spec) else object$fml_par_pred
  # Must mirror the FIT parameterisation exactly: intercept dropped, spline
  # block uncentred. `drop_intercept` defaults TRUE for fits from this version;
  # the isTRUE()/is.null() pair keeps an older pinned fit (which carried an
  # xs_center) scoring on its own parameterisation rather than silently mixing.
  drop_int = if (is.null(object$drop_intercept)) is.null(object$xs_center)
             else isTRUE(object$drop_intercept)
  par = .gap_par_mm(fml_pred, newdata, xlev = object$xlevels,
                    drop_intercept = drop_int)
  Xpar = par$X
  # Score on EXACTLY the parametric columns the fit kept after rank pruning
  # (select by name -- positional selection would silently shift if a level is
  # absent from newdata).
  if (!is.null(object$par_cols)) {
    miss = setdiff(object$par_cols, colnames(Xpar))
    if (length(miss)) {
      stop("newdata is missing parametric column(s) the fit uses: ",
           paste(miss, collapse = ", "))
    }
    Xpar = Xpar[, object$par_cols, drop = FALSE]
  }
  s = log(as.numeric(newdata[[object$spec$dist_var]]) + object$spec$offset)
  Xs = mgcv::PredictMat(object$sm, data.frame(.s = s))
  if (!is.null(object$xs_center)) Xs = sweep(Xs, 2L, object$xs_center, "-")
  cbind(Xpar, Xs)
}

# alpha / nu^2 aligned to the ROWS of newdata (0 / 0 when pooling is off).
.gap_shape_pool_terms = function(object, newdata, override = NULL) {
  pool = object$pool
  if (is.null(pool)) return(list(alpha = 0, nu2 = 0))
  pv = object$spec$pool_var %||% CPPORTAL_GAP_POOL_VAR
  if (!(pv %in% names(newdata))) {
    stop("newdata is missing the pooling variable '", pv,
         "' the fit's partially pooled metro intercept needs")
  }
  .gap_pool_alpha(pool, newdata[[pv]], override = override)
}

#' @export
predict.cpportal_gap_shape_fit = function(object, newdata = NULL,
                                          se.fit = FALSE, ...) {
  if (is.null(newdata)) {
    fit = object$fitted.values
    if (!se.fit) return(fit)
    newdata = object$train
  }
  X = .gap_shape_design(object, newdata)
  # Pooled metro intercept. A metro absent from training scores at alpha = 0
  # (the grand mean) with nu^2 = tau^2 -- it is PREDICTED, not dropped, which is
  # the whole point for a zero-donor metro.
  pa = .gap_shape_pool_terms(object, newdata)
  fit = as.numeric(X %*% object$coefficients) + pa$alpha
  if (!se.fit) return(fit)
  # nu_m^2 is the PREDICTION variance of the metro level (tau^2 sigma^2 /
  # (k tau^2 + sigma^2)); it is independent of the mean-function coefficients,
  # so the two variances add.
  se_plugin = sqrt(pmax(rowSums((X %*% object$vcov) * X) + pa$nu2, 0))
  se = se_plugin
  se_method = "plugin_active_set"
  gs6_violation = FALSE
  n_gs6_violation = 0L
  # Pass the FIT's shape state, not the environment's: a fit built with an
  # explicit `shape = TRUE` while CPPORTAL_FECT_M9_SHAPE is unset would
  # otherwise silently get B = 0 and report the plug-in SE as headline.
  B = cpportal_gap_shape_boot_B(enabled = object$shape_enabled %||% TRUE)
  n_boot_failed = 0L
  n_boot_ok = 0L
  n_stage2_used = 0L
  two_stage = FALSE
  n_row_boot = 0L
  n_row_plugin = length(se_plugin)
  boot_min_rep = NA_integer_
  boot_fallback_groups = character(0)
  if (B > 0L) {
    se_boot = tryCatch(.gap_shape_boot_se(object, newdata, B),
                       error = function(e) {
                         warning(sprintf(paste0("[M9-shape] bootstrap ABORTED ",
                                 "(%s); falling back to the plug-in ",
                                 "cross-check SE"), conditionMessage(e)),
                                 call. = FALSE, immediate. = TRUE)
                         NULL
                       })
    n_boot_failed = if (is.null(se_boot)) B else
      as.integer(attr(se_boot, "n_boot_failed") %||% 0L)
    n_boot_ok = if (is.null(se_boot)) 0L else
      as.integer(attr(se_boot, "n_boot_ok") %||% 0L)
    n_stage2_used = if (is.null(se_boot)) 0L else
      as.integer(attr(se_boot, "n_stage2_used") %||% 0L)
    two_stage = !is.null(se_boot) && isTRUE(attr(se_boot, "two_stage"))
    # An all-NA bootstrap is NOT an SE. Keep the plug-in and say so.
    if (!is.null(se_boot) && !any(is.finite(se_boot))) se_boot = NULL
    if (!is.null(se_boot)) {
      # PARTIAL bootstrap: rows the bootstrap could not cover with enough
      # replicates (see the minimum-replicate rule in .gap_shape_boot_se) come
      # back NA and keep the plug-in. That is a per-ROW mix of two SE methods,
      # so it is named in `se_method` and counted in the return value -- a
      # silently partial SE is the failure mode this whole path guards against.
      use_boot = is.finite(se_boot)
      n_row_boot = sum(use_boot)
      n_row_plugin = sum(!use_boot)
      fb_grp = attr(se_boot, "fallback_groups") %||% character(0)
      boot_min_rep = as.integer(attr(se_boot, "min_rep") %||% NA_integer_)
      boot_fallback_groups = fb_grp
      if (n_row_plugin > 0L) {
        warning(sprintf(paste0("[M9-shape] MIXED SE: %d of %d row(s) carry the ",
                               "monitor bootstrap SE (B=%d); the remaining %d ",
                               "fall back to the plug-in active-set SE%s"),
                        n_row_boot, length(se_boot), B, n_row_plugin,
                        if (length(fb_grp))
                          paste0(" (metro_id: ", paste(fb_grp, collapse = ", "), ")")
                        else ""),
                call. = FALSE, immediate. = TRUE)
      }
      # G-S6 (section 33b, pre-registered): se_boot >= se_plugin, because a
      # boundary-constrained plug-in UNDERSTATES. F5: when that ordering is
      # VIOLATED the old code silently pmax()ed it away, which converts a
      # failed pre-registered check into a quiet SE substitution. Report it.
      bad = is.finite(se_boot) & is.finite(se_plugin) & (se_boot < se_plugin)
      n_gs6_violation = sum(bad)
      if (n_gs6_violation > 0L) {
        gs6_violation = TRUE
        warning(sprintf(paste0("[M9-shape] G-S6 VIOLATION: bootstrap SE (B=%d) ",
                               "< plug-in SE for %d of %d prediction rows ",
                               "(min ratio %.3f). The bootstrap is still the ",
                               "reported SE; the pre-registered ordering did ",
                               "NOT hold and the gate must record this."),
                       B, n_gs6_violation, length(se_boot),
                       min(se_boot[bad] / se_plugin[bad])),
                call. = FALSE, immediate. = TRUE)
      }
      # The bootstrap IS the registered gate-arm SE -- report it as estimated,
      # not silently floored at the cross-check value.
      se = as.numeric(se_plugin)
      se[use_boot] = as.numeric(se_boot)[use_boot]
      se_method = sprintf("monitor_bootstrap%s_B%d%s",
                          if (two_stage) "_two_stage" else "", B,
                          if (n_row_plugin > 0L)
                            sprintf("_PLUGIN_ROWS%d", n_row_plugin) else "")
    } else if (B > 0L) {
      se_method = sprintf("plugin_active_set_BOOT_FAILED_B%d", B)
    }
  }
  list(fit = fit, se.fit = as.numeric(se),
       se_plugin = as.numeric(se_plugin), se_method = se_method,
       boot_B = B, n_boot_failed = as.integer(n_boot_failed),
       n_boot_ok = as.integer(n_boot_ok),
       boot_two_stage = two_stage,
       n_boot_stage2 = as.integer(n_stage2_used),
       # Per-ROW SE provenance. n_row_boot + n_row_plugin == nrow(newdata).
       n_row_boot = as.integer(n_row_boot),
       n_row_plugin = as.integer(n_row_plugin),
       boot_min_rep = boot_min_rep,
       boot_fallback_groups = boot_fallback_groups,
       gs6_violation = gs6_violation,
       n_gs6_violation = as.integer(n_gs6_violation),
       n_fallback = object$n_fallback %||% cpportal_gap_fallback_count(),
       df = object$df.residual, residual.scale = object$sigma)
}

# Monitor-level nonparametric bootstrap, resampling monitors WITHIN metro,
# full constrained refit per replicate (design headline: B = 400).
#
# FAILED REPLICATES ARE NOT SILENT. A replicate whose constrained refit errors
# (or whose scoring errors) used to `next` away, leaving an all-NA column that
# sd(na.rm = TRUE) quietly ignored -- so a bootstrap that mostly failed reported
# an SE off however few replicates survived, with no trace. Now: count them,
# stamp the count on the fit's mutable `boot_state`, warn once, and return NA
# (not a fake SE) if too few replicates survived to compute one.
#' Attach the monitor-MONTH refinement to a base fit as bootstrap STAGE 2.
#'
#' M9-r3 (Tim's standing directive 2026-07-31). The reported gap point
#' prediction is `coalesce(monthly, monitor-level)`, so an SE that resamples
#' only the monitor-level stage understates: it holds the refinement fixed at
#' its point estimate, as if the monthly coefficients were known. The cluster
#' bootstrap now resamples MONITORS ONCE and refits BOTH stages on that
#' resample -- the two stages share the same monitors, so their errors are
#' dependent and must be drawn jointly, not convolved.
#'
#' @param stage2 list(train, formula, spec, weight_fn, newdata, key). `train`
#'   and `newdata` are monitor-MONTH frames; `weight_fn(df)` REBUILDS the
#'   weights on the resampled frame (1/m_i must be recomputed -- a bootstrap
#'   draw changes every monitor's month count).
cpportal_gap_attach_stage2 = function(fit, stage2) {
  if (!inherits(fit, "cpportal_gap_shape_fit")) return(fit)
  need = c("train", "formula", "spec", "newdata", "key")
  if (!all(need %in% names(stage2))) {
    stop("stage2 must carry: ", paste(need, collapse = ", "))
  }
  fit$stage2 = stage2
  fit
}

#' How many cores may the gap bootstrap use?
#'
#' Default: half the detected cores, capped at 8 -- the bootstrap runs INSIDE a
#' fect training job that already has its own parallelism, so taking the whole
#' machine would oversubscribe. 0 or 1 forces the serial path.
cpportal_gap_boot_cores = function(B = Inf) {
  raw = trimws(Sys.getenv("CPPORTAL_FECT_GAP_BOOT_CORES", ""))
  n = if (nzchar(raw)) suppressWarnings(as.integer(raw)) else NA_integer_
  if (is.na(n)) {
    dc = tryCatch(parallel::detectCores(logical = FALSE), error = function(e) 1L)
    if (!is.finite(dc) || dc < 1L) dc = 1L
    n = min(8L, max(1L, dc %/% 2L))
  }
  if (n < 1L) n = 1L
  # Below ~16 replicates the PSOCK setup costs more than it saves.
  if (B < 16L) n = 1L
  as.integer(n)
}

# Rows of `newdata` a given replicate fit CAN score: every factor level the
# replicate's model frame carries must cover the row's value. Same idea as the
# `ok_row` block in diagnostics/validate_m7_from_panel.R's
# lm_cv_leave_monitors_out(): read the fitted object's `xlevels` and drop the
# scoring rows the fold never saw, instead of letting model.frame() raise
# "factor has new levels" and take the whole replicate down with it.
.gap_shape_boot_scorable = function(fit, newdata) {
  ok = rep(TRUE, nrow(newdata))
  xl = fit$xlevels
  if (!length(xl)) return(ok)
  for (lv in names(xl)) {
    # `.getXlevels()` keys are TERM LABELS ("factor(metro_id)"), but a column
    # that is already a factor appears under its bare name. Try both.
    v = if (lv %in% names(newdata)) lv else sub("^factor\\((.*)\\)$", "\\1", lv)
    if (!(v %in% names(newdata))) next
    ok = ok & (as.character(newdata[[v]]) %in% xl[[lv]])
  }
  ok
}

# ONE bootstrap replicate: returns the prediction vector, or a character reason.
# TOP LEVEL on purpose -- a PSOCK worker calls it by name out of its own global
# environment, so nothing from the master's call frame rides along with the task.
.gap_boot_one = function(dr, ctx) {
  take = dr$take; draw = dr$draw
  d = ctx$d; newdata = ctx$newdata; s2 = ctx$s2
  fb = tryCatch(suppressWarnings(
         cpportal_gap_fit_shaped(ctx$formula, d[take, , drop = FALSE], ctx$spec,
                                 weights = if (is.null(ctx$w_base)) NULL else ctx$w_base[take],
                                 pool_alpha_override = draw,
                                 pool_alpha_start = ctx$a_start1,
                                 pool_lambda_start = ctx$lam_start1)),
                error = function(e) paste0("stage1: ", conditionMessage(e)))
  if (is.character(fb)) return(fb)
  # Score only what this replicate CAN score. A level it never saw is a
  # per-ROW limitation, not a reason to throw the other ~thousand rows away.
  sc = .gap_shape_boot_scorable(fb, newdata)
  if (!any(sc)) return("stage1_score: replicate carries no scorable rows")
  pb = rep(NA_real_, nrow(newdata))
  psc = tryCatch({
         nd = newdata[sc, , drop = FALSE]
         # pooled/hybrid metro intercept rides on top of the fixed part
         ab = .gap_shape_pool_terms(fb, nd, override = draw)$alpha
         as.numeric(.gap_shape_design(fb, nd) %*% fb$coefficients) + ab
       },
                error = function(e) paste0("stage1_score: ", conditionMessage(e)))
  if (is.character(psc)) return(psc)
  if (!any(is.finite(psc))) return("stage1_score: no finite predictions")
  pb[sc] = psc
  # A partial replicate is REPORTED, not silently averaged in: the collector
  # counts it and the per-row replicate tally decides which rows keep a
  # bootstrap SE at all.
  attr(pb, "partial") = !all(sc)
  attr(pb, "n_unscorable") = sum(!sc)
  if (!is.null(s2)) {
    pm = tryCatch(suppressWarnings(
           .gap_boot_stage2(s2, as.character(d[[s2$key]][take]), draw,
                            pool_start = ctx$a_start2,
                            lambda_start = ctx$lam_start2)),
                  error = function(e) paste0("stage2: ", conditionMessage(e)))
    # A stage-2 failure is a FAILED REPLICATE, not a quiet demotion to the
    # stage-1-only prediction: mixing the two across replicates would give an
    # SE for an estimator nobody runs.
    if (is.character(pm)) return(pm)
    hit = match(as.character(newdata[[s2$key]]), names(pm))
    ov = as.numeric(pm)[hit]
    # The reported gap point prediction IS coalesce(monthly, monitor-level), so
    # a row the monthly stage covers is fully reproduced by this replicate even
    # if stage 1 could not score it.
    use = is.finite(ov)
    a_part = attr(pb, "partial"); a_uns = attr(pb, "n_unscorable")
    pb[use] = ov[use]
    attr(pb, "partial") = a_part; attr(pb, "n_unscorable") = a_uns
    attr(pb, "stage2") = TRUE
  }
  pb
}

# Run B replicates over the pre-drawn `draws`, in parallel when it pays. The
# RESULT is index-aligned with `draws` and independent of the core count -- the
# draws were made serially before this is called, so the parallel and the serial
# path see exactly the same resamples.
.gap_boot_run = function(draws, ctx, B) {
  serial = function() lapply(draws, function(dr)
    tryCatch(.gap_boot_one(dr, ctx),
             error = function(e) paste0("replicate: ", conditionMessage(e))))
  nc = cpportal_gap_boot_cores(B)
  if (nc <= 1L || !requireNamespace("parallel", quietly = TRUE)) return(serial())
  src = .gap_shape_source_path()
  if (is.na(src)) return(serial())
  cl = tryCatch(parallel::makePSOCKcluster(nc), error = function(e) NULL)
  if (is.null(cl)) return(serial())
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  ok = tryCatch({
    parallel::clusterCall(cl, function(p) {
      suppressWarnings(suppressMessages(source(p, local = FALSE))); TRUE
    }, src)
    # ctx lands in each worker's GLOBAL environment, once.
    parallel::clusterExport(cl, "ctx", envir = environment())
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(serial())
  # environment(g) <- globalenv() is the whole trick: the task R serialises is
  # then just `g` plus an empty-looking environment, and `ctx` / .gap_boot_one
  # are resolved on the worker side.
  g = function(dr) tryCatch(.gap_boot_one(dr, ctx),
                            error = function(e) paste0("replicate: ", conditionMessage(e)))
  environment(g) = globalenv()
  message(sprintf("[M9-shape] bootstrap: %d replicates across %d worker(s)", B, nc))
  out = tryCatch(parallel::parLapplyLB(cl, draws, g), error = function(e) NULL)
  if (is.null(out)) serial() else out
}

# Where this file lives, so PSOCK workers can re-source it (the closure they
# receive references cpportal_gap_fit_shaped et al. by name).
.gap_shape_source_path = function() {
  for (p in c("job/model/fect/train/gap_shape.R", "gap_shape.R",
              "../train/gap_shape.R")) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/"))
  }
  NA_character_
}

.gap_shape_boot_se = function(object, newdata, B) {
  d = object$train
  grp_var = if ("metro_id" %in% names(d)) "metro_id" else NULL
  grp = if (!is.null(grp_var)) as.character(d[[grp_var]]) else rep("1", nrow(d))
  idx_by = split(seq_len(nrow(d)), grp)
  # DEGENERATE STRATA. Resampling monitors WITHIN metro can express no
  # variability at all for a metro with a single donor monitor: every replicate
  # draws the same row, that metro's fixed effect is re-estimated off the same
  # observation, and the bootstrap SD for its rows collapses toward 0 -- an SE
  # that is not small because the estimate is precise but because the resampling
  # scheme has nothing to resample. Reporting it would be anti-conservative in
  # the worst way, so those rows are excluded here and take the plug-in
  # fallback. (Measured: Gothenburg 957 and Bergen 958, k = 1 each.)
  thin_strata = names(idx_by)[lengths(idx_by) < 2L]
  w_base = object$weights
  if (!is.null(w_base) && length(w_base) != nrow(d)) w_base = NULL
  s2 = object$stage2
  # Stage 2 needs a monitor key on BOTH the base training frame (to translate a
  # resampled row into a monitor) and the scoring frame (to write the monthly
  # override back onto the right base row).
  if (!is.null(s2)) {
    key = s2$key
    if (!(key %in% names(d)) || !(key %in% names(newdata)) ||
        !(key %in% names(s2$train)) || !(key %in% names(s2$newdata))) {
      warning(sprintf(paste0("[M9-shape] two-stage bootstrap DISABLED: key '%s' ",
                             "is missing from one of the four frames; the SE ",
                             "covers the monitor-level stage ONLY and therefore ",
                             "UNDERSTATES the coalesced prediction"), key),
              call. = FALSE, immediate. = TRUE)
      s2 = NULL
    }
  }
  # --- HYBRID: which metros can the nonparametric resample speak for? --------
  # A metro with k_m >= K donors keeps the ordinary within-metro monitor
  # resample -- that is a genuine draw from its donor distribution. A metro with
  # k_m < K cannot be resampled meaningfully (with k_m = 1 every replicate draws
  # the SAME row, which is why the constrained solve went rank-deficient and all
  # 400 replicates died on `has new levels`). For those metros the rows are held
  # FIXED in the design and the level is drawn PARAMETRICALLY instead,
  # alpha_m* ~ N(alpha_hat_m, nu_hat_m^2). Metros in `newdata` but not in
  # training (k_m = 0) get alpha_m* ~ N(0, tau^2). Both sources of variation
  # enter the SAME replicate, so the SE covers them jointly rather than as two
  # convolved bootstraps.
  pool = object$pool
  K = object$spec$pool_min_donors %||% CPPORTAL_GAP_POOL_MIN_DONORS_DEFAULT
  pv = object$spec$pool_var %||% CPPORTAL_GAP_POOL_VAR
  small = character(0); small_mu = numeric(0); small_sd = numeric(0)
  if (!is.null(pool)) {
    cand = unique(c(names(pool$k),
                    if (pv %in% names(newdata)) as.character(newdata[[pv]])))
    kk = .gap_pool_k(pool, cand)
    small = cand[kk < K]
    if (length(small)) {
      pa = .gap_pool_alpha(pool, small)
      small_mu = pa$alpha
      small_sd = sqrt(pmax(pa$nu2, 0))
      names(small_mu) = small; names(small_sd) = small
      message(sprintf(paste0("[M9-shape] hybrid bootstrap: %d metro(s) below ",
              "K=%d donors draw alpha* ~ N(alpha_hat, nu^2) with rows held ",
              "fixed [%s]; %d metro(s) keep the nonparametric within-metro ",
              "resample"), length(small), K,
              paste(sprintf("%s: k=%d, alpha=%.4f, nu=%.4f", small,
                            .gap_pool_k(pool, small), small_mu, small_sd),
                    collapse = "; "),
              length(cand) - length(small)))
    }
  }
  # Rows of a small-k metro are NOT resampled -- they are the fixed design.
  resample_grp = setdiff(names(idx_by), small)

  # --- WARM STARTS (pure speed; the fixed point is start-independent) --------
  # Every replicate re-runs a backfit sweep and an outer REML search that the
  # HEADLINE fit has already run on almost the same data. Seeding the backfit
  # from the headline alpha removes sweeps. Stage 2 needs the headline monthly
  # fit; take it off the recipe when the caller supplied it,
  # otherwise fit it once here (1 replicate's worth, amortised over B).
  a_start1 = if (is.null(pool)) NULL else pool$alpha
  lam_start1 = if (is.null(pool)) NULL else pool$lambda
  a_start2 = NULL; lam_start2 = NULL
  if (!is.null(s2)) {
    ref2 = s2$fit
    if (is.null(ref2)) {
      ref2 = tryCatch(suppressWarnings(cpportal_gap_fit_shaped(
               s2$formula, s2$train, s2$spec,
               weights = if (is.function(s2$weight_fn)) s2$weight_fn(s2$train) else NULL)),
             error = function(e) NULL)
    }
    if (inherits(ref2, "cpportal_gap_shape_fit")) {
      a_start2 = if (is.null(ref2$pool)) NULL else ref2$pool$alpha
      lam_start2 = if (is.null(ref2$pool)) NULL else ref2$pool$lambda
    }
  }

  P = matrix(NA_real_, nrow(newdata), B)
  # --- draw ALL B resamples up front ----------------------------------------
  # Two reasons, both load-bearing. (1) The draws are then a pure function of
  # the RNG state at entry, so a parallel run is byte-identical to a serial one
  # at the same seed -- the workers receive indices, they do not draw. (2) It
  # keeps the parallel branch from having to reason about L'Ecuyer streams.
  draws = lapply(seq_len(B), function(b) {
    take = unlist(lapply(names(idx_by), function(g) {
      ix = idx_by[[g]]
      # NEVER `sample(ix, ...)`: R's one-element footgun means sample(5, 1)
      # draws from 1:5, not from c(5), so a stratum holding a single row would
      # resample arbitrary OTHER rows and essentially never its own. Index the
      # stratum explicitly. (Identical RNG consumption for length(ix) > 1, so
      # this does not move any result measured on strata of size >= 2.)
      if (g %in% resample_grp)
        ix[sample.int(length(ix), length(ix), replace = TRUE)] else ix
    }), use.names = FALSE)
    list(take = take,
         draw = if (!length(small)) NULL else
           stats::setNames(stats::rnorm(length(small), small_mu, small_sd), small))
  })

  # The replicate's whole working set, assembled ONCE. Passed as a single
  # object so a PSOCK worker can be handed it once (clusterExport) instead of
  # re-serialising a 17k-row training frame with every task -- which is what a
  # closure over the enclosing frame does, and why the first parallel attempt
  # was no faster than serial.
  ctx = list(formula = object$formula, spec = object$spec, d = d,
             w_base = w_base, newdata = newdata, s2 = s2,
             a_start1 = a_start1, a_start2 = a_start2,
             lam_start1 = lam_start1, lam_start2 = lam_start2)

  res = .gap_boot_run(draws, ctx, B)

  n_failed = 0L; n_partial = 0L; n_stage2_used = 0L; reasons = character(0)
  for (b in seq_len(B)) {
    r = res[[b]]
    if (is.null(r) || is.character(r)) {
      n_failed = n_failed + 1L
      if (is.character(r)) reasons = c(reasons, r)
      next
    }
    # A replicate that scored only SOME rows is kept and COUNTED -- the per-row
    # tally below decides which rows still earn a bootstrap SE.
    if (isTRUE(attr(r, "partial"))) {
      n_partial = n_partial + 1L
      reasons = c(reasons,
                  sprintf("stage1_score: %d row(s) unscorable (level unseen)",
                          as.integer(attr(r, "n_unscorable") %||% 0L)))
    }
    if (isTRUE(attr(r, "stage2"))) n_stage2_used = n_stage2_used + 1L
    P[, b] = as.numeric(r)
  }
  n_ok = B - n_failed
  # PER-ROW replicate counts. A row's bootstrap SE is only as good as the number
  # of replicates that actually scored IT, and that number is no longer the same
  # for every row.
  n_rep_row = as.integer(rowSums(is.finite(P)))
  # MINIMUM-REPLICATE RULE: 12.5% of B (50 of the design's B = 400). An SD from
  # n draws has relative standard error ~ 1/sqrt(2(n-1)); at n = 50 that is
  # ~10%, which is a usable SE. Below that the "bootstrap SE" is mostly draw
  # noise, and reporting one would be exactly the silent-partial-SE failure this
  # function exists to prevent -- so those rows return NA and take the
  # documented plug-in fallback instead.
  min_rep = max(2L, as.integer(ceiling(0.125 * B)))
  usable = n_rep_row >= min_rep
  in_thin = rep(FALSE, nrow(newdata))
  if (length(thin_strata) && !is.null(grp_var) && grp_var %in% names(newdata)) {
    in_thin = as.character(newdata[[grp_var]]) %in% thin_strata
    usable = usable & !in_thin
  }
  n_row_fallback = sum(!usable)
  # WHICH rows fell back, in the terms a reader of the log can act on.
  fb_grp = if (n_row_fallback > 0L && "metro_id" %in% names(newdata))
    sort(unique(as.character(newdata$metro_id)[!usable])) else character(0)
  if (!is.null(object$boot_state)) {
    object$boot_state$n_boot_failed = n_failed
    object$boot_state$boot_B = B
    object$boot_state$n_boot_ok = n_ok
    object$boot_state$n_boot_partial = n_partial
    object$boot_state$n_stage2_used = n_stage2_used
    object$boot_state$two_stage = !is.null(s2)
    object$boot_state$reasons = unique(reasons)
    object$boot_state$n_rep_row = n_rep_row
    object$boot_state$min_rep = min_rep
    object$boot_state$n_row_boot = sum(usable)
    object$boot_state$n_row_fallback = n_row_fallback
    object$boot_state$fallback_groups = fb_grp
    object$boot_state$thin_strata = thin_strata
    object$boot_state$n_row_thin = sum(in_thin)
  }
  if (n_failed > 0L || n_partial > 0L) {
    warning(sprintf(paste0("[M9-shape] bootstrap: %d of %d replicate(s) FAILED ",
                           "outright and were dropped; %d scored only SOME ",
                           "rows (%d fully usable)%s"),
                    n_failed, B, n_partial, n_ok - n_partial,
                    if (length(reasons))
                      paste0("; first reason: ", reasons[1]) else ""),
            call. = FALSE, immediate. = TRUE)
  }
  if (n_ok < 2L) {
    warning(sprintf(paste0("[M9-shape] bootstrap UNUSABLE: only %d replicate(s) ",
                           "succeeded of %d; se_boot is NA (the plug-in ",
                           "cross-check is all that is left)"), n_ok, B),
            call. = FALSE, immediate. = TRUE)
    return(structure(rep(NA_real_, nrow(newdata)), n_boot_failed = n_failed,
                     n_boot_ok = n_ok, n_stage2_used = n_stage2_used,
                     two_stage = !is.null(s2), n_rep_row = n_rep_row,
                     min_rep = min_rep, n_row_boot = 0L,
                     n_row_fallback = nrow(newdata), fallback_groups = fb_grp))
  }
  if (n_row_fallback > 0L) {
    warning(sprintf(paste0("[M9-shape] bootstrap: %d of %d prediction row(s) ",
                           "were scored by fewer than %d of %d replicate(s) ",
                           "(min %d, median %d) -- their bootstrap SE would be ",
                           "draw noise, so they return NA and fall back to the ",
                           "plug-in active-set SE, which is ANTI-CONSERVATIVE ",
                           "where constraints bind. Affected metro_id(s): %s.%s"),
                    n_row_fallback, nrow(newdata), min_rep, B,
                    min(n_rep_row[!usable]),
                    as.integer(stats::median(n_rep_row[!usable])),
                    if (length(fb_grp)) paste(fb_grp, collapse = ", ") else "n/a",
                    if (sum(in_thin) > 0L)
                      sprintf(paste0(" Of these, %d row(s) sit in SINGLE-DONOR ",
                                     "strata (metro_id %s), where a within-metro ",
                                     "monitor resample has nothing to resample ",
                                     "and the bootstrap SD collapses to ~0; they ",
                                     "are excluded by rule, not by luck."),
                              sum(in_thin), paste(thin_strata, collapse = ", "))
                    else ""),
            call. = FALSE, immediate. = TRUE)
  }
  se = rep(NA_real_, nrow(newdata))
  if (any(usable)) {
    se[usable] = apply(P[usable, , drop = FALSE], 1, stats::sd, na.rm = TRUE)
  }
  structure(se,
            n_boot_failed = n_failed, n_boot_ok = n_ok,
            n_stage2_used = n_stage2_used, two_stage = !is.null(s2),
            n_rep_row = n_rep_row, min_rep = min_rep,
            n_row_boot = sum(usable), n_row_fallback = n_row_fallback,
            fallback_groups = fb_grp)
}

# One bootstrap replicate of STAGE 2 (the monitor-MONTH refinement).
#
# `mon_ids` is the stage-1 resample AT MONITOR GRAIN, with multiplicity -- the
# same monitors, drawn the same way. Every month of a monitor drawn twice enters
# twice, which is what makes this ONE cluster bootstrap over monitors rather
# than two independent ones. The weights are REBUILT on the resampled frame, so
# the 1/m_i cluster balance is honest for the draw.
#
# Returns a NAMED vector: monitor -> mean monthly prediction over the months the
# refinement covers for that monitor. That is the monitor-level summary of the
# coalesced prediction the downstream ATT actually averages.
#' @param pool_draw named alpha_m* for the small-k metros, drawn ONCE per
#'   replicate in .gap_shape_boot_se() and passed to BOTH stages. The two stages
#'   share the metro, so its level must be the SAME draw in each -- drawing
#'   twice would treat one metro's siting offset as two independent quantities
#'   and understate the coalesced prediction's variance.
.gap_boot_stage2 = function(s2, mon_ids, pool_draw = NULL,
                            pool_start = NULL, lambda_start = NULL) {
  tr = s2$train
  key = s2$key
  idx_by_mon = split(seq_len(nrow(tr)), as.character(tr[[key]]))
  take = unlist(idx_by_mon[mon_ids[mon_ids %in% names(idx_by_mon)]],
                use.names = FALSE)
  if (length(take) < 100L) stop("stage-2 resample too small (", length(take), " rows)")
  trb = tr[take, , drop = FALSE]
  wb = if (is.function(s2$weight_fn)) s2$weight_fn(trb) else NULL
  fm = cpportal_gap_fit_shaped(s2$formula, trb, s2$spec, weights = wb,
                               pool_alpha_override = pool_draw,
                               pool_alpha_start = pool_start,
                               pool_lambda_start = lambda_start)
  nd = s2$newdata
  # Drop scoring rows whose factor levels this replicate never saw (an unseen
  # month-of-year or siting class); they simply keep the stage-1 value.
  for (lv in names(fm$xlevels)) {
    v = sub("^factor\\((.*)\\)$", "\\1", lv)
    if (v %in% names(nd)) nd = nd[as.character(nd[[v]]) %in% fm$xlevels[[lv]], , drop = FALSE]
  }
  if (nrow(nd) == 0L) stop("stage-2 replicate scores no rows")
  pm = as.numeric(.gap_shape_design(fm, nd) %*% fm$coefficients) +
    .gap_shape_pool_terms(fm, nd, override = pool_draw)$alpha
  ok = is.finite(pm)
  if (!any(ok)) stop("stage-2 replicate produced no finite predictions")
  tapply(pm[ok], as.character(nd[[key]])[ok], mean)
}

# --- accessors used by train/functions.R (work for lm AND the shape fit) ----

#' Failed bootstrap replicates recorded on the fit by the last predict() call.
cpportal_gap_fit_n_boot_failed = function(fit) {
  if (!inherits(fit, "cpportal_gap_shape_fit")) return(0L)
  bs = fit$boot_state
  if (is.null(bs)) return(0L)
  as.integer(bs$n_boot_failed %||% 0L)
}

cpportal_gap_r2 = function(fit) {
  if (inherits(fit, "cpportal_gap_shape_fit")) return(fit$r.squared)
  summary(fit)$r.squared
}

cpportal_gap_sigma = function(fit) {
  if (inherits(fit, "cpportal_gap_shape_fit")) return(fit$sigma)
  summary(fit)$sigma
}

cpportal_gap_fit_label = function(fit) {
  if (inherits(fit, "cpportal_gap_shape_fit")) {
    sprintf("M9-shape(k=%d,%s%s%s | active: %s)", fit$spec$k,
            paste(fit$spec$constraints, collapse = "+"),
            if (isTRUE(fit$weighted)) ",weighted" else "",
            if (!is.null(fit$pool))
              sprintf(",pooled-metro(tau=%.3f,sigma=%.3f,K=%d)",
                      sqrt(fit$pool$tau2), sqrt(fit$pool$sigma2),
                      fit$pool$min_donors) else "",
            if (fit$n_active) paste(unique(fit$active_labels), collapse = ",") else "none")
  } else "linear-lm"
}

#' Monotonicity spot-check: is the fitted h(d) non-increasing in dist_road on a
#' probe grid, holding every parametric column at a reference row? The C1 cone
#' is imposed on the KNOT COEFFICIENTS; this verifies the property survives on
#' the km axis a reader cares about, at the SCORING code path (PredictMat), not
#' just in the solver's parameterisation.
cpportal_gap_monotone_check = function(fit, ref_row = NULL, n_grid = 200L,
                                       lo_km = NULL, hi_km = NULL) {
  if (!inherits(fit, "cpportal_gap_shape_fit")) return(NULL)
  dv = fit$spec$dist_var
  d0 = as.numeric(fit$train[[dv]])
  d0 = d0[is.finite(d0)]
  lo = if (is.null(lo_km)) min(d0) else lo_km
  hi = if (is.null(hi_km)) max(d0) else hi_km
  base = if (is.null(ref_row)) fit$train[1, , drop = FALSE] else ref_row[1, , drop = FALSE]
  grid = base[rep(1L, n_grid), , drop = FALSE]
  grid[[dv]] = seq(lo, hi, length.out = n_grid)
  p = as.numeric(.gap_shape_design(fit, grid) %*% fit$coefficients)
  dif = diff(p)
  list(monotone = all(dif <= 1e-8), max_increase = max(c(dif, -Inf)),
       lo_km = lo, hi_km = hi, n_grid = n_grid,
       pred_lo = p[1], pred_hi = p[length(p)])
}

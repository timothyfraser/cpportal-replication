# =============================================================================
# run_replication.R — reproduce the paper's per-metro ATTs with the REAL
# training code.
#
#   Rscript replication/run_replication.R --dry-run     # synthetic fixture
#   Rscript replication/run_replication.R               # Dataverse extract
#
# Run from the repository root (private cpportal, or the public
# cpportal-replication mirror — the layout is identical by design).
#
# -----------------------------------------------------------------------------
# THE POINT OF THIS FILE
# -----------------------------------------------------------------------------
# It does not reimplement anything. Every estimation step below is a call into
# the mirrored, verbatim training code that produced the paper's numbers:
#
#   add_treatment_zones()            job/model/functions.R
#   fect_untreat_uncharged_days()    job/model/fect/train/functions.R
#   fect_add_d_unch()                job/model/fect/train/functions.R
#   add_groups()                     job/model/functions.R
#   fect_outcome_formula()           job/model/fect/train/functions.R
#   fect_outcome_covariates()        job/model/fect/train/functions.R
#   get_fect()                       job/model/fect/functions_fect.R
#   get_qis_fect()                   job/model/fect/functions_fect.R
#
# That is the same sequence `train_fect_bundle()` runs, with exactly ONE
# substitution: `train_fect_bundle()` gets its panel, its treated pairs and its
# policy rows from three private Postgres/PostGIS queries; here they are read
# from the Dataverse deposit (or, under --dry-run, from a synthetic fixture).
# Nothing else is swapped, wrapped or approximated. If the estimator changes
# upstream, this runner changes with it on the next mirror push.
#
# The private steps this file substitutes for, and why they cannot be mirrored:
#   fetch_panel()               reads public.model_panel_daily
#   fetch_zone_treated_pairs()  PostGIS ST_Intersects(monitors, zones)
#   fect_policy_schedules()     reads public.congestion_pricing_periods
# All three need the private database. Their OUTPUTS are what the deposit ships.
# =============================================================================

# -----------------------------------------------------------------------------
# 0. REPLICATION MODE — set before anything else is sourced or read
# -----------------------------------------------------------------------------
# CPPORTAL_REPLICATION_RUN=1 is the mode switch the mirrored training code
# reads. Its one effect today is in `connect_db()` (job/model/functions.R):
# under this flag it reads NO .env file of any kind and stops immediately,
# because a replication run gets its panel, its treated pairs and its policy
# rows from the Dataverse snapshot and must never open — or look for the
# credentials of — the private database. Unset, the same function behaves
# exactly as it does in production.
Sys.setenv(CPPORTAL_REPLICATION_RUN = "1")

# Fixed, NON-SENSITIVE estimator settings. Every one of these is a documented
# production value, pinned here so a replication run is deterministic no matter
# what the replicator happens to have exported in their shell. None is a
# credential; none names a host, account or key.
#
#   CPPORTAL_FECT_UNTREAT_UNCHARGED=1  uncharged days are UNTREATED in the fit
#                                      (Tim 2026-08-08). Default ON; pinned so
#                                      an exported "0" cannot silently change
#                                      the estimand.
#   CPPORTAL_FECT_UNCH_DUMMY=1         D_unch (in-window untreated) enters X.
#                                      Default ON.
#   CPPORTAL_FECT_EPISODE_EXCLUDE=1    episode exclusion on. Default ON.
#   CPPORTAL_FECT_AQ_METHOD=cfe        the paper's fect method for AQ outcomes.
#   CPPORTAL_FECT_AQ_COVARIATES_OVERRIDE=""  no override: use the catalogue in
#                                      job/model/fect/train/functions.R.
#   CPPORTAL_FECT_METRO_AS_Z=0         metro dummies NOT added to Z (off in
#   CPPORTAL_FECT_METRO_YEAR_AS_X=0    production; both are validation knobs).
#   CPPORTAL_FECT_TOL=0.003            production convergence tolerance
#                                      (docs/model/TIMING.md).
#
# Not set here because they gate steps this runner does not execute (imputation
# / anchoring / pin publication live in train_fect_bundle(), not here):
# CPPORTAL_FECT_SAT_ANCHOR, CPPORTAL_FECT_ANCHOR_METHOD, CPPORTAL_FECT_IMPUTE_*,
# CPPORTAL_FECT_PANEL_SOURCE, CPPORTAL_FECT_PARTITION_MODE, CONNECT_*.
REPLICATION_ENV <- c(
  CPPORTAL_FECT_UNTREAT_UNCHARGED       = "1",
  CPPORTAL_FECT_UNCH_DUMMY              = "1",
  CPPORTAL_FECT_EPISODE_EXCLUDE         = "1",
  CPPORTAL_FECT_AQ_METHOD               = "cfe",
  CPPORTAL_FECT_AQ_COVARIATES_OVERRIDE  = "",
  CPPORTAL_FECT_METRO_AS_Z              = "0",
  CPPORTAL_FECT_METRO_YEAR_AS_X         = "0",
  CPPORTAL_FECT_TOL                     = "0.003"
)
do.call(Sys.setenv, as.list(REPLICATION_ENV))

args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && length(args) > i) args[[i + 1L]] else default
}
dry_run     <- "--dry-run" %in% args
outcome_var <- opt("--outcome", "aq_daily_mean")
out_dir     <- opt("--out", file.path("replication", "output"))
fixture_dir <- opt("--fixture-dir", file.path("replication", "fixtures"))
data_dir    <- opt("--data-dir", file.path("replication", "data"))
# --extract <dir> is the REAL-RUN input: the Dataverse deposit as produced by
# replication/make_extract.R (gzipped CSVs + spec.json + MANIFEST.json).
extract_dir <- opt("--extract", NULL)
seed        <- as.integer(opt("--seed", "20260828"))
cores_arg   <- opt("--cores", NULL)
heartbeat   <- opt("--heartbeat", NULL)
pidfile     <- opt("--pidfile", NULL)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

say <- function(...) message("[replication] ", ...)

# Write the PID of THIS R process. The launcher's `$!` is the shell's child,
# which under Git Bash on Windows is a wrapper rather than Rterm.exe — a watcher
# tracking it can report "alive" after the real process has died, or fail to
# kill it. Sys.getpid() is the process that actually holds the fit.
if (!is.null(pidfile)) {
  cat(Sys.getpid(), "
", sep = "", file = pidfile)
  message("[replication] pid ", Sys.getpid(), " -> ", pidfile)
}
beat <- function(...) {
  say(...)
  if (!is.null(heartbeat)) {
    cat(format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), " ", paste0(...), "\n",
        sep = "", file = heartbeat, append = TRUE)
  }
}

# -----------------------------------------------------------------------------
# 0. NO-PIN GUARD — armed BEFORE the training code is sourced
# -----------------------------------------------------------------------------
# The trainer this run sources contains pick_board() / write_bundle(), and
# connect_db() re-reads job/.env (which restores CONNECT_SERVER — the footgun
# that once made a "local" benchmark publish a production pin). Arm the guard
# first, install it after the source(), and assert around the fit. See
# replication/no_pin_guard.R for the four independent levels.
source(file.path("replication", "no_pin_guard.R"))
arm_no_pin_guard()

# -----------------------------------------------------------------------------
# 1. Source the mirrored training code
# -----------------------------------------------------------------------------
# Same order as job/model/fect/train/job.R. Paths are repo-root-relative and are
# preserved verbatim by the mirror, so this resolves identically in both repos.
MIRRORED <- c(
  "job/model/functions.R",
  "job/model/did/functions_did.R",
  "job/model/fect/functions_fect.R",
  "job/model/fect/train/episode_lib.R",
  "job/model/fect/train/functions.R"
)
for (f in MIRRORED) {
  if (!file.exists(f)) {
    stop("cannot find mirrored file '", f, "'. Run this from the repository ",
         "root.", call. = FALSE)
  }
  suppressWarnings(source(f))
}
say("sourced ", length(MIRRORED), " mirrored training file(s)")

# --- assert the run is credential-free (T9b's in-source guard) ---------------
# ORDER MATTERS, and this is the merge of two independent guards, not a choice
# between them. Probe FIRST, while `connect_db` is still the REAL one just
# sourced from job/model/functions.R: that is the only moment at which the T9b
# replication-mode branch inside connect_db() can actually be tested. Shim it
# first (below) and the probe would only prove that the shim is installed.
#
# Not a comment, an assertion: call connect_db() and require that it refuses.
# If this ever returns a handle (or a NULL from a missing PGHOST rather than the
# replication stop), the guard has regressed and the run must not continue.
.probe <- tryCatch({ connect_db(); "returned" },
                   error = function(e) conditionMessage(e))
if (!grepl("no database connection is used or permitted", .probe, fixed = TRUE)) {
  stop("replication guard missing: connect_db() did not refuse under ",
       "CPPORTAL_REPLICATION_RUN=1 (got: ", .probe, "). Refusing to run.",
       call. = FALSE)
}
rm(.probe)
say("replication mode: connect_db() refuses; no .env is read and no database ",
    "connection is opened", if (file.exists("job/.env"))
      " (a job/.env exists here and is deliberately ignored)" else "")

# --- levels 2 + 3 of the no-pin guard (T9c) ----------------------------------
# Shim pins::board_connect / pins::pin_write in the pins namespace, and
# connect_db / pick_board / write_bundle in globalenv, so the definitions just
# sourced can no longer reach a Connect board even if the in-source guard above
# were removed. Belt AND braces: the probe proves the trainer refuses, these
# shims make the refusal unnecessary.
install_no_pin_guard()
assert_no_pin_possible("post-source")

set.seed(seed)
say("seed=", seed)

if (!requireNamespace("fect", quietly = TRUE)) {
  stop("package 'fect' is required. Install it with:\n",
       "  remotes::install_github('xuyiqing/fect')", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 2. Inputs
# -----------------------------------------------------------------------------
read_csv_strict <- function(dir, name, required) {
  # An extract from make_extract.R ships gzipped; the fixture and the Dataverse
  # download ship plain. Accept either, prefer .gz.
  p <- file.path(dir, paste0(name, ".gz"))
  if (!file.exists(p)) p <- file.path(dir, name)
  if (!file.exists(p)) {
    stop("missing input: ", file.path(dir, name), " (or .gz)", call. = FALSE)
  }
  x <- utils::read.csv(p, stringsAsFactors = FALSE)
  miss <- setdiff(required, names(x))
  if (length(miss) > 0L) {
    stop(p, " is missing required column(s): ", paste(miss, collapse = ", "),
         "\nSee replication/panel_contract.md.", call. = FALSE)
  }
  # A deposit must never carry a licensed TomTom column. Same predicate the
  # extract builder asserts on, applied again at READ time so a hand-edited or
  # re-downloaded input cannot slip one in.
  assert_no_licensed(names(x), what = basename(p))
  tibble::as_tibble(x)
}

source(file.path("replication", "check_no_licensed.R"))

src_dir <- if (dry_run) fixture_dir else if (!is.null(extract_dir)) extract_dir else data_dir
if (!is.null(extract_dir) && !dry_run) {
  say("REAL RUN — reading the extract in ", extract_dir)
  mf <- file.path(extract_dir, "MANIFEST.json")
  if (file.exists(mf)) {
    m <- jsonlite::fromJSON(mf)
    say("  manifest: spec=", m$spec_id, " outcome=", m$outcome,
        " system_types=", paste(m$system_types, collapse = ","),
        " date_to=", m$date_to, " pin_version=", m$pin_version)
    say("  manifest: panel rows=", m$panel_stats$rows,
        " monitors=", m$panel_stats$monitors,
        " metros=", m$panel_stats$metros)
    say("  manifest: licensed_check=", m$licensed_check)
  } else {
    say("  WARNING: no MANIFEST.json in ", extract_dir)
  }
}
if (dry_run) {
  say("DRY RUN — reading the SYNTHETIC fixture in ", fixture_dir)
  say("  fixture values are fabricated; the ATTs below are meaningless and ",
      "must never be cited. The purpose is to prove the mirrored code path ",
      "executes outside the private infrastructure.")
} else if (is.null(extract_dir)) {
  say("reading the Dataverse extract in ", data_dir,
      " (fetch it with: Rscript replication/pull_dataverse.R)")
}

# Column contract, from the generated panel_contract.md — see
# replication/generate_panel_contract.R.
panel <- read_csv_strict(src_dir, "panel.csv",
                         c("fullaqsid", "date", "metro_id", outcome_var))
zone_pairs <- read_csv_strict(src_dir, "zone_pairs.csv",
                              c("metro_id", "fullaqsid", "policy_id",
                                "start_date", "end_date"))
policy_schedules <- read_csv_strict(src_dir, "policy_schedules.csv",
                                    c("policy_id", "policy_uid",
                                      "system_type", "metro_id"))

panel <- panel |>
  mutate(date      = as.Date(.data$date),
         metro_id  = as.integer(.data$metro_id),
         fullaqsid = as.character(.data$fullaqsid))
zone_pairs <- zone_pairs |>
  mutate(metro_id   = as.integer(.data$metro_id),
         fullaqsid  = as.character(.data$fullaqsid),
         policy_id  = as.integer(.data$policy_id),
         start_date = as.Date(.data$start_date),
         end_date   = as.Date(.data$end_date))
policy_schedules <- policy_schedules |>
  mutate(policy_id   = as.integer(.data$policy_id),
         policy_uid  = as.character(.data$policy_uid),
         system_type = as.character(.data$system_type),
         metro_id    = as.integer(.data$metro_id))

# TREATED SYSTEM TYPES — always BOTH. `cordon` alone silently excludes London's
# ULEZ and understates London's treated set; narrowing this is a change to the
# ESTIMAND, not a config tweak.
SYSTEM_TYPES <- c("cordon", "ULEZ")
keep_policies <- policy_schedules$policy_id[policy_schedules$system_type %in% SYSTEM_TYPES]
n_before <- nrow(zone_pairs)
zone_pairs <- zone_pairs |> filter(.data$policy_id %in% keep_policies)
say("treated set: system_types=", paste(SYSTEM_TYPES, collapse = ","),
    "  pairs ", n_before, " -> ", nrow(zone_pairs),
    "  monitors=", dplyr::n_distinct(zone_pairs$fullaqsid),
    "  policies=", dplyr::n_distinct(zone_pairs$policy_id))

say("panel: ", nrow(panel), " monitor-days, ",
    dplyr::n_distinct(panel$fullaqsid), " monitors, ",
    dplyr::n_distinct(panel$metro_id), " metros, ",
    format(min(panel$date)), " .. ", format(max(panel$date)))

# -----------------------------------------------------------------------------
# 3. The paper spec, built by the real catalogue functions
# -----------------------------------------------------------------------------
cov <- fect_outcome_covariates(outcome_var)
spec <- list(
  id           = paste0("replication_all_priority_", outcome_var),
  outcome_var  = outcome_var,
  outcome_kind = fect_outcome_kind(outcome_var),
  covariates   = cov$covariates,
  z_covariates = cov$z_covariates,
  fect_method  = cov$fect_method,
  system_types = SYSTEM_TYPES
)
say("spec=", spec$id, "  kind=", spec$outcome_kind, "  method=", spec$fect_method)
say("  outcome formula: ", deparse(fect_outcome_formula(outcome_var)))
say("  X (", length(all.vars(spec$covariates)), "): ",
    paste(all.vars(spec$covariates), collapse = ", "))
say("  Z (", length(spec$z_covariates), "): ",
    paste(spec$z_covariates, collapse = ", "))

need <- fect_spec_panel_cols(spec)
miss <- setdiff(need, names(panel))
if (length(miss) > 0L) {
  stop("panel.csv is missing model columns: ", paste(miss, collapse = ", "),
       "\nSee replication/panel_contract.md section 4.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 4. Treatment encoding — the real functions, in the real order
# -----------------------------------------------------------------------------
panel <- add_treatment_zones(panel, zone_pairs)
say("add_treatment_zones: treated rows = ", sum(panel$treated %in% TRUE))

# Uncharged days are UNTREATED (Tim 2026-08-08), resolved EITHER/OR across
# schemes (2026-08-09): a monitor-day stays treated if ANY covering (zone,
# period) pair charges that weekday. Rows are kept; only `treated` moves.
panel <- fect_untreat_uncharged_days(panel, spec_id = spec$id,
                                     zone_pairs = zone_pairs,
                                     policy_schedules = policy_schedules)
say("after charged-days flip: treated rows = ", sum(panel$treated %in% TRUE))

d <- fect_add_d_unch(panel, spec)
panel <- d$panel
spec  <- d$spec

# POST-JOIN DEDUPE — the step train_fect_bundle() runs at
# job/model/fect/train/functions.R:3189 ("dedupe after add_treatment_zones").
# It is not optional and it is not cosmetic: `add_treatment_zones()` joins the
# panel to zone_pairs, and a monitor covered by SEVERAL overlapping policies
# comes back with one row per policy. London alone has six (CCZ-2003,
# CCZ-WE-2007, CCZ-CORE-2011, ULEZ-2019-CENTRAL, ULEZ-2021-INNER,
# ULEZ-2023-WIDE), so its monitor-days arrive up to 6x. `fect` then refuses the
# panel outright:
#     "Observations are not uniquely defined by unit and time indicators."
# Routed through the trainer's own helper so there is exactly ONE dedupe rule
# in play, here and upstream. (The synthetic fixture does not reach this: its
# two policies do not produce a surviving duplicate, which is why the dry run
# passed while the real panel could not fit.)
panel <- fect_dedupe_unit_time(panel, .tag = "replication",
                               .what = "dedupe after add_treatment_zones")

n_dup <- sum(duplicated(paste(panel$fullaqsid, panel$date)))
if (n_dup > 0L) {
  stop("panel still has ", n_dup, " duplicate (fullaqsid, date) rows after ",
       "dedupe — fect will reject it.", call. = FALSE)
}
say("unit-time keys unique: ", nrow(panel), " rows")

panel <- add_groups(panel)

# -----------------------------------------------------------------------------
# 5. Fit
# -----------------------------------------------------------------------------
# THE PRODUCTION FIT CALL, MIRRORED ARGUMENT FOR ARGUMENT.
#
# The authority is the call site in `train_fect_bundle()` at
# job/model/fect/train/functions.R:3426-3442 — NOT `get_fect()`'s own signature
# defaults, and NOT its roxygen block. Those two disagree with the trainer on
# the points that matter most, and this runner previously followed the wrong
# one for three failed runs:
#
#   argument          get_fect() default     WHAT THE TRAINER PASSES
#   ---------------   --------------------   ------------------------------
#   vartype           "bootstrap"            "analytic"
#   nboots            1000                   (irrelevant — no bootstrap runs)
#   use_metro_index   TRUE                   FALSE
#   cores             8                      8
#   na.rm             method-derived         identical(fect_method, "cfe")
#
# FULL CALL-SITE AUDIT, argument by argument (verified 2026-08-28 against
# job/model/fect/train/functions.R:3426-3442). Every parameter of get_fect() is
# listed, including the ones neither side passes, because "not passed by either"
# is itself a match that has to be CHECKED rather than assumed:
#
#   parameter        trainer passes            this runner passes        match
#   --------------   -----------------------   -----------------------   -----
#   data             panel                     panel                     yes
#   outcome          spec$outcome              fect_outcome_formula(     yes —
#                    (built by the same        outcome_var)              same
#                    fect_outcome_formula                                value
#                    at functions.R:400)
#   covariates       spec$covariates           spec$covariates           yes
#   z_covariates     spec$z_covariates         spec$z_covariates         yes
#   treated_col      "treated" (explicit)      default "treated"         yes*
#   date_col         "date" (explicit)         default "date"            yes*
#   unit_col         "fullaqsid" (explicit)    default "fullaqsid"       yes*
#   metro_col        "metro_id" (explicit)     default "metro_id"        yes*
#   use_metro_index  FALSE                     FALSE                     yes
#   response_inverse not passed (NULL)         not passed (NULL)         yes
#   na.rm            identical(method,"cfe")   identical(method,"cfe")   yes
#   se               TRUE                      TRUE                      yes
#   parallel         TRUE                      TRUE                      yes
#   cores            8L                        8L (--cores overridable)  yes**
#   nboots           not passed                not passed                yes***
#   vartype          "analytic"                "analytic"                yes
#   method           spec$fect_method          spec$fect_method          yes
#   mc_lambda        not passed (NULL)         not passed (NULL)         yes
#   ... (tol)        CPPORTAL_FECT_TOL, no     same env var, DEFAULT     yes**
#                    default (unset => fect's  "0.003"
#                    1e-3); prod sets 0.003
#   ... (max.iter)   CPPORTAL_FECT_MAX_ITER    same env var              yes
#
#   *   passed positionally-by-default rather than explicitly; the default IS
#       the trainer's literal, so the value reaching fect::fect() is identical.
#   **  deliberate, documented divergences in MECHANISM with identical values in
#       production: `cores` stays overridable for a small machine, and `tol`
#       DEFAULTS to the production value instead of inheriting an unset env —
#       a replicator has no Connect content to read the setting from.
#   *** get_fect() sets se=FALSE before calling fect::fect() whenever
#       vartype=="analytic", so fect never reaches its bootstrap branch and
#       `nboots` is not a parameter of this code path on either side.
#
# TWO NON-ARGUMENT DIFFERENCES, both deliberate:
#   1. The trainer calls get_fect through purrr::possibly (`fit_safe`), so a
#      failed spec returns NULL and the nightly continues. This runner calls
#      get_fect DIRECTLY: a replication that half-fails must abort loudly, not
#      write a plausible-looking empty result.
#   2. get_qis_fect(): the trainer passes start=spec$date_from, end=spec$date_to;
#      this runner passes neither, so the effect grid spans min/max of the panel.
#      EQUIVALENT ONLY BECAUSE make_extract.R truncates the extract to exactly
#      [date_from, date_to] — verified against MANIFEST.json, which records the
#      window the panel was cut to. If a hand-built extract ever carries a wider
#      window, this stops being equivalent.
#
# WHY THIS IS A CORRECTNESS FIX, NOT A PERFORMANCE TWEAK:
#
#  * vartype = "analytic". get_fect() line 321 does
#        analytic_se = isTRUE(se) && identical(vartype, "analytic")
#        if (analytic_se) se = FALSE
#    so `se = FALSE` reaches fect::fect(), which NEVER enters its bootstrap
#    branch, never calls future::plan(multisession), and never serializes the
#    5.75 GiB `one.nonpara` closure to PSOCK workers. SEs are computed post-hoc
#    by fect_pred_se(method = "twoway_resid"). Production has no bootstrap at
#    all. Runs 1-3 of this replication died inside a bootstrap that production
#    does not run: the entire memory crisis was self-inflicted by following the
#    wrapper's docstring instead of the trainer.
#
#  * use_metro_index = FALSE. The "Plan (a)" comment at functions.R:3301-3307
#    records that metro_id is unit-invariant, so a third additive FE on top of
#    the unit FE is mathematically redundant AND "silently failing on every spec
#    we inspected (Mat::elem / inv_sympd)". Run 3 hit exactly that:
#        std::out_of_range: Mat::elem(): index out of bounds
#    Leaving the default TRUE meant attempting a doomed fit, paying its full
#    cost, and only then falling back — with the failed attempt's workspace
#    still resident. Production forces FALSE and goes straight to the two-way
#    index. The try/fallback is not something to harden; it is something this
#    runner should never have reached.
#
# WHAT IS DELIBERATELY *NOT* MIRRORED: cores. The trainer hardcodes 8; here it
# stays overridable via --cores. With no bootstrap there is no PSOCK export, so
# the analytic path's cost is the EM fit itself plus fect_pred_se(); `parallel`
# and `cores` are forwarded exactly as the trainer sets them and simply have
# far less to do.
na_rm_for_method <- identical(spec$fect_method, "cfe")
cores <- if (!is.null(cores_arg)) as.integer(cores_arg) else if (dry_run) 1L else 8L

# Convergence tolerance: PRODUCTION SETS CPPORTAL_FECT_TOL=0.003 (it is NOT
# running the fect package default of 1e-3). The trainer reads the same env var
# at job/model/fect/train/functions.R:3322 and forwards it into fect::fect()
# through get_fect()'s `...`, so setting it here reproduces the production fit
# rather than a differently-converged one.
.tol <- suppressWarnings(as.numeric(Sys.getenv("CPPORTAL_FECT_TOL", "0.003")))
.maxit <- suppressWarnings(as.integer(Sys.getenv("CPPORTAL_FECT_MAX_ITERATION", "")))
fect_extra <- list()
if (is.finite(.tol) && .tol > 0) fect_extra$tol <- .tol
if (is.finite(.maxit) && .maxit > 0) fect_extra$max.iteration <- .maxit

beat("fitting fect (method=", spec$fect_method, ", vartype=analytic, ",
     "use_metro_index=FALSE, cores=", cores, ", tol=", .tol,
     ", na.rm=", na_rm_for_method, ") ...")
beat("  NO bootstrap: production computes SEs analytically (twoway_resid), so ",
     "no future/PSOCK export happens. Expect [fect.timing] lines, and NO ",
     "'Parallel computing ...' banner.")

# Start the fit from a clean heap so nothing from panel assembly is still
# resident alongside the fit's own allocations.
invisible(gc(full = TRUE))

# The last thing checked before the long-running call, and the first thing
# checked after it: a pin write is impossible on both sides of the fit.
assert_no_pin_possible("pre-fit")

t0 <- Sys.time()
fit <- do.call(get_fect, c(list(
  data            = panel,
  outcome         = fect_outcome_formula(outcome_var),
  covariates      = spec$covariates,
  z_covariates    = spec$z_covariates,
  method          = spec$fect_method,
  na.rm           = na_rm_for_method,
  se              = TRUE,
  vartype         = "analytic",
  parallel        = TRUE,
  cores           = cores,
  use_metro_index = FALSE
), fect_extra))
beat("fit done in ", sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "mins"))), " min")

assert_no_pin_possible("post-fit")

# -----------------------------------------------------------------------------
# 6. Per-metro ATTs (paper Table 1 grain)
# -----------------------------------------------------------------------------
# A real run also emits the per-monitor-day cells (`per_day_metro_unit`) and the
# monthly grain, because the paper's Table 1 headline is NOT the pinned
# `per_metro` row: it is the D2b day-pool over those cells with the charged-days
# cut applied (see the honest-scope note at the bottom). Emitting the cells is
# what lets `reproduce_paper.R` close that gap without forking the estimator.
aggs <- if (dry_run) {
  c("overall", "per_metro")
} else {
  c("overall", "per_metro", "per_metro_month", "per_day_metro_unit")
}
beat("computing QoI aggregates: ", paste(aggs, collapse = ", "))
qis <- get_qis_fect(fit, data = panel, aggregates = aggs)
att <- dplyr::bind_rows(qis)

# -----------------------------------------------------------------------------
# 6b. EPISODE POSTPROCESS — the production step this runner used to skip
# -----------------------------------------------------------------------------
# `train_fect_bundle()` runs this immediately after `get_qis_fect()`
# (job/model/fect/train/functions.R:3745-3760): it stamps `episode_flag` on the
# day-grain ATT rows (rel99_x2 threshold, +/-1 day dilation) and rebuilds the
# fitted overall / per_metro / per_metro_month rows on charged, non-episode
# cells. Episode is NEVER a train covariate; this is a SCORING exclusion.
#
# WHY IT IS NOT OPTIONAL HERE. The serving layer drops `episode_flag == TRUE`
# before pooling, but `cpportal_episode_days_filter()` FAILS OPEN when the
# column is absent. A run that skips this step therefore does not error — it
# silently pools episode days that every published number excludes. Measured on
# the 2026-08-28 run, which skipped it: the replication pooled 1-3% MORE
# monitor-days than the paper in every city (London 19,251 vs 18,813; Bergen
# 12,622 vs 12,223), and every per-city ATT came out ~0.13 ug/m3 less negative.
# See replication/REPRODUCTION_REPORT.md.
#
# `fect_episode_postprocess_att()` lives in the already-mirrored
# job/model/fect/train/episode_lib.R, so this is a CALL to the production
# function, not a reimplementation. Guarded exactly as the trainer guards it.
if (identical(spec$outcome_kind, "aq") && nrow(att) > 0L &&
    exists("fect_episode_postprocess_att", mode = "function")) {
  ep_outcome <- if ("aq_daily_mean" %in% names(panel)) "aq_daily_mean" else outcome_var
  beat("episode postprocess (production scoring exclusion) on '", ep_outcome, "' ...")
  att <- tryCatch(
    fect_episode_postprocess_att(att, panel, outcome_col = ep_outcome),
    error = function(e) {
      beat("  episode postprocess FAILED: ", conditionMessage(e),
           " — att left unchanged (report this; do not compare to the paper)")
      att
    }
  )
  beat("  episode_flag present=", "episode_flag" %in% names(att),
       " flagged_rows=", if ("episode_flag" %in% names(att))
         sum(att$episode_flag %in% TRUE) else 0L)
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, paste0("att_", outcome_var, ".csv"))
utils::write.csv(att, out_path, row.names = FALSE, na = "")

# Per-type split, so C2 can load just the grain it needs without reading a
# multi-hundred-MB combined file.
for (ty in unique(att$type)) {
  p <- file.path(out_dir, paste0("att_", outcome_var, "__", ty, ".csv"))
  utils::write.csv(att[att$type == ty, , drop = FALSE], p,
                   row.names = FALSE, na = "")
  beat("  wrote ", basename(p), "  rows=", sum(att$type == ty))
}

# Run metadata, so the reproduction report can state exactly what produced it.
jsonlite::write_json(list(
  finished_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  outcome     = outcome_var,
  spec_id     = spec$id,
  seed        = seed,
  vartype     = "analytic",
  use_metro_index = FALSE,
  na_rm       = na_rm_for_method,
  cores       = cores,
  fit_call_note = paste(
    "Mirrors the production trainer call site,",
    "job/model/fect/train/functions.R:3426-3442:",
    "se=TRUE, vartype='analytic', parallel=TRUE, cores=8,",
    "use_metro_index=FALSE, na.rm=identical(fect_method,'cfe').",
    "Production computes SEs analytically (twoway_resid) and runs NO",
    "bootstrap, so nboots is not a parameter of this path."),
  tol         = .tol,
  system_types = SYSTEM_TYPES,
  basis        = "fitted",
  anchored_run = FALSE,
  fit_minutes  = as.numeric(difftime(Sys.time(), t0, units = "mins")),
  panel_rows   = nrow(panel),
  panel_monitors = dplyr::n_distinct(panel$fullaqsid),
  panel_metros   = dplyr::n_distinct(panel$metro_id),
  treated_rows   = sum(panel$treated %in% TRUE),
  att_rows_by_type = as.list(table(att$type)),
  no_pin_guard = "PASS (pre-fit and post-fit assertions)"
), file.path(out_dir, paste0("run_meta_", outcome_var, ".json")),
   auto_unbox = TRUE, pretty = TRUE, null = "null")

beat("wrote ", out_path, "  (", nrow(att), " ATT rows)")
cat("\n=== ATT rows (metro grain; per-cell grains are in their own files) ===\n")
print(as.data.frame(
  att |>
    filter(.data$type %in% c("overall", "per_metro")) |>
    select(any_of(c("type", "metro_id", "att", "se_att", "t", "p_value",
                    "pct_change", "n_effects")))
))
cat("\n")

# -----------------------------------------------------------------------------
# 7. Honest scope note — the HONESTY CLAUSE, stated as a precise gap map
# -----------------------------------------------------------------------------
# These are FITTED-basis ATTs from the mirrored estimator, on the paper's
# fitted-only universe. Three things stand between them and the paper's
# published Table 1, and they are NOT the same size:
#
#  (a) THE CHARGED-DAYS POOLING CUT — closable here, no new math.
#      The published per-city ATT is the D2b pool over per-monitor-day cells
#      with non-charging days removed at POOLING time. This run emits those
#      cells (`per_day_metro_unit`), and the pooling function itself already
#      exists as `combine_att_window_d2b()` in app/v2/api/R/cross_city_att.R.
#      The pinned `per_metro` row printed above is an ALL-DAYS pool and will NOT
#      equal the paper's number for the five metros with an excluded weekday
#      (943, 949, 950, 955, 957); it will for NYC (1) and Bergen (958).
#
#  (b) THE WINDOWED / ERA ROWS — needs a mirror addition, not a rewrite.
#      Table 1's London rows are per-ERA (lon_e1, lon_e2, ...) and Table 4's
#      Y1..Y10 horizon rows are windowed pools. Both come from
#      `metro_window_estimate()` / `summarize_window_att()` in
#      app/v2/api/R/cross_city_att.R, which is SERVING-side and is not on the
#      mirror allowlist today. The correct fix is to ADD cross_city_att.R (or
#      the pooling half of it) to replication/mirror_allowlist.txt so the public
#      repo runs the SAME function the portal runs — NOT to re-implement D2b
#      pooling inside replication/. A second implementation of the pool is
#      exactly how the portal and the paper would drift apart.
#
#  (c) THE ANCHORED (M10) PATH — out of scope by construction.
#      Oslo (956) is anchored at zone scope and has no fitted per-monitor-day
#      row at all. The paper's fitted-only universe (PAPER_EFFECTS_FITTED_METROS)
#      is the seven fitted cities; Oslo is reported from the anchored path and is
#      not reproducible from this run. That is a documented scope line, not a
#      discrepancy to smooth over.
#
# See replication/README.md, "What is and is not reproducible".
say("HONESTY CLAUSE — see the block comment at the end of this file:")
say("  (a) charged-days pooling cut: CLOSABLE from this run's per_day_metro_unit")
say("      cells; the pinned per_metro row above is an ALL-DAYS pool and will")
say("      differ for metros 943/949/950/955/957.")
say("  (b) windowed + per-era rows (Table 1 London eras, Table 4 Y1..Y10):")
say("      need app/v2/api/R/cross_city_att.R ADDED to the mirror allowlist —")
say("      mirror it, never re-implement combine_att_window_d2b().")
say("  (c) anchored M10 (Oslo 956): out of scope; fitted-only run by design.")

if (nrow(att) == 0L) {
  stop("no ATT rows produced", call. = FALSE)
}
invisible(NULL)

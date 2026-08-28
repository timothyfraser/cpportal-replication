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

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

say <- function(...) message("[replication] ", ...)

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

# --- assert the run is credential-free ---------------------------------------
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

if (!requireNamespace("fect", quietly = TRUE)) {
  stop("package 'fect' is required. Install it with:\n",
       "  remotes::install_github('xuyiqing/fect')", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 2. Inputs
# -----------------------------------------------------------------------------
read_csv_strict <- function(dir, name, required) {
  p <- file.path(dir, name)
  if (!file.exists(p)) stop("missing input: ", p, call. = FALSE)
  x <- utils::read.csv(p, stringsAsFactors = FALSE)
  miss <- setdiff(required, names(x))
  if (length(miss) > 0L) {
    stop(p, " is missing required column(s): ", paste(miss, collapse = ", "),
         "\nSee replication/panel_contract.md.", call. = FALSE)
  }
  tibble::as_tibble(x)
}

src_dir <- if (dry_run) fixture_dir else data_dir
if (dry_run) {
  say("DRY RUN — reading the SYNTHETIC fixture in ", fixture_dir)
  say("  fixture values are fabricated; the ATTs below are meaningless and ",
      "must never be cited. The purpose is to prove the mirrored code path ",
      "executes outside the private infrastructure.")
} else {
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

panel <- add_groups(panel)

# -----------------------------------------------------------------------------
# 5. Fit
# -----------------------------------------------------------------------------
# Dry runs use a small bootstrap so the fixture finishes in seconds; a real
# replication run should leave the production defaults alone.
nboots <- if (dry_run) 20L else FECT_NBOOTS_PRODUCTION
cores  <- if (dry_run) 1L else FECT_CORES_DEFAULT
say("fitting fect (method=", spec$fect_method, ", nboots=", nboots,
    ", cores=", cores, ") ...")

t0 <- Sys.time()
fit <- get_fect(
  data         = panel,
  outcome      = fect_outcome_formula(outcome_var),
  covariates   = spec$covariates,
  z_covariates = spec$z_covariates,
  method       = spec$fect_method,
  se           = TRUE,
  nboots       = nboots,
  cores        = cores,
  parallel     = cores > 1L
)
say("fit done in ", sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s")

# -----------------------------------------------------------------------------
# 6. Per-metro ATTs (paper Table 1 grain)
# -----------------------------------------------------------------------------
qis <- get_qis_fect(fit, data = panel,
                    aggregates = c("overall", "per_metro"))
att <- dplyr::bind_rows(qis)

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
out_path <- file.path(out_dir, paste0("att_", outcome_var, ".csv"))
utils::write.csv(att, out_path, row.names = FALSE, na = "")

say("wrote ", out_path, "  (", nrow(att), " ATT rows)")
cat("\n=== ATT rows ===\n")
print(as.data.frame(att |> select(any_of(c("type", "metro_id", "att", "se_att",
                                           "t", "p_value", "pct_change",
                                           "n_effects")))))
cat("\n")

# -----------------------------------------------------------------------------
# 7. Honest scope note
# -----------------------------------------------------------------------------
say("NOTE: these are FITTED-basis ATTs from the mirrored estimator. The paper's")
say("  published table additionally applies (a) the serving-side charged-days")
say("  pooling cut and (b) the anchored (M10) path for metros fect cannot fit,")
say("  both of which live in the portal's serving layer, not in the training")
say("  code. See replication/README.md section 'What is and is not reproducible'.")

if (nrow(att) == 0L) {
  stop("no ATT rows produced", call. = FALSE)
}
invisible(NULL)

# =============================================================================
# generate_panel_contract.R — write replication/panel_contract.md FROM the real
# training declarations. Never hand-edit panel_contract.md.
#
#   Rscript replication/generate_panel_contract.R           # write
#   Rscript replication/generate_panel_contract.R --check    # verify, exit 1 on drift
#
# Run from the repository root.
#
# How the column list is obtained: this script SOURCES the mirrored training
# files into a throwaway environment and CALLS the same functions the trainer
# calls — `fect_outcome_covariates()` and `fect_spec_panel_cols()` — rather than
# regexing or transcribing a list. If someone adds a covariate to
# `fect_aq_default_covariates` in job/model/fect/train/functions.R, `--check`
# fails on the next CI run and the contract is regenerated in the same PR.
#
# Sourcing is safe here: all four files are pure definitions at load time. None
# of them opens a connection, reads job/.env, or contacts anything at source().
# =============================================================================

args   <- commandArgs(trailingOnly = TRUE)
check  <- "--check" %in% args
root   <- {
  i <- match("--root", args)
  if (!is.na(i) && length(args) > i) args[[i + 1L]] else "."
}

SOURCES <- c(
  "job/model/functions.R",
  "job/model/did/functions_did.R",
  "job/model/fect/functions_fect.R",
  "job/model/fect/train/episode_lib.R",
  "job/model/fect/train/functions.R"
)

env <- new.env(parent = globalenv())

# The training files `library()` the DB stack (DBI, RPostgres, dbplyr) at load
# time. This generator never touches a database and never calls a function that
# needs them — it only reads declarations and calls base-R helpers — so a
# missing package must not stop it. Stubbing `library`/`require` inside the
# sourcing environment keeps this runnable on a bare CI runner with base R only.
# Every value below still comes from EXECUTING the real declarations.
env$library <- function(package, ...) {
  nm <- tryCatch(as.character(substitute(package)), error = function(e) "")
  suppressWarnings(suppressMessages(
    tryCatch(base::library(nm, character.only = TRUE),
             error = function(e) invisible(NULL))
  ))
  invisible(NULL)
}
env$require <- function(package, ...) {
  nm <- tryCatch(as.character(substitute(package)), error = function(e) "")
  isTRUE(suppressWarnings(suppressMessages(
    requireNamespace(nm, quietly = TRUE)
  )))
}

for (s in SOURCES) {
  p <- file.path(root, s)
  if (!file.exists(p)) stop("generate_panel_contract: missing ", p, call. = FALSE)
  suppressWarnings(suppressMessages(sys.source(p, envir = env, keep.source = FALSE)))
}

get <- function(nm) {
  if (!exists(nm, envir = env, inherits = FALSE)) {
    stop("generate_panel_contract: '", nm, "' not found after sourcing — the ",
         "training declarations moved; fix this generator, do not hand-edit ",
         "panel_contract.md", call. = FALSE)
  }
  get0(nm, envir = env, inherits = FALSE)
}

aq_outcomes      <- get("fect_aq_outcomes")
traffic_outcomes <- get("fect_traffic_outcomes")
required_cols    <- get("fect_panel_required_cols")()

# Exercise the real dispatcher for both outcome kinds.
aq_cov      <- get("fect_outcome_covariates")(aq_outcomes[[1L]])
traffic_cov <- get("fect_outcome_covariates")(traffic_outcomes[[1L]])

aq_covariates      <- all.vars(aq_cov$covariates)
aq_z_covariates    <- as.character(aq_cov$z_covariates)
traffic_covariates <- all.vars(traffic_cov$covariates)

# The exact column set fetch_panel() selects for the paper spec, via the real
# helper. spec is the minimal shape fect_spec_panel_cols() consumes.
paper_spec <- list(
  outcome_var  = "aq_daily_mean",
  covariates   = aq_cov$covariates,
  z_covariates = aq_cov$z_covariates
)
paper_cols <- get("fect_spec_panel_cols")(paper_spec)
# fetch_panel() additionally fetches partition_key to break the -1HR/-24HR tie,
# then drops it before returning. The deposit must carry it.
paper_cols_fetched <- unique(c(paper_cols, "partition_key"))

bullet <- function(x) paste0("- `", x, "`", collapse = "\n")
n <- length

md <- c(
"<!-- GENERATED FILE — DO NOT EDIT.",
"     Source: Rscript replication/generate_panel_contract.R",
"     Derived by SOURCING the real training code and calling",
"     fect_outcome_covariates() / fect_spec_panel_cols(). Any hand edit is",
"     reverted by the next run and fails `--check` in CI. -->",
"",
"# Panel contract — what the FECT model actually consumes",
"",
"This is the column contract for the Harvard Dataverse deposit. It is generated",
"from the mirrored training code, so it cannot drift from what",
"`job/model/fect/train/functions.R` really asks `model_panel_daily` for.",
"",
"## 1. Keys (every row)",
"",
"The panel grain is **one row per (monitor, local calendar day, partition key)**.",
"",
bullet(required_cols),
"",
"| key | meaning |",
"|---|---|",
"| `fullaqsid` | monitor identity — the FECT *unit*. Stable across the panel; a monitor belongs to exactly one metro for life. |",
"| `date` | the **metro-local** calendar day (not UTC). The FECT *time* index. |",
"| `metro_id` | metro the monitor sits in; used for the metro index and per-metro ATT pooling. |",
"| `partition_key` | `\"<pollutant>-1HR\"` / `\"<pollutant>-24HR\"`. Filtered on, used to prefer 1HR over 24HR in the (monitor, day) dedupe, then dropped before the fit. |",
"",
sprintf("## 2. Outcomes (%d AQ + %d traffic)", n(aq_outcomes), n(traffic_outcomes)),
"",
sprintf("### %d air-quality outcomes", n(aq_outcomes)),
"",
"Daily aggregates of the monitor's pollutant concentration. `mp`/`ep`/`do`/`no`",
"are the morning-peak / evening-peak / daytime-off-peak / night windows;",
"`mean`/`med`/`max` are the within-window statistics. All are fitted on the",
"**sqrt** scale for variance stabilisation (`fect_outcome_formula()`), and",
"back-transformed to native units by Monte-Carlo moments at pooling time.",
"",
bullet(aq_outcomes),
"",
sprintf("### %d traffic outcomes (speeds; identity transform)", n(traffic_outcomes)),
"",
bullet(traffic_outcomes),
"",
"The paper spec is `all_priority` x `aq_daily_mean`. The other outcomes are",
"trained by the same code with the same covariates; the deposit should carry",
"all of them so a replicator can reproduce the robustness grid.",
"",
sprintf("## 3. Covariates — AQ specs (%d time-varying X + %d time-invariant Z)",
        n(aq_covariates), n(aq_z_covariates)),
"",
sprintf("### %d time-varying covariates (X)", n(aq_covariates)),
"",
bullet(aq_covariates),
"",
sprintf("### %d time-invariant covariate(s) (Z)", n(aq_z_covariates)),
"",
bullet(aq_z_covariates),
"",
"Z enters separately because `fect(method = \"cfe\")` partials time-invariant",
"terms out differently from X; under `method = \"mc\"` the same code folds Z into X.",
"",
sprintf("### %d covariates — traffic specs", n(traffic_covariates)),
"",
bullet(traffic_covariates),
"",
"## 4. The exact fetched column set for the paper spec",
"",
sprintf("`fect_spec_panel_cols()` for `all_priority` x `aq_daily_mean`, plus the"),
sprintf("`partition_key` that `fetch_panel()` adds for the dedupe — %d columns:",
        n(paper_cols_fetched)),
"",
paste0("```\n", paste(paper_cols_fetched, collapse = "\n"), "\n```"),
"",
"## 5. Non-panel tables the model also needs",
"",
"`train_fect_bundle()` reads three things the panel does not carry. The deposit",
"must ship them as flat extracts; `replication/run_replication.R` reads them and",
"hands them to the same functions the trainer calls.",
"",
"### 5.1 Treated (zone, monitor, period) pairs — `zone_pairs`",
"",
"Produced privately by `fetch_zone_treated_pairs()` (zone scope; the paper",
"default) or `fetch_metro_treated_pairs()` (metro scope) via PostGIS",
"`ST_Intersects(monitors.geometry, zones.geometry)`. Both return the identical",
"schema, so everything downstream is byte-for-byte unchanged. Deposit columns:",
"",
"- `fullaqsid` — monitor",
"- `metro_id` — metro",
"- `zone_id` — charging zone the monitor falls inside",
"- `policy_id` — `congestion_pricing_periods` row id",
"- `policy_uid` — stable text id (e.g. `LON-ULEZ-2019-CENTRAL`)",
"- `system_type` — `cordon` / `ULEZ` / ... . **The paper's treated set is",
"  `cordon,ULEZ` — both, always.** `cordon` alone silently excludes London's ULEZ.",
"- `date_start`, `date_end` — the active policy window (`date_end` NA = open)",
"",
"Because the pairs are pre-resolved, **a replicator does not need PostGIS or the",
"zone polygons** to reproduce the paper spec. The polygons are still worth",
"depositing for anyone who wants to re-derive the pairs; see",
"`replication/DATAVERSE_DEPOSIT.md`.",
"",
"### 5.2 Policy charging schedules — `policy_schedules`",
"",
"Feeds `fect_untreat_uncharged_days()`. A monitor-day is **treated iff at least",
"one covering (zone, period) pair charges on that weekday** — an EITHER/OR rule",
"across schemes, not a single per-metro weekday map. London is the reason: the",
"CCZ charges Mon-Fri, but the ULEZ charges 7/7 from 2019-04-08, so an in-ULEZ",
"CCZ monitor keeps its weekends and a CCZ-only monitor does not. Deposit columns:",
"",
"- `policy_id`, `policy_uid`, `system_type`",
"- `excluded_wdays` — comma-separated `strftime('%u')` weekday numbers the scheme",
"  does **not** charge (empty = charges 7/7)",
"",
"Non-charging days are **kept** in the panel (outcome and covariates still inform",
"the day-of-week / seasonal / factor structure); only the `treated` flag moves.",
"Treatment therefore becomes non-absorbing where flipped, which `fect`'s `cfe`",
"method handles natively.",
"",
"### 5.3 Metro roster",
"",
"`metro_id -> metro name / country`, for labelling per-metro ATT rows. Cosmetic;",
"the fit does not use it.",
"",
"## 6. What a replicator gets, and what they do not",
"",
"Reproducible from the deposit + the mirrored code: the panel assembly from",
"`fetch_panel()` onward, the treatment encoding, the `fect` fit, the per-cell",
"effects, and the per-metro ATTs.",
"",
"Not reproducible without the private infrastructure: the ETL that BUILDS",
"`model_panel_daily` (AirNow / EPA AQS / EEA ingest, Open-Meteo weather, WorldPop",
"rasters, ACAG satellite PM2.5, OSM road distances, the bg3 background",
"concentration procedure) and the licensed TomTom traffic columns, which are not",
"redistributable. The deposit is the OUTPUT of that ETL.",
""
)

out_path <- file.path(root, "replication", "panel_contract.md")

# Write in BINARY mode so the bytes are LF-only and identical on Windows and
# Linux — otherwise `--check` compares a CRLF file against an LF string and
# reports permanent drift.
new_text <- paste0(paste(md, collapse = "\n"))
if (!endsWith(new_text, "\n")) new_text <- paste0(new_text, "\n")

read_bytes <- function(p) {
  con <- file(p, "rb"); on.exit(close(con))
  rawToChar(readBin(con, "raw", n = file.info(p)$size))
}

if (check) {
  if (!file.exists(out_path)) {
    cat("panel_contract --check: FAIL — ", out_path, " does not exist\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cur <- gsub("\r\n", "\n", read_bytes(out_path), fixed = TRUE)
  if (!identical(cur, new_text)) {
    cat("panel_contract --check: FAIL — panel_contract.md is stale.\n",
        "Regenerate: Rscript replication/generate_panel_contract.R\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("panel_contract --check: OK — matches the training declarations (",
      length(aq_outcomes), " AQ outcomes, ", length(aq_covariates), " X + ",
      length(aq_z_covariates), " Z covariates).\n", sep = "")
  quit(save = "no", status = 0L)
}

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
con <- file(out_path, "wb")
writeBin(charToRaw(new_text), con)
close(con)
cat("wrote ", out_path, " (", length(aq_outcomes), " AQ outcomes, ",
    length(aq_covariates), " X + ", length(aq_z_covariates), " Z covariates, ",
    length(paper_cols_fetched), " fetched columns)\n", sep = "")

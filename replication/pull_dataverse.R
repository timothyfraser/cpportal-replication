# =============================================================================
# pull_dataverse.R — download the deposited panel extract and validate it
# against the panel contract.
#
#   export DATAVERSE_API_KEY=...          # Harvard Dataverse account -> API Token
#   Rscript replication/pull_dataverse.R
#   Rscript replication/pull_dataverse.R --doi doi:10.7910/DVN/XXXXXX
#   Rscript replication/pull_dataverse.R --validate-only
#
# Run from the repository root. Writes into replication/data/, which
# replication/run_replication.R then reads.
#
# THE DOI IS A PLACEHOLDER until the deposit exists. The script refuses to
# invent one: with no --doi and no CPPORTAL_REPLICATION_DOI it stops and tells
# you where the real DOI will be published.
#
# No credential is ever written to disk or echoed. The key is read from the
# environment, sent once as an X-Dataverse-key header, and never logged.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
opt <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (!is.na(i) && length(args) > i) args[[i + 1L]] else default
}
validate_only <- "--validate-only" %in% args
out_dir  <- opt("--out", file.path("replication", "data"))
server   <- opt("--server", Sys.getenv("DATAVERSE_SERVER", "https://dataverse.harvard.edu"))

# --- the DOI, which does not exist yet ---------------------------------------
# Replace this constant when Tim's deposit is published, and update the citation
# block in replication/README.md and replication/DATAVERSE_DEPOSIT.md in the
# same commit.
DOI_PLACEHOLDER <- "doi:10.7910/DVN/PLACEHOLDER"
doi <- opt("--doi", Sys.getenv("CPPORTAL_REPLICATION_DOI", DOI_PLACEHOLDER))

say <- function(...) message("[dataverse] ", ...)

# --- expected files and their required columns -------------------------------
# Column requirements are the KEYS from replication/panel_contract.md. The full
# model column list is enforced by run_replication.R via the real
# fect_spec_panel_cols(), so this stays a cheap structural gate.
EXPECTED <- list(
  list(name = "panel.csv",
       required = c("fullaqsid", "date", "metro_id", "partition_key",
                    "aq_daily_mean")),
  list(name = "zone_pairs.csv",
       required = c("metro_id", "fullaqsid", "policy_id",
                    "start_date", "end_date")),
  list(name = "policy_schedules.csv",
       required = c("policy_id", "policy_uid", "system_type", "metro_id"))
)

validate_dir <- function(dir) {
  ok <- TRUE
  contract <- file.path("replication", "panel_contract.md")
  for (e in EXPECTED) {
    p <- file.path(dir, e$name)
    if (!file.exists(p)) {
      if (file.exists(paste0(p, ".gz"))) {
        p <- paste0(p, ".gz")
      } else if (file.exists(sub("\\.csv$", ".zip", p))) {
        p <- sub("\\.csv$", ".zip", p)
      } else {
        say("MISSING  ", p, " (.gz or .zip)"); ok <- FALSE; next
      }
    }
    hdr <- if (endsWith(p, ".zip")) {
      con <- unz(p, e$name)
      names(utils::read.csv(con, nrows = 1L, stringsAsFactors = FALSE))
    } else {
      names(utils::read.csv(p, nrows = 1L, stringsAsFactors = FALSE))
    }
    miss <- setdiff(e$required, hdr)
    if (length(miss) > 0L) {
      say("BAD      ", p, " — missing column(s): ", paste(miss, collapse = ", "))
      ok <- FALSE
    } else {
      say("OK       ", p, " (", length(hdr), " columns)")
    }
  }
  if (!ok) {
    say("validation FAILED — see ", contract, " for the full column contract")
  }
  ok
}

if (validate_only) {
  quit(save = "no", status = if (validate_dir(out_dir)) 0L else 1L)
}

if (identical(doi, DOI_PLACEHOLDER)) {
  stop(
    "No dataset DOI.\n",
    "  The CP Portal replication deposit has not been published yet, so this\n",
    "  script has nothing to download. When it exists, the DOI will be listed\n",
    "  in replication/README.md; pass it with --doi, set CPPORTAL_REPLICATION_DOI,\n",
    "  or update DOI_PLACEHOLDER in this file.\n",
    "  To exercise the pipeline meanwhile:\n",
    "    Rscript replication/run_replication.R --dry-run",
    call. = FALSE
  )
}

key <- Sys.getenv("DATAVERSE_API_KEY", "")
if (!nzchar(key)) {
  stop("DATAVERSE_API_KEY is not set. Create an API token in your Harvard\n",
       "  Dataverse account (Account -> API Token) and export it. The token is\n",
       "  read from the environment only; nothing writes it to disk.",
       call. = FALSE)
}

if (!requireNamespace("httr2", quietly = TRUE)) {
  stop("package 'httr2' is required: install.packages('httr2')", call. = FALSE)
}

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
say("server=", server, "  doi=", doi, "  out=", out_dir)

req_json <- function(url) {
  httr2::request(url) |>
    httr2::req_headers(`X-Dataverse-key` = key) |>
    httr2::req_user_agent("cpportal-replication") |>
    httr2::req_retry(max_tries = 3L) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

# 1. list the dataset's latest-version files
meta_url <- paste0(server, "/api/datasets/:persistentId/versions/:latest",
                   "?persistentId=", utils::URLencode(doi, reserved = TRUE))
meta <- tryCatch(req_json(meta_url), error = function(e) {
  stop("Dataverse metadata request failed: ", conditionMessage(e),
       "\n  Check the DOI, the server, and that your API token can read it.",
       call. = FALSE)
})

files <- meta$data$files
if (is.null(files) || length(files) == 0L) {
  stop("dataset ", doi, " has no files in its latest version", call. = FALSE)
}
say("dataset version has ", length(files), " file(s)")

`%||%` <- function(a, b) if (is.null(a)) b else a

by_label <- list()
for (f in files) {
  lbl <- f$dataFile$filename %||% f$label
  by_label[[lbl]] <- f$dataFile$id
}

# 2. download the three expected files
for (e in EXPECTED) {
  id <- by_label[[e$name]]
  if (is.null(id)) {
    say("WARNING  '", e$name, "' is not in the deposit — skipping")
    next
  }
  dest <- file.path(out_dir, e$name)
  say("downloading ", e$name, " (dataFile id ", id, ") ...")
  httr2::request(paste0(server, "/api/access/datafile/", id)) |>
    httr2::req_headers(`X-Dataverse-key` = key) |>
    httr2::req_user_agent("cpportal-replication") |>
    httr2::req_retry(max_tries = 3L) |>
    httr2::req_perform(path = dest)
  say("  -> ", dest, " (", file.info(dest)$size, " bytes)")
}

# 3. validate against the contract
if (!validate_dir(out_dir)) quit(save = "no", status = 1L)

say("done. Next: Rscript replication/run_replication.R")

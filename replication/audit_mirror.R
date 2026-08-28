# =============================================================================
# audit_mirror.R — CI-enforced secret/plumbing audit of the mirror allowlist.
#
#   Rscript replication/audit_mirror.R
#
# Run from the repository root. Exits 0 when every allowlisted file is safe to
# publish, non-zero (with the offending file:line) otherwise. The mirror
# workflow runs this BEFORE it pushes anything, so the file-by-file attestation
# in docs/REPLICATION_MIRROR.md is a CI gate rather than a promise someone made
# once in a PR body.
#
# What it forbids, and why the distinction matters:
#
#   * SECRET VALUES and PRIVATE ENDPOINTS — a JWT, a service-role key, a
#     postgres:// URL, an AWS ARN, a *.supabase.co host, the Connect host.
#     These are unconditional failures. None exist today.
#
#   * NEW `.env` READS — `readRenviron()` anywhere outside the single
#     grandfathered occurrence pinned below. The pin is exact: file, count, and
#     enclosing function. If someone adds a second one, or moves the first, the
#     mirror fails and a human has to look.
#
# Env var *names* (`Sys.getenv("PGPASSWORD")`, `Sys.getenv("CONNECT_API_KEY")`)
# are NOT secrets and are not flagged: they leak nothing, and the training code
# is full of `CPPORTAL_FECT_*` knobs a replicator genuinely needs to see.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1L]] else "."

allowlist_path <- file.path(root, "replication", "mirror_allowlist.txt")
if (!file.exists(allowlist_path)) {
  stop("audit_mirror: allowlist not found at ", allowlist_path,
       " (run from the repository root)", call. = FALSE)
}

read_allowlist <- function(path) {
  ln <- readLines(path, warn = FALSE)
  ln <- trimws(ln)
  ln <- ln[nzchar(ln) & !startsWith(ln, "#")]
  unique(ln)
}

paths <- read_allowlist(allowlist_path)

# --- the grandfathered .env read ---------------------------------------------
# job/model/functions.R is the shared model-utility file. 9 of its 14 functions
# are DB-free math the replication runner genuinely calls (add_treatment_zones,
# add_groups, default_outcome_aq, screen_covariates_collinearity, ...). The
# other 5 take a live DBI handle they are GIVEN; only connect_db() builds one,
# and only connect_db() reads job/.env. connect_db() is unreachable from
# replication/run_replication.R (which supplies a Dataverse-built panel and
# never opens a connection), and `readRenviron("job/.env")` is a no-op on a
# public checkout because that file does not exist there.
#
# Factoring connect_db() out would touch 60+ call sites across the repo AND the
# vendored Connect train bundle (_bundled/model_functions.R) — a production risk
# far larger than the zero-byte disclosure it would prevent. So it is pinned
# here instead: exactly one occurrence, in this file, inside connect_db().
ENV_READ_PIN <- list(
  path  = "job/model/functions.R",
  count = 1L,
  fn    = "connect_db"
)

# --- forbidden literal patterns ----------------------------------------------
FORBIDDEN <- list(
  list(id = "jwt",            re = "eyJ[A-Za-z0-9_-]{16,}"),
  list(id = "supabase_key",   re = "sb_secret_[A-Za-z0-9_-]{8,}|sbp_[A-Za-z0-9]{16,}"),
  list(id = "supabase_host",  re = "[A-Za-z0-9-]+\\.supabase\\.(co|in)"),
  list(id = "postgres_url",   re = "postgres(ql)?://[^\"' ]+"),
  list(id = "aws_key",        re = "AKIA[0-9A-Z]{16}"),
  list(id = "aws_arn",        re = "arn:aws[a-z-]*:[a-z0-9-]+:"),
  list(id = "connect_host",   re = "connect\\.systems-apps\\.com"),
  list(id = "bearer_literal", re = "Bearer +[A-Za-z0-9._~+/=-]{16,}"),
  # A secret ASSIGNED a literal, e.g. key = "abc123..." — env reads are fine.
  list(id = "assigned_secret",
       re = paste0("(?i)(password|secret|api[_-]?key|service_role|token)",
                   "\\s*(=|<-)\\s*[\"'][^\"']{12,}[\"']")),
  list(id = "private_ip",     re = "\\b(10|192\\.168|172\\.(1[6-9]|2[0-9]|3[01]))\\.[0-9]{1,3}\\.[0-9]{1,3}\\b")
)

# Patterns that are legitimate matches for `assigned_secret`'s loose regex but
# are obviously not secrets. Kept tiny and explicit on purpose.
BENIGN <- c(
  "REPLICATION_MIRROR_TOKEN",   # the secret's NAME, in prose
  "DATAVERSE_API_KEY"           # ditto
)

fail <- list()
note <- function(path, line_no, id, text) {
  fail[[length(fail) + 1L]] <<- sprintf("  %s:%d  [%s]  %s",
                                        path, line_no, id, trimws(substr(text, 1L, 140L)))
}

missing <- character(0)

for (p in paths) {
  full <- file.path(root, p)
  if (!file.exists(full)) { missing <- c(missing, p); next }
  # Binary-ish / non-text files are not mirrored today; every allowlisted path
  # is text. readLines with warn=FALSE tolerates a missing final newline.
  lines <- tryCatch(readLines(full, warn = FALSE),
                    error = function(e) {
                      note(p, 0L, "unreadable", conditionMessage(e)); character(0)
                    })

  for (f in FORBIDDEN) {
    hits <- grep(f$re, lines, perl = TRUE)
    for (h in hits) {
      if (any(vapply(BENIGN, function(b) grepl(b, lines[[h]], fixed = TRUE),
                     logical(1L)))) next
      note(p, h, f$id, lines[[h]])
    }
  }

  # --- .env reads, pinned ---------------------------------------------------
  # This file is skipped for this rule only: it necessarily names the pattern it
  # searches for, in its own regex and its own comments. Every other allowlisted
  # file — including anything added to replication/ later — is scanned.
  env_hits <- if (identical(p, "replication/audit_mirror.R")) {
    integer(0)
  } else {
    grep("readRenviron", lines, perl = TRUE)
  }
  if (length(env_hits) > 0L) {
    if (!identical(p, ENV_READ_PIN$path)) {
      for (h in env_hits) note(p, h, "unpinned_env_read", lines[[h]])
    } else if (length(env_hits) != ENV_READ_PIN$count) {
      note(p, env_hits[[1L]], "env_read_count_changed",
           sprintf("expected %d readRenviron() in %s, found %d — re-audit before mirroring",
                   ENV_READ_PIN$count, p, length(env_hits)))
    } else {
      # Confirm it is still inside connect_db(): the nearest preceding
      # top-level `name <- function(` / `name = function(` must be the pin.
      defs <- grep("^[A-Za-z._][A-Za-z0-9._]*\\s*(<-|=)\\s*function", lines, perl = TRUE)
      before <- defs[defs < env_hits[[1L]]]
      owner <- if (length(before) == 0L) "<file top level>" else
        sub("\\s*(<-|=).*$", "", trimws(lines[[max(before)]]))
      if (!identical(owner, ENV_READ_PIN$fn)) {
        note(p, env_hits[[1L]], "env_read_moved",
             sprintf("readRenviron() is now inside '%s', pinned to '%s'", owner, ENV_READ_PIN$fn))
      }
    }
  }
}

if (length(missing) > 0L) {
  cat("audit_mirror: FAIL — allowlisted paths do not exist:\n")
  cat(paste0("  ", missing, collapse = "\n"), "\n", sep = "")
}

if (length(fail) > 0L) {
  cat("audit_mirror: FAIL — ", length(fail), " finding(s):\n", sep = "")
  cat(paste(unlist(fail), collapse = "\n"), "\n", sep = "")
}

if (length(missing) > 0L || length(fail) > 0L) {
  quit(save = "no", status = 1L)
}

cat("audit_mirror: OK — ", length(paths),
    " allowlisted file(s) clean (no secret values, no private endpoints,",
    " no unpinned .env reads).\n", sep = "")

# =============================================================================
# no_pin_guard.R — make a replication run STRUCTURALLY UNABLE to write a pin
# or touch the private database.
#
#   source("replication/no_pin_guard.R")   # BEFORE the training code is sourced
#   install_no_pin_guard()                 # AFTER it is sourced
#   assert_no_pin_possible("pre-fit")      # any time
#
# -----------------------------------------------------------------------------
# WHY THIS FILE EXISTS
# -----------------------------------------------------------------------------
# A replication run sources the REAL trainer. The real trainer contains
# `pick_board()` (which calls `pins::board_connect()` when CONNECT_SERVER and
# CONNECT_API_KEY are set) and `write_bundle()` (which calls `pins::pin_write()`).
# Neither is on `run_replication.R`'s call path — but "not on the call path" is
# an argument, and the repo has been bitten before by exactly the class of
# footgun that turns an argument into a production write:
#
#   docs/model/TIMING.md, rule 1: `connect_db()` (job/model/functions.R:250)
#   re-reads the private env file, which RESTORES CONNECT_SERVER after a caller
#   unset it — so a "local" benchmark silently wrote a PRODUCTION pin.
#
# Unsetting the env var is therefore not sufficient by itself. This guard makes
# the write impossible at four independent levels; any ONE of them alone would
# stop it, and level 3 is the one that survives the .env footgun.
#
#   1. ENV       CONNECT_* unset, and CPPORTAL_REPLICATION_RUN=1 set.
#   2. NAMESPACE pins::board_connect and pins::pin_write are replaced, in the
#                pins namespace, with functions that stop(). Nothing in the
#                process can construct a Connect board or write a pin — not the
#                trainer, not a transitive call, not a typo.
#   3. TRAINER   connect_db(), pick_board(), write_bundle() are redefined in the
#                global environment to stop() immediately. Because they are
#                redefined AFTER the trainer is sourced, the .env-restores-
#                CONNECT_SERVER footgun cannot fire: connect_db never runs.
#   4. ASSERT    assert_no_pin_possible() re-checks the env AND the namespace
#                shims at chosen points (before and after the fit), so a
#                mid-run restoration is caught rather than assumed away.
# =============================================================================

.NO_PIN_STOP <- function(what) {
  function(...) {
    stop("[no-pin-guard] ", what, " is DISABLED in a replication run ",
         "(CPPORTAL_REPLICATION_RUN=1). A replication must never write a pin ",
         "or open the private database. If you meant to retrain, run ",
         "job/model/fect/train/job.R on Connect instead.", call. = FALSE)
  }
}

#' Level 1 — call this BEFORE sourcing the training code.
arm_no_pin_guard <- function() {
  Sys.setenv(CPPORTAL_REPLICATION_RUN = "1")
  Sys.unsetenv(c("CONNECT_SERVER", "CONNECT_API_KEY", "CONNECT_API_KEY_ADMIN",
                 "POSIT_CONNECT_API_KEY", "CONNECT_CONTENT_GUID"))
  message("[no-pin-guard] armed: CPPORTAL_REPLICATION_RUN=1, CONNECT_* unset")
  invisible(TRUE)
}

#' Levels 2 + 3 — call this AFTER sourcing the training code.
install_no_pin_guard <- function(env = globalenv()) {
  # 2. namespace level
  if (requireNamespace("pins", quietly = TRUE)) {
    for (fn in c("board_connect", "pin_write")) {
      ok <- tryCatch({
        utils::assignInNamespace(fn, .NO_PIN_STOP(paste0("pins::", fn)),
                                 ns = "pins")
        TRUE
      }, error = function(e) FALSE)
      message("[no-pin-guard] pins::", fn, " shimmed: ", ok)
      if (!ok) {
        stop("[no-pin-guard] could NOT shim pins::", fn,
             " — refusing to run: the guard cannot be proven.", call. = FALSE)
      }
    }
  } else {
    message("[no-pin-guard] package 'pins' not installed — nothing to shim ",
            "(a pin write is impossible by absence)")
  }

  # 3. trainer level
  for (fn in c("connect_db", "pick_board", "write_bundle")) {
    assign(fn, .NO_PIN_STOP(paste0(fn, "()")), envir = env)
    message("[no-pin-guard] ", fn, "() shimmed in ", environmentName(env))
  }
  invisible(TRUE)
}

#' Level 4 — re-check at any point. Cheap; call it liberally.
assert_no_pin_possible <- function(stage = "") {
  tag <- paste0("[no-pin-guard][", stage, "] ")

  for (v in c("CONNECT_SERVER", "CONNECT_API_KEY", "CONNECT_API_KEY_ADMIN")) {
    if (nzchar(Sys.getenv(v))) {
      stop(tag, v, " is SET. Something restored it mid-run (the classic cause ",
           "is connect_db() re-reading job/.env). Aborting before anything can ",
           "reach a Connect board.", call. = FALSE)
    }
  }

  if (!identical(Sys.getenv("CPPORTAL_REPLICATION_RUN"), "1")) {
    stop(tag, "CPPORTAL_REPLICATION_RUN is no longer 1.", call. = FALSE)
  }

  if (requireNamespace("pins", quietly = TRUE)) {
    for (fn in c("board_connect", "pin_write")) {
      f <- get(fn, envir = asNamespace("pins"))
      shimmed <- tryCatch({
        f(); FALSE                      # a live function would not stop()
      }, error = function(e) grepl("no-pin-guard", conditionMessage(e)))
      if (!isTRUE(shimmed)) {
        stop(tag, "pins::", fn, " is NOT shimmed any more.", call. = FALSE)
      }
    }
  }

  message(tag, "OK — CONNECT_* empty, pins::board_connect and pins::pin_write ",
          "both refuse to run")
  invisible(TRUE)
}

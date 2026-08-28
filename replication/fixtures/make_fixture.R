# =============================================================================
# make_fixture.R — build the tiny SYNTHETIC dry-run panel.
#
#   Rscript replication/fixtures/make_fixture.R     # from the repository root
#
# Writes three CSVs next to this file:
#   panel.csv             ~200 monitor-days, the exact columns of the paper spec
#   zone_pairs.csv        treated (monitor, zone, policy) pairs
#   policy_schedules.csv  per-policy charging weekdays
#
# EVERY VALUE IS FABRICATED. No observation, monitor id, coordinate, or ATT here
# comes from the real database — this fixture exists only to prove that the
# MIRRORED code path executes end-to-end outside the private infrastructure. Its
# numbers are meaningless; do not cite them.
#
# The column list is taken from replication/panel_contract.md, which is itself
# generated from the real training declarations, so a covariate added upstream
# makes the fixture fail loudly instead of silently under-specifying the panel.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

set.seed(20260828L)

here <- file.path("replication", "fixtures")
if (!dir.exists(here)) stop("run from the repository root", call. = FALSE)

# --- columns, read from the generated contract (never hand-copied) -----------
contract <- readLines(file.path("replication", "panel_contract.md"), warn = FALSE)
fence <- grep("^```$", contract)
sec4 <- grep("^## 4\\.", contract)
if (length(sec4) != 1L || length(fence) < 2L) {
  stop("make_fixture: cannot locate the column block in panel_contract.md; ",
       "regenerate it with Rscript replication/generate_panel_contract.R",
       call. = FALSE)
}
open <- min(fence[fence > sec4]); close_ <- min(fence[fence > open])
cols <- trimws(contract[(open + 1L):(close_ - 1L)])
cols <- cols[nzchar(cols)]
message("[fixture] contract columns: ", length(cols))

# --- panel skeleton: 2 metros x 5 monitors x 20 days -------------------------
# Metro 943 is TREATED (a Mon-Fri cordon plus a 7/7 scheme), metro 999 is a pure
# control. 943 is London's real metro_id: it is used here ONLY so the fixture
# exercises the real either/or branch of fect_untreat_uncharged_days() (London's
# CCZ charges Mon-Fri, its ULEZ charges 7/7, so in-ULEZ monitors keep their
# weekends and cordon-only monitors do not). 999 is deliberately absent from
# FECT_CHARGED_DAYS_EXCLUDED_WDAYS. Every monitor id and every value below is
# fabricated.
TREATED_METRO <- 943L
CONTROL_METRO <- 999L
metros    <- c(TREATED_METRO, CONTROL_METRO)
per_metro <- 5L
n_days    <- 20L
dates <- seq(as.Date("2019-03-25"), by = "day", length.out = n_days)

units <- tidyr::expand_grid(metro_id = metros, k = seq_len(per_metro)) |>
  mutate(fullaqsid = sprintf("FIX-%03d-%02d", .data$metro_id, .data$k)) |>
  select("metro_id", "fullaqsid")

panel <- tidyr::expand_grid(units, date = dates)
n <- nrow(panel)
message("[fixture] monitor-days: ", n)

# --- fabricated covariates ---------------------------------------------------
# Plausible ranges only; the point is that the fit runs, not that it means
# anything. Covariates are given genuine cross-unit and cross-day variation so
# fect's two-way FE design matrix is not singular.
u <- as.integer(factor(panel$fullaqsid))
d <- as.integer(panel$date - min(panel$date))

fab <- list(
  temp_daily_mean                          = 12 + 0.10 * d + 0.6 * u + rnorm(n, 0, 1.0),
  rhum_daily_mean                          = 62 + 0.20 * sin(d / 3) * 10 + rnorm(n, 0, 3),
  precip_daily_mean                        = pmax(0, rnorm(n, 1.2, 1.0)),
  ws_daily_mean                            = pmax(0.2, 3.0 + rnorm(n, 0, 0.8)),
  wd_daily_mean_deg                        = runif(n, 0, 359),
  barpr_daily_mean                         = 1012 + rnorm(n, 0, 3),
  population                               = 5000 + 900 * u + rnorm(n, 0, 50),
  bg3                                      = pmax(0.5, 6 + 0.3 * sin(d / 5) + rnorm(n, 0, 0.4)),
  sat_monthly_mean                         = pmax(0.5, 9 + 0.05 * d + rnorm(n, 0, 0.3)),
  metro_pop_density_year                   = ifelse(panel$metro_id == TREATED_METRO, 5200, 3100) + rnorm(n, 0, 20),
  dist_km_motorway_trunk_primary_secondary = rep(round(runif(length(unique(u)), 0.1, 4.0), 3), each = n_days)
)
for (nm in names(fab)) panel[[nm]] <- fab[[nm]]

panel$partition_key <- "PM2.5-1HR"

# --- fabricated outcome ------------------------------------------------------
# A unit effect + a day effect + covariate loadings + a real (negative) policy
# effect on treated metro-1 monitors from the policy start, + noise. The fit
# should be able to see something; whether it recovers this number is not the
# test — executing the mirrored path is.
policy_start <- as.Date("2019-04-01")
true_effect  <- -1.5
post_treated <- panel$metro_id == TREATED_METRO & panel$date >= policy_start

panel$aq_daily_mean <- pmax(
  0.5,
  10 + 0.8 * u - 0.05 * d +
    0.15 * panel$temp_daily_mean +
    0.30 * panel$bg3 -
    0.40 * panel$ws_daily_mean +
    true_effect * post_treated +
    rnorm(n, 0, 0.5)
)

missing_cols <- setdiff(cols, names(panel))
if (length(missing_cols) > 0L) {
  stop("make_fixture: the panel contract gained columns this fixture does not ",
       "fabricate: ", paste(missing_cols, collapse = ", "),
       ". Add them here, then regenerate.", call. = FALSE)
}
panel <- panel |> select(dplyr::all_of(cols))

# --- treated pairs -----------------------------------------------------------
# Schema is exactly what fetch_zone_treated_pairs() returns:
#   metro_id, fullaqsid, policy_id, start_date, end_date
# All five treated monitors are covered by the Mon-Fri cordon; monitors 01-03
# are ALSO covered by a 7/7 scheme, so they keep their weekends while 04-05 lose
# theirs. A miniature of the real London CCZ-vs-ULEZ either/or case.
treated_units <- units |> filter(.data$metro_id == TREATED_METRO) |> pull("fullaqsid")

zone_pairs <- bind_rows(
  tibble::tibble(fullaqsid = treated_units,      policy_id = 9001L),
  tibble::tibble(fullaqsid = treated_units[1:3], policy_id = 9002L)
) |>
  mutate(
    metro_id   = TREATED_METRO,
    start_date = policy_start,
    end_date   = as.Date(NA)
  ) |>
  select("metro_id", "fullaqsid", "policy_id", "start_date", "end_date")

# --- policy rows -------------------------------------------------------------
# Schema is exactly what fect_policy_schedules() returns:
#   policy_id, policy_uid, system_type, metro_id
# The weekday schedule itself is NOT data — it is resolved in code from
# system_type (ULEZ => 7/7 via FECT_CHARGED_DAYS_7_7_SYSTEM_TYPES) and the
# metro's cordon map (FECT_CHARGED_DAYS_EXCLUDED_WDAYS: 943 => Sat+Sun).
policy_schedules <- tibble::tibble(
  policy_id   = c(9001L, 9002L),
  policy_uid  = c("FIX-CORDON-2019", "FIX-ULEZ-2019"),
  system_type = c("cordon", "ULEZ"),
  metro_id    = TREATED_METRO
)

readr_write <- function(x, name) {
  p <- file.path(here, name)
  utils::write.csv(x, p, row.names = FALSE, na = "")
  message("[fixture] wrote ", p, "  rows=", nrow(x), " cols=", ncol(x))
}

readr_write(panel, "panel.csv")
readr_write(zone_pairs, "zone_pairs.csv")
readr_write(policy_schedules, "policy_schedules.csv")

message("[fixture] done — SYNTHETIC data only, not derived from the cpportal database.")

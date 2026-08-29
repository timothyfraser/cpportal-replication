# =============================================================================
# make_paper_values.R — generate replication/paper_values.json from
# results/results.json.
#
#   Rscript replication/make_paper_values.R
#
# WHY GENERATED, NEVER TYPED
# --------------------------
# `results/results.json` is the ONE artifact every published number comes from:
# the `\Stat{}` macros in the .tex, build_slides.R, and the paper figures all
# read it (see .claude/skills/paper-figure/SKILL.md, "no typed numerals"). If
# the replication's comparison targets were transcribed by hand they would drift
# from the paper the first time a retrain moved a number, and the reproduction
# report would silently start comparing against a value the paper no longer
# prints. So the targets are EXTRACTED, with the same provenance metadata the
# stat carries (endpoint, metro_id, basis, estimand), and the extraction is
# re-runnable.
#
# Every value here is already public: results.json is what the manuscript
# prints. Nothing private is copied out.
# =============================================================================

suppressPackageStartupMessages({ library(jsonlite) })

say <- function(...) message("[paper-values] ", ...)

RESULTS <- "results/results.json"
OUT     <- "replication/paper_values.json"
if (!file.exists(RESULTS)) stop("run from the repository root; missing ", RESULTS, call. = FALSE)

rj    <- jsonlite::fromJSON(RESULTS, simplifyVector = FALSE)
meta  <- rj$meta
stats <- rj$stats
say("read ", length(stats), " stats  (pin_version=", meta$pin_version,
    ", date_to=", meta$date_to, ")")

# The groups the replication is trying to reproduce, and what each one is.
# Adding a group here is how coverage grows; a group NOT listed shows up in
# `gaps` below so the mapping table can never silently omit it.
GROUPS <- list(
  t1 = list(prefix = "t1.", label = "Table 1 — per-city headline ATT / CI / pct"),
  t2 = list(prefix = "t2.", label = "Table 2 — per-city + per-era ATT, pooled rows"),
  t4 = list(prefix = "t4.", label = "Table 4 — year-horizon Y1..Yk pooled ATT")
)

pick <- function(prefix) {
  keys <- grep(paste0("^", prefix), names(stats), value = TRUE)
  out <- list()
  for (k in sort(keys)) {
    s <- stats[[k]]
    out[[k]] <- list(
      key         = k,
      value       = s$value,
      formatted   = s$formatted,
      macro       = s$macro,
      unit        = s$unit,
      description = s$description,
      source      = s$source
    )
  }
  out
}

groups <- list()
for (g in names(GROUPS)) {
  v <- pick(GROUPS[[g]]$prefix)
  groups[[g]] <- list(label = GROUPS[[g]]$label, n = length(v), values = v)
  say("group ", g, ": ", length(v), " values")
}

covered  <- unlist(lapply(GROUPS, `[[`, "prefix"))
all_pref <- sort(unique(vapply(strsplit(names(stats), ".", fixed = TRUE),
                               function(x) paste0(x[[1]], "."), character(1))))
uncovered <- setdiff(all_pref, covered)

payload <- list(
  generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  generator    = "replication/make_paper_values.R",
  source       = RESULTS,
  provenance   = list(
    pin_version   = meta$pin_version,
    spec_id       = meta$spec_id,
    pollutant     = meta$pollutant,
    date_from     = meta$date_from,
    date_to       = meta$date_to,
    att_estimand  = meta$att_estimand,
    anchored_method = meta$anchored_method
  ),
  # What the replication run CAN and CANNOT be compared against. Read this
  # before writing a verdict column: a mismatch on a row listed here as a
  # known gap is a scope line, not a failed reproduction.
  comparability = list(
    fitted_only = paste(
      "The replication run is FITTED-ONLY. Anchored (M10) cities — Oslo (956)",
      "at zone scope — have no fitted per-monitor-day row and cannot be",
      "reproduced from it."),
    charged_days = paste(
      "Published per-city ATTs are pooled over CHARGED DAYS ONLY. The run's",
      "pinned per_metro row is an ALL-DAYS pool; the comparable quantity is",
      "the D2b pool over its per_day_metro_unit cells with the charged-days",
      "cut applied."),
    serving_side = paste(
      "Windowed / per-era rows (Table 1 London eras, Table 4 Y1..Yk) come from",
      "metro_window_estimate() / combine_att_window_d2b() in",
      "app/v2/api/R/cross_city_att.R, which is NOT on the mirror allowlist",
      "today. Closing this means MIRRORING that file, never re-implementing",
      "the pool.")
  ),
  groups = groups,
  gaps   = list(
    uncovered_prefixes = as.list(uncovered),
    note = paste(
      "These results.json prefixes are not yet targets of the replication",
      "comparison. Each must appear in replication/README.md's mapping table",
      "with a script+output or an explicit NOT-YET line and a reason.")
  )
)

jsonlite::write_json(payload, OUT, auto_unbox = TRUE, pretty = TRUE, null = "null")
say("wrote ", OUT, "  groups=", length(groups),
    "  values=", sum(vapply(groups, `[[`, integer(1), "n")),
    "  uncovered_prefixes=", length(uncovered))

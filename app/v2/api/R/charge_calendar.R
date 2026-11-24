# =============================================================================
# charge_calendar.R -- THE charged-days resolver (one file, three bundles)
# =============================================================================
# SOURCE OF TRUTH: app/v2/api/R/charge_calendar.R (this file) and its data,
# app/v2/api/data/charge_calendar.csv (+ charge_calendar.bib). The API bundle
# can only ship app/v2/api/**, so both live here and are VENDORED, never
# hand-edited, into:
#   * job/model/fect/train/_bundled/        (Rscript job/model/fect/train/setup.R)
#   * job/model/breathe/train/_bundled/     (Rscript job/model/breathe/train/setup.R)
#   * job/cache_publish/R/ + data/          (Rscript job/cache_publish/setup.R)
#
# The question it answers: was scheme S (or, at metro grain, ANY scheme of metro
# M) charging on day D? Callers:
#   * train, per (zone, period) pair -- fect_untreat_uncharged_days():
#       charged = coalesce(charge_calendar_status(pair, day), weekday_rule(pair))
#   * train/diagnostic metro grain -- fect_charged_days_metro_uncharged()
#   * serving metro grain -- cpportal_charged_days_filter()
#   * BREATHE -- through the same train flip (job/model/breathe/R/charged_days.R).
#
# RULE (Tim, 2026-09-30): inside a policy window every day is CHARGED unless a
# source says that specific day (or that weekday over a stated window) was not.
# So only `evidence == "conclusive"` rows may make a day uncharged. A
# `charged` row may also be `partial` (uncertain => charged is the default
# anyway, and such a row overrides a weekday assumption in the conservative
# direction). `inconclusive` rows are inert: they document a gap, never encode
# one. `rate_zero` counts as uncharged (Tim's ruling; the data file only carries
# rate_zero where EVERY gantry covering the zone was zero), switchable for
# diagnostics only via CPPORTAL_CHARGED_RATE_ZERO_IS_UNCHARGED=0.
#
# A MISSING OR MALFORMED CALENDAR IS A HARD ERROR. There is no silent
# "everything charged" fallback: that would quietly change the estimand.
# =============================================================================

CHARGE_CALENDAR_COLUMNS = c("metro_id", "scope", "from", "to", "weekdays",
                            "status", "rule_type", "precedence", "claim",
                            "evidence", "cite_keys", "quote", "added", "by")
CHARGE_CALENDAR_STATUSES  = c("charged", "uncharged", "rate_zero", "suspended")
CHARGE_CALENDAR_EVIDENCE  = c("conclusive", "partial", "inconclusive")
CHARGE_CALENDAR_OPEN_END  = as.Date("9999-12-31")

# -----------------------------------------------------------------------------
# WEEKDAY FALLBACK SCHEDULE -- used where no calendar row speaks about a day.
# Excluded ISO weekdays (1=Mon .. 7=Sun) per metro. A metro absent from the
# list charges every day (NYC, Oslo, Bergen). Bergen (958) was `6,7` until
# 2026-09-30: that weekend exemption was an unsourced assumption and is now
# carried, with its sources, by the calendar (weekends 1986-01-02..2004-02-01
# only). Gothenburg (957) shares Stockholm's statute (Lag 2004:629).
# -----------------------------------------------------------------------------
CHARGE_CALENDAR_WEEKDAY_EXCLUDED = list(
  `943` = c(6L, 7L),  # London CCZ              -- weekends not charged (pre-2020-06-22; calendar overrides after)
  `949` = c(6L, 7L),  # Stockholm congestion tax -- weekends not charged
  `950` = c(6L, 7L),  # Milan Area C/Ecopass    -- weekends not charged
  `955` = 7L,         # Singapore ERP           -- Sundays not charged
  `957` = c(6L, 7L)   # Gothenburg congestion tax -- weekends not charged
)

#' `system_type` values that charge SEVEN DAYS A WEEK (weekday fallback =
#' exclude nothing). London's ULEZ runs every day except 25 December; the
#' 25 December exemption is a calendar row.
CHARGE_CALENDAR_7_7_SYSTEM_TYPES = c("ULEZ")

# -----------------------------------------------------------------------------
# METRO-GRAIN SCHEMES -- for callers that see only (metro_id, day). A metro-day
# is uncharged iff EVERY scheme of the metro in force that day is uncharged
# (Tim 2026-08-09, either/or). A metro absent from this list is ONE scheme that
# every calendar row of the metro speaks for. London is the only metro that
# hosts two schemes on different schedules at once; its split date is the ULEZ
# start (period 43, 2019-04-08), the same boundary the pre-2026-09-30 segment
# literal carried. `scope` matches calendar rows by policy_uid prefix.
#
# RESIDUAL, STATED LOUDLY: 2019-04-08..2021-10-24 five CCZ monitors sat outside
# the central ULEZ; the metro grain counts their weekends as charged (485
# monitor-days on three monitors). The per-pair train flip is exact.
# -----------------------------------------------------------------------------
CHARGE_CALENDAR_METRO_SCHEMES = list(
  `943` = list(
    list(scope = "LON-CCZ",  from = as.Date("1900-01-01"), to = CHARGE_CALENDAR_OPEN_END,
         system_type = "cordon"),
    list(scope = "LON-ULEZ", from = as.Date("2019-04-08"), to = CHARGE_CALENDAR_OPEN_END,
         system_type = "ULEZ")
  )
)

.charge_calendar_cache = new.env(parent = emptyenv())

#' Candidate locations of charge_calendar.csv, first hit wins.
charge_calendar_candidates = function() {
  c(Sys.getenv("CPPORTAL_CHARGE_CALENDAR_PATH", ""),
    "data/charge_calendar.csv",                          # API / cache_publish on Connect
    "_bundled/charge_calendar.csv",                      # FECT / BREATHE train on Connect
    "app/v2/api/data/charge_calendar.csv",               # repo root (source of truth)
    "../data/charge_calendar.csv",                       # cwd = app/v2/api/R or app/v2/api/tests
    "job/model/fect/train/_bundled/charge_calendar.csv") # repo root, vendored copy
}

#' Parse a weekdays cell: "all", "1-5", "6,7", "1-5,7". Errors on anything else.
charge_calendar_parse_weekdays = function(x) {
  x = gsub("\\s+", "", tolower(as.character(x)))
  lapply(x, function(s) {
    if (is.na(s) || !nzchar(s) || s == "all") return(1:7)
    out = integer(0)
    for (tok in strsplit(s, ",", fixed = TRUE)[[1]]) {
      if (grepl("^[1-7]$", tok)) {
        out = c(out, as.integer(tok))
      } else if (grepl("^[1-7]-[1-7]$", tok)) {
        ab = as.integer(strsplit(tok, "-", fixed = TRUE)[[1]])
        if (ab[1] > ab[2]) stop("charge_calendar: bad weekdays range '", s, "'", call. = FALSE)
        out = c(out, seq.int(ab[1], ab[2]))
      } else {
        stop("charge_calendar: bad weekdays value '", s,
             "' (use all, ISO 1-7, ranges a-b, comma lists)", call. = FALSE)
      }
    }
    sort(unique(out))
  })
}

#' Default precedence by rule_type when the cell is empty
#' (holiday 30 > suspension 20 > scheme_change 15 > schedule 10).
.charge_calendar_default_precedence = function(rule_type) {
  p = c(holiday = 30L, suspension = 20L, scheme_change = 15L, schedule = 10L)
  out = unname(p[as.character(rule_type)])
  out[is.na(out)] = 10L
  out
}

#' Validate + normalise a raw calendar data.frame (all-character columns).
charge_calendar_prepare = function(raw, path = "<data.frame>") {
  miss = setdiff(CHARGE_CALENDAR_COLUMNS, names(raw))
  if (length(miss)) {
    stop("charge_calendar: ", path, " is missing column(s): ",
         paste(miss, collapse = ", "), call. = FALSE)
  }
  n = nrow(raw)
  chr = function(v) { v = as.character(v); v[is.na(v)] = ""; trimws(v) }
  cal = data.frame(
    metro_id  = suppressWarnings(as.integer(chr(raw$metro_id))),
    scope     = chr(raw$scope),
    from      = suppressWarnings(as.Date(chr(raw$from))),
    to        = suppressWarnings(as.Date(ifelse(chr(raw$to) == "", NA, chr(raw$to)))),
    status    = tolower(chr(raw$status)),
    rule_type = tolower(chr(raw$rule_type)),
    precedence = suppressWarnings(as.integer(chr(raw$precedence))),
    evidence  = tolower(chr(raw$evidence)),
    stringsAsFactors = FALSE
  )
  bad = function(ok, what) {
    if (any(!ok)) stop("charge_calendar: ", path, ": ", what, " (rows ",
                       paste(utils::head(which(!ok) + 1L, 10), collapse = ","),
                       ")", call. = FALSE)
  }
  bad(is.finite(cal$metro_id), "metro_id not an integer")
  bad(nzchar(cal$scope), "empty scope")
  bad(!is.na(cal$from), "from is not a date")
  to_raw = chr(raw$to)
  bad(to_raw == "" | !is.na(cal$to), "to is not a date (leave empty for open)")
  cal$to[is.na(cal$to)] = CHARGE_CALENDAR_OPEN_END
  bad(cal$to >= cal$from, "to < from")
  bad(cal$status %in% CHARGE_CALENDAR_STATUSES, "unknown status")
  bad(cal$evidence %in% CHARGE_CALENDAR_EVIDENCE, "unknown evidence grade")
  np = is.na(cal$precedence)
  cal$precedence[np] = .charge_calendar_default_precedence(cal$rule_type[np])
  cal$weekdays = charge_calendar_parse_weekdays(chr(raw$weekdays))
  # Inert rows are dropped here, once: inconclusive never encodes; an
  # uncharged-type status needs conclusive evidence (uncertain => charged).
  cal$active = cal$evidence == "conclusive" |
    (cal$evidence == "partial" & cal$status == "charged")
  attr(cal, "path") = path
  attr(cal, "n_rows") = n
  cal
}

#' Load (and cache) the charge calendar. HARD ERROR if the file is missing.
#'
#' @param path Explicit path; NULL searches `charge_calendar_candidates()`.
#' @param refresh Re-read even if cached.
charge_calendar_load = function(path = NULL, refresh = FALSE) {
  # PERF (2026-10-01): the candidate search (file.exists + normalizePath) ran
  # on EVERY resolver call -- 0.2 s of one London /att call. The resolved file
  # is remembered per process, keyed on the CPPORTAL_CHARGE_CALENDAR_PATH
  # override + the working directory, so moving either re-resolves, and
  # `refresh = TRUE` always re-resolves and re-reads. (A loaded calendar was
  # already served from the cache without re-reading the file; this only
  # stops re-searching for it.)
  rkey = if (is.null(path)) paste0(Sys.getenv("CPPORTAL_CHARGE_CALENDAR_PATH", ""), "|", getwd())
  if (is.null(path) && !refresh) {
    res = .charge_calendar_cache[[".resolved"]]
    if (!is.null(res) && identical(res$rkey, rkey) && !is.null(.charge_calendar_cache[[res$key]])) {
      return(.charge_calendar_cache[[res$key]])
    }
  }
  if (is.null(path)) {
    cands = charge_calendar_candidates()
    cands = cands[nzchar(cands)]
    hit = cands[file.exists(cands)]
    if (!length(hit)) {
      stop("charge_calendar: charge_calendar.csv NOT FOUND (looked in: ",
           paste(cands, collapse = " | "), ", cwd=", getwd(), "). Refusing to ",
           "treat every day as charged -- vendor it (setup.R) or set ",
           "CPPORTAL_CHARGE_CALENDAR_PATH.", call. = FALSE)
    }
    path = hit[1]
  } else if (!file.exists(path)) {
    stop("charge_calendar: charge_calendar.csv NOT FOUND at ", path, call. = FALSE)
  }
  key = normalizePath(path, winslash = "/", mustWork = TRUE)
  if (!is.null(rkey)) {
    assign(".resolved", list(rkey = rkey, key = key), envir = .charge_calendar_cache)
  }
  if (!refresh && !is.null(.charge_calendar_cache[[key]])) return(.charge_calendar_cache[[key]])
  raw = utils::read.csv(path, colClasses = "character", na.strings = character(0),
                        check.names = FALSE, strip.white = TRUE,
                        stringsAsFactors = FALSE, encoding = "UTF-8")
  cal = charge_calendar_prepare(raw, path = key)
  assign(key, cal, envir = .charge_calendar_cache)
  cal
}

.charge_calendar_rate_zero_uncharged = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_CHARGED_RATE_ZERO_IS_UNCHARGED", "")))
  !(v %in% c("0", "false", "f", "no", "n", "off"))
}

#' Does a row scope speak for `policy_uid`? `metro` matches everything; else a
#' `;`-separated list of policy_uids, each matching exactly or as a prefix
#' followed by "-" in either direction (row `LON-CCZ` covers `LON-CCZ-CORE-2011`;
#' metro-grain scheme `LON-CCZ` is covered by row `LON-CCZ-2003`). `policy_uid`
#' NA matches only `metro` rows; `policy_uid = "*"` matches every row.
charge_calendar_scope_match = function(scope, policy_uid) {
  if (identical(scope, "metro")) return(TRUE)
  if (is.na(policy_uid) || !nzchar(policy_uid)) return(FALSE)
  if (identical(policy_uid, "*")) return(TRUE)
  toks = trimws(strsplit(scope, ";", fixed = TRUE)[[1]])
  any(toks == policy_uid | startsWith(policy_uid, paste0(toks, "-")) |
        startsWith(toks, paste0(policy_uid, "-")))
}

#' Calendar status of (metro, scheme, day) -- NA where no active row speaks.
#'
#' @param metro_id Scalar or parallel to `dates`.
#' @param policy_uid Scalar policy_uid of the scheme (pair grain). NA matches
#'   only `metro`-scoped rows. NULL means METRO GRAIN: the OR over the metro's
#'   schemes with the weekday fallback applied, never NA (see
#'   `charge_calendar_metro_uncharged()`).
#' @param dates Date vector.
#' @param cal `charge_calendar_load()` output.
#' @return factor(levels = c("charged", "uncharged")), NA where no row speaks.
charge_calendar_status = function(metro_id, policy_uid, dates,
                                  cal = charge_calendar_load()) {
  dts = suppressWarnings(as.Date(dates))
  n = length(dts)
  if (is.null(policy_uid)) {
    unch = charge_calendar_metro_uncharged(rep_len(metro_id, n), dts, cal = cal)
    return(factor(ifelse(unch, "uncharged", "charged"), levels = c("charged", "uncharged")))
  }
  mids = rep_len(suppressWarnings(as.integer(metro_id)), n)
  puid = as.character(policy_uid)[1]
  out  = rep(NA_character_, n)
  ok   = is.finite(mids) & !is.na(dts)
  rz_unch = .charge_calendar_rate_zero_uncharged()
  act = cal[cal$active, , drop = FALSE]
  for (m in intersect(unique(mids[ok]), unique(act$metro_id))) {
    sel = which(ok & mids == m)
    # resolve on the UNIQUE days only, then scatter back
    ud = sort(unique(dts[sel]))
    rows = act[act$metro_id == m & act$to >= ud[1] & act$from <= ud[length(ud)], , drop = FALSE]
    if (!nrow(rows)) next
    keep = vapply(rows$scope, charge_calendar_scope_match, logical(1), policy_uid = puid)
    rows = rows[keep, , drop = FALSE]
    if (!nrow(rows)) next
    # Apply rows in ascending precedence so the highest wins; within a tie the
    # `charged` rows go last, so an unresolved tie reads CHARGED (the default).
    rows = rows[order(rows$precedence, rows$status == "charged"), , drop = FALSE]
    uw = charge_calendar_iso_wday(ud)
    uout = rep(NA_character_, length(ud))
    for (r in seq_len(nrow(rows))) {
      lo = findInterval(rows$from[r] - 1, ud) + 1L    # first ud >= from
      hi = findInterval(rows$to[r], ud)               # last  ud <= to
      if (hi < lo) next
      idx = lo:hi
      idx = idx[uw[idx] %in% rows$weekdays[[r]]]
      if (!length(idx)) next
      st = rows$status[r]
      uout[idx] = if (st == "charged") "charged"
                  else if (st == "rate_zero" && !rz_unch) "charged"
                  else "uncharged"
    }
    out[sel] = uout[match(dts[sel], ud)]
  }
  factor(out, levels = c("charged", "uncharged"))
}

#' Weekday-fallback excluded weekdays for one scheme.
charge_calendar_fallback_excluded = function(metro_id, system_type = NA_character_) {
  st = as.character(system_type)[1]
  if (!is.na(st) && st %in% CHARGE_CALENDAR_7_7_SYSTEM_TYPES) return(integer(0))
  mid = suppressWarnings(as.integer(metro_id))[1]
  if (!is.finite(mid)) return(integer(0))
  as.integer(CHARGE_CALENDAR_WEEKDAY_EXCLUDED[[as.character(mid)]] %||% integer(0))
}

#' Vectorised METRO-GRAIN "this metro charges nothing on this day".
#'
#' A metro-day is uncharged iff every scheme of the metro in force that day is
#' uncharged, each scheme resolving as coalesce(calendar row, weekday fallback).
#' A non-finite metro id or an unparseable date is always FALSE (never dropped,
#' never flipped).
charge_calendar_metro_uncharged = function(metro_ids, dates, cal = charge_calendar_load()) {
  mids = suppressWarnings(as.integer(metro_ids))
  dts  = suppressWarnings(as.Date(dates))
  n    = max(length(mids), length(dts))
  mids = rep_len(mids, n); dts = dts[rep_len(seq_along(dts), n)]
  wday = charge_calendar_iso_wday(dts)
  ok   = is.finite(mids) & !is.na(dts)
  out  = logical(n)
  for (m in unique(mids[ok])) {
    sel = which(ok & mids == m)
    d = dts[sel]; w = wday[sel]
    schemes = CHARGE_CALENDAR_METRO_SCHEMES[[as.character(m)]] %||%
      list(list(scope = "*", from = as.Date("1900-01-01"), to = CHARGE_CALENDAR_OPEN_END,
                system_type = NA_character_))
    charged_any = logical(length(sel))
    for (s in schemes) {
      live = d >= s$from & d <= s$to
      if (!any(live)) next
      st = charge_calendar_status(m, s$scope, d, cal = cal)
      ex = charge_calendar_fallback_excluded(m, s$system_type)
      ch = ifelse(is.na(st), !(w %in% ex), st == "charged")
      charged_any = charged_any | (live & ch)
    }
    out[sel] = !charged_any
  }
  out
}

#' ISO weekday (1 = Mon .. 7 = Sun) of a Date vector, by arithmetic.
#'
#' Identical to `as.integer(format(dts, "%u"))` for every Date -- whole,
#' fractional (floor, as format() does), negative, NA and +/-Inf (both NA) --
#' and ~100x cheaper: format() costs ~0.66 s per 52k London cells, paid on
#' every pool call (perf, 2026-10-01). Day 0 of the Date epoch,
#' 1970-01-01, is a Thursday (ISO 4). Checked 1900-2100 and on the edge cases
#' in app/v2/api/tests/test_charge_calendar_perf.R.
charge_calendar_iso_wday = function(dts) {
  x = floor(unclass(dts))
  x[!is.finite(x)] = NA
  as.integer((x + 3) %% 7) + 1L
}

# -----------------------------------------------------------------------------
# MEMOISED METRO-GRAIN LOOKUP (perf, 2026-10-01; ADR-0043 addendum).
#
# Whether (metro, day) is charged is a pure function of: the calendar, the
# metro-grain scheme map, the weekday fallback, the 7/7 system types and the
# rate_zero switch. `charge_calendar_metro_uncharged()` is POINTWISE -- a day's
# answer never depends on the other days in the call -- so the lookup computes
# each metro ONCE, by calling that EXISTING resolver on every day of a
# whole-year span, and afterwards answers a call by indexing. Same function,
# same inputs, so the answers are byte-identical by construction; the parity
# test (tests/test_charge_calendar_perf.R) proves it on the full panel grid.
#
# INVALIDATION. The cached state is rebuilt whenever any input changes:
# `identical()` on the calendar object (so a `charge_calendar_load(refresh =
# TRUE)` that re-reads a changed CSV, or a test-built calendar, gets its own
# grid), on the three maps above, and the rate_zero switch value.
#
# ESCAPE HATCHES. Anything the dense grid cannot represent exactly -- a
# fractional or infinite date, or a span over 200 years -- goes through the
# direct resolver for that call. CPPORTAL_CHARGE_CALENDAR_FAST=0 turns the whole
# fast path off (the parity test and tests/bench_pool.R use it as "before").
# -----------------------------------------------------------------------------
CHARGE_CALENDAR_LOOKUP_MAX_SPAN = 73050L   # 200 years of days

#' The column `cpportal_stamp_charged_days()` (R/cross_city_att.R) writes on a
#' pin's ATT rows at bundle load and `cpportal_charged_days_filter()` trusts:
#' TRUE where the METRO-GRAIN calendar says the metro charged that day (the
#' exact complement of `charge_calendar_metro_uncharged()`). Defined HERE so
#' the filter works wherever this resolver is loaded.
#'
#' Deliberately NOT `charged_day`: BREATHE pins already carry a `charged_day`
#' from the train-side PER-PAIR flip, a finer rule than the metro grain
#' (London's central-ULEZ residual, 485 monitor-days). Trusting that column in
#' the serving filter would silently change BREATHE's served numbers.
CPPORTAL_CHARGED_STAMP_COL = ".charged_metro_grain"

#' Is the memoised fast path on? (CPPORTAL_CHARGE_CALENDAR_FAST, default on.)
charge_calendar_fast_enabled = function() {
  v = tolower(trimws(Sys.getenv("CPPORTAL_CHARGE_CALENDAR_FAST", "")))
  !(v %in% c("0", "false", "f", "no", "n", "off"))
}

# The lookup state for `cal`, reset when any input of the resolver changed.
.charge_calendar_lookup_state = function(cal) {
  sig = list(cal = cal, rz = .charge_calendar_rate_zero_uncharged(),
             schemes = CHARGE_CALENDAR_METRO_SCHEMES,
             excluded = CHARGE_CALENDAR_WEEKDAY_EXCLUDED,
             sys77 = CHARGE_CALENDAR_7_7_SYSTEM_TYPES)
  lk = .charge_calendar_cache[[".metro_lookup"]]
  if (is.null(lk) || !identical(lk$sig, sig)) {
    lk = new.env(parent = emptyenv())
    lk$sig = sig
    lk$metro = integer(0); lk$lo = integer(0); lk$hi = integer(0)
    lk$vec = list(); lk$off = integer(0); lk$flat = logical(0)
    assign(".metro_lookup", lk, envir = .charge_calendar_cache)
  }
  lk
}

# First / last day number of the calendar years holding days `a` / `b`.
.charge_calendar_year_lo = function(a) {
  as.integer(as.Date(sprintf("%04d-01-01", as.POSIXlt(.Date(a))$year + 1900L)))
}
.charge_calendar_year_hi = function(b) {
  as.integer(as.Date(sprintf("%04d-12-31", as.POSIXlt(.Date(b))$year + 1900L)))
}

#' Build (or widen) the cached grid so every metro in `metros` covers day
#' numbers `lo..hi`. Each metro's span is whole calendar years, computed by
#' `charge_calendar_metro_uncharged()` itself. Returns FALSE (caller falls back
#' to the direct resolver) when the span would exceed the 200-year cap.
.charge_calendar_lookup_ensure = function(lk, metros, lo, hi, cal) {
  lo = .charge_calendar_year_lo(lo); hi = .charge_calendar_year_hi(hi)
  changed = FALSE
  for (m in metros) {
    i = match(m, lk$metro)
    if (!is.na(i) && lk$lo[i] <= lo && lk$hi[i] >= hi) next
    new_lo = if (is.na(i)) lo else min(lo, lk$lo[i])
    new_hi = if (is.na(i)) hi else max(hi, lk$hi[i])
    if (new_hi - new_lo + 1L > CHARGE_CALENDAR_LOOKUP_MAX_SPAN) return(FALSE)
    days = .Date(as.double(seq.int(new_lo, new_hi)))
    v = charge_calendar_metro_uncharged(rep.int(m, length(days)), days, cal = cal)
    if (is.na(i)) {
      lk$metro = c(lk$metro, m); lk$lo = c(lk$lo, new_lo); lk$hi = c(lk$hi, new_hi)
      lk$vec[[length(lk$metro)]] = v
    } else {
      lk$lo[i] = new_lo; lk$hi[i] = new_hi; lk$vec[[i]] = v
    }
    changed = TRUE
  }
  if (changed) {
    lens = vapply(lk$vec, length, integer(1))
    lk$off = c(0L, cumsum(lens))[seq_along(lens)]
    lk$flat = unlist(lk$vec, use.names = FALSE)
  }
  TRUE
}

#' Drop-in, memoised `charge_calendar_metro_uncharged()`.
#'
#' Same arguments, same coercions, same NA rules (a non-finite metro id or an
#' unparseable date is FALSE), same answer -- one vectorised index per call
#' instead of re-resolving every cell. Falls back to the direct resolver when
#' the fast path is off or the input cannot be represented on the day grid.
charge_calendar_metro_uncharged_lookup = function(metro_ids, dates,
                                                  cal = charge_calendar_load()) {
  if (!charge_calendar_fast_enabled()) {
    return(charge_calendar_metro_uncharged(metro_ids, dates, cal = cal))
  }
  mids = suppressWarnings(as.integer(metro_ids))
  dts  = suppressWarnings(as.Date(dates))
  n    = max(length(mids), length(dts))
  mids = rep_len(mids, n); dts = dts[rep_len(seq_along(dts), n)]
  ok   = is.finite(mids) & !is.na(dts)
  out  = logical(n)
  if (!any(ok)) return(out)
  dn = unclass(dts)[ok]
  if (any(!is.finite(dn)) || any(dn != floor(dn))) {
    return(charge_calendar_metro_uncharged(metro_ids, dates, cal = cal))
  }
  d  = as.integer(dn)
  m  = mids[ok]
  lk = .charge_calendar_lookup_state(cal)
  rg = range(d)
  if (!.charge_calendar_lookup_ensure(lk, unique(m), rg[1], rg[2], cal)) {
    return(charge_calendar_metro_uncharged(metro_ids, dates, cal = cal))
  }
  i = match(m, lk$metro)
  out[ok] = lk$flat[lk$off[i] + (d - lk$lo[i] + 1L)]
  out
}

if (!exists("%||%", mode = "function")) `%||%` = function(x, y) if (is.null(x)) y else x

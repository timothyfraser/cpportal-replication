# =============================================================================
# Model pipeline functions (workflow.R and future plumber API)
# =============================================================================
# Requires: DBI, RPostgres, dplyr, tidyr (and fect for summarize_effects)
# =============================================================================

library(DBI, quietly = TRUE, warn.conflicts = FALSE)
library(RPostgres, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE, warn.conflicts = FALSE)

#' Default AQ outcome for DiD and fect (`sqrt` scale; native units via `inverse_fn` in meta).
#'
#' For identity on `aq_daily_mean`, pass `outcome = ~ aq_daily_mean` explicitly.
#'
#' @return One-sided formula.
#' @keywords internal
default_outcome_aq = function() {
  stats::as.formula("~ sqrt(aq_daily_mean)")
}

# -----------------------------------------------------------------------------
# normalize_background_method: normalize background_method parameter
# -----------------------------------------------------------------------------
#' Normalize background_method parameter to a standard format.
#' @param bg_method Input: NULL, "prefer", 1, "upwind", "hard", 2, or "min_daily"
#' @return List with mode ("prefer" or "hard") and method_id (1, 2, or NULL for prefer)
normalize_background_method <- function(bg_method) {
  if (is.null(bg_method)) {
    return(list(mode = "prefer", method_id = NULL))
  }
  
  # Convert to character for comparison
  bg_char <- tolower(trimws(as.character(bg_method)))
  
  if (bg_char %in% c("prefer", "")) {
    return(list(mode = "prefer", method_id = NULL))
  }
  
  if (bg_char %in% c("1", "upwind", "hard")) {
    return(list(mode = "hard", method_id = 1L))
  }
  
  if (bg_char %in% c("2", "min_daily")) {
    return(list(mode = "hard", method_id = 2L))
  }
  
  # Default to prefer mode if unrecognized
  warning("Unrecognized background_method: ", bg_method, ". Defaulting to prefer mode.")
  return(list(mode = "prefer", method_id = NULL))
}

# -----------------------------------------------------------------------------
# get_model_ready_monitors: return monitors with outcome + requested covariates
# -----------------------------------------------------------------------------
#' Get monitor IDs (and optionally per-date details) that have all required data
#' for modeling: outcome (pollutant) plus each requested covariate (TEMP, RHUM,
#' background, population). Uses actual data tables (not source_metro_*_checks).
#'
#' @param conn DBI connection
#' @param pollutant Pollutant code (e.g. "PM2.5")
#' @param date_from Start date (inclusive)
#' @param date_to End date (inclusive)
#' @param metro_ids Optional numeric vector of metro_id to restrict; NULL = all
#' @param required_variables Character vector of covariate names that must be
#'   present: "TEMP", "RHUM", "background", "population". Outcome is always required.
#'   Omit "population" to qualify monitors without yearly pop data.
#' @param return_details If TRUE, return a list with fullaqsids and details tibble
#' @return If return_details FALSE: character vector of fullaqsid. If TRUE: list
#'   with fullaqsids (character) and details (tibble: fullaqsid, metro_id, date).
get_model_ready_monitors <- function(conn, pollutant, date_from, date_to,
                                     metro_ids = NULL,
                                     required_variables = c("TEMP", "RHUM", "background", "population"),
                                     return_details = FALSE) {
  if (is.null(conn)) return(if (return_details) list(fullaqsids = character(0), details = tibble()) else character(0))
  # Normalize dates
  date_from <- as.Date(date_from)
  date_to   <- as.Date(date_to)
  if (date_from > date_to) {
    warning("date_from > date_to; returning empty.")
    return(if (return_details) list(fullaqsids = character(0), details = tibble()) else character(0))
  }
  # Normalize required_variables for downstream mapping
  req_vars <- unique(tolower(trimws(required_variables)))
  # Boolean flags for inline / legacy path
  require_weather   <- any(c("temp", "rhum") %in% req_vars)
  require_background <- "background" %in% req_vars
  require_pop       <- "population" %in% req_vars

  # Preferred path: wrapper function that accepts explicit variable list
  use_wrapper <- tryCatch({
    chk <- DBI::dbGetQuery(conn,
      "SELECT 1 FROM pg_proc WHERE proname = 'get_model_ready_monitor_dates_for_vars' LIMIT 1")
    nrow(chk) > 0L
  }, error = function(e) FALSE)

  res <- NULL
  if (use_wrapper) {
    # Map R-side variable names to DB-side symbolic names
    vars_for_db <- character(0)
    if (any(c("temp", "rhum") %in% req_vars)) {
      vars_for_db <- c(vars_for_db, "TEMP", "RHUM")
    }
    if ("background" %in% req_vars) {
      vars_for_db <- c(vars_for_db, "BACKGROUND")
    }
    if ("population" %in% req_vars) {
      vars_for_db <- c(vars_for_db, "POPULATION")
    }
    vars_for_db <- unique(vars_for_db)

    res <- tryCatch({
      if (is.null(metro_ids)) {
        DBI::dbGetQuery(conn,
          "SELECT fullaqsid, metro_id, date
           FROM get_model_ready_monitor_dates_for_vars(
             $1::text, $2::date, $3::date, NULL::bigint[], $4::text[]
           )",
          list(
            pollutant,
            format(date_from, "%Y-%m-%d"),
            format(date_to, "%Y-%m-%d"),
            if (length(vars_for_db) == 0L) array(NA_character_, 0L) else vars_for_db
          )
        )
      } else {
        arr <- paste0("{", paste(as.character(metro_ids), collapse = ","), "}")
        DBI::dbGetQuery(conn,
          "SELECT fullaqsid, metro_id, date
           FROM get_model_ready_monitor_dates_for_vars(
             $1::text, $2::date, $3::date, $4::bigint[], $5::text[]
           )",
          list(
            pollutant,
            format(date_from, "%Y-%m-%d"),
            format(date_to, "%Y-%m-%d"),
            arr,
            if (length(vars_for_db) == 0L) array(NA_character_, 0L) else vars_for_db
          )
        )
      }
    }, error = function(e) {
      use_wrapper <<- FALSE
      NULL
    })
  }

  # Fallback: use original boolean-based function or inline SQL
  use_fn <- FALSE
  if (!use_wrapper || is.null(res)) {
    use_fn <- tryCatch({
      chk <- DBI::dbGetQuery(conn,
        "SELECT 1 FROM pg_proc WHERE proname = 'get_model_ready_monitor_dates' LIMIT 1")
      nrow(chk) > 0L
    }, error = function(e) FALSE)
  }
  if (use_fn && (is.null(res) || !nrow(res))) {
    res <- tryCatch({
      if (is.null(metro_ids)) {
        DBI::dbGetQuery(conn,
          "SELECT fullaqsid, metro_id, date FROM get_model_ready_monitor_dates($1::text, $2::date, $3::date, NULL::bigint[], $4::boolean, $5::boolean, $6::boolean)",
          list(pollutant, format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"), require_weather, require_background, require_pop))
      } else {
        arr <- paste0("{", paste(as.character(metro_ids), collapse = ","), "}")
        DBI::dbGetQuery(conn,
          "SELECT fullaqsid, metro_id, date FROM get_model_ready_monitor_dates($1::text, $2::date, $3::date, $4::bigint[], $5::boolean, $6::boolean, $7::boolean)",
          list(pollutant, format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"), arr, require_weather, require_background, require_pop))
      }
    }, error = function(e) {
      use_fn <<- FALSE
      NULL
    })
  }
  if ((!use_wrapper && !use_fn) || is.null(res)) {
    # Inline SQL (same logic as get_model_ready_monitor_dates with optional covariates)
    q <- "
    WITH date_range AS (SELECT $1::date AS d_from, $2::date AS d_to),
    required_year_count AS (
      SELECT (EXTRACT(YEAR FROM $2::date)::int - EXTRACT(YEAR FROM $1::date)::int + 1)::int AS n
    ),
    monitors_with_pop AS (
      SELECT p.fullaqsid
      FROM pop p
      WHERE p.year BETWEEN EXTRACT(YEAR FROM $1::date)::int AND EXTRACT(YEAR FROM $2::date)::int
        AND p.population IS NOT NULL
      GROUP BY p.fullaqsid
      HAVING COUNT(DISTINCT p.year) = (SELECT n FROM required_year_count)
    ),
    aq_dates AS (
      SELECT DISTINCT aq.fullaqsid, aq.date
      FROM airquality aq
      JOIN monitors m ON aq.fullaqsid = m.fullaqsid AND aq.parameter = m.parameter
      WHERE aq.parameter = $3 AND aq.metric = '1HR' AND aq.value IS NOT NULL
        AND aq.date BETWEEN $1::date AND $2::date
    ),
    weather_dates AS (
      SELECT fullaqsid, date FROM weather
      WHERE date BETWEEN $1::date AND $2::date
      GROUP BY fullaqsid, date
      HAVING COUNT(*) FILTER (WHERE TEMP IS NOT NULL) >= 1 AND COUNT(*) FILTER (WHERE RHUM IS NOT NULL) >= 1
    ),
    bg_dates AS (
      SELECT fullaqsid, date FROM public.bg
      WHERE parameter = $3 AND method_id = 3 AND bg IS NOT NULL
        AND date BETWEEN $1::date AND $2::date
    )
    SELECT aq.fullaqsid, m.metro_id, aq.date
    FROM aq_dates aq
    JOIN monitors m ON aq.fullaqsid = m.fullaqsid AND m.parameter = $3
    LEFT JOIN weather_dates w ON aq.fullaqsid = w.fullaqsid AND aq.date = w.date
    LEFT JOIN bg_dates b ON aq.fullaqsid = b.fullaqsid AND aq.date = b.date
    LEFT JOIN monitors_with_pop mp ON aq.fullaqsid = mp.fullaqsid
    WHERE m.metro_id IS NOT NULL
      AND (NOT %s OR w.fullaqsid IS NOT NULL)
      AND (NOT %s OR b.fullaqsid IS NOT NULL)
      AND (NOT %s OR mp.fullaqsid IS NOT NULL)
    "
    if (!is.null(metro_ids) && length(metro_ids) > 0L) {
      q <- sub("WHERE m.metro_id IS NOT NULL",
               "WHERE m.metro_id IS NOT NULL AND m.metro_id = ANY($4::bigint[])", q, fixed = TRUE)
      q <- sprintf(q, "$5::boolean", "$6::boolean", "$7::boolean")
      params <- list(format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"), pollutant,
                     paste0("{", paste(as.character(metro_ids), collapse = ","), "}"),
                     require_weather, require_background, require_pop)
    } else {
      q <- sprintf(q, "$4::boolean", "$5::boolean", "$6::boolean")
      params <- list(format(date_from, "%Y-%m-%d"), format(date_to, "%Y-%m-%d"), pollutant,
                     require_weather, require_background, require_pop)
    }
    res <- DBI::dbGetQuery(conn, q, params = params)
  }
  res <- as_tibble(res)
  if (nrow(res) == 0L) {
    return(if (return_details) list(fullaqsids = character(0), details = res) else character(0))
  }
  fullaqsids <- unique(res$fullaqsid)
  if (return_details) {
    list(fullaqsids = fullaqsids, details = res)
  } else {
    fullaqsids
  }
}

# -----------------------------------------------------------------------------
# connect_db: open DB connection using env vars (same as job scripts)
# -----------------------------------------------------------------------------
#' Connect to PostgreSQL using PGHOST, PGPORT, PGUSER, PGPASSWORD, PGDATABASE.
#' Loads .env from job/ if present (when run from repo root).
#' @return DBI connection or NULL on failure
connect_db <- function() {
  # REPLICATION MODE (CPPORTAL_REPLICATION_RUN=1): no .env of any kind is read
  # and no connection is opened. A replication run gets its panel, its treated
  # pairs and its policy rows from the Dataverse snapshot, so a database handle
  # is neither needed nor permitted. Unset/any other value => production, i.e.
  # exactly the behavior below. See replication/run_replication.R.
  if (identical(Sys.getenv("CPPORTAL_REPLICATION_RUN"), "1")) {
    stop("connect_db(): replication runs read the Dataverse snapshot; ",
         "no database connection is used or permitted ",
         "(CPPORTAL_REPLICATION_RUN=1).", call. = FALSE)
  }
  env_path <- "job/.env"
  if (basename(getwd()) == "model") env_path <- "../.env"
  if (file.exists(env_path)) readRenviron(env_path)
  pg_host <- Sys.getenv("PGHOST")
  if (!nzchar(pg_host)) return(NULL)
  tryCatch(
    dbConnect(
      RPostgres::Postgres(),
      host = pg_host,
      port = as.integer(Sys.getenv("PGPORT", "5432")),
      user = Sys.getenv("PGUSER"),
      password = Sys.getenv("PGPASSWORD"),
      dbname = Sys.getenv("PGDATABASE", "postgres")
    ),
    error = function(e) { message("DB connect failed: ", e$message); NULL }
  )
}

# -----------------------------------------------------------------------------
# fetch_panel_data: query airquality (with period), weather, background
# -----------------------------------------------------------------------------
#' Fetch panel data for the given monitors, pollutant, date range, and variables.
#' Optionally samples control monitors from other metro areas that have data in the date range.
#' Aggregates data to the requested temporal level before returning.
#' Returns aggregated data: one row per (fullaqsid, time_id) based on temporal_level.
#' @param conn DBI connection (or NULL to return empty tibble)
#' @param date_from Start date (Date or "YYYY-MM-DD")
#' @param date_to End date (Date or "YYYY-MM-DD")
#' @param monitor_ids Character vector of fullaqsid (treated monitors)
#' @param pollutant Pollutant code (e.g. "PM2.5")
#' @param variables Character vector: e.g. c("outcome", "background", "TEMP", "RHUM", "population")
#' @param temporal_level "hourly" | "daily" | "weekly" | "monthly" - aggregation level
#' @param aggregation Named list: aggregation method per variable (mean, max, min, p97, sum)
#' @param n_control_monitors Integer, number of control monitors to sample from available pool (NULL = no sampling)
#' @param control_metro_filter Named list with optional filters: iso_id (character), min_monitors_per_metro (integer), max_monitors_per_metro (integer, cap control monitors per metro with random sample)
#' @param background_method NULL or "prefer" = prefer method 1 (upwind), fallback to method 2; 1/"upwind"/"hard" = only method 1; 2/"min_daily" = only method 2
#' @details If \code{population} is in \code{variables}, it is joined from table \code{pop} by (fullaqsid, year).
fetch_panel_data <- function(conn, date_from, date_to, monitor_ids, pollutant, variables,
                              temporal_level = "hourly", aggregation = NULL,
                              n_control_monitors = NULL, control_metro_filter = NULL,
                              background_method = NULL,
                              controls_mode = "none") {
  if (is.null(conn) || length(monitor_ids) == 0L) {
    return(tibble())
  }
  date_from <- as.Date(date_from)
  date_to   <- as.Date(date_to)
  # Normalize controls_mode: allow logical shortcuts and character options
  if (is.logical(controls_mode)) {
    controls_mode <- if (isTRUE(controls_mode)) "treated_like" else "none"
  }
  controls_mode <- match.arg(as.character(controls_mode), c("none", "treated_like", "skip"))
  
  # If control sampling is requested, identify treated metro_ids and sample controls
  all_monitor_ids <- monitor_ids
  if (!is.null(n_control_monitors) && n_control_monitors > 0L && controls_mode != "skip") {
    message("Model job: sampling ", n_control_monitors, " control monitors from other metro areas...")
    
    # Get metro_ids for treated monitors
    in_fullaqsid_treated <- paste(DBI::dbQuoteString(conn, monitor_ids), collapse = ", ")
    q_metros <- sprintf(
      "SELECT DISTINCT metro_id FROM monitors WHERE fullaqsid IN (%s) AND parameter = %s AND metro_id IS NOT NULL",
      in_fullaqsid_treated,
      DBI::dbQuoteString(conn, pollutant)
    )
    treated_metros <- dbGetQuery(conn, q_metros)
    treated_metro_ids <- treated_metros$metro_id
    treated_metro_ids <- treated_metro_ids[!is.na(treated_metro_ids)]
    
    if (length(treated_metro_ids) == 0L) {
      message("Warning: Could not identify metro_ids for treated monitors. Skipping control sampling.")
    } else {
      message("Model job: found ", length(treated_metro_ids), " treated metro area(s)")
      
      # Build country filter
      iso_param <- NULL
      if (!is.null(control_metro_filter) && !is.null(control_metro_filter$iso_id) && 
          nzchar(control_metro_filter$iso_id)) {
        iso_param <- control_metro_filter$iso_id
      }
      
      # Build metro exclusion clause (as string, similar to codebase pattern)
      metro_exclude_clause <- ""
      if (length(treated_metro_ids) > 0L) {
        # Convert to character and build NOT IN clause
        metro_ids_str <- paste(as.character(treated_metro_ids), collapse = ", ")
        metro_exclude_clause <- paste("AND m.metro_id NOT IN (", metro_ids_str, ")")
      }
      
      # Build country filter clause
      iso_filter_clause <- ""
      if (!is.null(iso_param)) {
        iso_filter_clause <- paste("AND mp.iso_id =", DBI::dbQuoteString(conn, iso_param))
      }
      
      # OPTIMIZED: Find all available control monitors in date range (one query instead of per-date)
      # Optionally cap per metro (max_monitors_per_metro), then global cap (n_control_monitors)
      max_per_metro <- NULL
      if (!is.null(control_metro_filter) && !is.null(control_metro_filter$max_monitors_per_metro) &&
          control_metro_filter$max_monitors_per_metro > 0L) {
        max_per_metro <- as.integer(control_metro_filter$max_monitors_per_metro)
      }
      message("Model job: finding available control monitors in date range...")
      if (!is.null(max_per_metro)) {
        q_available <- paste0(
          "WITH candidates AS (
             SELECT m.fullaqsid, m.metro_id
             FROM airquality aq
             JOIN monitors m ON aq.fullaqsid = m.fullaqsid AND aq.parameter = m.parameter
             JOIN metro_polygons mp ON m.metro_id = mp.metro_id
             WHERE aq.parameter = $1
               AND aq.date >= $2::date
               AND aq.date <= $3::date
               AND aq.metric = '1HR'
               AND aq.value IS NOT NULL
               ", metro_exclude_clause, "
               ", iso_filter_clause, "
             GROUP BY m.fullaqsid, m.metro_id
           ),
           ranked AS (
             SELECT fullaqsid, metro_id,
                    ROW_NUMBER() OVER (PARTITION BY metro_id ORDER BY RANDOM()) AS rn
             FROM candidates
           )
           SELECT fullaqsid FROM ranked
           WHERE rn <= $4
           ORDER BY RANDOM()
           LIMIT $5"
        )
        params_available <- list(
          pollutant,
          format(date_from, "%Y-%m-%d"),
          format(date_to, "%Y-%m-%d"),
          max_per_metro,
          n_control_monitors
        )
      } else {
        q_available <- paste0(
          "SELECT m.fullaqsid, COUNT(DISTINCT aq.date) AS date_count
           FROM airquality aq
           JOIN monitors m ON aq.fullaqsid = m.fullaqsid AND aq.parameter = m.parameter
           JOIN metro_polygons mp ON m.metro_id = mp.metro_id
           WHERE aq.parameter = $1
             AND aq.date >= $2::date
             AND aq.date <= $3::date
             AND aq.metric = '1HR'
             AND aq.value IS NOT NULL
             ", metro_exclude_clause, "
             ", iso_filter_clause, "
           GROUP BY m.fullaqsid
           ORDER BY RANDOM()
           LIMIT $4"
        )
        params_available <- list(
          pollutant,
          format(date_from, "%Y-%m-%d"),
          format(date_to, "%Y-%m-%d"),
          n_control_monitors
        )
      }
      
      available_controls <- dbGetQuery(conn, q_available, params = params_available)
      all_control_ids <- available_controls$fullaqsid
      
      message("Model job: sampled ", length(all_control_ids), " control monitors from available pool")
      
      # Combine treated and control monitors
      all_monitor_ids <- unique(c(monitor_ids, all_control_ids))
      message("Model job: total monitors (treated + control): ", length(all_monitor_ids))
    }
  }
  
  # Fetch data for all monitors (treated + control)
  in_fullaqsid <- paste(DBI::dbQuoteString(conn, all_monitor_ids), collapse = ", ")

  # Air quality (hourly 1HR) + period - query airquality directly and join to periods
  q_aq <- sprintf(
    "SELECT aq.fullaqsid, aq.date, aq.hour, aq.parameter, aq.value,
            p.treated AS period_treated, p.start_date AS period_start_date, p.end_date AS period_end_date
     FROM airquality aq
     JOIN monitors m ON aq.fullaqsid = m.fullaqsid AND aq.parameter = m.parameter
     LEFT JOIN congestion_pricing_periods p
       ON m.metro_id = p.metro_id
       AND aq.date >= p.start_date
       AND (p.end_date IS NULL OR aq.date <= p.end_date)
     WHERE aq.fullaqsid IN (%s) 
       AND aq.parameter = %s 
       AND aq.date >= %s 
       AND aq.date <= %s 
       AND aq.metric = '1HR'",
    in_fullaqsid,
    DBI::dbQuoteString(conn, pollutant),
    DBI::dbQuoteString(conn, format(date_from, "%Y-%m-%d")),
    DBI::dbQuoteString(conn, format(date_to, "%Y-%m-%d"))
  )
  aq <- dbGetQuery(conn, q_aq)
  if (nrow(aq) == 0L) return(tibble())
  aq <- as_tibble(aq)
  aq$date <- as.Date(aq$date)
  # Rename value to outcome for consistency
  aq <- rename(aq, outcome = "value")
  
  # Handle period_treated NA values: default to FALSE for control monitors without periods
  aq$period_treated[is.na(aq$period_treated)] <- FALSE

  # Weather: only requested columns
  weather_cols <- c("TEMP", "RHUM", "PRECIP", "WS", "WD", "BARPR", "SRAD", "CLOUD")
  req_weather <- intersect(variables, weather_cols)
  if (length(req_weather) > 0L) {
    # Initialize all requested weather columns with NA values first
    for (col in req_weather) {
      aq[[col]] <- NA_real_
    }
    
    # Then try to fetch and join actual weather data
    wcols <- paste(c("fullaqsid", "date", "hour", req_weather), collapse = ", ")
    q_w <- sprintf(
      "SELECT %s FROM weather WHERE fullaqsid IN (%s) AND date >= %s AND date <= %s",
      wcols, in_fullaqsid,
      DBI::dbQuoteString(conn, format(date_from, "%Y-%m-%d")),
      DBI::dbQuoteString(conn, format(date_to, "%Y-%m-%d"))
    )
    w <- as_tibble(dbGetQuery(conn, q_w))
    if (nrow(w) > 0L) {
      w$date <- as.Date(w$date)
      # Join weather data - this will add columns with _new suffix if they conflict
      # We'll merge them back into the original columns
      aq <- left_join(aq, w, by = c("fullaqsid", "date", "hour"), suffix = c("", "_new"))
      # Merge _new columns back into original columns
      for (col in req_weather) {
        new_col <- paste0(col, "_new")
        if (new_col %in% names(aq)) {
          # Use new values where they exist (non-NA), keep existing NA otherwise
          aq[[col]] <- ifelse(!is.na(aq[[new_col]]), aq[[new_col]], aq[[col]])
          aq[[new_col]] <- NULL
        }
      }
    }
    # If no weather data found, columns already have NA values from initialization above
  }

  # Background: from table by fullaqsid, parameter, date (daily)
  # Handle background separately since it's already daily - join after aggregation for non-hourly
  if ("background" %in% variables) {
    # Normalize background_method parameter
    bg_method_norm <- normalize_background_method(background_method)
    
    # Build query based on mode
    if (bg_method_norm$mode == "prefer") {
      # Prefer mode: use window function to rank methods (method_id 1 preferred, fallback to 2)
      q_bg <- sprintf(
        "SELECT fullaqsid, parameter, date, bg_mean AS background
         FROM (
           SELECT 
             fullaqsid, parameter, date, bg_mean, method_id,
             ROW_NUMBER() OVER (
               PARTITION BY fullaqsid, parameter, date 
               ORDER BY method_id
             ) AS method_rank
           FROM background_concentration
           WHERE fullaqsid IN (%s) AND parameter = %s AND date >= %s AND date <= %s
         ) ranked
         WHERE method_rank = 1",
        in_fullaqsid,
        DBI::dbQuoteString(conn, pollutant),
        DBI::dbQuoteString(conn, format(date_from, "%Y-%m-%d")),
        DBI::dbQuoteString(conn, format(date_to, "%Y-%m-%d"))
      )
    } else {
      # Hard filter mode: only use specified method_id (no fallback)
      q_bg <- sprintf(
        "SELECT fullaqsid, parameter, date, bg_mean AS background
         FROM background_concentration
         WHERE fullaqsid IN (%s) AND parameter = %s AND date >= %s AND date <= %s AND method_id = %d",
        in_fullaqsid,
        DBI::dbQuoteString(conn, pollutant),
        DBI::dbQuoteString(conn, format(date_from, "%Y-%m-%d")),
        DBI::dbQuoteString(conn, format(date_to, "%Y-%m-%d")),
        bg_method_norm$method_id
      )
    }
    bg <- as_tibble(dbGetQuery(conn, q_bg))
    bg$date <- as.Date(bg$date)
    bg <- select(bg, "fullaqsid", "date", "background")
    
    if (temporal_level == "hourly") {
      # For hourly, join background by fullaqsid and date (each hour gets same daily value)
      aq <- left_join(aq, bg, by = c("fullaqsid", "date"))
    }
    # For daily/weekly/monthly, we'll join background after aggregation
  }

  # Aggregate to temporal level if not hourly
  if (temporal_level != "hourly") {
    if (is.null(aggregation)) {
      # Default aggregation: mean for all variables
      aggregation <- setNames(rep("mean", length(variables)), variables)
    }
    # Aggregate hourly data (excluding background if it's already daily)
    vars_to_agg <- if ("background" %in% variables) setdiff(variables, "background") else variables
    aq <- aggregate_panel(
      data = aq,
      temporal_level = temporal_level,
      aggregation = aggregation,
      variables = vars_to_agg
    )
    
    # Now join background for daily/weekly/monthly (after aggregation)
    if ("background" %in% variables) {
      if (temporal_level == "daily") {
        # For daily, time_id IS the date, so join by matching time_id to bg.date
        bg$time_id <- as.Date(bg$date)
        aq <- left_join(aq, select(bg, "fullaqsid", "time_id", "background"), by = c("fullaqsid", "time_id"))
      } else {
        # For weekly/monthly, aggregate background from daily to weekly/monthly
        bg$time_id <- as.Date(bg$date)
        if (temporal_level == "weekly") {
          bg$time_id <- as.Date(cut(bg$date, "week"))
        } else if (temporal_level == "monthly") {
          bg$time_id <- as.Date(cut(bg$date, "month"))
        }
        # Aggregate background using the specified method (or mean by default)
        bg_agg_method <- aggregation[["background"]] %||% "mean"
        bg_agg <- bg %>%
          group_by(.data[["fullaqsid"]], .data[["time_id"]]) %>%
          summarise(
            background = {
              bg_vals <- .data[["background"]]
              if (all(is.na(bg_vals))) {
                NA_real_
              } else {
                bg_clean <- bg_vals[!is.na(bg_vals)]
                if (length(bg_clean) == 0L) {
                  NA_real_
                } else {
                  switch(bg_agg_method,
                    mean = mean(bg_clean),
                    max = max(bg_clean),
                    min = min(bg_clean),
                    sum = sum(bg_clean),
                    p97 = quantile(bg_clean, probs = 0.97, names = FALSE),
                    mean(bg_clean)
                  )
                }
              }
            },
            .groups = "drop"
          )
        aq <- left_join(aq, bg_agg, by = c("fullaqsid", "time_id"))
      }
    }
  }

  # Population: from pop table by (fullaqsid, year); one value per monitor-year
  if ("population" %in% variables) {
    aq_dates <- if ("time_id" %in% names(aq)) aq$time_id else aq$date
    aq_years <- as.integer(format(as.Date(aq_dates), "%Y"))
    years_needed <- sort(unique(aq_years))
    if (length(years_needed) > 0L) {
      q_pop <- sprintf(
        "SELECT fullaqsid, year, population FROM pop WHERE fullaqsid IN (%s) AND year >= %d AND year <= %d",
        in_fullaqsid,
        min(years_needed),
        max(years_needed)
      )
      pop_df <- as_tibble(DBI::dbGetQuery(conn, q_pop))
      if (nrow(pop_df) > 0L) {
        # One row per (fullaqsid, year) in case pop has multiple metros per site
        pop_df <- pop_df %>%
          group_by(.data[["fullaqsid"]], .data[["year"]]) %>%
          summarise(population = first(.data[["population"]]), .groups = "drop")
        pop_df$year <- as.integer(pop_df$year)
        aq$._year <- as.integer(format(as.Date(aq_dates), "%Y"))
        aq <- left_join(aq, pop_df, by = c("fullaqsid", "._year" = "year"))
        aq$._year <- NULL
        if (!"population" %in% names(aq)) aq$population <- NA_real_
      } else {
        aq$population <- NA_real_
      }
    } else {
      aq$population <- NA_real_
    }
  }

  aq
}

# -----------------------------------------------------------------------------
# aggregate_panel: roll up to one row per (unit, time_id) using aggregation list
# -----------------------------------------------------------------------------
#' Aggregate long-form data to the requested temporal level.
#' @param data Long-form tibble from fetch_panel_data (columns: fullaqsid, date, hour, outcome, ...)
#' @param temporal_level "hourly" | "daily" | "weekly" | "monthly"
#' @param aggregation Named list: aggregation method per variable (mean, max, min, p97, sum)
#' @param variables Character vector of variable names (outcome plus covariates)
aggregate_panel <- function(data, temporal_level, aggregation, variables) {
  if (nrow(data) == 0L) return(data)
  data <- as_tibble(data)
  # Time unit
  if (temporal_level == "hourly") {
    data$time_id <- paste0(format(data$date, "%Y-%m-%d"), "_", sprintf("%02d", data$hour))
    group_by_vars <- c("fullaqsid", "date", "hour", "time_id")
  } else {
    data$time_id <- as.Date(data$date)
    if (temporal_level == "weekly") {
      data$time_id <- as.Date(cut(data$date, "week"))
    } else if (temporal_level == "monthly") {
      data$time_id <- as.Date(cut(data$date, "month"))
    }
    # For daily/weekly/monthly, use only time_id (not date) to avoid duplication
    group_by_vars <- c("fullaqsid", "time_id")
  }

  agg_one <- function(x, fun) {
    # If all values are NA or vector is empty, return NA (not -Inf/Inf)
    if (length(x) == 0L || all(is.na(x))) return(NA_real_)
    
    # For max/min, check if any non-NA values exist before calling
    if (fun %in% c("max", "min") && sum(!is.na(x)) == 0L) {
      return(NA_real_)
    }
    
    result <- switch(fun,
      mean = {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0L) NA_real_ else mean(x_clean)
      },
      max  = {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0L) NA_real_ else max(x_clean)
      },
      min  = {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0L) NA_real_ else min(x_clean)
      },
      sum  = {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0L) NA_real_ else sum(x_clean)
      },
      p97  = {
        x_clean <- x[!is.na(x)]
        if (length(x_clean) == 0L) NA_real_ else quantile(x_clean, probs = 0.97, names = FALSE)
      },
      mean(x, na.rm = TRUE)
    )
    
    # Handle -Inf/Inf from max/min (safety check)
    if (is.infinite(result)) return(NA_real_)
    result
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x
  out <- data %>%
    group_by(across(all_of(group_by_vars))) %>%
    summarise(
      across(
        any_of(variables),
        function(x) agg_one(x, aggregation[[cur_column()]] %||% "mean"),
        .names = "{.col}"
      ),
      .groups = "drop"
    )
  # Keep period_treated if present (take first per group)
  if ("period_treated" %in% names(data)) {
    period <- data %>%
      group_by(across(all_of(group_by_vars))) %>%
      summarise(period_treated = first(.data[["period_treated"]]), .groups = "drop")
    out <- left_join(out, period, by = group_by_vars)
  }
  
  # Remove date and hour columns for non-hourly aggregations (only keep time_id)
  if (temporal_level != "hourly") {
    out <- select(out, -any_of(c("date", "hour")))
  }
  
  out
}

# -----------------------------------------------------------------------------
# add_treatment_indicator: add binary D = 1 for (treated monitor, time >= treated_from)
# -----------------------------------------------------------------------------
#' Add treatment indicator column D.
#' @param data Panel tibble with fullaqsid and time_id (Date) or (date, hour)
#' @param treated_from Start of treatment (Date or "YYYY-MM-DD")
#' @param treated_to End of treatment (NULL = through end of data)
#' @param treated_monitor_ids Character vector of fullaqsid that are treated; if NULL, use period_treated from data
add_treatment_indicator <- function(data, treated_from, treated_to = NULL, treated_monitor_ids = NULL) {
  if (nrow(data) == 0L) return(data)
  treated_from <- as.Date(treated_from)
  treated_to   <- if (is.null(treated_to)) as.Date("9999-12-31") else as.Date(treated_to)
  data <- as_tibble(data)
  # Resolve time column for comparison
  time_col <- if ("date" %in% names(data)) "date" else "time_id"
  if (!time_col %in% names(data)) return(mutate(data, D = 0L))
  data$._time <- as.Date(data[[time_col]])
  if (!is.null(treated_monitor_ids)) {
    data$D <- as.integer(data$fullaqsid %in% treated_monitor_ids & data$._time >= treated_from & data$._time <= treated_to)
  } else if ("period_treated" %in% names(data)) {
    period_treated <- data$period_treated
    data$D <- as.integer(period_treated == TRUE & data$._time >= treated_from & data$._time <= treated_to)
  } else {
    data$D <- 0L
  }
  data$._time <- NULL
  data
}

# -----------------------------------------------------------------------------
# summarize_effects: extract ATT and optional event-study from fect fit
# -----------------------------------------------------------------------------
#' Extract ATT and, if present, balance/event-study estimates from a fect fit.
#' @param fit Return value from fect::fect() (fect object)
#' @return List with att (scalar or table), and optionally att_balance, att_series
summarize_effects <- function(fit) {
  if (!inherits(fit, "fect")) stop("fit must be a fect object")
  out <- list()
  if (!is.null(fit$est.avg)) out$att <- fit$est.avg
  if (!is.null(fit$est.balance.avg)) out$att_balance <- fit$est.balance.avg
  if (!is.null(fit$est.balance.att)) out$att_balance_series <- as.data.frame(fit$est.balance.att)
  out
}


# We're going to make a treatment variable
add_treatment = function(data, policies){
    policy_windows = policies %>%
        filter(treated %in% TRUE) %>%
        transmute(
            metro_id,
            start_date,
            end_date = coalesce(end_date, as.Date("9999-12-31"))
        ) %>%
        distinct()

    treatment_start_dates = policies %>%
        filter(treated %in% TRUE) %>%
        group_by(metro_id) %>%
        summarize(treatment_start_date = min(start_date), .groups = "drop")

    data = data %>%
        select(-any_of(c("treated", "start_date", "end_date", "treated_policy", "treatment_start_date", "days_since_treatment"))) %>%
        left_join(
            policy_windows %>% mutate(treated_policy = TRUE),
            by = join_by(metro_id, between(date, start_date, end_date))
        ) %>%
        left_join(treatment_start_dates, by = "metro_id") %>%
        mutate(treated = coalesce(treated_policy, FALSE)) %>%
        mutate(days_since_treatment = as.integer(date - treatment_start_date)) %>%
        select(-treated_policy, -treatment_start_date)

    return(data)
}

# -----------------------------------------------------------------------------
# fetch_zone_treated_pairs: monitors spatially inside treated zone polygons
# -----------------------------------------------------------------------------

#' Find (monitor, period) pairs where the monitor falls inside a zone polygon
#'
#' Spatially intersects `public.monitors.geom` with `public.zones.geometry` for
#' periods in `public.congestion_pricing_periods` whose `treated = TRUE` and
#' `system_type` is in the requested set (default `c("cordon")`). One row per
#' (metro_id, fullaqsid, policy_id, start_date, end_date); a monitor inside
#' multiple zones for the same metro produces one row per active period.
#'
#' This is the **zone-level** counterpart to the metro-wide `add_treatment()`:
#' only monitors that physically sit inside a zone are marked treated; other
#' monitors in the same metro stay as within-metro controls.
#'
#' @param db DBI connection (see [connect_db()]).
#' @param metro_ids Integer vector of metro_ids to consider.
#' @param system_types Character vector of `congestion_pricing_periods.system_type`
#'   values to count as treated (e.g. `c("cordon", "ULEZ")`).
#' @return Tibble with columns `metro_id`, `fullaqsid`, `policy_id`,
#'   `start_date`, `end_date`. Empty tibble (with the right schema) when no
#'   matches.
#' @export
fetch_zone_treated_pairs = function(db, metro_ids, system_types = c("cordon")) {
  empty = tibble::tibble(
    metro_id    = integer(),
    fullaqsid   = character(),
    policy_id   = integer(),
    start_date  = as.Date(character()),
    end_date    = as.Date(character())
  )

  if (is.null(db)) return(empty)
  if (length(metro_ids) == 0L) return(empty)
  if (length(system_types) == 0L) return(empty)

  metro_arr  = paste0("{", paste(as.integer(metro_ids), collapse = ","), "}")
  system_arr = paste0("{", paste(as.character(system_types), collapse = ","), "}")

  q = "
    SELECT m.metro_id, m.fullaqsid, cpp.id AS policy_id,
           cpp.start_date, cpp.end_date
    FROM public.monitors m
    JOIN public.zones z
      ON z.metro_id = m.metro_id
    JOIN public.congestion_pricing_periods cpp
      ON cpp.id = z.policy_id
    WHERE cpp.treated = TRUE
      AND cpp.system_type = ANY($1::text[])
      AND m.metro_id      = ANY($2::bigint[])
      AND ST_Intersects(m.geom, z.geometry)
  "
  res = tryCatch(
    DBI::dbGetQuery(db, q, params = list(system_arr, metro_arr)),
    error = function(e) {
      message("[fetch_zone_treated_pairs] SQL failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res) || nrow(res) == 0L) return(empty)

  res |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      metro_id   = as.integer(.data$metro_id),
      fullaqsid  = as.character(.data$fullaqsid),
      policy_id  = as.integer(.data$policy_id),
      start_date = as.Date(.data$start_date),
      end_date   = as.Date(.data$end_date)
    ) |>
    dplyr::distinct()
}

# -----------------------------------------------------------------------------
# fetch_metro_treated_pairs: monitors anywhere in a metro with an active period
# -----------------------------------------------------------------------------

#' Find (monitor, period) pairs for **metro-wide** treatment
#'
#' The metro-level counterpart to [fetch_zone_treated_pairs()]. Marks **every
#' monitor in metro M** as treated during M's active congestion-pricing period,
#' regardless of whether the monitor sits inside a zone polygon — i.e. it drops
#' the `ST_Intersects(monitors.geom, zones.geometry)` gate and joins
#' `public.monitors` directly to `public.congestion_pricing_periods` by
#' `metro_id`. Returns the **same schema** as [fetch_zone_treated_pairs()]
#' (`metro_id`, `fullaqsid`, `policy_id`, `start_date`, `end_date`) so the rest
#' of the training pipeline — [add_treatment_zones()], [add_groups()],
#' `get_fect()` — is byte-for-byte unchanged; only *which* monitors are marked
#' treated differs.
#'
#' Used to build the **metro models** alongside the existing **zone models**
#' (see `job/model/HANDOFF_METRO_MODELS.md`). Because metro-wide treatment is
#' diluted (most metro monitors sit far from the zone), the resulting
#' per-monitor-day effect `e_itm` is expected to be weaker than the zone
#' `e_itz` — that is by design, and is exactly why `e_it_adj` prefers `e_itz`
#' inside the zone.
#'
#' @param db DBI connection (see [connect_db()]).
#' @param metro_ids Integer vector of metro_ids to consider.
#' @param system_types Character vector of `congestion_pricing_periods.system_type`
#'   values to count as treated (e.g. `c("cordon", "ULEZ")`).
#' @return Tibble with columns `metro_id`, `fullaqsid`, `policy_id`,
#'   `start_date`, `end_date`. Empty tibble (with the right schema) when no
#'   matches.
#' @export
fetch_metro_treated_pairs = function(db, metro_ids, system_types = c("cordon")) {
  empty = tibble::tibble(
    metro_id    = integer(),
    fullaqsid   = character(),
    policy_id   = integer(),
    start_date  = as.Date(character()),
    end_date    = as.Date(character())
  )

  if (is.null(db)) return(empty)
  if (length(metro_ids) == 0L) return(empty)
  if (length(system_types) == 0L) return(empty)

  metro_arr  = paste0("{", paste(as.integer(metro_ids), collapse = ","), "}")
  system_arr = paste0("{", paste(as.character(system_types), collapse = ","), "}")

  # No zone join, no ST_Intersects: every monitor in a treated metro is a pair.
  q = "
    SELECT m.metro_id, m.fullaqsid, cpp.id AS policy_id,
           cpp.start_date, cpp.end_date
    FROM public.monitors m
    JOIN public.congestion_pricing_periods cpp
      ON cpp.metro_id = m.metro_id
    WHERE cpp.treated = TRUE
      AND cpp.system_type = ANY($1::text[])
      AND m.metro_id      = ANY($2::bigint[])
  "
  res = tryCatch(
    DBI::dbGetQuery(db, q, params = list(system_arr, metro_arr)),
    error = function(e) {
      message("[fetch_metro_treated_pairs] SQL failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(res) || nrow(res) == 0L) return(empty)

  res |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      metro_id   = as.integer(.data$metro_id),
      fullaqsid  = as.character(.data$fullaqsid),
      policy_id  = as.integer(.data$policy_id),
      start_date = as.Date(.data$start_date),
      end_date   = as.Date(.data$end_date)
    ) |>
    dplyr::distinct()
}

# -----------------------------------------------------------------------------
# add_treatment_zones: zone-level analogue of add_treatment()
# -----------------------------------------------------------------------------

#' Mark `treated = TRUE` only for monitors inside a treated zone polygon
#'
#' Zone-level analogue of [add_treatment()]. Joins by **monitor** (`fullaqsid`)
#' and the period date window, instead of by `metro_id`. Monitors absent from
#' `zone_pairs` always get `treated = FALSE` even when their metro has an active
#' policy — they become within-metro controls.
#'
#' @param data Panel tibble with at least `fullaqsid` and `date`.
#' @param zone_pairs Output of [fetch_zone_treated_pairs()].
#' @return `data` with replaced `treated` (logical) and `days_since_treatment`
#'   (integer) columns. `start_date`/`end_date` columns are not added.
#' @export
add_treatment_zones = function(data, zone_pairs){
    if (!"fullaqsid" %in% names(data) || !"date" %in% names(data)) {
        stop("add_treatment_zones() needs 'fullaqsid' and 'date' columns", call. = FALSE)
    }

    if (is.null(zone_pairs) || nrow(zone_pairs) == 0L) {
        return(
            data |>
                select(-any_of(c("treated", "start_date", "end_date",
                                 "treated_policy", "treatment_start_date",
                                 "days_since_treatment"))) |>
                mutate(treated = FALSE,
                       days_since_treatment = NA_integer_)
        )
    }

    pair_windows = zone_pairs |>
        transmute(
            fullaqsid = as.character(.data$fullaqsid),
            start_date = as.Date(.data$start_date),
            end_date   = coalesce(as.Date(.data$end_date), as.Date("9999-12-31"))
        ) |>
        distinct()

    treatment_start_dates = zone_pairs |>
        group_by(.data$fullaqsid) |>
        summarize(treatment_start_date = min(as.Date(.data$start_date)),
                  .groups = "drop") |>
        mutate(fullaqsid = as.character(.data$fullaqsid))

    data |>
        select(-any_of(c("treated", "start_date", "end_date",
                         "treated_policy", "treatment_start_date",
                         "days_since_treatment"))) |>
        mutate(.fullaqsid_chr = as.character(.data$fullaqsid),
               .date_dt = as.Date(.data$date)) |>
        left_join(
            pair_windows |> mutate(treated_policy = TRUE),
            by = join_by(.fullaqsid_chr == fullaqsid,
                         between(.date_dt, start_date, end_date))
        ) |>
        left_join(treatment_start_dates,
                  by = c(".fullaqsid_chr" = "fullaqsid")) |>
        mutate(
            treated = coalesce(.data$treated_policy, FALSE),
            days_since_treatment = as.integer(.data$.date_dt - .data$treatment_start_date)
        ) |>
        select(-any_of(c("treated_policy", "treatment_start_date",
                         "start_date", "end_date",
                         ".fullaqsid_chr", ".date_dt")))
}


add_groups = function(data){
    data = data %>% 
       select(-any_of(c("week", "month", "weekyear", "monthyear"))) %>%
       mutate(week = week(date),
              month = month(date),
              weekyear = paste0(week, "-", year(date)),
              monthyear = paste0(month, "-", year(date)))
}

# -----------------------------------------------------------------------------
# screen_covariates_collinearity: pooled correlation + optional max VIF (screening)
# -----------------------------------------------------------------------------

#' Pooled collinearity screening for covariate columns (not TWFE/CFE-structural VIF)
#'
#' Computes the maximum absolute off-diagonal correlation and, when \pkg{car} is
#' installed, the maximum \code{\link[car]{vif}} from a pooled \code{lm} with a
#' dummy response—same spirit as \code{vif_max_pooled_covariates_fect()} /
#' \code{get_gof_fect()} in \code{job/model/fect/functions_fect.R}. Use for
#' **screening** time-varying \code{covariate_vars} and optional time-invariant
#' \code{z_vars}; re-run when the panel slice changes.
#'
#' @param data A data frame.
#' @param covariate_vars Names of columns to include (time-varying RHS controls).
#' @param z_vars Optional additional column names (e.g. time-invariant \code{Z} columns for \code{fect}).
#' @return One-row tibble: \code{max_abs_corr}, \code{vifmax}, \code{n} (complete cases), \code{note}.
#' @keywords internal
screen_covariates_collinearity = function(data, covariate_vars, z_vars = character()) {
  vars = unique(c(as.character(covariate_vars), as.character(z_vars)))
  vars = vars[nzchar(vars)]
  note = "pooled screening on complete cases; not TWFE/CFE structural VIF or FE-partialed correlations"
  if (length(vars) < 2L) {
    return(tibble::tibble(max_abs_corr = NA_real_, vifmax = NA_real_, n = NA_integer_, note = note))
  }
  miss = setdiff(vars, names(data))
  if (length(miss) > 0L) {
    return(tibble::tibble(
      max_abs_corr = NA_real_,
      vifmax = NA_real_,
      n = NA_integer_,
      note = paste0(note, "; missing columns: ", paste(miss, collapse = ", "))
    ))
  }
  df = data[, vars, drop = FALSE]
  df = df[stats::complete.cases(df), , drop = FALSE]
  n = nrow(df)
  if (n < 3L) {
    return(tibble::tibble(max_abs_corr = NA_real_, vifmax = NA_real_, n = as.integer(n), note = paste0(note, "; insufficient complete rows")))
  }
  cor_m = stats::cor(df, use = "pairwise.complete.obs")
  upper = cor_m[upper.tri(cor_m)]
  max_abs_corr = suppressWarnings(max(abs(upper), na.rm = TRUE))
  if (!is.finite(max_abs_corr)) {
    max_abs_corr = NA_real_
  }

  vifmax = NA_real_
  if (requireNamespace("car", quietly = TRUE)) {
    rhs = paste(vars, collapse = " + ")
    df$.y_screen = stats::rnorm(n)
    f = stats::as.formula(paste0(".y_screen ~ ", rhs))
    m = tryCatch(stats::lm(f, data = df), error = function(e) NULL)
    if (!is.null(m)) {
      v = tryCatch(car::vif(m), error = function(e) NULL)
      if (!is.null(v)) {
        if (is.matrix(v)) {
          vifmax = max(v[, 1L], na.rm = TRUE)
        } else {
          vifmax = max(as.numeric(v), na.rm = TRUE)
        }
      }
    }
  }

  tibble::tibble(max_abs_corr = max_abs_corr, vifmax = vifmax, n = as.integer(n), note = note)
}

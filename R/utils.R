# ==============================================================================
# INTERNAL HELPER FUNCTIONS
# ==============================================================================

#' @importFrom rlang %||%
NULL

# Null-coalescing operator from rlang
`%||%` <- rlang::`%||%`

# Coerce a column to numeric type with proper error handling
# @param df A data frame
# @param col Column name (character) to coerce
# @return The modified data frame
coerce_numeric_col <- function(df, col) {
  if (!is.numeric(df[[col]])) {
    if (is.factor(df[[col]]) || is.character(df[[col]])) {
      stop(sprintf(
        paste0(
          "Cannot compute rolling statistics or moving averages on non-numeric column '%s'. ",
          "Coercing factors or character vectors would yield meaningless numeric codes. ",
          "Convert the column to a numeric type before calling this function."
        ),
        col
      ), call. = FALSE)
    }
    original <- df[[col]]
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    # Check if conversion introduced NAs
    new_nas <- sum(is.na(df[[col]])) - sum(is.na(original))
    if (new_nas > 0) {
      warning(sprintf(
        paste0(
          "Column '%s': Coercion to numeric introduced %d new NA value(s). ",
          "Check that this column contains valid numeric data."
        ),
        col, new_nas
      ), call. = FALSE)
    }
  }

  df
}

# Return a type-appropriate NA value for a given vector
# @param x A vector of any type
# @return A single NA value matching the type of x
.typed_na <- function(x) {
  if (is.factor(x)) return(factor(NA, levels = levels(x)))
  if (inherits(x, "Date")) return(as.Date(NA))
  if (inherits(x, "POSIXlt")) {
    # Preserve POSIXlt class and timezone
    na_val <- as.POSIXlt(NA)
    attr(na_val, "tzone") <- attr(x, "tzone")
    return(na_val)
  }
  if (inherits(x, "POSIXct")) {
    # Preserve timezone for POSIXct
    tz <- attr(x, "tzone") %||% ""
    return(as.POSIXct(NA, tz = tz))
  }
  if (is.integer(x)) return(NA_integer_)
  if (is.numeric(x)) return(NA_real_)
  if (is.logical(x)) return(NA)
  if (is.character(x)) return(NA_character_)
  return(NA)
}

# Determine if a date is the end of month (no lubridate dependency)
# @param x A Date vector
# @return Integer vector (1 = end of month, 0 = not)
.is_eom <- function(x) {
  y <- as.POSIXlt(x)
  next_month_first <- as.Date(sprintf("%d-%02d-01", ifelse(y$mon == 11, y$year + 1901, y$year + 1900),
                                      ifelse(y$mon == 11, 1, y$mon + 2)))
  as.integer(x == (next_month_first - 1))
}

# Create target-based features from a single time series vector (used in recursive forecasting)
# @param y Numeric vector of target values
# @param target_col Name of the target column (for naming features)
# @param p Number of lags to create
# @param q Vector of MA window sizes
# @param roll_windows Vector of rolling window sizes
# @param roll_stats Character vector of rolling statistics to compute
# @param trend_windows Vector of window sizes for trend slopes
# @param trend_degrees Vector of polynomial degrees for trend features
# @return Named list of feature values
.make_target_feats <- function(y, target_col, p = NULL, q = NULL,
                               roll_windows = NULL, roll_stats = c("sum","sd","min","max"),
                               trend_windows = NULL, trend_degrees = NULL) {
  out <- list()
  # lags - p is a vector of specific lag indices (e.g., c(1, 4, 6, 12))
  # Formula parsing already expands p(k) to 1:k, so no expansion needed here
  if (!is.null(p) && length(p) > 0 && all(p > 0)) {
    lag_indices <- as.integer(p)
    for (L in lag_indices) {
      nm <- paste0(target_col, "_lag_", L)
      # Align with dplyr::lag(x, n=L) semantics used during training
      # Training: at row i with y[i], lag_1 column contains y[i-1]
      # Forecasting: given history y, we're forecasting next step, so lag_1 = y[length(y)]
      # Example: y = c(10, 20, 30), we're about to forecast y[4]
      #          lag_1 should be y[3] = 30 (most recent observation)
      #          lag_2 should be y[2] = 20 (second most recent)
      out[[nm]] <- if (length(y) >= L) y[length(y) - L + 1] else NA_real_
    }
  }
  # simple MAs
  if (!is.null(q) && length(q)) {
    for (w in q) {
      nm <- paste0(target_col, "_ma_", w)
      if (length(y) >= w) {
        # mean(..., na.rm=TRUE) returns NaN if all values are NA
        # Convert NaN to NA_real_ to match expected behavior and avoid issues in model adapters
        m <- mean(tail(y, w), na.rm = TRUE)
        out[[nm]] <- if (is.nan(m)) NA_real_ else m
      } else {
        out[[nm]] <- NA_real_
      }
    }
  }
  # trend features
  if (!is.null(trend_degrees) && length(trend_degrees)) {
    trend_idx <- length(y) + 1  # Next step index (forecast step), not current history length
    for (d in trend_degrees) {
      nm <- paste0("trend", d)
      out[[nm]] <- if (d == 1L) trend_idx else trend_idx ^ d
    }
  }
  # rolling stats on the fly
  if (!is.null(roll_windows) && length(roll_windows)) {
    for (w in roll_windows) {
      # CRITICAL: Match training behavior which uses .complete = TRUE
      # Return NA if window is incomplete (length(y) < w)
      # This prevents distribution shift between training and forecasting
      if (length(y) < w) {
        # Incomplete window - return NA for all stats (matches feat_rolling_dt behavior)
        if ("sum" %in% roll_stats) out[[paste0(target_col, "_rollsum_", w)]] <- NA_real_
        if ("sd"  %in% roll_stats) out[[paste0(target_col, "_rollsd_",  w)]] <- NA_real_
        if ("min" %in% roll_stats) out[[paste0(target_col, "_rollmin_", w)]] <- NA_real_
        if ("max" %in% roll_stats) out[[paste0(target_col, "_rollmax_", w)]] <- NA_real_
        next
      }

      # Full window available
      seg <- tail(y, w)
      # Return NA if all values in window are NA (prevents sum->0, min->Inf, max->-Inf)
      has_valid <- length(seg) > 0 && any(!is.na(seg))

      if ("sum" %in% roll_stats) {
        out[[paste0(target_col, "_rollsum_", w)]] <- if (has_valid) sum(seg, na.rm = TRUE) else NA_real_
      }
      if ("sd"  %in% roll_stats) {
        out[[paste0(target_col, "_rollsd_",  w)]] <- if (has_valid && length(seg[!is.na(seg)]) >= 2) {
          stats::sd(seg, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
      if ("min" %in% roll_stats) {
        out[[paste0(target_col, "_rollmin_", w)]] <- if (has_valid) min(seg, na.rm = TRUE) else NA_real_
      }
      if ("max" %in% roll_stats) {
        out[[paste0(target_col, "_rollmax_", w)]] <- if (has_valid) max(seg, na.rm = TRUE) else NA_real_
      }
    }
  }
  # trend slopes
  if (!is.null(trend_windows) && length(trend_windows)) {
    for (w in trend_windows) {
      nm <- paste0(target_col, "_rollslope_", w)

      # Match training behavior: return NA if window is incomplete
      if (length(y) < w) {
        out[[nm]] <- NA_real_
        next
      }

      seg <- tail(y, w)
      if (all(is.na(seg)) || length(seg) < 2) {
        out[[nm]] <- NA_real_
      } else {
        x <- seq_along(seg)
        out[[nm]] <- as.numeric(coef(lm(seg ~ x))[2])
      }
    }
  }
  out
}

# Add calendar features to a data frame
#
# Supports both Date and POSIXct datetime columns.
# Sub-daily features (hod, moh) require POSIXct input.
#
# @param df Data frame with date/datetime column
# @param date_col Name of the date column
# @param dow Logical, add day of week
# @param month Logical, add month
# @param woy Logical, add week of year
# @param eom Logical, add end of month indicator
# @param dom Logical, add day of month
# @param hod Logical, add hour of day (0-23) - requires POSIXct
# @param moh Logical, add minute of hour (0-59) - requires POSIXct
# @param holidays Date vector or data frame of holiday dates
# @return Data frame with calendar features added
#' @importFrom dplyr mutate left_join select any_of
#' @importFrom tidyr as_tibble
.add_calendar_feats <- function(df, date_col, dow = FALSE, month = FALSE,
                                 woy = FALSE, eom = FALSE, dom = FALSE,
                                 hod = FALSE, moh = FALSE, holidays = NULL) {
  result <- df
  is_datetime <- inherits(df[[date_col]], "POSIXct")


  # Daily calendar features (work with both Date and POSIXct)
  # Use strftime("%u") for locale-independent weekday numbers (1=Monday, 7=Sunday)
  if (dow) {
    result <- result %>% mutate(
      dow = factor(strftime(.data[[date_col]], "%u"), levels = sprintf("%d", 1:7))
    )
  }
  if (month) result <- result %>% mutate(month = factor(format(.data[[date_col]], "%m"), levels = sprintf("%02d", 1:12)))
  if (dom)   result <- result %>% mutate(dom = as.integer(format(.data[[date_col]], "%d")))
  if (eom)   result <- result %>% mutate(eom = .is_eom(.data[[date_col]]))
  if (woy)   result <- result %>% mutate(woy = as.integer(strftime(.data[[date_col]], "%V")))

  # Sub-daily calendar features (require POSIXct)
  if (hod) {
    if (!is_datetime) {
      warning("hod() requires POSIXct datetime; Date column will return 0 for all rows")
      result <- result %>% mutate(hod = 0L)
    } else {
      result <- result %>% mutate(hod = as.integer(format(.data[[date_col]], "%H")))
    }
  }
  if (moh) {
    if (!is_datetime) {
      warning("moh() requires POSIXct datetime; Date column will return 0 for all rows")
      result <- result %>% mutate(moh = 0L)
    } else {
      result <- result %>% mutate(moh = as.integer(format(.data[[date_col]], "%M")))
    }
  }

  # Holidays
  if (!is.null(holidays)) {
    # Handle different input formats: Date vector or data frame
    if (inherits(holidays, "Date")) {
      hol <- tibble(!!date_col := holidays)
    } else {
      hol <- as_tibble(holidays)
      # Use first column as the date column, coerce to Date
      first_col <- names(hol)[1]
      hol[[first_col]] <- as.Date(hol[[first_col]])
      names(hol)[1] <- date_col
    }
    # Deduplicate holidays to prevent row expansion during join
    hol <- hol %>% distinct()

    # For POSIXct series, join on date part only (holidays are typically daily)
    if (is_datetime) {
      result <- result %>%
        mutate(.hol_join_date = as.Date(.data[[date_col]])) %>%
        mutate(holiday = 0L)
      hol <- hol %>%
        rename(.hol_join_date = !!date_col) %>%
        distinct(.data$.hol_join_date, .keep_all = TRUE) %>%
        mutate(holiday = 1L)
      result <- result %>%
        left_join(hol, by = ".hol_join_date") %>%
        mutate(holiday = coalesce(holiday.y, holiday.x)) %>%
        select(-any_of(c("holiday.x", "holiday.y", ".hol_join_date")))
    } else {
      result <- result %>%
        mutate(holiday = 0L) %>%
        left_join(hol %>% distinct(!!rlang::sym(date_col), .keep_all = TRUE) %>% mutate(holiday = 1L), by = date_col) %>%
        mutate(holiday = coalesce(holiday.y, holiday.x)) %>%
        select(-any_of(c("holiday.x", "holiday.y")))
    }
  }
  result
}

# Helper to create named list of functions for mutate across
# @param vec Vector of values to iterate over
# @param fn_gen Function that generates a function for each value
# @param name_gen Function that generates a name for each value
# @return Named list of functions
list_to_named_list <- function(vec, fn_gen, name_gen) {
  fns <- lapply(vec, fn_gen)
  names(fns) <- sapply(vec, name_gen)
  fns
}

# Harmonize a data frame to match a predictor schema (for forecasting)
# @param df Data frame to harmonize
# @param schema List of predictor schemas (name, type, levels)
# @return Harmonized data frame
#' @importFrom dplyr select all_of
.harmonize_to_schema <- function(df, schema) {
  if (is.null(schema) || !length(schema)) return(df)
  result <- df
  for (sc in schema) {
    v <- sc$name
    if (!v %in% names(result)) {
      if (!is.null(sc$levels)) {
        result[[v]] <- factor(NA_character_, levels = sc$levels)
      } else if (identical(sc$type, "integer")) {
        result[[v]] <- NA_integer_
      } else if (identical(sc$type, "numeric")) {
        result[[v]] <- NA_real_
      } else if (identical(sc$type, "character")) {
        result[[v]] <- NA_character_
      } else if (identical(sc$type, "Date")) {
        result[[v]] <- as.Date(NA)
      } else if (identical(sc$type, "POSIXct")) {
        tz <- sc$tzone %||% ""
        result[[v]] <- as.POSIXct(NA, tz = tz)
      } else if (identical(sc$type, "POSIXlt")) {
        na_val <- as.POSIXlt(NA)
        attr(na_val, "tzone") <- sc$tzone %||% ""
        result[[v]] <- na_val
      } else if (identical(sc$type, "logical")) {
        result[[v]] <- NA
      } else {
        result[[v]] <- NA
      }
    }
    if (!is.null(sc$levels)) {
      old_val <- result[[v]]
      result[[v]] <- factor(as.character(result[[v]]), levels = sc$levels)

      # Check if any values became NA due to unknown levels
      new_nas <- sum(is.na(result[[v]])) - sum(is.na(old_val))
      if (new_nas > 0) {
        unknown_levels <- setdiff(unique(as.character(old_val)[!is.na(old_val)]), sc$levels)
        if (length(unknown_levels) > 0) {
          warning(sprintf(
            "Column '%s': %d value(s) with unknown factor level(s) [%s] converted to NA (not seen during training)",
            v, new_nas, paste(unknown_levels, collapse = ", ")
          ), call. = FALSE)
        }
      }
    } else if (identical(sc$type, "integer")) {
      result[[v]] <- as.integer(result[[v]])
    } else if (identical(sc$type, "numeric")) {
      result[[v]] <- as.numeric(result[[v]])
    } else if (identical(sc$type, "logical")) {
      result[[v]] <- as.logical(result[[v]])
    } else if (identical(sc$type, "character")) {
      result[[v]] <- as.character(result[[v]])
    } else if (identical(sc$type, "Date")) {
      result[[v]] <- as.Date(result[[v]])
    } else if (identical(sc$type, "POSIXct")) {
      tz <- sc$tzone %||% ""
      result[[v]] <- as.POSIXct(result[[v]], tz = tz)
    } else if (identical(sc$type, "POSIXlt")) {
      tz <- sc$tzone %||% ""
      result[[v]] <- as.POSIXlt(result[[v]], tz = tz)
    }
  }
  result
}

# Select predictor columns from a data frame
# @param df Data frame
# @param predictor_names Character vector of predictor column names
# @return Data frame with only predictor columns
#' @importFrom dplyr select all_of
.select_predictors <- function(df, predictor_names) {
  if (is.null(predictor_names) || !length(predictor_names)) {
    return(df[0, , drop = FALSE][1, , drop = FALSE])
  }
  df %>% select(all_of(predictor_names))
}

# Return x if not NULL, otherwise return empty default
# @param x Value to check
# @param empty Default value if x is NULL
# @return x or empty
.get_or_empty <- function(x, empty) if (is.null(x)) empty else x

#' Generate future dates/datetimes using stored frequency (stable approach)
#'
#' This function generates future dates in a stable way using explicit frequency.
#' Supports both Date and POSIXct datetime objects. For sub-daily frequencies
#' (second, minute, halfhour, hour), uses POSIXct arithmetic. For daily+ frequencies,
#' uses seq.Date or seq.POSIXt as appropriate.
#'
#' Special handling:
#' - businessday: Generates daily dates but filters out weekends (Sat/Sun)
#' - biweekly: Generates dates 14 days apart
#'
#' @param last_date The last observed date (Date or POSIXct object)
#' @param h Integer, number of future periods
#' @param frequency Frequency specification (character like "month", "hour", or numeric like 30)
#' @param historical_dates Historical date vector (for fallback)
#'
#' @return Vector of future dates/datetimes (same class as last_date)
#' @importFrom stats median
#' @noRd
.generate_future_dates_stable <- function(last_date, h, frequency = NULL, historical_dates = NULL) {
  is_datetime <- inherits(last_date, "POSIXct")

  # If frequency is provided, use stable generation
  if (!is.null(frequency)) {
    # Handle businessday specially: generate more dates than needed, filter weekends
    if (is.character(frequency) && frequency == "businessday") {
      return(.generate_business_days(last_date, h))
    }

    if (is.character(frequency)) {
      # Map frequency to seq() 'by' argument
      by_arg <- switch(frequency,
        # Sub-daily (require POSIXct)
        second = "sec",
        minute = "min",
        halfhour = "30 mins",
        hour = "hour",
        # Daily+
        day = "day",
        biweekly = "14 days",
        week = "week",
        month = "month",
        quarter = "quarter",
        year = "year",
        frequency  # fallback: pass through
      )

      # seq.Date(from, length.out=n, by=freq) generates: from, from+freq, ..., from+(n-1)*freq
      # We want the h dates AFTER last_date, so we generate h+1 and remove the first
      if (is_datetime) {
        future_dates <- seq(last_date, length.out = h + 1, by = by_arg)
      } else {
        future_dates <- seq(last_date, length.out = h + 1, by = by_arg)
      }
      return(future_dates[-1])  # Remove first element (last_date), keep h future dates

    } else if (is.numeric(frequency)) {
      # For numeric frequency: add multiples of the frequency to last_date
      # Numeric frequency is in days for Date objects
      if (is_datetime) {
        # Use seq.POSIXt with "days" to handle DST correctly
        # frequency is in days, so generate sequence with that step
        future_dates <- seq(last_date, by = paste(frequency, "days"), length.out = h + 1)
        return(future_dates[-1])
      } else {
        return(last_date + seq_len(h) * frequency)
      }
    }
  }

  # Fallback: use median-based approach (old behavior)
  if (!is.null(historical_dates)) {
    d <- historical_dates[!is.na(historical_dates)]
    if (length(d) >= 2) {
      step_num <- as.integer(stats::median(diff(d), na.rm = TRUE))
      # For POSIXct, diff() returns seconds; for Date, diff() returns days
      if (is_datetime) {
        # Use seq.POSIXt for DST-safe date arithmetic
        # Convert seconds to days for seq() (approximate, but DST-aware)
        step_days <- step_num / 86400
        future_dates <- seq(last_date, by = paste(step_days, "days"), length.out = h + 1)
        return(future_dates[-1])
      } else {
        return(last_date + seq_len(h) * step_num)
      }
    }
  }

  # Default fallback: 1 day (DST-safe for POSIXct)
  if (is_datetime) {
    future_dates <- seq(last_date, by = "1 day", length.out = h + 1)
    return(future_dates[-1])

  } else {
    return(last_date + seq_len(h))
  }
}

#' Generate future business days (weekdays only)
#'
#' Generates h future dates, skipping weekends (Saturday and Sunday).
#' Works with both Date and POSIXct objects.
#'
#' @param last_date The last observed date (Date or POSIXct)
#' @param h Integer, number of business days to generate
#' @return Vector of h business day dates/datetimes
#' @noRd
.generate_business_days <- function(last_date, h) {
  is_datetime <- inherits(last_date, "POSIXct")

  # Generate more dates than needed to account for weekends
  # Worst case: 2 weekends per 5 business days, so generate ~1.5x more
  n_generate <- ceiling(h * 1.5) + 10

  if (is_datetime) {
    candidates <- seq(last_date, length.out = n_generate + 1, by = "day")
  } else {
    candidates <- seq(last_date, length.out = n_generate + 1, by = "day")
  }

  # Remove first element (last_date itself)
  candidates <- candidates[-1]

  # Filter out weekends using numeric wday (0=Sunday, 6=Saturday) for locale-independence
  wday_values <- as.POSIXlt(candidates)$wday
  is_weekend <- wday_values %in% c(0, 6)

  business_days <- candidates[!is_weekend]

  if (length(business_days) < h) {
    # Rare case: need to generate more dates
    # Recursively call with more dates
    return(.generate_business_days(last_date, h + 20)[1:h])
  }

  return(business_days[1:h])
}

# Parse formula to extract feature specifications
# @param formula Formula object specifying the model
# @param data Data frame being modeled
# @return List with parsed feature specifications
.parse_fit_formula <- function(formula, data) {
  trm <- terms(formula)
  target_col <- all.vars(formula[[2]])[1]
  terms_chr  <- attr(trm, "term.labels")

  parse_num_list <- function(x) {
    xs <- trimws(strsplit(x, ",")[[1]])
    as.integer(xs[nzchar(xs)])
  }

  spec <- list(
    target       = target_col,
    p            = NULL,
    q            = integer(),
    cal          = list(dow=FALSE, month=FALSE, woy=FALSE, eom=FALSE, dom=FALSE, hod=FALSE, moh=FALSE),
    trend        = integer(),
    raw_vars     = character(),
    xreg         = character(),
    xreg_lags    = list(),
    xreg_ma      = list(),
    rolls        = list(sum = integer(), sd = integer(), min = integer(), max = integer(), slope = integer()),
    requested_cols = character()
  )

  for (t in terms_chr) {
    # Target lags: p(k), p(1:k), p(c(1, 4, 6, 12))
    # Supports three forms:
    #   p(12)             - k lags (lag_1 through lag_12)
    #   p(1:12)           - range of lags (lag_1 through lag_12)
    #   p(c(1, 4, 6, 12)) - explicit set of lags
    if (grepl("^p\\(", t)) {
      inner <- sub("^p\\((.*)\\)$", "\\1", t)
      inner <- trimws(inner)

      # Safe parsing without eval() - only allow numeric patterns
      lags <- if (grepl("^-?\\d+$", inner)) {
        # Single integer: p(12) means 12 lags (1:12), matching documentation
        k <- as.integer(inner)
        if (k < 1) {
          stop("p(...): lags must be positive integers. Got: ", inner)
        }
        seq_len(k)
      } else if (grepl("^-?\\d+:-?\\d+$", inner)) {
        # Range: p(1:12)
        parts <- as.integer(strsplit(inner, ":")[[1]])
        seq.int(parts[1], parts[2])
      } else if (grepl("^c\\(", inner)) {
        # Explicit vector: p(c(1, 4, 6, 12))
        # Extract content inside c(...) and parse as comma-separated integers
        c_inner <- sub("^c\\((.*)\\)$", "\\1", inner)
        # Only allow digits, commas, minus, and whitespace (validated later)
        if (!grepl("^[\\d,\\s-]+$", c_inner, perl = TRUE)) {
          stop("p(c(...)): only positive integers allowed inside c(). Got: ", inner)
        }
        parts <- strsplit(c_inner, ",")[[1]]
        as.integer(trimws(parts))
      } else {
        stop("p(...): unrecognized lag specification '", inner, "'. ",
             "Use p(k) for k lags (1:k), p(a:b) for range, or p(c(1,4,6)) for explicit set.")
      }

      # Validate
      if (length(lags) == 0 || any(is.na(lags)) || any(lags < 1)) {
        stop("p(...): lags must be positive integers. Got: ", inner)
      }
      # Deduplicate and sort
      lags <- sort(unique(lags))
      spec$p <- lags
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_lag_", lags))
      next
    }
    # Target moving averages: q(...)
    if (grepl("^q\\(", t)) {
      qs <- parse_num_list(sub("^q\\(([^)]*)\\)$", "\\1", t))
      if (length(qs)==0 || any(!is.finite(qs) | qs < 1)) stop("q(...): windows must be positive integers.")
      spec$q <- unique(c(spec$q, qs))
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_ma_", qs))
      next
    }

    # Calendar features (daily and sub-daily)
    # Daily: dow(), month(), woy(), eom(), dom()
    # Sub-daily: hod() (hour of day), moh() (minute of hour)
    if (t %in% c("dow()", "month()", "woy()", "eom()", "dom()", "hod()", "moh()")) {
      if (t=="dow()")   { spec$cal$dow   <- TRUE; spec$requested_cols <- c(spec$requested_cols, "dow") }
      if (t=="month()") { spec$cal$month <- TRUE; spec$requested_cols <- c(spec$requested_cols, "month") }
      if (t=="woy()")   { spec$cal$woy   <- TRUE; spec$requested_cols <- c(spec$requested_cols, "woy") }
      if (t=="eom()")   { spec$cal$eom   <- TRUE; spec$requested_cols <- c(spec$requested_cols, "eom") }
      if (t=="dom()")   { spec$cal$dom   <- TRUE; spec$requested_cols <- c(spec$requested_cols, "dom") }
      if (t=="hod()")   { spec$cal$hod   <- TRUE; spec$requested_cols <- c(spec$requested_cols, "hod") }
      if (t=="moh()")   { spec$cal$moh   <- TRUE; spec$requested_cols <- c(spec$requested_cols, "moh") }
      next
    }

    # Trend features: trend() or trend(1,2,3)
    if (grepl("^trend\\(", t)) {
      inside <- sub("^trend\\(([^)]*)\\)$", "\\1", t)
      if (nzchar(inside)) {
        degrees <- parse_num_list(inside)
        if (length(degrees)==0 || any(!is.finite(degrees) | degrees < 1)) stop("trend(...): degrees must be positive integers.")
      } else {
        degrees <- 1L
      }
      spec$trend <- sort(unique(c(spec$trend, degrees)))
      spec$requested_cols <- c(spec$requested_cols, paste0("trend", degrees))
      next
    }

    # Rolling statistics on target
    if (grepl("^rollsum\\(", t)) {
      ws <- parse_num_list(sub("^rollsum\\(([^)]*)\\)$", "\\1", t))
      if (length(ws)==0 || any(!is.finite(ws) | ws < 1)) stop("rollsum(...): windows must be positive integers.")
      spec$rolls$sum <- sort(unique(c(spec$rolls$sum, ws)))
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_rollsum_", ws))
      next
    }
    if (grepl("^rollsd\\(", t)) {
      ws <- parse_num_list(sub("^rollsd\\(([^)]*)\\)$", "\\1", t))
      if (length(ws)==0 || any(!is.finite(ws) | ws < 1)) stop("rollsd(...): windows must be positive integers.")
      spec$rolls$sd <- sort(unique(c(spec$rolls$sd, ws)))
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_rollsd_", ws))
      next
    }
    if (grepl("^rollmin\\(", t)) {
      ws <- parse_num_list(sub("^rollmin\\(([^)]*)\\)$", "\\1", t))
      if (length(ws)==0 || any(!is.finite(ws) | ws < 1)) stop("rollmin(...): windows must be positive integers.")
      spec$rolls$min <- sort(unique(c(spec$rolls$min, ws)))
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_rollmin_", ws))
      next
    }
    if (grepl("^rollmax\\(", t)) {
      ws <- parse_num_list(sub("^rollmax\\(([^)]*)\\)$", "\\1", t))
      if (length(ws)==0 || any(!is.finite(ws) | ws < 1)) stop("rollmax(...): windows must be positive integers.")
      spec$rolls$max <- sort(unique(c(spec$rolls$max, ws)))
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_rollmax_", ws))
      next
    }
    if (grepl("^rollslope\\(", t)) {
      ws <- parse_num_list(sub("^rollslope\\(([^)]*)\\)$", "\\1", t))
      if (length(ws)==0 || any(!is.finite(ws) | ws < 2)) stop("rollslope(...): windows must be integers >= 2.")
      spec$rolls$slope <- sort(unique(c(spec$rolls$slope, ws)))
      spec$requested_cols <- c(spec$requested_cols, paste0(spec$target, "_rollslope_", ws))
      next
    }

    # Exogenous variable lags: lag(var, ...)
    if (grepl("^lag\\(", t)) {
      inside <- sub("^lag\\(([^)]*)\\)$", "\\1", t)
      parts  <- strsplit(inside, ",")[[1]]
      var    <- trimws(parts[1])
      if (!nzchar(var) || !var %in% names(data)) stop(sprintf("lag(): unknown variable '%s'", var))
      ks     <- as.integer(trimws(parts[-1]))
      if (length(ks)==0 || any(!is.finite(ks) | ks < 0)) stop("lag(var, ...): lags must be non-negative integers.")
      spec$xreg <- unique(c(spec$xreg, var))
      spec$xreg_lags[[var]] <- sort(unique(c(spec$xreg_lags[[var]] %||% integer(), ks)))
      Lpos <- setdiff(ks, 0L)
      if (length(Lpos)) spec$requested_cols <- c(spec$requested_cols, paste0(var, "_lag_", Lpos))
      if (0L %in% ks)   spec$requested_cols <- c(spec$requested_cols, var)
      next
    }
    # Exogenous variable MAs: ma(var, ...)
    if (grepl("^ma\\(", t)) {
      inside <- sub("^ma\\(([^)]*)\\)$", "\\1", t)
      parts  <- strsplit(inside, ",")[[1]]
      var    <- trimws(parts[1])
      if (!nzchar(var) || !var %in% names(data)) stop(sprintf("ma(): unknown variable '%s'", var))
      ws     <- as.integer(trimws(parts[-1]))
      if (length(ws)==0 || any(!is.finite(ws) | ws < 1)) stop("ma(var, ...): windows must be positive integers.")
      spec$xreg <- unique(c(spec$xreg, var))
      spec$xreg_ma[[var]] <- sort(unique(c(spec$xreg_ma[[var]] %||% integer(), ws)))
      spec$requested_cols <- c(spec$requested_cols, paste0(var, "_ma_", ws))
      next
    }

    # Raw column (no transformation)
    if (!t %in% names(data)) stop(sprintf("Unsupported term '%s' (not a special and not a column).", t))
    spec$raw_vars <- c(spec$raw_vars, t)
    spec$requested_cols <- c(spec$requested_cols, t)
  }

  spec$requested_cols <- unique(spec$requested_cols)
  spec$raw_vars       <- unique(spec$raw_vars)
  spec
}

#' Create a TimeSeries Object with Complete Preprocessing Pipeline
#'
#' This function creates a TimeSeries object that bundles data with frequency information
#' and provides a comprehensive preprocessing pipeline for time series data. It handles:
#' \enumerate{
#'   \item Frequency detection and validation
#'   \item Irregular calendar completion (missing dates)
#'   \item Target variable gap-filling
#'   \item Exogenous variable gap-filling
#' }
#'
#' All preprocessing is auditable via metadata and \code{is_imputed} flags.
#'
#' @param data A data frame containing the time series data
#' @param date Character string naming the date column. Accepts:
#'   \itemize{
#'     \item \strong{Date}: For daily and longer frequencies (day, week, month, quarter, year)
#'     \item \strong{POSIXct}: Required for sub-daily frequencies (second, minute, halfhour, hour)
#'   }
#' @param groups Optional character vector naming grouping columns for panel data.
#'   All preprocessing operations (time grid completion, gap-filling) are performed
#'   independently per group.
#' @param frequency Character string or numeric specifying the time frequency:
#'   \itemize{
#'     \item \strong{Sub-daily} (require POSIXct): "second", "minute", "halfhour", "hour"
#'     \item \strong{Daily+} (work with Date or POSIXct): "day", "businessday", "biweekly",
#'       "week", "month", "quarter", "year"
#'     \item \strong{Numeric}: Custom interval in days (e.g., 7 for weekly, 14 for biweekly)
#'   }
#'   If NULL and \code{auto_detect = TRUE}, frequency is inferred from median date/time differences.
#' @param auto_detect Logical, if TRUE and frequency is NULL, attempt to detect frequency
#'   from the data. Default: TRUE.
#'
#' @param target Character string naming the target column (optional).
#'   Required if \code{target_na} is specified. The target column will be gap-filled
#'   according to the \code{target_na} strategy.
#'
#' @param target_na List specifying gap-filling strategy for missing target values.
#'   \itemize{
#'     \item \code{strategy}: Character, one of:
#'       \itemize{
#'         \item \strong{"error"} - Fail if NAs present (forces explicit choice)
#'         \item \strong{"zero"} - Replace NAs with 0 (useful for count data)
#'         \item \strong{"locf"} - Last observation carried forward
#'         \item \strong{"nocb"} - Next observation carried backward
#'         \item \strong{"linear"} - Linear interpolation (time-aware)
#'         \item \strong{"rolling_mean"} - Centered or right-aligned rolling mean
#'         \item \strong{"stl"} - Seasonal-Trend-Loess decomposition (auto-detects period)
#'         \item \strong{"borrow"} - Cross-series borrowing from peer groups
#'         \item \strong{"custom"} - User-provided function
#'       }
#'     \item \code{params}: List of strategy-specific parameters (see Details)
#'   }
#'   Default: NULL (no target gap-filling). Adds \code{{target}_is_imputed} flag column.
#'
#' @param fill_time Logical, if TRUE, complete missing dates using \code{tidyr::complete()}.
#'   Uses the \code{frequency} parameter to determine step size. When enabled, adds rows
#'   for missing dates with NA values in all dynamic columns. Respects group boundaries
#'   for panel data. Default: FALSE.
#'
#' @param xreg_na Named list specifying gap-filling strategies for exogenous columns.
#'   Each element should be: \code{column_name = list(strategy = "...", params = list(...))}.
#'   \itemize{
#'     \item \strong{Keys}: Column names to fill (must exist in \code{data})
#'     \item \strong{Values}: Lists with \code{strategy} and \code{params} (same as \code{target_na})
#'   }
#'   Example: \code{list(price = list(strategy = "locf"), promo = list(strategy = "zero"))}.
#'   Each column gets its own \code{{column}_is_imputed} flag. Filling is done per-group
#'   if \code{groups} is specified.
#'
#' @return A \code{TimeSeries} object (S3 class) with components:
#'   \itemize{
#'     \item \code{data} - Data frame with completed calendar and filled gaps (if requested)
#'     \item \code{date} - Name of the date column
#'     \item \code{groups} - Names of grouping columns (or NULL)
#'     \item \code{frequency} - Time frequency specification
#'     \item \code{target} - Name of the target column (or NULL)
#'     \item \code{target_na_meta} - Metadata about target gap-filling (or NULL):
#'       \itemize{
#'         \item \code{strategy} - Strategy used
#'         \item \code{params} - Parameters used
#'         \item \code{n_imputed} - Number of values imputed
#'         \item \code{n_total} - Total observations
#'         \item \code{pct_imputed} - Percentage imputed
#'       }
#'     \item \code{xreg_na_meta} - Named list of metadata for each exogenous column (or empty list)
#'     \item \code{time_fill_meta} - Metadata about time grid completion (or NULL):
#'       \itemize{
#'         \item \code{n_added} - Number of rows added
#'         \item \code{by} - Step size used
#'         \item \code{n_total} - Total observations after completion
#'       }
#'   }
#'
#' @section Preprocessing Pipeline Order:
#' TimeSeries() applies preprocessing in this order:
#' \enumerate{
#'   \item \strong{Sort data} by groups (if present) and date
#'   \item \strong{Complete time grid} (if \code{fill_time$enabled = TRUE})
#'     \itemize{
#'       \item Per-group for panel data
#'       \item Adds rows for missing dates with NA values
#'     }
#'   \item \strong{Fill target} (if \code{target} and \code{target_na} specified)
#'     \itemize{
#'       \item Uses specified strategy
#'       \item Adds \code{{target}_is_imputed} flag
#'     }
#'   \item \strong{Fill exogenous columns} (if \code{xreg_na} specified)
#'     \itemize{
#'       \item Per-column, per-group filling
#'       \item Adds \code{{column}_is_imputed} flag for each
#'     }
#' }
#'
#' @section Gap-Filling Strategy Parameters:
#' Common parameters across strategies:
#' \itemize{
#'   \item \code{max_gap}: Maximum consecutive NAs to fill (default: Inf).
#'         Throws error if gap exceeds this value.
#' }
#'
#' Strategy-specific parameters:
#' \itemize{
#'   \item \strong{linear}: \code{extrapolate = FALSE} - Allow extrapolation beyond observed range
#'   \item \strong{rolling_mean}: \code{window = 7, center = TRUE} - Window size and alignment
#'   \item \strong{stl}: \code{period = NULL, robust = TRUE} - Seasonal period (auto-detected if NULL) and robust fitting
#'   \item \strong{borrow}: \code{method = "median", neighbors = NULL} - Aggregation method and neighbor filtering
#'   \item \strong{custom}: \code{fn = function(y, dates, params) {...}} - User-provided function
#' }
#'
#' See \code{?fill_gaps} for detailed documentation of each strategy.
#'
#' @section Auditability:
#' All gap-filling operations add \code{is_imputed} flags:
#' \itemize{
#'   \item \code{{target}_is_imputed} - Logical vector marking imputed target values
#'   \item \code{{column}_is_imputed} - Logical vector for each exogenous column
#' }
#'
#' These flags can be used to:
#' \itemize{
#'   \item Filter imputed rows: \code{data[!data$sales_is_imputed, ]}
#'   \item Use as model predictor: \code{sales ~ ... + sales_is_imputed}
#'   \item Weight observations: \code{lm(..., weights = ifelse(is_imputed, 0.5, 1))}
#' }
#'
#' Metadata is stored in the TimeSeries object for full reproducibility.
#'
#' @export
#' @importFrom stats median
#' @examples
#' \dontrun{
#' # ===== Basic Usage =====
#' # Create TimeSeries with auto-detected frequency
#' ts <- TimeSeries(retail, date = "date", groups = "store")
#'
#' # Specify frequency explicitly
#' ts <- TimeSeries(retail, date = "date", groups = "store", frequency = "day")
#'
#' # ===== Target Gap-Filling =====
#' # Fill target with last observation carried forward
#' ts <- TimeSeries(
#'   retail,
#'   date = "date",
#'   target = "sales",
#'   target_na = list(strategy = "locf", params = list(max_gap = 7))
#' )
#'
#' # Fill target with seasonal decomposition
#' ts <- TimeSeries(
#'   retail,
#'   date = "date",
#'   groups = "store",
#'   target = "sales",
#'   target_na = list(strategy = "stl", params = list(period = 7))
#' )
#'
#' # ===== Complete Preprocessing Pipeline =====
#' # Handle irregular calendar + target gaps + exogenous gaps
#' ts <- TimeSeries(
#'   sales_df,
#'   date = "date",
#'   groups = c("store", "item"),
#'   frequency = "day",
#'   target = "sales",
#'   target_na = list(strategy = "locf"),
#'   fill_time = TRUE,  # Complete missing dates using frequency
#'   xreg_na = list(
#'     price = list(strategy = "linear"),  # Smooth interpolation
#'     promo = list(strategy = "zero")     # NA = no promotion
#'   )
#' )
#'
#' # Inspect preprocessing results
#' print(ts)  # Shows all metadata
#' summary(ts$data$sales_is_imputed)  # Check how many values imputed
#'
#' # ===== Using with fit() and forecast() =====
#' # fit() extracts cleaned data automatically
#' m <- fit(sales ~ p(7) + price + promo, data = ts, model = lm)
#'
#' # forecast() uses stored frequency
#' fc <- forecast(m, h = 14)
#'
#' # ===== Panel Data Example =====
#' # Each store gets independent preprocessing
#' ts <- TimeSeries(
#'   panel_df,
#'   date = "date",
#'   groups = "store",
#'   frequency = "day",
#'   target = "sales",
#'   target_na = list(strategy = "borrow", params = list(method = "median")),
#'   fill_time = TRUE,  # Uses frequency = "day"
#'   xreg_na = list(
#'     price = list(strategy = "locf"),
#'     temp = list(strategy = "linear")
#'   )
#' )
#'
#' # Metadata shows per-store/column imputation counts
#' ts$time_fill_meta  # Rows added to complete calendar
#' ts$target_na_meta  # Target imputation stats
#' ts$xreg_na_meta    # Exogenous imputation stats per column
#' }
TimeSeries <- function(data, date, groups = NULL, frequency = NULL, auto_detect = TRUE,
                       target = NULL, target_na = NULL,
                       fill_time = FALSE,
                       xreg_na = NULL) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(date), length(date) == 1, date %in% names(data))

  # Accept both Date and POSIXct datetime columns
  # POSIXct is required for sub-daily frequencies (second, minute, halfhour, hour)
  is_datetime <- inherits(data[[date]], "POSIXct")
  is_date <- inherits(data[[date]], "Date")

  if (!is_datetime && !is_date) {
    stop("`date` column must be of class Date or POSIXct. ",
         "Use POSIXct for sub-daily frequencies (hour, minute, etc.).")
  }

  if (!is.null(groups)) {
    stopifnot(is.character(groups))
    missing_groups <- setdiff(groups, names(data))
    if (length(missing_groups) > 0) {
      stop("Group column(s) not found: ", paste(missing_groups, collapse = ", "))
    }
  }

  # CRITICAL: Sort data by groups and date to ensure correct lag/diff calculations
  if (!is.null(groups)) {
    data <- data %>% dplyr::arrange(dplyr::across(dplyr::all_of(c(groups, date))))
  } else {
    data <- data %>% dplyr::arrange(dplyr::across(dplyr::all_of(date)))
  }

  # Auto-detect frequency if not provided
  if (is.null(frequency) && auto_detect) {
    frequency <- .detect_frequency(data, date, groups)
    message("Auto-detected frequency: ", .format_frequency(frequency))
  }

  # Validate frequency
  if (!is.null(frequency)) {
    frequency <- .validate_frequency(frequency)
  } else {
    stop("Frequency must be specified or auto_detect must be TRUE.")
  }

  # Validate that sub-daily frequencies have POSIXct datetime column
  if (.is_subdaily_frequency(frequency) && !is_datetime) {
    stop("Sub-daily frequency '", frequency, "' requires a POSIXct datetime column. ",
         "Convert your date column using as.POSIXct() or lubridate::as_datetime().")
  }

  # Store datetime type for later use
  datetime_type <- if (is_datetime) "POSIXct" else "Date"

  # Initialize metadata
  time_fill_meta <- NULL
  target_na_meta <- NULL
  xreg_na_meta <- list()

  # 1. Complete time grid if requested
  n_orig <- nrow(data)
  if (isTRUE(fill_time)) {
    step_size <- .frequency_to_by(frequency)

    # Check for NA dates before completing grid
    if (any(is.na(data[[date]]))) {
      stop("Cannot complete time grid: NA values found in date column '", date, "'. ",
           "Remove or impute NA dates before using fill_time=TRUE.")
    }

    if (!is.null(groups) && length(groups) > 0) {
      # Grouped data: complete grid per group
      data <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
        tidyr::complete(
          !!rlang::sym(date) := seq(min(!!rlang::sym(date), na.rm = TRUE), max(!!rlang::sym(date), na.rm = TRUE), by = step_size)
        ) %>%
        dplyr::ungroup()

      # NOTE: We intentionally do NOT fill other columns with group values.
      # Previously, columns not in {date, groups, target, xreg_na} were treated as "static"
      # and filled with the first non-NA value per group. This caused data corruption for
      # dynamic exogenous variables (e.g., price) that weren't listed in xreg_na.
      #
      # Now, all columns except date/groups remain NA on filled rows. Users should:
      # 1. Include dynamic exogenous variables in xreg_na if they need NA handling
      # 2. Handle static columns (like store_name) separately if needed
    } else {
      # Ungrouped data: complete grid globally
      data <- data %>%
        tidyr::complete(
          !!rlang::sym(date) := seq(min(!!rlang::sym(date), na.rm = TRUE), max(!!rlang::sym(date), na.rm = TRUE), by = step_size)
        )
    }

    n_after_complete <- nrow(data)
    n_weekends_removed <- 0L

    # For businessday frequency, filter out weekends (Saturday=6, Sunday=0 in wday)
    if (identical(frequency, "businessday")) {
      wday_values <- as.POSIXlt(data[[date]])$wday
      is_weekend <- wday_values %in% c(0, 6)
      n_weekends_removed <- sum(is_weekend)
      data <- data[!is_weekend, ]
    }

    n_added <- n_after_complete - n_orig
    n_net_change <- nrow(data) - n_orig
    time_fill_meta <- list(
      n_added = n_added,
      n_weekends_removed = n_weekends_removed,
      n_net_change = n_net_change,
      by = step_size,
      n_total = nrow(data)
    )

    if (n_added > 0 || n_weekends_removed > 0) {
      msg <- sprintf("Time grid completed: %d rows added", n_added)
      if (n_weekends_removed > 0) {
        msg <- sprintf("%s, %d weekend rows removed", msg, n_weekends_removed)
      }
      msg <- sprintf("%s (step size: %s)", msg, step_size)
      message(msg)
    }
  }

  # 2. Fill target column if requested
  if (!is.null(target) && !is.null(target_na)) {
    # Validate target column
    if (!target %in% names(data)) {
      stop("Target column '", target, "' not found in data")
    }

    # Extract strategy and params
    strategy <- target_na$strategy %||% "error"
    params <- target_na$params %||% list()

    # Apply gap-filling
    n_before <- sum(is.na(data[[target]]))
    data <- fill_gaps(data, target, date, groups, strategy, params)
    n_after <- sum(is.na(data[[target]]))

    # Store metadata
    target_na_meta <- list(
      strategy = strategy,
      params = params,
      n_imputed = n_before - n_after,
      n_total = nrow(data),
      pct_imputed = 100 * (n_before - n_after) / nrow(data)
    )

    message("Target gap-filling: ", n_before - n_after, " values imputed (",
            sprintf("%.1f%%", target_na_meta$pct_imputed), ") using '", strategy, "' strategy")
  }

  # 3. Fill exogenous columns if requested
  if (!is.null(xreg_na) && length(xreg_na) > 0) {
    for (col in names(xreg_na)) {
      # Validate column exists
      if (!col %in% names(data)) {
        stop("Exogenous column '", col, "' specified in xreg_na not found in data")
      }

      cfg <- xreg_na[[col]]
      strategy <- cfg$strategy %||% "error"
      params <- cfg$params %||% list()

      # Count NAs before filling
      n_before <- sum(is.na(data[[col]]))

      # Apply filling per group
      if (!is.null(groups) && length(groups) > 0) {
        data <- data %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
          dplyr::group_modify(~ .fill_column_with_strategy(.x, col, date, strategy, params)) %>%
          dplyr::ungroup()
      } else {
        data <- .fill_column_with_strategy(data, col, date, strategy, params)
      }

      n_after <- sum(is.na(data[[col]]))

      # Store metadata
      xreg_na_meta[[col]] <- list(
        strategy = strategy,
        params = params,
        n_imputed = n_before - n_after,
        n_total = nrow(data),
        pct_imputed = 100 * (n_before - n_after) / nrow(data)
      )

      message("Exogenous '", col, "': ", n_before - n_after, " values imputed (",
              sprintf("%.1f%%", xreg_na_meta[[col]]$pct_imputed), ") using '", strategy, "' strategy")
    }
  }

  # Warn if fill_time created trailing NAs in target that would block forecasting
  if (isTRUE(fill_time) && !is.null(target) && is.null(target_na)) {
    # Check for trailing NAs in each group (or globally if ungrouped)
    if (!is.null(groups) && length(groups) > 0) {
      trailing_na_check <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
        dplyr::arrange(dplyr::across(dplyr::all_of(date)), .by_group = TRUE) %>%
        dplyr::summarise(
          has_trailing_na = is.na(dplyr::last(.data[[target]])),
          .groups = "drop"
        )
      n_groups_with_trailing_na <- sum(trailing_na_check$has_trailing_na)
      if (n_groups_with_trailing_na > 0) {
        warning(
          "fill_time=TRUE created rows where '", target, "' is NA at the end of ",
          n_groups_with_trailing_na, " group(s). ",
          "This will cause forecast() to fail with 'trailing NA' error. ",
          "Consider setting target_na to fill these gaps, or ensure your data ",
          "ends with non-NA target values before calling TimeSeries().",
          call. = FALSE
        )
      }
    } else {
      # Ungrouped: check if last row has NA target
      data_sorted <- data %>% dplyr::arrange(dplyr::across(dplyr::all_of(date)))
      if (is.na(data_sorted[[target]][nrow(data_sorted)])) {
        warning(
          "fill_time=TRUE created rows where '", target, "' is NA at the end of the series. ",
          "This will cause forecast() to fail with 'trailing NA' error. ",
          "Consider setting target_na to fill these gaps, or ensure your data ",
          "ends with non-NA target values before calling TimeSeries().",
          call. = FALSE
        )
      }
    }
  }

  structure(
    list(
      data = data,
      date = date,
      groups = groups,
      frequency = frequency,
      datetime_type = datetime_type,  # "POSIXct" or "Date"
      target = target,
      target_na_meta = target_na_meta,
      xreg_na_meta = xreg_na_meta,
      time_fill_meta = time_fill_meta
    ),
    class = "TimeSeries"
  )
}

#' Print method for TimeSeries
#' @param x A TimeSeries object
#' @param ... Additional arguments (unused)
#' @export
print.TimeSeries <- function(x, ...) {
  cat("TimeSeries object\n")
  cat("----------------\n")
  cat("Date column:", x$date, "(", x$datetime_type, ")\n")
  cat("Frequency:", .format_frequency(x$frequency), "\n")
  if (!is.null(x$groups)) {
    cat("Groups:", paste(x$groups, collapse = ", "), "\n")
  }
  cat("Observations:", nrow(x$data), "\n")

  # Show time grid completion info
  if (!is.null(x$time_fill_meta) && x$time_fill_meta$n_added > 0) {
    cat("Time grid:", x$time_fill_meta$n_added, "rows added (step:", x$time_fill_meta$by, ")\n")
  }

  # Show target filling info
  if (!is.null(x$target)) {
    cat("Target:", x$target)
    if (!is.null(x$target_na_meta) && x$target_na_meta$n_imputed > 0) {
      cat(" [", x$target_na_meta$strategy, ": ",
          x$target_na_meta$n_imputed, " imputed, ",
          sprintf("%.1f%%", x$target_na_meta$pct_imputed), "]", sep = "")
    }
    cat("\n")
  }

  # Show exogenous filling info
  if (!is.null(x$xreg_na_meta) && length(x$xreg_na_meta) > 0) {
    cat("Exogenous:\n")
    for (col in names(x$xreg_na_meta)) {
      meta <- x$xreg_na_meta[[col]]
      if (meta$n_imputed > 0) {
        cat("  ", col, " [", meta$strategy, ": ",
            meta$n_imputed, " imputed, ",
            sprintf("%.1f%%", meta$pct_imputed), "]\n", sep = "")
      } else {
        cat("  ", col, " [", meta$strategy, ": no gaps]\n", sep = "")
      }
    }
  }

  cat("\nData (first few rows):\n")
  print(head(x$data))
  invisible(x)
}

#' Detect frequency from data
#'
#' Supports both Date and POSIXct datetime objects.
#' For POSIXct, differences are computed in seconds to detect sub-daily frequencies.
#' For Date, differences are computed in days.
#'
#' @importFrom stats median sd
#' @importFrom dplyr group_by summarise across all_of %>%
#' @noRd
.detect_frequency <- function(data, date, groups = NULL) {
  # Check if datetime is POSIXct (sub-daily capable) or Date
  is_datetime <- inherits(data[[date]], "POSIXct")

  # Calculate median difference
  if (!is.null(groups) && length(groups) > 0) {
    # For grouped data, compute median per group
    diffs <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups))) %>%
      dplyr::summarise(
        diff_median = {
          d <- .data[[date]]
          d <- d[!is.na(d)]
          if (length(d) >= 2) {
            # diff() returns difftime for both Date and POSIXct
            # For POSIXct: convert to seconds for sub-daily detection
            # For Date: convert to days for daily+ detection
            raw_diff <- diff(d)
            if (inherits(raw_diff, "difftime")) {
              # Note: is_datetime is captured from outer scope
              if (inherits(d, "POSIXct")) {
                as.numeric(stats::median(as.numeric(raw_diff, units = "secs"), na.rm = TRUE))
              } else {
                as.numeric(stats::median(as.numeric(raw_diff, units = "days"), na.rm = TRUE))
              }
            } else {
              as.numeric(stats::median(raw_diff, na.rm = TRUE))
            }
          } else {
            NA_real_
          }
        },
        .groups = "drop"
      )

    # Check if all groups have similar frequency
    valid_diffs <- diffs$diff_median[!is.na(diffs$diff_median)]
    if (length(valid_diffs) == 0) {
      stop("Cannot detect frequency: insufficient data across all groups.")
    }

    # Check variance - if too high, groups have different frequencies
    if (length(valid_diffs) > 1) {
      cv <- stats::sd(valid_diffs) / mean(valid_diffs)  # Coefficient of variation
      if (cv > 0.15) {  # More than 15% variation
        unit <- if (is_datetime) "seconds" else "days"
        warning(paste0(
          "Groups appear to have different frequencies. ",
          "Median differences range from ", round(min(valid_diffs), 1),
          " to ", round(max(valid_diffs), 1), " ", unit, ". ",
          "Using median across groups, but consider specifying frequency explicitly."
        ))
      }
    }

    # Use median of group medians
    median_diff <- stats::median(valid_diffs)
  } else {
    d <- data[[date]]
    d <- d[!is.na(d)]
    if (length(d) < 2) {
      stop("Cannot detect frequency: need at least 2 date observations.")
    }
    # diff() returns difftime for both Date and POSIXct
    # For POSIXct: convert to seconds for sub-daily detection
    # For Date: convert to days for daily+ detection
    raw_diff <- diff(d)
    if (inherits(raw_diff, "difftime")) {
      if (is_datetime) {
        # POSIXct: use seconds for sub-daily frequencies
        median_diff <- as.numeric(stats::median(as.numeric(raw_diff, units = "secs"), na.rm = TRUE))
      } else {
        # Date: use days for daily+ frequencies
        median_diff <- as.numeric(stats::median(as.numeric(raw_diff, units = "days"), na.rm = TRUE))
      }
    } else {
      median_diff <- as.numeric(stats::median(raw_diff, na.rm = TRUE))
    }
  }

  # Map to standard frequencies
  if (is.na(median_diff)) {
    stop("Cannot detect frequency: insufficient data.")
  }

  # Check for duplicate timestamps (median_diff = 0)
  if (median_diff == 0) {
    stop("Cannot detect frequency: duplicate timestamps detected. ",
         "Remove duplicate timestamps or specify frequency explicitly.")
  }

  # Route to appropriate detection based on datetime type

  if (is_datetime) {
    return(.detect_frequency_from_seconds(median_diff, data, date))
  } else {
    return(.detect_frequency_from_days(median_diff, data, date))
  }
}

#' Detect frequency from median difference in seconds (for POSIXct data)
#' @noRd
.detect_frequency_from_seconds <- function(median_diff_secs, data = NULL, date = NULL) {
  # Tolerance as percentage of expected value
  tol_pct <- 0.1  # 10% tolerance

  # Sub-daily frequencies (in seconds)
  if (abs(median_diff_secs - 1) < 1) {
    return("second")
  } else if (abs(median_diff_secs - 60) < 60 * tol_pct) {
    return("minute")
  } else if (abs(median_diff_secs - 1800) < 1800 * tol_pct) {  # 30 min
    return("halfhour")
  } else if (abs(median_diff_secs - 3600) < 3600 * tol_pct) {  # 1 hour
    return("hour")
  }

  # Daily+ frequencies (convert to days)
  median_diff_days <- median_diff_secs / 86400

  # Check for businessday pattern: ~1 day on average but skips weekends

  if (abs(median_diff_days - 1) < 0.5) {
    # Could be daily or business day - check weekend pattern if data available
    # Use numeric wday (0=Sunday, 6=Saturday) for locale-independence
    if (!is.null(data) && !is.null(date)) {
      wday_values <- as.POSIXlt(data[[date]])$wday
      has_weekends <- any(wday_values %in% c(0, 6))
      if (!has_weekends) {
        return("businessday")
      }
    }
    return("day")
  } else if (abs(median_diff_days - 7) < 0.5) {
    return("week")
  } else if (abs(median_diff_days - 14) < 1) {
    return("biweekly")
  } else if (median_diff_days >= 28 && median_diff_days <= 31) {
    return("month")
  } else if (median_diff_days >= 89 && median_diff_days <= 92) {
    return("quarter")
  } else if (median_diff_days >= 364 && median_diff_days <= 367) {
    return("year")
  } else {
    # Return numeric (in days for consistency)
    return(as.integer(round(median_diff_days)))
  }
}

#' Detect frequency from median difference in days (for Date data)
#' @noRd
.detect_frequency_from_days <- function(median_diff_days, data = NULL, date = NULL) {
  # Tolerance for matching
  tol <- 0.5

  # Check for businessday pattern: ~1 day on average but skips weekends
  if (abs(median_diff_days - 1) < tol) {
    # Could be daily or business day - check weekend pattern if data available
    if (!is.null(data) && !is.null(date)) {
      wday_values <- as.POSIXlt(data[[date]])$wday
      has_weekends <- any(wday_values %in% c(0, 6))
      if (!has_weekends) {
        return("businessday")
      }
    }
    return("day")
  } else if (abs(median_diff_days - 7) < tol) {
    return("week")
  } else if (abs(median_diff_days - 14) < 1) {
    return("biweekly")
  } else if (median_diff_days >= 28 && median_diff_days <= 31) {
    return("month")
  } else if (median_diff_days >= 89 && median_diff_days <= 92) {
    return("quarter")
  } else if (median_diff_days >= 364 && median_diff_days <= 367) {
    return("year")
  } else {
    # Return numeric for custom intervals
    return(as.integer(round(median_diff_days)))
  }
}

#' Validate frequency specification
#' @noRd
.validate_frequency <- function(frequency) {
  # Sub-daily frequencies require POSIXct datetime objects
  # Daily+ frequencies work with both Date and POSIXct
  valid_strings <- c(
    # Sub-daily (require POSIXct)
    "second", "minute", "halfhour", "hour",
    # Daily+ (work with Date or POSIXct)
    "day", "businessday", "biweekly", "week", "month", "quarter", "year"
  )

  if (is.character(frequency)) {
    if (length(frequency) != 1) {
      stop("Frequency must be a single character string or numeric value.")
    }
    if (!frequency %in% valid_strings) {
      stop("Invalid frequency string. Valid values: ",
           paste(valid_strings, collapse = ", "))
    }
    return(frequency)
  } else if (is.numeric(frequency)) {
    if (length(frequency) != 1 || frequency <= 0) {
      stop("Numeric frequency must be a single positive value.")
    }
    return(as.integer(frequency))
  } else {
    stop("Frequency must be character or numeric.")
  }
}

#' Check if frequency is sub-daily (requires POSIXct)
#' @noRd
.is_subdaily_frequency <- function(frequency) {
  is.character(frequency) && frequency %in% c("second", "minute", "halfhour", "hour")
}

#' Get seasonal periods for a frequency (based on Hyndman's article)
#' @noRd
.get_seasonal_periods <- function(frequency) {
  if (is.numeric(frequency)) {
    # Custom numeric frequency: approximate yearly seasonality
    return(c(round(365.25 / frequency)))
  }

  switch(frequency,
    second = c(60, 3600, 86400, 604800),      # minute, hour, day, week
    minute = c(60, 1440, 10080),               # hour, day, week
    halfhour = c(48, 336, 17532),              # day, week, year
    hour = c(24, 168, 8766),                   # day, week, year
    day = c(7, 365),                           # week, year
    businessday = c(5, 260),                   # week, year (approx)
    biweekly = c(26),                          # year
    week = c(52),                              # year
    month = c(12),                             # year
    quarter = c(4),                            # year
    year = c(1),                               # no seasonality
    c(1)                                       # fallback
  )
}

#' Convert frequency to duration in seconds (for sub-daily) or days (for daily+)
#' @noRd
.frequency_to_duration <- function(frequency) {
  if (is.numeric(frequency)) {
    return(list(value = frequency, unit = "days"))
  }

  switch(frequency,
    second = list(value = 1, unit = "secs"),
    minute = list(value = 60, unit = "secs"),
    halfhour = list(value = 1800, unit = "secs"),
    hour = list(value = 3600, unit = "secs"),
    day = list(value = 1, unit = "days"),
    businessday = list(value = 1, unit = "days"),  # special handling needed
    biweekly = list(value = 14, unit = "days"),
    week = list(value = 7, unit = "days"),
    month = list(value = NA, unit = "months"),     # variable length
    quarter = list(value = NA, unit = "quarters"), # variable length
    year = list(value = NA, unit = "years"),       # variable length
    stop("Unknown frequency: ", frequency)
  )
}

#' Format frequency for printing
#' @noRd
.format_frequency <- function(frequency) {
  if (is.character(frequency)) {
    # Provide friendly names for new frequencies
    friendly <- switch(frequency,
      second = "second",
      minute = "minute",
      halfhour = "half-hour (30 min)",
      hour = "hour",
      day = "day",
      businessday = "business day (weekdays)",
      biweekly = "bi-weekly (14 days)",
      week = "week",
      month = "month",
      quarter = "quarter",
      year = "year",
      frequency  # fallback
    )
    return(friendly)
  } else if (is.numeric(frequency)) {
    return(paste(frequency, "days"))
  } else {
    return("unknown")
  }
}

#' Convert frequency to seq.Date or seq.POSIXt 'by' parameter
#'
#' Maps chronofeat frequency strings to R's seq() 'by' argument format.
#' For sub-daily frequencies, returns values suitable for seq.POSIXt().
#' For daily+ frequencies, returns values suitable for seq.Date().
#'
#' @noRd
.frequency_to_by <- function(frequency) {
  if (is.character(frequency)) {
    # Map to R's seq() 'by' argument
    by_arg <- switch(frequency,
      # Sub-daily (for seq.POSIXt)
      second = "sec",
      minute = "min",
      halfhour = "30 mins",
      hour = "hour",
      # Daily+ (for seq.Date or seq.POSIXt)
      day = "day",
      businessday = "day",  # handled specially in date generation
      biweekly = "14 days",
      week = "week",
      month = "month",
      quarter = "quarter",
      year = "year",
      frequency  # fallback: pass through
    )
    return(by_arg)
  } else if (is.numeric(frequency)) {
    return(paste(frequency, "days"))  # "7 days", "14 days", etc.
  } else {
    stop("Invalid frequency type")
  }
}

#' Fill a single column using gap-filling strategy
#'
#' Dispatcher that mirrors fill_gaps() but works on a single column
#'
#' @param df Data frame (for one group)
#' @param col Character, name of column to fill
#' @param date Character, name of date column
#' @param strategy Character, gap-filling strategy
#' @param params List of strategy-specific parameters
#' @return Data frame with filled column and is_imputed flag
#' @noRd
.fill_column_with_strategy <- function(df, col, date, strategy, params = list()) {
  # Call existing filler infrastructure
  result <- .fill_single_series(
    y = df[[col]],
    dates = df[[date]],
    strategy = strategy,
    params = params
  )

  df[[col]] <- result$values
  df[[paste0(col, "_is_imputed")]] <- result$is_imputed
  df
}


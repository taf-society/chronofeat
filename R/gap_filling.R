#' Fill Missing Target Values with Configurable Strategies
#'
#' Apply gap-filling strategies to handle missing target values in time series data.
#' This ensures train/forecast parity by making features deterministic and auditable.
#'
#' @param data Data frame with time series data
#' @param target Character, name of target column
#' @param date Character, name of date column
#' @param groups Character vector of group column names (NULL for ungrouped)
#' @param strategy Character, gap-filling strategy. One of:
#'   \itemize{
#'     \item \code{"error"} - Fail if any NAs present (default, forces explicit choice)
#'     \item \code{"zero"} - Replace NAs with 0 (appropriate for count data)
#'     \item \code{"locf"} - Last observation carried forward
#'     \item \code{"nocb"} - Next observation carried backward
#'     \item \code{"linear"} - Linear interpolation (Phase 2)
#'     \item \code{"rolling_mean"} - Centered rolling mean (Phase 2)
#'     \item \code{"stl"} - Seasonal decomposition (Phase 3)
#'     \item \code{"borrow"} - Cross-series borrowing for panel data (Phase 3)
#'     \item \code{"custom"} - User-provided function (Phase 4)
#'   }
#' @param params List of strategy-specific parameters. Common parameters:
#'   \itemize{
#'     \item \code{max_gap} - Maximum gap length to fill (for locf, nocb)
#'     \item \code{extrapolate} - Allow extrapolation (for linear)
#'     \item \code{window} - Window size (for rolling_mean)
#'     \item \code{period} - Seasonal period (for stl)
#'     \item \code{fn} - User function (for custom)
#'   }
#'
#' @return Data frame with:
#'   \itemize{
#'     \item Original columns
#'     \item Filled target column
#'     \item \code{{target}_is_imputed} - Logical flag indicating imputed values
#'   }
#'
#' @details
#' The \code{is_imputed} flag allows downstream models to:
#' \itemize{
#'   \item Filter out imputed rows if desired
#'   \item Use the flag as a predictor to learn different behavior
#'   \item Weight imputed observations differently
#' }
#'
#' Gap-filling respects group boundaries and never fills across groups.
#' Each group's time series is filled independently.
#'
#' @export
#' @importFrom dplyr %>% group_by across all_of group_split bind_rows
#' @examples
#' \dontrun{
#' # Retail sales: missing = no sale
#' sales_filled <- fill_gaps(sales_data, target = "revenue", date = "date",
#'                           groups = "store", strategy = "zero")
#'
#' # Sensor data: carry forward up to 3 missing readings
#' sensor_filled <- fill_gaps(sensor_data, target = "temperature",
#'                            date = "timestamp", groups = "device",
#'                            strategy = "locf",
#'                            params = list(max_gap = 3))
#'
#' # Check imputation summary
#' table(sensor_filled$temperature_is_imputed)
#'
#' # Use filled data in forecasting
#' ts <- TimeSeries(sensor_filled, date = "timestamp", groups = "device")
#' m <- fit(temperature ~ p(7) + rollsum(7), data = ts, model = lm)
#' }
fill_gaps <- function(data, target, date, groups = NULL,
                      strategy = "error", params = list()) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(target), length(target) == 1, target %in% names(data))
  stopifnot(is.character(date), length(date) == 1, date %in% names(data))

  if (!is.null(groups)) {
    stopifnot(is.character(groups))
    missing_groups <- setdiff(groups, names(data))
    if (length(missing_groups) > 0) {
      stop("Group column(s) not found: ", paste(missing_groups, collapse = ", "))
    }
  }

  .fill_target_gaps(data, target, date, groups, strategy, params)
}


#' Gap-Filling Dispatcher (Internal)
#'
#' Fill missing target values using specified strategy.
#'
#' @param data Data frame with time series data
#' @param target_col Character, name of target column
#' @param date_col Character, name of date column
#' @param groups_chr Character vector of group column names (NULL for ungrouped)
#' @param strategy Character, one of: "error", "zero", "locf", "nocb", "linear", "rolling_mean", "stl", "borrow", "custom"
#' @param params List of strategy-specific parameters
#'
#' @return Data frame with filled target and is_imputed flag column
#' @keywords internal
.fill_target_gaps <- function(data, target_col, date_col, groups_chr = NULL,
                               strategy = "error", params = list()) {

  # Validate strategy
  valid_strategies <- c("error", "zero", "locf", "nocb", "linear", "rolling_mean", "stl", "borrow", "custom")
  if (!strategy %in% valid_strategies) {
    stop("Invalid strategy '", strategy, "'. Must be one of: ", paste(valid_strategies, collapse = ", "))
  }

  # Check if target column exists
  if (!target_col %in% names(data)) {
    stop("Target column '", target_col, "' not found in data")
  }

  # Sort data by date (and groups if present) to ensure time order
  # This prevents LOCF/NOCB from producing wrong results on unsorted data
  if (!is.null(groups_chr) && length(groups_chr) > 0) {
    data <- data %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(c(groups_chr, date_col))))
  } else {
    data <- data %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(date_col)))
  }

  # Initialize is_imputed flag column
  imputed_col <- paste0(target_col, "_is_imputed")
  data[[imputed_col]] <- FALSE

  # If strategy is "error", check for NAs and fail
  if (strategy == "error") {
    n_missing <- sum(is.na(data[[target_col]]))
    if (n_missing > 0) {
      stop(
        "Found ", n_missing, " missing values in target column '", target_col, "'. ",
        "Please specify a gap-filling strategy via target_na parameter. ",
        "See ?TimeSeries for available strategies."
      )
    }
    return(data)
  }

  # Apply gap-filling per group
  if (!is.null(groups_chr) && length(groups_chr) > 0) {
    # Special handling for "borrow" strategy which needs access to all series
    if (strategy == "borrow") {
      # Preserve original row order: .fill_borrow_grouped() reorders by group
      data$.row_id <- seq_len(nrow(data))
      was_na <- is.na(data[[target_col]])  # Track original NAs before filling
      data <- .fill_borrow_grouped(data, target_col, date_col, groups_chr, params)
      # Restore original order before applying was_na flags
      data <- data[order(data$.row_id), , drop = FALSE]
      data[[imputed_col]] <- !is.na(data[[target_col]]) & was_na
      data$.row_id <- NULL
    } else {
      # Standard per-group filling
      data_split <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groups_chr))) %>%
        dplyr::group_split()

      filled_groups <- lapply(data_split, function(grp) {
        result <- .fill_single_series(
          y = grp[[target_col]],
          dates = grp[[date_col]],
          strategy = strategy,
          params = params
        )
        grp[[target_col]] <- result$values
        grp[[imputed_col]] <- result$is_imputed
        grp
      })

      data <- dplyr::bind_rows(filled_groups)
    }
  } else {
    # Ungrouped data: fill entire series
    result <- .fill_single_series(
      y = data[[target_col]],
      dates = data[[date_col]],
      strategy = strategy,
      params = params
    )
    data[[target_col]] <- result$values
    data[[imputed_col]] <- result$is_imputed
  }

  data
}


#' Fill Single Time Series
#'
#' Apply gap-filling strategy to a single time series.
#'
#' @param y Numeric vector with potential NAs
#' @param dates Date vector (same length as y)
#' @param strategy Character, gap-filling strategy
#' @param params List of strategy-specific parameters
#'
#' @return List with:
#'   \itemize{
#'     \item values: Numeric vector (filled)
#'     \item is_imputed: Logical vector indicating imputed positions
#'   }
#' @keywords internal
.fill_single_series <- function(y, dates, strategy, params = list()) {

  # Track which values were originally NA
  is_na <- is.na(y)

  # If no NAs, return as-is
  if (!any(is_na)) {
    return(list(values = y, is_imputed = rep(FALSE, length(y))))
  }

  # Dispatch to strategy-specific function
  fill_result <- switch(
    strategy,
    zero = list(values = .fill_zero(y), is_imputed = is_na),
    locf = list(values = .fill_locf(y, params), is_imputed = is_na),
    nocb = list(values = .fill_nocb(y, params), is_imputed = is_na),
    linear = list(values = .fill_linear(y, dates, params), is_imputed = is_na),
    rolling_mean = list(values = .fill_rolling_mean(y, params), is_imputed = is_na),
    stl = list(values = .fill_stl(y, dates, params), is_imputed = is_na),
    borrow = stop("Borrow strategy requires multiple series, not applicable to single series fill"),
    custom = .fill_custom(y, dates, params),  # Custom returns full list
    stop("Strategy '", strategy, "' not implemented yet")
  )

  # Validate custom result
  if (strategy == "custom") {
    if (length(fill_result$is_imputed) != length(y)) {
      stop("Custom fill function returned is_imputed of length ", length(fill_result$is_imputed),
           ", expected ", length(y))
    }
  }

  fill_result
}


#' Zero-Fill Strategy
#'
#' Replace all NAs with 0. Appropriate for count data where missing = no events.
#'
#' @param y Numeric vector
#' @return Filled numeric vector
#' @keywords internal
.fill_zero <- function(y) {
  y[is.na(y)] <- 0
  y
}


#' Last Observation Carried Forward (LOCF)
#'
#' Fill NAs by carrying forward the last non-NA value.
#'
#' @param y Numeric vector
#' @param params List with:
#'   \itemize{
#'     \item max_gap: Maximum gap length to fill (default: Inf)
#'   }
#' @return Filled numeric vector
#' @keywords internal
.fill_locf <- function(y, params = list()) {
  max_gap <- params$max_gap %||% Inf

  # Check for leading NAs
  if (is.na(y[1])) {
    stop("Cannot use LOCF strategy with leading NAs. Consider nocb or another strategy.")
  }

  # Check gap lengths
  if (is.finite(max_gap)) {
    gap_lengths <- rle(is.na(y))
    na_gaps <- gap_lengths$lengths[gap_lengths$values]
    if (any(na_gaps > max_gap)) {
      stop("Gap of length ", max(na_gaps), " exceeds max_gap=", max_gap)
    }
  }

  # Fill using zoo::na.locf
  if (requireNamespace("zoo", quietly = TRUE)) {
    return(zoo::na.locf(y, na.rm = FALSE))
  }

  # Fallback: manual implementation
  for (i in seq_along(y)) {
    if (is.na(y[i]) && i > 1) {
      y[i] <- y[i - 1]
    }
  }
  y
}


#' Next Observation Carried Backward (NOCB)
#'
#' Fill NAs by carrying backward the next non-NA value.
#'
#' @param y Numeric vector
#' @param params List with:
#'   \itemize{
#'     \item max_gap: Maximum gap length to fill (default: Inf)
#'   }
#' @return Filled numeric vector
#' @keywords internal
.fill_nocb <- function(y, params = list()) {
  max_gap <- params$max_gap %||% Inf

  # Check for trailing NAs
  if (is.na(y[length(y)])) {
    stop("Cannot use NOCB strategy with trailing NAs. Consider locf or another strategy.")
  }

  # Check gap lengths
  if (is.finite(max_gap)) {
    gap_lengths <- rle(is.na(y))
    na_gaps <- gap_lengths$lengths[gap_lengths$values]
    if (any(na_gaps > max_gap)) {
      stop("Gap of length ", max(na_gaps), " exceeds max_gap=", max_gap)
    }
  }

  # Fill using zoo::na.locf with fromLast=TRUE
  if (requireNamespace("zoo", quietly = TRUE)) {
    return(zoo::na.locf(y, na.rm = FALSE, fromLast = TRUE))
  }

  # Fallback: manual implementation (reverse, locf, reverse)
  y_rev <- rev(y)
  for (i in seq_along(y_rev)) {
    if (is.na(y_rev[i]) && i > 1) {
      y_rev[i] <- y_rev[i - 1]
    }
  }
  rev(y_rev)
}


#' Linear Interpolation
#'
#' Fill NAs using linear interpolation between known points.
#'
#' @param y Numeric vector
#' @param dates Date vector
#' @param params List with:
#'   \itemize{
#'     \item extrapolate: Allow extrapolation beyond known range (default: FALSE)
#'     \item max_gap: Maximum gap length to fill (default: Inf)
#'   }
#' @return Filled numeric vector
#' @keywords internal
.fill_linear <- function(y, dates, params = list()) {
  extrapolate <- params$extrapolate %||% FALSE
  max_gap <- params$max_gap %||% Inf

  # Check for sufficient non-NA values
  non_na_idx <- which(!is.na(y))
  if (length(non_na_idx) < 2) {
    stop("Linear interpolation requires at least 2 non-NA values")
  }

  # Check gap lengths
  if (is.finite(max_gap)) {
    gap_lengths <- rle(is.na(y))
    na_gaps <- gap_lengths$lengths[gap_lengths$values]
    if (any(na_gaps > max_gap)) {
      stop("Gap of length ", max(na_gaps), " exceeds max_gap=", max_gap)
    }
  }

  # Handle leading/trailing NAs
  first_valid <- min(non_na_idx)
  last_valid <- max(non_na_idx)

  if (!extrapolate && (first_valid > 1 || last_valid < length(y))) {
    stop("Linear interpolation with extrapolate=FALSE cannot fill leading or trailing NAs. ",
         "Set extrapolate=TRUE or use locf/nocb for edges.")
  }

  # Use zoo::na.approx if available
  if (requireNamespace("zoo", quietly = TRUE)) {
    # zoo uses time-based interpolation if x is provided
    result <- tryCatch({
      zoo::na.approx(zoo::zoo(y, as.numeric(dates)), na.rm = FALSE, rule = if (extrapolate) 2 else 1)
    }, error = function(e) {
      # Fallback to simple interpolation
      zoo::na.approx(y, na.rm = FALSE)
    })
    return(as.numeric(result))
  }

  # Fallback: manual linear interpolation
  y_filled <- y
  for (i in seq_along(y_filled)) {
    if (is.na(y_filled[i])) {
      # Find previous and next non-NA
      prev_indices <- which(!is.na(y[1:(i-1)]))
      next_indices <- which(!is.na(y[(i+1):length(y)]))

      if (length(prev_indices) > 0 && length(next_indices) > 0) {
        # Internal gap - interpolate
        prev_idx <- max(prev_indices)
        next_idx <- min(next_indices) + i

        x0 <- as.numeric(dates[prev_idx])
        x1 <- as.numeric(dates[next_idx])
        xi <- as.numeric(dates[i])
        y0 <- y[prev_idx]
        y1 <- y[next_idx]

        y_filled[i] <- y0 + (y1 - y0) * (xi - x0) / (x1 - x0)
      } else if (extrapolate) {
        # Leading or trailing NA with extrapolation
        if (length(prev_indices) > 0) {
          # Trailing NA - extrapolate forward
          prev_idx <- max(prev_indices)
          if (prev_idx >= 2) {
            # Use last two points for slope
            prev_prev_idx <- max(which(!is.na(y[1:(prev_idx-1)])))
            if (length(prev_prev_idx) > 0) {
              slope <- (y[prev_idx] - y[prev_prev_idx]) /
                       as.numeric(dates[prev_idx] - dates[prev_prev_idx])
              y_filled[i] <- y[prev_idx] + slope * as.numeric(dates[i] - dates[prev_idx])
            } else {
              y_filled[i] <- y[prev_idx]  # Constant extrapolation
            }
          } else {
            y_filled[i] <- y[prev_idx]  # Only one point, constant
          }
        } else if (length(next_indices) > 0) {
          # Leading NA - extrapolate backward
          next_idx <- min(next_indices) + i
          if (next_idx <= length(y) - 1) {
            # Use first two points for slope
            next_next_indices <- which(!is.na(y[(next_idx+1):length(y)]))
            if (length(next_next_indices) > 0) {
              next_next_idx <- min(next_next_indices) + next_idx
              slope <- (y[next_next_idx] - y[next_idx]) /
                       as.numeric(dates[next_next_idx] - dates[next_idx])
              y_filled[i] <- y[next_idx] - slope * as.numeric(dates[next_idx] - dates[i])
            } else {
              y_filled[i] <- y[next_idx]  # Constant extrapolation
            }
          } else {
            y_filled[i] <- y[next_idx]  # Only one point, constant
          }
        }
      }
    }
  }
  y_filled
}


#' Rolling Mean Imputation
#'
#' Fill NAs using centered rolling mean of nearby observations.
#'
#' @param y Numeric vector
#' @param params List with:
#'   \itemize{
#'     \item window: Window size (default: 7)
#'     \item center: Center the window (default: TRUE)
#'     \item max_gap: Maximum gap length to fill (default: Inf)
#'   }
#' @return Filled numeric vector
#' @keywords internal
.fill_rolling_mean <- function(y, params = list()) {
  window <- params$window %||% 7
  center <- params$center %||% TRUE
  max_gap <- params$max_gap %||% Inf

  # Check gap lengths
  if (is.finite(max_gap)) {
    gap_lengths <- rle(is.na(y))
    na_gaps <- gap_lengths$lengths[gap_lengths$values]
    if (any(na_gaps > max_gap)) {
      stop("Gap of length ", max(na_gaps), " exceeds max_gap=", max_gap)
    }
  }

  # Use zoo::rollapply if available
  if (requireNamespace("zoo", quietly = TRUE)) {
    # Compute rolling mean, preserving NAs
    if (center) {
      # Centered window
      half_win <- floor(window / 2)
      rolled <- zoo::rollapply(zoo::zoo(y), width = window, FUN = mean, na.rm = TRUE,
                               align = "center", fill = NA, partial = TRUE)
    } else {
      # Right-aligned window (includes current and past values)
      rolled <- zoo::rollapply(zoo::zoo(y), width = window, FUN = mean, na.rm = TRUE,
                               align = "right", fill = NA, partial = TRUE)
    }

    # Fill only the originally NA positions
    y_filled <- y
    na_positions <- is.na(y)
    y_filled[na_positions] <- as.numeric(rolled)[na_positions]

    return(y_filled)
  }

  # Fallback: manual rolling mean
  y_filled <- y
  na_positions <- which(is.na(y))

  for (i in na_positions) {
    if (center) {
      # Centered window
      half_win <- floor(window / 2)
      start_idx <- max(1, i - half_win)
      end_idx <- min(length(y), i + half_win)
    } else {
      # Right-aligned window
      start_idx <- max(1, i - window + 1)
      end_idx <- i
    }

    # Compute mean of non-NA values in window (excluding current position)
    window_vals <- y[start_idx:end_idx]
    window_vals[start_idx:end_idx == i] <- NA  # Exclude current NA position
    valid_vals <- window_vals[!is.na(window_vals)]

    if (length(valid_vals) > 0) {
      y_filled[i] <- mean(valid_vals)
    }
    # If no valid values in window, leave as NA
  }

  y_filled
}


#' STL Decomposition Imputation
#'
#' Fill NAs using seasonal decomposition fitted values.
#'
#' @param y Numeric vector
#' @param dates Date vector
#' @param params List with:
#'   \itemize{
#'     \item period: Seasonal period (NULL = auto-detect, e.g., 7 for weekly, 365 for yearly)
#'     \item robust: Use robust fitting (default: TRUE)
#'     \item max_gap: Maximum gap length to fill (default: Inf)
#'   }
#' @return Filled numeric vector
#' @keywords internal
.fill_stl <- function(y, dates, params = list()) {
  period <- params$period %||% NULL
  robust <- params$robust %||% TRUE
  max_gap <- params$max_gap %||% Inf

  # Check gap lengths
  if (is.finite(max_gap)) {
    gap_lengths <- rle(is.na(y))
    na_gaps <- gap_lengths$lengths[gap_lengths$values]
    if (any(na_gaps > max_gap)) {
      stop("Gap of length ", max(na_gaps), " exceeds max_gap=", max_gap)
    }
  }

  # Auto-detect period if not provided
  if (is.null(period)) {
    # Try to infer from dates
    if (length(dates) >= 2) {
      median_diff <- median(diff(as.numeric(dates)), na.rm = TRUE)

      # Map to standard periods
      if (abs(median_diff - 1) < 0.5) {
        period <- 7  # Daily data, weekly seasonality
      } else if (abs(median_diff - 7) < 0.5) {
        period <- 52  # Weekly data, yearly seasonality
      } else if (median_diff >= 28 && median_diff <= 31) {
        period <- 12  # Monthly data, yearly seasonality
      } else {
        stop("Could not auto-detect period. Please specify params$period explicitly.")
      }
    } else {
      stop("Insufficient data to auto-detect period. Please specify params$period.")
    }
  }

  # Check if we have enough data for STL
  non_na <- sum(!is.na(y))
  if (non_na < 2 * period) {
    stop("STL requires at least 2 x period (", 2 * period, ") non-NA observations. ",
         "Only ", non_na, " available. Consider using linear or rolling_mean instead.")
  }

  # First pass: fill large internal gaps with linear interpolation
  # STL can't decompose series with NAs
  y_temp <- y
  na_positions <- which(is.na(y_temp))

  if (length(na_positions) > 0) {
    # Use linear interpolation to get initial estimates
    # This allows STL to run, then we'll replace with STL-based estimates
    y_temp_filled <- tryCatch({
      if (requireNamespace("zoo", quietly = TRUE)) {
        as.numeric(zoo::na.approx(zoo::zoo(y_temp, as.numeric(dates)), na.rm = FALSE, rule = 2))
      } else {
        # Simple fallback: use locf
        for (i in seq_along(y_temp)) {
          if (is.na(y_temp[i]) && i > 1) {
            y_temp[i] <- y_temp[i - 1]
          }
        }
        y_temp
      }
    }, error = function(e) {
      stop("Could not pre-fill NAs for STL decomposition: ", e$message)
    })
  } else {
    y_temp_filled <- y_temp
  }

  # Perform STL decomposition
  ts_obj <- tryCatch({
    stats::ts(y_temp_filled, frequency = period)
  }, error = function(e) {
    stop("Could not create ts object with period=", period, ": ", e$message)
  })

  stl_result <- tryCatch({
    stats::stl(ts_obj, s.window = "periodic", robust = robust)
  }, error = function(e) {
    stop("STL decomposition failed: ", e$message,
         ". Try adjusting period or using a simpler strategy.")
  })

  # Extract fitted values (trend + seasonal + remainder)
  fitted <- stl_result$time.series[, "seasonal"] + stl_result$time.series[, "trend"]

  # Fill only the originally NA positions with STL-based estimates
  y_filled <- y
  y_filled[na_positions] <- fitted[na_positions]

  y_filled
}


#' Cross-Series Borrowing (Grouped Data)
#'
#' Fill NAs in one series by borrowing from similar peer series.
#'
#' @param data Full data frame with all groups
#' @param target_col Character, name of target column
#' @param date_col Character, name of date column
#' @param groups_chr Character vector of group column names
#' @param params List with:
#'   \itemize{
#'     \item method: "median" (default), "mean", or "weighted"
#'     \item neighbors: NULL (all groups), character vector (for single group column),
#'                      or named list (for multi-level groups, e.g., list(region = c("North", "South")))
#'     \item max_gap: Maximum gap length to fill (default: Inf)
#'   }
#' @return Data frame with filled target column
#' @keywords internal
.fill_borrow_grouped <- function(data, target_col, date_col, groups_chr, params = list()) {
  method <- params$method %||% "median"
  neighbors <- params$neighbors %||% NULL
  max_gap <- params$max_gap %||% Inf

  if (!method %in% c("median", "mean", "weighted")) {
    stop("borrow method must be 'median', 'mean', or 'weighted'")
  }

  # Check gap lengths
  if (is.finite(max_gap)) {
    # Helper to find max NA run length, returning 0 if no NAs
    max_na_run <- function(x) {
      r <- rle(is.na(x))
      na_runs <- r$lengths[r$values]
      if (length(na_runs) == 0) return(0L)
      max(na_runs)
    }

    gap_check <- data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups_chr))) %>%
      dplyr::summarise(
        max_gap_len = max_na_run(.data[[target_col]]),
        .groups = "drop"
      )
    if (any(gap_check$max_gap_len > max_gap, na.rm = TRUE)) {
      stop("Some groups have gaps exceeding max_gap=", max_gap)
    }
  }

  # Split by group
  data_split <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups_chr))) %>%
    dplyr::group_split()

  # For each group, fill NAs by borrowing from peers
  filled_groups <- lapply(data_split, function(grp) {
    grp_key <- grp %>% dplyr::select(dplyr::all_of(groups_chr)) %>% dplyr::distinct()
    na_positions <- which(is.na(grp[[target_col]]))

    if (length(na_positions) == 0) {
      return(grp)  # No NAs, nothing to fill
    }

    # Get peer series (all other groups)
    peer_data <- data %>%
      dplyr::anti_join(grp_key, by = groups_chr)

    # Filter to specified neighbors if provided
    if (!is.null(neighbors)) {
      if (is.list(neighbors) && !is.null(names(neighbors))) {
        # Named list: filter by multiple group columns
        # e.g., neighbors = list(region = c("North", "South"), category = c("A", "B"))
        for (col_name in names(neighbors)) {
          if (col_name %in% groups_chr) {
            peer_data <- peer_data %>%
              dplyr::filter(.data[[col_name]] %in% neighbors[[col_name]])
          } else {
            warning("neighbors list contains '", col_name, "' which is not a group column. Ignoring.")
          }
        }
      } else if (is.character(neighbors)) {
        # Character vector: assume it applies to the first group column (backward compatibility)
        if (length(groups_chr) > 0) {
          peer_data <- peer_data %>%
            dplyr::filter(.data[[groups_chr[1]]] %in% neighbors)
        }
      } else {
        warning("neighbors parameter should be NULL, a character vector, or a named list. Ignoring.")
      }
    }

    if (nrow(peer_data) == 0) {
      warning("No peer series available for borrowing. Leaving NAs unchanged.")
      return(grp)
    }

    # For weighted method, compute distances to peers once and create a weight lookup
    peer_weight_lookup <- NULL
    if (method == "weighted") {
      # Compute similarity based on overlapping non-NA values
      grp_values <- grp[[target_col]]
      peer_groups <- peer_data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(groups_chr))) %>%
        dplyr::group_split()

      # Create group keys for lookup
      peer_group_keys <- sapply(peer_groups, function(pg) {
        paste(pg[1, groups_chr, drop = TRUE], collapse = "|||")
      })

      peer_distances <- sapply(peer_groups, function(pg) {
        # Align by date and compute mean absolute difference
        merged <- dplyr::inner_join(
          grp %>% dplyr::select(dplyr::all_of(c(date_col, target_col))),
          pg %>% dplyr::select(dplyr::all_of(c(date_col, target_col))),
          by = date_col,
          suffix = c("_grp", "_peer")
        )
        valid <- !is.na(merged[[paste0(target_col, "_grp")]]) & !is.na(merged[[paste0(target_col, "_peer")]])
        if (sum(valid) == 0) return(Inf)
        mean(abs(merged[[paste0(target_col, "_grp")]][valid] - merged[[paste0(target_col, "_peer")]][valid]))
      })

      # Convert distances to weights (inverse)
      finite_dists <- peer_distances[is.finite(peer_distances)]
      if (length(finite_dists) == 0) {
        # No finite distances (no overlap with any peer) - use equal weights
        peer_weights <- rep(1 / length(peer_distances), length(peer_distances))
      } else {
        # Replace Inf with large finite value (10x max finite distance)
        peer_distances[is.infinite(peer_distances)] <- max(finite_dists) * 10
        peer_weights <- 1 / (peer_distances + 1e-6)
        peer_weights <- peer_weights / sum(peer_weights)
      }

      # Create named lookup: group_key -> weight
      peer_weight_lookup <- setNames(peer_weights, peer_group_keys)
    }

    # For each NA position, borrow from peers at same date
    for (i in na_positions) {
      target_date <- grp[[date_col]][i]

      # Find peer values at this date WITH their group identity
      peer_at_date <- peer_data %>%
        dplyr::filter(.data[[date_col]] == target_date) %>%
        dplyr::filter(!is.na(.data[[target_col]]))

      if (nrow(peer_at_date) > 0) {
        peer_values <- peer_at_date[[target_col]]

        # Borrow using specified method
        if (method == "median") {
          grp[[target_col]][i] <- stats::median(peer_values)
        } else if (method == "mean") {
          grp[[target_col]][i] <- mean(peer_values)
        } else if (method == "weighted") {
          # Get weights for the peers that have values at this date
          peer_keys <- apply(peer_at_date[, groups_chr, drop = FALSE], 1, function(row) {
            paste(row, collapse = "|||")
          })

          # Look up weights for these specific peers
          matched_weights <- peer_weight_lookup[peer_keys]

          if (all(!is.na(matched_weights)) && length(matched_weights) > 0) {
            # Re-normalize weights for the subset of peers present
            matched_weights <- matched_weights / sum(matched_weights)
            grp[[target_col]][i] <- sum(peer_values * matched_weights)
          } else {
            # Fallback to median if weights can't be matched
            grp[[target_col]][i] <- stats::median(peer_values)
          }
        }
      }
      # If no peer values available, leave as NA
    }

    grp
  })

  dplyr::bind_rows(filled_groups)
}


#' Custom User Function
#'
#' Apply user-provided filling function.
#'
#' @param y Numeric vector
#' @param dates Date vector
#' @param params List with:
#'   \itemize{
#'     \item fn: User function with signature fn(y, dates, params)
#'   }
#' @return Filled numeric vector
#' @keywords internal
.fill_custom <- function(y, dates, params = list()) {
  if (is.null(params$fn) || !is.function(params$fn)) {
    stop("custom strategy requires params$fn to be a function with signature: fn(y, dates, params)")
  }

  result <- params$fn(y = y, dates = dates, params = params)

  # Validate result
  if (!is.list(result) || !all(c("values", "is_imputed") %in% names(result))) {
    stop("Custom fill function must return list(values = ..., is_imputed = ...)")
  }

  if (length(result$values) != length(y)) {
    stop("Custom fill function returned ", length(result$values), " values, expected ", length(y))
  }

  # Return full list so is_imputed flag is propagated
  result
}

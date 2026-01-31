#' Build Future Date Grid
#'
#' Generates future dates for forecasting, handling both single-series and grouped data.
#' For grouped data, creates a cross-product of groups x future dates.
#'
#' @param history Data frame with historical data
#' @param h Integer, forecast horizon
#' @param date_col Character, name of date column
#' @param groups_chr Character vector of group column names (NULL for ungrouped)
#' @param frequency Frequency object for date generation
#'
#' @return Tibble with future dates (and group keys if grouped)
#' @keywords internal
.build_future_grid <- function(history, h, date_col, groups_chr = NULL, frequency = NULL) {

  if (!is.null(groups_chr) && length(groups_chr) > 0) {
    # Grouped: get last date per group
    steps <- history %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups_chr))) %>%
      dplyr::summarise(
        start = tail(.data[[date_col]][!is.na(.data[[date_col]])], 1),
        .groups = "drop"
      )

    # Generate future dates for each group
    future_list <- lapply(seq_len(nrow(steps)), function(i) {
      row_data <- steps[i, ]
      start_date <- row_data$start

      # Check for all-NA dates in this group
      if (length(start_date) == 0 || is.na(start_date)) {
        grp_desc <- paste(vapply(groups_chr, function(g) paste0(g, "=", row_data[[g]]), character(1)), collapse = ", ")
        stop("Cannot generate future dates: group (", grp_desc, ") has no valid (non-NA) dates.", call. = FALSE)
      }

      # Filter historical data for this group
      grp_filter <- rep(TRUE, nrow(history))
      for (g in groups_chr) {
        grp_filter <- grp_filter & (history[[g]] == row_data[[g]])
      }
      d_hist <- history[[date_col]][grp_filter]

      # Generate future dates
      future_dates <- .generate_future_dates_stable(start_date, h, frequency, d_hist)

      # Create result data frame
      result <- row_data[rep(1, h), groups_chr, drop = FALSE]
      result[[date_col]] <- future_dates
      result
    })

    dplyr::bind_rows(future_list)

  } else {
    # Ungrouped: single series
    d <- history[[date_col]]
    d <- d[!is.na(d)]
    last_date <- tail(d, 1)
    tibble::tibble(!!rlang::sym(date_col) := .generate_future_dates_stable(last_date, h, frequency, d))
  }
}

#' Prepare Single Prediction Row
#'
#' Assembles features for a single forecast step by combining target-based features
#' (lags, MAs, rolling stats) with calendar and exogenous variable features.
#'
#' @param y_hist Numeric vector of historical target values
#' @param next_date Date for this forecast step (used for calendar/xreg lookup)
#' @param target_col Character, name of target column
#' @param p Integer, number of lags (NULL if none)
#' @param q Integer vector of MA windows (NULL if none)
#' @param roll_windows Integer vector of rolling stat windows (NULL if none)
#' @param roll_stats Character vector of rolling statistics to compute
#' @param trend_windows Integer vector for trend slopes (NULL if none)
#' @param trend_degrees Integer vector for polynomial trends (NULL if none)
#' @param CAL_row Single-row tibble of calendar features for this date (NULL if none)
#' @param XF_row Single-row tibble of xreg features for this date (NULL if none)
#' @param groups_chr Character vector of group column names (NULL if ungrouped)
#' @param date_col Character, name of date column
#'
#' @return Single-row tibble with all features
#' @keywords internal
.prepare_feature_row <- function(y_hist, next_date, target_col,
                                  p = NULL, q = NULL,
                                  roll_windows = NULL, roll_stats = c("sum","sd","min","max"),
                                  trend_windows = NULL, trend_degrees = NULL,
                                  CAL_row = NULL, XF_row = NULL,
                                  groups_chr = NULL, date_col = "date") {

  # Target-based features (lags, MAs, rolling stats, trends)
  feats_y <- .make_target_feats(y_hist, target_col, p, q,
                                 roll_windows, roll_stats,
                                 trend_windows, trend_degrees)
  new_row <- if (length(feats_y) == 0) {
    tibble::tibble(.rows = 1)
  } else {
    tidyr::as_tibble(feats_y)
  }

  # Calendar features
  if (!is.null(CAL_row) && nrow(CAL_row) > 0) {
    exclude_cols <- c(groups_chr, date_col)
    cal_cols <- setdiff(names(CAL_row), exclude_cols)
    if (length(cal_cols) > 0) {
      new_row <- dplyr::bind_cols(new_row, CAL_row %>% dplyr::select(dplyr::all_of(cal_cols)))
    }
  }

  # Exogenous variable features
  if (!is.null(XF_row) && nrow(XF_row) > 0) {
    exclude_cols <- c(groups_chr, date_col)
    xr_cols <- setdiff(names(XF_row), exclude_cols)
    if (length(xr_cols) > 0) {
      new_row <- dplyr::bind_cols(new_row, XF_row %>% dplyr::select(dplyr::all_of(xr_cols)))
    }
  }

  new_row
}

#' Check for Trailing NA Targets
#'
#' Validates that the target column does not end with NA values after sorting by date.
#' Trailing NAs will cause lags and other target-based features to produce NA values,
#' which leads to NA forecasts.
#'
#' @param data Data frame with history, already sorted by groups and date
#' @param target_col Character, name of target column
#' @param groups_chr Character vector of group column names (NULL for ungrouped)
#'
#' @return NULL (invisibly). Stops with error if trailing NAs found.
#' @keywords internal
.check_trailing_na <- function(data, target_col, groups_chr = NULL) {
  if (!is.null(groups_chr) && length(groups_chr) > 0) {
    # Grouped data: check each group
    data %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(groups_chr))) %>%
      dplyr::group_map(function(grp, key) {
        vals <- grp[[target_col]]
        if (length(vals) > 0 && is.na(vals[length(vals)])) {
          grp_str <- paste(names(key), "=", key, collapse = ", ")
          stop("Target column '", target_col, "' has trailing NA for group [", grp_str, "]. ",
               "This will produce NA forecasts. ",
               "Consider using TimeSeries() with target_na parameter to fill gaps before fitting.")
        }
        NULL
      })
  } else {
    # Ungrouped data: check single series
    vals <- data[[target_col]]
    if (length(vals) > 0 && is.na(vals[length(vals)])) {
      stop("Target column '", target_col, "' has trailing NA. ",
           "This will produce NA forecasts. ",
           "Consider using TimeSeries() with target_na parameter to fill gaps before fitting.")
    }
  }
  invisible(NULL)
}

#' Apply Schema and Predictor Selection
#'
#' Harmonizes a prediction row to match the training schema and selects
#' only the predictors needed by the model.
#'
#' @param new_row Single-row tibble with raw features
#' @param schema Named list of column types from training (NULL to skip)
#' @param predictors Character vector of predictor names (NULL to skip)
#'
#' @return Single-row tibble, harmonized and filtered
#' @keywords internal
.apply_schema <- function(new_row, schema = NULL, predictors = NULL) {
  # Schema harmonization (type coercion, add missing columns)
  if (!is.null(schema) && length(schema)) {
    new_row <- .harmonize_to_schema(new_row, schema)
  }

  # Predictor selection
  if (!is.null(predictors) && length(predictors)) {
    miss <- setdiff(predictors, names(new_row))
    if (length(miss)) {
      for (m in miss) new_row[[m]] <- NA
    }
    new_row <- new_row %>% dplyr::select(dplyr::all_of(predictors))
  }

  new_row
}

# Internal helper for recursive forecasting - not exported
# @importFrom dplyr arrange across all_of group_by summarise group_split select distinct inner_join pull filter bind_cols bind_rows rename %>% count rowwise reframe
#' @importFrom tidyr as_tibble unnest
# @importFrom rlang as_name ensym sym !! .data
# @importFrom stats predict median as.formula model.matrix
recursive_forecast_dt <- function(model_obj,
                                  model_spec,
                                  history,
                                  future_xreg = NULL,
                                  date, target, groups = NULL,
                                  p = NULL, q = NULL,
                                  roll_windows = NULL, roll_stats = c("sum","sd","min","max"),
                                  trend_windows = NULL,
                                  trend_degrees = NULL,
                                  xreg = NULL, xreg_lags = NULL, xreg_ma = NULL,
                                  cal_dow = TRUE, cal_month = TRUE, cal_woy = FALSE, cal_eom = TRUE, cal_dom = FALSE,
                                  cal_hod = FALSE, cal_moh = FALSE, holidays = NULL,
                                  h = NULL, xreg_strategy = c("error","carry","zeros","NA"),
                                  schema = NULL, predictors = NULL,
                                  return_index = FALSE,
                                  frequency = NULL,
                                  copy = TRUE) {

  stopifnot(inherits(history, "data.frame"))
  xreg_strategy <- match.arg(xreg_strategy)

  date_col   <- if (is.character(date))   date   else rlang::as_name(rlang::ensym(date))
  target_col <- if (is.character(target)) target else rlang::as_name(rlang::ensym(target))
  groups_chr <- if (is.null(groups)) NULL else groups

  H <- if (copy) tidyr::as_tibble(history) else history
  H <- H %>% arrange(across(all_of(c(groups_chr, date_col))))

  # what features are needed?
  need_calendar <- isTRUE(cal_dow) || isTRUE(cal_month) || isTRUE(cal_woy) || isTRUE(cal_eom) || isTRUE(cal_dom) ||
                   isTRUE(cal_hod) || isTRUE(cal_moh) || !is.null(holidays)
  xr_cols <- unique(c(xreg, names(xreg_lags), names(xreg_ma)))
  xr_cols <- xr_cols[!is.na(xr_cols)]
  have_xregs <- length(xr_cols) > 0

  # horizon
  if (is.null(h)) {
    if (!is.null(future_xreg)) {
      Ftmp <- tidyr::as_tibble(future_xreg) %>% arrange(across(all_of(c(groups_chr, date_col))))
      n_per_grp <- Ftmp %>% count(across(all_of(groups_chr))) %>% pull(n)
      if (length(unique(n_per_grp)) != 1L) stop("future_xreg must have identical horizon per group or specify h explicitly.")
      h <- unique(n_per_grp)
    } else {
      stop("Provide `h` when future_xreg is NULL.")
    }
  }

  # Build future base grid
  if (!is.null(future_xreg)) {
    F_base <- tidyr::as_tibble(future_xreg) %>%
      select(all_of(c(groups_chr, date_col))) %>%
      arrange(across(all_of(c(groups_chr, date_col))))
  } else {
    F_base <- .build_future_grid(H, h, date_col, groups_chr, frequency)
  }

  # calendar features on that same grid
  F_cal <- if (need_calendar) {
    .add_calendar_feats(F_base, date_col, cal_dow, cal_month, cal_woy, cal_eom, cal_dom, cal_hod, cal_moh, holidays)
  } else F_base

  # xreg lags/MAs
  if (have_xregs) {
    F <- if (!is.null(future_xreg)) tidyr::as_tibble(future_xreg) else F_base
    F <- F %>% arrange(across(all_of(c(groups_chr, date_col))))

    HX   <- H %>% select(all_of(c(groups_chr, date_col, xr_cols)))
    FX   <- F %>% select(all_of(c(groups_chr, date_col, xr_cols)))
    both <- bind_rows(HX, FX) %>% arrange(across(all_of(c(groups_chr, date_col))))

    both_feat <- feat_lag_ma_dt(
      df        = both,
      date      = date_col,
      target    = xr_cols[1],
      p         = NULL, q = NULL,
      groups    = groups_chr,
      xreg      = xr_cols,
      xreg_lags = xreg_lags,
      xreg_ma   = xreg_ma
    )
    xreg_feat_cols <- setdiff(names(both_feat), c(groups_chr, date_col))
    XF <- both_feat %>%
      inner_join(FX, by = c(groups_chr, date_col)) %>%
      select(all_of(c(groups_chr, date_col, xreg_feat_cols)))
  } else {
    XF <- NULL
  }

  # branch: date-free path (no calendar, no xregs)
  if (!need_calendar && !have_xregs) {
    out <- H %>%
      group_by(across(all_of(groups_chr))) %>%
      summarise(
        result = {
          y_hist <- dplyr::pick(dplyr::all_of(target_col))[[1]]
          preds  <- numeric(h)

          for (t in seq_len(h)) {
            feats_y <- .make_target_feats(y_hist, target_col, p, q, roll_windows, roll_stats, trend_windows, trend_degrees)
            new_row <- if (length(feats_y) == 0) tibble::tibble(.rows = 1) else tidyr::as_tibble(feats_y)

            if (!is.null(schema) && length(schema)) new_row <- .harmonize_to_schema(new_row, schema)
            if (!is.null(predictors) && length(predictors)) {
              miss <- setdiff(predictors, names(new_row))
              if (length(miss)) for (m in miss) new_row[[m]] <- NA
              new_row <- new_row %>% select(all_of(predictors))
            }

            # Pass raw data frame to model's predict function
            # Models handle their own encoding (one-hot, native categorical, etc.)
            pred <- tryCatch(as.numeric(model_spec$predict(model_obj, newdata = new_row)),
                             error = function(e) NA_real_)
            if (length(pred) != 1 || !is.finite(pred)) pred <- NA_real_
            preds[t] <- pred
            y_hist <- c(y_hist, pred)
          }

          if (isTRUE(return_index)) {
            list(tibble::tibble(step = seq_len(h), forecast = preds))
          } else {
            # Generate proper future dates
            d <- dplyr::pick(dplyr::all_of(date_col))[[1]]
            d <- d[!is.na(d)]
            last_d <- tail(d, 1)
            future_dates <- .generate_future_dates_stable(last_d, h, frequency, d)
            list(tibble::tibble(!!rlang::sym(date_col) := future_dates, forecast = preds))
          }
        },
        .groups = "drop"
      ) %>%
      unnest(result)

    out <- out %>% rename(!!paste0(target_col, "_forecast") := forecast)
    return(out)
  }

  # Unified forecasting path (handles both grouped and ungrouped)
  # For ungrouped data, treat as single pseudo-group
  groups_to_iterate <- if (!is.null(groups_chr) && length(groups_chr)) {
    H %>%
      group_by(across(all_of(groups_chr))) %>%
      group_split()
  } else {
    list(H)  # Single "group" for ungrouped data
  }

  out <- lapply(groups_to_iterate, function(grp_data) {
    # Extract group-specific data
    y_hist <- grp_data[[target_col]]

    # Group key (empty tibble for ungrouped)
    by_key <- if (!is.null(groups_chr) && length(groups_chr)) {
      grp_data %>%
        select(all_of(groups_chr)) %>%
        distinct()
    } else {
      tibble::tibble(.rows = 1)
    }

    # Extract dates, calendar, and xreg for this group
    if (!is.null(groups_chr) && length(groups_chr)) {
      dates_grp <- F_base %>%
        inner_join(by_key, by = groups_chr) %>%
        pull(!!rlang::sym(date_col))

      CAL_grp <- F_cal %>%
        inner_join(by_key, by = groups_chr) %>%
        arrange(!!rlang::sym(date_col))

      XF_grp <- if (!is.null(XF)) {
        XF %>%
          inner_join(by_key, by = groups_chr) %>%
          arrange(!!rlang::sym(date_col))
      } else {
        NULL
      }
    } else {
      dates_grp <- F_base[[date_col]]
      CAL_grp <- F_cal
      XF_grp <- XF
    }

    preds <- numeric(length(dates_grp))

    # Recursive forecasting loop
    for (t in seq_along(dates_grp)) {
      next_d <- dates_grp[t]

      # Prepare calendar and xreg rows for this step
      cal_row <- if (need_calendar && nrow(CAL_grp) > 0) CAL_grp[t, ] else NULL
      xr_row <- if (!is.null(XF_grp) && nrow(XF_grp) > 0) {
        xr_match <- XF_grp %>% filter(rlang::.data[[date_col]] == next_d)
        if (nrow(xr_match) == 1L) {
          xr_match
        } else {
          # Missing xreg: create row with type-aware NAs
          exclude_cols <- if (!is.null(groups_chr)) c(groups_chr, date_col) else date_col
          xr_template <- XF_grp[1, , drop = FALSE]
          for (mc in setdiff(names(xr_template), exclude_cols)) {
            xr_template[[mc]] <- .typed_na(xr_template[[mc]])
          }
          xr_template
        }
      } else {
        NULL
      }

      # Assemble features using helper
      new_row <- .prepare_feature_row(
        y_hist, next_d, target_col,
        p, q, roll_windows, roll_stats,
        trend_windows, trend_degrees,
        cal_row, xr_row,
        groups_chr, date_col
      )

      # Apply schema and predictor selection
      new_row <- .apply_schema(new_row, schema, predictors)

      # Predict
      pred <- tryCatch(as.numeric(model_spec$predict(model_obj, newdata = new_row)),
                       error = function(e) NA_real_)
      if (length(pred) != 1 || !is.finite(pred)) pred <- NA_real_
      preds[t] <- pred
      y_hist <- c(y_hist, pred)
    }

    # Build output for this group
    if (isTRUE(return_index)) {
      result <- tibble::tibble(step = seq_len(length(dates_grp)), forecast = preds)
    } else {
      result <- tibble::tibble(!!rlang::sym(date_col) := dates_grp, forecast = preds)
    }

    # Add group keys if grouped
    if (!is.null(groups_chr) && length(groups_chr) && nrow(by_key) > 0) {
      bind_cols(by_key, result)
    } else {
      result
    }
  }) %>%
    bind_rows()

  out <- out %>% rename(!!paste0(target_col, "_forecast") := forecast)
  out
}

#' @importFrom generics forecast
#' @export
generics::forecast

#' Generate Recursive Multi-Step Forecasts
#'
#' Produce forecasts from a fitted tsfeature_fit model using recursive prediction.
#' Each forecast step feeds back into the model as a lag feature for subsequent steps.
#'
#' @param object A \code{tsfeature_fit} object created by \code{fit()}
#' @param h Integer, forecast horizon (number of steps ahead). Required if \code{future} is NULL
#' @param future Optional data frame containing future dates and exogenous variables.
#'   Must include date column, group columns, and any raw exogenous variables used in the model
#' @param xreg_strategy Strategy for handling missing exogenous variables when \code{future} is NULL:
#'   \itemize{
#'     \item "carry" - Carry forward last observed values
#'     \item "zeros" - Fill with zeros
#'     \item "NA" - Fill with NA
#'     \item "error" - Throw error if exogenous variables needed
#'   }
#' @param return_index Logical, if TRUE return forecast steps (1, 2, ..., h) instead of dates
#' @param use_cpp Logical, if TRUE use C++ accelerated forecasting when possible (default: TRUE)
#' @param verbose Logical, if TRUE print informational messages about which forecasting path is used (default: FALSE)
#' @param ... Additional arguments (currently unused, for S3 method compatibility)
#'
#' @return Data frame with columns:
#'   \itemize{
#'     \item Group columns (if specified in fit)
#'     \item Date column or step index
#'     \item \code{{target}_forecast} - Point forecasts
#'   }
#'
#' @details
#' The forecast process:
#' \enumerate{
#'   \item For each group, compute features from history
#'   \item Predict one step ahead
#'   \item Append prediction to history
#'   \item Repeat for h steps
#' }
#'
#' If \code{future} is not provided, future dates are generated based on the frequency
#' stored in the model (from TimeSeries object) or using the median time difference
#' in the historical data within each group (fallback).
#'
#' @section Edge Cases During Forecasting:
#'
#' **Incomplete Windows for Rolling Statistics:**
#'
#' During recursive forecasting, when the requested window size exceeds available
#' history (actual observations + previous predictions), rolling statistics and
#' moving averages **return NA** to match training behavior.
#'
#' Example at forecast step 3 with rollsum(7):
#' \itemize{
#'   \item Available history: 3 predicted values
#'   \item Requested window: 7
#'   \item Result: NA (incomplete window)
#' }
#'
#' This ensures consistency between training and forecasting:
#' \itemize{
#'   \item Training uses \code{slider::slide_dbl(..., .complete = TRUE)} which returns NA for incomplete windows
#'   \item Forecasting mirrors this by returning NA when \code{length(history) < window}
#'   \item Prevents train/test feature distribution mismatch
#'   \item Models see the same NA pattern during training and forecasting
#' }
#'
#' The same NA-on-incomplete behavior applies to: rollsd(), rollmin(), rollmax(), rollslope(), and moving averages.
#'
#' **Unknown Factor Levels:**
#'
#' If \code{future} contains factor variables with levels not observed during training
#' (e.g., a new category), those values are silently converted to NA. This can happen with
#' calendar features if training data doesn't cover all days/months, or with categorical
#' exogenous variables.
#'
#' **Trend Features:**
#'
#' Trend features (trend(1), trend(2), etc.) continue incrementing beyond the training
#' range. For example, if trained on 100 observations, forecast step 1 will have trend=101.
#'
#'
#' @importFrom dplyr group_by slice_tail left_join %>% select all_of arrange across summarise group_split distinct inner_join pull filter bind_cols bind_rows rename count rowwise reframe
#' @importFrom tidyr as_tibble unnest
#' @importFrom stats median as.formula model.matrix predict
#' @importFrom rlang as_name ensym sym !! .data
#' @importFrom generics forecast
#' @examples
#' \dontrun{
#' # Fit a model
#' m <- fit(value ~ p(12) + q(7, 28) + month(),
#'          data = retail, date = "date", groups = "items", model = lm)
#'
#' # Generate 24-step ahead forecast
#' fc <- forecast(m, h = 24)
#'
#' # Forecast with step index instead of dates
#' fc <- forecast(m, h = 24, return_index = TRUE)
#'
#' # Provide future exogenous variables
#' future_data <- expand.grid(
#'   date = seq(as.Date("2010-01-01"), by = "month", length.out = 12),
#'   items = unique(retail$items),
#'   price = 9.99,
#'   promotion = 0
#' )
#' fc <- forecast(m, future = future_data)
#' }
#' @export
forecast.tsfeature_fit <- function(object,
                                   h = NULL,
                                   future = NULL,
                                   xreg_strategy = c("carry","zeros","NA","error"),
                                   return_index = FALSE,
                                   use_cpp = TRUE,
                                   verbose = FALSE,
                                   ...) {
  stopifnot(inherits(object, "tsfeature_fit"))
  xreg_strategy <- match.arg(xreg_strategy)

  spec      <- object$spec
  date_col  <- object$meta$date
  if (is.null(date_col) || !is.character(date_col) || length(date_col) != 1L || !nzchar(date_col)) {
    stop("`object$meta$date` must be a non-empty character scalar naming the date column.")
  }
  groups    <- .get_or_empty(object$meta$groups, character())
  frequency <- .get_or_empty(object$meta$frequency, NULL)
  gcols     <- groups
  target    <- spec$target

  if (is.null(object$history_raw)) {
    stop("This model lacks `history_raw`. Re-fit with the updated `fit()` that stores history_raw.")
  }

  # ---------- helpers ----------
  .fill_na_like <- function(x) {
    if (is.factor(x)) return(factor(NA, levels = levels(x)))
    if (inherits(x, "Date")) return(as.Date(NA))
    if (inherits(x, "POSIXt")) return(as.POSIXct(NA))
    if (is.integer(x)) return(NA_integer_)
    if (is.numeric(x)) return(NA_real_)
    if (is.logical(x)) return(NA)
    if (is.character(x)) return(NA_character_)
    return(NA)
  }
  .fill_zero_like <- function(x, vname) {
    if (is.factor(x)) stop("zeros strategy not supported for factor xreg: ", vname)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) stop("zeros strategy not supported for date/time xreg: ", vname)
    if (is.integer(x)) return(0L)
    if (is.numeric(x)) return(0)
    if (is.logical(x)) return(FALSE)
    if (is.character(x)) return("")
    stop("zeros strategy not supported for type of ", vname)
  }
  .assert_non_na_dates <- function(d, column) {
    d2 <- d[!is.na(d)]
    if (!length(d2)) stop("No non-NA dates found in history for column `", column, "`.")
    tail(d2, 1)
  }
  .compute_roll_spec <- function(rolls, trend) {
    rolls <- .get_or_empty(rolls, list(sum = integer(), sd = integer(), min = integer(), max = integer(), slope = integer()))
    roll_windows  <- unique(as.integer(c(rolls$sum, rolls$sd, rolls$min, rolls$max)))
    if (length(roll_windows) && anyNA(roll_windows)) roll_windows <- roll_windows[!is.na(roll_windows)]
    if (length(roll_windows) && any(roll_windows <= 0L)) stop("roll windows must be positive integers")
    roll_stats <- character()
    if (length(rolls$sum) > 0L) roll_stats <- c(roll_stats, "sum")
    if (length(rolls$sd)  > 0L) roll_stats <- c(roll_stats, "sd")
    if (length(rolls$min) > 0L) roll_stats <- c(roll_stats, "min")
    if (length(rolls$max) > 0L) roll_stats <- c(roll_stats, "max")
    trend_windows <- .get_or_empty(rolls$slope, integer())
    trend_degrees <- .get_or_empty(trend, integer())
    list(roll_windows = roll_windows, roll_stats = roll_stats,
         trend_windows = as.integer(trend_windows), trend_degrees = as.integer(trend_degrees))
  }
  .target_feature_names <- function(target, p_val, q_val, roll_windows, roll_stats, trend_windows, trend_degrees) {
    nm <- character()
    # p_val is now a vector of specific lag indices (e.g., c(1, 4, 6, 12))
    if (!is.null(p_val) && length(p_val) > 0L) for (L in p_val) nm <- c(nm, paste0(target, "_lag_", L))
    if (!is.null(q_val) && length(q_val) > 0L) for (w in q_val) nm <- c(nm, paste0(target, "_ma_", w))
    if (!is.null(roll_windows) && length(roll_windows) > 0L) {
      for (w in roll_windows) {
        if (length(roll_stats)) {
          if ("sum" %in% roll_stats) nm <- c(nm, paste0(target, "_rollsum_", w))
          if ("sd"  %in% roll_stats) nm <- c(nm, paste0(target, "_rollsd_",  w))
          if ("min" %in% roll_stats) nm <- c(nm, paste0(target, "_rollmin_", w))
          if ("max" %in% roll_stats) nm <- c(nm, paste0(target, "_rollmax_", w))
        }
      }
    }
    if (!is.null(trend_windows) && length(trend_windows) > 0L) for (w in trend_windows) nm <- c(nm, paste0(target, "_rollslope_", w))
    if (!is.null(trend_degrees) && length(trend_degrees) > 0L) for (d in trend_degrees) nm <- c(nm, paste0("trend", d))
    nm
  }
  .make_predict_wrapper <- function(object, target_feat_names, calendar_list, h, schema = NULL) {
    model_obj  <- object$model_obj
    model_spec <- object$model_spec
    predictors <- .get_or_empty(object$predictors, character())
    step <- 0L
    calls <- 0L
    force(model_obj); force(model_spec); force(predictors); force(calendar_list); force(h); force(schema)

    function(feature_mat) {
      calls <<- calls + 1L
      # target-based features (minimal base-R conversions to avoid tidyverse overhead)
      if (is.null(feature_mat)) {
        n_rows <- 1L
        target_df <- data.frame(matrix(nrow = n_rows, ncol = 0))
      } else {
        target_df <- as.data.frame(feature_mat, stringsAsFactors = FALSE)
        n_rows <- nrow(target_df)
        if (n_rows == 0) {
          n_rows <- 1L
          target_df <- data.frame(matrix(nrow = n_rows, ncol = 0))
        }
      }
      if (length(target_feat_names) > 0 && ncol(target_df) == length(target_feat_names)) {
        names(target_df) <- target_feat_names
      }
      combined_df <- target_df

      # calendar features for this step
      if (!is.null(calendar_list)) {
        idx_start <- step + 1L
        idx_end <- step + n_rows
        if (idx_end > length(calendar_list)) {
          stop(sprintf("Calendar list index out of bounds: step %d, rows %d, have %d rows (h=%d)",
                       idx_start, n_rows, length(calendar_list), h))
        }
        cal_entries <- calendar_list[idx_start:idx_end]
        cal_df <- if (length(cal_entries) == 1L) {
          cal_entries[[1L]]
        } else {
          Reduce(function(a, b) {
            if (is.null(a) || ncol(a) == 0) return(b)
            if (is.null(b) || ncol(b) == 0) return(a)
            rbind(a, b)
          }, cal_entries)
        }
        if (!is.null(cal_df) && ncol(cal_df) > 0) {
          rownames(cal_df) <- NULL
          if (!nrow(combined_df)) {
            combined_df <- data.frame(matrix(nrow = n_rows, ncol = 0))
          }
          combined_df <- cbind(combined_df, cal_df)
        }
      }

      # order to predictors and drop extras
      if (length(predictors) > 0) {
        miss <- setdiff(predictors, names(combined_df))
        if (length(miss)) {
          for (m in miss) combined_df[[m]] <- NA
        }
        extra <- setdiff(names(combined_df), predictors)
        if (length(extra)) warning("Extra predictors provided to model and will be dropped: ", paste(extra, collapse = ", "))
        combined_df <- combined_df[, predictors, drop = FALSE]
      }

      # Harmonize factor levels to match training schema (prevents "new factor levels" errors)
      if (!is.null(schema) && length(schema) > 0) {
        combined_df <- .harmonize_to_schema(combined_df, schema)
      }

      pred <- tryCatch({
        model_spec$predict(model_obj, newdata = combined_df)
      }, error = function(e) {
        stop(sprintf("Prediction failed at step %d: %s\nColumns: %s\nStructure:\n%s",
                     step + 1L, e$message, paste(colnames(combined_df), collapse = ", "),
                     paste(capture.output(str(combined_df)), collapse = "\n")))
      })

      pred_num <- as.numeric(pred)
      if (length(pred_num) == 0) stop(sprintf("Prediction returned empty vector at step %d", step))
      step <<- step + n_rows
      pred_num
    }
  }

  # ---------- load & validate history ----------
  H <- tidyr::as_tibble(object$history_raw)
  if (!(date_col %in% names(H))) stop(sprintf("Date column '%s' not found in `history_raw`.", date_col))
  missing_g <- setdiff(gcols, names(H))
  if (length(missing_g)) stop(sprintf("Group column(s) missing: %s", paste(missing_g, collapse = ", ")))
  H <- H %>% dplyr::arrange(dplyr::across(dplyr::all_of(c(gcols, date_col))))

  # Check for trailing NAs in target (after sorting)
  .check_trailing_na(H, target, gcols)

  # ---------- shared roll / trend spec ----------
  p_val <- spec$p
  q_val <- if (length(spec$q) > 0) as.integer(spec$q) else NULL
  roll_info <- .compute_roll_spec(spec$rolls, spec$trend)
  roll_windows  <- roll_info$roll_windows
  roll_stats    <- roll_info$roll_stats
  trend_windows <- roll_info$trend_windows
  trend_degrees <- roll_info$trend_degrees

  target_dep <- FALSE
  # p_val is now a vector of lag indices - check if any lags are specified
  if (!target_dep && !is.null(p_val) && length(p_val) > 0 && all(!is.na(p_val)) && all(p_val > 0)) target_dep <- TRUE
  if (!target_dep && length(q_val) > 0) target_dep <- TRUE
  if (!target_dep && length(roll_windows) > 0) target_dep <- TRUE
  if (!target_dep && length(trend_windows) > 0) target_dep <- TRUE
  batch_size_cpp <- if (target_dep) 1L else -1L

  # ---------- attempt fast C++ path (now supports return_index) ----------
  has_calendar <- any(unlist(spec$cal))
  # Check for any exogenous predictors: xreg (lag/ma sources), xreg_lags, xreg_ma, or raw_vars
  # Raw vars (e.g., value ~ price) require the R path since C++ doesn't handle them
  has_xreg <- length(spec$xreg) > 0 || length(spec$xreg_lags) > 0 ||
              length(spec$xreg_ma) > 0 || length(spec$raw_vars) > 0

  if (use_cpp && is.null(future) && !is.null(h) && !has_xreg) {
    if (verbose) {
      message("Using C++ accelerated forecasting (iterative", if (has_calendar) " with calendar features" else "", ")")
    }

    # Build history tibble and validate last date
    d_hist_all <- H[[date_col]]
    last_date <- .assert_non_na_dates(d_hist_all, date_col)

    # Pre-compute calendar features for h steps if needed
    calendar_list <- NULL
    if (has_calendar) {
      future_dates <- .generate_future_dates_stable(last_date, h, frequency, d_hist_all)
      F_base <- tibble::tibble(!!rlang::sym(date_col) := future_dates)
      F_cal  <- .add_calendar_feats(F_base, date_col,
                                    spec$cal$dow, spec$cal$month, spec$cal$woy,
                                    spec$cal$eom, spec$cal$dom,
                                    spec$cal$hod, spec$cal$moh, NULL)
      cal_cols <- setdiff(names(F_cal), date_col)
      if (length(cal_cols) > 0) {
        F_cal_subset <- F_cal[, cal_cols, drop = FALSE]
        if (nrow(F_cal_subset) != h) stop("Calendar feature generator produced ", nrow(F_cal_subset), " rows; expected ", h)
        calendar_list <- lapply(seq_len(h), function(i) F_cal_subset[i, , drop = FALSE])
      }
    }

    target_feat_names <- .target_feature_names(target, p_val, q_val, roll_windows, roll_stats, trend_windows, trend_degrees)

    if (is.null(gcols) || length(gcols) == 0) {
      # Single series
      y_hist <- H[[target]]
      predict_wrapper <- .make_predict_wrapper(object, target_feat_names, calendar_list, h, object$schema)

      forecasts_vec <- forecast_iterative_cpp(
        y_hist = y_hist,
        h = h,
        predict_fn = predict_wrapper,
        p = p_val,
        q = q_val,
        roll_windows = roll_windows,
        roll_stats = roll_stats,
        trend_windows = trend_windows,
        trend_degrees = trend_degrees,
        batch_size = batch_size_cpp
      )

      d <- H[[date_col]]
      last_date <- .assert_non_na_dates(d, date_col)
      future_dates <- .generate_future_dates_stable(last_date, h, frequency, d)

      if (isTRUE(return_index)) {
        res <- tibble::tibble(
          step = seq_len(h),
          !!rlang::sym(paste0(target, "_forecast")) := forecasts_vec
        )
      } else {
        res <- tibble::tibble(
          !!rlang::sym(date_col) := future_dates,
          !!rlang::sym(paste0(target, "_forecast")) := forecasts_vec
        )
      }
      return(res)
    } else {
      # Multi-group
      H <- H %>% dplyr::arrange(dplyr::across(dplyr::all_of(c(gcols, date_col))))
      group_data <- H %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
        dplyr::summarise(
          y_hist = list(.data[[target]]),
          last_date = tail(.data[[date_col]][!is.na(.data[[date_col]])], 1),
          .groups = "drop"
        )

      result_list <- vector("list", nrow(group_data))
      for (i in seq_len(nrow(group_data))) {
        y_vec  <- group_data$y_hist[[i]]
        last_d <- group_data$last_date[i]

        # Historical dates for this group
        grp_filter <- rep(TRUE, nrow(H))
        for (g in gcols) grp_filter <- grp_filter & (H[[g]] == group_data[[g]][i])
        d_hist <- H[[date_col]][grp_filter]

        # Future dates for this group
        future_dates <- .generate_future_dates_stable(last_d, h, frequency, d_hist)

        # Calendar features for THIS group
        calendar_list_g <- NULL
        if (has_calendar) {
          F_base_grp <- tibble::tibble(!!rlang::sym(date_col) := future_dates)
          F_cal_grp  <- .add_calendar_feats(F_base_grp, date_col,
                                            spec$cal$dow, spec$cal$month, spec$cal$woy,
                                            spec$cal$eom, spec$cal$dom,
                                            spec$cal$hod, spec$cal$moh, NULL)
          cal_cols <- setdiff(names(F_cal_grp), date_col)
          if (length(cal_cols) > 0) {
            F_cal_subset <- F_cal_grp[, cal_cols, drop = FALSE]
            if (nrow(F_cal_subset) != h) stop("Calendar feature generator produced ", nrow(F_cal_subset), " rows; expected ", h, " (group ", i, ")")
            calendar_list_g <- lapply(seq_len(h), function(j) F_cal_subset[j, , drop = FALSE])
          }
        }

        predict_wrapper <- .make_predict_wrapper(object, target_feat_names, calendar_list_g, h, object$schema)

        forecasts_vec <- forecast_iterative_cpp(
          y_hist = y_vec,
          h = h,
          predict_fn = predict_wrapper,
          p = p_val,
          q = q_val,
          roll_windows = roll_windows,
          roll_stats = roll_stats,
          trend_windows = trend_windows,
          trend_degrees = trend_degrees,
          batch_size = batch_size_cpp
        )

        grp_info <- group_data[i, gcols, drop = FALSE]
        grp_info <- grp_info[rep(1, h), , drop = FALSE]

        if (isTRUE(return_index)) {
          out <- dplyr::bind_cols(
            grp_info,
            tibble::tibble(
              step = seq_len(h),
              !!rlang::sym(paste0(target, "_forecast")) := forecasts_vec
            )
          )
        } else {
          out <- dplyr::bind_cols(
            grp_info,
            tibble::tibble(
              !!rlang::sym(date_col) := future_dates,
              !!rlang::sym(paste0(target, "_forecast")) := forecasts_vec
            )
          )
        }
        result_list[[i]] <- out
      }
      return(dplyr::bind_rows(result_list))
    }
  }

  # ---------- general path (xregs and/or future provided) ----------
  if (is.null(future)) {
    if (is.null(h) || !is.numeric(h) || length(h) != 1L || h < 1 || h != as.integer(h)) {
      stop("Provide a positive integer `h` when `future` is NULL.")
    }
  }

  raw_vars       <- .get_or_empty(spec$raw_vars, character())
  xreg_base      <- .get_or_empty(spec$xreg, character())
  need_base_vars <- unique(c(raw_vars, xreg_base))

  # treat raw vars as lag-0 xregs
  xreg_aug      <- unique(c(xreg_base, raw_vars))
  xreg_lags_aug <- .get_or_empty(spec$xreg_lags, list())
  for (rv in raw_vars) xreg_lags_aug[[rv]] <- sort(unique(c(0L, .get_or_empty(xreg_lags_aug[[rv]], integer()))))
  xreg_ma_aug <- .get_or_empty(spec$xreg_ma, list())

  # Build/validate future_xreg
  future_xreg <- NULL
  if (!is.null(future)) {
    future_xreg <- tidyr::as_tibble(future)
    need_cols <- unique(c(date_col, gcols, need_base_vars))
    miss <- setdiff(need_cols, names(future_xreg))
    if (length(miss)) {
      stop("`future` is missing columns: ", paste(miss, collapse = ", "),
           ". It must contain: ", paste(need_cols, collapse = ", "), ".")
    }
    future_xreg <- future_xreg %>% dplyr::arrange(dplyr::across(dplyr::all_of(c(gcols, date_col))))
  } else {
    # Generate future dates using stable frequency-based approach
    if (length(gcols)) {
      steps <- H %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
        dplyr::summarise(
          start = tail(.data[[date_col]][!is.na(.data[[date_col]])], 1),
          .groups = "drop"
        )

      future_list <- lapply(seq_len(nrow(steps)), function(i) {
        row_data <- steps[i, ]
        start_date <- row_data$start

        grp_filter <- rep(TRUE, nrow(H))
        for (g in gcols) grp_filter <- grp_filter & (H[[g]] == row_data[[g]])
        d_hist <- H[[date_col]][grp_filter]

        future_dates <- .generate_future_dates_stable(start_date, h, frequency, d_hist)

        result <- row_data[rep(1, h), gcols, drop = FALSE]
        result[[date_col]] <- future_dates
        result
      })

      future_xreg <- dplyr::bind_rows(future_list)
    } else {
      d <- H[[date_col]]
      last_date <- .assert_non_na_dates(d, date_col)
      future_xreg <- tibble::tibble(!!rlang::sym(date_col) := .generate_future_dates_stable(last_date, h, frequency, d))
    }

    if (length(need_base_vars)) {
      if (xreg_strategy == "carry") {
        # Carry the last NON-NA value per column (not just the last row)
        # This handles trailing NAs after fill_time without xreg gap-filling
        if (length(gcols)) {
          # Grouped: get last non-NA per variable per group
          last_vals <- H %>%
            dplyr::select(dplyr::all_of(c(gcols, need_base_vars))) %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
            dplyr::summarise(
              dplyr::across(
                dplyr::all_of(need_base_vars),
                ~ {
                  non_na <- .x[!is.na(.x)]
                  if (length(non_na) == 0) NA else utils::tail(non_na, 1)
                }
              ),
              .groups = "drop"
            )
          # Check for columns that are entirely NA in some groups
          for (v in need_base_vars) {
            na_groups <- last_vals[[v]]
            if (any(is.na(na_groups))) {
              warning("xreg_strategy='carry': column '", v, "' has no non-NA values in some groups. ",
                      "Forecasts for those groups will have NA features.", call. = FALSE)
            }
          }
          # Check if we're carrying from further back (last row was NA)
          last_row <- H %>%
            dplyr::group_by(dplyr::across(dplyr::all_of(gcols))) %>%
            dplyr::slice_tail(n = 1) %>%
            dplyr::ungroup()
          for (v in need_base_vars) {
            if (any(is.na(last_row[[v]]) & !is.na(last_vals[[v]]))) {
              warning("xreg_strategy='carry': column '", v, "' has trailing NA(s); ",
                      "carrying last non-NA value from earlier in history.", call. = FALSE)
            }
          }
          future_xreg <- future_xreg %>% dplyr::left_join(last_vals, by = gcols)
        } else {
          # Ungrouped: get last non-NA per variable
          for (v in need_base_vars) {
            non_na <- H[[v]][!is.na(H[[v]])]
            if (length(non_na) == 0) {
              warning("xreg_strategy='carry': column '", v, "' has no non-NA values. ",
                      "Forecasts will have NA features.", call. = FALSE)
              future_xreg[[v]] <- NA
            } else {
              carried_val <- utils::tail(non_na, 1)
              last_val <- utils::tail(H[[v]], 1)
              if (is.na(last_val)) {
                warning("xreg_strategy='carry': column '", v, "' has trailing NA; ",
                        "carrying last non-NA value from earlier in history.", call. = FALSE)
              }
              future_xreg[[v]] <- carried_val
            }
          }
        }
      } else if (xreg_strategy == "zeros") {
        for (v in need_base_vars) future_xreg[[v]] <- .fill_zero_like(H[[v]], v)
      } else if (xreg_strategy == "NA") {
        for (v in need_base_vars) future_xreg[[v]] <- .fill_na_like(H[[v]])
      } else {
        stop("Exogenous variables required but `future` not provided. Missing: ", paste(need_base_vars, collapse = ", "))
      }
    }
  }

  FC <- recursive_forecast_dt(
    model_obj     = object$model_obj,
    model_spec    = object$model_spec,
    history       = H,
    future_xreg   = future_xreg,
    date          = date_col,
    target        = target,
    groups        = if (length(gcols)) gcols else NULL,
    p             = p_val,
    q             = q_val,
    roll_windows  = roll_windows,
    roll_stats    = roll_stats,
    trend_windows = trend_windows,
    trend_degrees = trend_degrees,
    xreg          = xreg_aug,
    xreg_lags     = xreg_lags_aug,
    xreg_ma       = xreg_ma_aug,
    cal_dow       = spec$cal$dow,
    cal_month     = spec$cal$month,
    cal_woy       = spec$cal$woy,
    cal_eom       = spec$cal$eom,
    cal_dom       = spec$cal$dom,
    h             = h,
    xreg_strategy = xreg_strategy,
    schema        = object$schema,
    predictors    = object$predictors,
    return_index  = return_index,
    frequency     = frequency,
    copy          = TRUE
  )
  FC
}

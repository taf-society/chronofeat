#' Time Series Cross-Validation
#'
#' Perform time series cross-validation using expanding or sliding windows.
#' Evaluates forecast accuracy across multiple time periods to assess model
#' performance and tune hyperparameters.
#'
#' @param formula A formula specifying the target and features (same as \code{fit()})
#' @param data A data frame or TimeSeries object containing the time series data
#' @param date Character string naming the date column (Date or POSIXct class)
#' @param groups Character vector naming grouping columns for panel data
#' @param model Model specification (same as \code{fit()})
#' @param h Forecast horizon (number of periods ahead to forecast)
#' @param n_windows Number of cross-validation folds/windows
#' @param window_type Type of training window: "expanding" (default) or "sliding"
#' @param window_size Size of training window for sliding window (ignored for expanding)
#' @param step_size Number of periods to step forward between folds (default: h)
#' @param metric Evaluation metric: "rmse", "mae", "mape", or a custom function
#' @param return_predictions Logical, whether to return individual predictions (default: FALSE)
#' @param ... Additional arguments passed to the model's fit function
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{metrics} - Data frame with metrics per fold and overall
#'     \item \code{predictions} - Data frame with predictions (if return_predictions = TRUE)
#'     \item \code{params} - List of CV parameters used
#'   }
#'
#' @details
#' The function performs time series cross-validation by:
#' \enumerate{
#'   \item Splitting data into multiple train/test windows
#'   \item Fitting the model on each training window
#'   \item Generating h-step ahead forecasts for each test window
#'   \item Computing specified metrics for each fold
#' }
#'
#' Expanding window: Training set grows with each fold (recommended for most cases)
#' Sliding window: Training set has fixed size, slides forward
#'
#' @export
#' @importFrom dplyr filter arrange group_by summarise n %>% select all_of bind_rows mutate inner_join rename distinct
#' @importFrom stats median
#' @examples
#' \dontrun{
#' # Basic CV with expanding window
#' cv_results <- cv_forecast(
#'   value ~ p(12) + month(),
#'   data = retail,
#'   date = "date",
#'   groups = "items",
#'   model = lm,
#'   h = 6,
#'   n_windows = 5
#' )
#' print(cv_results$metrics)
#'
#' # CV with custom model specification
#' custom_model <- list(
#'   fit = function(y, X, ...) {
#'     train_df <- cbind(data.frame(.response = y), X)
#'     lm(.response ~ ., data = train_df)
#'   },
#'   predict = function(object, newdata, ...) {
#'     stats::predict(object, newdata = newdata)
#'   }
#' )
#' cv_results <- cv_forecast(
#'   value ~ p(12) + trend() + rollsum(7, 28),
#'   data = retail,
#'   date = "date",
#'   groups = "items",
#'   model = custom_model,
#'   h = 12,
#'   n_windows = 5,
#'   metric = "rmse"
#' )
#'
#' # Sliding window CV
#' cv_results <- cv_forecast(
#'   value ~ p(6) + month(),
#'   data = retail,
#'   date = "date",
#'   groups = "items",
#'   model = lm,
#'   h = 3,
#'   n_windows = 10,
#'   window_type = "sliding",
#'   window_size = 120
#' )
#' }
cv_forecast <- function(formula,
                        data,
                        date,
                        groups = NULL,
                        model,
                        h = 1,
                        n_windows = 5,
                        window_type = c("expanding", "sliding"),
                        window_size = NULL,
                        step_size = NULL,
                        metric = "rmse",
                        return_predictions = FALSE,
                        ...) {

  window_type <- match.arg(window_type)

  # Prepare data
  DF <- if (inherits(data, "TimeSeries")) data$data else data
  date_col <- if (inherits(data, "TimeSeries")) (if (is.null(date)) data$date else date) else date
  groups_chr <- if (inherits(data, "TimeSeries")) (if (is.null(groups)) data$groups else groups) else groups
  frequency <- if (inherits(data, "TimeSeries")) data$frequency else NULL

  stopifnot(is.data.frame(DF))
  stopifnot(is.character(date_col), length(date_col) == 1, date_col %in% names(DF))
  if (!is.null(groups_chr)) stopifnot(is.character(groups_chr))
  stopifnot(inherits(DF[[date_col]], "Date") || inherits(DF[[date_col]], "POSIXct"))

  # Get target column from formula
  target_col <- all.vars(formula[[2]])[1]
  stopifnot(target_col %in% names(DF))

  # Sort data
  DF <- DF %>% arrange(across(all_of(c(groups_chr, date_col))))

  # Default step size is horizon
  if (is.null(step_size)) step_size <- h

  # Create time series splits
  splits <- .create_ts_splits(
    df = DF,
    date_col = date_col,
    groups = groups_chr,
    h = h,
    n_windows = n_windows,
    window_type = window_type,
    window_size = window_size,
    step_size = step_size
  )

  if (length(splits) == 0) {
    stop("Unable to create CV splits. Check that data has sufficient length.")
  }

  # Metric function
  metric_fn <- .get_metric_function(metric)

  # Perform CV
  results <- list()
  predictions <- list()

  for (i in seq_along(splits)) {
    split <- splits[[i]]

    # Fit model on training data
    # Preserve frequency if available by creating TimeSeries object
    train_data <- if (!is.null(frequency)) {
      TimeSeries(
        data = split$train,
        date = date_col,
        groups = groups_chr,
        frequency = frequency,
        auto_detect = FALSE
      )
    } else {
      split$train
    }

    m <- tryCatch({
      fit(formula = formula,
          data = train_data,
          date = date_col,
          groups = groups_chr,
          model = model,
          ...)
    }, error = function(e) {
      warning(sprintf("Fold %d: Model fitting failed - %s", i, e$message))
      return(NULL)
    })

    if (is.null(m)) next

    # Prepare future data from test set
    # Include xreg columns (raw_vars from spec) so CV matches production forecasts
    spec <- m$spec
    raw_vars <- .get_or_empty(spec$raw_vars, character())
    xreg_base <- .get_or_empty(spec$xreg, character())
    needed_xreg_cols <- unique(c(raw_vars, xreg_base))

    # Select date, groups, and any raw xreg columns that exist in test set
    future_cols <- c(date_col, groups_chr, needed_xreg_cols)
    future_cols <- intersect(future_cols, names(split$test))

    future_dates <- split$test %>%
      select(all_of(future_cols)) %>%
      distinct()

    # Generate forecast
    fc <- tryCatch({
      forecast(m, future = future_dates)
    }, error = function(e) {
      warning(sprintf("Fold %d: Forecasting failed - %s", i, e$message))
      return(NULL)
    })

    if (is.null(fc)) next

    # Merge with actuals
    fc_col <- names(fc)[!names(fc) %in% c(date_col, groups_chr)]

    join_by_cols <- if (!is.null(groups_chr)) c(date_col, groups_chr) else date_col

    eval_data <- split$test %>%
      select(all_of(c(date_col, groups_chr, target_col))) %>%
      inner_join(
        fc %>% select(all_of(c(date_col, groups_chr, fc_col))),
        by = join_by_cols
      )

    if (nrow(eval_data) == 0) {
      warning(sprintf("Fold %d: No matching forecast dates found", i))
      next
    }

    # Calculate metrics
    metric_value <- metric_fn(eval_data[[target_col]], eval_data[[fc_col]])

    results[[i]] <- data.frame(
      fold = i,
      train_start = split$train_start,
      train_end = split$train_end,
      test_start = split$test_start,
      test_end = split$test_end,
      n_train = nrow(split$train),
      n_test = nrow(eval_data),
      metric_value = metric_value,
      stringsAsFactors = FALSE
    )

    if (return_predictions) {
      predictions[[i]] <- eval_data %>%
        mutate(fold = i) %>%
        rename(actual = all_of(target_col), predicted = all_of(fc_col))
    }
  }

  # Combine results
  metrics_df <- bind_rows(results)

  if (nrow(metrics_df) == 0) {
    stop("All CV folds failed. Check data, formula, and model specification.")
  }

  # Compute overall metric weighted by fold test sizes

  # Different metrics require different aggregation:
  # - RMSE: sqrt(weighted.mean(rmse^2, n_test)) because it's root-mean-square
  # - MSE/MAE/MAPE: weighted.mean(metric, n_test)
  # - Custom: weighted.mean (best default)
  fold_metrics <- metrics_df$metric_value
  fold_weights <- metrics_df$n_test
  valid <- !is.na(fold_metrics) & !is.na(fold_weights) & fold_weights > 0

  if (sum(valid) == 0) {
    overall_metric <- NA_real_
  } else if (is.character(metric) && metric == "rmse") {
    # RMSE: aggregate as sqrt of weighted mean of squared values
    overall_metric <- sqrt(stats::weighted.mean(fold_metrics[valid]^2, fold_weights[valid]))
  } else {
    # MAE, MAPE, MSE, custom: weighted mean by test size
    overall_metric <- stats::weighted.mean(fold_metrics[valid], fold_weights[valid])
  }

  # Add overall metrics
  # Use same date class as input (Date or POSIXct)
  na_date <- if (inherits(DF[[date_col]], "POSIXct")) as.POSIXct(NA) else as.Date(NA)
  overall <- data.frame(
    fold = 0L,
    train_start = na_date,
    train_end = na_date,
    test_start = na_date,
    test_end = na_date,
    n_train = NA_integer_,
    n_test = sum(metrics_df$n_test),
    metric_value = overall_metric,
    stringsAsFactors = FALSE
  )

  metrics_df <- bind_rows(metrics_df, overall)
  names(metrics_df)[names(metrics_df) == "metric_value"] <- if (is.character(metric)) metric else "custom_metric"

  # Prepare output
  output <- list(
    metrics = metrics_df,
    params = list(
      h = h,
      n_windows = n_windows,
      window_type = window_type,
      window_size = window_size,
      step_size = step_size,
      metric = metric,
      n_folds_completed = nrow(metrics_df) - 1
    )
  )

  if (return_predictions) {
    output$predictions <- bind_rows(predictions)
  }

  structure(output, class = "cv_forecast")
}

# Helper: Create time series splits
# @importFrom dplyr filter group_by summarise %>% pull
.create_ts_splits <- function(df, date_col, groups, h, n_windows,
                               window_type, window_size, step_size) {
  splits <- list()

  if (is.null(groups)) {
    # Single time series
    dates <- sort(unique(df[[date_col]]))
    n_dates <- length(dates)

    # Minimum training size
    min_train <- if (window_type == "sliding" && !is.null(window_size)) window_size else 10

    # Calculate split points
    test_ends <- seq(n_dates - h * (n_windows - 1), n_dates, by = step_size)
    test_ends <- test_ends[test_ends <= n_dates]
    test_ends <- tail(test_ends, n_windows)

    for (i in seq_along(test_ends)) {
      test_end_idx <- test_ends[i]
      test_start_idx <- test_end_idx - h + 1

      if (test_start_idx < 1) next

      # Training window
      if (window_type == "expanding") {
        train_end_idx <- test_start_idx - 1
        train_start_idx <- 1
      } else {
        train_end_idx <- test_start_idx - 1
        train_start_idx <- max(1, train_end_idx - window_size + 1)
      }

      if (train_end_idx < train_start_idx || train_end_idx - train_start_idx + 1 < min_train) next

      train_dates <- dates[train_start_idx:train_end_idx]
      test_dates <- dates[test_start_idx:test_end_idx]

      splits[[length(splits) + 1]] <- list(
        train = df %>% filter(.data[[date_col]] %in% train_dates),
        test = df %>% filter(.data[[date_col]] %in% test_dates),
        train_start = min(train_dates),
        train_end = max(train_dates),
        test_start = min(test_dates),
        test_end = max(test_dates)
      )
    }
  } else {
    # Panel data - validate each group has same dates
    dates_per_group <- df %>%
      group_by(across(all_of(groups))) %>%
      summarise(
        dates = list(sort(unique(.data[[date_col]]))),
        n_dates = length(unique(.data[[date_col]])),
        .groups = "drop"
      )

    # Validate all groups have identical date sequences
    if (nrow(dates_per_group) > 1) {
      first_dates <- dates_per_group$dates[[1]]
      first_n <- dates_per_group$n_dates[1]

      for (i in 2:nrow(dates_per_group)) {
        curr_dates <- dates_per_group$dates[[i]]
        curr_n <- dates_per_group$n_dates[i]

        # Check if date sequences are identical
        if (!identical(curr_dates, first_dates)) {
          # Build group identifier string
          grp_id <- paste(sapply(groups, function(g) {
            as.character(dates_per_group[[g]][i])
          }), collapse = "/")

          first_grp_id <- paste(sapply(groups, function(g) {
            as.character(dates_per_group[[g]][1])
          }), collapse = "/")

          stop(sprintf(
            paste0(
              "Cross-validation requires all groups to have identical date sequences.\n",
              "  Group '%s' has %d dates (from %s to %s)\n",
              "  Group '%s' has %d dates (from %s to %s)\n",
              "Suggestions:\n",
              "  1. Fill missing dates with NA or interpolated values\n",
              "  2. Filter data to common date range across all groups\n",
              "  3. Use per-group validation instead of panel CV"
            ),
            grp_id, curr_n, min(curr_dates), max(curr_dates),
            first_grp_id, first_n, min(first_dates), max(first_dates)
          ), call. = FALSE)
        }
      }
    }

    # All groups validated - use first group's dates as template
    dates <- dates_per_group$dates[[1]]
    n_dates <- length(dates)

    min_train <- if (window_type == "sliding" && !is.null(window_size)) window_size else 10

    test_ends <- seq(n_dates - h * (n_windows - 1), n_dates, by = step_size)
    test_ends <- test_ends[test_ends <= n_dates]
    test_ends <- tail(test_ends, n_windows)

    for (i in seq_along(test_ends)) {
      test_end_idx <- test_ends[i]
      test_start_idx <- test_end_idx - h + 1

      if (test_start_idx < 1) next

      if (window_type == "expanding") {
        train_end_idx <- test_start_idx - 1
        train_start_idx <- 1
      } else {
        train_end_idx <- test_start_idx - 1
        train_start_idx <- max(1, train_end_idx - window_size + 1)
      }

      if (train_end_idx < train_start_idx || train_end_idx - train_start_idx + 1 < min_train) next

      train_dates <- dates[train_start_idx:train_end_idx]
      test_dates <- dates[test_start_idx:test_end_idx]

      splits[[length(splits) + 1]] <- list(
        train = df %>% filter(.data[[date_col]] %in% train_dates),
        test = df %>% filter(.data[[date_col]] %in% test_dates),
        train_start = min(train_dates),
        train_end = max(train_dates),
        test_start = min(test_dates),
        test_end = max(test_dates)
      )
    }
  }

  splits
}

# Helper: Get metric function
.get_metric_function <- function(metric) {
  if (is.function(metric)) {
    return(metric)
  }

  if (!is.character(metric) || length(metric) != 1) {
    stop("metric must be a character string or a function")
  }

  switch(tolower(metric),
    rmse = function(actual, predicted) {
      sqrt(mean((actual - predicted)^2, na.rm = TRUE))
    },
    mae = function(actual, predicted) {
      mean(abs(actual - predicted), na.rm = TRUE)
    },
    mape = function(actual, predicted) {
      mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
    },
    mse = function(actual, predicted) {
      mean((actual - predicted)^2, na.rm = TRUE)
    },
    stop(sprintf("Unknown metric: '%s'. Use 'rmse', 'mae', 'mape', 'mse', or provide a custom function.", metric))
  )
}

#' Print method for cv_forecast
#' @param x A cv_forecast object
#' @param ... Additional arguments (ignored)
#' @export
print.cv_forecast <- function(x, ...) {
  cat("Time Series Cross-Validation Results\n")
  cat("=====================================\n\n")

  cat("Parameters:\n")
  cat(sprintf("  Horizon (h): %d\n", x$params$h))
  cat(sprintf("  Number of windows: %d\n", x$params$n_windows))
  cat(sprintf("  Completed folds: %d\n", x$params$n_folds_completed))
  cat(sprintf("  Window type: %s\n", x$params$window_type))
  if (!is.null(x$params$window_size)) {
    cat(sprintf("  Window size: %d\n", x$params$window_size))
  }
  cat(sprintf("  Step size: %d\n", x$params$step_size))
  cat(sprintf("  Metric: %s\n\n", if (is.character(x$params$metric)) x$params$metric else "custom"))

  cat("Metrics by fold:\n")
  print(x$metrics, row.names = FALSE)

  invisible(x)
}

#' Summary method for cv_forecast
#' @param object A cv_forecast object
#' @param ... Additional arguments (ignored)
#' @export
summary.cv_forecast <- function(object, ...) {
  metric_col <- names(object$metrics)[8]
  # fold is numeric: 0L = overall, 1L/2L/... = individual folds
  fold_metrics <- object$metrics[object$metrics$fold != 0L, metric_col]

  cat("Time Series Cross-Validation Summary\n")
  cat("====================================\n\n")

  cat(sprintf("Metric: %s\n", metric_col))
  cat(sprintf("  Mean:   %.4f\n", mean(fold_metrics, na.rm = TRUE)))
  cat(sprintf("  Median: %.4f\n", stats::median(fold_metrics, na.rm = TRUE)))
  cat(sprintf("  SD:     %.4f\n", stats::sd(fold_metrics, na.rm = TRUE)))
  cat(sprintf("  Min:    %.4f\n", min(fold_metrics, na.rm = TRUE)))
  cat(sprintf("  Max:    %.4f\n", max(fold_metrics, na.rm = TRUE)))

  invisible(object)
}
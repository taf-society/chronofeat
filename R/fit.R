#' Convert Model Function to Model Specification
#'
#' Converts a model function (e.g., `lm`, `glm`) to a model specification list
#' with `fit` and `predict` functions compatible with the chronofeat API.
#'
#' @param model_fn A model function that accepts a formula and data argument
#' @param ... Additional arguments passed to the model function during fitting
#'
#' @return A list with `fit` and `predict` functions:
#'   \itemize{
#'     \item \code{fit(y, X, ...)} - Fits the model with response y and predictors X
#'     \item \code{predict(object, newdata, ...)} - Generates predictions from fitted model
#'   }
#'
#' @details
#' This function provides backward compatibility for users who want to pass
#' model functions directly (e.g., `model = lm`) instead of model specifications.
#'
#' The wrapper creates a formula-based interface internally, combining y and X
#' into a temporary data frame and fitting the model using the standard R formula
#' interface.
#'
#' Supported model functions include any that accept the standard formula/data
#' interface: `lm`, `glm`, `randomForest::randomForest`, `ranger::ranger`, etc.
#'
#' @examples
#' \dontrun{
#' # Convert lm to model spec
#' lm_spec <- as_model_spec(lm)
#'
#' # Convert glm with family argument
#' glm_spec <- as_model_spec(glm, family = poisson())
#'
#' # Use directly in fit()
#' m <- fit(value ~ p(12), data = df, date = "date", model = lm)
#' }
#'
#' @export
as_model_spec <- function(model_fn, ...) {
  if (!is.function(model_fn)) {
    stop("model_fn must be a function (e.g., lm, glm, randomForest)")
  }

  # Capture additional arguments for the model
 extra_args <- list(...)

  list(
    fit = function(y, X, ...) {
      # Combine y and X into a data frame for formula-based fitting
      train_df <- cbind(data.frame(.response = y), X)

      # Build formula: .response ~ .
      # This uses all columns in X as predictors
      if (ncol(X) > 0) {
        fml <- stats::as.formula(".response ~ .")
      } else {
        # Intercept-only model
        fml <- stats::as.formula(".response ~ 1")
      }

      # Merge extra_args with any runtime ... args
      call_args <- c(list(formula = fml, data = train_df), extra_args, list(...))

      # Call the model function
      do.call(model_fn, call_args)
    },

    predict = function(object, newdata, ...) {
      # Standard predict interface
      # Most R models support predict(object, newdata)
      stats::predict(object, newdata = newdata, ...)
    }
  )
}

#' Fit a Time Series Forecasting Model with Formula Interface
#'
#' Train a forecasting model using a formula-based feature specification.
#' This function automatically generates time series features (lags, MAs, rolling stats,
#' calendar features) from the formula and fits the specified model.
#'
#' @param formula A formula specifying the target and features using special syntax:
#'   \itemize{
#'     \item \code{p(k)} - Create k lags of the target (e.g., p(12))
#'     \item \code{q(w1, w2, ...)} - Create moving averages with specified windows (e.g., q(7,28))
#'     \item \code{dow()}, \code{month()}, \code{woy()}, \code{eom()}, \code{dom()} - Calendar features
#'     \item \code{rollsum(w1,w2)}, \code{rollsd(w)}, \code{rollmin(w)}, \code{rollmax(w)} - Rolling statistics
#'     \item \code{rollslope(w)} - Rolling trend slopes
#'     \item \code{lag(varname, k1, k2, ...)} - Lags of exogenous variables
#'     \item \code{ma(varname, w1, w2, ...)} - MAs of exogenous variables
#'     \item Raw column names for direct inclusion
#'   }
#' @param data A data frame or TimeSeries object containing the time series data
#' @param date Character string naming the date column (Date class required)
#' @param groups Character vector naming grouping columns for panel data
#' @param model Model specification as a list with `fit` and `predict` functions, or
#'   a model function (for backward compatibility, e.g., `lm`, `glm`).
#'   \itemize{
#'     \item \code{fit(y, X, ...)} - Function that fits model with y (response) and X (predictors matrix)
#'     \item \code{predict(object, newdata, ...)} - Function that predicts from fitted model
#'   }
#' @param ... Additional arguments passed to the model$fit function
#'
#' @return A `tsfeature_fit` object containing:
#'   \itemize{
#'     \item \code{model} - The fitted model object
#'     \item \code{data} - Training data with features
#'     \item \code{history_raw} - Raw historical data for forecasting
#'     \item \code{formula} - Expanded formula used for modeling
#'     \item \code{spec} - Feature specifications
#'     \item \code{meta} - Metadata (date, groups)
#'     \item \code{predictors} - Predictor column names
#'     \item \code{schema} - Predictor schema for type consistency
#'   }
#'
#' @details
#' The function automatically:
#' \enumerate{
#'   \item Parses the formula to identify feature specifications
#'   \item Generates all requested features within groups
#'   \item Removes rows with NA values in features (due to lagging/rolling)
#'   \item Fits the model on the feature-engineered data
#'   \item Stores necessary metadata for forecasting
#' }
#'
#' @section Edge Cases and Important Behaviors:
#'
#' **Data Sorting:**
#' If using a TimeSeries object, data is automatically sorted by groups and date.
#' This ensures correct lag and difference calculations. If passing a plain data frame,
#' ensure it is pre-sorted by groups (if any) and date.
#'
#' **Minimum Data Requirements:**
#' \itemize{
#'   \item Lags: If history length < lag number, the lag feature will be NA
#'   \item Moving averages: If history length < window size, returns NA
#'   \item Rolling statistics: Computed on available data (see below)
#'   \item Frequency detection: Requires at least 2 date observations per group
#' }
#'
#' **Factor Variables:**
#' Factor levels observed during training are stored in the model schema. During
#' forecasting, if new factor levels appear that weren't seen in training, they
#' will be silently converted to NA. Consider this when using categorical features
#' like day-of-week or month.
#'
#' @export
#' @importFrom dplyr select all_of filter if_all %>%
#' @importFrom stats lm as.formula
#' @examples
#' \dontrun{
#' # Load retail data
#' load("data/retail.rda")
#'
#' # Simple model with 12 lags using linear regression
#' m1 <- fit(value ~ p(12), data = retail,
#'           date = "date", groups = "items", model = lm)
#'
#' # Model with lags, MAs, and calendar features
#' m2 <- fit(value ~ p(12) + q(7, 28) + month() + dow(),
#'           data = retail, date = "date", groups = "items", model = lm)
#'
#' # GLM for count data
#' m3 <- fit(count ~ p(7) + dow(), data = count_data,
#'           date = "date", groups = "store",
#'           model = glm, family = poisson())
#'
#' # Custom model specification
#' custom_model <- list(
#'   fit = function(y, X, ...) {
#'     # Your custom fitting logic
#'     train_df <- cbind(data.frame(.response = y), X)
#'     lm(.response ~ ., data = train_df, ...)
#'   },
#'   predict = function(object, newdata, ...) {
#'     stats::predict(object, newdata = newdata, ...)
#'   }
#' )
#' m4 <- fit(value ~ p(12), data = retail,
#'           date = "date", groups = "items", model = custom_model)
#' }
fit <- function(formula, data, date = NULL, groups = NULL, model, ...) {
  DF        <- if (inherits(data, "TimeSeries")) data$data else data
  date      <- if (inherits(data, "TimeSeries")) (if (is.null(date)) data$date else date) else date
  groups    <- if (inherits(data, "TimeSeries")) (if (is.null(groups)) data$groups else groups) else groups
  frequency <- if (inherits(data, "TimeSeries")) data$frequency else NULL
  datetime_type <- if (inherits(data, "TimeSeries")) data$datetime_type else {
    # Detect from the date column
    if (inherits(DF[[date]], "POSIXct")) "POSIXct" else "Date"
  }

  stopifnot(is.data.frame(DF))
  stopifnot(is.character(date), length(date)==1, date %in% names(DF))
  if (!is.null(groups)) stopifnot(is.character(groups))

  # Handle model specification

  # Track if model came from as_model_spec to avoid double-forwarding ...

  # When model is a function, as_model_spec captures ... into extra_args,
  # so we should NOT forward ... again to model$fit (would cause duplicate args)
  model_from_function <- FALSE
  if (is.function(model)) {
    # Backward compatibility: convert function to model spec
    model <- as_model_spec(model, ...)
    model_from_function <- TRUE
  } else if (is.list(model)) {
    # Validate model specification
    if (!all(c("fit", "predict") %in% names(model))) {
      stop("Model specification must be a list with 'fit' and 'predict' functions.")
    }
    if (!is.function(model$fit) || !is.function(model$predict)) {
      stop("Model$fit and model$predict must be functions.")
    }
  } else {
    stop("Model must be a function or a list with 'fit' and 'predict' functions.")
  }

  spec <- .parse_fit_formula(formula, DF)
  target_col <- spec$target
  if (!target_col %in% names(DF)) stop(sprintf("target column '%s' not found.", target_col))
  if (!inherits(DF[[date]], "Date") && !inherits(DF[[date]], "POSIXct")) {
    stop("`date` column must be of class Date or POSIXct.")
  }

  add_calendar <- any(unlist(spec$cal))
  cal_opts <- if (add_calendar) {
    list(dow = spec$cal$dow, woy = spec$cal$woy, month = spec$cal$month,
         eom = spec$cal$eom, dom = spec$cal$dom,
         hod = spec$cal$hod, moh = spec$cal$moh)
  } else list()

  # Base: target lags/MAs + calendar + xreg lag/ma (no target rolling yet)
  result <- build_features_dt(
    data         = DF,
    date         = date,
    target       = target_col,
    groups       = groups,
    p            = spec$p,
    q            = spec$q,
    xreg         = spec$xreg,
    xreg_lags    = spec$xreg_lags,
    xreg_ma      = spec$xreg_ma,
    add_calendar = add_calendar, cal_opts = cal_opts,
    add_roll     = FALSE, roll_opts = list(),
    copy         = TRUE
  )

  # Add trend features if requested
  if (length(spec$trend)) {
    result <- feat_trend(result, date = date, groups = groups, degrees = spec$trend)
  }

  # Add requested target rolling features, per-stat to avoid extras
  if (length(spec$rolls$sum)) {
    result <- feat_rolling_dt(result, date, target_col, groups = groups,
                           windows = spec$rolls$sum, stats = "sum", trend_windows = NULL)
  }
  if (length(spec$rolls$sd)) {
    result <- feat_rolling_dt(result, date, target_col, groups = groups,
                           windows = spec$rolls$sd, stats = "sd", trend_windows = NULL)
  }
  if (length(spec$rolls$min)) {
    result <- feat_rolling_dt(result, date, target_col, groups = groups,
                           windows = spec$rolls$min, stats = "min", trend_windows = NULL)
  }
  if (length(spec$rolls$max)) {
    result <- feat_rolling_dt(result, date, target_col, groups = groups,
                           windows = spec$rolls$max, stats = "max", trend_windows = NULL)
  }
  if (length(spec$rolls$slope)) {
    result <- feat_rolling_dt(result, date, target_col, groups = groups,
                           windows = integer(), stats = character(0),
                           trend_windows = spec$rolls$slope)
  }

  base_cols <- c(target_col, date, groups %||% character())
  feat_cols <- spec$requested_cols
  keep_cols <- unique(c(base_cols, feat_cols))
  miss <- setdiff(feat_cols, names(result))
  if (length(miss)) stop("Requested feature columns not found after build: ", paste(miss, collapse=", "))

  train_data <- result %>% select(all_of(keep_cols))


  # Drop rows with NA in target column

  # This must happen before fitting to avoid crashes or corrupt training
  train_data <- train_data %>% filter(!is.na(.data[[target_col]]))

  # Drop rows with NA in feature columns (introduced by lags/MAs/rolling)
  # With the fix in .make_target_feats(), forecasting now returns NA for incomplete
  # rolling windows, matching training behavior (feat_rolling_dt uses .complete = TRUE)
  # This prevents distribution shift between training and forecasting
  if (length(feat_cols)) {
    train_data <- train_data %>% filter(if_all(all_of(feat_cols), ~!is.na(.)))
  }

  # Validate we have data left after filtering
  if (nrow(train_data) == 0) {
    stop("No valid training rows remain after removing NA values. ",
         "Check that your data has sufficient history for the requested lags/windows, ",
         "and that the target column is not entirely NA.")
  }

  # Prepare y (response) and X (predictors data frame)
  y <- train_data[[target_col]]
  predictor_names <- setdiff(colnames(train_data), c(target_col, date, groups %||% character()))

  # Extract predictors as DATA FRAME (not matrix), keeping factors intact
  if (length(predictor_names) > 0) {
    X <- train_data[, predictor_names, drop = FALSE]
  } else {
    # Intercept-only model: empty data frame
    X <- data.frame(row.names = seq_along(y))
  }

  # Fit the model using model$fit
  # Models now receive raw data frame with factors, not expanded matrix
  # Note: Don't forward ... if model came from as_model_spec (args already captured)
  fit_obj <- if (model_from_function) {
    model$fit(y = y, X = X)
  } else {
    model$fit(y = y, X = X, ...)
  }

  # Keep a compact raw history for forecasting
  raw_needed <- unique(c(
    target_col, date,
    groups %||% character(),
    spec$xreg,
    spec$raw_vars
  ))
  history_raw <- DF %>% select(all_of(raw_needed))

  # Keep predictor schema for forecasting
  predictor_schema <- lapply(predictor_names, function(v) {
    col <- train_data[[v]]
    lv <- if (is.factor(col)) levels(col) else NULL
    tz <- if (inherits(col, "POSIXt")) (attr(col, "tzone") %||% "") else NULL
    list(name = v, type = class(col)[1], levels = lv, tzone = tz)
  })

  structure(list(
    model_obj      = fit_obj,
    model_spec     = model,
    data           = train_data,
    history_raw    = history_raw,
    spec           = spec,
    meta           = list(date = date, groups = groups, frequency = frequency, datetime_type = datetime_type),
    predictors     = predictor_names,
    schema         = predictor_schema
  ), class = "tsfeature_fit")
}

# Helper function: build features wrapper
# @importFrom dplyr %>%
build_features_dt <- function(data, date, target, groups = NULL,
                              p = NULL, q = NULL,
                              xreg = NULL, xreg_lags = NULL, xreg_ma = NULL,
                              add_calendar = TRUE, cal_opts = list(),
                              add_roll = TRUE, roll_opts = list(),
                              copy = TRUE) {
  stopifnot(inherits(data, "data.frame"))
  result <- if (copy) data else data

  result <- feat_lag_ma_dt(result, date, target, p, q, groups, xreg, xreg_lags, xreg_ma)

  if (add_calendar) {
    result <- do.call(feat_calendar_dt, c(list(df = result, date = as.name(date)), cal_opts))
  }
  if (add_roll) {
    result <- do.call(feat_rolling_dt, c(list(df = result, date = as.name(date),
                                               target = as.name(target), groups = groups), roll_opts))
  }
  result
}

#' chronofeat: Time-Based Feature Engineering for Forecasting
#'
#' @useDynLib chronofeat, .registration = TRUE
#' @importFrom stats setNames terms coef ts stl
#' @importFrom utils capture.output head str tail
#' @importFrom dplyr coalesce anti_join
#' @importFrom tibble tibble
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# Suppress R CMD check notes for non-standard evaluation patterns
# These are used in data.table-style syntax and dplyr NSE
utils::globalVariables(c(
  ":=",          # data.table assignment operator
  "h",           # holiday variable name in join
  "holiday.x",   # join suffix from dplyr
  "holiday.y",   # join suffix from dplyr
  "result"       # used in summarise
))

#' chronofeat Package Overview
#'
#' @name chronofeat
#' @description
#' The chronofeat package provides a formula-based interface for time series
#' feature engineering and forecasting. It handles lag creation, moving averages,
#' rolling statistics, calendar features, and recursive multi-step ahead forecasting.
#'
#' @section Main Functions:
#'
#' \describe{
#'   \item{\code{\link{TimeSeries}}}{Create a TimeSeries object with explicit frequency}
#'   \item{\code{\link{fit}}}{Fit a forecasting model with automatic feature engineering}
#'   \item{\code{\link{forecast}}}{Generate recursive multi-step ahead forecasts}
#'   \item{\code{\link{cv_forecast}}}{Time series cross-validation}
#' }
#'
#' @section Key Features:
#'
#' **Automatic Feature Engineering:**
#' \itemize{
#'   \item Lags: \code{p(k)} creates k lags of the target variable
#'   \item Moving Averages: \code{q(w1, w2)} creates MAs with specified windows
#'   \item Rolling Statistics: \code{rollsum()}, \code{rollsd()}, \code{rollmin()}, \code{rollmax()}, \code{rollslope()}
#'   \item Calendar Features: \code{dow()}, \code{month()}, \code{woy()}, \code{eom()}, \code{dom()}
#'   \item Trend Features: \code{trend(1, 2, 3)} for polynomial trends
#'   \item Exogenous Variables: \code{lag(varname, k1, k2)}, \code{ma(varname, w1, w2)}
#' }
#'
#' **Recursive Forecasting:**
#'
#' Multi-step ahead forecasts are generated recursively, where predictions
#' from previous steps feed back as inputs for future steps. This is the
#' standard approach for autoregressive models.
#'
#' **Stable Date Generation:**
#'
#' Use \code{TimeSeries} objects to store frequency information explicitly,
#' ensuring stable and predictable future date generation using R's \code{seq.Date()}
#' instead of median-based inference.
#'
#' **Model Agnostic:**
#'
#' Works with any model that can be expressed as a list with \code{fit(y, X)} and
#' \code{predict(object, newdata)} functions. Simply provide your model as:
#' \preformatted{
#' model <- list(
#'   fit = function(y, X, ...) { ... },
#'   predict = function(object, newdata, ...) { ... }
#' )
#' }
#'
#' @section Important Edge Cases:
#'
#' **Incomplete Windows:**
#'
#' During forecasting, when window size exceeds available history
#' (actual + previous predictions), rolling statistics and moving averages
#' return NA to match training behavior.
#'
#' **Factor Levels:**
#'
#' Factor levels observed during training are stored in the model schema.
#' Unknown levels in forecast data are converted to NA with a warning.
#'
#' **Data Sorting:**
#'
#' TimeSeries objects automatically sort data by groups and date. When using
#' plain data frames, ensure data is pre-sorted to avoid incorrect lag calculations.
#'
#' **Minimum Data Requirements:**
#' \itemize{
#'   \item Lags: Need at least \code{p} observations per group
#'   \item Moving Averages: Need at least \code{max(q)} observations
#'   \item Frequency Detection: Need at least 2 date observations per group
#' }
#'
#' @section Getting Started:
#'
#' \preformatted{
#' library(chronofeat)
#'
#' # Define your model
#' my_model <- list(
#'   fit = function(y, X, ...) lm.fit(as.matrix(cbind(1, X)), y),
#'   predict = function(object, newdata, ...) {
#'     as.numeric(cbind(1, as.matrix(newdata)) %*% object$coefficients)
#'   }
#' )
#'
#' # Create TimeSeries with frequency
#' ts_data <- TimeSeries(data, date = "date", groups = "items", frequency = "month")
#'
#' # Fit model with automatic feature engineering
#' m <- fit(value ~ p(12) + q(7, 28) + month() + dow(),
#'          data = ts_data,
#'          model = my_model)
#'
#' # Generate 24-step ahead forecast
#' fc <- forecast(m, h = 24)
#'
#' # Cross-validation
#' cv_results <- cv_forecast(
#'   value ~ p(12) + month(),
#'   data = ts_data,
#'   model = my_model,
#'   h = 6,
#'   n_windows = 5
#' )
#' }
#'
#' @section Learn More:
#'
#' \itemize{
#'   \item \code{vignette("feature-engineering-and-forecasting")} - Comprehensive guide
#'   \item \code{vignette("edge-cases-and-best-practices")} - Edge cases and best practices
#'   \item \code{?fit} - Detailed documentation on model fitting
#'   \item \code{?forecast} - Detailed documentation on forecasting
#'   \item \code{?TimeSeries} - Information about TimeSeries objects
#' }
#'
NULL

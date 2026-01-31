# Time Series Cross-Validation

Perform time series cross-validation using expanding or sliding windows.
Evaluates forecast accuracy across multiple time periods to assess model
performance and tune hyperparameters.

## Usage

``` r
cv_forecast(
  formula,
  data,
  date = NULL,
  groups = NULL,
  model,
  h = 1,
  n_windows = 5,
  window_type = c("expanding", "sliding"),
  window_size = NULL,
  step_size = NULL,
  metric = "rmse",
  return_predictions = FALSE,
  ...
)
```

## Arguments

- formula:

  A formula specifying the target and features (same as
  [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md))

- data:

  A data frame or TimeSeries object containing the time series data

- date:

  Character string naming the date column (Date or POSIXct class).
  Optional when `data` is a TimeSeries object (uses stored date column).

- groups:

  Character vector naming grouping columns for panel data

- model:

  Model specification (same as
  [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md))

- h:

  Forecast horizon (number of periods ahead to forecast)

- n_windows:

  Number of cross-validation folds/windows

- window_type:

  Type of training window: "expanding" (default) or "sliding"

- window_size:

  Size of training window for sliding window (ignored for expanding)

- step_size:

  Number of periods to step forward between folds (default: h)

- metric:

  Evaluation metric: "rmse", "mae", "mape", or a custom function

- return_predictions:

  Logical, whether to return individual predictions (default: FALSE)

- ...:

  Additional arguments passed to the model's fit function

## Value

A list containing:

- `metrics` - Data frame with metrics per fold and overall

- `predictions` - Data frame with predictions (if return_predictions =
  TRUE)

- `params` - List of CV parameters used

## Details

The function performs time series cross-validation by:

1.  Splitting data into multiple train/test windows

2.  Fitting the model on each training window

3.  Generating h-step ahead forecasts for each test window

4.  Computing specified metrics for each fold

Expanding window: Training set grows with each fold (recommended for
most cases) Sliding window: Training set has fixed size, slides forward

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic CV with expanding window
cv_results <- cv_forecast(
  value ~ p(12) + month(),
  data = retail,
  date = "date",
  groups = "items",
  model = lm,
  h = 6,
  n_windows = 5
)
print(cv_results$metrics)

# CV with custom model specification
custom_model <- list(
  fit = function(y, X, ...) {
    train_df <- cbind(data.frame(.response = y), X)
    lm(.response ~ ., data = train_df)
  },
  predict = function(object, newdata, ...) {
    stats::predict(object, newdata = newdata)
  }
)
cv_results <- cv_forecast(
  value ~ p(12) + trend() + rollsum(7, 28),
  data = retail,
  date = "date",
  groups = "items",
  model = custom_model,
  h = 12,
  n_windows = 5,
  metric = "rmse"
)

# Sliding window CV
cv_results <- cv_forecast(
  value ~ p(6) + month(),
  data = retail,
  date = "date",
  groups = "items",
  model = lm,
  h = 3,
  n_windows = 10,
  window_type = "sliding",
  window_size = 120
)
} # }
```

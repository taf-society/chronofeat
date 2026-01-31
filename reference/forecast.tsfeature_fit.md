# Generate Recursive Multi-Step Forecasts

Produce forecasts from a fitted tsfeature_fit model using recursive
prediction. Each forecast step feeds back into the model as a lag
feature for subsequent steps.

## Usage

``` r
# S3 method for class 'tsfeature_fit'
forecast(
  object,
  h = NULL,
  future = NULL,
  xreg_strategy = c("carry", "zeros", "NA", "error"),
  return_index = FALSE,
  use_cpp = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- object:

  A `tsfeature_fit` object created by
  [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md)

- h:

  Integer, forecast horizon (number of steps ahead). Required if
  `future` is NULL

- future:

  Optional data frame containing future dates and exogenous variables.
  Must include date column, group columns, and any raw exogenous
  variables used in the model

- xreg_strategy:

  Strategy for handling missing exogenous variables when `future` is
  NULL:

  - "carry" - Carry forward last observed values

  - "zeros" - Fill with zeros

  - "NA" - Fill with NA

  - "error" - Throw error if exogenous variables needed

- return_index:

  Logical, if TRUE return forecast steps (1, 2, ..., h) instead of dates

- use_cpp:

  Logical, if TRUE use C++ accelerated forecasting when possible
  (default: TRUE)

- verbose:

  Logical, if TRUE print informational messages about which forecasting
  path is used (default: FALSE)

- ...:

  Additional arguments (currently unused, for S3 method compatibility)

## Value

Data frame with columns:

- Group columns (if specified in fit)

- Date column or step index

- `{target}_forecast` - Point forecasts

## Details

The forecast process:

1.  For each group, compute features from history

2.  Predict one step ahead

3.  Append prediction to history

4.  Repeat for h steps

If `future` is not provided, future dates are generated based on the
frequency stored in the model (from TimeSeries object) or using the
median time difference in the historical data within each group
(fallback).

## Edge Cases During Forecasting

**Incomplete Windows for Rolling Statistics:**

During recursive forecasting, when the requested window size exceeds
available history (actual observations + previous predictions), rolling
statistics and moving averages **return NA** to match training behavior.

Example at forecast step 3 with rollsum(7):

- Available history: 3 predicted values

- Requested window: 7

- Result: NA (incomplete window)

This ensures consistency between training and forecasting:

- Training uses `slider::slide_dbl(..., .complete = TRUE)` which returns
  NA for incomplete windows

- Forecasting mirrors this by returning NA when
  `length(history) < window`

- Prevents train/test feature distribution mismatch

- Models see the same NA pattern during training and forecasting

The same NA-on-incomplete behavior applies to: rollsd(), rollmin(),
rollmax(), rollslope(), and moving averages.

**Unknown Factor Levels:**

If `future` contains factor variables with levels not observed during
training (e.g., a new category), those values are silently converted to
NA. This can happen with calendar features if training data doesn't
cover all days/months, or with categorical exogenous variables.

**Trend Features:**

Trend features (trend(1), trend(2), etc.) continue incrementing beyond
the training range. For example, if trained on 100 observations,
forecast step 1 will have trend=101.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fit a model
m <- fit(value ~ p(12) + q(7, 28) + month(),
         data = retail, date = "date", groups = "items", model = lm)

# Generate 24-step ahead forecast
fc <- forecast(m, h = 24)

# Forecast with step index instead of dates
fc <- forecast(m, h = 24, return_index = TRUE)

# Provide future exogenous variables
future_data <- expand.grid(
  date = seq(as.Date("2010-01-01"), by = "month", length.out = 12),
  items = unique(retail$items),
  price = 9.99,
  promotion = 0
)
fc <- forecast(m, future = future_data)
} # }
```

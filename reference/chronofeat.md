# chronofeat Package Overview

The chronofeat package provides a formula-based interface for time
series feature engineering and forecasting. It handles lag creation,
moving averages, rolling statistics, calendar features, and recursive
multi-step ahead forecasting.

## Main Functions

- [`TimeSeries`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md):

  Create a TimeSeries object with explicit frequency

- [`fit`](https://taf-society.github.io/chronofeat/reference/fit.md):

  Fit a forecasting model with automatic feature engineering

- [`forecast`](https://generics.r-lib.org/reference/forecast.html):

  Generate recursive multi-step ahead forecasts

- [`cv_forecast`](https://taf-society.github.io/chronofeat/reference/cv_forecast.md):

  Time series cross-validation

## Key Features

**Automatic Feature Engineering:**

- Lags: `p(k)` creates k lags of the target variable

- Moving Averages: `q(w1, w2)` creates MAs with specified windows

- Rolling Statistics: `rollsum()`, `rollsd()`, `rollmin()`, `rollmax()`,
  `rollslope()`

- Calendar Features: `dow()`, `month()`, `woy()`, `eom()`, `dom()`

- Trend Features: `trend(1, 2, 3)` for polynomial trends

- Exogenous Variables: `lag(varname, k1, k2)`, `ma(varname, w1, w2)`

**Recursive Forecasting:**

Multi-step ahead forecasts are generated recursively, where predictions
from previous steps feed back as inputs for future steps. This is the
standard approach for autoregressive models.

**Stable Date Generation:**

Use `TimeSeries` objects to store frequency information explicitly,
ensuring stable and predictable future date generation using R's
[`seq.Date()`](https://rdrr.io/r/base/seq.Date.html) instead of
median-based inference.

**Model Agnostic:**

Works with any model that can be expressed as a list with `fit(y, X)`
and `predict(object, newdata)` functions. Simply provide your model as:

    model <- list(
      fit = function(y, X, ...) { ... },
      predict = function(object, newdata, ...) { ... }
    )

## Important Edge Cases

**Incomplete Windows:**

During forecasting, when window size exceeds available history (actual +
previous predictions), rolling statistics and moving averages return NA
to match training behavior.

**Factor Levels:**

Factor levels observed during training are stored in the model schema.
Unknown levels in forecast data are converted to NA with a warning.

**Data Sorting:**

TimeSeries objects automatically sort data by groups and date. When
using plain data frames, ensure data is pre-sorted to avoid incorrect
lag calculations.

**Minimum Data Requirements:**

- Lags: Need at least `p` observations per group

- Moving Averages: Need at least `max(q)` observations

- Frequency Detection: Need at least 2 date observations per group

## Getting Started

    library(chronofeat)

    # Define your model
    my_model <- list(
      fit = function(y, X, ...) lm.fit(as.matrix(cbind(1, X)), y),
      predict = function(object, newdata, ...) {
        as.numeric(cbind(1, as.matrix(newdata)) 
      }
    )

    # Create TimeSeries with frequency
    ts_data <- TimeSeries(data, date = "date", groups = "items", frequency = "month")

    # Fit model with automatic feature engineering
    m <- fit(value ~ p(12) + q(7, 28) + month() + dow(),
             data = ts_data,
             model = my_model)

    # Generate 24-step ahead forecast
    fc <- forecast(m, h = 24)

    # Cross-validation
    cv_results <- cv_forecast(
      value ~ p(12) + month(),
      data = ts_data,
      model = my_model,
      h = 6,
      n_windows = 5
    )

## Learn More

- `vignette("feature-engineering-and-forecasting")` - Comprehensive
  guide

- `vignette("edge-cases-and-best-practices")` - Edge cases and best
  practices

- [`?fit`](https://taf-society.github.io/chronofeat/reference/fit.md) -
  Detailed documentation on model fitting

- [`?forecast`](https://generics.r-lib.org/reference/forecast.html) -
  Detailed documentation on forecasting

- [`?TimeSeries`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md) -
  Information about TimeSeries objects

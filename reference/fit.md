# Fit a Time Series Forecasting Model with Formula Interface

Train a forecasting model using a formula-based feature specification.
This function automatically generates time series features (lags, MAs,
rolling stats, calendar features) from the formula and fits the
specified model.

## Usage

``` r
fit(formula, data, date = NULL, groups = NULL, model, ...)
```

## Arguments

- formula:

  A formula specifying the target and features using special syntax:

  - `p(k)` - Create k lags of the target (e.g., p(12))

  - `q(w1, w2, ...)` - Create moving averages with specified windows
    (e.g., q(7,28))

  - `dow()`, `month()`, `woy()`, `eom()`, `dom()` - Calendar features

  - `rollsum(w1,w2)`, `rollsd(w)`, `rollmin(w)`, `rollmax(w)` - Rolling
    statistics

  - `rollslope(w)` - Rolling trend slopes

  - `lag(varname, k1, k2, ...)` - Lags of exogenous variables

  - `ma(varname, w1, w2, ...)` - MAs of exogenous variables

  - Raw column names for direct inclusion

- data:

  A data frame or TimeSeries object containing the time series data

- date:

  Character string naming the date column (Date class required)

- groups:

  Character vector naming grouping columns for panel data

- model:

  Model specification as a list with `fit` and `predict` functions, or a
  model function (for backward compatibility, e.g., `lm`, `glm`).

  - `fit(y, X, ...)` - Function that fits model with y (response) and X
    (predictors matrix)

  - `predict(object, newdata, ...)` - Function that predicts from fitted
    model

- ...:

  Additional arguments passed to the model\$fit function

## Value

A `tsfeature_fit` object containing:

- `model` - The fitted model object

- `data` - Training data with features

- `history_raw` - Raw historical data for forecasting

- `formula` - Expanded formula used for modeling

- `spec` - Feature specifications

- `meta` - Metadata (date, groups)

- `predictors` - Predictor column names

- `schema` - Predictor schema for type consistency

## Details

The function automatically:

1.  Parses the formula to identify feature specifications

2.  Generates all requested features within groups

3.  Removes rows with NA values in features (due to lagging/rolling)

4.  Fits the model on the feature-engineered data

5.  Stores necessary metadata for forecasting

## Edge Cases and Important Behaviors

**Data Sorting:** If using a TimeSeries object, data is automatically
sorted by groups and date. This ensures correct lag and difference
calculations. If passing a plain data frame, ensure it is pre-sorted by
groups (if any) and date.

**Minimum Data Requirements:**

- Lags: If history length \< lag number, the lag feature will be NA

- Moving averages: If history length \< window size, returns NA

- Rolling statistics: Computed on available data (see below)

- Frequency detection: Requires at least 2 date observations per group

**Factor Variables:** Factor levels observed during training are stored
in the model schema. During forecasting, if new factor levels appear
that weren't seen in training, they will be silently converted to NA.
Consider this when using categorical features like day-of-week or month.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load retail data
load("data/retail.rda")

# Simple model with 12 lags using linear regression
m1 <- fit(value ~ p(12), data = retail,
          date = "date", groups = "items", model = lm)

# Model with lags, MAs, and calendar features
m2 <- fit(value ~ p(12) + q(7, 28) + month() + dow(),
          data = retail, date = "date", groups = "items", model = lm)

# GLM for count data
m3 <- fit(count ~ p(7) + dow(), data = count_data,
          date = "date", groups = "store",
          model = glm, family = poisson())

# Custom model specification
custom_model <- list(
  fit = function(y, X, ...) {
    # Your custom fitting logic
    train_df <- cbind(data.frame(.response = y), X)
    lm(.response ~ ., data = train_df, ...)
  },
  predict = function(object, newdata, ...) {
    stats::predict(object, newdata = newdata, ...)
  }
)
m4 <- fit(value ~ p(12), data = retail,
          date = "date", groups = "items", model = custom_model)
} # }
```

# chronofeat: Time-Based Feature Engineering for Forecasting

A flexible, formula-based interface for time series feature engineering and forecasting. Automatically generates temporal features from a concise specification. Model-agnostic: works with any R model that accepts a fit/predict interface.

## Installation

```r
# Install from GitHub (development version)
# devtools::install_github("quantics/chronofeat")

# Or install from source
devtools::install()
```

## Key Features

- **Formula-based interface**: Specify features using concise special syntax
- **Automatic feature engineering**: Lags, moving averages, rolling statistics, calendar features
- **Model-agnostic**: Works with any R model via simple fit/predict interface
- **Multi-group forecasting**: Hierarchical forecasting with automatic group handling
- **Recursive multi-step prediction**: Proper handling of lag features in forecasting

## Quick Start

```r
library(chronofeat)
library(dplyr)

# Define your model (any model with fit/predict interface)
my_model <- list(
  fit = function(y, X, ...) {
    if (ncol(X) > 0) {
      X_mat <- model.matrix(~ ., data = X)
    } else {
      X_mat <- matrix(1, nrow = length(y), ncol = 1)
      colnames(X_mat) <- "(Intercept)"
    }
    lm.fit(x = X_mat, y = y)
  },
  predict = function(object, newdata, ...) {
    if (ncol(newdata) > 0) {
      X_new <- model.matrix(~ ., data = newdata)
    } else {
      X_new <- matrix(1, nrow = nrow(newdata), ncol = 1)
      colnames(X_new) <- "(Intercept)"
    }
    as.numeric(X_new %*% object$coefficients)
  }
)

# Create TimeSeries object with frequency
ts_data <- TimeSeries(retail, date = "date", groups = "items", frequency = "month")

# Fit model with 12 lags, moving averages, and calendar features
m <- fit(value ~ p(12) + q(7, 28) + month() + dow(),
         data = ts_data,
         model = my_model)

# Generate 24-step ahead forecast
fc <- forecast(m, h = 24)

# View forecasts
head(fc)
```

## Formula Syntax

The formula interface uses special functions to specify features:

### Target-Based Features

- `p(k)` - Create lag k of target (or `p(1:k)` for lags 1 through k)
- `q(w1, w2, ...)` - Moving averages with specified windows
- `rollsum(w1, w2)` - Rolling sums
- `rollsd(w)` - Rolling standard deviations
- `rollmin(w)`, `rollmax(w)` - Rolling min/max
- `rollslope(w)` - Rolling trend slopes
- `trend(1, 2)` - Polynomial trend features

### Calendar Features

- `dow()` - Day of week
- `month()` - Month
- `woy()` - Week of year
- `eom()` - End of month indicator
- `dom()` - Day of month

### Exogenous Variables

- `lag(var, k1, k2, ...)` - Lags of exogenous variables
- `ma(var, w1, w2, ...)` - Moving averages of exogenous variables
- `varname` - Include variable as-is (no transformation)

## Examples

### Simple Autoregressive Model

```r
m1 <- fit(sales ~ p(7),
          data = ts_data,
          model = my_model)
```

### With Seasonality

```r
m2 <- fit(sales ~ p(1:12) + month() + dow(),
          data = ts_data,
          model = my_model)
```

### With Rolling Features

```r
m3 <- fit(sales ~ p(7) + rollsum(7, 28) + rollsd(7) + month(),
          data = ts_data,
          model = my_model)
```

### With Exogenous Variables

```r
m4 <- fit(sales ~ p(7) + lag(price, 0, 1, 7) + lag(promotion, 0) + month(),
          data = ts_data,
          model = my_model)
```

## Model Specification

chronofeat is model-agnostic. Simply provide a list with `fit` and `predict` functions:

```r
my_model <- list(
  fit = function(y, X, ...) {
    # y: response vector
    # X: predictor data frame (factors intact)
    # Return fitted model object
  },
  predict = function(object, newdata, ...) {
    # object: fitted model from fit()
    # newdata: data frame with predictor columns
    # Return numeric predictions
  }
)
```

### Example: XGBoost Model

```r
xgb_model <- list(
  fit = function(y, X, ...) {
    X_mat <- model.matrix(~ . - 1, data = X)
    dtrain <- xgboost::xgb.DMatrix(data = X_mat, label = y)
    xgboost::xgboost(data = dtrain, nrounds = 100, verbose = 0, ...)
  },
  predict = function(object, newdata, ...) {
    X_mat <- model.matrix(~ . - 1, data = newdata)
    predict(object, xgboost::xgb.DMatrix(data = X_mat))
  }
)

m <- fit(sales ~ p(12) + rollsum(7, 28) + month(),
         data = ts_data,
         model = xgb_model)
```

### Example: Random Forest Model

```r
rf_model <- list(
  fit = function(y, X, ...) {
    data <- cbind(y = y, X)
    randomForest::randomForest(y ~ ., data = data, ...)
  },
  predict = function(object, newdata, ...) {
    predict(object, newdata = newdata)
  }
)
```

## Forecasting

### Basic Forecast

```r
fc <- forecast(m, h = 24)
```

### With Known Future Exogenous Variables

```r
future_data <- expand.grid(
  date = seq(as.Date("2024-01-01"), by = "day", length.out = 30),
  store = unique(df$store),
  price = 9.99,
  promotion = c(rep(1, 10), rep(0, 20))
)

fc <- forecast(m, future = future_data)
```

### Handling Missing Future Exogenous Variables

```r
fc <- forecast(m, h = 30, xreg_strategy = "carry")   # Carry last value
fc <- forecast(m, h = 30, xreg_strategy = "zeros")   # Fill with zeros
fc <- forecast(m, h = 30, xreg_strategy = "NA")      # Fill with NA
```

## Cross-Validation

```r
cv_results <- cv_forecast(
  sales ~ p(12) + month(),
  data = ts_data,
  model = my_model,
  h = 6,
  n_windows = 5
)

# View metrics
cv_results$metrics
```

## Best Practices

### Choosing Lags

- **Daily data**: `p(7)` or `p(1:14)` for weekly patterns
- **Weekly data**: `p(4)` or `p(1:8)` for monthly patterns
- **Monthly data**: `p(12)` or `p(1:24)` for annual patterns

### Choosing MA Windows

- **Short-term**: `q(3, 7)` for noise smoothing
- **Medium-term**: `q(14, 28)` for trend capture
- **Long-term**: `q(90, 180)` for seasonal smoothing

### Rolling Statistics

- `rollsd()` captures volatility changes
- `rollslope()` captures trend direction
- `rollsum()` useful for cumulative effects

### Calendar Features

- Always include `month()` for monthly seasonality
- `dow()` critical for daily/weekly data
- `eom()` useful for retail (end-of-month effects)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License - see LICENSE file for details

## Citation

If you use this package in your research, please cite:

```
Akay, R. (2025). chronofeat: Time-Based Feature Engineering for Forecasting.
R package version 0.6.0.
```

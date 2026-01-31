# chronofeat

![chronofeat logo](reference/figures/logo.png)

**chronofeat** is an R package for time-based feature engineering and
forecasting. It provides a flexible, formula-based interface for
creating temporal features (lags, moving averages, rolling statistics,
calendar features) and works with any R model that has a fit/predict
interface.

## Features

- **Formula-based feature specification**: Define features using
  intuitive syntax like `value ~ p(12) + q(7) + month()`
- **Model-agnostic**: Works with lm, glm, xgboost, lightgbm,
  randomForest, or any custom model
- **Recursive multi-step forecasting**: Automatically generates features
  at each forecast step
- **Panel data support**: Handle multiple time series with proper group
  boundaries
- **C++ acceleration**: Fast recursive forecasting via cpp11

## About TAFS

**TAFS (Time Series Analysis and Forecasting Society)** is a non-profit
association (“Verein”) in Vienna, Austria. It connects academics,
experts, practitioners, and students focused on time-series,
forecasting, and decision science. Contributions remain fully open
source. Learn more at [taf-society.org](https://taf-society.org/).

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("taf-society/chronofeat")
```

## Quick Start

``` r
library(chronofeat)

# Load sample data
data(retail)

# Create TimeSeries object
ts_data <- TimeSeries(retail, date = "date", groups = "items", frequency = "month")

# Fit a model with formula-based features
model <- fit(
  value ~ p(12) + q(7, 12) + month(),
  data = ts_data,
  model = lm
)

# Generate forecasts
forecasts <- forecast(model, h = 6)
```

## Formula Syntax

| Syntax        | Description      |
|---------------|------------------|
| `p(k)`        | k lags of target |
| `p(1, 7, 12)` | Specific lags    |
| `q(w1, w2)`   | Moving averages  |
| `dow()`       | Day of week      |
| `month()`     | Month            |
| `rollsum(w)`  | Rolling sum      |
| `rollsd(w)`   | Rolling std dev  |
| `trend(d)`    | Polynomial trend |
| `lag(var, k)` | Lag of exogenous |

## Documentation

Visit the [package website](https://taf-society.github.io/chronofeat/)
for:

- [Getting
  Started](https://taf-society.github.io/chronofeat/articles/getting-started.html)
- [Building Custom
  Models](https://taf-society.github.io/chronofeat/articles/custom-models.html)
- [Feature Engineering
  Reference](https://taf-society.github.io/chronofeat/articles/feature-engineering.html)
- [Cross-Validation](https://taf-society.github.io/chronofeat/articles/cross-validation.html)

# Getting Started with chronofeat

## Overview

**chronofeat** is an R package for time series feature engineering and
forecasting. It provides:

- **Formula-based feature specification**: Define lags, moving averages,
  rolling statistics, and calendar features using a concise formula
  syntax
- **Model-agnostic design**: Works with any R model that has a
  fit/predict interface (lm, glm, xgboost, randomForest, etc.)
- **Recursive multi-step forecasting**: Automatically generates features
  at each forecast step
- **Panel data support**: Handle multiple time series (groups) in a
  single workflow

## Installation

``` r
# Install from GitHub
remotes::install_github("quantics/chronofeat")
```

``` r
library(chronofeat)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

## Quick Start

The typical chronofeat workflow has three steps:

1.  **Prepare data** with
    [`TimeSeries()`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md)
    (optional but recommended)
2.  **Fit a model** with
    [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md)
    using a feature formula
3.  **Generate forecasts** with
    [`forecast()`](https://generics.r-lib.org/reference/forecast.html)

### Step 1: Load and Prepare Data

chronofeat includes a sample retail sales dataset with 42 product
categories (items) and monthly observations:

``` r
data(retail)
head(retail)
#> # A tibble: 6 × 3
#>   date       items value
#>   <date>     <fct> <dbl>
#> 1 1982-04-01 V10    94  
#> 2 1982-05-01 V10   106. 
#> 3 1982-06-01 V10    95.1
#> 4 1982-07-01 V10    95.3
#> 5 1982-08-01 V10    82.8
#> 6 1982-09-01 V10    89.4
```

Create a `TimeSeries` object to bundle the data with frequency
information:

``` r
ts_data <- TimeSeries(
  data = retail,
  date = "date",
  groups = "items",
  frequency = "month"
)

ts_data
#> TimeSeries object
#> ----------------
#> Date column: date ( Date )
#> Frequency: month 
#> Groups: items 
#> Observations: 13986 
#> 
#> Data (first few rows):
#> # A tibble: 6 × 3
#>   date       items value
#>   <date>     <fct> <dbl>
#> 1 1982-04-01 V10    94  
#> 2 1982-05-01 V10   106. 
#> 3 1982-06-01 V10    95.1
#> 4 1982-07-01 V10    95.3
#> 5 1982-08-01 V10    82.8
#> 6 1982-09-01 V10    89.4
```

The `TimeSeries` object:

- Automatically sorts data by groups and date
- Detects or validates the time frequency
- Provides optional preprocessing (gap-filling, calendar completion)

### Step 2: Fit a Model

Use [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md)
with a formula to specify your target variable and features:

``` r
model <- fit(
  value ~ p(12) + q(7, 12) + month(),
  data = ts_data,
  model = lm
)
```

This formula creates:

- `p(12)`: 12 lags of the target (value_lag_1 through value_lag_12)
- `q(7, 12)`: 7-period and 12-period moving averages
- `month()`: Month as a factor variable

The model object contains the fitted model and all metadata needed for
forecasting:

``` r
# Check what predictors were created
model$predictors
#>  [1] "value_lag_1"  "value_lag_2"  "value_lag_3"  "value_lag_4"  "value_lag_5" 
#>  [6] "value_lag_6"  "value_lag_7"  "value_lag_8"  "value_lag_9"  "value_lag_10"
#> [11] "value_lag_11" "value_lag_12" "value_ma_7"   "value_ma_12"  "month"
```

### Step 3: Generate Forecasts

Use [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
to generate multi-step ahead predictions:

``` r
fc <- forecast(model, h = 6)
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata = newdata, ...): prediction from
#> rank-deficient fit; attr(*, "non-estim") has doubtful cases
head(fc)
#> # A tibble: 6 × 3
#>   items date       value_forecast
#>   <fct> <date>              <dbl>
#> 1 V10   2010-01-01           380.
#> 2 V10   2010-02-01           357.
#> 3 V10   2010-03-01           344.
#> 4 V10   2010-04-01           364.
#> 5 V10   2010-05-01           378.
#> 6 V10   2010-06-01           409.
```

The forecast is recursive: each predicted value becomes history for
generating features at the next step.

### Visualizing Results

``` r
library(ggplot2)

# Get one item for plotting
item_a <- retail %>%
  filter(items == levels(items)[1]) %>%
  tail(36)

fc_a <- fc %>%
  filter(items == levels(retail$items)[1])

ggplot() +
  geom_line(data = item_a, aes(x = date, y = value), color = "black") +
  geom_line(data = fc_a, aes(x = date, y = value_forecast), color = "blue") +
  geom_point(data = fc_a, aes(x = date, y = value_forecast), color = "blue") +
  labs(title = "Actual vs Forecast", x = "Date", y = "Value") +
  theme_minimal()
```

## Understanding the Formula Syntax

The formula’s left side specifies the target variable. The right side
specifies features:

| Syntax         | Description      | Example Output                               |
|----------------|------------------|----------------------------------------------|
| `p(k)`         | k lags of target | `value_lag_1`, …, `value_lag_k`              |
| `p(1, 7, 12)`  | Specific lags    | `value_lag_1`, `value_lag_7`, `value_lag_12` |
| `q(w1, w2)`    | Moving averages  | `value_ma_w1`, `value_ma_w2`                 |
| `dow()`        | Day of week      | `dow` (factor)                               |
| `month()`      | Month            | `month` (factor)                             |
| `woy()`        | Week of year     | `woy` (integer)                              |
| `eom()`        | End of month     | `eom` (0/1)                                  |
| `dom()`        | Day of month     | `dom` (integer)                              |
| `rollsum(w)`   | Rolling sum      | `value_rollsum_w`                            |
| `rollsd(w)`    | Rolling std dev  | `value_rollsd_w`                             |
| `rollmin(w)`   | Rolling minimum  | `value_rollmin_w`                            |
| `rollmax(w)`   | Rolling maximum  | `value_rollmax_w`                            |
| `rollslope(w)` | Rolling trend    | `value_rollslope_w`                          |
| `trend(d)`     | Polynomial trend | `trend1`, `trend2`, …                        |
| `lag(var, k)`  | Lag of exogenous | `var_lag_k`                                  |
| `ma(var, w)`   | MA of exogenous  | `var_ma_w`                                   |
| `varname`      | Raw column       | Include as-is                                |

### Example Formulas

``` r
# Basic autoregressive model
value ~ p(12)

# Add calendar seasonality
value ~ p(12) + month() + dow()

# Add rolling statistics
value ~ p(12) + q(7) + rollsum(7, 28) + rollsd(7)

# Include exogenous variable with lags
value ~ p(12) + lag(price, 0, 1, 7) + promo

# Complex model
value ~ p(1, 7, 12) + q(7, 28) + month() + dow() + rollslope(12) + trend(1)
```

## Using Different Models

chronofeat works with any model that has a standard R interface. Pass
the model function directly:

``` r
# Linear regression
m_lm <- fit(value ~ p(12) + month(), data = ts_data, model = lm)

# GLM for count data
m_glm <- fit(count ~ p(7) + dow(), data = count_data,
             date = "date", model = glm, family = poisson())

# Random Forest
library(randomForest)
m_rf <- fit(value ~ p(12) + month(), data = ts_data, model = randomForest)
```

For more control, provide a custom model specification with `fit` and
`predict` functions:

``` r
xgb_spec <- list(
  fit = function(y, X, ...) {
    xgboost::xgboost(
      data = as.matrix(X),
      label = y,
      nrounds = 100,
      verbose = 0,
      ...
    )
  },
  predict = function(object, newdata, ...) {
    predict(object, as.matrix(newdata))
  }
)

m_xgb <- fit(value ~ p(12) + month(), data = ts_data, model = xgb_spec)
```

See the [Building Custom
Models](https://taf-society.github.io/chronofeat/articles/custom-models.md)
article for detailed examples with XGBoost, LightGBM, and other
frameworks.

## Panel Data (Multiple Time Series)

chronofeat handles panel data (multiple time series) through the
`groups` parameter. Each group gets its own:

- Lag and MA calculations (no cross-contamination)
- Forecasts (predictions stay within group boundaries)

``` r
# The retail dataset has 42 items
n_distinct(retail$items)
#> [1] 42

# fit() handles all groups automatically
model <- fit(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm
)

# forecast() produces forecasts for each group
fc <- forecast(model, h = 3)
fc %>%
  group_by(items) %>%
  summarise(n = n()) %>%
  head()
#> # A tibble: 6 × 2
#>   items     n
#>   <fct> <int>
#> 1 V10       3
#> 2 V11       3
#> 3 V12       3
#> 4 V13       3
#> 5 V14       3
#> 6 V15       3
```

## Cross-Validation

Evaluate model performance with time series cross-validation:

``` r
cv_results <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5,
  metric = "rmse"
)

print(cv_results)
```

See the
[Cross-Validation](https://taf-society.github.io/chronofeat/articles/cross-validation.md)
article for more details.

## Next Steps

- **[Building Custom
  Models](https://taf-society.github.io/chronofeat/articles/custom-models.md)**:
  Learn how to integrate XGBoost, LightGBM, neural networks, and custom
  ensembles
- **[Feature Engineering
  Reference](https://taf-society.github.io/chronofeat/articles/feature-engineering.md)**:
  Complete guide to all available features
- **[Data
  Preprocessing](https://taf-society.github.io/chronofeat/articles/preprocessing.md)**:
  Handle missing dates, gaps, and irregular calendars
- **[Cross-Validation](https://taf-society.github.io/chronofeat/articles/cross-validation.md)**:
  Proper evaluation workflows for time series
- **[Advanced
  Workflows](https://taf-society.github.io/chronofeat/articles/advanced-workflows.md)**:
  Hyperparameter tuning, model comparison, and production patterns

## Key Functions Reference

| Function                                                                                 | Purpose                                        |
|------------------------------------------------------------------------------------------|------------------------------------------------|
| [`TimeSeries()`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md)       | Create a time series object with preprocessing |
| [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md)                     | Fit a forecasting model with feature formula   |
| [`forecast()`](https://generics.r-lib.org/reference/forecast.html)                       | Generate recursive multi-step forecasts        |
| [`cv_forecast()`](https://taf-society.github.io/chronofeat/reference/cv_forecast.md)     | Time series cross-validation                   |
| [`as_model_spec()`](https://taf-society.github.io/chronofeat/reference/as_model_spec.md) | Convert model function to specification        |

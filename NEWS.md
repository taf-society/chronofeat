# chronofeat 0.6.0

## Major Changes

* **pkgdown Documentation Site**: Complete documentation overhaul with 6 comprehensive articles
  - Getting Started guide
  - Building Custom Models (primary focus)
  - Feature Engineering Reference
  - Data Preprocessing
  - Cross-Validation
  - Advanced Workflows (including ParBayesianOptimization)

* **Complete Forecast Engine Refactoring**: Eliminated ~155 lines of duplication through systematic helper extraction and unified forecast loop
  - **`.build_future_grid()`**: Unified future date generation for grouped/ungrouped data
  - **`.prepare_feature_row()`**: Centralized target/calendar/xreg feature assembly
  - **`.apply_schema()`**: Centralized schema harmonization and predictor selection
  - **Unified forecast loop**: Replaced separate grouped/ungrouped branches with single loop treating ungrouped as pseudo-group

## Bug Fixes

* **Window Validation**: C++ forecasting path now returns NA for incomplete rolling/trend windows instead of silently shortening them, aligning with R-side behavior and preventing train/test feature distribution mismatch
* **Cross-Validation**: Added panel alignment validation that detects and reports misaligned date ranges across groups
* **Grouped Features**: Fixed grouped rolling/MA/lag features that were bleeding across group boundaries by ensuring operations respect `group_by()`
* **Trend Index**: Corrected trend feature indexing during recursive forecasting - now uses `length(y) + 1` for next step instead of `length(y)`
* **Factor Coercion**: Now errors when attempting rolling statistics on factor columns instead of silently converting to meaningless numeric codes

# chronofeat 0.5.0

Initial release with formula-based time series forecasting engine.

## Features

* Formula interface for feature specification: `value ~ p(12) + q(7) + month() + dow()`
* Automatic feature engineering:
  - Target lags: `p(k)` creates k lags
  - Moving averages: `q(w1, w2, ...)` creates MAs with specified windows
  - Calendar features: `dow()`, `month()`, `woy()`, `eom()`, `dom()`
  - Rolling statistics: `rollsum()`, `rollsd()`, `rollmin()`, `rollmax()`, `rollslope()`
  - Trend features: `trend(1, 2, ...)` for polynomial trends
  - Exogenous variable lags/MAs: `lag(var, k)`, `ma(var, w)`
* Model-agnostic interface supporting any R model with fit/predict
* C++ accelerated recursive forecasting via Rcpp
* Multi-group/panel data support
* Time series cross-validation with `cv_forecast()`
* TimeSeries object with frequency detection and preprocessing

# Package index

## Core Forecasting

Main functions for fitting models and generating forecasts.

- [`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md) :
  Fit a Time Series Forecasting Model with Formula Interface
- [`forecast(`*`<tsfeature_fit>`*`)`](https://taf-society.github.io/chronofeat/reference/forecast.tsfeature_fit.md)
  : Generate Recursive Multi-Step Forecasts
- [`cv_forecast()`](https://taf-society.github.io/chronofeat/reference/cv_forecast.md)
  : Time Series Cross-Validation
- [`TimeSeries()`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md)
  : Create a TimeSeries Object with Complete Preprocessing Pipeline

## Feature Engineering

Functions for creating time series features from formulas.

- [`feat_lag_ma_dt()`](https://taf-society.github.io/chronofeat/reference/feat_lag_ma_dt.md)
  : Create Lag and Moving Average Features
- [`feat_rolling_dt()`](https://taf-society.github.io/chronofeat/reference/feat_rolling_dt.md)
  : Add Rolling Window Statistics
- [`feat_calendar_dt()`](https://taf-society.github.io/chronofeat/reference/feat_calendar_dt.md)
  : Add Calendar Features
- [`feat_trend()`](https://taf-society.github.io/chronofeat/reference/feat_trend.md)
  : Add Trend Features

## Data Preprocessing

Functions for preparing and cleaning time series data.

- [`fill_gaps()`](https://taf-society.github.io/chronofeat/reference/fill_gaps.md)
  : Fill Missing Target Values with Configurable Strategies

## Utilities

Helper functions and model specification tools.

- [`as_model_spec()`](https://taf-society.github.io/chronofeat/reference/as_model_spec.md)
  : Convert Model Function to Model Specification

## Datasets

Example datasets included in the package.

- [`retail`](https://taf-society.github.io/chronofeat/reference/retail.md)
  : Retail Sales Dataset

# Prepare Single Prediction Row

Assembles features for a single forecast step by combining target-based
features (lags, MAs, rolling stats) with calendar and exogenous variable
features.

## Usage

``` r
.prepare_feature_row(
  y_hist,
  next_date,
  target_col,
  p = NULL,
  q = NULL,
  roll_windows = NULL,
  roll_stats = c("sum", "sd", "min", "max"),
  trend_windows = NULL,
  trend_degrees = NULL,
  CAL_row = NULL,
  XF_row = NULL,
  groups_chr = NULL,
  date_col = "date"
)
```

## Arguments

- y_hist:

  Numeric vector of historical target values

- next_date:

  Date for this forecast step (used for calendar/xreg lookup)

- target_col:

  Character, name of target column

- p:

  Integer, number of lags (NULL if none)

- q:

  Integer vector of MA windows (NULL if none)

- roll_windows:

  Integer vector of rolling stat windows (NULL if none)

- roll_stats:

  Character vector of rolling statistics to compute

- trend_windows:

  Integer vector for trend slopes (NULL if none)

- trend_degrees:

  Integer vector for polynomial trends (NULL if none)

- CAL_row:

  Single-row tibble of calendar features for this date (NULL if none)

- XF_row:

  Single-row tibble of xreg features for this date (NULL if none)

- groups_chr:

  Character vector of group column names (NULL if ungrouped)

- date_col:

  Character, name of date column

## Value

Single-row tibble with all features

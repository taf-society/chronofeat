# Create Lag and Moving Average Features

Add lagged and moving average features for target and exogenous
variables. This function creates features suitable for time series
modeling by computing lags and rolling averages within groups.

## Usage

``` r
feat_lag_ma_dt(
  df,
  date,
  target,
  p = NULL,
  q = NULL,
  groups = NULL,
  xreg = NULL,
  xreg_lags = NULL,
  xreg_ma = NULL
)
```

## Arguments

- df:

  A data frame containing time series data

- date:

  Character string naming the date column

- target:

  Character string naming the target variable column

- p:

  Integer, number of target lags to create (e.g., p=12 creates lag_1
  through lag_12)

- q:

  Integer vector, moving average window sizes for target (e.g.,
  q=c(7,28))

- groups:

  Character vector naming grouping columns (e.g., c("store", "item"))

- xreg:

  Character vector naming exogenous variable columns to transform

- xreg_lags:

  Named list of lag specifications for exogenous variables (e.g.,
  list(price = c(1,7), promotion = c(0,1)))

- xreg_ma:

  Named list of MA window specifications for exogenous variables (e.g.,
  list(price = c(7,28)))

## Value

Data frame with original columns plus new lag and MA features

## Details

Lags and MAs are computed within each group separately. The data is
automatically sorted by groups and date before computation. Use lag 0 in
xreg_lags to include the current value of an exogenous variable.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create 7 lags and 7-day MA for sales
df_feat <- feat_lag_ma_dt(df, date = "date", target = "sales",
                           p = 7, q = 7, groups = "store_id")

# Add exogenous variable features
df_feat <- feat_lag_ma_dt(df, date = "date", target = "sales",
                           p = 3, groups = "store_id",
                           xreg = "price",
                           xreg_lags = list(price = c(0, 1, 7)))
} # }
```

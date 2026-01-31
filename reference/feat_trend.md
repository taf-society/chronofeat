# Add Trend Features

Generate polynomial trend features for time series data. Trends are
computed as sequential indices within each group, with optional
polynomial transformations.

## Usage

``` r
feat_trend(df, date, groups = NULL, degrees = 1L)
```

## Arguments

- df:

  A data frame containing time series data

- date:

  Symbol or character naming the date column (used for sorting)

- groups:

  Character vector naming grouping columns

- degrees:

  Integer vector of polynomial degrees (e.g., c(1, 2) for linear and
  quadratic)

## Value

Data frame with trend features added

## Details

Trend features are sequential indices (1, 2, 3, ...) within each group.
Polynomial degrees allow capturing non-linear trends:

- degree 1: linear trend (1, 2, 3, ...)

- degree 2: quadratic trend (1, 4, 9, ...)

- degree 3: cubic trend (1, 8, 27, ...)

## Examples

``` r
if (FALSE) { # \dontrun{
# Add linear trend
df_trend <- feat_trend(df, date = date, groups = "store_id", degrees = 1)

# Add linear and quadratic trends
df_trend <- feat_trend(df, date = date, groups = "store_id", degrees = c(1, 2))
} # }
```

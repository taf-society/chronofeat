# STL Decomposition Imputation

Fill NAs using seasonal decomposition fitted values.

## Usage

``` r
.fill_stl(y, dates, params = list())
```

## Arguments

- y:

  Numeric vector

- dates:

  Date vector

- params:

  List with:

  - period: Seasonal period (NULL = auto-detect, e.g., 7 for weekly, 365
    for yearly)

  - robust: Use robust fitting (default: TRUE)

  - max_gap: Maximum gap length to fill (default: Inf)

## Value

Filled numeric vector

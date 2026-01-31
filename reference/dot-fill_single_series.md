# Fill Single Time Series

Apply gap-filling strategy to a single time series.

## Usage

``` r
.fill_single_series(y, dates, strategy, params = list())
```

## Arguments

- y:

  Numeric vector with potential NAs

- dates:

  Date vector (same length as y)

- strategy:

  Character, gap-filling strategy

- params:

  List of strategy-specific parameters

## Value

List with:

- values: Numeric vector (filled)

- is_imputed: Logical vector indicating imputed positions

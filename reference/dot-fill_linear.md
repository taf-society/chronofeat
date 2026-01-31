# Linear Interpolation

Fill NAs using linear interpolation between known points.

## Usage

``` r
.fill_linear(y, dates, params = list())
```

## Arguments

- y:

  Numeric vector

- dates:

  Date vector

- params:

  List with:

  - extrapolate: Allow extrapolation beyond known range (default: FALSE)

  - max_gap: Maximum gap length to fill (default: Inf)

## Value

Filled numeric vector

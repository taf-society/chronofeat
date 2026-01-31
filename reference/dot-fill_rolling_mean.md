# Rolling Mean Imputation

Fill NAs using centered rolling mean of nearby observations.

## Usage

``` r
.fill_rolling_mean(y, params = list())
```

## Arguments

- y:

  Numeric vector

- params:

  List with:

  - window: Window size (default: 7)

  - center: Center the window (default: TRUE)

  - max_gap: Maximum gap length to fill (default: Inf)

## Value

Filled numeric vector

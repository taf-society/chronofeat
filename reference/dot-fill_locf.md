# Last Observation Carried Forward (LOCF)

Fill NAs by carrying forward the last non-NA value.

## Usage

``` r
.fill_locf(y, params = list())
```

## Arguments

- y:

  Numeric vector

- params:

  List with:

  - max_gap: Maximum gap length to fill (default: Inf)

## Value

Filled numeric vector

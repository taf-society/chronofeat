# Next Observation Carried Backward (NOCB)

Fill NAs by carrying backward the next non-NA value.

## Usage

``` r
.fill_nocb(y, params = list())
```

## Arguments

- y:

  Numeric vector

- params:

  List with:

  - max_gap: Maximum gap length to fill (default: Inf)

## Value

Filled numeric vector

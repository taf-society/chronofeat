# Gap-Filling Dispatcher (Internal)

Fill missing target values using specified strategy.

## Usage

``` r
.fill_target_gaps(
  data,
  target_col,
  date_col,
  groups_chr = NULL,
  strategy = "error",
  params = list()
)
```

## Arguments

- data:

  Data frame with time series data

- target_col:

  Character, name of target column

- date_col:

  Character, name of date column

- groups_chr:

  Character vector of group column names (NULL for ungrouped)

- strategy:

  Character, one of: "error", "zero", "locf", "nocb", "linear",
  "rolling_mean", "stl", "borrow", "custom"

- params:

  List of strategy-specific parameters

## Value

Data frame with filled target and is_imputed flag column

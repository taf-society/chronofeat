# Check for Trailing NA Targets

Validates that the target column does not end with NA values after
sorting by date. Trailing NAs will cause lags and other target-based
features to produce NA values, which leads to NA forecasts.

## Usage

``` r
.check_trailing_na(data, target_col, groups_chr = NULL)
```

## Arguments

- data:

  Data frame with history, already sorted by groups and date

- target_col:

  Character, name of target column

- groups_chr:

  Character vector of group column names (NULL for ungrouped)

## Value

NULL (invisibly). Stops with error if trailing NAs found.

# Build Future Date Grid

Generates future dates for forecasting, handling both single-series and
grouped data. For grouped data, creates a cross-product of groups x
future dates.

## Usage

``` r
.build_future_grid(history, h, date_col, groups_chr = NULL, frequency = NULL)
```

## Arguments

- history:

  Data frame with historical data

- h:

  Integer, forecast horizon

- date_col:

  Character, name of date column

- groups_chr:

  Character vector of group column names (NULL for ungrouped)

- frequency:

  Frequency object for date generation

## Value

Tibble with future dates (and group keys if grouped)

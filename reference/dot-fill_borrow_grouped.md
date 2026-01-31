# Cross-Series Borrowing (Grouped Data)

Fill NAs in one series by borrowing from similar peer series.

## Usage

``` r
.fill_borrow_grouped(data, target_col, date_col, groups_chr, params = list())
```

## Arguments

- data:

  Full data frame with all groups

- target_col:

  Character, name of target column

- date_col:

  Character, name of date column

- groups_chr:

  Character vector of group column names

- params:

  List with:

  - method: "median" (default), "mean", or "weighted"

  - neighbors: NULL (all groups), character vector (for single group
    column), or named list (for multi-level groups, e.g., list(region =
    c("North", "South")))

  - max_gap: Maximum gap length to fill (default: Inf)

## Value

Data frame with filled target column

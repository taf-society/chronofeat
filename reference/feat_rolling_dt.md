# Add Rolling Window Statistics

Compute rolling window statistics (sum, standard deviation, min, max)
and rolling trend slopes for the target variable. All statistics are
computed within groups if specified.

## Usage

``` r
feat_rolling_dt(
  df,
  date,
  target,
  groups = NULL,
  windows = c(7, 28),
  stats = c("sum", "sd", "min", "max"),
  trend_windows = NULL
)
```

## Arguments

- df:

  A data frame containing time series data

- date:

  Symbol or character naming the date column

- target:

  Symbol or character naming the target variable column

- groups:

  Character vector naming grouping columns

- windows:

  Integer vector of window sizes for rolling statistics (e.g., c(7, 28,
  90))

- stats:

  Character vector specifying which statistics to compute: "sum", "sd",
  "min", "max"

- trend_windows:

  Integer vector of window sizes for rolling linear trend slopes

## Value

Data frame with rolling statistic features added

## Details

Rolling statistics use right-aligned windows (including current
observation). Trend slopes are computed by fitting y ~ x within each
window. Missing values are handled with na.rm = TRUE for statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
# Add 7-day and 28-day rolling statistics
df_roll <- feat_rolling_dt(df, date = date, target = sales,
                            groups = "store_id",
                            windows = c(7, 28),
                            stats = c("sum", "sd", "min", "max"))

# Add rolling trend slopes
df_roll <- feat_rolling_dt(df, date = date, target = sales,
                            groups = "store_id",
                            windows = c(7, 28),
                            stats = "sum",
                            trend_windows = c(7, 14))
} # }
```

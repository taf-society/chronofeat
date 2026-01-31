# Add Calendar Features

Add calendar-based features such as day of week, month, week of year,
etc. These features are commonly useful for capturing seasonal patterns
in time series.

## Usage

``` r
feat_calendar_dt(
  df,
  date,
  dow = TRUE,
  woy = FALSE,
  month = TRUE,
  eom = TRUE,
  dom = FALSE
)
```

## Arguments

- df:

  A data frame containing time series data

- date:

  Symbol or character naming the date column (must be Date class)

- dow:

  Logical, add day of week as ordered factor (Monday-Sunday)

- woy:

  Logical, add week of year (1-53)

- month:

  Logical, add month as factor (01-12)

- eom:

  Logical, add end-of-month indicator (0 or 1)

- dom:

  Logical, add day of month (1-31)

## Value

Data frame with calendar features added

## Examples

``` r
if (FALSE) { # \dontrun{
# Add default calendar features (dow, month, eom)
df_cal <- feat_calendar_dt(df, date = date)

# Add all calendar features
df_cal <- feat_calendar_dt(df, date = date,
                            dow = TRUE, woy = TRUE, month = TRUE,
                            eom = TRUE, dom = TRUE)
} # }
```

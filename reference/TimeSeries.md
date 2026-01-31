# Create a TimeSeries Object with Complete Preprocessing Pipeline

This function creates a TimeSeries object that bundles data with
frequency information and provides a comprehensive preprocessing
pipeline for time series data. It handles:

1.  Frequency detection and validation

2.  Irregular calendar completion (missing dates)

3.  Target variable gap-filling

4.  Exogenous variable gap-filling

## Usage

``` r
TimeSeries(
  data,
  date,
  groups = NULL,
  frequency = NULL,
  auto_detect = TRUE,
  target = NULL,
  target_na = NULL,
  fill_time = FALSE,
  xreg_na = NULL
)
```

## Arguments

- data:

  A data frame containing the time series data

- date:

  Character string naming the date column. Accepts:

  - **Date**: For daily and longer frequencies (day, week, month,
    quarter, year)

  - **POSIXct**: Required for sub-daily frequencies (second, minute,
    halfhour, hour)

- groups:

  Optional character vector naming grouping columns for panel data. All
  preprocessing operations (time grid completion, gap-filling) are
  performed independently per group.

- frequency:

  Character string or numeric specifying the time frequency:

  - **Sub-daily** (require POSIXct): "second", "minute", "halfhour",
    "hour"

  - **Daily+** (work with Date or POSIXct): "day", "businessday",
    "biweekly", "week", "month", "quarter", "year"

  - **Numeric**: Custom interval in days (e.g., 7 for weekly, 14 for
    biweekly)

  If NULL and `auto_detect = TRUE`, frequency is inferred from median
  date/time differences.

- auto_detect:

  Logical, if TRUE and frequency is NULL, attempt to detect frequency
  from the data. Default: TRUE.

- target:

  Character string naming the target column (optional). Required if
  `target_na` is specified. The target column will be gap-filled
  according to the `target_na` strategy.

- target_na:

  List specifying gap-filling strategy for missing target values.

  - `strategy`: Character, one of:

    - **"error"** - Fail if NAs present (forces explicit choice)

    - **"zero"** - Replace NAs with 0 (useful for count data)

    - **"locf"** - Last observation carried forward

    - **"nocb"** - Next observation carried backward

    - **"linear"** - Linear interpolation (time-aware)

    - **"rolling_mean"** - Centered or right-aligned rolling mean

    - **"stl"** - Seasonal-Trend-Loess decomposition (auto-detects
      period)

    - **"borrow"** - Cross-series borrowing from peer groups

    - **"custom"** - User-provided function

  - `params`: List of strategy-specific parameters (see Details)

  Default: NULL (no target gap-filling). Adds `{target}_is_imputed` flag
  column.

- fill_time:

  Logical, if TRUE, complete missing dates using
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html).
  Uses the `frequency` parameter to determine step size. When enabled,
  adds rows for missing dates with NA values in all dynamic columns.
  Respects group boundaries for panel data. Default: FALSE.

- xreg_na:

  Named list specifying gap-filling strategies for exogenous columns.
  Each element should be:
  `column_name = list(strategy = "...", params = list(...))`.

  - **Keys**: Column names to fill (must exist in `data`)

  - **Values**: Lists with `strategy` and `params` (same as `target_na`)

  Example:
  `list(price = list(strategy = "locf"), promo = list(strategy = "zero"))`.
  Each column gets its own `{column}_is_imputed` flag. Filling is done
  per-group if `groups` is specified.

## Value

A `TimeSeries` object (S3 class) with components:

- `data` - Data frame with completed calendar and filled gaps (if
  requested)

- `date` - Name of the date column

- `groups` - Names of grouping columns (or NULL)

- `frequency` - Time frequency specification

- `target` - Name of the target column (or NULL)

- `target_na_meta` - Metadata about target gap-filling (or NULL):

  - `strategy` - Strategy used

  - `params` - Parameters used

  - `n_imputed` - Number of values imputed

  - `n_total` - Total observations

  - `pct_imputed` - Percentage imputed

- `xreg_na_meta` - Named list of metadata for each exogenous column (or
  empty list)

- `time_fill_meta` - Metadata about time grid completion (or NULL):

  - `n_added` - Number of rows added

  - `by` - Step size used

  - `n_total` - Total observations after completion

## Details

All preprocessing is auditable via metadata and `is_imputed` flags.

## Preprocessing Pipeline Order

TimeSeries() applies preprocessing in this order:

1.  **Sort data** by groups (if present) and date

2.  **Complete time grid** (if `fill_time$enabled = TRUE`)

    - Per-group for panel data

    - Adds rows for missing dates with NA values

3.  **Fill target** (if `target` and `target_na` specified)

    - Uses specified strategy

    - Adds `{target}_is_imputed` flag

4.  **Fill exogenous columns** (if `xreg_na` specified)

    - Per-column, per-group filling

    - Adds `{column}_is_imputed` flag for each

## Gap-Filling Strategy Parameters

Common parameters across strategies:

- `max_gap`: Maximum consecutive NAs to fill (default: Inf). Throws
  error if gap exceeds this value.

Strategy-specific parameters:

- **linear**: `extrapolate = FALSE` - Allow extrapolation beyond
  observed range

- **rolling_mean**: `window = 7, center = TRUE` - Window size and
  alignment

- **stl**: `period = NULL, robust = TRUE` - Seasonal period
  (auto-detected if NULL) and robust fitting

- **borrow**: `method = "median", neighbors = NULL` - Aggregation method
  and neighbor filtering

- **custom**: `fn = function(y, dates, params) {...}` - User-provided
  function

See
[`?fill_gaps`](https://taf-society.github.io/chronofeat/reference/fill_gaps.md)
for detailed documentation of each strategy.

## Auditability

All gap-filling operations add `is_imputed` flags:

- `{target}_is_imputed` - Logical vector marking imputed target values

- `{column}_is_imputed` - Logical vector for each exogenous column

These flags can be used to:

- Filter imputed rows: `data[!data$sales_is_imputed, ]`

- Use as model predictor: `sales ~ ... + sales_is_imputed`

- Weight observations: `lm(..., weights = ifelse(is_imputed, 0.5, 1))`

Metadata is stored in the TimeSeries object for full reproducibility.

## Examples

``` r
if (FALSE) { # \dontrun{
# ===== Basic Usage =====
# Create TimeSeries with auto-detected frequency
ts <- TimeSeries(retail, date = "date", groups = "store")

# Specify frequency explicitly
ts <- TimeSeries(retail, date = "date", groups = "store", frequency = "day")

# ===== Target Gap-Filling =====
# Fill target with last observation carried forward
ts <- TimeSeries(
  retail,
  date = "date",
  target = "sales",
  target_na = list(strategy = "locf", params = list(max_gap = 7))
)

# Fill target with seasonal decomposition
ts <- TimeSeries(
  retail,
  date = "date",
  groups = "store",
  target = "sales",
  target_na = list(strategy = "stl", params = list(period = 7))
)

# ===== Complete Preprocessing Pipeline =====
# Handle irregular calendar + target gaps + exogenous gaps
ts <- TimeSeries(
  sales_df,
  date = "date",
  groups = c("store", "item"),
  frequency = "day",
  target = "sales",
  target_na = list(strategy = "locf"),
  fill_time = TRUE,  # Complete missing dates using frequency
  xreg_na = list(
    price = list(strategy = "linear"),  # Smooth interpolation
    promo = list(strategy = "zero")     # NA = no promotion
  )
)

# Inspect preprocessing results
print(ts)  # Shows all metadata
summary(ts$data$sales_is_imputed)  # Check how many values imputed

# ===== Using with fit() and forecast() =====
# fit() extracts cleaned data automatically
m <- fit(sales ~ p(7) + price + promo, data = ts, model = lm)

# forecast() uses stored frequency
fc <- forecast(m, h = 14)

# ===== Panel Data Example =====
# Each store gets independent preprocessing
ts <- TimeSeries(
  panel_df,
  date = "date",
  groups = "store",
  frequency = "day",
  target = "sales",
  target_na = list(strategy = "borrow", params = list(method = "median")),
  fill_time = TRUE,  # Uses frequency = "day"
  xreg_na = list(
    price = list(strategy = "locf"),
    temp = list(strategy = "linear")
  )
)

# Metadata shows per-store/column imputation counts
ts$time_fill_meta  # Rows added to complete calendar
ts$target_na_meta  # Target imputation stats
ts$xreg_na_meta    # Exogenous imputation stats per column
} # }
```

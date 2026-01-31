# Data Preprocessing

## Overview

Real-world time series data often has issues:

- **Irregular calendars**: Missing dates (weekends, holidays, outages)
- **Target gaps**: Missing values in the variable you want to forecast
- **Exogenous gaps**: Missing values in predictor variables

The
[`TimeSeries()`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md)
function provides a complete preprocessing pipeline to handle all three
problems in a single, auditable workflow.

``` r
library(chronofeat)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

------------------------------------------------------------------------

## The TimeSeries Object

[`TimeSeries()`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md)
creates an object that bundles your data with metadata:

``` r
data(retail)

ts_data <- TimeSeries(
  data = retail,
  date = "date",
  groups = "items",
  frequency = "month"
)

ts_data
#> TimeSeries object
#> ----------------
#> Date column: date ( Date )
#> Frequency: month 
#> Groups: items 
#> Observations: 13986 
#> 
#> Data (first few rows):
#> # A tibble: 6 × 3
#>   date       items value
#>   <date>     <fct> <dbl>
#> 1 1982-04-01 V10    94  
#> 2 1982-05-01 V10   106. 
#> 3 1982-06-01 V10    95.1
#> 4 1982-07-01 V10    95.3
#> 5 1982-08-01 V10    82.8
#> 6 1982-09-01 V10    89.4
```

### What TimeSeries Does

1.  **Validates** the date column (must be Date or POSIXct)
2.  **Sorts** data by groups and date (critical for lag calculations)
3.  **Detects or validates** the time frequency
4.  **Optionally** completes the time grid and fills gaps

### Accessing the Data

``` r
# Get the processed data frame
head(ts_data$data)
#> # A tibble: 6 × 3
#>   date       items value
#>   <date>     <fct> <dbl>
#> 1 1982-04-01 V10    94  
#> 2 1982-05-01 V10   106. 
#> 3 1982-06-01 V10    95.1
#> 4 1982-07-01 V10    95.3
#> 5 1982-08-01 V10    82.8
#> 6 1982-09-01 V10    89.4

# Access metadata
ts_data$frequency
#> [1] "month"
ts_data$groups
#> [1] "items"
ts_data$date
#> [1] "date"
```

------------------------------------------------------------------------

## Frequency Detection

TimeSeries can auto-detect frequency or validate your specification:

### Auto-Detection

``` r
ts_auto <- TimeSeries(
  data = retail,
  date = "date",
  groups = "items"
  # frequency not specified - will be auto-detected
)
#> Auto-detected frequency: month
```

### Explicit Specification

``` r
ts_explicit <- TimeSeries(
  data = retail,
  date = "date",
  groups = "items",
  frequency = "month"
)
```

### Supported Frequencies

| Frequency       | Description           | Date Type Required |
|-----------------|-----------------------|--------------------|
| `"second"`      | Per-second            | POSIXct            |
| `"minute"`      | Per-minute            | POSIXct            |
| `"halfhour"`    | 30-minute intervals   | POSIXct            |
| `"hour"`        | Hourly                | POSIXct            |
| `"day"`         | Daily                 | Date or POSIXct    |
| `"businessday"` | Weekdays only         | Date or POSIXct    |
| `"biweekly"`    | Every 2 weeks         | Date or POSIXct    |
| `"week"`        | Weekly                | Date or POSIXct    |
| `"month"`       | Monthly               | Date or POSIXct    |
| `"quarter"`     | Quarterly             | Date or POSIXct    |
| `"year"`        | Yearly                | Date or POSIXct    |
| Numeric         | Custom days (e.g., 7) | Date or POSIXct    |

------------------------------------------------------------------------

## Completing the Time Grid

Real data often has missing dates. Use `fill_time = TRUE` to complete
the calendar:

``` r
# Create data with missing dates
sales_irregular <- data.frame(
  store = rep("A", 5),
  date = as.Date(c('2024-01-01', '2024-01-02', '2024-01-03',
                   '2024-01-08', '2024-01-09')),  # Missing Jan 4-7
  sales = c(100, 120, 110, 130, 125)
)

ts_complete <- TimeSeries(
  sales_irregular,
  date = "date",
  groups = "store",
  frequency = "day",
  fill_time = TRUE
)
#> Time grid completed: 4 rows added (step size: day)

ts_complete$data
#> # A tibble: 9 × 3
#>   store date       sales
#>   <chr> <date>     <dbl>
#> 1 A     2024-01-01   100
#> 2 A     2024-01-02   120
#> 3 A     2024-01-03   110
#> 4 A     2024-01-04    NA
#> 5 A     2024-01-05    NA
#> 6 A     2024-01-06    NA
#> 7 A     2024-01-07    NA
#> 8 A     2024-01-08   130
#> 9 A     2024-01-09   125
```

**What happens:**

- Rows are added for missing dates (2024-01-04 through 2024-01-07)
- `sales` is NA for the new rows
- `time_fill_meta` tracks what was added

``` r
ts_complete$time_fill_meta
#> $n_added
#> [1] 4
#> 
#> $by
#> [1] "day"
#> 
#> $n_total
#> [1] 9
```

------------------------------------------------------------------------

## Target Gap-Filling

Fill missing values in your target variable using various strategies.

### Basic Usage

``` r
# Data with gaps
sales_with_gaps <- data.frame(
  date = seq(as.Date('2024-01-01'), by = 'day', length.out = 10),
  sales = c(100, 120, NA, NA, 150, 160, NA, 180, 190, 200)
)

ts_filled <- TimeSeries(
  sales_with_gaps,
  date = "date",
  frequency = "day",
  target = "sales",
  target_na = list(strategy = "locf")
)
#> Target gap-filling: 3 values imputed (30.0%) using 'locf' strategy

ts_filled$data
#>          date sales sales_is_imputed
#> 1  2024-01-01   100            FALSE
#> 2  2024-01-02   120            FALSE
#> 3  2024-01-03   120             TRUE
#> 4  2024-01-04   120             TRUE
#> 5  2024-01-05   150            FALSE
#> 6  2024-01-06   160            FALSE
#> 7  2024-01-07   160             TRUE
#> 8  2024-01-08   180            FALSE
#> 9  2024-01-09   190            FALSE
#> 10 2024-01-10   200            FALSE
```

### The `is_imputed` Flag

Every filled value is tracked:

``` r
# See which values were imputed
ts_filled$data %>%
  select(date, sales, sales_is_imputed)
#>          date sales sales_is_imputed
#> 1  2024-01-01   100            FALSE
#> 2  2024-01-02   120            FALSE
#> 3  2024-01-03   120             TRUE
#> 4  2024-01-04   120             TRUE
#> 5  2024-01-05   150            FALSE
#> 6  2024-01-06   160            FALSE
#> 7  2024-01-07   160             TRUE
#> 8  2024-01-08   180            FALSE
#> 9  2024-01-09   190            FALSE
#> 10 2024-01-10   200            FALSE
```

**Use the flag for:**

- Filtering: Train only on real data
- Modeling: Use as a predictor (`sales_is_imputed` in formula)
- Weighting: Down-weight imputed observations

### Available Strategies

| Strategy         | Description                       | Best For                         |
|------------------|-----------------------------------|----------------------------------|
| `"locf"`         | Last observation carried forward  | Sensor outages, sticky values    |
| `"nocb"`         | Next observation carried backward | Leading indicators               |
| `"linear"`       | Linear interpolation              | Smooth, continuous variables     |
| `"zero"`         | Replace with 0                    | Count data (missing = no events) |
| `"rolling_mean"` | Rolling mean imputation           | Noisy data                       |
| `"stl"`          | Seasonal decomposition            | Seasonal patterns                |
| `"borrow"`       | Borrow from peer groups           | Panel data cold-start            |
| `"custom"`       | User-provided function            | Special cases                    |

### Strategy Examples

#### Last Observation Carried Forward (LOCF)

``` r
ts <- TimeSeries(
  data, date = "date", frequency = "day",
  target = "sales",
  target_na = list(strategy = "locf")
)
```

Simple and fast. Good for sensor data with short outages.

#### Linear Interpolation

``` r
ts <- TimeSeries(
  data, date = "date", frequency = "day",
  target = "sales",
  target_na = list(strategy = "linear")
)
```

Smooth interpolation between known values. Good for slowly-changing
variables.

#### Seasonal Decomposition (STL)

``` r
ts <- TimeSeries(
  data, date = "date", frequency = "day",
  target = "sales",
  target_na = list(
    strategy = "stl",
    params = list(period = 7)  # Weekly seasonality
  )
)
```

Uses seasonal pattern to fill gaps. Best for clearly seasonal data.

#### Cross-Series Borrowing

``` r
# For panel data: fill from peer groups
ts <- TimeSeries(
  panel_data,
  date = "date",
  groups = "store",
  frequency = "day",
  target = "sales",
  target_na = list(
    strategy = "borrow",
    params = list(method = "median")  # Use median of peers
  )
)
```

Uses values from other groups at the same date. Good for cold-start
problems.

### Strategy Parameters

Control gap-filling behavior:

``` r
ts <- TimeSeries(
  data, date = "date", frequency = "day",
  target = "sales",
  target_na = list(
    strategy = "locf",
    params = list(
      max_gap = 7  # Error if gap > 7 days
    )
  )
)
```

Common parameters:

- `max_gap`: Maximum consecutive NAs to fill (error if exceeded)
- `period`: Seasonal period for STL
- `window`: Window size for rolling_mean
- `center`: TRUE for centered window, FALSE for right-aligned

------------------------------------------------------------------------

## Exogenous Gap-Filling

Fill gaps in predictor variables with different strategies per column:

``` r
retail_with_gaps <- data.frame(
  date = seq(as.Date('2024-01-01'), by = 'day', length.out = 10),
  sales = c(100, 120, 130, 140, 150, 160, 170, 180, 190, 200),
  price = c(10, NA, NA, 10, 12, 12, NA, 12, 12, 12),
  promo = c(0, 0, 1, 1, NA, NA, 0, 0, 0, 1),
  temp = c(20, 21, NA, 23, 24, NA, 26, 27, 28, 29)
)

ts_xreg <- TimeSeries(
  retail_with_gaps,
  date = "date",
  frequency = "day",
  xreg_na = list(
    price = list(strategy = "locf"),      # Prices are sticky
    promo = list(strategy = "zero"),      # Missing = no promotion
    temp = list(strategy = "linear")      # Smooth weather interpolation
  )
)
#> Exogenous 'price': 3 values imputed (30.0%) using 'locf' strategy
#> Exogenous 'promo': 2 values imputed (20.0%) using 'zero' strategy
#> Exogenous 'temp': 2 values imputed (20.0%) using 'linear' strategy

ts_xreg$data %>%
  select(date, price, price_is_imputed, promo, promo_is_imputed, temp, temp_is_imputed)
#>          date price price_is_imputed promo promo_is_imputed temp
#> 1  2024-01-01    10            FALSE     0            FALSE   20
#> 2  2024-01-02    10             TRUE     0            FALSE   21
#> 3  2024-01-03    10             TRUE     1            FALSE   22
#> 4  2024-01-04    10            FALSE     1            FALSE   23
#> 5  2024-01-05    12            FALSE     0             TRUE   24
#> 6  2024-01-06    12            FALSE     0             TRUE   25
#> 7  2024-01-07    12             TRUE     0            FALSE   26
#> 8  2024-01-08    12            FALSE     0            FALSE   27
#> 9  2024-01-09    12            FALSE     0            FALSE   28
#> 10 2024-01-10    12            FALSE     1            FALSE   29
#>    temp_is_imputed
#> 1            FALSE
#> 2            FALSE
#> 3             TRUE
#> 4            FALSE
#> 5            FALSE
#> 6             TRUE
#> 7            FALSE
#> 8            FALSE
#> 9            FALSE
#> 10           FALSE
```

### Strategy Selection Guide

| Data Type           | Recommended Strategy        |
|---------------------|-----------------------------|
| Prices (sticky)     | `locf` with `max_gap = 7`   |
| Promotions (binary) | `zero` (missing = no promo) |
| Weather             | `linear`                    |
| Count data          | `zero`                      |
| Sensor readings     | `locf` or `rolling_mean`    |
| Seasonal patterns   | `stl`                       |
| Panel cold-start    | `borrow`                    |

------------------------------------------------------------------------

## Complete Pipeline Example

Handle all three problems in one call:

``` r
# Messy data: missing dates + target gaps + exogenous gaps
messy_data <- data.frame(
  store = rep("A", 7),
  date = as.Date(c('2024-01-01', '2024-01-02', '2024-01-03',
                   '2024-01-06', '2024-01-07', '2024-01-08', '2024-01-09')),
  sales = c(100, 120, NA, 160, 170, NA, 190),
  price = c(10, 10, 10, NA, 12, 12, 12),
  promo = c(0, 1, 1, NA, 0, 0, 0)
)

ts_clean <- TimeSeries(
  messy_data,
  date = "date",
  groups = "store",
  frequency = "day",
  fill_time = TRUE,
  target = "sales",
  target_na = list(strategy = "locf"),
  xreg_na = list(
    price = list(strategy = "locf"),
    promo = list(strategy = "zero")
  )
)
#> Time grid completed: 2 rows added (step size: day)
#> Target gap-filling: 4 values imputed (44.4%) using 'locf' strategy
#> Exogenous 'price': 3 values imputed (33.3%) using 'locf' strategy
#> Exogenous 'promo': 3 values imputed (33.3%) using 'zero' strategy

ts_clean
#> TimeSeries object
#> ----------------
#> Date column: date ( Date )
#> Frequency: day 
#> Groups: store 
#> Observations: 9 
#> Time grid: 2 rows added (step: day )
#> Target: sales [locf: 4 imputed, 44.4%]
#> Exogenous:
#>   price [locf: 3 imputed, 33.3%]
#>   promo [zero: 3 imputed, 33.3%]
#> 
#> Data (first few rows):
#> # A tibble: 6 × 8
#>   store date       sales price promo sales_is_imputed price_is_imputed
#>   <chr> <date>     <dbl> <dbl> <dbl> <lgl>            <lgl>           
#> 1 A     2024-01-01   100    10     0 FALSE            FALSE           
#> 2 A     2024-01-02   120    10     1 FALSE            FALSE           
#> 3 A     2024-01-03   120    10     1 TRUE             FALSE           
#> 4 A     2024-01-04   120    10     0 TRUE             TRUE            
#> 5 A     2024-01-05   120    10     0 TRUE             TRUE            
#> 6 A     2024-01-06   160    10     0 FALSE            TRUE            
#> # ℹ 1 more variable: promo_is_imputed <lgl>
```

``` r
ts_clean$data
#> # A tibble: 9 × 8
#>   store date       sales price promo sales_is_imputed price_is_imputed
#>   <chr> <date>     <dbl> <dbl> <dbl> <lgl>            <lgl>           
#> 1 A     2024-01-01   100    10     0 FALSE            FALSE           
#> 2 A     2024-01-02   120    10     1 FALSE            FALSE           
#> 3 A     2024-01-03   120    10     1 TRUE             FALSE           
#> 4 A     2024-01-04   120    10     0 TRUE             TRUE            
#> 5 A     2024-01-05   120    10     0 TRUE             TRUE            
#> 6 A     2024-01-06   160    10     0 FALSE            TRUE            
#> 7 A     2024-01-07   170    12     0 FALSE            FALSE           
#> 8 A     2024-01-08   170    12     0 TRUE             FALSE           
#> 9 A     2024-01-09   190    12     0 FALSE            FALSE           
#> # ℹ 1 more variable: promo_is_imputed <lgl>
```

------------------------------------------------------------------------

## Metadata and Auditability

TimeSeries tracks all preprocessing:

``` r
# Time grid completion
ts_clean$time_fill_meta
#> $n_added
#> [1] 2
#> 
#> $by
#> [1] "day"
#> 
#> $n_total
#> [1] 9

# Target gap-filling
ts_clean$target_na_meta
#> $strategy
#> [1] "locf"
#> 
#> $params
#> list()
#> 
#> $n_imputed
#> [1] 4
#> 
#> $n_total
#> [1] 9
#> 
#> $pct_imputed
#> [1] 44.44444

# Exogenous gap-filling
ts_clean$xreg_na_meta
#> $price
#> $price$strategy
#> [1] "locf"
#> 
#> $price$params
#> list()
#> 
#> $price$n_imputed
#> [1] 3
#> 
#> $price$n_total
#> [1] 9
#> 
#> $price$pct_imputed
#> [1] 33.33333
#> 
#> 
#> $promo
#> $promo$strategy
#> [1] "zero"
#> 
#> $promo$params
#> list()
#> 
#> $promo$n_imputed
#> [1] 3
#> 
#> $promo$n_total
#> [1] 9
#> 
#> $promo$pct_imputed
#> [1] 33.33333
```

### Saving Metadata for Reproducibility

``` r
# Save preprocessing configuration
config <- list(
  created = Sys.time(),
  frequency = ts_clean$frequency,
  fill_time = ts_clean$time_fill_meta,
  target_na = ts_clean$target_na_meta,
  xreg_na = ts_clean$xreg_na_meta
)

saveRDS(config, "preprocessing_config.rds")
```

------------------------------------------------------------------------

## Integration with fit() and forecast()

[`fit()`](https://taf-society.github.io/chronofeat/reference/fit.md)
automatically extracts and uses the processed data:

``` r
ts <- TimeSeries(
  retail, date = "date", groups = "items",
  frequency = "month",
  target = "value",
  target_na = list(strategy = "locf")
)

# fit() uses ts$data automatically
m <- fit(value ~ p(12) + month(), data = ts, model = lm)

# forecast() uses stored frequency for date generation
fc <- forecast(m, h = 12)
```

### The is_imputed Flag as a Predictor

``` r
# Let the model know which values were imputed
m <- fit(
  value ~ p(12) + month() + value_is_imputed,
  data = ts,
  model = lm
)
```

### Weighting by Imputation Status

``` r
# Down-weight imputed observations (requires custom model spec)
weighted_lm_spec <- list(
  fit = function(y, X, ...) {
    # Check for is_imputed column
    if ("value_is_imputed" %in% names(X)) {
      weights <- ifelse(X$value_is_imputed, 0.5, 1.0)
      X$value_is_imputed <- NULL  # Remove from predictors
    } else {
      weights <- rep(1, length(y))
    }

    train_df <- cbind(data.frame(.response = y), X)
    lm(.response ~ ., data = train_df, weights = weights)
  },
  predict = function(object, newdata, ...) {
    newdata$value_is_imputed <- NULL
    predict(object, newdata = newdata)
  }
)
```

------------------------------------------------------------------------

## Best Practices

### 1. Validate Preprocessing Results

``` r
# Check imputation rates
ts$data %>%
  summarise(
    pct_sales_imputed = 100 * mean(sales_is_imputed),
    pct_price_imputed = 100 * mean(price_is_imputed)
  )

# Check for remaining NAs
sapply(ts$data, function(x) sum(is.na(x)))
```

### 2. Use max_gap to Prevent Over-Imputation

``` r
# Refuse to fill gaps longer than 7 days
target_na = list(
  strategy = "locf",
  params = list(max_gap = 7)
)
```

### 3. Compare Strategies

``` r
# Test different strategies
ts_locf <- TimeSeries(..., target_na = list(strategy = "locf"))
ts_linear <- TimeSeries(..., target_na = list(strategy = "linear"))
ts_stl <- TimeSeries(..., target_na = list(strategy = "stl"))

# Compare forecast accuracy with cross-validation
cv_locf <- cv_forecast(value ~ p(12), data = ts_locf, model = lm, h = 6)
cv_linear <- cv_forecast(value ~ p(12), data = ts_linear, model = lm, h = 6)
cv_stl <- cv_forecast(value ~ p(12), data = ts_stl, model = lm, h = 6)
```

### 4. Document Preprocessing Decisions

``` r
# Record your choices
preprocessing_notes <- list(
  target_strategy = "STL with period=7",
  rationale = "Weekly seasonality detected in autocorrelation",
  alternatives_tried = c("locf", "linear"),
  validation = "STL gave lowest CV RMSE"
)
```

------------------------------------------------------------------------

## Warning: Trailing NA After fill_time

If `fill_time = TRUE` adds rows at the end of your series without target
values, forecasting will fail:

``` r
# This will warn about trailing NA
ts <- TimeSeries(
  data_ending_mid_month,
  date = "date",
  frequency = "day",
  fill_time = TRUE,  # Completes to end of month
  target = "sales"   # But no target_na specified!
)

# forecast() will error: "trailing NA"
```

**Solution**: Either specify `target_na` to fill the gaps, or ensure
your data ends with non-NA target values.

------------------------------------------------------------------------

## Summary

| Parameter   | Purpose                                  |
|-------------|------------------------------------------|
| `date`      | Name of date column                      |
| `groups`    | Names of group columns (panel data)      |
| `frequency` | Time frequency (auto-detected if NULL)   |
| `fill_time` | Complete missing dates                   |
| `target`    | Name of target column for gap-filling    |
| `target_na` | Strategy and params for target gaps      |
| `xreg_na`   | Per-column strategies for exogenous gaps |

**Key outputs:**

- `$data` - Processed data frame
- `$*_is_imputed` columns - Track which values were filled
- `$*_meta` - Metadata for auditability

See also:
[`?fill_gaps`](https://taf-society.github.io/chronofeat/reference/fill_gaps.md)
for detailed strategy documentation.

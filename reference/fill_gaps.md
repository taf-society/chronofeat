# Fill Missing Target Values with Configurable Strategies

Apply gap-filling strategies to handle missing target values in time
series data. This ensures train/forecast parity by making features
deterministic and auditable.

## Usage

``` r
fill_gaps(
  data,
  target,
  date,
  groups = NULL,
  strategy = "error",
  params = list()
)
```

## Arguments

- data:

  Data frame with time series data

- target:

  Character, name of target column

- date:

  Character, name of date column

- groups:

  Character vector of group column names (NULL for ungrouped)

- strategy:

  Character, gap-filling strategy. One of:

  - `"error"` - Fail if any NAs present (default, forces explicit
    choice)

  - `"zero"` - Replace NAs with 0 (appropriate for count data)

  - `"locf"` - Last observation carried forward

  - `"nocb"` - Next observation carried backward

  - `"linear"` - Linear interpolation (Phase 2)

  - `"rolling_mean"` - Centered rolling mean (Phase 2)

  - `"stl"` - Seasonal decomposition (Phase 3)

  - `"borrow"` - Cross-series borrowing for panel data (Phase 3)

  - `"custom"` - User-provided function (Phase 4)

- params:

  List of strategy-specific parameters. Common parameters:

  - `max_gap` - Maximum gap length to fill (for locf, nocb)

  - `extrapolate` - Allow extrapolation (for linear)

  - `window` - Window size (for rolling_mean)

  - `period` - Seasonal period (for stl)

  - `fn` - User function (for custom)

## Value

Data frame with:

- Original columns

- Filled target column

- `{target}_is_imputed` - Logical flag indicating imputed values

## Details

The `is_imputed` flag allows downstream models to:

- Filter out imputed rows if desired

- Use the flag as a predictor to learn different behavior

- Weight imputed observations differently

Gap-filling respects group boundaries and never fills across groups.
Each group's time series is filled independently.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retail sales: missing = no sale
sales_filled <- fill_gaps(sales_data, target = "revenue", date = "date",
                          groups = "store", strategy = "zero")

# Sensor data: carry forward up to 3 missing readings
sensor_filled <- fill_gaps(sensor_data, target = "temperature",
                           date = "timestamp", groups = "device",
                           strategy = "locf",
                           params = list(max_gap = 3))

# Check imputation summary
table(sensor_filled$temperature_is_imputed)

# Use filled data in forecasting
ts <- TimeSeries(sensor_filled, date = "timestamp", groups = "device")
m <- fit(temperature ~ p(7) + rollsum(7), data = ts, model = lm)
} # }
```

# Cross-Validation

## Overview

Time series cross-validation is essential for:

- **Evaluating** model performance realistically
- **Comparing** different models or feature sets
- **Tuning** hyperparameters
- **Estimating** forecast uncertainty

Standard k-fold CV doesn’t work for time series because it violates
temporal ordering. chronofeat provides
[`cv_forecast()`](https://taf-society.github.io/chronofeat/reference/cv_forecast.md)
for proper time series cross-validation.

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

data(retail)
ts_data <- TimeSeries(retail, date = "date", groups = "items", frequency = "month")
```

------------------------------------------------------------------------

## Basic Usage

``` r
cv_results <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5
)

print(cv_results)
#> Time Series Cross-Validation Results
#> =====================================
#> 
#> Parameters:
#>   Horizon (h): 6
#>   Number of windows: 5
#>   Completed folds: 5
#>   Window type: expanding
#>   Step size: 6
#>   Metric: rmse
#> 
#> Metrics by fold:
#>  fold train_start  train_end test_start   test_end n_train n_test     rmse
#>     1  1982-04-01 2007-06-01 2007-07-01 2007-12-01   12726    252 36.54537
#>     2  1982-04-01 2007-12-01 2008-01-01 2008-06-01   12978    252 43.84257
#>     3  1982-04-01 2008-06-01 2008-07-01 2008-12-01   13230    252 71.45520
#>     4  1982-04-01 2008-12-01 2009-01-01 2009-06-01   13482    252 51.45988
#>     5  1982-04-01 2009-06-01 2009-07-01 2009-12-01   13734    252 57.48774
#>     0        <NA>       <NA>       <NA>       <NA>      NA   1260 53.50989
```

### Understanding the Output

The output contains:

- **metrics**: Per-fold and overall performance metrics
- **params**: CV configuration parameters
- **predictions** (optional): Individual predictions for analysis

``` r
cv_results$metrics
#>   fold train_start  train_end test_start   test_end n_train n_test     rmse
#> 1    1  1982-04-01 2007-06-01 2007-07-01 2007-12-01   12726    252 36.54537
#> 2    2  1982-04-01 2007-12-01 2008-01-01 2008-06-01   12978    252 43.84257
#> 3    3  1982-04-01 2008-06-01 2008-07-01 2008-12-01   13230    252 71.45520
#> 4    4  1982-04-01 2008-12-01 2009-01-01 2009-06-01   13482    252 51.45988
#> 5    5  1982-04-01 2009-06-01 2009-07-01 2009-12-01   13734    252 57.48774
#> 6    0        <NA>       <NA>       <NA>       <NA>      NA   1260 53.50989
```

- `fold = 0` is the overall (weighted average) metric
- Each fold shows training and test date ranges
- `n_train` and `n_test` show observation counts

------------------------------------------------------------------------

## Window Types

### Expanding Window (Default)

The training set grows with each fold:

    Fold 1: [----Train----][Test]
    Fold 2: [------Train------][Test]
    Fold 3: [--------Train--------][Test]

``` r
cv_expanding <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5,
  window_type = "expanding"
)
```

**Best for:** Most cases. More training data improves model estimates.

### Sliding Window

Fixed-size training window that slides forward:

    Fold 1: [----Train----][Test]
    Fold 2:    [----Train----][Test]
    Fold 3:       [----Train----][Test]

``` r
cv_sliding <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5,
  window_type = "sliding",
  window_size = 120  # Use last 120 observations for training
)
```

**Best for:**

- Non-stationary data where old patterns don’t apply
- Models that should only use recent data
- Testing how model performs with limited history

------------------------------------------------------------------------

## Configuring CV

### Horizon (`h`)

Forecast horizon - how many steps ahead to predict:

``` r
# Match your production forecast horizon
cv_results <- cv_forecast(
  ...,
  h = 12  # If you forecast 12 months ahead in production
)
```

### Number of Windows (`n_windows`)

More windows = more reliable estimates, but slower:

``` r
# Quick check
cv_quick <- cv_forecast(..., n_windows = 3)

# Thorough evaluation
cv_thorough <- cv_forecast(..., n_windows = 10)
```

### Step Size (`step_size`)

How far to move between folds (default: `h`):

``` r
# Non-overlapping test sets (default)
cv_results <- cv_forecast(..., h = 6, step_size = 6)

# Overlapping for more windows from limited data
cv_results <- cv_forecast(..., h = 6, step_size = 1)
```

------------------------------------------------------------------------

## Metrics

### Built-in Metrics

``` r
# Root Mean Squared Error (default)
cv_forecast(..., metric = "rmse")

# Mean Absolute Error
cv_forecast(..., metric = "mae")

# Mean Absolute Percentage Error
cv_forecast(..., metric = "mape")

# Mean Squared Error
cv_forecast(..., metric = "mse")
```

### Custom Metrics

Provide a function that takes `(actual, predicted)`:

``` r
# Symmetric MAPE
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted) + 1e-8)) * 100
}

cv_results <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5,
  metric = smape
)
```

------------------------------------------------------------------------

## Analyzing Results

### Summary Statistics

``` r
summary(cv_results)
#> Time Series Cross-Validation Summary
#> ====================================
#> 
#> Metric: rmse
#>   Mean:   52.1582
#>   Median: 51.4599
#>   SD:     13.3620
#>   Min:    36.5454
#>   Max:    71.4552
```

### Predictions by Fold

Request individual predictions for detailed analysis:

``` r
cv_with_preds <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 3,
  return_predictions = TRUE
)

head(cv_with_preds$predictions)
#> # A tibble: 6 × 5
#>   date       items actual predicted  fold
#>   <date>     <fct>  <dbl>     <dbl> <int>
#> 1 2008-07-01 V10     330.      321.     1
#> 2 2008-08-01 V10     310.      304.     1
#> 3 2008-09-01 V10     326.      316.     1
#> 4 2008-10-01 V10     342.      363.     1
#> 5 2008-11-01 V10     352.      367.     1
#> 6 2008-12-01 V10     535.      518.     1
```

### Per-Step Accuracy

Analyze how accuracy degrades with forecast horizon:

``` r
# Add step number within each fold
predictions <- cv_with_preds$predictions %>%
  group_by(fold) %>%
  mutate(step = row_number()) %>%
  ungroup()

# Calculate RMSE by step
step_accuracy <- predictions %>%
  group_by(step) %>%
  summarise(
    rmse = sqrt(mean((actual - predicted)^2)),
    mae = mean(abs(actual - predicted)),
    n = n()
  )

print(step_accuracy)

# Visualize
library(ggplot2)
ggplot(step_accuracy, aes(x = step, y = rmse)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Forecast Accuracy by Horizon",
    x = "Steps Ahead",
    y = "RMSE"
  ) +
  theme_minimal()
```

### Per-Group Accuracy (Panel Data)

``` r
group_accuracy <- cv_with_preds$predictions %>%
  group_by(items) %>%
  summarise(
    rmse = sqrt(mean((actual - predicted)^2)),
    mae = mean(abs(actual - predicted)),
    n = n()
  ) %>%
  arrange(rmse)

# Best performing groups
head(group_accuracy)

# Worst performing groups
tail(group_accuracy)
```

------------------------------------------------------------------------

## Model Comparison

Compare different models or feature sets:

``` r
# Model 1: Simple
cv_simple <- cv_forecast(
  value ~ p(12),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5
)

# Model 2: With seasonality
cv_seasonal <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5
)

# Model 3: Complex
cv_complex <- cv_forecast(
  value ~ p(12) + q(12) + month() + rollsum(12),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5
)

# Compare
comparison <- data.frame(
  model = c("Simple", "Seasonal", "Complex"),
  rmse = c(
    cv_simple$metrics[cv_simple$metrics$fold == 0, "rmse"],
    cv_seasonal$metrics[cv_seasonal$metrics$fold == 0, "rmse"],
    cv_complex$metrics[cv_complex$metrics$fold == 0, "rmse"]
  )
)

print(comparison)
```

### Comparing Different ML Models

``` r
library(randomForest)

# Linear model
cv_lm <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5
)

# Random Forest
cv_rf <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = randomForest,
  h = 6,
  n_windows = 5,
  ntree = 100
)

# XGBoost (with custom spec)
xgb_spec <- list(
  fit = function(y, X, ...) {
    X_mat <- model.matrix(~ . - 1, data = X)
    xgboost::xgboost(data = X_mat, label = y, nrounds = 100, verbose = 0)
  },
  predict = function(object, newdata, ...) {
    X_mat <- model.matrix(~ . - 1, data = newdata)
    predict(object, X_mat)
  }
)

cv_xgb <- cv_forecast(
  value ~ p(12) + month(),
  data = ts_data,
  model = xgb_spec,
  h = 6,
  n_windows = 5
)
```

------------------------------------------------------------------------

## Model Selection Workflow

A recommended workflow for selecting the best model:

### 1. Establish Baseline

``` r
# Simple model as baseline
cv_baseline <- cv_forecast(
  value ~ p(12),
  data = ts_data,
  model = lm,
  h = 6,
  n_windows = 5
)

baseline_rmse <- cv_baseline$metrics %>%
  filter(fold == 0) %>%
  pull(rmse)

cat("Baseline RMSE:", baseline_rmse, "\n")
```

### 2. Feature Selection

``` r
# Test adding features incrementally
features_to_test <- list(
  base = "value ~ p(12)",
  calendar = "value ~ p(12) + month()",
  rolling = "value ~ p(12) + month() + rollsum(12)",
  trend = "value ~ p(12) + month() + rollsum(12) + trend(1)"
)

feature_results <- lapply(names(features_to_test), function(name) {
  cv <- cv_forecast(
    as.formula(features_to_test[[name]]),
    data = ts_data,
    model = lm,
    h = 6,
    n_windows = 5
  )
  data.frame(
    features = name,
    rmse = cv$metrics[cv$metrics$fold == 0, "rmse"]
  )
})

feature_comparison <- do.call(rbind, feature_results)
print(feature_comparison)
```

### 3. Model Selection

``` r
# Test different models with best features
best_formula <- value ~ p(12) + month() + rollsum(12)

models_to_test <- list(
  lm = lm,
  rf = randomForest,
  xgb = xgb_spec  # from above
)

model_results <- lapply(names(models_to_test), function(name) {
  cv <- cv_forecast(
    best_formula,
    data = ts_data,
    model = models_to_test[[name]],
    h = 6,
    n_windows = 5
  )
  data.frame(
    model = name,
    rmse = cv$metrics[cv$metrics$fold == 0, "rmse"]
  )
})

model_comparison <- do.call(rbind, model_results)
print(model_comparison)
```

### 4. Final Validation

``` r
# Hold out the last fold for final validation
# Train on all but the most recent data
# Test on most recent data only
```

------------------------------------------------------------------------

## Panel Data Considerations

### Aligned Date Sequences

[`cv_forecast()`](https://taf-society.github.io/chronofeat/reference/cv_forecast.md)
requires all groups to have identical date sequences:

``` r
# This will error if groups have different dates
cv_results <- cv_forecast(
  value ~ p(12) + month(),
  data = unbalanced_panel,  # Groups have different dates
  groups = "store",
  ...
)
# Error: Cross-validation requires all groups to have identical date sequences
```

**Solutions:**

1.  Use
    [`TimeSeries()`](https://taf-society.github.io/chronofeat/reference/TimeSeries.md)
    with `fill_time = TRUE` to complete calendars
2.  Filter to common date range across groups
3.  Perform per-group validation separately

### Per-Group Validation

For unbalanced panels, validate each group separately:

``` r
groups <- unique(ts_data$data$items)

group_cv <- lapply(groups, function(g) {
  group_data <- ts_data$data %>% filter(items == g)

  cv <- tryCatch({
    cv_forecast(
      value ~ p(12) + month(),
      data = group_data,
      date = "date",
      model = lm,
      h = 6,
      n_windows = 3
    )
  }, error = function(e) NULL)

  if (!is.null(cv)) {
    data.frame(
      group = g,
      rmse = cv$metrics[cv$metrics$fold == 0, "rmse"]
    )
  } else {
    NULL
  }
})

group_cv_results <- do.call(rbind, group_cv)
```

------------------------------------------------------------------------

## Best Practices

### 1. Match Production Horizon

``` r
# If you forecast 12 months ahead in production
cv_results <- cv_forecast(..., h = 12)
# Not h = 1 (too optimistic) or h = 24 (different task)
```

### 2. Use Enough Windows

``` r
# At least 5 windows for stable estimates
cv_results <- cv_forecast(..., n_windows = 5)

# More is better if you have enough data
cv_results <- cv_forecast(..., n_windows = 10)
```

### 3. Watch for Data Leakage

- Don’t use future information in features
- Ensure rolling windows are backward-looking
- Don’t tune on test data

### 4. Consider Computational Cost

``` r
# CV runs fit() n_windows times
# For slow models, start with fewer windows
cv_quick <- cv_forecast(..., n_windows = 3)

# Then increase for final evaluation
cv_final <- cv_forecast(..., n_windows = 10)
```

### 5. Report Variability

``` r
# Don't just report mean RMSE
# Show variability across folds
fold_rmse <- cv_results$metrics %>%
  filter(fold != 0) %>%
  pull(rmse)

cat("Mean RMSE:", mean(fold_rmse), "\n")
cat("SD RMSE:", sd(fold_rmse), "\n")
cat("Range:", min(fold_rmse), "-", max(fold_rmse), "\n")
```

------------------------------------------------------------------------

## Troubleshooting

### “Unable to create CV splits”

**Cause**: Not enough data for requested windows.

**Solution**: Reduce `n_windows` or `h`, or use more data.

### Different Results Each Run

**Cause**: Random model (e.g., Random Forest) without seed.

**Solution**: Set seed before CV:

``` r
set.seed(42)
cv_results <- cv_forecast(...)
```

### Very Different Fold Metrics

**Cause**: Non-stationarity, outliers, or regime changes.

**Solutions**:

- Use sliding window for non-stationary data
- Investigate outlier periods
- Consider different models for different regimes

### High Variability in Per-Step Accuracy

**Cause**: Different difficulty at different horizons.

**Solution**: This is expected! Short-term forecasts are usually more
accurate.

------------------------------------------------------------------------

## Summary

| Parameter            | Description                        | Default     |
|----------------------|------------------------------------|-------------|
| `h`                  | Forecast horizon                   | Required    |
| `n_windows`          | Number of CV folds                 | 5           |
| `window_type`        | “expanding” or “sliding”           | “expanding” |
| `window_size`        | Size for sliding window            | NULL        |
| `step_size`          | Steps between folds                | h           |
| `metric`             | “rmse”, “mae”, “mape”, or function | “rmse”      |
| `return_predictions` | Return individual predictions      | FALSE       |

**Key outputs:**

- `$metrics` - Per-fold and overall metrics
- `$predictions` - Individual predictions (if requested)
- `$params` - CV configuration

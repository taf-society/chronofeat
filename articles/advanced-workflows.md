# Advanced Workflows

## Overview

This article covers advanced topics for production-ready forecasting:

- Hyperparameter tuning
- Model comparison frameworks
- Production deployment patterns
- Edge cases and best practices

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

## Hyperparameter Tuning

### Grid Search with CV

Systematically search hyperparameter space:

``` r
library(xgboost)

# Define parameter grid
param_grid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3)
)

# XGBoost model specification factory
make_xgb_spec <- function(nrounds, max_depth, eta) {
  list(
    fit = function(y, X, ...) {
      X_mat <- model.matrix(~ . - 1, data = X)
      xgboost(
        data = X_mat, label = y,
        nrounds = nrounds,
        max_depth = max_depth,
        eta = eta,
        verbose = 0
      )
    },
    predict = function(object, newdata, ...) {
      X_mat <- model.matrix(~ . - 1, data = newdata)
      predict(object, X_mat)
    }
  )
}

# Run grid search
results <- lapply(1:nrow(param_grid), function(i) {
  params <- param_grid[i, ]

  cv <- cv_forecast(
    value ~ p(12) + month(),
    data = ts_data,
    model = make_xgb_spec(params$nrounds, params$max_depth, params$eta),
    h = 6,
    n_windows = 3
  )

  data.frame(
    nrounds = params$nrounds,
    max_depth = params$max_depth,
    eta = params$eta,
    rmse = cv$metrics[cv$metrics$fold == 0, "rmse"]
  )
})

grid_results <- do.call(rbind, results)
best_params <- grid_results[which.min(grid_results$rmse), ]
print(best_params)
```

### Random Search

More efficient for large parameter spaces:

``` r
set.seed(42)
n_trials <- 20

random_params <- data.frame(
  nrounds = sample(50:300, n_trials, replace = TRUE),
  max_depth = sample(3:12, n_trials, replace = TRUE),
  eta = runif(n_trials, 0.01, 0.3),
  subsample = runif(n_trials, 0.6, 1.0),
  colsample_bytree = runif(n_trials, 0.6, 1.0)
)

# Run random search (similar to grid search above)
```

### Bayesian Optimization with ParBayesianOptimization

Bayesian optimization is more efficient than grid or random search
because it uses past evaluation results to intelligently choose the next
parameters to try. The `ParBayesianOptimization` package provides an
easy-to-use interface for this.

``` r
# Install if needed
install.packages("ParBayesianOptimization")
```

#### Basic Setup

The key components are:

1.  **Scoring function**: Takes parameters, returns a score (higher =
    better, so negate RMSE)
2.  **Parameter bounds**: Define the search space
3.  **Initial points**: Random evaluations to start the optimization

``` r
library(ParBayesianOptimization)
library(xgboost)

# Prepare data once (outside the scoring function for efficiency)
ts_data <- TimeSeries(retail, date = "date", groups = "items", frequency = "month")

# Define the scoring function
# ParBayesianOptimization MAXIMIZES, so return negative RMSE
scoring_function <- function(max_depth, eta, subsample, colsample_bytree) {

  # Create XGBoost model specification with these parameters
  xgb_spec <- list(
    fit = function(y, X, ...) {
      # Convert factors to numeric
      X_mat <- model.matrix(~ . - 1, data = X)

      xgboost(
        data = X_mat,
        label = y,
        nrounds = 100,  # Fixed for speed; tune separately if needed
        max_depth = max_depth,
        eta = eta,
        subsample = subsample,
        colsample_bytree = colsample_bytree,
        objective = "reg:squarederror",
        verbose = 0
      )
    },
    predict = function(object, newdata, ...) {
      X_mat <- model.matrix(~ . - 1, data = newdata)
      predict(object, X_mat)
    }
  )

  # Run cross-validation
  cv_result <- cv_forecast(
    value ~ p(12) + month(),
    data = ts_data,
    model = xgb_spec,
    h = 6,
    n_windows = 3,
    metric = "rmse"
  )

  # Extract overall RMSE
 rmse <- cv_result$metrics[cv_result$metrics$fold == 0, "rmse"]

  # Return negative RMSE (ParBayesianOptimization maximizes)
  list(Score = -rmse)
}

# Define parameter bounds
bounds <- list(
  max_depth = c(2L, 10L),
  eta = c(0.01, 0.3),
  subsample = c(0.5, 1.0),
  colsample_bytree = c(0.5, 1.0)
)

# Run optimization
set.seed(42)
opt_result <- bayesOpt(
  FUN = scoring_function,
  bounds = bounds,
  initPoints = 5,   # Initial random evaluations
  iters.n = 15,     # Bayesian optimization iterations
  iters.k = 1,      # Points to sample per iteration
  verbose = 1
)
```

#### Analyzing Results

``` r
# Best parameters found
getBestPars(opt_result)

# Full optimization history
opt_result$scoreSummary

# Plot optimization progress
plot(opt_result)
```

#### Complete XGBoost Tuning Example

Here’s a production-ready example with more parameters:

``` r
library(ParBayesianOptimization)
library(xgboost)
library(chronofeat)
library(dplyr)

# Load and prepare data
data(retail)
ts_data <- TimeSeries(retail, date = "date", groups = "items", frequency = "month")

# Comprehensive scoring function
xgb_scoring <- function(max_depth, eta, min_child_weight, subsample,
                        colsample_bytree, gamma, lambda, alpha) {

  xgb_spec <- list(
    fit = function(y, X, ...) {
      X_mat <- model.matrix(~ . - 1, data = X)

      # Use early stopping with internal validation
      n <- length(y)
      train_idx <- 1:floor(n * 0.8)
      val_idx <- (floor(n * 0.8) + 1):n

      dtrain <- xgb.DMatrix(data = X_mat[train_idx, ], label = y[train_idx])
      dval <- xgb.DMatrix(data = X_mat[val_idx, ], label = y[val_idx])

      xgb.train(
        params = list(
          objective = "reg:squarederror",
          eval_metric = "rmse",
          max_depth = max_depth,
          eta = eta,
          min_child_weight = min_child_weight,
          subsample = subsample,
          colsample_bytree = colsample_bytree,
          gamma = gamma,
          lambda = lambda,
          alpha = alpha
        ),
        data = dtrain,
        nrounds = 500,
        watchlist = list(val = dval),
        early_stopping_rounds = 20,
        verbose = 0
      )
    },
    predict = function(object, newdata, ...) {
      X_mat <- model.matrix(~ . - 1, data = newdata)
      predict(object, X_mat)
    }
  )

  cv_result <- cv_forecast(
    value ~ p(12) + q(12) + month(),
    data = ts_data,
    model = xgb_spec,
    h = 6,
    n_windows = 3,
    metric = "rmse"
  )

  rmse <- cv_result$metrics[cv_result$metrics$fold == 0, "rmse"]
  list(Score = -rmse)
}

# Comprehensive parameter bounds
bounds <- list(
  max_depth = c(2L, 12L),
  eta = c(0.01, 0.3),
  min_child_weight = c(1L, 10L),
  subsample = c(0.5, 1.0),
  colsample_bytree = c(0.3, 1.0),
  gamma = c(0, 5),
  lambda = c(0, 3),    # L2 regularization
  alpha = c(0, 3)      # L1 regularization
)

# Run optimization with more iterations
set.seed(123)
opt_result <- bayesOpt(
  FUN = xgb_scoring,
  bounds = bounds,
  initPoints = 10,    # More initial points for higher dimensions
  iters.n = 30,       # More iterations
  iters.k = 1,
  acq = "ucb",        # Upper Confidence Bound acquisition function
  kappa = 2.576,      # Exploration parameter
  verbose = 1
)

# Get best parameters
best_params <- getBestPars(opt_result)
print(best_params)

# Best score achieved (remember to negate back to RMSE)
best_rmse <- -max(opt_result$scoreSummary$Score)
cat("Best RMSE:", best_rmse, "\n")
```

#### Tuning LightGBM with ParBayesianOptimization

``` r
library(lightgbm)

lgb_scoring <- function(num_leaves, learning_rate, feature_fraction,
                        bagging_fraction, min_data_in_leaf, lambda_l1, lambda_l2) {

  lgb_spec <- list(
    fit = function(y, X, ...) {
      # LightGBM handles categoricals natively
      cat_cols <- names(X)[sapply(X, is.factor)]
      X_processed <- X
      for (col in cat_cols) {
        X_processed[[col]] <- as.integer(X[[col]]) - 1L
      }

      dtrain <- lgb.Dataset(
        data = as.matrix(X_processed),
        label = y,
        categorical_feature = cat_cols
      )

      lgb.train(
        params = list(
          objective = "regression",
          metric = "rmse",
          num_leaves = num_leaves,
          learning_rate = learning_rate,
          feature_fraction = feature_fraction,
          bagging_fraction = bagging_fraction,
          bagging_freq = 1,
          min_data_in_leaf = min_data_in_leaf,
          lambda_l1 = lambda_l1,
          lambda_l2 = lambda_l2,
          verbosity = -1
        ),
        data = dtrain,
        nrounds = 200
      )
    },
    predict = function(object, newdata, ...) {
      cat_cols <- names(newdata)[sapply(newdata, is.factor)]
      X_processed <- newdata
      for (col in cat_cols) {
        X_processed[[col]] <- as.integer(newdata[[col]]) - 1L
      }
      predict(object, as.matrix(X_processed))
    }
  )

  cv_result <- cv_forecast(
    value ~ p(12) + month(),
    data = ts_data,
    model = lgb_spec,
    h = 6,
    n_windows = 3
  )

  rmse <- cv_result$metrics[cv_result$metrics$fold == 0, "rmse"]
  list(Score = -rmse)
}

bounds_lgb <- list(
  num_leaves = c(8L, 128L),
  learning_rate = c(0.01, 0.3),
  feature_fraction = c(0.5, 1.0),
  bagging_fraction = c(0.5, 1.0),
  min_data_in_leaf = c(5L, 50L),
  lambda_l1 = c(0, 3),
  lambda_l2 = c(0, 3)
)

opt_lgb <- bayesOpt(
  FUN = lgb_scoring,
  bounds = bounds_lgb,
  initPoints = 8,
  iters.n = 20,
  verbose = 1
)
```

#### Parallel Bayesian Optimization

For faster tuning, use parallel evaluation:

``` r
library(doParallel)

# Setup parallel backend
cl <- makeCluster(4)
registerDoParallel(cl)

# Export required objects to workers
clusterExport(cl, c("ts_data", "cv_forecast", "fit", "forecast"))
clusterEvalQ(cl, {
  library(chronofeat)
  library(xgboost)
  library(dplyr)
})

# Run parallel optimization
opt_parallel <- bayesOpt(
  FUN = scoring_function,
  bounds = bounds,
  initPoints = 8,
  iters.n = 20,
  iters.k = 4,       # Evaluate 4 points in parallel per iteration
  parallel = TRUE,
  verbose = 1
)

stopCluster(cl)
```

#### Tips for Effective Bayesian Optimization

1.  **Start with reasonable bounds**: Too wide bounds waste evaluations
2.  **Use enough initial points**: At least 2× number of parameters
3.  **Monitor convergence**: Plot scores to see if more iterations help
4.  **Fix some parameters**: Tune most impactful parameters first (eta,
    max_depth)
5.  **Use early stopping**: Reduces time per evaluation

``` r
# Check if optimization has converged
scores <- opt_result$scoreSummary$Score
plot(scores, type = "l", main = "Optimization Progress",
     xlab = "Iteration", ylab = "Score (negative RMSE)")

# If scores are still improving, run more iterations
opt_result <- addIterations(opt_result, iters.n = 10, verbose = 1)
```

#### Using Optimized Parameters

After finding the best parameters, train your final model:

``` r
# Get best parameters
best <- getBestPars(opt_result)

# Create final model specification
final_xgb_spec <- list(
  fit = function(y, X, ...) {
    X_mat <- model.matrix(~ . - 1, data = X)
    xgboost(
      data = X_mat,
      label = y,
      nrounds = 200,
      max_depth = best$max_depth,
      eta = best$eta,
      subsample = best$subsample,
      colsample_bytree = best$colsample_bytree,
      verbose = 0
    )
  },
  predict = function(object, newdata, ...) {
    X_mat <- model.matrix(~ . - 1, data = newdata)
    predict(object, X_mat)
  }
)

# Fit final model on all data
final_model <- fit(
  value ~ p(12) + q(12) + month(),
  data = ts_data,
  model = final_xgb_spec
)

# Generate forecasts
forecasts <- forecast(final_model, h = 12)
```

------------------------------------------------------------------------

## Model Comparison Framework

### Structured Comparison

``` r
# Define models to compare
models <- list(
  lm = list(
    spec = lm,
    features = "value ~ p(12) + month()"
  ),
  lm_complex = list(
    spec = lm,
    features = "value ~ p(12) + q(12) + month() + rollsum(12)"
  ),
  rf = list(
    spec = randomForest::randomForest,
    features = "value ~ p(12) + month()"
  ),
  xgb = list(
    spec = make_xgb_spec(100, 6, 0.1),
    features = "value ~ p(12) + month()"
  )
)

# Run comparison
comparison <- lapply(names(models), function(name) {
  m <- models[[name]]

  cv <- cv_forecast(
    as.formula(m$features),
    data = ts_data,
    model = m$spec,
    h = 6,
    n_windows = 5,
    return_predictions = TRUE
  )

  fold_metrics <- cv$metrics %>%
    filter(fold != 0) %>%
    pull(rmse)

  data.frame(
    model = name,
    rmse_mean = mean(fold_metrics),
    rmse_sd = sd(fold_metrics),
    rmse_min = min(fold_metrics),
    rmse_max = max(fold_metrics)
  )
})

comparison_df <- do.call(rbind, comparison)
print(comparison_df)
```

### Statistical Comparison

Test if differences are significant:

``` r
# Collect fold-level metrics for each model
fold_metrics_by_model <- list()

for (name in names(models)) {
  cv <- cv_forecast(
    as.formula(models[[name]]$features),
    data = ts_data,
    model = models[[name]]$spec,
    h = 6,
    n_windows = 10
  )

  fold_metrics_by_model[[name]] <- cv$metrics %>%
    filter(fold != 0) %>%
    pull(rmse)
}

# Paired t-test between models
t.test(
  fold_metrics_by_model$lm,
  fold_metrics_by_model$xgb,
  paired = TRUE
)

# Friedman test for multiple models
fold_matrix <- do.call(cbind, fold_metrics_by_model)
friedman.test(fold_matrix)
```

------------------------------------------------------------------------

## Production Deployment

### Model Serialization

``` r
# Fit final model
final_model <- fit(
  value ~ p(12) + month(),
  data = ts_data,
  model = lm
)

# Save model
saveRDS(final_model, "forecast_model.rds")

# Load model
loaded_model <- readRDS("forecast_model.rds")

# Generate forecasts
fc <- forecast(loaded_model, h = 12)
```

### Versioned Model Storage

``` r
save_model <- function(model, name, version = NULL) {
  if (is.null(version)) {
    version <- format(Sys.time(), "%Y%m%d_%H%M%S")
  }

  metadata <- list(
    name = name,
    version = version,
    created = Sys.time(),
    formula = deparse(model$spec$formula),
    predictors = model$predictors,
    n_train = nrow(model$data)
  )

  filename <- sprintf("models/%s_%s.rds", name, version)
  saveRDS(list(model = model, metadata = metadata), filename)

  cat("Saved:", filename, "\n")
  invisible(filename)
}

load_model <- function(path) {
  obj <- readRDS(path)
  cat("Loaded model:", obj$metadata$name, "\n")
  cat("Version:", obj$metadata$version, "\n")
  cat("Created:", as.character(obj$metadata$created), "\n")
  obj$model
}
```

### Batch Forecasting

``` r
batch_forecast <- function(model, new_data, h, output_dir) {
  # Create TimeSeries from new data
  ts_new <- TimeSeries(
    new_data,
    date = model$meta$date,
    groups = model$meta$groups,
    frequency = model$meta$frequency
  )

  # Update model with new history
  updated_model <- fit(
    formula = model$spec$formula_obj,
    data = ts_new,
    model = model$model_spec
  )

  # Generate forecasts
  fc <- forecast(updated_model, h = h)

  # Save results
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(output_dir, sprintf("forecast_%s.csv", timestamp))
  write.csv(fc, filename, row.names = FALSE)

  cat("Forecasts saved to:", filename, "\n")
  fc
}
```

### Monitoring and Alerts

``` r
monitor_forecast_accuracy <- function(forecasts, actuals, threshold = 0.2) {
  # Join forecasts with actuals
  eval_data <- forecasts %>%
    inner_join(actuals, by = c("date", "items")) %>%
    mutate(
      error = actual - predicted,
      abs_pct_error = abs(error) / abs(actual)
    )

  # Calculate metrics
  metrics <- eval_data %>%
    summarise(
      rmse = sqrt(mean(error^2)),
      mape = mean(abs_pct_error) * 100,
      max_error = max(abs(error)),
      n_large_errors = sum(abs_pct_error > threshold)
    )

  # Alert if needed
  if (metrics$mape > 20 || metrics$n_large_errors > 5) {
    warning("Forecast quality degradation detected!")
    # Send alert (email, Slack, etc.)
  }

  metrics
}
```

------------------------------------------------------------------------

## Edge Cases and Best Practices

### Handling New Groups (Cold Start)

When a new group appears without history:

``` r
# Option 1: Use global model (ignore groups)
global_model <- fit(
  value ~ p(12) + month(),
  data = ts_data,
  date = "date",
  groups = NULL,  # Train on pooled data
  model = lm
)

# Option 2: Borrow from similar groups
# Find similar groups based on recent behavior
find_similar_groups <- function(new_group_data, historical_data, n_similar = 5) {
  # Calculate group summaries
  group_stats <- historical_data %>%
    group_by(items) %>%
    summarise(
      mean_value = mean(value),
      trend = coef(lm(value ~ seq_along(value)))[2]
    )

  # Find closest groups
  new_stats <- new_group_data %>%
    summarise(
      mean_value = mean(value),
      trend = coef(lm(value ~ seq_along(value)))[2]
    )

  group_stats %>%
    mutate(distance = sqrt(
      (mean_value - new_stats$mean_value)^2 +
      (trend - new_stats$trend)^2
    )) %>%
    arrange(distance) %>%
    head(n_similar) %>%
    pull(items)
}
```

### Handling Missing Future Exogenous Variables

``` r
# Option 1: Carry forward
fc <- forecast(model, h = 12, xreg_strategy = "carry")

# Option 2: Use scenario analysis
scenarios <- list(
  optimistic = data.frame(
    date = future_dates,
    price = 9.99,
    promo = 1
  ),
  pessimistic = data.frame(
    date = future_dates,
    price = 12.99,
    promo = 0
  ),
  baseline = data.frame(
    date = future_dates,
    price = 10.99,
    promo = 0.5
  )
)

scenario_forecasts <- lapply(names(scenarios), function(name) {
  fc <- forecast(model, future = scenarios[[name]])
  fc$scenario <- name
  fc
})

all_scenarios <- do.call(rbind, scenario_forecasts)
```

### Dealing with Structural Breaks

When patterns change dramatically:

``` r
# Detect potential break points
detect_breaks <- function(ts_data, target_col) {
  library(strucchange)
  y <- ts_data[[target_col]]
  bp <- breakpoints(y ~ 1)
  breakdates(bp)
}

# Option 1: Only use data after break
recent_data <- ts_data %>%
  filter(date >= break_date)

# Option 2: Add regime indicator
ts_data <- ts_data %>%
  mutate(regime = ifelse(date >= break_date, "new", "old"))

# Include regime in model
model <- fit(
  value ~ p(12) + month() + regime,
  data = ts_data,
  model = lm
)
```

### Intermittent Demand (Many Zeros)

For data with many zero values:

``` r
# Croston's method components
ts_data <- ts_data %>%
  mutate(
    demand_occurred = value > 0,
    demand_size = ifelse(demand_occurred, value, NA)
  )

# Model demand occurrence (classification)
occurrence_model <- fit(
  demand_occurred ~ p(12) + dow() + month(),
  data = ts_data,
  model = glm,
  family = binomial()
)

# Model demand size (regression on non-zero)
size_model <- fit(
  demand_size ~ p(12) + dow() + month(),
  data = ts_data %>% filter(demand_occurred),
  model = lm
)

# Combine predictions
forecast_intermittent <- function(occurrence_model, size_model, h) {
  # Forecast probability
  fc_prob <- forecast(occurrence_model, h = h)

  # Forecast size
  fc_size <- forecast(size_model, h = h)

  # Combined forecast = probability × expected size
  fc_prob$demand_forecast <- fc_prob$demand_occurred_forecast * fc_size$demand_size_forecast
  fc_prob
}
```

### Very Long Horizons

For horizons longer than seasonal periods:

``` r
# Option 1: Direct forecasting (train separate models)
train_direct_models <- function(ts_data, max_horizon = 24) {
  models <- list()

  for (h in 1:max_horizon) {
    # Create target shifted by h periods
    data_h <- ts_data %>%
      group_by(items) %>%
      mutate(target_h = lead(value, h)) %>%
      ungroup() %>%
      filter(!is.na(target_h))

    models[[h]] <- fit(
      target_h ~ p(12) + month(),
      data = data_h,
      date = "date",
      groups = "items",
      model = lm
    )
  }

  models
}

# Option 2: Ensemble recursive + direct
forecast_ensemble <- function(recursive_fc, direct_fc, weights = c(0.5, 0.5)) {
  weights[1] * recursive_fc + weights[2] * direct_fc
}
```

------------------------------------------------------------------------

## Performance Optimization

### Reducing Computation Time

``` r
# 1. Use C++ path (default when no exogenous variables)
fc <- forecast(model, h = 12, use_cpp = TRUE, verbose = TRUE)

# 2. Reduce features
# Fewer features = faster fit and predict
model_fast <- fit(value ~ p(3) + month(), data = ts_data, model = lm)

# 3. Use faster models
# ranger instead of randomForest
# lightgbm instead of xgboost for many categories

# 4. Parallelize CV
library(future)
library(future.apply)
plan(multisession, workers = 4)

cv_parallel <- function(formula, data, models, h, n_windows) {
  future_lapply(models, function(m) {
    cv_forecast(formula, data = data, model = m, h = h, n_windows = n_windows)
  })
}
```

### Memory Management

``` r
# 1. Don't store predictions if not needed
cv <- cv_forecast(..., return_predictions = FALSE)

# 2. Process groups in batches
process_in_batches <- function(ts_data, batch_size = 10) {
  groups <- unique(ts_data$data[[ts_data$groups]])
  n_batches <- ceiling(length(groups) / batch_size)

  results <- list()
  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(groups))
    batch_groups <- groups[start_idx:end_idx]

    batch_data <- ts_data$data %>%
      filter(.data[[ts_data$groups]] %in% batch_groups)

    # Process batch
    results[[i]] <- process_batch(batch_data)

    # Clear memory
    gc()
  }

  do.call(rbind, results)
}
```

------------------------------------------------------------------------

## Summary Checklist

### Before Fitting

Data sorted by groups and date

No trailing NAs in target

All factor levels present in training

Sufficient history for lag orders

Frequency correctly specified

### Model Selection

Cross-validated with appropriate horizon

Compared multiple models

Checked per-step accuracy degradation

Statistical significance of improvements

Simple model as baseline

### Production

Model serialization tested

Version tracking in place

Monitoring pipeline ready

Edge cases handled (new groups, missing data)

Retraining schedule defined

### Performance

Computation time acceptable

Memory usage monitored

Parallelization considered for CV

C++ path enabled where possible

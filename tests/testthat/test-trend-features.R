# Test trend features in forecasting
# Ensures trend index advances correctly

test_that("trend features advance correctly during forecasting", {
  skip_if_not_installed("dplyr")

  # Create simple time series
  df <- data.frame(
    date = as.Date("2023-01-01") + 0:99,
    value = 10 + (1:100) * 0.5 + rnorm(100, 0, 2)
  )

  # Fit model with trend
  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(3) + trend(1), data = ts_obj, model = test_model_lm())

  # Generate 5-step forecast
  fc <- forecast(m, h = 5)

  # The model should have been trained on rows 4-100 (after removing NAs from lags)
  # So training trend values were 4, 5, ..., 100
  # Forecast trend should be 101, 102, 103, 104, 105

  # We can't directly access trend values in forecast output,
  # but we can verify the forecast advances properly
  expect_equal(nrow(fc), 5)
  expect_true(all(!is.na(fc$value_forecast)))

  # Check that predictions are monotonic (since we have positive trend)
  # This is a proxy test - if trend was stuck, forecasts would plateau
  diffs <- diff(fc$value_forecast)
  # At least some differences should be positive (trend effect)
  expect_true(mean(diffs) > 0)
})

test_that("trend features in .make_target_feats start at length(y) + 1", {
  # Direct unit test of the helper function
  y <- 1:100

  # Call .make_target_feats
  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    trend_degrees = c(1, 2)
  )

  # Trend should be 101 (next step), not 100 (current length)
  expect_equal(feats$trend1, 101)
  expect_equal(feats$trend2, 101^2)
})

test_that("trend features work correctly with grouped data", {
  skip_if_not_installed("dplyr")

  # Two groups with different lengths
  df <- data.frame(
    series = c(rep("A", 50), rep("B", 70)),
    date = c(as.Date("2023-01-01") + 0:49, as.Date("2023-01-01") + 0:69),
    value = c(1:50 + rnorm(50), 1:70 + rnorm(70))
  )

  ts_obj <- TimeSeries(df, date = "date", groups = "series", frequency = "day")
  m <- fit(value ~ p(2) + trend(1), data = ts_obj, model = test_model_lm())

  # Forecast
  fc <- forecast(m, h = 3)

  # Should have 3 forecasts per group
  expect_equal(nrow(fc), 6)
  expect_true(all(!is.na(fc$value_forecast)))

  # Each group should have 3 rows
  fc_counts <- table(fc$series)
  expect_equal(as.numeric(fc_counts["A"]), 3)
  expect_equal(as.numeric(fc_counts["B"]), 3)
})

# Test lag specification in p() parameter
# Tests the three supported forms:
#   p(12)           - single lag only
#   p(1:12)         - range of lags
#   p(c(1, 4, 6, 12)) - explicit set of lags

test_that("p(12) creates only lag_12 (single lag)", {
  skip_if_not_installed("dplyr")

  set.seed(123)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = cumsum(rnorm(100, 1, 5))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with p(12) - should create only lag_12

m <- fit(value ~ p(12), data = ts_obj, model = test_model_lm())

  # Check that only lag_12 is in the predictors
  expect_true("value_lag_12" %in% m$predictors)
  expect_equal(length(m$predictors), 1)
  expect_false("value_lag_1" %in% m$predictors)
  expect_false("value_lag_6" %in% m$predictors)
  expect_false("value_lag_11" %in% m$predictors)

  # Verify the spec stored the correct lag indices
  expect_equal(m$spec$p, 12L)
})

test_that("p(1:12) creates lags 1 through 12 (range)", {
  skip_if_not_installed("dplyr")

  set.seed(123)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = cumsum(rnorm(100, 1, 5))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with p(1:12) - should create lag_1 through lag_12
  m <- fit(value ~ p(1:12), data = ts_obj, model = test_model_lm())

  # Check that all lags 1-12 are in the predictors
  expected_lags <- paste0("value_lag_", 1:12)
  for (lag_name in expected_lags) {
    expect_true(lag_name %in% m$predictors, info = paste("Missing:", lag_name))
  }
  expect_equal(length(m$predictors), 12)

  # Verify the spec stored the correct lag indices
  expect_equal(m$spec$p, 1:12)
})

test_that("p(c(1, 4, 6, 12)) creates only specified lags (explicit set)", {
  skip_if_not_installed("dplyr")

  set.seed(123)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = cumsum(rnorm(100, 1, 5))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with explicit lag set
  m <- fit(value ~ p(c(1, 4, 6, 12)), data = ts_obj, model = test_model_lm())

  # Check that only specified lags are in the predictors
  expected_lags <- paste0("value_lag_", c(1, 4, 6, 12))
  for (lag_name in expected_lags) {
    expect_true(lag_name %in% m$predictors, info = paste("Missing:", lag_name))
  }
  expect_equal(length(m$predictors), 4)

  # Check that non-specified lags are NOT included
  expect_false("value_lag_2" %in% m$predictors)
  expect_false("value_lag_3" %in% m$predictors)
  expect_false("value_lag_5" %in% m$predictors)
  expect_false("value_lag_7" %in% m$predictors)

  # Verify the spec stored the correct lag indices (sorted)
  expect_equal(m$spec$p, c(1L, 4L, 6L, 12L))
})

test_that("feat_lag_ma_dt creates correct lags for explicit set", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2020-01-01") + 0:9,
    value = 1:10
  )

  # Test with explicit lag set c(1, 3, 5)
  result <- feat_lag_ma_dt(df, date = "date", target = "value", p = c(1, 3, 5))

  # Check that correct columns were created
  expect_true("value_lag_1" %in% names(result))
  expect_true("value_lag_3" %in% names(result))
  expect_true("value_lag_5" %in% names(result))

  # Check that lag_2, lag_4 are NOT created
  expect_false("value_lag_2" %in% names(result))
  expect_false("value_lag_4" %in% names(result))

  # Verify lag values are correct
  # For row 6 (value=6): lag_1=5, lag_3=3, lag_5=1
  row6 <- result[6, ]
  expect_equal(row6$value_lag_1, 5)
  expect_equal(row6$value_lag_3, 3)
  expect_equal(row6$value_lag_5, 1)
})

test_that(".make_target_feats computes lags correctly for explicit set", {
  # Test the internal function directly
  y <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

  # Test with explicit lag set c(1, 4, 6, 12)
  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    p = c(1, 4, 6, 12)
  )

  # lag_1 should be most recent: 100
  expect_equal(feats$value_lag_1, 100)

  # lag_4 should be 4 steps back: 70
  expect_equal(feats$value_lag_4, 70)

  # lag_6 should be 6 steps back: 50
  expect_equal(feats$value_lag_6, 50)

  # lag_12 should be NA (not enough history)
  expect_true(is.na(feats$value_lag_12))

  # Check that only the requested lags are present
  expect_equal(length(feats), 4)
  expect_true(all(names(feats) %in% c("value_lag_1", "value_lag_4", "value_lag_6", "value_lag_12")))
})

test_that("forecast works correctly with explicit lag set", {
  skip_if_not_installed("dplyr")

  set.seed(456)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = cumsum(rnorm(100, 1, 5))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with explicit lag set
  m <- fit(value ~ p(c(1, 4, 6, 12)), data = ts_obj, model = test_model_lm())

  # Forecast should work
  fc <- forecast(m, h = 10)

  expect_equal(nrow(fc), 10)
  expect_true(all(!is.na(fc$value_forecast)))
})

test_that("R and C++ forecasting paths match with explicit lag set", {
  skip_if_not_installed("dplyr")

  set.seed(789)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = cumsum(rnorm(100, 1, 5))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(c(1, 4, 6, 12)), data = ts_obj, model = test_model_lm())

  # C++ path
  fc_cpp <- forecast(m, h = 10, use_cpp = TRUE)

  # R path
  fc_r <- forecast(m, h = 10, use_cpp = FALSE)

  # Should be identical (or very close due to floating point)
  expect_equal(fc_cpp$value_forecast, fc_r$value_forecast, tolerance = 1e-10)
})

test_that("grouped forecast works with explicit lag set", {
  skip_if_not_installed("dplyr")

  set.seed(321)
  df <- data.frame(
    series = rep(c("A", "B"), each = 50),
    date = rep(seq(as.Date("2020-01-01"), by = "day", length.out = 50), 2),
    value = cumsum(rnorm(100, 1, 3))
  )

  ts_obj <- TimeSeries(df, date = "date", groups = "series", frequency = "day")
  m <- fit(value ~ p(c(1, 7, 14)), data = ts_obj, model = test_model_lm())

  # Forecast with C++
  fc_cpp <- forecast(m, h = 5, use_cpp = TRUE)

  # Forecast with R
  fc_r <- forecast(m, h = 5, use_cpp = FALSE)

  # Sort for comparison
  fc_cpp <- fc_cpp %>% dplyr::arrange(series, date)
  fc_r <- fc_r %>% dplyr::arrange(series, date)

  expect_equal(fc_cpp$value_forecast, fc_r$value_forecast, tolerance = 1e-10)
})

test_that("p() rejects invalid inputs", {
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:9,
    value = 1:10
  )

  # Zero lag should fail
  expect_error(
    fit(value ~ p(0), data = df, date = "date", model = test_model_lm()),
    "positive integers"
  )

  # Negative lag should fail
  expect_error(
    fit(value ~ p(-1), data = df, date = "date", model = test_model_lm()),
    "positive integers"
  )

  # Empty vector should fail
  expect_error(
    fit(value ~ p(c()), data = df, date = "date", model = test_model_lm()),
    "positive integers"
  )
})

test_that("single lag 1 works correctly", {
  skip_if_not_installed("dplyr")

  set.seed(111)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 50),
    value = cumsum(rnorm(50, 1, 2))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with single lag 1
  m <- fit(value ~ p(1), data = ts_obj, model = test_model_lm())

  expect_true("value_lag_1" %in% m$predictors)
  expect_equal(length(m$predictors), 1)
  expect_equal(m$spec$p, 1L)

  # Forecast should work
  fc <- forecast(m, h = 5)
  expect_equal(nrow(fc), 5)
})

test_that("high single lag works correctly", {
  skip_if_not_installed("dplyr")

  set.seed(222)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 200),
    value = cumsum(rnorm(200, 1, 2))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with single high lag (e.g., seasonal lag 365 days - but we use 50 for test)
  m <- fit(value ~ p(50), data = ts_obj, model = test_model_lm())

  expect_true("value_lag_50" %in% m$predictors)
  expect_equal(length(m$predictors), 1)
  expect_equal(m$spec$p, 50L)

  # Forecast should work
  fc <- forecast(m, h = 5)
  expect_equal(nrow(fc), 5)
})

test_that("duplicate lags in explicit set are handled (deduplicated)", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2020-01-01") + 0:20,
    value = 1:21
  )

  # Fit with duplicates - should be deduplicated to c(1, 4, 12)
  m <- fit(value ~ p(c(1, 4, 4, 12, 1)), data = df, date = "date", model = test_model_lm())

  expect_equal(m$spec$p, c(1L, 4L, 12L))
  expect_equal(length(m$predictors), 3)
})

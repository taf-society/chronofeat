# Test recursive forecasting lag correctness
# Ensures lags are computed correctly in .make_target_feats()

test_that("lag calculation is correct in .make_target_feats", {
  # Test vector: y = c(10, 20, 30, 40, 50)
  y <- c(10, 20, 30, 40, 50)

  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    p = 3
  )

  # lag_1 should be the MOST RECENT value: 50
  expect_equal(feats$value_lag_1, 50)

  # lag_2 should be one step back: 40
  expect_equal(feats$value_lag_2, 40)

  # lag_3 should be two steps back: 30
  expect_equal(feats$value_lag_3, 30)
})

test_that("lag calculation with exact-size history", {
  # Edge case: length(y) == lag requested
  y <- c(10, 20, 30)

  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    p = 3
  )

  # Should work when length(y) == L (not require length(y) > L)
  expect_equal(feats$value_lag_1, 30)
  expect_equal(feats$value_lag_2, 20)
  expect_equal(feats$value_lag_3, 10)
})

test_that("lag calculation with insufficient history", {
  # length(y) < lag requested → should return NA
  y <- c(10, 20)

  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    p = 3
  )

  expect_equal(feats$value_lag_1, 20)
  expect_equal(feats$value_lag_2, 10)
  expect_true(is.na(feats$value_lag_3))  # Not enough history
})

test_that("R vs C++ forecast paths give identical results", {
  skip_if_not_installed("dplyr")

  set.seed(123)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = cumsum(rnorm(100, 1, 5))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(3) + trend(1), data = ts_obj, model = test_model_lm())

  # Forecast with C++ (default)
  fc_cpp <- forecast(m, h = 10, use_cpp = TRUE)

  # Forecast with R fallback
  fc_r <- forecast(m, h = 10, use_cpp = FALSE)

  # Should be identical (or very close due to floating point)
  expect_equal(fc_cpp$value_forecast, fc_r$value_forecast, tolerance = 1e-10)
})

test_that("grouped R vs C++ forecast paths match", {
  skip_if_not_installed("dplyr")

  set.seed(123)
  df <- data.frame(
    series = rep(c("A", "B"), each = 50),
    date = rep(seq(as.Date("2020-01-01"), by = "day", length.out = 50), 2),
    value = cumsum(rnorm(100, 1, 3))
  )

  ts_obj <- TimeSeries(df, date = "date", groups = "series", frequency = "day")
  m <- fit(value ~ p(2), data = ts_obj, model = test_model_lm())

  # C++ path
  fc_cpp <- forecast(m, h = 5, use_cpp = TRUE)

  # R fallback path
  fc_r <- forecast(m, h = 5, use_cpp = FALSE)

  # Sort both for comparison
  fc_cpp <- fc_cpp %>% arrange(series, date)
  fc_r <- fc_r %>% arrange(series, date)

  expect_equal(fc_cpp$value_forecast, fc_r$value_forecast, tolerance = 1e-10)
})

test_that("forecast with calendar features R vs C++ match", {
  skip_if_not_installed("dplyr")

  set.seed(456)
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 100),
    value = 100 + 10 * sin(2 * pi * (1:100) / 7) + rnorm(100, 0, 3)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(3) + dow(), data = ts_obj, model = test_model_lm())

  # C++ path (supports calendar now)
  fc_cpp <- forecast(m, h = 14, use_cpp = TRUE)

  # R path
  fc_r <- forecast(m, h = 14, use_cpp = FALSE)

  expect_equal(fc_cpp$value_forecast, fc_r$value_forecast, tolerance = 1e-10)
})

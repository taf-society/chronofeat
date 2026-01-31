# Test training/forecast window mismatch fix
# Ensures forecasting returns NA for incomplete windows (matching training)

test_that("forecasting returns NA for incomplete rolling windows", {
  skip_if_not_installed("dplyr")

  set.seed(123)
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:100,
    value = cumsum(rnorm(101, 1, 2))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Model with rolling sum that has 10-day window
  m <- fit(value ~ p(2) + rollsum(10),
           data = ts_obj,
           model = test_model_lm())

  # Training filters rows with NA features
  # p(2) creates lag_1, lag_2 -> rows 1-2 have NA
  # rollsum(10) needs window of 10 -> rows 1-9 have incomplete window (NA)
  # Max filter: 9 rows
  # Row 10 is first complete (has lag_1=row9, lag_2=row8, rollsum of rows 1-10)
  expect_equal(nrow(m$data), 101 - 9,
               info = "Training should filter incomplete window rows")

  # Forecast - early steps will have rollsum NA (incomplete)
  # This is tested via the helper function directly
  feats_step3 <- chronofeat:::.make_target_feats(
    y = c(100, 105, 110),  # 3 predictions so far
    target_col = "value",
    roll_windows = 10,
    roll_stats = "sum"
  )

  # rollsum(10) with only 3 values → should be NA (matching training)
  expect_true(is.na(feats_step3$value_rollsum_10),
              info = "Incomplete rolling window should return NA")
})

test_that("training keeps partial MAs but filters them out", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2020-01-01") + 0:20,
    value = 1:21 + rnorm(21, 0, 0.5)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Model with MA (MAs are critical - require full window)
  m <- fit(value ~ p(1) + q(7),
           data = ts_obj,
           model = test_model_lm())

  # p(1) creates lag_1 -> row 1 has NA
  # q(7) creates MA with window 7 using .complete=TRUE -> rows 1-6 have NA
  # Max filter: 6 rows
  # Row 7 is first complete (has lag_1=row6, MA7 of rows 1-7)
  expect_equal(nrow(m$data), 21 - 6,
               info = "Training should filter partial MA rows")
})

test_that("forecast with complete rolling windows works correctly", {
  skip_if_not_installed("dplyr")

  set.seed(456)
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:100,
    value = 100 + cumsum(rnorm(101, 0.5, 3))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Model with rolling features (window = 7)
  m <- fit(value ~ p(2) + rollsum(7),
           data = ts_obj,
           model = test_model_lm())

  # Forecast h > window size, so later steps will have complete windows
  fc <- forecast(m, h = 10)

  # Steps 8-10 should have complete rollsum windows
  # (We can't directly test this, but verify forecasts are reasonable)
  expect_true(all(!is.na(fc$value_forecast)),
              info = "All forecasts should be valid")
})

test_that("model with only lags still filters correctly", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2020-01-01") + 0:50,
    value = 1:51 + rnorm(51)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Model with only lags (no rolling)
  m <- fit(value ~ p(5),
           data = ts_obj,
           model = test_model_lm())

  # Should filter first 5 rows (lags)
  expect_equal(nrow(m$data), 51 - 5,
               info = "Training should filter lag rows only")
})

test_that("model with rolling but no lags filters incomplete windows", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2020-01-01") + 0:50,
    value = cumsum(rnorm(51, 1, 2))
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Model with rolling stats but no lags
  m <- fit(value ~ rollsum(10) + trend(1),
           data = ts_obj,
           model = test_model_lm())

  # Should filter rows with incomplete windows
  # Rows 1-9: rollsum NA (incomplete)
  # Rows 10-51: rollsum complete
  expect_equal(nrow(m$data), 51 - 9,
               info = "Training should filter incomplete rolling windows")
})

test_that("incomplete window check in .make_target_feats", {
  # Direct unit test of the fix
  y <- c(10, 20, 30)  # Only 3 values

  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    roll_windows = c(5, 7),
    roll_stats = c("sum", "sd", "min", "max")
  )

  # Window size 5 with only 3 values → all stats should be NA
  expect_true(is.na(feats$value_rollsum_5))
  expect_true(is.na(feats$value_rollsd_5))
  expect_true(is.na(feats$value_rollmin_5))
  expect_true(is.na(feats$value_rollmax_5))

  # Window size 7 → also NA
  expect_true(is.na(feats$value_rollsum_7))
  expect_true(is.na(feats$value_rollsd_7))
})

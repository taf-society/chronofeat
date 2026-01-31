# Test forecasting with exogenous regressors
# Ensures future_xreg is handled correctly

test_that("basic forecast without xreg works", {
  skip_if_not_installed("dplyr")

  # Simple data without exogenous variables
  df <- data.frame(
    date = as.Date("2023-01-01") + 0:99,
    value = 10 + (1:100) * 0.5 + rnorm(100, 0, 2)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(3), data = ts_obj, model = test_model_lm())

  # Forecast
  fc <- forecast(m, h = 5)

  expect_equal(nrow(fc), 5)
  expect_true(all(!is.na(fc$value_forecast)))
  expect_true("date" %in% names(fc))
  expect_true("value_forecast" %in% names(fc))
})

test_that("grouped forecast works correctly", {
  skip_if_not_installed("dplyr")

  # Two groups
  df <- data.frame(
    series = rep(c("A", "B"), each = 50),
    date = rep(as.Date("2023-01-01") + 0:49, 2),
    value = c(1:50 + rnorm(50), 51:100 + rnorm(50))
  )

  ts_obj <- TimeSeries(df, date = "date", groups = "series", frequency = "day")
  m <- fit(value ~ p(2), data = ts_obj, model = test_model_lm())

  # Forecast
  fc <- forecast(m, h = 5)

  expect_equal(nrow(fc), 10)  # 5 per group
  expect_equal(sum(fc$series == "A"), 5)
  expect_equal(sum(fc$series == "B"), 5)
  expect_true(all(!is.na(fc$value_forecast)))
})

test_that("forecast with return_index works", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2023-01-01") + 0:49,
    value = 1:50 + rnorm(50)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(2), data = ts_obj, model = test_model_lm())

  # Forecast with step index instead of dates
  fc <- forecast(m, h = 5, return_index = TRUE)

  expect_equal(nrow(fc), 5)
  expect_true("step" %in% names(fc))
  expect_equal(fc$step, 1:5)
  # Verify date column is NOT present when return_index = TRUE
  expect_false("date" %in% names(fc))
})

test_that("forecast verbose parameter works", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2023-01-01") + 0:49,
    value = 1:50 + rnorm(50)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")
  m <- fit(value ~ p(2), data = ts_obj, model = test_model_lm())

  # Should not message by default
  expect_silent(fc <- forecast(m, h = 5, verbose = FALSE))

  # Should message when verbose = TRUE
  expect_message(
    fc <- forecast(m, h = 5, verbose = TRUE),
    "C\\+\\+ accelerated"
  )
})

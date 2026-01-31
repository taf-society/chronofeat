# Test coerce_numeric_col behavior
# Ensures factors cause errors instead of silent conversion
# Note: coerce_numeric_col is an internal function, accessed via :::

test_that("coerce_numeric_col errors on factor columns", {
  df <- data.frame(
    x = factor(c("Low", "Medium", "High")),
    y = 1:3
  )

  # Should error when trying to coerce a factor
  expect_error(
    chronofeat:::coerce_numeric_col(df, "x"),
    "Cannot compute rolling statistics or moving averages on non-numeric column 'x'"
  )
})

test_that("coerce_numeric_col allows numeric columns", {
  df <- data.frame(
    x = c(1.5, 2.5, 3.5),
    y = 1:3
  )

  # Should not error
  result <- chronofeat:::coerce_numeric_col(df, "x")
  expect_identical(result$x, df$x)
})

test_that("coerce_numeric_col allows integer columns", {
  df <- data.frame(
    x = 1:5,
    y = 1:5
  )

  # Should not error
  result <- chronofeat:::coerce_numeric_col(df, "x")
  expect_identical(result$x, df$x)
})

test_that("coerce_numeric_col errors on character columns", {
  df <- data.frame(
    x = c("1", "2", "not_a_number"),
    y = 1:3
  )

  # Should error on character (now treated like factors)
  expect_error(
    chronofeat:::coerce_numeric_col(df, "x"),
    "Cannot compute rolling statistics or moving averages on non-numeric column 'x'"
  )
})

test_that("coerce_numeric_col errors on character that looks numeric", {
  df <- data.frame(
    x = c("1.5", "2.5", "3.5"),
    y = 1:3
  )

  # Even valid-looking character strings error (user must convert explicitly)
  expect_error(
    chronofeat:::coerce_numeric_col(df, "x"),
    "Cannot compute rolling statistics or moving averages on non-numeric column"
  )
})

test_that("feat_lag_ma_dt errors when trying MA on factor", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2023-01-01") + 0:9,
    value = factor(rep(c("Low", "High"), 5))
  )

  # Should error when computing MA on factor
  expect_error(
    feat_lag_ma_dt(df, date = "date", target = "value", q = 3),
    "Cannot compute rolling statistics or moving averages on non-numeric column"
  )
})

test_that("feat_rolling_dt errors when trying rolling stats on factor", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2023-01-01") + 0:9,
    value = factor(rep(c("Low", "High"), 5))
  )

  # Should error when computing rolling stats on factor
  expect_error(
    feat_rolling_dt(df, date = "date", target = "value", windows = 3, stats = "sum"),
    "Cannot compute rolling statistics or moving averages on non-numeric column"
  )
})

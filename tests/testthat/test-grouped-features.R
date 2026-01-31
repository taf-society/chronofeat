# Test grouped rolling and MA features
# Ensures features don't bleed across series boundaries

test_that("grouped moving averages respect series boundaries", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("slider")

  # Create simple two-group data
  df <- data.frame(
    series = rep(c("A", "B"), each = 10),
    date = rep(as.Date("2023-01-01") + 0:9, 2),
    value = c(rep(10, 10), rep(100, 10))  # Different values per group
  )

  # Compute 3-period MA
  result <- feat_lag_ma_dt(df, date = "date", target = "value",
                           p = NULL, q = 3, groups = "series")

  # First 2 rows of each group should be NA (incomplete window)
  expect_true(is.na(result$value_ma_3[1]))
  expect_true(is.na(result$value_ma_3[2]))
  expect_true(is.na(result$value_ma_3[11]))
  expect_true(is.na(result$value_ma_3[12]))

  # Row 3 of group A should be mean(10, 10, 10) = 10
  expect_equal(result$value_ma_3[3], 10)

  # Row 13 of group B should be mean(100, 100, 100) = 100
  # NOT contaminated by group A values
  expect_equal(result$value_ma_3[13], 100)
})

test_that("grouped rolling stats respect series boundaries", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("slider")

  # Create simple two-group data with distinct patterns
  df <- data.frame(
    series = rep(c("A", "B"), each = 10),
    date = rep(as.Date("2023-01-01") + 0:9, 2),
    value = c(1:10, 101:110)
  )

  # Compute 3-period rollsum
  result <- feat_rolling_dt(df, date = "date", target = "value",
                            groups = "series", windows = 3, stats = "sum")

  # First 2 rows of each group should be NA
  expect_true(is.na(result$value_rollsum_3[1]))
  expect_true(is.na(result$value_rollsum_3[2]))
  expect_true(is.na(result$value_rollsum_3[11]))
  expect_true(is.na(result$value_rollsum_3[12]))

  # Row 3 of group A: sum(1, 2, 3) = 6
  expect_equal(result$value_rollsum_3[3], 6)

  # Row 13 of group B: sum(101, 102, 103) = 306
  # NOT contaminated by group A
  expect_equal(result$value_rollsum_3[13], 306)
})

test_that("grouped lags respect series boundaries", {
  skip_if_not_installed("dplyr")

  # Create two-group data
  df <- data.frame(
    series = rep(c("A", "B"), each = 5),
    date = rep(as.Date("2023-01-01") + 0:4, 2),
    value = c(1:5, 101:105)
  )

  # Compute lag 1
  result <- feat_lag_ma_dt(df, date = "date", target = "value",
                           p = 1, q = NULL, groups = "series")

  # First row of each group should be NA
  expect_true(is.na(result$value_lag_1[1]))
  expect_true(is.na(result$value_lag_1[6]))

  # Second row of group A should be 1 (previous value in group A)
  expect_equal(result$value_lag_1[2], 1)

  # Second row of group B should be 101 (NOT 5 from group A!)
  expect_equal(result$value_lag_1[7], 101)
})

test_that("xreg moving averages respect series boundaries", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("slider")

  # Create two-group data with exogenous variable
  df <- data.frame(
    series = rep(c("A", "B"), each = 5),
    date = rep(as.Date("2023-01-01") + 0:4, 2),
    value = c(1:5, 101:105),
    price = c(rep(10, 5), rep(20, 5))
  )

  # Compute MA on xreg
  result <- feat_lag_ma_dt(df, date = "date", target = "value",
                           p = NULL, q = NULL, groups = "series",
                           xreg = "price", xreg_ma = list(price = 3))

  # First 2 rows of each group should be NA
  expect_true(is.na(result$price_ma_3[1]))
  expect_true(is.na(result$price_ma_3[2]))
  expect_true(is.na(result$price_ma_3[6]))
  expect_true(is.na(result$price_ma_3[7]))

  # Row 3 of group A: mean(10, 10, 10) = 10
  expect_equal(result$price_ma_3[3], 10)

  # Row 8 of group B: mean(20, 20, 20) = 20 (NOT contaminated)
  expect_equal(result$price_ma_3[8], 20)
})

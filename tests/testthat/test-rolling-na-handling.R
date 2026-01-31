# Test rolling stats NA handling
# Ensures all-NA windows return NA (not 0, Inf, -Inf)

test_that("rolling stats return NA when all values are NA", {
  # All-NA window
  y <- c(NA, NA, NA, NA)

  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    roll_windows = 3,
    roll_stats = c("sum", "sd", "min", "max")
  )

  # Should all be NA (not 0, Inf, -Inf)
  expect_true(is.na(feats$value_rollsum_3))
  expect_true(is.na(feats$value_rollsd_3))
  expect_true(is.na(feats$value_rollmin_3))
  expect_true(is.na(feats$value_rollmax_3))
})

test_that("rolling stats work correctly with some NAs", {
  # Mixed: some valid, some NA
  y <- c(10, NA, 30, 40, NA)

  feats <- chronofeat:::.make_target_feats(
    y = y,
    target_col = "value",
    roll_windows = 3,
    roll_stats = c("sum", "min", "max")
  )

  # Window contains: c(40, NA) (last 3 of: 30, 40, NA)
  # Should compute on non-NA values
  expect_true(!is.na(feats$value_rollsum_3))
  expect_true(!is.na(feats$value_rollmin_3))
  expect_true(!is.na(feats$value_rollmax_3))
})

test_that("rolling sd requires at least 2 non-NA values", {
  # Only 1 non-NA value
  y1 <- c(NA, NA, 10)

  feats1 <- chronofeat:::.make_target_feats(
    y = y1,
    target_col = "value",
    roll_windows = 3,
    roll_stats = "sd"
  )

  # SD needs >= 2 values
  expect_true(is.na(feats1$value_rollsd_3))

  # 2 non-NA values → should work
  y2 <- c(NA, 10, 20)

  feats2 <- chronofeat:::.make_target_feats(
    y = y2,
    target_col = "value",
    roll_windows = 3,
    roll_stats = "sd"
  )

  expect_true(!is.na(feats2$value_rollsd_3))
  expect_equal(feats2$value_rollsd_3, sd(c(10, 20)))
})

test_that("training data with all-NA rolling features is filtered out", {
  skip_if_not_installed("dplyr")

  # Create data where first few rows have all NAs
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:20,
    value = c(NA, NA, NA, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180)
  )

  ts_obj <- TimeSeries(df, date = "date", frequency = "day")

  # Fit with rolling sum
  m <- fit(value ~ p(1) + rollsum(5),
           data = ts_obj,
           model = test_model_lm())

  # Training data should have filtered out rows with NA rollsum
  # First 7 rows have insufficient data (3 NAs + need 5 for rollsum)
  expect_true(nrow(m$data) < nrow(df))

  # All training features should be non-NA
  expect_true(all(!is.na(m$data$value_lag_1)))
  expect_true(all(!is.na(m$data$value_rollsum_5)))
})

test_that("all-NA window doesn't create spurious zero features", {
  skip_if_not_installed("dplyr")

  # Data with a gap (NAs in middle)
  df <- data.frame(
    date = as.Date("2020-01-01") + 0:14,
    value = c(10, 20, 30, NA, NA, NA, NA, NA, 60, 70, 80, 90, 100, 110, 120)
  )

  # Build features directly
  result <- feat_rolling_dt(
    df = df,
    date = "date",
    target = "value",
    groups = NULL,
    windows = 5,
    stats = "sum"
  )

  # Row 8 has window c(NA, NA, NA, NA, NA) → should be NA (not 0)
  expect_true(is.na(result$value_rollsum_5[8]),
              info = "Row 8 (all-NA window) should have NA rollsum")

  # Verify no zeros in rollsum (0 indicates bug with all-NA windows)
  rollsum_values <- result$value_rollsum_5[!is.na(result$value_rollsum_5)]
  expect_true(all(rollsum_values > 0),
              info = "No rollsum should be exactly 0 (indicates all-NA bug)")

  # Rows 5-7 have partial windows with at least one valid value
  # So they should NOT be NA (they should have values: 60, 50, 30)
  expect_false(is.na(result$value_rollsum_5[5]))
  expect_false(is.na(result$value_rollsum_5[6]))
  expect_false(is.na(result$value_rollsum_5[7]))
})

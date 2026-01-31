# Test CV date validation for panel data
# Ensures CV detects and errors on misaligned group dates

test_that("cv_forecast errors on misaligned group dates", {
  skip_if_not_installed("dplyr")

  # Create panel with different date coverage
  # Store A has all 100 days
  # Store B is missing 10 days (simulating closures)
  df <- data.frame(
    store = c(rep("A", 100), rep("B", 90)),
    date = c(
      as.Date("2020-01-01") + 0:99,
      as.Date("2020-01-01") + setdiff(0:99, seq(5, 95, by = 10))
    ),
    value = rnorm(190, 100, 10)
  )

  # Should error with clear message
  expect_error(
    cv_forecast(
      value ~ p(3),
      data = df,
      date = "date",
      groups = "store",
      model = test_model_lm(),
      h = 5,
      n_windows = 3
    ),
    "identical date sequences"
  )
})

test_that("cv_forecast works with aligned group dates", {
  skip_if_not_installed("dplyr")

  # Create panel where all groups have same dates
  df <- data.frame(
    store = rep(c("A", "B", "C"), each = 100),
    date = rep(as.Date("2020-01-01") + 0:99, 3),
    value = rnorm(300, 100, 10)
  )

  # Should work fine
  expect_silent({
    cv_results <- cv_forecast(
      value ~ p(2),
      data = df,
      date = "date",
      groups = "store",
      model = test_model_lm(),
      h = 5,
      n_windows = 3
    )
  })

  # Should return valid results
  expect_true("metrics" %in% names(cv_results))
  expect_true(nrow(cv_results$metrics) > 0)
})

test_that("cv_forecast error message is informative", {
  skip_if_not_installed("dplyr")

  # Misaligned dates
  df <- data.frame(
    region = c(rep("North", 50), rep("South", 45)),
    date = c(
      as.Date("2020-01-01") + 0:49,
      as.Date("2020-01-01") + 0:44
    ),
    sales = rnorm(95, 1000, 100)
  )

  # Error should mention which groups differ and suggest fixes
  expect_error(
    cv_forecast(
      sales ~ p(2),
      data = df,
      date = "date",
      groups = "region",
      model = test_model_lm(),
      h = 3,
      n_windows = 2
    ),
    regex = "Suggestions"
  )
})

test_that("cv_forecast works with single group (no validation needed)", {
  skip_if_not_installed("dplyr")

  df <- data.frame(
    date = as.Date("2020-01-01") + 0:100,
    value = cumsum(rnorm(101, 1, 5))
  )

  # Single series - no validation needed
  expect_silent({
    cv_results <- cv_forecast(
      value ~ p(3),
      data = df,
      date = "date",
      model = test_model_lm(),
      h = 5,
      n_windows = 3
    )
  })

  expect_true("metrics" %in% names(cv_results))
})

test_that("cv_forecast validates dates even with many groups", {
  skip_if_not_installed("dplyr")

  # 5 stores, 4 have aligned dates, 1 doesn't
  df <- data.frame(
    store = c(rep("A", 100), rep("B", 100), rep("C", 100),
              rep("D", 100), rep("E", 95)),  # E is missing 5 dates
    date = c(
      rep(as.Date("2020-01-01") + 0:99, 4),
      as.Date("2020-01-01") + setdiff(0:99, c(10, 30, 50, 70, 90))
    ),
    value = rnorm(495, 50, 10)
  )

  # Should detect store E mismatch
  expect_error(
    cv_forecast(
      value ~ p(2),
      data = df,
      date = "date",
      groups = "store",
      model = test_model_lm(),
      h = 5,
      n_windows = 3
    ),
    "identical date sequences"
  )
})

test_that("cv_forecast detects different date ranges", {
  skip_if_not_installed("dplyr")

  # Same number of dates but different ranges
  df <- data.frame(
    product = c(rep("X", 50), rep("Y", 50)),
    date = c(
      as.Date("2020-01-01") + 0:49,   # Jan-Feb
      as.Date("2020-03-01") + 0:49    # Mar-Apr (completely different!)
    ),
    value = rnorm(100, 200, 20)
  )

  # Should error - different date sequences
  expect_error(
    cv_forecast(
      value ~ p(2),
      data = df,
      date = "date",
      groups = "product",
      model = test_model_lm(),
      h = 5,
      n_windows = 2
    ),
    "identical date sequences"
  )
})

test_that("cv_forecast allows identical sequences in different order", {
  skip_if_not_installed("dplyr")

  # Both groups have same dates, just in different order in the data frame
  dates <- as.Date("2020-01-01") + 0:99

  df <- data.frame(
    group = c(rep("G1", 100), rep("G2", 100)),
    date = c(dates, sample(dates)),  # G2 dates shuffled
    value = rnorm(200, 150, 15)
  )

  # Should work - dates get sorted internally
  expect_silent({
    cv_results <- cv_forecast(
      value ~ p(2),
      data = df,
      date = "date",
      groups = "group",
      model = test_model_lm(),
      h = 5,
      n_windows = 3
    )
  })

  expect_true("metrics" %in% names(cv_results))
})

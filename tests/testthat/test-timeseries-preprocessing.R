test_that("fill_time completes irregular calendar for ungrouped data", {
  # Missing dates: 2020-01-03, 2020-01-04
  df <- data.frame(
    date = as.Date(c('2020-01-01', '2020-01-02', '2020-01-05', '2020-01-06')),
    value = c(10, 20, 50, 60)
  )

  ts <- TimeSeries(df, date = 'date', frequency = 'day', fill_time = TRUE)

  # Should have 6 rows (2020-01-01 to 2020-01-06)
  expect_equal(nrow(ts$data), 6)

  # New rows should have NA values
  expect_equal(sum(is.na(ts$data$value)), 2)

  # Metadata correct
  expect_equal(ts$time_fill_meta$n_added, 2)
  expect_equal(ts$time_fill_meta$by, "day")
})


test_that("fill_time completes irregular calendar for grouped data", {
  df <- data.frame(
    store = rep(c('A', 'B'), each = 4),
    date = c(
      # Store A: missing 2020-01-03
      as.Date(c('2020-01-01', '2020-01-02', '2020-01-04', '2020-01-05')),
      # Store B: missing 2020-01-04
      as.Date(c('2020-01-01', '2020-01-02', '2020-01-03', '2020-01-05'))
    ),
    sales = c(10, 20, 40, 50, 15, 25, 35, 55)
  )

  ts <- TimeSeries(df, date = 'date', groups = 'store', frequency = 'day',
                   fill_time = TRUE)

  # Should have 10 rows (5 per store)
  expect_equal(nrow(ts$data), 10)
  expect_equal(sum(ts$data$store == 'A'), 5)
  expect_equal(sum(ts$data$store == 'B'), 5)

  # Check added rows
  expect_equal(ts$time_fill_meta$n_added, 2)
})


test_that("fill_time uses frequency for step size", {
  df <- data.frame(
    date = as.Date(c('2020-01-01', '2020-01-08', '2020-01-22')),  # Weekly-ish
    value = c(10, 20, 30)
  )

  ts <- TimeSeries(df, date = 'date', frequency = 'week', fill_time = TRUE)

  # 2020-01-01, 2020-01-08, 2020-01-15, 2020-01-22
  expect_equal(nrow(ts$data), 4)
  expect_equal(ts$time_fill_meta$by, "week")
})


test_that("Complete preprocessing pipeline: fill_time + target_na + xreg_na", {
  df <- data.frame(
    store = rep(c('A', 'B'), each = 3),
    date = c(
      # Store A: missing 2020-01-02
      as.Date(c('2020-01-01', '2020-01-03', '2020-01-04')),
      # Store B: missing 2020-01-03
      as.Date(c('2020-01-01', '2020-01-02', '2020-01-04'))
    ),
    sales = c(10, NA, 40, 15, 25, 45),  # Store A has NA
    price = c(5, NA, 7, 4, NA, 6)       # Both have NAs
  )

  ts <- TimeSeries(
    df,
    date = 'date',
    groups = 'store',
    frequency = 'day',
    target = 'sales',
    target_na = list(strategy = 'locf'),
    fill_time = TRUE,
    xreg_na = list(price = list(strategy = 'locf'))
  )

  # Should have 8 rows (4 per store: 2020-01-01 to 2020-01-04)
  expect_equal(nrow(ts$data), 8)
  expect_equal(sum(ts$data$store == 'A'), 4)
  expect_equal(sum(ts$data$store == 'B'), 4)

  # All gaps filled
  expect_equal(sum(is.na(ts$data$sales)), 0)
  expect_equal(sum(is.na(ts$data$price)), 0)

  # Check is_imputed flags
  expect_true("sales_is_imputed" %in% names(ts$data))
  expect_true("price_is_imputed" %in% names(ts$data))

  # Metadata correct
  expect_equal(ts$time_fill_meta$n_added, 2)
  expect_true(ts$target_na_meta$n_imputed > 0)
  expect_true(ts$xreg_na_meta$price$n_imputed > 0)
})


test_that("xreg_na fills multiple exogenous columns independently", {
  df <- data.frame(
    date = seq(as.Date('2020-01-01'), by = 'day', length.out = 10),
    value = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    price = c(5, NA, NA, 8, 9, 10, 11, 12, 13, 14),
    promo = c(0, 0, 1, NA, NA, 1, 0, 0, 0, 0)
  )

  ts <- TimeSeries(
    df,
    date = 'date',
    frequency = 'day',
    xreg_na = list(
      price = list(strategy = 'linear'),
      promo = list(strategy = 'zero')
    )
  )

  # All NAs filled
  expect_equal(sum(is.na(ts$data$price)), 0)
  expect_equal(sum(is.na(ts$data$promo)), 0)

  # Check strategies applied correctly
  expect_true(ts$xreg_na_meta$price$strategy == 'linear')
  expect_true(ts$xreg_na_meta$promo$strategy == 'zero')

  # Zero-fill should produce 0
  expect_equal(ts$data$promo[4], 0)
  expect_equal(ts$data$promo[5], 0)
})


test_that("fill_time disabled by default", {
  df <- data.frame(
    date = as.Date(c('2020-01-01', '2020-01-05')),  # Gap in dates
    value = c(10, 50)
  )

  ts <- TimeSeries(df, date = 'date', frequency = 'day')

  # Should NOT fill gaps
  expect_equal(nrow(ts$data), 2)
  expect_null(ts$time_fill_meta)
})


test_that("xreg_na with groupby works correctly", {
  df <- data.frame(
    store = rep(c('A', 'B'), each = 5),
    date = rep(seq(as.Date('2020-01-01'), by = 'day', length.out = 5), 2),
    sales = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55),
    price = c(5, NA, 7, 8, 9, 4, 5, NA, 7, 8)
  )

  ts <- TimeSeries(
    df,
    date = 'date',
    groups = 'store',
    frequency = 'day',
    xreg_na = list(price = list(strategy = 'linear'))
  )

  # All NAs filled
  expect_equal(sum(is.na(ts$data$price)), 0)

  # Check groupwise filling (Store A and Store B filled independently)
  store_A <- ts$data[ts$data$store == 'A', ]
  store_B <- ts$data[ts$data$store == 'B', ]

  # Store A position 2: linear interpolation between 5 and 7 = 6
  expect_equal(store_A$price[2], 6)

  # Store B position 3: linear interpolation between 5 and 7 = 6
  expect_equal(store_B$price[3], 6)
})


test_that("Print method shows all preprocessing metadata", {
  df <- data.frame(
    date = as.Date(c('2020-01-01', '2020-01-03')),
    value = c(10, 30),
    price = c(5, NA)
  )

  ts <- TimeSeries(
    df,
    date = 'date',
    frequency = 'day',
    target = 'value',
    target_na = list(strategy = 'locf'),
    fill_time = TRUE,
    xreg_na = list(price = list(strategy = 'locf'))
  )

  # Capture print output
  output <- capture.output(print(ts))

  # Should contain metadata
  expect_true(any(grepl("Time grid.*added", output)))
  expect_true(any(grepl("Target.*value", output)))
  expect_true(any(grepl("Exogenous", output)))
  expect_true(any(grepl("price", output)))
})


test_that("Error when xreg column not found", {
  df <- data.frame(
    date = seq(as.Date('2020-01-01'), by = 'day', length.out = 5),
    value = c(10, 20, 30, 40, 50)
  )

  expect_error(
    TimeSeries(
      df,
      date = 'date',
      frequency = 'day',
      xreg_na = list(price = list(strategy = 'locf'))
    ),
    "Exogenous column 'price'"
  )
})


test_that("Metadata tracks zero imputations correctly", {
  df <- data.frame(
    date = seq(as.Date('2020-01-01'), by = 'day', length.out = 5),
    value = c(10, 20, 30, 40, 50),
    price = c(5, 6, 7, 8, 9)  # No NAs
  )

  ts <- TimeSeries(
    df,
    date = 'date',
    frequency = 'day',
    xreg_na = list(price = list(strategy = 'locf'))
  )

  # No NAs, so n_imputed should be 0
  expect_equal(ts$xreg_na_meta$price$n_imputed, 0)
  expect_equal(ts$xreg_na_meta$price$pct_imputed, 0)
})

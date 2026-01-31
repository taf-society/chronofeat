test_that("STL strategy fills gaps with seasonal pattern", {
  # Create seasonal data with weekly pattern
  n <- 100
  dates <- seq(as.Date('2020-01-01'), by = 'day', length.out = n)
  # Weekly seasonality: 50 + 10*sin(2*pi*t/7)
  seasonal_pattern <- 50 + 10 * sin(2 * pi * (1:n) / 7)
  values <- seasonal_pattern + rnorm(n, 0, 2)

  df <- data.frame(date = dates, value = values)

  # Add gaps
  gap_positions <- c(20, 21, 50, 51, 52)
  df$value[gap_positions] <- NA

  # Fill with STL
  result <- fill_gaps(df, 'value', 'date', strategy = 'stl',
                      params = list(period = 7))

  # Check: NAs filled
  expect_false(any(is.na(result$value)))

  # Check: is_imputed flag accurate
  expect_true(all(result$value_is_imputed[gap_positions]))
  expect_false(any(result$value_is_imputed[-gap_positions]))

  # Check: filled values reasonable (within 3 SD of pattern)
  filled_values <- result$value[gap_positions]
  expected_values <- seasonal_pattern[gap_positions]
  expect_true(all(abs(filled_values - expected_values) < 30))  # 3 * 10 (amplitude)
})


test_that("STL auto-detects period from daily data", {
  # Daily data with weekly seasonality
  n <- 60
  dates <- seq(as.Date('2020-01-01'), by = 'day', length.out = n)
  values <- 50 + 10 * sin(2 * pi * (1:n) / 7) + rnorm(n, 0, 1)

  df <- data.frame(date = dates, value = values)
  df$value[c(20, 30, 40)] <- NA

  # Fill without specifying period
  result <- fill_gaps(df, 'value', 'date', strategy = 'stl')

  # Should auto-detect period = 7
  expect_false(any(is.na(result$value)))
  expect_true(all(result$value_is_imputed[c(20, 30, 40)]))
})


test_that("STL auto-detects period from monthly data", {
  # Monthly data with yearly seasonality
  n <- 60
  dates <- seq(as.Date('2020-01-01'), by = 'month', length.out = n)
  values <- 100 + 20 * sin(2 * pi * (1:n) / 12) + rnorm(n, 0, 5)

  df <- data.frame(date = dates, value = values)
  df$value[c(15, 25, 35)] <- NA

  # Fill without specifying period
  result <- fill_gaps(df, 'value', 'date', strategy = 'stl')

  # Should auto-detect period = 12
  expect_false(any(is.na(result$value)))
  expect_true(all(result$value_is_imputed[c(15, 25, 35)]))
})


test_that("STL requires sufficient data", {
  # Only 10 observations with period = 7
  n <- 10
  dates <- seq(as.Date('2020-01-01'), by = 'day', length.out = n)
  values <- 50 + rnorm(n, 0, 2)

  df <- data.frame(date = dates, value = values)
  df$value[5] <- NA

  # Should fail: not enough data for STL (needs 2 * period = 14)
  expect_error(
    fill_gaps(df, 'value', 'date', strategy = 'stl', params = list(period = 7)),
    "STL requires at least 2"
  )
})


test_that("STL handles grouped data independently", {
  # Two stores with different seasonal patterns
  n <- 60
  dates <- rep(seq(as.Date('2020-01-01'), by = 'day', length.out = n), 2)
  stores <- rep(c('A', 'B'), each = n)

  # Store A: amplitude 10, Store B: amplitude 20
  values_A <- 50 + 10 * sin(2 * pi * (1:n) / 7) + rnorm(n, 0, 1)
  values_B <- 100 + 20 * sin(2 * pi * (1:n) / 7) + rnorm(n, 0, 2)

  df <- data.frame(
    store = stores,
    date = dates,
    sales = c(values_A, values_B)
  )

  # Add gaps to both stores
  df$sales[c(20, 30)] <- NA  # Store A gaps
  df$sales[n + c(25, 35)] <- NA  # Store B gaps

  # Fill with STL
  result <- fill_gaps(df, 'sales', 'date', groups = 'store',
                      strategy = 'stl', params = list(period = 7))

  # Check: All NAs filled
  expect_false(any(is.na(result$sales)))

  # Check: Store A and Store B filled independently
  result_A <- result[result$store == 'A', ]
  result_B <- result[result$store == 'B', ]

  # Store A filled values should be around 50 (its base level)
  filled_A <- result_A$sales[c(20, 30)]
  expect_true(all(filled_A > 30 & filled_A < 70))  # Rough range check

  # Store B filled values should be around 100 (its base level)
  filled_B <- result_B$sales[c(25, 35)]
  expect_true(all(filled_B > 70 & filled_B < 130))  # Rough range check
})


test_that("Borrow strategy fills from peer series (median)", {
  # 3 stores, Store A has gaps, B and C are complete
  dates <- rep(seq(as.Date('2020-01-01'), by = 'day', length.out = 20), 3)
  stores <- rep(c('A', 'B', 'C'), each = 20)

  sales <- c(
    # Store A: gaps at positions 3, 4
    c(10, 20, NA, NA, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200),
    # Store B: complete (20 values)
    c(12, 22, 32, 42, 52, 62, 72, 82, 92, 102, 112, 122, 132, 142, 152, 162, 172, 182, 192, 202),
    # Store C: complete (20 values)
    c(8, 18, 28, 38, 48, 58, 68, 78, 88, 98, 108, 118, 128, 138, 148, 158, 168, 178, 188, 198)
  )

  df <- data.frame(store = stores, date = dates, sales = sales)

  # Fill using borrow (median of peers at same date)
  result <- fill_gaps(df, 'sales', 'date', groups = 'store',
                      strategy = 'borrow', params = list(method = 'median'))

  # Store A position 3: median(32 from B, 28 from C) = 30
  # Store A position 4: median(42 from B, 38 from C) = 40
  result_A <- result[result$store == 'A', ]
  expect_equal(result_A$sales[3], 30)
  expect_equal(result_A$sales[4], 40)

  # Check: is_imputed flag
  expect_true(result_A$sales_is_imputed[3])
  expect_true(result_A$sales_is_imputed[4])
  expect_false(any(result_A$sales_is_imputed[-c(3, 4)]))
})


test_that("Borrow strategy fills from peer series (mean)", {
  # 3 stores, Store A has gaps
  dates <- rep(seq(as.Date('2020-01-01'), by = 'day', length.out = 20), 3)
  stores <- rep(c('A', 'B', 'C'), each = 20)

  sales <- c(
    # Store A: gaps at positions 5, 6
    c(10, 20, 30, 40, NA, NA, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200),
    # Store B: complete (20 values)
    c(12, 22, 32, 42, 52, 62, 72, 82, 92, 102, 112, 122, 132, 142, 152, 162, 172, 182, 192, 202),
    # Store C: complete (20 values)
    c(8, 18, 28, 38, 48, 58, 68, 78, 88, 98, 108, 118, 128, 138, 148, 158, 168, 178, 188, 198)
  )

  df <- data.frame(store = stores, date = dates, sales = sales)

  # Fill using borrow (mean of peers)
  result <- fill_gaps(df, 'sales', 'date', groups = 'store',
                      strategy = 'borrow', params = list(method = 'mean'))

  # Store A position 5: mean(52 from B, 48 from C) = 50
  # Store A position 6: mean(62 from B, 58 from C) = 60
  result_A <- result[result$store == 'A', ]
  expect_equal(result_A$sales[5], 50)
  expect_equal(result_A$sales[6], 60)

  # Check: is_imputed flag
  expect_true(result_A$sales_is_imputed[5])
  expect_true(result_A$sales_is_imputed[6])
})


test_that("Borrow handles case where no peers have data at that date", {
  # Store A has gap at date where all peers also have NAs
  dates <- rep(seq(as.Date('2020-01-01'), by = 'day', length.out = 10), 2)
  stores <- rep(c('A', 'B'), each = 10)

  sales <- c(
    # Store A: gap at position 5
    c(10, 20, 30, 40, NA, 60, 70, 80, 90, 100),
    # Store B: also gap at position 5
    c(12, 22, 32, 42, NA, 62, 72, 82, 92, 102)
  )

  df <- data.frame(store = stores, date = dates, sales = sales)

  # Fill using borrow
  result <- fill_gaps(df, 'sales', 'date', groups = 'store',
                      strategy = 'borrow', params = list(method = 'median'))

  # Store A position 5 should still be NA (no peer data available)
  result_A <- result[result$store == 'A', ]
  expect_true(is.na(result_A$sales[5]))

  # Store B position 5 should also still be NA
  result_B <- result[result$store == 'B', ]
  expect_true(is.na(result_B$sales[5]))
})


test_that("Borrow respects group boundaries", {
  # 3 stores, verify no cross-contamination
  dates <- rep(seq(as.Date('2020-01-01'), by = 'day', length.out = 10), 3)
  stores <- rep(c('A', 'B', 'C'), each = 10)

  sales <- c(
    # Store A: gap at position 3, base level 10
    c(10, 20, NA, 40, 50, 60, 70, 80, 90, 100),
    # Store B: complete, base level 100
    seq(100, 190, by = 10),
    # Store C: complete, base level 200
    seq(200, 290, by = 10)
  )

  df <- data.frame(store = stores, date = dates, sales = sales)

  # Fill Store A using borrow
  result <- fill_gaps(df, 'sales', 'date', groups = 'store',
                      strategy = 'borrow', params = list(method = 'median'))

  result_A <- result[result$store == 'A', ]

  # Store A position 3: median(120 from B, 220 from C) = 170
  # This should NOT be influenced by Store A's own data
  expect_equal(result_A$sales[3], 170)

  # Verify Store B and Store C unchanged
  result_B <- result[result$store == 'B', ]
  result_C <- result[result$store == 'C', ]
  expect_equal(result_B$sales, seq(100, 190, by = 10))
  expect_equal(result_C$sales, seq(200, 290, by = 10))
})


test_that("Borrow works with single peer", {
  # Only 2 stores total
  dates <- rep(seq(as.Date('2020-01-01'), by = 'day', length.out = 10), 2)
  stores <- rep(c('A', 'B'), each = 10)

  sales <- c(
    # Store A: gap at position 4
    c(10, 20, 30, NA, 50, 60, 70, 80, 90, 100),
    # Store B: complete
    seq(15, 105, by = 10)
  )

  df <- data.frame(store = stores, date = dates, sales = sales)

  # Fill using borrow (median with 1 peer = that peer's value)
  result <- fill_gaps(df, 'sales', 'date', groups = 'store',
                      strategy = 'borrow', params = list(method = 'median'))

  result_A <- result[result$store == 'A', ]

  # Store A position 4: only B has data, value = 45
  expect_equal(result_A$sales[4], 45)
})

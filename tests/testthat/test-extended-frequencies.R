# Tests for extended frequency support (sub-daily, businessday, biweekly)

# ==============================================================================
# FREQUENCY VALIDATION TESTS
# ==============================================================================

test_that(".validate_frequency accepts all new frequency strings", {
  # Sub-daily frequencies
  expect_equal(chronofeat:::.validate_frequency("second"), "second")
  expect_equal(chronofeat:::.validate_frequency("minute"), "minute")
  expect_equal(chronofeat:::.validate_frequency("halfhour"), "halfhour")
  expect_equal(chronofeat:::.validate_frequency("hour"), "hour")

 # New daily+ frequencies
  expect_equal(chronofeat:::.validate_frequency("businessday"), "businessday")
  expect_equal(chronofeat:::.validate_frequency("biweekly"), "biweekly")

  # Existing frequencies still work
  expect_equal(chronofeat:::.validate_frequency("day"), "day")
  expect_equal(chronofeat:::.validate_frequency("week"), "week")
  expect_equal(chronofeat:::.validate_frequency("month"), "month")
  expect_equal(chronofeat:::.validate_frequency("quarter"), "quarter")
  expect_equal(chronofeat:::.validate_frequency("year"), "year")
})

test_that(".validate_frequency rejects invalid frequencies", {
  expect_error(chronofeat:::.validate_frequency("hourly"), "Invalid frequency")
  expect_error(chronofeat:::.validate_frequency("daily"), "Invalid frequency")
  expect_error(chronofeat:::.validate_frequency("seconds"), "Invalid frequency")
})

test_that(".is_subdaily_frequency correctly identifies sub-daily frequencies", {
  # Sub-daily

  expect_true(chronofeat:::.is_subdaily_frequency("second"))
  expect_true(chronofeat:::.is_subdaily_frequency("minute"))
  expect_true(chronofeat:::.is_subdaily_frequency("halfhour"))
  expect_true(chronofeat:::.is_subdaily_frequency("hour"))

  # Not sub-daily
  expect_false(chronofeat:::.is_subdaily_frequency("day"))
  expect_false(chronofeat:::.is_subdaily_frequency("businessday"))
  expect_false(chronofeat:::.is_subdaily_frequency("week"))
  expect_false(chronofeat:::.is_subdaily_frequency("month"))
  expect_false(chronofeat:::.is_subdaily_frequency(7))  # numeric
})

# ==============================================================================
# FREQUENCY DETECTION TESTS - POSIXct (sub-daily)
# ==============================================================================

test_that(".detect_frequency detects hourly POSIXct data", {
  hourly_dates <- seq(as.POSIXct("2024-01-01"), by = "hour", length.out = 100)
  df <- data.frame(timestamp = hourly_dates, value = rnorm(100))
  detected <- chronofeat:::.detect_frequency(df, "timestamp", NULL)
  expect_equal(detected, "hour")
})

test_that(".detect_frequency detects minute POSIXct data", {
  minute_dates <- seq(as.POSIXct("2024-01-01"), by = "min", length.out = 100)
  df <- data.frame(timestamp = minute_dates, value = rnorm(100))
  detected <- chronofeat:::.detect_frequency(df, "timestamp", NULL)
  expect_equal(detected, "minute")
})

test_that(".detect_frequency detects half-hour POSIXct data", {
  halfhour_dates <- seq(as.POSIXct("2024-01-01"), by = "30 mins", length.out = 100)
  df <- data.frame(timestamp = halfhour_dates, value = rnorm(100))
  detected <- chronofeat:::.detect_frequency(df, "timestamp", NULL)
  expect_equal(detected, "halfhour")
})

test_that(".detect_frequency detects daily POSIXct data", {
  daily_dates <- seq(as.POSIXct("2024-01-01"), by = "day", length.out = 100)
  df <- data.frame(timestamp = daily_dates, value = rnorm(100))
  detected <- chronofeat:::.detect_frequency(df, "timestamp", NULL)
  expect_equal(detected, "day")
})

# ==============================================================================
# FREQUENCY DETECTION TESTS - Date (daily+)
# ==============================================================================

test_that(".detect_frequency detects daily Date data", {
  daily_dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  df <- data.frame(date = daily_dates, value = rnorm(100))
  detected <- chronofeat:::.detect_frequency(df, "date", NULL)
  expect_equal(detected, "day")
})

test_that(".detect_frequency detects weekly Date data", {
  weekly_dates <- seq(as.Date("2024-01-01"), by = "week", length.out = 50)
  df <- data.frame(date = weekly_dates, value = rnorm(50))
  detected <- chronofeat:::.detect_frequency(df, "date", NULL)
  expect_equal(detected, "week")
})

test_that(".detect_frequency detects biweekly Date data", {
  biweekly_dates <- seq(as.Date("2024-01-01"), by = "14 days", length.out = 26)
  df <- data.frame(date = biweekly_dates, value = rnorm(26))
  detected <- chronofeat:::.detect_frequency(df, "date", NULL)
  expect_equal(detected, "biweekly")
})

test_that(".detect_frequency detects monthly Date data", {
  monthly_dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 24)
  df <- data.frame(date = monthly_dates, value = rnorm(24))
  detected <- chronofeat:::.detect_frequency(df, "date", NULL)
  expect_equal(detected, "month")
})

test_that(".detect_frequency detects business day pattern (no weekends)", {
  # Generate only weekdays
  all_dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 100)
  is_weekend <- weekdays(all_dates) %in% c("Saturday", "Sunday")
  business_dates <- all_dates[!is_weekend]
  df <- data.frame(date = business_dates, value = rnorm(length(business_dates)))
  detected <- chronofeat:::.detect_frequency(df, "date", NULL)
  expect_equal(detected, "businessday")
})

# ==============================================================================
# TIMESERIES CREATION TESTS
# ==============================================================================

test_that("TimeSeries accepts POSIXct datetime column", {
  hourly_data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "hour", length.out = 48),
    value = rnorm(48)
  )
  ts <- TimeSeries(hourly_data, date = "timestamp", frequency = "hour")

  expect_s3_class(ts, "TimeSeries")
  expect_equal(ts$frequency, "hour")
  expect_equal(ts$datetime_type, "POSIXct")
  expect_equal(nrow(ts$data), 48)
})

test_that("TimeSeries rejects sub-daily frequency with Date column", {
  date_data <- data.frame(
    date = as.Date("2024-01-01") + 0:9,
    value = rnorm(10)
  )

  expect_error(
    TimeSeries(date_data, date = "date", frequency = "hour"),
    "Sub-daily frequency.*requires a POSIXct"
  )
  expect_error(
    TimeSeries(date_data, date = "date", frequency = "minute"),
    "Sub-daily frequency.*requires a POSIXct"
  )
})

test_that("TimeSeries works with businessday frequency", {
  # Generate business days only
  all_dates <- seq(as.Date("2024-01-01"), by = "day", length.out = 50)
  is_weekend <- weekdays(all_dates) %in% c("Saturday", "Sunday")
  business_dates <- all_dates[!is_weekend]

  bday_data <- data.frame(
    date = business_dates,
    value = rnorm(length(business_dates))
  )
  ts <- TimeSeries(bday_data, date = "date", frequency = "businessday")

  expect_s3_class(ts, "TimeSeries")
  expect_equal(ts$frequency, "businessday")
  expect_equal(ts$datetime_type, "Date")
})

test_that("TimeSeries works with biweekly frequency", {
  biweekly_dates <- seq(as.Date("2024-01-01"), by = "14 days", length.out = 26)
  df <- data.frame(date = biweekly_dates, value = rnorm(26))
  ts <- TimeSeries(df, date = "date", frequency = "biweekly")

  expect_s3_class(ts, "TimeSeries")
  expect_equal(ts$frequency, "biweekly")
})

test_that("TimeSeries auto-detects hourly frequency from POSIXct", {
  hourly_data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "hour", length.out = 100),
    value = rnorm(100)
  )

  expect_message(
    ts <- TimeSeries(hourly_data, date = "timestamp"),
    "Auto-detected frequency: hour"
  )
  expect_equal(ts$frequency, "hour")
})

# ==============================================================================
# BUSINESS DAY GENERATION TESTS
# ==============================================================================

test_that(".generate_business_days skips weekends", {
  # Start on a Friday
  friday <- as.Date("2024-01-05")
  bdays <- chronofeat:::.generate_business_days(friday, 5)

  expect_length(bdays, 5)
  # Should be Mon, Tue, Wed, Thu, Fri of next week
  expect_equal(bdays[1], as.Date("2024-01-08"))  # Monday
  expect_equal(bdays[2], as.Date("2024-01-09"))  # Tuesday
  expect_equal(bdays[3], as.Date("2024-01-10"))  # Wednesday
  expect_equal(bdays[4], as.Date("2024-01-11"))  # Thursday
  expect_equal(bdays[5], as.Date("2024-01-12"))  # Friday

  # Verify no weekends
  weekday_names <- weekdays(bdays)
  expect_false(any(weekday_names %in% c("Saturday", "Sunday")))
})

test_that(".generate_business_days works with POSIXct", {
  friday <- as.POSIXct("2024-01-05 10:00:00")
  bdays <- chronofeat:::.generate_business_days(friday, 3)

  expect_length(bdays, 3)
  expect_s3_class(bdays, "POSIXct")

  # No weekends
  weekday_names <- weekdays(bdays)
  expect_false(any(weekday_names %in% c("Saturday", "Sunday")))
})

# ==============================================================================
# FUTURE DATE GENERATION TESTS
# ==============================================================================

test_that(".generate_future_dates_stable works with hourly frequency", {
  last_date <- as.POSIXct("2024-01-01 12:00:00")
  future <- chronofeat:::.generate_future_dates_stable(last_date, 5, "hour")

  expect_length(future, 5)
  expect_s3_class(future, "POSIXct")
  expect_equal(future[1], as.POSIXct("2024-01-01 13:00:00"))
  expect_equal(future[5], as.POSIXct("2024-01-01 17:00:00"))
})

test_that(".generate_future_dates_stable works with minute frequency", {
  last_date <- as.POSIXct("2024-01-01 12:00:00")
  future <- chronofeat:::.generate_future_dates_stable(last_date, 5, "minute")

  expect_length(future, 5)
  expect_equal(future[1], as.POSIXct("2024-01-01 12:01:00"))
  expect_equal(future[5], as.POSIXct("2024-01-01 12:05:00"))
})

test_that(".generate_future_dates_stable works with halfhour frequency", {
  last_date <- as.POSIXct("2024-01-01 12:00:00")
  future <- chronofeat:::.generate_future_dates_stable(last_date, 4, "halfhour")

  expect_length(future, 4)
  expect_equal(future[1], as.POSIXct("2024-01-01 12:30:00"))
  expect_equal(future[2], as.POSIXct("2024-01-01 13:00:00"))
  expect_equal(future[4], as.POSIXct("2024-01-01 14:00:00"))
})

test_that(".generate_future_dates_stable works with biweekly frequency", {
  last_date <- as.Date("2024-01-01")
  future <- chronofeat:::.generate_future_dates_stable(last_date, 3, "biweekly")

  expect_length(future, 3)
  expect_equal(future[1], as.Date("2024-01-15"))
  expect_equal(future[2], as.Date("2024-01-29"))
  expect_equal(future[3], as.Date("2024-02-12"))
})

test_that(".generate_future_dates_stable works with businessday frequency", {
  # Friday
  last_date <- as.Date("2024-01-05")
  future <- chronofeat:::.generate_future_dates_stable(last_date, 5, "businessday")

  expect_length(future, 5)
  # Should skip weekend
  expect_equal(future[1], as.Date("2024-01-08"))  # Monday

  # No weekends in result
  weekday_names <- weekdays(future)
  expect_false(any(weekday_names %in% c("Saturday", "Sunday")))
})

# ==============================================================================
# SEASONAL PERIODS TESTS
# ==============================================================================

test_that(".get_seasonal_periods returns correct periods for sub-daily frequencies", {
  # Hour: 24 (day), 168 (week), 8766 (year)
  hour_periods <- chronofeat:::.get_seasonal_periods("hour")
  expect_equal(hour_periods, c(24, 168, 8766))

  # Minute: 60 (hour), 1440 (day), 10080 (week)
  minute_periods <- chronofeat:::.get_seasonal_periods("minute")
  expect_equal(minute_periods, c(60, 1440, 10080))

  # Half-hour: 48 (day), 336 (week), 17532 (year)
  halfhour_periods <- chronofeat:::.get_seasonal_periods("halfhour")
  expect_equal(halfhour_periods, c(48, 336, 17532))
})

test_that(".get_seasonal_periods returns correct periods for daily+ frequencies", {
  expect_equal(chronofeat:::.get_seasonal_periods("day"), c(7, 365))
  expect_equal(chronofeat:::.get_seasonal_periods("businessday"), c(5, 260))
  expect_equal(chronofeat:::.get_seasonal_periods("biweekly"), c(26))
  expect_equal(chronofeat:::.get_seasonal_periods("week"), c(52))
  expect_equal(chronofeat:::.get_seasonal_periods("month"), c(12))
})

# ==============================================================================
# CALENDAR FEATURE TESTS
# ==============================================================================

test_that(".add_calendar_feats adds hod (hour of day) for POSIXct", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2024-01-01 08:00:00", "2024-01-01 14:00:00", "2024-01-01 23:00:00")),
    value = 1:3
  )

  result <- chronofeat:::.add_calendar_feats(df, "timestamp", hod = TRUE)

  expect_true("hod" %in% names(result))
  expect_equal(result$hod, c(8L, 14L, 23L))
})

test_that(".add_calendar_feats adds moh (minute of hour) for POSIXct", {
  df <- data.frame(
    timestamp = as.POSIXct(c("2024-01-01 08:15:00", "2024-01-01 14:30:00", "2024-01-01 23:45:00")),
    value = 1:3
  )

  result <- chronofeat:::.add_calendar_feats(df, "timestamp", moh = TRUE)

  expect_true("moh" %in% names(result))
  expect_equal(result$moh, c(15L, 30L, 45L))
})

test_that(".add_calendar_feats warns when using hod/moh with Date", {
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    value = 1:2
  )

  expect_warning(
    result <- chronofeat:::.add_calendar_feats(df, "date", hod = TRUE),
    "hod.*requires POSIXct"
  )
  expect_equal(result$hod, c(0L, 0L))  # Returns 0 as fallback
})

# ==============================================================================
# FORMULA PARSING TESTS
# ==============================================================================

test_that("Formula parser recognizes hod() and moh() calendar features", {
  df <- data.frame(
    timestamp = as.POSIXct("2024-01-01") + 0:9 * 3600,
    value = rnorm(10)
  )

  spec <- chronofeat:::.parse_fit_formula(value ~ p(1) + hod() + moh(), df)

  expect_true(spec$cal$hod)
  expect_true(spec$cal$moh)
  expect_true("hod" %in% spec$requested_cols)
  expect_true("moh" %in% spec$requested_cols)
})

# ==============================================================================
# METADATA STORAGE TESTS
# ==============================================================================

test_that("fit() stores datetime_type in metadata", {
  # Create hourly data
  hourly_data <- data.frame(
    timestamp = seq(as.POSIXct("2024-01-01"), by = "hour", length.out = 100),
    value = rnorm(100)
  )
  ts <- TimeSeries(hourly_data, date = "timestamp", frequency = "hour")

  m <- fit(value ~ p(1), data = ts, model = lm)

  expect_equal(m$meta$datetime_type, "POSIXct")
  expect_equal(m$meta$frequency, "hour")
})

test_that("fit() stores Date datetime_type for Date columns", {
  daily_data <- data.frame(
    date = seq(as.Date("2024-01-01"), by = "day", length.out = 100),
    value = rnorm(100)
  )
  ts <- TimeSeries(daily_data, date = "date", frequency = "day")

  m <- fit(value ~ p(1), data = ts, model = lm)

  expect_equal(m$meta$datetime_type, "Date")
  expect_equal(m$meta$frequency, "day")
})

# ==============================================================================
# FILL_TIME TESTS WITH NEW FREQUENCIES
# ==============================================================================

test_that("fill_time works with biweekly frequency", {
  # Missing one biweekly period
  df <- data.frame(
    date = as.Date(c("2024-01-01", "2024-01-15", "2024-02-12")),  # Missing 2024-01-29
    value = c(10, 20, 30)
  )

  ts <- TimeSeries(df, date = "date", frequency = "biweekly", fill_time = TRUE)

  expect_equal(nrow(ts$data), 4)
  expect_equal(ts$time_fill_meta$n_added, 1)
  expect_true(as.Date("2024-01-29") %in% ts$data$date)
})

test_that("fill_time works with hourly POSIXct data", {
  # Missing 2 hours
  df <- data.frame(
    timestamp = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-01 01:00:00",
                             "2024-01-01 04:00:00", "2024-01-01 05:00:00")),
    value = c(10, 20, 50, 60)
  )

  ts <- TimeSeries(df, date = "timestamp", frequency = "hour", fill_time = TRUE)

  expect_equal(nrow(ts$data), 6)
  expect_equal(ts$time_fill_meta$n_added, 2)
})

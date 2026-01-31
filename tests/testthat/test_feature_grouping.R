test_that("feat_lag_ma_dt computes moving averages within groups only", {
  df <- data.frame(
    grp = rep(c("A", "B"), each = 4),
    date = rep(seq.Date(as.Date("2020-01-01"), by = "day", length.out = 4), 2),
    y = c(1, 2, 3, 4, 10, 20, 30, 40),
    x = c(5, 6, 7, 8, 50, 60, 70, 80)
  )

  res <- chronofeat::feat_lag_ma_dt(
    df = df,
    date = "date",
    target = "y",
    q = 2,
    groups = "grp",
    xreg = "x",
    xreg_ma = list(x = 2)
  )

  expect_true(all(is.na(res$y_ma_2[res$grp == "A"][1])))
  expect_true(all(is.na(res$y_ma_2[res$grp == "B"][1])))

  expect_equal(res$y_ma_2[res$grp == "A"][2], mean(c(1, 2)))
  expect_equal(res$y_ma_2[res$grp == "B"][2], mean(c(10, 20)))

  expect_true(all(is.na(res$x_ma_2[res$grp == "A"][1])))
  expect_true(all(is.na(res$x_ma_2[res$grp == "B"][1])))
  expect_equal(res$x_ma_2[res$grp == "A"][2], mean(c(5, 6)))
  expect_equal(res$x_ma_2[res$grp == "B"][2], mean(c(50, 60)))
})

test_that("feat_rolling_dt rolling windows respect groups", {
  df <- data.frame(
    grp = rep(c("A", "B"), each = 4),
    date = rep(seq.Date(as.Date("2020-01-01"), by = "day", length.out = 4), 2),
    y = c(1, 2, 3, 4, 10, 20, 30, 40)
  )

  res <- chronofeat::feat_rolling_dt(
    df = df,
    date = "date",
    target = "y",
    groups = "grp",
    windows = 2,
    stats = c("sum", "min", "max")
  )

  expect_true(all(is.na(res$y_rollsum_2[res$grp == "A"][1])))
  expect_true(all(is.na(res$y_rollsum_2[res$grp == "B"][1])))

  expect_equal(res$y_rollsum_2[res$grp == "A"][2], sum(c(1, 2)))
  expect_equal(res$y_rollsum_2[res$grp == "B"][2], sum(c(10, 20)))

  expect_equal(res$y_rollmin_2[res$grp == "B"][2], min(c(10, 20)))
  expect_equal(res$y_rollmax_2[res$grp == "B"][2], max(c(10, 20)))
})

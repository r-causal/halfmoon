test_that("autoplot works for halfmoon_balance", {
  balance_data <- check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = w_ate)

  p <- autoplot(balance_data)
  expect_s3_class(p, "ggplot")

  expect_doppelganger(
    "autoplot-halfmoon-balance",
    autoplot(balance_data)
  )
})

test_that("autoplot works for halfmoon_auc", {
  auc_data <- check_model_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))

  p <- autoplot(auc_data)
  expect_s3_class(p, "ggplot")

  expect_doppelganger(
    "autoplot-halfmoon-auc",
    autoplot(auc_data)
  )
})

test_that("autoplot works for halfmoon_ess", {
  ess_data <- check_ess(nhefs_weights, .wts = c(w_ate, w_att))

  p <- autoplot(ess_data)
  expect_s3_class(p, "ggplot")

  expect_doppelganger(
    "autoplot-halfmoon-ess",
    autoplot(ess_data)
  )
})

test_that("autoplot works for halfmoon_calibration", {
  cal_data <- check_model_calibration(nhefs_weights, .fitted, qsmk)

  p <- autoplot(cal_data)
  expect_s3_class(p, "ggplot")

  expect_doppelganger(
    "autoplot-halfmoon-calibration",
    autoplot(cal_data)
  )
})

test_that("autoplot works for halfmoon_roc", {
  roc_data <- check_model_roc_curve(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))

  p <- autoplot(roc_data)
  expect_s3_class(p, "ggplot")

  expect_doppelganger(
    "autoplot-halfmoon-roc",
    autoplot(roc_data)
  )
})

test_that("autoplot works for halfmoon_qq", {
  qq_data <- check_qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))

  p <- autoplot(qq_data)
  expect_s3_class(p, "ggplot")

  expect_doppelganger(
    "autoplot-halfmoon-qq",
    autoplot(qq_data)
  )
})

test_that("plot methods work for all halfmoon classes", {
  # Create all data types
  balance_data <- check_balance(nhefs_weights, age, qsmk, .wts = w_ate)
  auc_data <- check_model_auc(nhefs_weights, qsmk, .fitted, w_ate)
  ess_data <- check_ess(nhefs_weights, .wts = w_ate)
  cal_data <- check_model_calibration(nhefs_weights, .fitted, qsmk)
  roc_data <- check_model_roc_curve(nhefs_weights, qsmk, .fitted, w_ate)
  qq_data <- check_qq(nhefs_weights, age, qsmk, .wts = w_ate)

  # Test plot() methods with vdiffr
  expect_doppelganger(
    "plot-halfmoon-balance",
    plot(balance_data)
  )

  expect_doppelganger(
    "plot-halfmoon-auc",
    plot(auc_data)
  )

  expect_doppelganger(
    "plot-halfmoon-ess",
    plot(ess_data)
  )

  expect_doppelganger(
    "plot-halfmoon-calibration",
    plot(cal_data)
  )

  expect_doppelganger(
    "plot-halfmoon-roc",
    plot(roc_data)
  )

  expect_doppelganger(
    "plot-halfmoon-qq",
    plot(qq_data)
  )
})

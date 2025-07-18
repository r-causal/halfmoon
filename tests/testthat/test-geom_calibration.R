library(ggplot2)

test_that("check_calibration works with basic input", {
  # Create simple test data with larger sample size
  set.seed(123)
  n <- 500
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test basic functionality
  result <- check_calibration(test_data, "pred", "obs")

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c(".bin", "x_mean", "y_mean", "count", "lower", "upper") %in% names(result)
  ))
  expect_true(nrow(result) > 0)
  expect_true(all(result$x_mean >= 0 & result$x_mean <= 1))
  expect_true(all(result$y_mean >= 0 & result$y_mean <= 1))
  expect_true(all(result$count > 0))
})

test_that("check_calibration works with quoted column names", {
  # Create simple test data
  set.seed(123)
  n <- 500
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with quoted column names
  result <- check_calibration(test_data, x = "pred", y = "obs")

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("check_calibration works with unquoted column names", {
  # Create simple test data
  set.seed(123)
  n <- 500
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with unquoted column names (using rlang::sym to avoid scoping issues)
  result <- check_calibration(test_data, rlang::sym("pred"), rlang::sym("obs"))

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("check_calibration handles different binning methods", {
  # Create test data
  set.seed(123)
  n <- 500
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test equal_width binning
  result_eq <- check_calibration(
    test_data,
    "pred",
    "obs",
    binning_method = "equal_width"
  )

  # Test quantile binning
  result_qt <- check_calibration(
    test_data,
    "pred",
    "obs",
    binning_method = "quantile"
  )

  expect_s3_class(result_eq, "tbl_df")
  expect_s3_class(result_qt, "tbl_df")
  expect_true(nrow(result_eq) > 0)
  expect_true(nrow(result_qt) > 0)
})

test_that("check_calibration handles edge cases", {
  # Test empty data
  empty_data <- data.frame(pred = numeric(0), obs = numeric(0))
  result_empty <- check_calibration(empty_data, "pred", "obs")

  expect_s3_class(result_empty, "tbl_df")
  expect_equal(nrow(result_empty), 0)

  # Test all zeros
  all_zeros <- data.frame(pred = runif(50, 0, 1), obs = rep(0, 50))
  result_zeros <- check_calibration(all_zeros, "pred", "obs")

  expect_s3_class(result_zeros, "tbl_df")
  expect_true(all(result_zeros$y_mean == 0))

  # Test all ones
  all_ones <- data.frame(pred = runif(50, 0, 1), obs = rep(1, 50))
  result_ones <- check_calibration(all_ones, "pred", "obs")

  expect_s3_class(result_ones, "tbl_df")
  expect_true(all(result_ones$y_mean == 1))
})

test_that("check_calibration handles NA values", {
  # Create test data with NAs
  set.seed(123)
  n <- 100
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  # Add some NAs
  predicted[1:5] <- NA
  actual[6:10] <- NA

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with na.rm = TRUE
  result_na_rm <- check_calibration(test_data, "pred", "obs", na.rm = TRUE)

  expect_s3_class(result_na_rm, "tbl_df")
  expect_true(nrow(result_na_rm) > 0)

  # Test with na.rm = FALSE (should have fewer complete cases)
  result_na_keep <- check_calibration(test_data, "pred", "obs", na.rm = FALSE)

  expect_s3_class(result_na_keep, "tbl_df")
})

test_that("geom_calibration works with breaks method", {
  # Create test data with known calibration pattern
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test breaks method
  p_breaks <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "breaks", bins = 5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_breaks, "ggplot")
  expect_doppelganger("calibration breaks method", p_breaks)
})

test_that("geom_calibration works with logistic method", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test logistic method (linear)
  p_logistic <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "logistic", smooth = FALSE, show_points = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_logistic, "ggplot")
  expect_doppelganger("calibration logistic method", p_logistic)
})

test_that("geom_calibration works with windowed method", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test windowed method
  p_windowed <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "windowed", window_size = 0.2, step_size = 0.1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_windowed, "ggplot")
  expect_doppelganger("calibration windowed method", p_windowed)
})

test_that("geom_calibration works with different show options", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with ribbon hidden
  p_no_ribbon <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "breaks", show_ribbon = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    lims(x = c(0, 1), y = c(0, 1))

  # Test with points hidden
  p_no_points <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "breaks", show_points = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_no_ribbon, "ggplot")
  expect_s3_class(p_no_points, "ggplot")
  expect_doppelganger("calibration no ribbon", p_no_ribbon)
  expect_doppelganger("calibration no points", p_no_points)
})

test_that("geom_calibration handles different confidence levels", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with 90% confidence level
  p_90 <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "breaks", conf_level = 0.90) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_90, "ggplot")
  expect_doppelganger("calibration 90% confidence", p_90)
})

test_that("geom_calibration works with nhefs_weights data", {
  # Test with actual package data
  # Create binary outcome from propensity score
  set.seed(123)
  cal_data <- nhefs_weights
  cal_data$binary_outcome <- rbinom(nrow(cal_data), 1, cal_data$.fitted)

  p_nhefs <- ggplot(cal_data, aes(x = .fitted, y = binary_outcome)) +
    geom_calibration(method = "breaks", bins = 8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Propensity Score", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_nhefs, "ggplot")
  expect_doppelganger("calibration nhefs data", p_nhefs)
})

test_that("geom_calibration errors with invalid method", {
  # Create test data
  set.seed(123)
  n <- 50
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with invalid method - error occurs during rendering
  p <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "invalid_method")

  expect_error(
    ggplot_build(p),
    "Method must be 'breaks', 'logistic', or 'windowed'"
  )
})

test_that("check_calibration errors with invalid bins", {
  # Create test data
  set.seed(123)
  n <- 50
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with invalid bins
  expect_error(
    check_calibration(cal_data, "pred", "obs", bins = 1),
    "`bins` must be an integer > 1"
  )

  expect_error(
    check_calibration(cal_data, "pred", "obs", bins = 2.5),
    "`bins` must be an integer > 1"
  )
})

test_that("geom_calibration logistic method works with smooth = TRUE", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  cal_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test logistic method with smooth = TRUE (should work regardless of mgcv availability)
  p_smooth <- ggplot(cal_data, aes(x = pred, y = obs)) +
    geom_calibration(method = "logistic", smooth = TRUE, show_points = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_smooth, "ggplot")
  expect_doppelganger("calibration logistic smooth", p_smooth)
})

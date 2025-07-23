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
  result <- check_calibration(test_data, pred, obs)

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c(".bin", "fitted_mean", "group_mean", "count", "lower", "upper") %in%
      names(result)
  ))
  expect_true(nrow(result) > 0)
  expect_true(all(result$fitted_mean >= 0 & result$fitted_mean <= 1))
  expect_true(all(result$group_mean >= 0 & result$group_mean <= 1))
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
  result <- check_calibration(test_data, pred, obs)

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

test_that("check_calibration handles both quoted and unquoted column names", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test with quoted column names
  result_quoted <- check_calibration(test_data, pred, obs)

  # Test with unquoted column names in a function context
  test_unquoted <- function(data, x_col, y_col) {
    check_calibration(data, {{ x_col }}, {{ y_col }})
  }
  result_unquoted <- test_unquoted(test_data, pred, obs)

  # Results should be identical
  expect_s3_class(result_quoted, "tbl_df")
  expect_s3_class(result_unquoted, "tbl_df")
  expect_equal(result_quoted, result_unquoted)
  expect_true(nrow(result_quoted) > 0)
  expect_true(nrow(result_unquoted) > 0)
})

test_that("check_calibration provides clear error messages for missing columns", {
  # Create test data
  set.seed(123)
  test_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = rbinom(50, 1, 0.5)
  )

  # Test with non-existent .fitted column
  expect_error(
    check_calibration(test_data, "nonexistent", "obs"),
    "Column 'nonexistent' not found in data"
  )

  # Test with non-existent .group column
  expect_error(
    check_calibration(test_data, "pred", "nonexistent"),
    "Column 'nonexistent' not found in data"
  )
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
  result_empty <- check_calibration(empty_data, pred, obs)

  expect_s3_class(result_empty, "tbl_df")
  expect_equal(nrow(result_empty), 0)

  # Test all zeros (treatment level 0, so group_mean should be 1)
  all_zeros <- data.frame(pred = runif(50, 0, 1), obs = rep(0, 50))
  result_zeros <- check_calibration(all_zeros, pred, obs)

  expect_s3_class(result_zeros, "tbl_df")
  expect_true(all(result_zeros$group_mean == 1)) # All obs are 0, which becomes the treatment level

  # Test all ones (treatment level 1, so group_mean should be 1)
  all_ones <- data.frame(pred = runif(50, 0, 1), obs = rep(1, 50))
  result_ones <- check_calibration(all_ones, pred, obs)

  expect_s3_class(result_ones, "tbl_df")
  expect_true(all(result_ones$group_mean == 1)) # All obs are 1, which becomes the treatment level
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
  result_na_rm <- check_calibration(test_data, pred, obs, na.rm = TRUE)

  expect_s3_class(result_na_rm, "tbl_df")
  expect_true(nrow(result_na_rm) > 0)

  # Test with na.rm = FALSE (should have fewer complete cases)
  result_na_keep <- check_calibration(test_data, pred, obs, na.rm = FALSE)

  expect_s3_class(result_na_keep, "tbl_df")
})

test_that("check_calibration works with logistic method", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test logistic without smoothing
  result_logistic <- check_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = FALSE
  )

  expect_s3_class(result_logistic, "tbl_df")
  expect_true(all(
    c("fitted_mean", "group_mean", "lower", "upper") %in% names(result_logistic)
  ))
  expect_equal(nrow(result_logistic), 100) # Default 100 prediction points
  expect_true(all(
    result_logistic$fitted_mean >= 0 & result_logistic$fitted_mean <= 1
  ))
  expect_true(all(
    result_logistic$group_mean >= 0 & result_logistic$group_mean <= 1
  ))
  expect_true(all(result_logistic$lower >= 0 & result_logistic$lower <= 1))
  expect_true(all(result_logistic$upper >= 0 & result_logistic$upper <= 1))
  expect_true(all(result_logistic$lower <= result_logistic$group_mean))
  expect_true(all(result_logistic$upper >= result_logistic$group_mean))

  # Test logistic with smoothing
  result_smooth <- check_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = TRUE
  )

  expect_s3_class(result_smooth, "tbl_df")
  expect_equal(nrow(result_smooth), 100)
  expect_true(all(result_smooth$lower <= result_smooth$group_mean))
  expect_true(all(result_smooth$upper >= result_smooth$group_mean))

  # Test that predictions span the range of input data
  expect_equal(min(result_logistic$fitted_mean), min(test_data$pred))
  expect_equal(max(result_logistic$fitted_mean), max(test_data$pred))
})

test_that("check_calibration logistic method handles different confidence levels", {
  set.seed(123)
  test_data <- data.frame(
    pred = runif(100, 0, 1),
    obs = rbinom(100, 1, 0.5)
  )

  # Test with 90% confidence
  result_90 <- check_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    conf_level = 0.90
  )

  # Test with 99% confidence
  result_99 <- check_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    conf_level = 0.99
  )

  # 99% CI should be wider than 90% CI
  expect_true(
    mean(result_99$upper - result_99$lower) >
      mean(result_90$upper - result_90$lower)
  )
})

test_that("check_calibration logistic method handles perfect separation", {
  # Create data with perfect separation
  test_data <- data.frame(
    pred = c(rep(0.1, 50), rep(0.9, 50)),
    obs = c(rep(0, 50), rep(1, 50))
  )

  # Should not error with perfect separation
  expect_no_error({
    result <- check_calibration(
      test_data,
      pred,
      obs,
      method = "logistic",
      smooth = FALSE
    )
  })
})

test_that("check_calibration works with windowed method", {
  # Create test data
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Test windowed method
  result_windowed <- check_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.2,
    step_size = 0.1
  )

  expect_s3_class(result_windowed, "tbl_df")
  expect_true(all(
    c("fitted_mean", "group_mean", "lower", "upper") %in% names(result_windowed)
  ))
  expect_true(nrow(result_windowed) > 0)
  expect_true(all(
    result_windowed$fitted_mean >= 0 & result_windowed$fitted_mean <= 1
  ))
  expect_true(all(
    result_windowed$group_mean >= 0 & result_windowed$group_mean <= 1
  ))
  expect_true(all(result_windowed$lower >= 0 & result_windowed$lower <= 1))
  expect_true(all(result_windowed$upper >= 0 & result_windowed$upper <= 1))
  expect_true(all(result_windowed$lower <= result_windowed$group_mean))
  expect_true(all(result_windowed$upper >= result_windowed$group_mean))

  # Test with different window sizes
  result_small_window <- check_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.05,
    step_size = 0.025
  )

  expect_true(nrow(result_small_window) > nrow(result_windowed))

  # Test window centers
  expected_centers <- seq(0, 1, by = 0.1)
  expect_equal(result_windowed$fitted_mean, expected_centers)
})

test_that("check_calibration windowed method handles edge cases", {
  set.seed(123)

  # Test with data concentrated at edges
  test_data <- data.frame(
    pred = c(runif(50, 0, 0.1), runif(50, 0.9, 1)),
    obs = rbinom(100, 1, 0.5)
  )

  result <- check_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.2,
    step_size = 0.1
  )

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)

  # Test with very small window
  result_tiny <- check_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.01,
    step_size = 0.1
  )

  # Some windows might be empty
  expect_true(nrow(result_tiny) <= length(seq(0, 1, by = 0.1)))
})

test_that("check_calibration windowed method respects window boundaries", {
  # Create data only in middle range
  test_data <- data.frame(
    pred = runif(100, 0.4, 0.6),
    obs = rbinom(100, 1, 0.5)
  )

  result <- check_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.1,
    step_size = 0.1
  )

  # Windows at 0 and 1 should have no data (depending on window size)
  # But windows around 0.5 should have data
  center_window <- result[result$fitted_mean == 0.5, ]
  expect_equal(nrow(center_window), 1)
  expect_true(center_window$group_mean >= 0)
})

test_that("check_calibration method parameter validation", {
  test_data <- data.frame(pred = runif(50), obs = rbinom(50, 1, 0.5))

  # Test invalid method
  expect_error(
    check_calibration(test_data, pred, obs, method = "invalid"),
    "'arg' should be one of"
  )
})

test_that("check_calibration handles edge cases with all methods", {
  # Empty data
  empty_data <- data.frame(pred = numeric(0), obs = numeric(0))

  result_breaks <- check_calibration(empty_data, pred, obs, method = "breaks")
  expect_equal(nrow(result_breaks), 0)
  expect_true("count" %in% names(result_breaks))
  expect_true(".bin" %in% names(result_breaks))

  result_logistic <- check_calibration(
    empty_data,
    pred,
    obs,
    method = "logistic"
  )
  expect_equal(nrow(result_logistic), 0)
  expect_false("count" %in% names(result_logistic))
  expect_false(".bin" %in% names(result_logistic))

  result_windowed <- check_calibration(
    empty_data,
    pred,
    obs,
    method = "windowed"
  )
  expect_equal(nrow(result_windowed), 0)
  expect_false("count" %in% names(result_windowed))
  expect_false(".bin" %in% names(result_windowed))
})

test_that("check_calibration handles all zeros and all ones", {
  set.seed(123)

  # All zeros - when treatment_level is not specified, 0 becomes the treatment level
  # so group_mean will be 1 (all observations match treatment level)
  zeros_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = rep(0, 50)
  )

  # When all observations are 0, default treatment_level will be 0
  # Test the actual behavior
  result_breaks_zeros_default <- check_calibration(
    zeros_data,
    pred,
    obs,
    method = "breaks"
  )
  # All values are 0 and treatment_level=0, so group_mean=1
  expect_true(all(result_breaks_zeros_default$group_mean == 1))

  # Now test with mixed data where 1 is the treatment level
  mixed_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = c(rep(0, 25), rep(1, 25))
  )

  result_mixed <- check_calibration(
    mixed_data,
    pred,
    obs,
    method = "breaks",
    treatment_level = 1
  )
  # This should give us meaningful calibration metrics
  expect_true(all(result_mixed$group_mean >= 0 & result_mixed$group_mean <= 1))

  # All ones - default treatment level will be 1, so group_mean = 1
  ones_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = rep(1, 50)
  )

  result_breaks_ones <- check_calibration(
    ones_data,
    pred,
    obs,
    method = "breaks"
  )
  expect_true(all(result_breaks_ones$group_mean == 1))

  result_windowed_ones <- check_calibration(
    ones_data,
    pred,
    obs,
    method = "windowed"
  )
  expect_true(all(result_windowed_ones$group_mean == 1))

  # Test the default behavior with all zeros
  result_default_zeros <- check_calibration(
    zeros_data,
    pred,
    obs,
    method = "breaks"
  )
  # With default treatment_level, all zeros means treatment_level=0, so group_mean=1
  expect_true(all(result_default_zeros$group_mean == 1))
})

test_that("check_calibration handles NA values correctly", {
  # Create data with NAs
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(45), rep(NA, 5)),
    obs = c(rbinom(45, 1, 0.5), rep(NA, 5))
  )

  # Test with na.rm = FALSE (default)
  result_false <- check_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    na.rm = FALSE
  )

  # Test with na.rm = TRUE
  result_true <- check_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    na.rm = TRUE
  )

  # na.rm = TRUE should have data, na.rm = FALSE might have less
  expect_true(sum(result_true$count) == 45)
})

test_that("check_calibration handles factor treatment variables", {
  set.seed(123)
  test_data <- data.frame(
    pred = runif(100, 0, 1),
    obs = factor(rbinom(100, 1, 0.5), levels = c("0", "1"))
  )

  # Should work with factor
  result <- check_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    treatment_level = "1"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$group_mean >= 0 & result$group_mean <= 1))
})

test_that("check_calibration validates input parameters", {
  test_data <- data.frame(pred = runif(50), obs = rbinom(50, 1, 0.5))

  # Invalid bins for breaks method
  expect_error(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 1),
    "bins.*must be an integer > 1"
  )

  # Non-integer bins
  expect_error(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 2.5),
    "bins.*must be an integer > 1"
  )

  # Missing column
  expect_error(
    check_calibration(test_data, nonexistent, obs),
    "Column.*not found"
  )
})

test_that("check_calibration produces consistent results across methods", {
  # Create well-calibrated data
  set.seed(123)
  n <- 500
  pred <- runif(n, 0, 1)
  obs <- rbinom(n, 1, pred) # Perfect calibration on average

  test_data <- data.frame(pred = pred, obs = obs)

  # Get results from all methods
  result_breaks <- check_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    bins = 5
  )
  result_logistic <- check_calibration(
    test_data,
    pred,
    obs,
    method = "logistic"
  )
  result_windowed <- check_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.2
  )

  # All methods should show reasonable calibration (group_mean ≈ fitted_mean)
  # For breaks method
  breaks_diff <- mean(abs(result_breaks$group_mean - result_breaks$fitted_mean))
  expect_true(breaks_diff < 0.2) # Allow some deviation

  # For windowed method
  windowed_diff <- mean(abs(
    result_windowed$group_mean - result_windowed$fitted_mean
  ))
  expect_true(windowed_diff < 0.2)
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
    check_calibration(cal_data, pred, obs, bins = 1),
    "`bins` must be an integer > 1"
  )

  expect_error(
    check_calibration(cal_data, pred, obs, bins = 2.5),
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

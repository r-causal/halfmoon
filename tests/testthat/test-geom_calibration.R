library(ggplot2)

test_that("check_model_calibration works with basic input", {
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
  result <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs
  ))

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c(".bin", "predicted_rate", "observed_rate", "count", "lower", "upper") %in%
      names(result)
  ))
  expect_true(nrow(result) > 0)
  expect_true(all(result$predicted_rate >= 0 & result$predicted_rate <= 1))
  expect_true(all(result$observed_rate >= 0 & result$observed_rate <= 1))
  expect_true(all(result$count > 0))
})

test_that("check_model_calibration works with quoted column names", {
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
  result <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs
  ))

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("check_model_calibration works with unquoted column names", {
  # Create simple test data
  set.seed(123)
  n <- 500
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  result <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    "pred",
    "obs"
  ))

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
})

test_that("check_model_calibration provides clear error messages for missing columns", {
  # Create test data
  set.seed(123)
  test_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = rbinom(50, 1, 0.5)
  )

  # Test with non-existent .fitted column
  expect_halfmoon_error(
    check_model_calibration(test_data, "nonexistent", "obs"),
    "halfmoon_column_error"
  )

  # Test with non-existent .group column
  expect_halfmoon_error(
    check_model_calibration(test_data, "pred", "nonexistent"),
    "halfmoon_column_error"
  )
})

test_that("check_model_calibration handles different binning methods", {
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
  result_eq <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    "pred",
    "obs",
    binning_method = "equal_width"
  ))

  # Test quantile binning
  result_qt <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    "pred",
    "obs",
    binning_method = "quantile"
  ))

  expect_s3_class(result_eq, "tbl_df")
  expect_s3_class(result_qt, "tbl_df")
  expect_true(nrow(result_eq) > 0)
  expect_true(nrow(result_qt) > 0)
})

test_that("check_model_calibration handles edge cases", {
  # Test empty data
  empty_data <- data.frame(pred = numeric(0), obs = numeric(0))
  result_empty <- check_model_calibration(empty_data, pred, obs)

  expect_s3_class(result_empty, "tbl_df")
  expect_equal(nrow(result_empty), 0)

  # Test all zeros (treatment level 0, so observed_rate should be 1)
  all_zeros <- data.frame(pred = runif(50, 0, 1), obs = rep(0, 50))
  result_zeros <- suppress_calibration_warnings(check_model_calibration(
    all_zeros,
    pred,
    obs
  ))

  expect_s3_class(result_zeros, "tbl_df")
  expect_true(all(result_zeros$observed_rate == 1)) # All obs are 0, which becomes the treatment level

  # Test all ones (treatment level 1, so observed_rate should be 1)
  all_ones <- data.frame(pred = runif(50, 0, 1), obs = rep(1, 50))
  result_ones <- suppress_calibration_warnings(check_model_calibration(
    all_ones,
    pred,
    obs
  ))

  expect_s3_class(result_ones, "tbl_df")
  expect_true(all(result_ones$observed_rate == 1)) # All obs are 1, which becomes the treatment level
})

test_that("check_model_calibration handles NA values", {
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
  result_na_rm <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    na.rm = TRUE
  ))

  expect_s3_class(result_na_rm, "tbl_df")
  expect_true(nrow(result_na_rm) > 0)

  # Test with na.rm = FALSE (should have fewer complete cases)
  result_na_keep <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    na.rm = FALSE
  ))

  expect_s3_class(result_na_keep, "tbl_df")
})

test_that("check_model_calibration works with logistic method", {
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
  result_logistic <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = FALSE
  )

  expect_s3_class(result_logistic, "tbl_df")
  expect_true(all(
    c("predicted_rate", "observed_rate", "lower", "upper") %in%
      names(result_logistic)
  ))
  expect_equal(nrow(result_logistic), 100) # Default 100 prediction points
  expect_true(all(
    result_logistic$predicted_rate >= 0 & result_logistic$predicted_rate <= 1
  ))
  expect_true(all(
    result_logistic$observed_rate >= 0 & result_logistic$observed_rate <= 1
  ))
  expect_true(all(result_logistic$lower >= 0 & result_logistic$lower <= 1))
  expect_true(all(result_logistic$upper >= 0 & result_logistic$upper <= 1))
  expect_true(all(result_logistic$lower <= result_logistic$observed_rate))
  expect_true(all(result_logistic$upper >= result_logistic$observed_rate))

  # Test logistic with smoothing
  result_smooth <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = TRUE
  ))

  expect_s3_class(result_smooth, "tbl_df")
  expect_equal(nrow(result_smooth), 100)
  expect_true(all(result_smooth$lower <= result_smooth$observed_rate))
  expect_true(all(result_smooth$upper >= result_smooth$observed_rate))

  # Test that predictions span the range of input data
  expect_equal(min(result_logistic$predicted_rate), min(test_data$pred))
  expect_equal(max(result_logistic$predicted_rate), max(test_data$pred))
})

test_that("check_model_calibration logistic method handles different confidence levels", {
  set.seed(123)
  test_data <- data.frame(
    pred = runif(100, 0, 1),
    obs = rbinom(100, 1, 0.5)
  )

  # Test with 90% confidence
  result_90 <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    conf_level = 0.90
  ))

  # Test with 99% confidence
  result_99 <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    conf_level = 0.99
  ))

  # 99% CI should be wider than 90% CI
  expect_true(
    mean(result_99$upper - result_99$lower) >
      mean(result_90$upper - result_90$lower)
  )
})

test_that("check_model_calibration logistic method handles perfect separation", {
  # Create data with perfect separation
  test_data <- data.frame(
    pred = c(rep(0.1, 50), rep(0.9, 50)),
    obs = c(rep(0, 50), rep(1, 50))
  )

  # Should not error with perfect separation
  expect_no_error({
    result <- check_model_calibration(
      test_data,
      pred,
      obs,
      method = "logistic",
      smooth = FALSE
    )
  })
})

test_that("check_model_calibration works with windowed method", {
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
  result_windowed <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.2,
    step_size = 0.1
  ))

  expect_s3_class(result_windowed, "tbl_df")
  expect_true(all(
    c("predicted_rate", "observed_rate", "lower", "upper") %in%
      names(result_windowed)
  ))
  expect_true(nrow(result_windowed) > 0)
  expect_true(all(
    result_windowed$predicted_rate >= 0 & result_windowed$predicted_rate <= 1
  ))
  expect_true(all(
    result_windowed$observed_rate >= 0 & result_windowed$observed_rate <= 1
  ))
  expect_true(all(result_windowed$lower >= 0 & result_windowed$lower <= 1))
  expect_true(all(result_windowed$upper >= 0 & result_windowed$upper <= 1))
  expect_true(all(result_windowed$lower <= result_windowed$observed_rate))
  expect_true(all(result_windowed$upper >= result_windowed$observed_rate))

  # Test with different window sizes
  result_small_window <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.05,
    step_size = 0.025
  ))

  expect_true(nrow(result_small_window) > nrow(result_windowed))

  # Test window centers
  expected_centers <- seq(0, 1, by = 0.1)
  expect_equal(result_windowed$predicted_rate, expected_centers)
})

test_that("check_model_calibration windowed method handles edge cases", {
  set.seed(123)

  # Test with data concentrated at edges
  test_data <- data.frame(
    pred = c(runif(50, 0, 0.1), runif(50, 0.9, 1)),
    obs = rbinom(100, 1, 0.5)
  )

  result <- check_model_calibration(
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
  result_tiny <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.01,
    step_size = 0.1
  ))

  # Some windows might be empty
  expect_true(nrow(result_tiny) <= length(seq(0, 1, by = 0.1)))
})

test_that("check_model_calibration windowed method respects window boundaries", {
  # Create data only in middle range
  test_data <- data.frame(
    pred = runif(100, 0.4, 0.6),
    obs = rbinom(100, 1, 0.5)
  )

  result <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.1,
    step_size = 0.1
  )

  # Windows at 0 and 1 should have no data (depending on window size)
  # But windows around 0.5 should have data
  center_window <- result[result$predicted_rate == 0.5, ]
  expect_equal(nrow(center_window), 1)
  expect_true(center_window$observed_rate >= 0)
})

test_that("check_model_calibration method parameter validation", {
  test_data <- data.frame(pred = runif(50), obs = rbinom(50, 1, 0.5))

  # Test invalid method
  expect_halfmoon_error(
    check_model_calibration(test_data, pred, obs, method = "invalid")
  )
})

test_that("check_model_calibration handles edge cases with all methods", {
  # Empty data
  empty_data <- data.frame(pred = numeric(0), obs = numeric(0))

  result_breaks <- check_model_calibration(
    empty_data,
    pred,
    obs,
    method = "breaks"
  )
  expect_equal(nrow(result_breaks), 0)
  expect_true("count" %in% names(result_breaks))
  expect_true(".bin" %in% names(result_breaks))

  result_logistic <- check_model_calibration(
    empty_data,
    pred,
    obs,
    method = "logistic"
  )
  expect_equal(nrow(result_logistic), 0)
  expect_false("count" %in% names(result_logistic))
  expect_false(".bin" %in% names(result_logistic))

  result_windowed <- check_model_calibration(
    empty_data,
    pred,
    obs,
    method = "windowed"
  )
  expect_equal(nrow(result_windowed), 0)
  expect_false("count" %in% names(result_windowed))
  expect_false(".bin" %in% names(result_windowed))
})

test_that("check_model_calibration handles all zeros and all ones", {
  set.seed(123)

  # All zeros - when treatment_level is not specified, 0 becomes the treatment level
  # so observed_rate will be 1 (all observations match treatment level)
  zeros_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = rep(0, 50)
  )

  # When all observations are 0, default treatment_level will be 0
  # Test the actual behavior
  result_breaks_zeros_default <- suppress_calibration_warnings(check_model_calibration(
    zeros_data,
    pred,
    obs,
    method = "breaks"
  ))
  # All values are 0 and treatment_level=0, so observed_rate=1
  expect_true(all(result_breaks_zeros_default$observed_rate == 1))

  # Now test with mixed data where 1 is the treatment level
  mixed_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = c(rep(0, 25), rep(1, 25))
  )

  result_mixed <- suppress_calibration_warnings(check_model_calibration(
    mixed_data,
    pred,
    obs,
    method = "breaks",
    treatment_level = 1
  ))
  # This should give us meaningful calibration metrics
  expect_true(all(
    result_mixed$observed_rate >= 0 & result_mixed$observed_rate <= 1
  ))

  # All ones - default treatment level will be 1, so observed_rate = 1
  ones_data <- data.frame(
    pred = runif(50, 0, 1),
    obs = rep(1, 50)
  )

  result_breaks_ones <- suppress_calibration_warnings(check_model_calibration(
    ones_data,
    pred,
    obs,
    method = "breaks"
  ))
  expect_true(all(result_breaks_ones$observed_rate == 1))

  result_windowed_ones <- suppress_calibration_warnings(check_model_calibration(
    ones_data,
    pred,
    obs,
    method = "windowed"
  ))
  expect_true(all(result_windowed_ones$observed_rate == 1))

  # Test the default behavior with all zeros
  result_default_zeros <- suppress_calibration_warnings(check_model_calibration(
    zeros_data,
    pred,
    obs,
    method = "breaks"
  ))
  # With default treatment_level, all zeros means treatment_level=0, so observed_rate=1
  expect_true(all(result_default_zeros$observed_rate == 1))
})

test_that("check_model_calibration handles NA values correctly", {
  # Create data with NAs
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(45), rep(NA, 5)),
    obs = c(rbinom(45, 1, 0.5), rep(NA, 5))
  )

  # Test with na.rm = FALSE (default)
  result_false <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    na.rm = FALSE
  ))

  # Test with na.rm = TRUE
  result_true <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    na.rm = TRUE
  ))

  # na.rm = TRUE should have data, na.rm = FALSE might have less
  expect_true(sum(result_true$count) == 45)
})

test_that("check_model_calibration handles factor treatment variables", {
  set.seed(123)
  test_data <- data.frame(
    pred = runif(100, 0, 1),
    obs = factor(rbinom(100, 1, 0.5), levels = c("0", "1"))
  )

  # Should work with factor
  result <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    treatment_level = "1"
  ))

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$observed_rate >= 0 & result$observed_rate <= 1))
})

test_that("check_model_calibration validates input parameters", {
  test_data <- data.frame(pred = runif(50), obs = rbinom(50, 1, 0.5))

  # Invalid bins for breaks method
  expect_halfmoon_error(
    check_model_calibration(test_data, pred, obs, method = "breaks", bins = 1),
    "halfmoon_arg_error"
  )

  # Non-integer bins
  expect_halfmoon_error(
    check_model_calibration(
      test_data,
      pred,
      obs,
      method = "breaks",
      bins = 2.5
    ),
    "halfmoon_arg_error"
  )

  # Missing column
  expect_halfmoon_error(
    check_model_calibration(test_data, nonexistent, obs),
    "halfmoon_column_error"
  )
})

test_that("check_model_calibration produces consistent results across methods", {
  # Create well-calibrated data
  set.seed(123)
  n <- 500
  pred <- runif(n, 0, 1)
  obs <- rbinom(n, 1, pred) # Perfect calibration on average

  test_data <- data.frame(pred = pred, obs = obs)

  # Get results from all methods
  result_breaks <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    bins = 5
  )
  result_logistic <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic"
  ))
  result_windowed <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    window_size = 0.2
  ))

  # All methods should show reasonable calibration (observed_rate ≈ predicted_rate)
  # For breaks method
  breaks_diff <- mean(abs(
    result_breaks$observed_rate - result_breaks$predicted_rate
  ))
  expect_true(breaks_diff < 0.2) # Allow some deviation

  # For windowed method
  windowed_diff <- mean(abs(
    result_windowed$observed_rate - result_windowed$predicted_rate
  ))
  expect_true(windowed_diff < 0.2)
})

test_that("check_model_calibration provides helpful warnings for small cell sizes", {
  # Create test data with imbalanced distribution to create small cells
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    pred = c(runif(90, 0, 0.5), runif(10, 0.9, 1)), # Most values in lower range
    obs = rbinom(n, 1, 0.3)
  )

  # Test breaks method with small cells
  expect_halfmoon_warning(
    check_model_calibration(test_data, pred, obs, method = "breaks", bins = 10),
    class = "halfmoon_data_warning"
  )
})

test_that("check_model_calibration provides helpful warnings for extreme proportions", {
  # Create data where some bins have all 0s or all 1s
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(50, 0, 0.3), runif(50, 0.7, 1)),
    obs = c(rep(0, 50), rep(1, 50)) # Perfect separation
  )

  expect_halfmoon_warning(
    check_model_calibration(test_data, pred, obs, method = "breaks", bins = 10),
    "halfmoon_data_warning"
  )
})

test_that("check_model_calibration windowed method provides helpful warnings", {
  # Create sparse data
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(20, 0.4, 0.6)), # Concentrated in middle
    obs = rbinom(20, 1, 0.5)
  )

  # Small window size will create windows with few observations
  expect_halfmoon_warning(
    check_model_calibration(
      test_data,
      pred,
      obs,
      method = "windowed",
      window_size = 0.05,
      step_size = 0.1
    ),
    "halfmoon_data_warning"
  )

  # Capture the specific warning message
  warning_msg <- capture_warnings(
    check_model_calibration(
      test_data,
      pred,
      obs,
      method = "windowed",
      window_size = 0.05,
      step_size = 0.1
    )
  )

  expect_match(warning_msg[1], "windows centered at")
  expect_match(warning_msg[1], "Consider using a larger window size")
})

test_that("check_model_calibration doesn't warn for adequate sample sizes", {
  # Create well-distributed data
  set.seed(123)
  n <- 1000
  test_data <- data.frame(
    pred = runif(n, 0, 1),
    obs = rbinom(n, 1, 0.5)
  )

  # Should not produce warnings
  expect_no_warning(
    check_model_calibration(test_data, pred, obs, method = "breaks", bins = 10)
  )

  expect_no_warning(
    check_model_calibration(
      test_data,
      pred,
      obs,
      method = "windowed",
      window_size = 0.2,
      step_size = 0.1
    )
  )
})

test_that("warnings include specific bin/window information", {
  # Create data that will have problems in specific bins
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(95, 0, 0.8), runif(5, 0.9, 1)), # Very few in last bin
    obs = rbinom(100, 1, 0.3)
  )

  warning_msg <- capture_warnings(
    check_model_calibration(test_data, pred, obs, method = "breaks", bins = 10)
  )

  # Should mention specific bins
  expect_match(warning_msg[1], "bins [0-9, ]+")

  # For windowed method
  test_data2 <- data.frame(
    pred = runif(30, 0.4, 0.6),
    obs = rbinom(30, 1, 0.5)
  )

  warning_msg2 <- capture_warnings(
    check_model_calibration(
      test_data2,
      pred,
      obs,
      method = "windowed",
      window_size = 0.1,
      step_size = 0.1
    )
  )

  # Should mention specific window centers
  expect_match(warning_msg2[1], "windows centered at [0-9., ]+")
})

test_that("warnings handle edge case with no valid results gracefully", {
  # Empty data
  empty_data <- data.frame(pred = numeric(0), obs = numeric(0))

  # Should not produce warnings for empty data
  expect_no_warning(
    check_model_calibration(empty_data, pred, obs, method = "breaks")
  )

  expect_no_warning(
    check_model_calibration(empty_data, pred, obs, method = "windowed")
  )
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
  p_breaks <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
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
  p_logistic <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
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
  p_windowed <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
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
  p_no_ribbon <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
    geom_calibration(method = "breaks", show_ribbon = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    lims(x = c(0, 1), y = c(0, 1))

  # Test with points hidden
  p_no_points <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
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
  p_90 <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
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

  p_nhefs <- ggplot(cal_data, aes(estimate = .fitted, truth = binary_outcome)) +
    geom_calibration(method = "breaks", bins = 8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Propensity Score", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_nhefs, "ggplot")
  expect_doppelganger("calibration nhefs data", p_nhefs)
})

test_that("geom_calibration works with both numeric and factor outcomes", {
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    pred = runif(n, 0, 1)
  )
  test_data$obs_numeric <- rbinom(n, 1, test_data$pred)
  test_data$obs_factor <- factor(test_data$obs_numeric, levels = c(0, 1))

  # Test with numeric outcome
  p_numeric <- ggplot(test_data, aes(estimate = pred, truth = obs_numeric)) +
    geom_calibration(method = "breaks", bins = 5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Calibration with numeric outcome")

  # Test with factor outcome
  p_factor <- ggplot(test_data, aes(estimate = pred, truth = obs_factor)) +
    geom_calibration(method = "breaks", bins = 5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(title = "Calibration with factor outcome")

  # Both should work
  expect_s3_class(p_numeric, "ggplot")
  expect_s3_class(p_factor, "ggplot")
  expect_no_error(suppressWarnings(ggplot_build(p_numeric)))
  expect_no_error(suppressWarnings(ggplot_build(p_factor)))

  # Visual tests
  skip_on_ci()
  expect_doppelganger("calibration-numeric-outcome", p_numeric)
  expect_doppelganger("calibration-factor-outcome", p_factor)
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
  p <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
    geom_calibration(method = "invalid_method")

  # We expect a warning with the halfmoon_method_warning class
  # Use nested expect_warning to properly capture the warning without it leaking to test output
  expect_warning(
    expect_warning(
      ggplot_build(p),
      class = "halfmoon_method_warning",
      regexp = "Invalid calibration method"
    )
  )
})

test_that("check_model_calibration errors with invalid bins", {
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
  expect_halfmoon_error(
    check_model_calibration(cal_data, pred, obs, bins = 1)
  )

  expect_halfmoon_error(
    check_model_calibration(cal_data, pred, obs, bins = 2.5)
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
  p_smooth <- ggplot(cal_data, aes(estimate = pred, truth = obs)) +
    geom_calibration(method = "logistic", smooth = TRUE, show_points = FALSE) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  expect_s3_class(p_smooth, "ggplot")
  expect_doppelganger("calibration logistic smooth", p_smooth)
})

test_that("k parameter works in check_calibration", {
  # Create test data with non-linear relationship
  set.seed(123)
  n <- 500
  x <- runif(n, 0, 1)
  # Create a wavy pattern that would benefit from different k values
  true_prob <- plogis(10 * sin(2 * pi * x))
  actual <- rbinom(n, 1, true_prob)

  test_data <- data.frame(
    pred = x,
    obs = actual
  )

  # Test with default k = 10
  result_k10 <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = TRUE,
    k = 10
  )

  # Test with smaller k = 5
  result_k5 <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = TRUE,
    k = 5
  )

  # Test with larger k = 20
  result_k20 <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = TRUE,
    k = 20
  )

  # All should return valid results
  expect_s3_class(result_k10, "tbl_df")
  expect_s3_class(result_k5, "tbl_df")
  expect_s3_class(result_k20, "tbl_df")

  # All should have the same number of rows (100 prediction points)
  expect_equal(nrow(result_k10), 100)
  expect_equal(nrow(result_k5), 100)
  expect_equal(nrow(result_k20), 100)

  # Results should be different (different smoothness)
  # Can't test exact values but can verify structure
  expect_true(all(result_k5$observed_rate >= 0 & result_k5$observed_rate <= 1))
  expect_true(all(
    result_k10$observed_rate >= 0 & result_k10$observed_rate <= 1
  ))
  expect_true(all(
    result_k20$observed_rate >= 0 & result_k20$observed_rate <= 1
  ))
})

test_that("k parameter is ignored when smooth = FALSE", {
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # Results should be identical when smooth = FALSE
  result1 <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = FALSE,
    k = 5
  )

  result2 <- check_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    smooth = FALSE,
    k = 20
  )

  # Results should be identical
  expect_equal(result1$observed_rate, result2$observed_rate)
  expect_equal(result1$lower, result2$lower)
  expect_equal(result1$upper, result2$upper)
})

test_that("k parameter is ignored for non-logistic methods", {
  set.seed(123)
  n <- 200
  predicted <- runif(n, 0, 1)
  actual <- rbinom(n, 1, predicted)

  test_data <- data.frame(
    pred = predicted,
    obs = actual
  )

  # k should be ignored for breaks method
  result1 <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    k = 5
  ))

  result2 <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "breaks",
    k = 20
  ))

  # Results should be identical
  expect_equal(result1$observed_rate, result2$observed_rate)

  # k should be ignored for windowed method
  result3 <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    k = 5
  ))

  result4 <- suppress_calibration_warnings(check_model_calibration(
    test_data,
    pred,
    obs,
    method = "windowed",
    k = 20
  ))

  # Results should be identical
  expect_equal(result3$observed_rate, result4$observed_rate)
})

test_that("k parameter works in geom_calibration", {
  # Create test data
  set.seed(123)
  n <- 500
  x <- runif(n, 0, 1)
  true_prob <- plogis(10 * sin(2 * pi * x))
  actual <- rbinom(n, 1, true_prob)

  test_data <- data.frame(
    pred = x,
    obs = actual
  )

  # Create plots with different k values
  p_k5 <- ggplot(test_data, aes(estimate = pred, truth = obs)) +
    geom_calibration(method = "logistic", smooth = TRUE, k = 5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  p_k10 <- ggplot(test_data, aes(estimate = pred, truth = obs)) +
    geom_calibration(method = "logistic", smooth = TRUE, k = 10) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  p_k20 <- ggplot(test_data, aes(estimate = pred, truth = obs)) +
    geom_calibration(method = "logistic", smooth = TRUE, k = 20) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(x = "Predicted Probability", y = "Observed Rate") +
    lims(x = c(0, 1), y = c(0, 1))

  # Test with vdiffr
  expect_doppelganger("calibration k=5", p_k5)
  expect_doppelganger("calibration k=10", p_k10)
  expect_doppelganger("calibration k=20", p_k20)
})

test_that("k parameter works in plot_calibration", {
  # Create test data
  set.seed(123)
  n <- 500
  x <- runif(n, 0, 1)
  true_prob <- plogis(10 * sin(2 * pi * x))
  actual <- rbinom(n, 1, true_prob)

  test_data <- data.frame(
    pred = x,
    obs = actual
  )

  # Create plots with different k values
  p_k5 <- plot_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    k = 5
  )
  p_k10 <- plot_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    k = 10
  )
  p_k20 <- plot_model_calibration(
    test_data,
    pred,
    obs,
    method = "logistic",
    k = 20
  )

  # Test with vdiffr
  expect_doppelganger("plot_calibration k=5", p_k5)
  expect_doppelganger("plot_calibration k=10", p_k10)
  expect_doppelganger("plot_calibration k=20", p_k20)
})

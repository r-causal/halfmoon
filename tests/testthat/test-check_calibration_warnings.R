library(testthat)

test_that("check_calibration provides helpful warnings for small cell sizes", {
  # Create test data with imbalanced distribution to create small cells
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    pred = c(runif(90, 0, 0.5), runif(10, 0.9, 1)),  # Most values in lower range
    obs = rbinom(n, 1, 0.3)
  )
  
  # Test breaks method with small cells
  expect_warning(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 10),
    "Small sample sizes or extreme proportions detected"
  )
  
  # Capture the specific warning message
  warning_msg <- capture_warnings(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 10)
  )
  
  expect_match(warning_msg[1], "bins")
  expect_match(warning_msg[1], "n =")
  expect_match(warning_msg[1], "Consider using fewer bins")
})

test_that("check_calibration provides helpful warnings for extreme proportions", {
  # Create data where some bins have all 0s or all 1s
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(50, 0, 0.3), runif(50, 0.7, 1)),
    obs = c(rep(0, 50), rep(1, 50))  # Perfect separation
  )
  
  expect_warning(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 10),
    "Small sample sizes or extreme proportions detected"
  )
})

test_that("check_calibration windowed method provides helpful warnings", {
  # Create sparse data
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(20, 0.4, 0.6)),  # Concentrated in middle
    obs = rbinom(20, 1, 0.5)
  )
  
  # Small window size will create windows with few observations
  expect_warning(
    check_calibration(test_data, pred, obs, method = "windowed", 
                     window_size = 0.05, step_size = 0.1),
    "Small sample sizes or extreme proportions detected in windows"
  )
  
  # Capture the specific warning message
  warning_msg <- capture_warnings(
    check_calibration(test_data, pred, obs, method = "windowed", 
                     window_size = 0.05, step_size = 0.1)
  )
  
  expect_match(warning_msg[1], "windows centered at")
  expect_match(warning_msg[1], "Consider using a larger window size")
})

test_that("check_calibration doesn't warn for adequate sample sizes", {
  # Create well-distributed data
  set.seed(123)
  n <- 1000
  test_data <- data.frame(
    pred = runif(n, 0, 1),
    obs = rbinom(n, 1, 0.5)
  )
  
  # Should not produce warnings
  expect_no_warning(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 10)
  )
  
  expect_no_warning(
    check_calibration(test_data, pred, obs, method = "windowed", 
                     window_size = 0.2, step_size = 0.1)
  )
})

test_that("plot_calibration shows helpful warnings", {
  # Create test data with small cells
  set.seed(123)
  test_data <- data.frame(
    .fitted = c(runif(90, 0, 0.5), runif(10, 0.9, 1)),
    qsmk = rbinom(100, 1, 0.3)
  )
  
  # Should produce the helpful warning (may appear multiple times due to layers)
  suppressWarnings({
    expect_warning(
      plot_calibration(test_data, .fitted, qsmk),
      "Small sample sizes or extreme proportions detected"
    )
  })
})

test_that("warnings include specific bin/window information", {
  # Create data that will have problems in specific bins
  set.seed(123)
  test_data <- data.frame(
    pred = c(runif(95, 0, 0.8), runif(5, 0.9, 1)),  # Very few in last bin
    obs = rbinom(100, 1, 0.3)
  )
  
  warning_msg <- capture_warnings(
    check_calibration(test_data, pred, obs, method = "breaks", bins = 10)
  )
  
  # Should mention specific bins
  expect_match(warning_msg[1], "bins [0-9, ]+")
  
  # For windowed method
  test_data2 <- data.frame(
    pred = runif(30, 0.4, 0.6),
    obs = rbinom(30, 1, 0.5)
  )
  
  warning_msg2 <- capture_warnings(
    check_calibration(test_data2, pred, obs, method = "windowed", 
                     window_size = 0.1, step_size = 0.1)
  )
  
  # Should mention specific window centers
  expect_match(warning_msg2[1], "windows centered at [0-9., ]+")
})

test_that("warnings handle edge case with no valid results gracefully", {
  # Empty data
  empty_data <- data.frame(pred = numeric(0), obs = numeric(0))
  
  # Should not produce warnings for empty data
  expect_no_warning(
    check_calibration(empty_data, pred, obs, method = "breaks")
  )
  
  expect_no_warning(
    check_calibration(empty_data, pred, obs, method = "windowed")
  )
})
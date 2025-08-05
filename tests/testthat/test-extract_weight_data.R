# Tests for extract_weight_data() utility function

test_that("extract_weight_data handles NULL input", {
  result <- extract_weight_data(NULL)
  expect_null(result)
})

test_that("extract_weight_data passes through numeric vectors unchanged", {
  # Test simple numeric vector
  numeric_weights <- c(1, 2, 3, 4, 5)
  result <- extract_weight_data(numeric_weights)
  expect_identical(result, numeric_weights)
  expect_type(result, "double")
  
  # Test with different numeric values
  uniform_weights <- runif(10, 0.5, 1.5)
  result2 <- extract_weight_data(uniform_weights)
  expect_identical(result2, uniform_weights)
  
  # Test with extreme values
  extreme_weights <- c(0.001, 100, 0.5, 2.5)
  result3 <- extract_weight_data(extreme_weights)
  expect_identical(result3, extreme_weights)
})

test_that("extract_weight_data extracts data from psw objects", {
  skip_if_not_installed("propensity")
  
  # Create psw object from numeric data
  numeric_data <- c(1.2, 0.8, 1.5, 0.9, 1.1)
  psw_obj <- propensity::psw(numeric_data, estimand = "ate")
  
  # Extract should return the underlying numeric data
  result <- extract_weight_data(psw_obj)
  expect_equal(result, numeric_data)
  expect_type(result, "double")
  
  # Should not have psw class anymore
  expect_false(propensity::is_psw(result))
})

test_that("extract_weight_data preserves values from different psw estimands", {
  skip_if_not_installed("propensity")
  
  numeric_data <- c(2.1, 1.3, 0.7, 1.8, 1.0)
  
  # Test different estimands produce same numeric extraction
  psw_ate <- propensity::psw(numeric_data, estimand = "ate")
  psw_att <- propensity::psw(numeric_data, estimand = "att")
  psw_ato <- propensity::psw(numeric_data, estimand = "ato")
  
  result_ate <- extract_weight_data(psw_ate)
  result_att <- extract_weight_data(psw_att)
  result_ato <- extract_weight_data(psw_ato)
  
  expect_equal(result_ate, numeric_data)
  expect_equal(result_att, numeric_data)
  expect_equal(result_ato, numeric_data)
  
  # All should be identical
  expect_identical(result_ate, result_att)
  expect_identical(result_ate, result_ato)
})

test_that("extract_weight_data works with integer input", {
  # Test with integer vector (passes through unchanged)
  int_weights <- c(1L, 2L, 3L, 4L)
  result <- extract_weight_data(int_weights)
  expect_equal(result, c(1L, 2L, 3L, 4L))
  expect_type(result, "integer")
})

test_that("extract_weight_data preserves NA values", {
  # Test with NA values in numeric vector
  numeric_with_na <- c(1.2, NA, 0.8, NA, 1.5)
  result <- extract_weight_data(numeric_with_na)
  expect_identical(result, numeric_with_na)
  expect_equal(sum(is.na(result)), 2)
})

test_that("extract_weight_data works with edge case values", {
  # Test with zeros, very small, and very large values
  edge_weights <- c(0, 1e-10, 1e10, Inf, -Inf)
  result <- extract_weight_data(edge_weights)
  expect_identical(result, edge_weights)
  
  # Test with single value
  single_weight <- 1.5
  result_single <- extract_weight_data(single_weight)
  expect_identical(result_single, single_weight)
})
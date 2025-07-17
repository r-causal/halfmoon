# Comprehensive tests for compute_balance.R functions

# Test data using nhefs_weights from halfmoon package and additional synthetic scenarios
get_nhefs_compute_data <- function() {
  data(nhefs_weights, package = "halfmoon")

  # Use first 300 rows for faster testing
  nhefs_subset <- nhefs_weights[1:300, ]

  # Add test weights and special cases
  set.seed(123)
  nhefs_subset$w_uniform <- runif(nrow(nhefs_subset), 0.5, 1.5)
  nhefs_subset$w_extreme <- rep(c(0.01, 10), length.out = nrow(nhefs_subset))

  return(nhefs_subset)
}

# Legacy synthetic data generator for edge cases
create_test_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  list(
    x_cont = rnorm(n, mean = 10, sd = 2),
    x_binary = rbinom(n, 1, 0.3),
    x_skewed = rexp(n, rate = 0.5),
    x_zero_var = rep(5, n),
    g_balanced = rbinom(n, 1, 0.5),
    g_unbalanced = rbinom(n, 1, 0.2),
    g_factor = factor(
      rbinom(n, 1, 0.5),
      levels = c(0, 1),
      labels = c("control", "treated")
    ),
    w_uniform = runif(n, 0.5, 1.5),
    w_extreme = c(rep(0.01, n / 2), rep(10, n / 2)),
    na_indices = sample(1:n, size = n * 0.1)
  )
}

# =============================================================================
# TESTS FOR compute_smd()
# =============================================================================

test_that("compute_smd matches smd::smd estimate", {
  set.seed(1)
  x <- rnorm(100)
  g <- factor(sample(c(0, 1), 100, replace = TRUE))

  out_pkg <- compute_smd(covariate = x, group = g, reference_group = 1)
  out_base <- smd::smd(x, g, gref = 1)$estimate

  expect_equal(out_pkg, out_base)
})

test_that("compute_smd handles different reference groups", {
  data <- create_test_data()

  # Test with numeric reference groups
  smd_ref0 <- compute_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 0
  )
  smd_ref1 <- compute_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 1
  )

  expect_equal(smd_ref0, -smd_ref1, tolerance = 1e-10)

  # Test with factor reference groups
  smd_control <- compute_smd(
    covariate = data$x_cont,
    group = data$g_factor,
    reference_group = "control"
  )
  smd_treated <- compute_smd(
    covariate = data$x_cont,
    group = data$g_factor,
    reference_group = "treated"
  )

  expect_equal(smd_control, -smd_treated, tolerance = 1e-10)
})

test_that("compute_smd handles weights", {
  data <- create_test_data()

  # Weighted vs unweighted should generally be different
  smd_unweighted <- compute_smd(
    covariate = data$x_cont,
    group = data$g_balanced
  )
  smd_weighted <- compute_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )

  expect_false(identical(smd_unweighted, smd_weighted))

  # Both should be finite numbers
  expect_true(is.finite(smd_unweighted))
  expect_true(is.finite(smd_weighted))
})

test_that("compute_smd handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA
  g_na <- data$g_balanced
  w_na <- data$w_uniform

  # Should return NA when na.rm = FALSE
  expect_true(is.na(compute_smd(covariate = x_na, group = g_na, na_rm = FALSE)))

  # Should work when na.rm = TRUE
  smd_na_rm <- compute_smd(covariate = x_na, group = g_na, na_rm = TRUE)
  expect_true(is.finite(smd_na_rm))
})

test_that("compute_smd error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_error(compute_smd(covariate = data$x_cont, group = rep(1, 100)))
  expect_error(compute_smd(
    covariate = data$x_cont,
    group = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_error(compute_smd(
    covariate = data$x_cont[1:50],
    group = data$g_balanced
  ))
  expect_error(compute_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR compute_variance_ratio()
# =============================================================================

test_that("compute_variance_ratio handles basic cases", {
  data <- create_test_data()

  # Basic functionality
  vr <- compute_variance_ratio(covariate = data$x_cont, group = data$g_balanced)
  expect_true(is.finite(vr))
  expect_true(vr > 0)

  # With weights
  vr_weighted <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(vr_weighted))
  expect_true(vr_weighted > 0)
})

test_that("compute_variance_ratio handles reference groups", {
  data <- create_test_data()

  # Different reference groups should give reciprocal results
  vr_ref0 <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 0
  )
  vr_ref1 <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 1
  )

  expect_equal(vr_ref0, 1 / vr_ref1, tolerance = 1e-10)
})

test_that("compute_variance_ratio handles binary variables", {
  data <- create_test_data()

  # Binary variables should use p*(1-p) variance formula
  vr_binary <- compute_variance_ratio(
    covariate = data$x_binary,
    group = data$g_balanced
  )
  expect_true(is.finite(vr_binary))
  expect_true(vr_binary > 0)

  # With weights
  vr_binary_weighted <- compute_variance_ratio(
    covariate = data$x_binary,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(vr_binary_weighted))
  expect_true(vr_binary_weighted > 0)
})

test_that("compute_variance_ratio handles edge cases", {
  data <- create_test_data()

  # Zero variance scenarios
  x_zero <- c(rep(1, 50), rep(1, 50))
  g <- c(rep(0, 50), rep(1, 50))

  vr_zero_both <- compute_variance_ratio(covariate = x_zero, group = g)
  expect_equal(vr_zero_both, 1)

  # One group with zero variance
  x_mixed <- c(rep(1, 50), rnorm(50))
  vr_zero_one <- compute_variance_ratio(covariate = x_mixed, group = g)
  expect_true(vr_zero_one == 0 || vr_zero_one == Inf)
})

test_that("compute_variance_ratio handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Should return NA when na.rm = FALSE
  expect_equal(
    compute_variance_ratio(
      covariate = x_na,
      group = data$g_balanced,
      na_rm = FALSE
    ),
    NA_real_
  )

  # Should work when na.rm = TRUE if enough data remains
  vr_na_rm <- compute_variance_ratio(
    covariate = x_na,
    group = data$g_balanced,
    na_rm = TRUE
  )
  expect_true(is.finite(vr_na_rm) || is.na(vr_na_rm))
})

test_that("compute_variance_ratio error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_error(compute_variance_ratio(
    covariate = data$x_cont,
    group = rep(1, 100)
  ))
  expect_error(compute_variance_ratio(
    covariate = data$x_cont,
    group = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_error(compute_variance_ratio(
    covariate = data$x_cont[1:50],
    group = data$g_balanced
  ))
  expect_error(compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR compute_ks()
# =============================================================================

test_that("compute_ks handles basic cases", {
  data <- create_test_data()

  # Basic functionality
  ks <- compute_ks(covariate = data$x_cont, group = data$g_balanced)
  expect_true(is.finite(ks))
  expect_true(ks >= 0)
  expect_true(ks <= 1)

  # With weights
  ks_weighted <- compute_ks(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(ks_weighted))
  expect_true(ks_weighted >= 0)
  expect_true(ks_weighted <= 1)
})

test_that("compute_ks gives 0 for identical distributions", {
  # Identical distributions should give KS = 0
  x <- c(1, 2, 3, 1, 2, 3)
  g <- c(0, 0, 0, 1, 1, 1)

  ks_identical <- compute_ks(covariate = x, group = g)
  expect_equal(ks_identical, 0)
})

test_that("compute_ks gives >0 for different distributions", {
  # Different distributions should give KS > 0
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c(0, 0, 0, 1, 1, 1)

  ks_different <- compute_ks(covariate = x, group = g)
  expect_true(ks_different > 0)
})

test_that("compute_ks handles binary variables", {
  data <- create_test_data()

  # Binary variables should return difference in proportions
  ks_binary <- compute_ks(covariate = data$x_binary, group = data$g_balanced)
  expect_true(is.finite(ks_binary))
  expect_true(ks_binary >= 0)
  expect_true(ks_binary <= 1)

  # Should equal absolute difference in proportions
  prop_0 <- mean(data$x_binary[data$g_balanced == 0])
  prop_1 <- mean(data$x_binary[data$g_balanced == 1])
  expected_ks <- abs(prop_1 - prop_0)

  expect_equal(ks_binary, expected_ks, tolerance = 1e-10)
})

test_that("compute_ks handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Should return NA when na.rm = FALSE
  expect_equal(
    compute_ks(covariate = x_na, group = data$g_balanced, na_rm = FALSE),
    NA_real_
  )

  # Should work when na.rm = TRUE if enough data remains
  ks_na_rm <- compute_ks(
    covariate = x_na,
    group = data$g_balanced,
    na_rm = TRUE
  )
  expect_true(is.finite(ks_na_rm) || is.na(ks_na_rm))
})

test_that("compute_ks error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_error(compute_ks(covariate = data$x_cont, group = rep(1, 100)))
  expect_error(compute_ks(
    covariate = data$x_cont,
    group = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_error(compute_ks(
    covariate = data$x_cont[1:50],
    group = data$g_balanced
  ))
  expect_error(compute_ks(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR compute_correlation()
# =============================================================================

test_that("compute_correlation matches stats::cor when unweighted", {
  x <- 1:10
  y <- 2 * x + rnorm(10, sd = 0.1)

  cor_ours <- compute_correlation(x, y)
  cor_stats <- stats::cor(x, y)

  expect_equal(cor_ours, cor_stats)
})

test_that("compute_correlation handles weights correctly", {
  x <- c(0, 1, 0, 1)
  y <- c(0, 0, 1, 1)
  w <- c(1, 0, 0, 1)

  # Weighted on matching pairs (0,0) and (1,1) -> perfect correlation
  cor_weighted <- compute_correlation(x, y, weights = w)
  expect_equal(cor_weighted, 1)
})

test_that("compute_correlation handles various scenarios", {
  data <- create_test_data()

  # Basic correlation
  cor_basic <- compute_correlation(data$x_cont, data$x_skewed)
  expect_true(is.finite(cor_basic))
  expect_true(cor_basic >= -1 && cor_basic <= 1)

  # Weighted correlation
  cor_weighted <- compute_correlation(
    data$x_cont,
    data$x_skewed,
    weights = data$w_uniform
  )
  expect_true(is.finite(cor_weighted))
  expect_true(cor_weighted >= -1 && cor_weighted <= 1)

  # Perfect correlation
  x_perfect <- 1:100
  y_perfect <- 2 * x_perfect + 5
  cor_perfect <- compute_correlation(x_perfect, y_perfect)
  expect_equal(cor_perfect, 1, tolerance = 1e-10)

  # No correlation
  set.seed(123)
  x_uncorr <- rnorm(100)
  y_uncorr <- rnorm(100)
  cor_uncorr <- compute_correlation(x_uncorr, y_uncorr)
  expect_true(abs(cor_uncorr) < 0.5) # Should be close to 0
})

test_that("compute_correlation handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Should return NA when na.rm = FALSE
  expect_equal(
    compute_correlation(x_na, data$x_skewed, na_rm = FALSE),
    NA_real_
  )

  # Should work when na.rm = TRUE
  cor_na_rm <- compute_correlation(x_na, data$x_skewed, na_rm = TRUE)
  expect_true(is.finite(cor_na_rm))
})

test_that("compute_correlation handles edge cases", {
  # Zero variance should return NA with warning
  x_zero <- rep(1, 100)
  y_normal <- rnorm(100)

  expect_warning(
    {
      cor_zero <- compute_correlation(x_zero, y_normal)
      expect_true(is.na(cor_zero))
    },
    "the standard deviation is zero"
  )

  # Both zero variance should return NA with warning
  y_zero <- rep(2, 100)
  expect_warning(
    {
      cor_both_zero <- compute_correlation(x_zero, y_zero)
      expect_true(is.na(cor_both_zero))
    },
    "the standard deviation is zero"
  )
})

test_that("compute_correlation error handling", {
  data <- create_test_data()

  # Should error with mismatched lengths
  expect_error(compute_correlation(data$x_cont[1:50], data$x_skewed))
  expect_error(compute_correlation(
    data$x_cont,
    data$x_skewed,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR is_binary() helper function
# =============================================================================

test_that("is_binary correctly identifies binary variables", {
  # Binary variables
  expect_true(is_binary(c(0, 1, 0, 1, 0)))
  expect_true(is_binary(c(0, 0, 1, 1, 1)))
  expect_true(is_binary(c(1, 1, 1, 0, 0)))

  # Non-binary variables
  expect_false(is_binary(c(0, 1, 2)))
  expect_false(is_binary(c(1, 2, 3, 4)))
  expect_false(is_binary(c(0.5, 1.5)))
  expect_false(is_binary(rnorm(100)))

  # Edge cases
  expect_false(is_binary(c(0))) # Only one unique value
  expect_false(is_binary(c(1))) # Only one unique value
  expect_false(is_binary(c(0, 0, 0))) # Only one unique value
  expect_false(is_binary(c(1, 1, 1))) # Only one unique value

  # With missing values
  expect_true(is_binary(c(0, 1, NA, 0, 1)))
  expect_false(is_binary(c(0, 1, 2, NA)))
})

# =============================================================================
# PERFORMANCE AND STRESS TESTS
# =============================================================================

test_that("functions handle large datasets", {
  # Create large dataset
  n_large <- 10000
  data_large <- create_test_data(n = n_large, seed = 456)

  # Test all functions with large data
  expect_no_error({
    smd_large <- compute_smd(
      covariate = data_large$x_cont,
      group = data_large$g_balanced
    )
    vr_large <- compute_variance_ratio(
      covariate = data_large$x_cont,
      group = data_large$g_balanced
    )
    ks_large <- compute_ks(
      covariate = data_large$x_cont,
      group = data_large$g_balanced
    )
    cor_large <- compute_correlation(data_large$x_cont, data_large$x_skewed)
  })
})

test_that("functions handle extreme weights", {
  data <- create_test_data()

  # Test with extreme weights
  expect_no_error({
    smd_extreme <- compute_smd(
      covariate = data$x_cont,
      group = data$g_balanced,
      weights = data$w_extreme
    )
    vr_extreme <- compute_variance_ratio(
      covariate = data$x_cont,
      group = data$g_balanced,
      weights = data$w_extreme
    )
    ks_extreme <- compute_ks(
      covariate = data$x_cont,
      group = data$g_balanced,
      weights = data$w_extreme
    )
    cor_extreme <- compute_correlation(
      data$x_cont,
      data$x_skewed,
      weights = data$w_extreme
    )
  })

  # Results should be finite
  expect_true(is.finite(smd_extreme))
  expect_true(is.finite(vr_extreme))
  expect_true(is.finite(ks_extreme))
  expect_true(is.finite(cor_extreme))
})

test_that("functions handle unbalanced groups", {
  data <- create_test_data()

  # Test with very unbalanced groups
  expect_no_error({
    smd_unbal <- compute_smd(covariate = data$x_cont, group = data$g_unbalanced)
    vr_unbal <- compute_variance_ratio(
      covariate = data$x_cont,
      group = data$g_unbalanced
    )
    ks_unbal <- compute_ks(covariate = data$x_cont, group = data$g_unbalanced)
  })

  # Results should be finite
  expect_true(is.finite(smd_unbal))
  expect_true(is.finite(vr_unbal))
  expect_true(is.finite(ks_unbal))
})

# =============================================================================
# COBALT COMPARISON TESTS (CONDITIONAL)
# =============================================================================

test_that("compute_variance_ratio matches cobalt::col_w_vr", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Continuous variables
  our_vr_cont <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  cobalt_vr_cont <- cobalt::col_w_vr(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform
  )[1]
  expect_equal(our_vr_cont, cobalt_vr_cont, tolerance = 1e-10)

  # Binary variables
  our_vr_bin <- compute_variance_ratio(
    covariate = data$x_binary,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  cobalt_vr_bin <- cobalt::col_w_vr(
    matrix(data$x_binary, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform,
    bin.vars = TRUE
  )[1]
  expect_equal(our_vr_bin, cobalt_vr_bin, tolerance = 1e-10)

  # Unweighted
  our_vr_unw <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced
  )
  cobalt_vr_unw <- cobalt::col_w_vr(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]
  expect_equal(our_vr_unw, cobalt_vr_unw, tolerance = 1e-10)
})

test_that("compute_ks matches cobalt::col_w_ks", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Continuous variables
  our_ks_cont <- compute_ks(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  cobalt_ks_cont <- cobalt::col_w_ks(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform
  )[1]
  expect_equal(our_ks_cont, cobalt_ks_cont, tolerance = 1e-10)

  # Binary variables
  our_ks_bin <- compute_ks(
    covariate = data$x_binary,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  cobalt_ks_bin <- cobalt::col_w_ks(
    matrix(data$x_binary, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform,
    bin.vars = TRUE
  )[1]
  expect_equal(our_ks_bin, cobalt_ks_bin, tolerance = 1e-10)

  # Unweighted
  our_ks_unw <- compute_ks(covariate = data$x_cont, group = data$g_balanced)
  cobalt_ks_unw <- cobalt::col_w_ks(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]
  expect_equal(our_ks_unw, cobalt_ks_unw, tolerance = 1e-10)
})

test_that("compute_smd matches cobalt::col_w_smd for binary variables", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Binary variables should match exactly
  our_smd_bin <- compute_smd(
    covariate = data$x_binary,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  cobalt_smd_bin <- cobalt::col_w_smd(
    matrix(data$x_binary, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform,
    std = TRUE,
    bin.vars = TRUE
  )[1]
  expect_equal(our_smd_bin, cobalt_smd_bin, tolerance = 1e-10)
})

test_that("compute_smd is close to cobalt::col_w_smd for continuous variables", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Continuous variables should be close (different pooled variance approaches)
  our_smd_cont <- compute_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  cobalt_smd_cont <- cobalt::col_w_smd(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform,
    std = TRUE
  )[1]

  # Should be within 5% of each other
  relative_diff <- abs(our_smd_cont - cobalt_smd_cont) / abs(cobalt_smd_cont)
  expect_true(relative_diff < 0.05)
})

test_that("cobalt comparison with missing values", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Add missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Both should handle missing values similarly
  our_vr_na <- compute_variance_ratio(
    covariate = x_na,
    group = data$g_balanced,
    na_rm = TRUE
  )
  cobalt_vr_na <- cobalt::col_w_vr(
    matrix(x_na, ncol = 1),
    treat = data$g_balanced,
    na.rm = TRUE
  )[1]
  expect_equal(our_vr_na, cobalt_vr_na, tolerance = 1e-10)

  # NOTE: There is a bug in cobalt's col_w_ks function when handling missing values.
  # The bug occurs because cobalt applies na.rem to the data vector but not to the
  # corresponding treatment and weight vectors, causing an indexing mismatch.
  # This results in cobalt using the wrong weights for the wrong observations.
  # Our implementation correctly handles missing values by removing them from all
  # relevant vectors consistently. The correct KS statistic should be 0.1896466,
  # but cobalt returns 0.1714976 due to this bug.
  our_ks_na <- compute_ks(
    covariate = x_na,
    group = data$g_balanced,
    na_rm = TRUE
  )
  cobalt_ks_na <- cobalt::col_w_ks(
    matrix(x_na, ncol = 1),
    treat = data$g_balanced,
    na.rm = TRUE
  )[1]

  # Verify our implementation matches base R
  complete_mask <- !is.na(x_na)
  x_complete <- x_na[complete_mask]
  g_complete <- data$g_balanced[complete_mask]
  base_ks <- as.numeric(
    ks.test(x_complete[g_complete == 0], x_complete[g_complete == 1])$statistic
  )
  expect_equal(our_ks_na, base_ks, tolerance = 1e-10)

  # Document the cobalt bug (our implementation is correct)
  expect_equal(our_ks_na, 0.1896466, tolerance = 1e-6)
  expect_equal(cobalt_ks_na, 0.1714976, tolerance = 1e-6)
})

test_that("cobalt comparison with different reference groups", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Test variance ratio with different reference groups
  our_vr_ref0 <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 0
  )
  our_vr_ref1 <- compute_variance_ratio(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 1
  )

  # Cobalt always uses first group as reference
  cobalt_vr <- cobalt::col_w_vr(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]

  # One of our results should match cobalt's approach
  expect_true(
    abs(our_vr_ref0 - cobalt_vr) < 1e-10 || abs(our_vr_ref1 - cobalt_vr) < 1e-10
  )
})

# =============================================================================
# NHEFS-SPECIFIC TESTS
# =============================================================================

test_that("compute_smd works with NHEFS continuous variables", {
  data <- get_nhefs_compute_data()

  # Test with age and smoking cessation
  smd_age <- compute_smd(data$age, data$qsmk)
  expect_true(is.finite(smd_age))
  expect_true(abs(smd_age) < 5) # Reasonable SMD range

  # Test with baseline weight
  smd_wt <- compute_smd(data$wt71, data$qsmk)
  expect_true(is.finite(smd_wt))
  expect_true(abs(smd_wt) < 5)
})

test_that("compute_smd works with NHEFS factor variables", {
  data <- get_nhefs_compute_data()

  # Test with sex (factor)
  smd_sex <- compute_smd(as.numeric(data$sex), data$qsmk)
  expect_true(is.finite(smd_sex))

  # Test with race (factor)
  smd_race <- compute_smd(as.numeric(data$race), data$qsmk)
  expect_true(is.finite(smd_race))
})

test_that("compute_variance_ratio works with NHEFS data", {
  data <- get_nhefs_compute_data()

  # Test with continuous variables
  vr_age <- compute_variance_ratio(data$age, data$qsmk)
  expect_true(is.finite(vr_age))
  expect_true(vr_age > 0)

  vr_wt <- compute_variance_ratio(data$wt71, data$qsmk)
  expect_true(is.finite(vr_wt))
  expect_true(vr_wt > 0)

  # Test with weights
  vr_weighted <- compute_variance_ratio(
    data$age,
    data$qsmk,
    weights = data$w_uniform
  )
  expect_true(is.finite(vr_weighted))
  expect_true(vr_weighted > 0)
})

test_that("compute_ks works with NHEFS data", {
  data <- get_nhefs_compute_data()

  # Test with continuous variables
  ks_age <- compute_ks(data$age, data$qsmk)
  expect_true(is.finite(ks_age))
  expect_true(ks_age >= 0 && ks_age <= 1)

  ks_wt <- compute_ks(data$wt71, data$qsmk)
  expect_true(is.finite(ks_wt))
  expect_true(ks_wt >= 0 && ks_wt <= 1)

  # Test with weights
  ks_weighted <- compute_ks(data$age, data$qsmk, weights = data$w_uniform)
  expect_true(is.finite(ks_weighted))
  expect_true(ks_weighted >= 0 && ks_weighted <= 1)

  # Test with real propensity score weights
  ks_ps_weighted <- compute_ks(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(ks_ps_weighted))
  expect_true(ks_ps_weighted >= 0 && ks_ps_weighted <= 1)
})

test_that("compute_correlation works with NHEFS data", {
  data <- get_nhefs_compute_data()

  # Test correlation between related variables
  cor_age_smokeyrs <- compute_correlation(data$age, data$smokeyrs)
  expect_true(is.finite(cor_age_smokeyrs))
  expect_true(cor_age_smokeyrs >= -1 && cor_age_smokeyrs <= 1)

  cor_wt71_age <- compute_correlation(data$wt71, data$age)
  expect_true(is.finite(cor_wt71_age))
  expect_true(cor_wt71_age >= -1 && cor_wt71_age <= 1)

  # Test with weights
  cor_weighted <- compute_correlation(
    data$age,
    data$wt71,
    weights = data$w_uniform
  )
  expect_true(is.finite(cor_weighted))
  expect_true(cor_weighted >= -1 && cor_weighted <= 1)
})

test_that("all functions handle NHEFS missing values correctly", {
  data <- get_nhefs_compute_data()

  # Some NHEFS variables may have missing values naturally
  # Test that functions handle them appropriately

  # Create a version with deliberate missing values
  data_na <- data
  data_na$age[1:10] <- NA

  # All functions should return NA with na_rm = FALSE
  expect_true(is.na(compute_smd(data_na$age, data_na$qsmk, na_rm = FALSE)))
  expect_true(is.na(compute_variance_ratio(
    data_na$age,
    data_na$qsmk,
    na_rm = FALSE
  )))
  expect_true(is.na(compute_ks(data_na$age, data_na$qsmk, na_rm = FALSE)))
  expect_true(is.na(compute_correlation(
    data_na$age,
    data_na$wt71,
    na_rm = FALSE
  )))

  # All functions should work with na_rm = TRUE
  expect_true(is.finite(compute_smd(data_na$age, data_na$qsmk, na_rm = TRUE)))
  expect_true(is.finite(compute_variance_ratio(
    data_na$age,
    data_na$qsmk,
    na_rm = TRUE
  )))
  expect_true(is.finite(compute_ks(data_na$age, data_na$qsmk, na_rm = TRUE)))
  expect_true(is.finite(compute_correlation(
    data_na$age,
    data_na$wt71,
    na_rm = TRUE
  )))
})

test_that("compute functions handle realistic smoking cessation analysis", {
  data <- get_nhefs_compute_data()

  # Typical covariates for smoking cessation analysis
  covariates <- c("age", "wt71", "smokeintensity", "smokeyrs")

  # Test each function with all covariates
  for (var in covariates) {
    if (var %in% names(data)) {
      # SMD
      smd_val <- compute_smd(data[[var]], data$qsmk)
      expect_true(is.finite(smd_val), info = paste("SMD failed for", var))

      # Variance ratio
      vr_val <- compute_variance_ratio(data[[var]], data$qsmk)
      expect_true(
        is.finite(vr_val) && vr_val > 0,
        info = paste("VR failed for", var)
      )

      # KS statistic
      ks_val <- compute_ks(data[[var]], data$qsmk)
      expect_true(
        is.finite(ks_val) && ks_val >= 0 && ks_val <= 1,
        info = paste("KS failed for", var)
      )
    }
  }
})

test_that("compute functions work with NHEFS extreme cases", {
  data <- get_nhefs_compute_data()

  # Test with extreme weights
  smd_extreme <- compute_smd(data$age, data$qsmk, weights = data$w_extreme)
  expect_true(is.finite(smd_extreme))

  vr_extreme <- compute_variance_ratio(
    data$age,
    data$qsmk,
    weights = data$w_extreme
  )
  expect_true(is.finite(vr_extreme) && vr_extreme > 0)

  ks_extreme <- compute_ks(data$age, data$qsmk, weights = data$w_extreme)
  expect_true(is.finite(ks_extreme) && ks_extreme >= 0 && ks_extreme <= 1)
})

test_that("compute functions are consistent across NHEFS subsets", {
  data <- get_nhefs_compute_data()

  # Test that results are consistent when computed on subsets
  subset1 <- data[1:150, ]
  subset2 <- data[151:300, ]

  # Both subsets should produce finite results
  for (subset_data in list(subset1, subset2)) {
    smd_val <- compute_smd(subset_data$age, subset_data$qsmk)
    expect_true(is.finite(smd_val))

    vr_val <- compute_variance_ratio(subset_data$age, subset_data$qsmk)
    expect_true(is.finite(vr_val) && vr_val > 0)

    ks_val <- compute_ks(subset_data$age, subset_data$qsmk)
    expect_true(is.finite(ks_val) && ks_val >= 0 && ks_val <= 1)
  }
})

test_that("compute functions work with real propensity score weights from nhefs_weights", {
  data <- get_nhefs_compute_data()

  # Test all functions with ATE weights
  smd_ate <- compute_smd(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(smd_ate))

  vr_ate <- compute_variance_ratio(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(vr_ate) && vr_ate > 0)

  ks_ate <- compute_ks(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(ks_ate) && ks_ate >= 0 && ks_ate <= 1)

  # Test all functions with ATT weights
  smd_att <- compute_smd(data$age, data$qsmk, weights = data$w_att)
  expect_true(is.finite(smd_att))

  vr_att <- compute_variance_ratio(data$age, data$qsmk, weights = data$w_att)
  expect_true(is.finite(vr_att) && vr_att > 0)

  ks_att <- compute_ks(data$age, data$qsmk, weights = data$w_att)
  expect_true(is.finite(ks_att) && ks_att >= 0 && ks_att <= 1)

  # ATE and ATT estimates should generally be different
  expect_false(identical(smd_ate, smd_att))
  expect_false(identical(vr_ate, vr_att))
})

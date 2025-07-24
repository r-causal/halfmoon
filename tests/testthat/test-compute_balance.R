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
# TESTS FOR bal_smd()
# =============================================================================

test_that("bal_smd matches smd::smd estimate", {
  set.seed(1)
  x <- rnorm(100)
  g <- factor(sample(c(0, 1), 100, replace = TRUE))

  out_pkg <- bal_smd(covariate = x, group = g, reference_group = 1)
  out_base <- smd::smd(x, g, gref = 1)$estimate

  expect_equal(out_pkg, out_base)
})

test_that("bal_smd handles different reference groups", {
  data <- create_test_data()

  # Test with numeric reference groups
  smd_ref0 <- bal_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 0
  )
  smd_ref1 <- bal_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 1
  )

  expect_equal(smd_ref0, -smd_ref1, tolerance = 1e-10)

  # Test with factor reference groups
  smd_control <- bal_smd(
    covariate = data$x_cont,
    group = data$g_factor,
    reference_group = "control"
  )
  smd_treated <- bal_smd(
    covariate = data$x_cont,
    group = data$g_factor,
    reference_group = "treated"
  )

  expect_equal(smd_control, -smd_treated, tolerance = 1e-10)
})

test_that("bal_smd handles weights", {
  data <- create_test_data()

  # Weighted vs unweighted should generally be different
  smd_unweighted <- bal_smd(
    covariate = data$x_cont,
    group = data$g_balanced
  )
  smd_weighted <- bal_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )

  expect_false(identical(smd_unweighted, smd_weighted))

  # Both should be finite numbers
  expect_true(is.finite(smd_unweighted))
  expect_true(is.finite(smd_weighted))
})

test_that("bal_smd handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA
  g_na <- data$g_balanced
  w_na <- data$w_uniform

  # Should return NA when na.rm = FALSE
  expect_true(is.na(bal_smd(covariate = x_na, group = g_na, na.rm = FALSE)))

  # Should work when na.rm = TRUE
  smd_na.rm <- bal_smd(covariate = x_na, group = g_na, na.rm = TRUE)
  expect_true(is.finite(smd_na.rm))
})

test_that("bal_smd error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_error(bal_smd(covariate = data$x_cont, group = rep(1, 100)))
  expect_error(bal_smd(
    covariate = data$x_cont,
    group = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_error(bal_smd(
    covariate = data$x_cont[1:50],
    group = data$g_balanced
  ))
  expect_error(bal_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR bal_vr()
# =============================================================================

test_that("bal_vr handles basic cases", {
  data <- create_test_data()

  # Basic functionality
  vr <- bal_vr(covariate = data$x_cont, group = data$g_balanced)
  expect_true(is.finite(vr))
  expect_true(vr > 0)

  # With weights
  vr_weighted <- bal_vr(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(vr_weighted))
  expect_true(vr_weighted > 0)
})

test_that("bal_vr handles reference groups", {
  data <- create_test_data()

  # Different reference groups should give reciprocal results
  vr_ref0 <- bal_vr(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 0
  )
  vr_ref1 <- bal_vr(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 1
  )

  expect_equal(vr_ref0, 1 / vr_ref1, tolerance = 1e-10)
})

test_that("bal_vr handles binary variables", {
  data <- create_test_data()

  # Binary variables should use p*(1-p) variance formula
  vr_binary <- bal_vr(
    covariate = data$x_binary,
    group = data$g_balanced
  )
  expect_true(is.finite(vr_binary))
  expect_true(vr_binary > 0)

  # With weights
  vr_binary_weighted <- bal_vr(
    covariate = data$x_binary,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(vr_binary_weighted))
  expect_true(vr_binary_weighted > 0)
})

test_that("bal_vr handles edge cases", {
  data <- create_test_data()

  # Zero variance scenarios
  x_zero <- c(rep(1, 50), rep(1, 50))
  g <- c(rep(0, 50), rep(1, 50))

  vr_zero_both <- bal_vr(covariate = x_zero, group = g)
  expect_equal(vr_zero_both, 1)

  # One group with zero variance
  x_mixed <- c(rep(1, 50), rnorm(50))
  vr_zero_one <- bal_vr(covariate = x_mixed, group = g)
  expect_true(vr_zero_one == 0 || vr_zero_one == Inf)
})

test_that("bal_vr handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Should return NA when na.rm = FALSE
  expect_equal(
    bal_vr(
      covariate = x_na,
      group = data$g_balanced,
      na.rm = FALSE
    ),
    NA_real_
  )

  # Should work when na.rm = TRUE if enough data remains
  vr_na.rm <- bal_vr(
    covariate = x_na,
    group = data$g_balanced,
    na.rm = TRUE
  )
  expect_true(is.finite(vr_na.rm) || is.na(vr_na.rm))
})

test_that("bal_vr error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_error(bal_vr(
    covariate = data$x_cont,
    group = rep(1, 100)
  ))
  expect_error(bal_vr(
    covariate = data$x_cont,
    group = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_error(bal_vr(
    covariate = data$x_cont[1:50],
    group = data$g_balanced
  ))
  expect_error(bal_vr(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR bal_ks()
# =============================================================================

test_that("bal_ks handles basic cases", {
  data <- create_test_data()

  # Basic functionality
  ks <- bal_ks(covariate = data$x_cont, group = data$g_balanced)
  expect_true(is.finite(ks))
  expect_true(ks >= 0)
  expect_true(ks <= 1)

  # With weights
  ks_weighted <- bal_ks(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(ks_weighted))
  expect_true(ks_weighted >= 0)
  expect_true(ks_weighted <= 1)
})

test_that("bal_ks gives 0 for identical distributions", {
  # Identical distributions should give KS = 0
  x <- c(1, 2, 3, 1, 2, 3)
  g <- c(0, 0, 0, 1, 1, 1)

  ks_identical <- bal_ks(covariate = x, group = g)
  expect_equal(ks_identical, 0)
})

test_that("bal_ks gives >0 for different distributions", {
  # Different distributions should give KS > 0
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c(0, 0, 0, 1, 1, 1)

  ks_different <- bal_ks(covariate = x, group = g)
  expect_true(ks_different > 0)
})

test_that("bal_ks handles binary variables", {
  data <- create_test_data()

  # Binary variables should return difference in proportions
  ks_binary <- bal_ks(covariate = data$x_binary, group = data$g_balanced)
  expect_true(is.finite(ks_binary))
  expect_true(ks_binary >= 0)
  expect_true(ks_binary <= 1)

  # Should equal absolute difference in proportions
  prop_0 <- mean(data$x_binary[data$g_balanced == 0])
  prop_1 <- mean(data$x_binary[data$g_balanced == 1])
  expected_ks <- abs(prop_1 - prop_0)

  expect_equal(ks_binary, expected_ks, tolerance = 1e-10)
})

test_that("bal_ks handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Should return NA when na.rm = FALSE
  expect_equal(
    bal_ks(covariate = x_na, group = data$g_balanced, na.rm = FALSE),
    NA_real_
  )

  # Should work when na.rm = TRUE if enough data remains
  ks_na.rm <- bal_ks(
    covariate = x_na,
    group = data$g_balanced,
    na.rm = TRUE
  )
  expect_true(is.finite(ks_na.rm) || is.na(ks_na.rm))
})

test_that("bal_ks error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_error(bal_ks(covariate = data$x_cont, group = rep(1, 100)))
  expect_error(bal_ks(
    covariate = data$x_cont,
    group = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_error(bal_ks(
    covariate = data$x_cont[1:50],
    group = data$g_balanced
  ))
  expect_error(bal_ks(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform[1:50]
  ))
})

# =============================================================================
# TESTS FOR bal_corr()
# =============================================================================

test_that("bal_corr matches stats::cor when unweighted", {
  x <- 1:10
  y <- 2 * x + rnorm(10, sd = 0.1)

  cor_ours <- bal_corr(x, y)
  cor_stats <- stats::cor(x, y)

  expect_equal(cor_ours, cor_stats)
})

test_that("bal_corr handles weights correctly", {
  x <- c(0, 1, 0, 1)
  y <- c(0, 0, 1, 1)
  w <- c(1, 0, 0, 1)

  # Weighted on matching pairs (0,0) and (1,1) -> perfect correlation
  cor_weighted <- bal_corr(x, y, weights = w)
  expect_equal(cor_weighted, 1)
})

test_that("bal_corr handles various scenarios", {
  data <- create_test_data()

  # Basic correlation
  cor_basic <- bal_corr(data$x_cont, data$x_skewed)
  expect_true(is.finite(cor_basic))
  expect_true(cor_basic >= -1 && cor_basic <= 1)

  # Weighted correlation
  cor_weighted <- bal_corr(
    data$x_cont,
    data$x_skewed,
    weights = data$w_uniform
  )
  expect_true(is.finite(cor_weighted))
  expect_true(cor_weighted >= -1 && cor_weighted <= 1)

  # Perfect correlation
  x_perfect <- 1:100
  y_perfect <- 2 * x_perfect + 5
  cor_perfect <- bal_corr(x_perfect, y_perfect)
  expect_equal(cor_perfect, 1, tolerance = 1e-10)

  # No correlation
  set.seed(123)
  x_uncorr <- rnorm(100)
  y_uncorr <- rnorm(100)
  cor_uncorr <- bal_corr(x_uncorr, y_uncorr)
  expect_true(abs(cor_uncorr) < 0.5) # Should be close to 0
})

test_that("bal_corr handles missing values", {
  data <- create_test_data()

  # Introduce missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Should return NA when na.rm = FALSE
  expect_equal(
    bal_corr(x_na, data$x_skewed, na.rm = FALSE),
    NA_real_
  )

  # Should work when na.rm = TRUE
  cor_na.rm <- bal_corr(x_na, data$x_skewed, na.rm = TRUE)
  expect_true(is.finite(cor_na.rm))
})

test_that("bal_corr handles edge cases", {
  # Zero variance should return NA
  x_zero <- rep(1, 100)
  y_normal <- rnorm(100)

  cor_zero <- bal_corr(x_zero, y_normal)
  expect_true(is.na(cor_zero))

  # Both zero variance should return NA
  y_zero <- rep(2, 100)
  cor_both_zero <- bal_corr(x_zero, y_zero)
  expect_true(is.na(cor_both_zero))
})

test_that("bal_corr error handling", {
  data <- create_test_data()

  # Should error with mismatched lengths
  expect_error(bal_corr(data$x_cont[1:50], data$x_skewed))
  expect_error(bal_corr(
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
    smd_large <- bal_smd(
      covariate = data_large$x_cont,
      group = data_large$g_balanced
    )
    vr_large <- bal_vr(
      covariate = data_large$x_cont,
      group = data_large$g_balanced
    )
    ks_large <- bal_ks(
      covariate = data_large$x_cont,
      group = data_large$g_balanced
    )
    cor_large <- bal_corr(data_large$x_cont, data_large$x_skewed)
  })
})

test_that("functions handle extreme weights", {
  data <- create_test_data()

  # Test with extreme weights
  expect_no_error({
    smd_extreme <- bal_smd(
      covariate = data$x_cont,
      group = data$g_balanced,
      weights = data$w_extreme
    )
    vr_extreme <- bal_vr(
      covariate = data$x_cont,
      group = data$g_balanced,
      weights = data$w_extreme
    )
    ks_extreme <- bal_ks(
      covariate = data$x_cont,
      group = data$g_balanced,
      weights = data$w_extreme
    )
    cor_extreme <- bal_corr(
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
    smd_unbal <- bal_smd(covariate = data$x_cont, group = data$g_unbalanced)
    vr_unbal <- bal_vr(
      covariate = data$x_cont,
      group = data$g_unbalanced
    )
    ks_unbal <- bal_ks(covariate = data$x_cont, group = data$g_unbalanced)
  })

  # Results should be finite
  expect_true(is.finite(smd_unbal))
  expect_true(is.finite(vr_unbal))
  expect_true(is.finite(ks_unbal))
})

# =============================================================================
# COBALT COMPARISON TESTS
# =============================================================================

test_that("bal_vr matches cobalt::col_w_vr", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Continuous variables
  our_vr_cont <- bal_vr(
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
  our_vr_bin <- bal_vr(
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
  our_vr_unw <- bal_vr(
    covariate = data$x_cont,
    group = data$g_balanced
  )
  cobalt_vr_unw <- cobalt::col_w_vr(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]
  expect_equal(our_vr_unw, cobalt_vr_unw, tolerance = 1e-10)
})

test_that("bal_ks matches cobalt::col_w_ks", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Continuous variables
  our_ks_cont <- bal_ks(
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
  our_ks_bin <- bal_ks(
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
  our_ks_unw <- bal_ks(covariate = data$x_cont, group = data$g_balanced)
  cobalt_ks_unw <- cobalt::col_w_ks(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]
  expect_equal(our_ks_unw, cobalt_ks_unw, tolerance = 1e-10)
})

test_that("bal_smd matches cobalt::col_w_smd for binary variables", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Binary variables should match exactly
  # Note: cobalt uses group 1 as reference, so we specify reference_group = 1
  # to match cobalt's behavior for this test
  our_smd_bin <- bal_smd(
    covariate = data$x_binary,
    group = data$g_balanced,
    weights = data$w_uniform,
    reference_group = 1
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

test_that("bal_smd is close to cobalt::col_w_smd for continuous variables", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Continuous variables should be close (different pooled variance approaches)
  # Note: cobalt uses group 1 as reference, so we specify reference_group = 1
  # to match cobalt's behavior for this test
  our_smd_cont <- bal_smd(
    covariate = data$x_cont,
    group = data$g_balanced,
    weights = data$w_uniform,
    reference_group = 1
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
  our_vr_na <- bal_vr(
    covariate = x_na,
    group = data$g_balanced,
    na.rm = TRUE
  )
  cobalt_vr_na <- cobalt::col_w_vr(
    matrix(x_na, ncol = 1),
    treat = data$g_balanced,
    na.rm = TRUE
  )[1]
  expect_equal(our_vr_na, cobalt_vr_na, tolerance = 1e-10)

  our_ks_na <- bal_ks(
    covariate = x_na,
    group = data$g_balanced,
    na.rm = TRUE
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
})

test_that("cobalt comparison with different reference groups", {
  skip_if_not_installed("cobalt")
  data <- create_test_data(seed = 789)

  # Test variance ratio with different reference groups
  our_vr_ref0 <- bal_vr(
    covariate = data$x_cont,
    group = data$g_balanced,
    reference_group = 0
  )
  our_vr_ref1 <- bal_vr(
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

test_that("bal_smd works with NHEFS continuous variables", {
  data <- get_nhefs_compute_data()

  # Test with age and smoking cessation
  smd_age <- bal_smd(data$age, data$qsmk)
  expect_true(is.finite(smd_age))
  expect_true(abs(smd_age) < 5) # Reasonable SMD range

  # Test with baseline weight
  smd_wt <- bal_smd(data$wt71, data$qsmk)
  expect_true(is.finite(smd_wt))
  expect_true(abs(smd_wt) < 5)
})

test_that("bal_smd works with NHEFS factor variables", {
  data <- get_nhefs_compute_data()

  # Test with sex (factor)
  smd_sex <- bal_smd(as.numeric(data$sex), data$qsmk)
  expect_true(is.finite(smd_sex))

  # Test with race (factor)
  smd_race <- bal_smd(as.numeric(data$race), data$qsmk)
  expect_true(is.finite(smd_race))
})

test_that("bal_vr works with NHEFS data", {
  data <- get_nhefs_compute_data()

  # Test with continuous variables
  vr_age <- bal_vr(data$age, data$qsmk)
  expect_true(is.finite(vr_age))
  expect_true(vr_age > 0)

  vr_wt <- bal_vr(data$wt71, data$qsmk)
  expect_true(is.finite(vr_wt))
  expect_true(vr_wt > 0)

  # Test with weights
  vr_weighted <- bal_vr(
    data$age,
    data$qsmk,
    weights = data$w_uniform
  )
  expect_true(is.finite(vr_weighted))
  expect_true(vr_weighted > 0)
})

test_that("bal_ks works with NHEFS data", {
  data <- get_nhefs_compute_data()

  # Test with continuous variables
  ks_age <- bal_ks(data$age, data$qsmk)
  expect_true(is.finite(ks_age))
  expect_true(ks_age >= 0 && ks_age <= 1)

  ks_wt <- bal_ks(data$wt71, data$qsmk)
  expect_true(is.finite(ks_wt))
  expect_true(ks_wt >= 0 && ks_wt <= 1)

  # Test with weights
  ks_weighted <- bal_ks(data$age, data$qsmk, weights = data$w_uniform)
  expect_true(is.finite(ks_weighted))
  expect_true(ks_weighted >= 0 && ks_weighted <= 1)

  # Test with real propensity score weights
  ks_ps_weighted <- bal_ks(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(ks_ps_weighted))
  expect_true(ks_ps_weighted >= 0 && ks_ps_weighted <= 1)
})

test_that("bal_corr works with NHEFS data", {
  data <- get_nhefs_compute_data()

  # Test correlation between related variables
  cor_age_smokeyrs <- bal_corr(data$age, data$smokeyrs)
  expect_true(is.finite(cor_age_smokeyrs))
  expect_true(cor_age_smokeyrs >= -1 && cor_age_smokeyrs <= 1)

  cor_wt71_age <- bal_corr(data$wt71, data$age)
  expect_true(is.finite(cor_wt71_age))
  expect_true(cor_wt71_age >= -1 && cor_wt71_age <= 1)

  # Test with weights
  cor_weighted <- bal_corr(
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

  # All functions should return NA with na.rm = FALSE
  expect_true(is.na(bal_smd(data_na$age, data_na$qsmk, na.rm = FALSE)))
  expect_true(is.na(bal_vr(
    data_na$age,
    data_na$qsmk,
    na.rm = FALSE
  )))
  expect_true(is.na(bal_ks(data_na$age, data_na$qsmk, na.rm = FALSE)))
  expect_true(is.na(bal_corr(
    data_na$age,
    data_na$wt71,
    na.rm = FALSE
  )))

  # All functions should work with na.rm = TRUE
  expect_true(is.finite(bal_smd(data_na$age, data_na$qsmk, na.rm = TRUE)))
  expect_true(is.finite(bal_vr(
    data_na$age,
    data_na$qsmk,
    na.rm = TRUE
  )))
  expect_true(is.finite(bal_ks(data_na$age, data_na$qsmk, na.rm = TRUE)))
  expect_true(is.finite(bal_corr(
    data_na$age,
    data_na$wt71,
    na.rm = TRUE
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
      smd_val <- bal_smd(data[[var]], data$qsmk)
      expect_true(is.finite(smd_val), info = paste("SMD failed for", var))

      # Variance ratio
      vr_val <- bal_vr(data[[var]], data$qsmk)
      expect_true(
        is.finite(vr_val) && vr_val > 0,
        info = paste("VR failed for", var)
      )

      # KS statistic
      ks_val <- bal_ks(data[[var]], data$qsmk)
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
  smd_extreme <- bal_smd(data$age, data$qsmk, weights = data$w_extreme)
  expect_true(is.finite(smd_extreme))

  vr_extreme <- bal_vr(
    data$age,
    data$qsmk,
    weights = data$w_extreme
  )
  expect_true(is.finite(vr_extreme) && vr_extreme > 0)

  ks_extreme <- bal_ks(data$age, data$qsmk, weights = data$w_extreme)
  expect_true(is.finite(ks_extreme) && ks_extreme >= 0 && ks_extreme <= 1)
})

test_that("compute functions are consistent across NHEFS subsets", {
  data <- get_nhefs_compute_data()

  # Test that results are consistent when computed on subsets
  subset1 <- data[1:150, ]
  subset2 <- data[151:300, ]

  # Both subsets should produce finite results
  for (subset_data in list(subset1, subset2)) {
    smd_val <- bal_smd(subset_data$age, subset_data$qsmk)
    expect_true(is.finite(smd_val))

    vr_val <- bal_vr(subset_data$age, subset_data$qsmk)
    expect_true(is.finite(vr_val) && vr_val > 0)

    ks_val <- bal_ks(subset_data$age, subset_data$qsmk)
    expect_true(is.finite(ks_val) && ks_val >= 0 && ks_val <= 1)
  }
})

test_that("compute functions work with real propensity score weights from nhefs_weights", {
  data <- get_nhefs_compute_data()

  # Test all functions with ATE weights
  smd_ate <- bal_smd(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(smd_ate))

  vr_ate <- bal_vr(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(vr_ate) && vr_ate > 0)

  ks_ate <- bal_ks(data$age, data$qsmk, weights = data$w_ate)
  expect_true(is.finite(ks_ate) && ks_ate >= 0 && ks_ate <= 1)

  # Test all functions with ATT weights
  smd_att <- bal_smd(data$age, data$qsmk, weights = data$w_att)
  expect_true(is.finite(smd_att))

  vr_att <- bal_vr(data$age, data$qsmk, weights = data$w_att)
  expect_true(is.finite(vr_att) && vr_att > 0)

  ks_att <- bal_ks(data$age, data$qsmk, weights = data$w_att)
  expect_true(is.finite(ks_att) && ks_att >= 0 && ks_att <= 1)

  # ATE and ATT estimates should generally be different
  expect_false(identical(smd_ate, smd_att))
  expect_false(identical(vr_ate, vr_att))
})

# =============================================================================
# TESTS FOR bal_energy()
# =============================================================================

test_that("bal_energy handles basic binary treatment cases", {
  data <- create_test_data()

  # Basic functionality
  energy <- bal_energy(
    covariates = data.frame(x = data$x_cont, y = data$x_skewed),
    group = data$g_balanced
  )
  expect_true(is.finite(energy))
  expect_true(energy >= 0)

  # With weights
  energy_weighted <- bal_energy(
    covariates = data.frame(x = data$x_cont, y = data$x_skewed),
    group = data$g_balanced,
    weights = data$w_uniform
  )
  expect_true(is.finite(energy_weighted))
  expect_true(energy_weighted >= 0)
})

test_that("bal_energy handles different estimands", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)

  # ATE
  energy_ate <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    estimand = "ATE"
  )

  # ATT
  energy_att <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    estimand = "ATT"
  )

  # ATC
  energy_atc <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    estimand = "ATC"
  )

  # Between-group only
  energy_between <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    estimand = NULL
  )

  # All should be finite and non-negative
  expect_true(all(sapply(
    list(energy_ate, energy_att, energy_atc, energy_between),
    function(x) is.finite(x) && x >= 0
  )))

  # Different estimands should generally give different results
  expect_false(all(
    c(energy_ate, energy_att, energy_atc, energy_between) == energy_ate
  ))
})

test_that("bal_energy handles multi-category treatments", {
  set.seed(123)
  n <- 100
  covs <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  multi_group <- sample(c("A", "B", "C"), n, replace = TRUE)

  # Should work with multi-category treatment
  energy_multi <- bal_energy(
    covariates = covs,
    group = multi_group,
    estimand = "ATE"
  )
  expect_true(is.finite(energy_multi))
  expect_true(energy_multi >= 0)
})

test_that("bal_energy handles continuous treatments", {
  set.seed(123)
  n <- 100
  covs <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  continuous_treatment <- rnorm(n)

  # Should work with continuous treatment (uses distance correlation)
  energy_cont <- bal_energy(
    covariates = covs,
    group = continuous_treatment,
    estimand = NULL # Must be NULL for continuous
  )
  expect_true(is.finite(energy_cont))
  expect_true(energy_cont >= 0)
  expect_true(energy_cont <= 1) # Distance correlation is bounded [0,1]

  # Should error if estimand is not NULL for continuous treatment
  expect_error(bal_energy(
    covariates = covs,
    group = continuous_treatment,
    estimand = "ATE"
  ))
})

test_that("bal_energy handles perfect balance", {
  # Create perfectly balanced data
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  covs <- data.frame(x = c(x, x))
  group <- c(rep(0, n), rep(1, n))

  energy_perfect <- bal_energy(
    covariates = covs,
    group = group,
    estimand = "ATE"
  )

  # Should be very close to 0
  expect_true(energy_perfect < 0.01)
})

test_that("bal_energy handles binary variables", {
  set.seed(123)
  n <- 100
  covs <- data.frame(
    binary = rbinom(n, 1, 0.5),
    continuous = rnorm(n)
  )
  group <- rbinom(n, 1, 0.5)

  # Should identify and handle binary variables correctly
  energy <- bal_energy(
    covariates = covs,
    group = group
  )
  expect_true(is.finite(energy))
  expect_true(energy >= 0)
})

test_that("bal_energy handles missing values", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)

  # Introduce missing values
  covs$x[data$na_indices] <- NA

  # Should error when na.rm = FALSE
  expect_error(bal_energy(
    covariates = covs,
    group = data$g_balanced,
    na.rm = FALSE
  ))

  # Should work when na.rm = TRUE
  energy_na.rm <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    na.rm = TRUE
  )
  expect_true(is.finite(energy_na.rm) || is.na(energy_na.rm))
})

test_that("bal_energy use_improved parameter works", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)

  # Improved vs standard for ATE
  energy_improved <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    estimand = "ATE",
    use_improved = TRUE
  )

  energy_standard <- bal_energy(
    covariates = covs,
    group = data$g_balanced,
    estimand = "ATE",
    use_improved = FALSE
  )

  # Both should be valid
  expect_true(is.finite(energy_improved))
  expect_true(is.finite(energy_standard))

  # Generally different values
  expect_false(identical(energy_improved, energy_standard))
})

test_that("bal_energy error handling", {
  data <- create_test_data()

  # Should error with non-numeric covariates
  expect_error(bal_energy(
    covariates = data.frame(x = as.character(data$x_cont)),
    group = data$g_balanced
  ))

  # Should error with mismatched dimensions
  expect_error(bal_energy(
    covariates = data.frame(x = data$x_cont[1:50]),
    group = data$g_balanced
  ))

  # Should error with wrong number of groups (only 1)
  expect_error(bal_energy(
    covariates = data.frame(x = data$x_cont),
    group = rep(1, 100)
  ))

  # Should error with negative weights
  expect_error(bal_energy(
    covariates = data.frame(x = data$x_cont),
    group = data$g_balanced,
    weights = c(-1, rep(1, 99))
  ))
})

test_that("bal_energy handles NHEFS data", {
  data <- get_nhefs_compute_data()

  # Select numeric covariates
  covs <- dplyr::select(data, age, wt71, smokeyrs)

  # Basic energy distance
  energy <- bal_energy(
    covariates = covs,
    group = data$qsmk
  )
  expect_true(is.finite(energy))
  expect_true(energy >= 0)

  # With ATE weights
  energy_ate <- bal_energy(
    covariates = covs,
    group = data$qsmk,
    weights = data$w_ate,
    estimand = "ATE"
  )
  expect_true(is.finite(energy_ate))
  expect_true(energy_ate >= 0)

  # With ATT weights
  energy_att <- bal_energy(
    covariates = covs,
    group = data$qsmk,
    weights = data$w_att,
    estimand = "ATT"
  )
  expect_true(is.finite(energy_att))
  expect_true(energy_att >= 0)

  # Weighted should generally be lower (better balance)
  expect_true(energy_ate < energy || energy_att < energy)
})

test_that("bal_energy comparison with cobalt package", {
  testthat::skip_if_not_installed("cobalt")

  # Create test data
  set.seed(456)
  n <- 200
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rbinom(n, 1, 0.5)
  )
  treatment <- rbinom(n, 1, 0.5)
  weights <- runif(n, 0.5, 1.5)

  # Our implementation - using default estimand (NULL)
  our_energy <- bal_energy(
    covariates = covariates,
    group = treatment,
    weights = weights
  )

  # Cobalt implementation
  cobalt_energy <- cobalt::bal.compute(
    x = covariates,
    treat = treatment,
    weights = weights,
    stat = "energy.dist"
  )

  # Should be very close (within numerical tolerance)
  expect_equal(our_energy, cobalt_energy, tolerance = 1e-3) # Slightly relaxed tolerance
})

test_that("bal_energy multi-category comparison with cobalt", {
  testthat::skip_if_not_installed("cobalt")

  # Create test data with 3 groups
  set.seed(789)
  n <- 150
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  treatment <- factor(sample(1:3, n, replace = TRUE))

  # Our implementation
  our_energy <- bal_energy(
    covariates = covariates,
    group = treatment
  )

  # Cobalt implementation
  cobalt_energy <- cobalt::bal.compute(
    x = covariates,
    treat = treatment,
    stat = "energy.dist"
  )

  # Should be very close
  expect_equal(our_energy, cobalt_energy, tolerance = 1e-4)
})

test_that("bal_energy continuous treatment comparison with cobalt", {
  testthat::skip_if_not_installed("cobalt")

  # Create test data with continuous treatment
  set.seed(321)
  n <- 150
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  treatment <- rnorm(n)

  # Our implementation (distance correlation)
  our_dcor <- bal_energy(
    covariates = covariates,
    group = treatment,
    estimand = NULL,
    standardized = TRUE
  )

  # Cobalt implementation
  cobalt_dcor <- cobalt::bal.compute(
    x = covariates,
    treat = treatment,
    stat = "distance.cor"
  )

  # Should be very close
  expect_equal(our_dcor, cobalt_dcor, tolerance = 1e-4)
})

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

test_that("bal_smd maps each level to the matching smd::smd reference index", {
  set.seed(1)
  x <- rnorm(100)
  g <- factor(sample(c(0, 1), 100, replace = TRUE))

  # smd::smd() splits by factor level, so level "0" is gref 1 and level "1" is
  # gref 2. halfmoon negates the estimate to report comparison minus reference.
  expect_equal(
    bal_smd(.covariate = x, .exposure = g, .reference_level = "0"),
    -smd::smd(x, g, gref = 1)$estimate
  )
  expect_equal(
    bal_smd(.covariate = x, .exposure = g, .reference_level = "1"),
    -smd::smd(x, g, gref = 2)$estimate
  )

  # The mapping must not depend on which level appears in the first row.
  shuffled <- rev(seq_along(x))
  expect_equal(
    bal_smd(
      .covariate = x[shuffled],
      .exposure = g[shuffled],
      .reference_level = "0"
    ),
    bal_smd(.covariate = x, .exposure = g, .reference_level = "0")
  )
})

test_that("bal_smd is invariant to row order", {
  set.seed(2024)
  reordered <- order(nhefs_weights$qsmk, decreasing = TRUE)

  expect_equal(
    bal_smd(
      nhefs_weights$age[reordered],
      nhefs_weights$qsmk[reordered],
      .reference_level = 0
    ),
    bal_smd(nhefs_weights$age, nhefs_weights$qsmk, .reference_level = 0)
  )
  expect_equal(
    bal_smd(nhefs_weights$age[reordered], nhefs_weights$qsmk[reordered]),
    bal_smd(nhefs_weights$age, nhefs_weights$qsmk)
  )

  shuffled <- sample(nrow(nhefs_weights))
  expect_equal(
    bal_smd(
      nhefs_weights$wt71[shuffled],
      nhefs_weights$qsmk[shuffled],
      .weights = nhefs_weights$w_ate[shuffled]
    ),
    bal_smd(
      nhefs_weights$wt71,
      nhefs_weights$qsmk,
      .weights = nhefs_weights$w_ate
    )
  )
})

test_that("bal_smd resolves a named reference level to that level", {
  g <- factor(rep(c("control", "treated"), each = 50))
  set.seed(11)
  x <- c(rnorm(50), rnorm(50, mean = 1))

  expect_equal(
    bal_smd(x, g, .reference_level = "control"),
    bal_smd(x, g, .reference_level = 1)
  )
  expect_equal(
    bal_smd(x, g, .reference_level = "treated"),
    bal_smd(x, g, .reference_level = 2)
  )
})

test_that("bal_smd rejects a reference level that names no group", {
  expect_error(
    bal_smd(
      nhefs_weights$age,
      nhefs_weights$qsmk,
      .reference_level = "banana"
    ),
    class = "halfmoon_reference_error"
  )
  expect_error(
    bal_smd(nhefs_weights$age, nhefs_weights$qsmk, .reference_level = 7),
    class = "halfmoon_range_error"
  )
  expect_error(
    bal_smd(nhefs_weights$age, nhefs_weights$qsmk, .reference_level = c(0, 1)),
    class = "halfmoon_arg_error"
  )
})

test_that("bal_smd reports the comparison group minus the reference group", {
  set.seed(3)
  g <- rep(c(0, 1), each = 50)
  x <- c(rnorm(50, mean = 0), rnorm(50, mean = 10))

  expect_gt(bal_smd(x, g, .reference_level = 0), 0)
  expect_lt(bal_smd(x, g, .reference_level = 1), 0)

  # People who quit smoking are older, so the estimate is positive with the
  # non-quitters as the reference group.
  expect_equal(
    bal_smd(nhefs_weights$age, nhefs_weights$qsmk),
    0.2822208,
    tolerance = 1e-6
  )
})

test_that("bal_smd handles different reference groups", {
  data <- create_test_data()

  # Test with numeric reference groups
  smd_ref0 <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .reference_level = 0
  )
  smd_ref1 <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .reference_level = 1
  )

  expect_equal(smd_ref0, -smd_ref1, tolerance = 1e-10)

  # Test with factor reference groups
  smd_control <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_factor,
    .reference_level = "control"
  )
  smd_treated <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_factor,
    .reference_level = "treated"
  )

  expect_equal(smd_control, -smd_treated, tolerance = 1e-10)
})

test_that("bal_smd handles weights", {
  data <- create_test_data()

  # Weighted vs unweighted should generally be different
  smd_unweighted <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_balanced
  )
  smd_weighted <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
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
  expect_true(is.na(bal_smd(
    .covariate = x_na,
    .exposure = g_na,
    na.rm = FALSE
  )))

  # Should work when na.rm = TRUE
  smd_na.rm <- bal_smd(.covariate = x_na, .exposure = g_na, na.rm = TRUE)
  expect_true(is.finite(smd_na.rm))
})

test_that("bal_smd error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_halfmoon_error(
    bal_smd(.covariate = data$x_cont, .exposure = rep(1, 100)),
    "halfmoon_group_error"
  )

  # Now supports 3+ groups (categorical)
  expect_no_error(bal_smd(
    .covariate = data$x_cont,
    .exposure = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_halfmoon_error(
    bal_smd(
      .covariate = data$x_cont[1:50],
      .exposure = data$g_balanced
    ),
    "halfmoon_length_error"
  )

  expect_halfmoon_error(
    bal_smd(
      .covariate = data$x_cont,
      .exposure = data$g_balanced,
      .weights = data$w_uniform[1:50]
    ),
    "halfmoon_length_error"
  )
})

# =============================================================================
# TESTS FOR bal_vr()
# =============================================================================

test_that("bal_vr handles basic cases", {
  data <- create_test_data()

  # Basic functionality
  vr <- bal_vr(.covariate = data$x_cont, .exposure = data$g_balanced)
  expect_true(is.finite(vr))
  expect_true(vr > 0)

  # With weights
  vr_weighted <- bal_vr(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  expect_true(is.finite(vr_weighted))
  expect_true(vr_weighted > 0)
})

test_that("bal_vr handles reference groups", {
  data <- create_test_data()

  # Different reference groups should give reciprocal results
  vr_ref0 <- bal_vr(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .reference_level = 0
  )
  vr_ref1 <- bal_vr(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .reference_level = 1
  )

  expect_equal(vr_ref0, 1 / vr_ref1, tolerance = 1e-10)
})

test_that("bal_vr handles binary variables", {
  data <- create_test_data()

  # Binary variables should use p*(1-p) variance formula
  vr_binary <- bal_vr(
    .covariate = data$x_binary,
    .exposure = data$g_balanced
  )
  expect_true(is.finite(vr_binary))
  expect_true(vr_binary > 0)

  # With weights
  vr_binary_weighted <- bal_vr(
    .covariate = data$x_binary,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  expect_true(is.finite(vr_binary_weighted))
  expect_true(vr_binary_weighted > 0)
})

test_that("bal_vr handles edge cases", {
  data <- create_test_data()

  # Zero variance scenarios
  x_zero <- c(rep(1, 50), rep(1, 50))
  g <- c(rep(0, 50), rep(1, 50))

  vr_zero_both <- bal_vr(.covariate = x_zero, .exposure = g)
  expect_equal(vr_zero_both, 1)

  # One group with zero variance
  x_mixed <- c(rep(1, 50), rnorm(50))
  vr_zero_one <- bal_vr(.covariate = x_mixed, .exposure = g)
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
      .covariate = x_na,
      .exposure = data$g_balanced,
      na.rm = FALSE
    ),
    NA_real_
  )

  # Should work when na.rm = TRUE if enough data remains
  vr_na.rm <- bal_vr(
    .covariate = x_na,
    .exposure = data$g_balanced,
    na.rm = TRUE
  )
  expect_true(is.finite(vr_na.rm) || is.na(vr_na.rm))
})

test_that("bal_vr error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_halfmoon_error(
    bal_vr(
      .covariate = data$x_cont,
      .exposure = rep(1, 100)
    ),
    "halfmoon_group_error"
  )

  # Now supports 3+ groups (categorical)
  expect_no_error(bal_vr(
    .covariate = data$x_cont,
    .exposure = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_halfmoon_error(
    bal_vr(
      .covariate = data$x_cont[1:50],
      .exposure = data$g_balanced
    ),
    "halfmoon_length_error"
  )

  expect_halfmoon_error(
    bal_vr(
      .covariate = data$x_cont,
      .exposure = data$g_balanced,
      .weights = data$w_uniform[1:50]
    ),
    "halfmoon_length_error"
  )
})

# =============================================================================
# TESTS FOR bal_ks()
# =============================================================================

test_that("bal_ks handles basic cases", {
  data <- create_test_data()

  # Basic functionality
  ks <- bal_ks(.covariate = data$x_cont, .exposure = data$g_balanced)
  expect_true(is.finite(ks))
  expect_true(ks >= 0)
  expect_true(ks <= 1)

  # With weights
  ks_weighted <- bal_ks(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  expect_true(is.finite(ks_weighted))
  expect_true(ks_weighted >= 0)
  expect_true(ks_weighted <= 1)
})

test_that("bal_ks gives 0 for identical distributions", {
  # Identical distributions should give KS = 0
  x <- c(1, 2, 3, 1, 2, 3)
  g <- c(0, 0, 0, 1, 1, 1)

  ks_identical <- bal_ks(.covariate = x, .exposure = g)
  expect_equal(ks_identical, 0)
})

test_that("bal_ks gives >0 for different distributions", {
  # Different distributions should give KS > 0
  x <- c(1, 2, 3, 4, 5, 6)
  g <- c(0, 0, 0, 1, 1, 1)

  ks_different <- bal_ks(.covariate = x, .exposure = g)
  expect_true(ks_different > 0)
})

test_that("bal_ks handles binary variables", {
  data <- create_test_data()

  # Binary variables should return difference in proportions
  ks_binary <- bal_ks(.covariate = data$x_binary, .exposure = data$g_balanced)
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
    bal_ks(.covariate = x_na, .exposure = data$g_balanced, na.rm = FALSE),
    NA_real_
  )

  # Should work when na.rm = TRUE if enough data remains
  ks_na.rm <- bal_ks(
    .covariate = x_na,
    .exposure = data$g_balanced,
    na.rm = TRUE
  )
  expect_true(is.finite(ks_na.rm) || is.na(ks_na.rm))
})

test_that("bal_ks error handling", {
  data <- create_test_data()

  # Should error with wrong number of groups
  expect_halfmoon_error(
    bal_ks(.covariate = data$x_cont, .exposure = rep(1, 100)),
    "halfmoon_group_error"
  )
  # Now supports 3+ groups (categorical)
  expect_no_error(bal_ks(
    .covariate = data$x_cont,
    .exposure = c(rep(1, 50), rep(2, 25), rep(3, 25))
  ))

  # Should error with mismatched lengths
  expect_halfmoon_error(
    bal_ks(
      .covariate = data$x_cont[1:50],
      .exposure = data$g_balanced
    ),
    "halfmoon_length_error"
  )

  expect_halfmoon_error(
    bal_ks(
      .covariate = data$x_cont,
      .exposure = data$g_balanced,
      .weights = data$w_uniform[1:50]
    ),
    "halfmoon_length_error"
  )
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
  cor_weighted <- bal_corr(x, y, .weights = w)
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
    .weights = data$w_uniform
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

  expect_halfmoon_warning({
    cor_zero <- bal_corr(x_zero, y_normal)
  })
  expect_true(is.na(cor_zero))

  # Both zero variance should return NA
  y_zero <- rep(2, 100)
  expect_halfmoon_warning({
    cor_both_zero <- bal_corr(x_zero, y_zero)
  })
  expect_true(is.na(cor_both_zero))
})

test_that("bal_corr error handling", {
  data <- create_test_data()

  # Should error with mismatched lengths
  expect_halfmoon_error(
    bal_corr(data$x_cont[1:50], data$x_skewed),
    "halfmoon_length_error"
  )

  expect_halfmoon_error(
    bal_corr(
      data$x_cont,
      data$x_skewed,
      .weights = data$w_uniform[1:50]
    ),
    "halfmoon_length_error"
  )
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
      .covariate = data_large$x_cont,
      .exposure = data_large$g_balanced
    )
    vr_large <- bal_vr(
      .covariate = data_large$x_cont,
      .exposure = data_large$g_balanced
    )
    ks_large <- bal_ks(
      .covariate = data_large$x_cont,
      .exposure = data_large$g_balanced
    )
    cor_large <- bal_corr(data_large$x_cont, data_large$x_skewed)
  })
})

test_that("functions handle extreme weights", {
  data <- create_test_data()

  # Test with extreme weights
  expect_no_error({
    smd_extreme <- bal_smd(
      .covariate = data$x_cont,
      .exposure = data$g_balanced,
      .weights = data$w_extreme
    )
    vr_extreme <- bal_vr(
      .covariate = data$x_cont,
      .exposure = data$g_balanced,
      .weights = data$w_extreme
    )
    ks_extreme <- bal_ks(
      .covariate = data$x_cont,
      .exposure = data$g_balanced,
      .weights = data$w_extreme
    )
    cor_extreme <- bal_corr(
      data$x_cont,
      data$x_skewed,
      .weights = data$w_extreme
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
    smd_unbal <- bal_smd(
      .covariate = data$x_cont,
      .exposure = data$g_unbalanced
    )
    vr_unbal <- bal_vr(
      .covariate = data$x_cont,
      .exposure = data$g_unbalanced
    )
    ks_unbal <- bal_ks(.covariate = data$x_cont, .exposure = data$g_unbalanced)
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
  skip_on_cran()
  data <- create_test_data(seed = 789)

  # Continuous variables
  our_vr_cont <- bal_vr(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  cobalt_vr_cont <- cobalt::col_w_vr(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform
  )[1]
  expect_equal(our_vr_cont, cobalt_vr_cont, tolerance = 1e-10)

  # Binary variables
  our_vr_bin <- bal_vr(
    .covariate = data$x_binary,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
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
    .covariate = data$x_cont,
    .exposure = data$g_balanced
  )
  cobalt_vr_unw <- cobalt::col_w_vr(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]
  expect_equal(our_vr_unw, cobalt_vr_unw, tolerance = 1e-10)
})

test_that("bal_ks matches cobalt::col_w_ks", {
  skip_if_not_installed("cobalt")
  skip_on_cran()
  data <- create_test_data(seed = 789)

  # Continuous variables
  our_ks_cont <- bal_ks(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  cobalt_ks_cont <- cobalt::col_w_ks(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform
  )[1]
  expect_equal(our_ks_cont, cobalt_ks_cont, tolerance = 1e-10)

  # Binary variables
  our_ks_bin <- bal_ks(
    .covariate = data$x_binary,
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  cobalt_ks_bin <- cobalt::col_w_ks(
    matrix(data$x_binary, ncol = 1),
    treat = data$g_balanced,
    weights = data$w_uniform,
    bin.vars = TRUE
  )[1]
  expect_equal(our_ks_bin, cobalt_ks_bin, tolerance = 1e-10)

  # Unweighted
  our_ks_unw <- bal_ks(.covariate = data$x_cont, .exposure = data$g_balanced)
  cobalt_ks_unw <- cobalt::col_w_ks(
    matrix(data$x_cont, ncol = 1),
    treat = data$g_balanced
  )[1]
  expect_equal(our_ks_unw, cobalt_ks_unw, tolerance = 1e-10)
})

test_that("bal_smd matches cobalt::col_w_smd for binary variables", {
  skip_if_not_installed("cobalt")
  skip_on_cran()
  data <- create_test_data(seed = 789)

  # Binary variables should match exactly
  # Note: cobalt reports the treated group minus the untreated group, so group
  # 0 is the reference level for this comparison
  our_smd_bin <- bal_smd(
    .covariate = data$x_binary,
    .exposure = data$g_balanced,
    .weights = data$w_uniform,
    .reference_level = 0
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
  skip_on_cran()
  data <- create_test_data(seed = 789)

  # Continuous variables should be close (different pooled variance approaches)
  # Note: cobalt reports the treated group minus the untreated group, so group
  # 0 is the reference level for this comparison
  our_smd_cont <- bal_smd(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .weights = data$w_uniform,
    .reference_level = 0
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
  skip_on_cran()
  data <- create_test_data(seed = 789)

  # Add missing values
  x_na <- data$x_cont
  x_na[data$na_indices] <- NA

  # Both should handle missing values similarly
  our_vr_na <- bal_vr(
    .covariate = x_na,
    .exposure = data$g_balanced,
    na.rm = TRUE
  )
  cobalt_vr_na <- cobalt::col_w_vr(
    matrix(x_na, ncol = 1),
    treat = data$g_balanced,
    na.rm = TRUE
  )[1]
  expect_equal(our_vr_na, cobalt_vr_na, tolerance = 1e-10)

  our_ks_na <- bal_ks(
    .covariate = x_na,
    .exposure = data$g_balanced,
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
  skip_on_cran()
  data <- create_test_data(seed = 789)

  # Test variance ratio with different reference groups
  our_vr_ref0 <- bal_vr(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .reference_level = 0
  )
  our_vr_ref1 <- bal_vr(
    .covariate = data$x_cont,
    .exposure = data$g_balanced,
    .reference_level = 1
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
    .weights = data$w_uniform
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
  ks_weighted <- bal_ks(data$age, data$qsmk, .weights = data$w_uniform)
  expect_true(is.finite(ks_weighted))
  expect_true(ks_weighted >= 0 && ks_weighted <= 1)

  # Test with real propensity score weights
  ks_ps_weighted <- bal_ks(data$age, data$qsmk, .weights = data$w_ate)
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
    .weights = data$w_uniform
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
  smd_extreme <- bal_smd(data$age, data$qsmk, .weights = data$w_extreme)
  expect_true(is.finite(smd_extreme))

  vr_extreme <- bal_vr(
    data$age,
    data$qsmk,
    .weights = data$w_extreme
  )
  expect_true(is.finite(vr_extreme) && vr_extreme > 0)

  ks_extreme <- bal_ks(data$age, data$qsmk, .weights = data$w_extreme)
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
  smd_ate <- bal_smd(data$age, data$qsmk, .weights = data$w_ate)
  expect_true(is.finite(smd_ate))

  vr_ate <- bal_vr(data$age, data$qsmk, .weights = data$w_ate)
  expect_true(is.finite(vr_ate) && vr_ate > 0)

  ks_ate <- bal_ks(data$age, data$qsmk, .weights = data$w_ate)
  expect_true(is.finite(ks_ate) && ks_ate >= 0 && ks_ate <= 1)

  # Test all functions with ATT weights
  smd_att <- bal_smd(data$age, data$qsmk, .weights = data$w_att)
  expect_true(is.finite(smd_att))

  vr_att <- bal_vr(data$age, data$qsmk, .weights = data$w_att)
  expect_true(is.finite(vr_att) && vr_att > 0)

  ks_att <- bal_ks(data$age, data$qsmk, .weights = data$w_att)
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
    .covariates = data.frame(x = data$x_cont, y = data$x_skewed),
    .exposure = data$g_balanced
  )
  expect_true(is.finite(energy))
  expect_true(energy >= 0)

  # With weights
  energy_weighted <- bal_energy(
    .covariates = data.frame(x = data$x_cont, y = data$x_skewed),
    .exposure = data$g_balanced,
    .weights = data$w_uniform
  )
  expect_true(is.finite(energy_weighted))
  expect_true(energy_weighted >= 0)
})

test_that("bal_energy handles different estimands", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)

  # ATE
  energy_ate <- bal_energy(
    .covariates = covs,
    .exposure = data$g_balanced,
    estimand = "ATE"
  )

  # ATT
  energy_att <- bal_energy(
    .covariates = covs,
    .exposure = data$g_balanced,
    estimand = "ATT"
  )

  # ATC
  energy_atc <- bal_energy(
    .covariates = covs,
    .exposure = data$g_balanced,
    estimand = "ATC"
  )

  # Between-group only
  energy_between <- bal_energy(
    .covariates = covs,
    .exposure = data$g_balanced,
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

  # Unweighted, the focal target is the focal group's own distribution, so both
  # focal estimands reduce to the between-group energy distance.
  expect_equal(energy_att, energy_between, tolerance = 1e-8)
  expect_equal(energy_atc, energy_between, tolerance = 1e-8)

  # The ATE targets the whole unweighted sample instead, so it does not.
  expect_false(isTRUE(all.equal(energy_ate, energy_between)))
})

test_that("bal_energy matches cobalt for weighted binary estimands", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  set.seed(11)
  n <- 80
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rbinom(n, 1, 0.4)
  )
  treatment <- rbinom(n, 1, 0.5)
  weights <- runif(n, 0.3, 3)

  cobalt_energy <- function(estimand = NULL, focal = NULL, improved = TRUE) {
    init <- cobalt::bal.init(
      covariates,
      treat = treatment,
      stat = "energy.dist",
      estimand = estimand,
      focal = focal,
      improved = improved
    )
    cobalt::bal.compute(init, weights = weights)
  }

  expect_equal(
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATE"),
    cobalt_energy("ATE"),
    tolerance = 1e-8
  )

  expect_equal(
    bal_energy(
      covariates,
      treatment,
      .weights = weights,
      estimand = "ATE",
      use_improved = FALSE
    ),
    cobalt_energy("ATE", improved = FALSE),
    tolerance = 1e-8
  )

  expect_equal(
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATT"),
    cobalt_energy("ATT", focal = "1"),
    tolerance = 1e-8
  )

  expect_equal(
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATC"),
    cobalt_energy("ATC", focal = "0"),
    tolerance = 1e-8
  )

  expect_equal(
    bal_energy(covariates, treatment, .weights = weights),
    cobalt_energy(),
    tolerance = 1e-8
  )

  # Weights that vary inside the focal group move the weighted focal
  # distribution away from its unweighted target, so the focal estimands no
  # longer collapse onto the between-group distance.
  expect_false(isTRUE(all.equal(
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATT"),
    bal_energy(covariates, treatment, .weights = weights)
  )))
  expect_false(isTRUE(all.equal(
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATT"),
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATC")
  )))
})

test_that("bal_energy matches cobalt for a weighted multi-category ATE", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  set.seed(2027)
  n <- 90
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  treatment <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  weights <- runif(n, 0.3, 3)

  expect_equal(
    bal_energy(covariates, treatment, .weights = weights, estimand = "ATE"),
    cobalt::bal.compute(
      cobalt::bal.init(
        covariates,
        treat = treatment,
        stat = "energy.dist",
        estimand = "ATE"
      ),
      weights = weights
    ),
    tolerance = 1e-8
  )
})

test_that("bal_energy defaults the focal level to an observed level of any type", {
  set.seed(707)
  n <- 100
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  numeric_treatment <- rbinom(n, 1, 0.5)
  text_treatment <- factor(
    ifelse(numeric_treatment == 1, "treated", "control"),
    levels = c("control", "treated")
  )
  weights <- runif(n, 0.3, 3)

  text_energy <- function(estimand) {
    bal_energy(
      covariates,
      text_treatment,
      .weights = weights,
      estimand = estimand
    )
  }

  for (estimand in c("ATT", "ATC")) {
    expect_no_warning(text_energy(estimand))

    numeric_energy <- bal_energy(
      covariates,
      numeric_treatment,
      .weights = weights,
      estimand = estimand
    )

    expect_equal(text_energy(estimand), numeric_energy, tolerance = 1e-8)
    expect_gte(text_energy(estimand), 0)
  }

  # ATT takes the last observed level and ATC the first, so naming those levels
  # reproduces the defaults.
  expect_equal(
    bal_energy(
      covariates,
      text_treatment,
      .weights = weights,
      estimand = "ATT",
      .focal_level = "treated"
    ),
    bal_energy(
      covariates,
      text_treatment,
      .weights = weights,
      estimand = "ATT"
    ),
    tolerance = 1e-10
  )
  expect_equal(
    bal_energy(
      covariates,
      text_treatment,
      .weights = weights,
      estimand = "ATC",
      .focal_level = "control"
    ),
    bal_energy(
      covariates,
      text_treatment,
      .weights = weights,
      estimand = "ATC"
    ),
    tolerance = 1e-10
  )
})

test_that("bal_energy reads the observed levels of a factor exposure", {
  set.seed(808)
  n <- 60
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  declared <- factor(
    sample(c("a", "b"), n, replace = TRUE),
    levels = c("a", "b", "unused")
  )

  for (estimand in list(NULL, "ATE", "ATT", "ATC")) {
    expect_equal(
      bal_energy(covariates, declared, estimand = estimand),
      bal_energy(covariates, droplevels(declared), estimand = estimand),
      tolerance = 1e-10
    )
  }
})

test_that("bal_energy validates .focal_level", {
  set.seed(909)
  n <- 60
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  treatment <- rbinom(n, 1, 0.5)

  expect_error(
    bal_energy(
      covariates,
      treatment,
      estimand = "ATT",
      .focal_level = "typo"
    ),
    class = "halfmoon_reference_error"
  )

  expect_error(
    bal_energy(
      covariates,
      treatment,
      estimand = "ATC",
      .focal_level = c("0", "1")
    ),
    class = "halfmoon_arg_error"
  )

  expect_error(
    bal_energy(
      covariates,
      treatment,
      estimand = "ATT",
      .focal_level = character(0)
    ),
    class = "halfmoon_arg_error"
  )

  # A value that names a level is accepted whatever type it is supplied as
  expect_equal(
    bal_energy(covariates, treatment, estimand = "ATT", .focal_level = 1),
    bal_energy(covariates, treatment, estimand = "ATT", .focal_level = "1"),
    tolerance = 1e-10
  )
})

test_that("bal_energy rejects option arguments that are not single values", {
  set.seed(1010)
  n <- 40
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  treatment <- rbinom(n, 1, 0.5)

  expect_error(
    bal_energy(covariates, treatment, estimand = character(0)),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_energy(covariates, treatment, estimand = c("ATE", "ATT")),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_energy(covariates, treatment, criterion = character(0)),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_energy(covariates, treatment, exposure_type = character(0)),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_energy(covariates, treatment, use_improved = logical(0)),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_energy(covariates, treatment, standardized = NA),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_energy(covariates, treatment, na.rm = c(TRUE, FALSE)),
    class = "halfmoon_arg_error"
  )
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
    .covariates = covs,
    .exposure = multi_group,
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

  # Default dependence criterion returns D(w), which is finite and nonnegative
  # but not bounded above by 1.
  energy_cont <- bal_energy(
    .covariates = covs,
    .exposure = continuous_treatment,
    estimand = NULL # Must be NULL for continuous
  )
  expect_true(is.finite(energy_cont))
  expect_true(energy_cont >= 0)

  # The dcor criterion returns a distance correlation bounded in [0, 1].
  dcor_cont <- bal_energy(
    .covariates = covs,
    .exposure = continuous_treatment,
    criterion = "dcor"
  )
  expect_true(is.finite(dcor_cont))
  expect_true(dcor_cont >= 0)
  expect_true(dcor_cont <= 1)

  # Should error if estimand is not NULL for continuous treatment
  expect_halfmoon_error(
    bal_energy(
      .covariates = covs,
      .exposure = continuous_treatment,
      estimand = "ATE"
    ),
    "halfmoon_arg_error"
  )
})

test_that("bal_energy handles perfect balance", {
  # Create perfectly balanced data
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  covs <- data.frame(x = c(x, x))
  group <- c(rep(0, n), rep(1, n))

  energy_perfect <- bal_energy(
    .covariates = covs,
    .exposure = group,
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
    .covariates = covs,
    .exposure = group
  )
  expect_true(is.finite(energy))
  expect_true(energy >= 0)
})

test_that("bal_energy reports missing values it is asked to keep as NA", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)
  covs$x[data$na_indices] <- NA

  # A missing covariate, exposure, or weight is the NA sentinel, not an error
  expect_identical(
    bal_energy(
      .covariates = covs,
      .exposure = data$g_balanced,
      na.rm = FALSE
    ),
    NA_real_
  )

  exposure_na <- data$g_balanced
  exposure_na[data$na_indices] <- NA
  expect_identical(
    bal_energy(
      .covariates = data.frame(x = data$x_cont, y = data$x_skewed),
      .exposure = exposure_na
    ),
    NA_real_
  )

  weights_na <- data$w_uniform
  weights_na[data$na_indices] <- NA
  expect_identical(
    bal_energy(
      .covariates = data.frame(x = data$x_cont, y = data$x_skewed),
      .exposure = data$g_balanced,
      .weights = weights_na
    ),
    NA_real_
  )

  # A continuous exposure follows the same policy
  expect_identical(
    bal_energy(
      .covariates = covs,
      .exposure = rnorm(nrow(covs))
    ),
    NA_real_
  )
})

test_that("bal_energy drops incomplete rows when na.rm is TRUE", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)
  covs$x[data$na_indices] <- NA
  complete <- !is.na(covs$x)

  expect_equal(
    bal_energy(
      .covariates = covs,
      .exposure = data$g_balanced,
      .weights = data$w_uniform,
      na.rm = TRUE
    ),
    bal_energy(
      .covariates = covs[complete, ],
      .exposure = data$g_balanced[complete],
      .weights = data$w_uniform[complete]
    ),
    tolerance = 1e-10
  )
})

test_that("bal_energy use_improved parameter works", {
  data <- create_test_data()
  covs <- data.frame(x = data$x_cont, y = data$x_skewed)

  # Improved vs standard for ATE
  energy_improved <- bal_energy(
    .covariates = covs,
    .exposure = data$g_balanced,
    estimand = "ATE",
    use_improved = TRUE
  )

  energy_standard <- bal_energy(
    .covariates = covs,
    .exposure = data$g_balanced,
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

  # Should now handle non-numeric covariates by converting to dummy variables
  expect_no_error(bal_energy(
    .covariates = data.frame(x = as.character(data$x_cont)),
    .exposure = data$g_balanced
  ))

  # Should error with mismatched dimensions
  expect_halfmoon_error(
    bal_energy(
      .covariates = data.frame(x = data$x_cont[1:50]),
      .exposure = data$g_balanced
    ),
    "halfmoon_length_error"
  )

  # Should error with wrong number of groups (only 1)
  expect_halfmoon_error(
    bal_energy(
      .covariates = data.frame(x = data$x_cont),
      .exposure = rep(1, 100)
    ),
    "halfmoon_group_error"
  )

  # Should error with negative weights
  expect_halfmoon_error(
    bal_energy(
      .covariates = data.frame(x = data$x_cont),
      .exposure = data$g_balanced,
      .weights = c(-1, rep(1, 99))
    ),
    "halfmoon_range_error"
  )
})

test_that("bal_energy handles NHEFS data", {
  data <- get_nhefs_compute_data()

  # Select numeric covariates
  covs <- dplyr::select(data, age, wt71, smokeyrs)

  # Basic energy distance
  energy <- bal_energy(
    .covariates = covs,
    .exposure = data$qsmk
  )
  expect_true(is.finite(energy))
  expect_true(energy >= 0)

  # With ATE weights
  energy_ate <- bal_energy(
    .covariates = covs,
    .exposure = data$qsmk,
    .weights = data$w_ate,
    estimand = "ATE"
  )
  expect_true(is.finite(energy_ate))
  expect_true(energy_ate >= 0)

  # With ATT weights
  energy_att <- bal_energy(
    .covariates = covs,
    .exposure = data$qsmk,
    .weights = data$w_att,
    estimand = "ATT"
  )
  expect_true(is.finite(energy_att))
  expect_true(energy_att >= 0)

  # Weighted should generally be lower (better balance)
  expect_true(energy_ate < energy || energy_att < energy)
})

test_that("bal_energy comparison with cobalt package", {
  testthat::skip_if_not_installed("cobalt")
  skip_on_cran()

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
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights
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
  skip_on_cran()

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
    .covariates = covariates,
    .exposure = treatment
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

test_that("bal_energy dcor criterion reproduces cobalt distance.cor", {
  testthat::skip_if_not_installed("cobalt")
  skip_on_cran()

  # Create test data with continuous treatment
  set.seed(321)
  n <- 150
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  treatment <- rnorm(n)

  # Opt-in distance correlation criterion
  our_dcor <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    criterion = "dcor",
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

test_that("bal_energy dcor criterion with standardized = FALSE reproduces sqrt distance covariance", {
  skip_on_cran()

  set.seed(321)
  n <- 150
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  treatment <- rnorm(n)

  our_dcov <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    criterion = "dcor",
    standardized = FALSE
  )

  # Hand-coded replica of the unstandardized distance covariance algorithm:
  # weighted-variance-scaled distances, double-centered, quadratic form in
  # sum-1 weights, returned as sqrt(dcov).
  expected_sqrt_dcov <- function(covariates, treatment, weights = NULL) {
    covariates <- as.matrix(covariates)
    n_obs <- nrow(covariates)
    if (is.null(weights)) {
      weights <- rep(1, n_obs)
    }
    weights_norm <- weights / sum(weights)

    binary_vars <- apply(covariates, 2, function(col) {
      unique_vals <- unique(col)
      length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
    })

    covariate_vars <- vapply(
      seq_len(ncol(covariates)),
      function(j) {
        col <- covariates[, j]
        if (binary_vars[j]) {
          p <- sum(weights_norm * col)
          p * (1 - p)
        } else {
          mean_x <- sum(weights_norm * col)
          denom <- 1 - sum(weights_norm^2)
          if (denom <= 0) {
            sum(weights_norm * (col - mean_x)^2)
          } else {
            sum(weights_norm * (col - mean_x)^2) / denom
          }
        }
      },
      numeric(1)
    )

    mean_t <- sum(weights_norm * treatment)
    denom <- 1 - sum(weights_norm^2)
    treatment_var <- if (denom <= 0) {
      sum(weights_norm * (treatment - mean_t)^2)
    } else {
      sum(weights_norm * (treatment - mean_t)^2) / denom
    }

    covariate_vars[covariate_vars == 0] <- 1
    if (treatment_var == 0) {
      treatment_var <- 1
    }

    scaled_cov <- scale(covariates, scale = sqrt(covariate_vars))
    scaled_treat <- treatment / sqrt(treatment_var)

    cov_dist <- as.matrix(dist(scaled_cov))
    treat_dist <- as.matrix(dist(scaled_treat))

    cov_means <- colMeans(cov_dist)
    cov_centered <- cov_dist +
      mean(cov_means) -
      outer(cov_means, cov_means, "+")
    treat_means <- colMeans(treat_dist)
    treat_centered <- treat_dist +
      mean(treat_means) -
      outer(treat_means, treat_means, "+")

    P <- cov_centered * treat_centered
    dcov <- as.numeric(t(weights_norm) %*% P %*% weights_norm)
    if (dcov <= 0) {
      return(0)
    }
    sqrt(dcov)
  }

  expect_equal(
    our_dcov,
    expected_sqrt_dcov(covariates, treatment),
    tolerance = 1e-8
  )
})

test_that("bal_energy dependence criterion matches independenceWeights D_w", {
  skip_if_not_installed("independenceWeights")
  skip_on_cran()

  set.seed(2023)
  n <- 150
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rbinom(n, 1, 0.4)
  covariates <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  covariate_matrix <- as.matrix(covariates)
  treatment <- 0.5 * x1 - 0.3 * x3 + rnorm(n)
  weights <- runif(n, 0.2, 3)

  our_default <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights
  )
  ref_adj <- independenceWeights::weighted_energy_stats(
    treatment,
    covariate_matrix,
    weights,
    dimension_adj = TRUE
  )$D_w
  expect_equal(our_default, ref_adj, tolerance = 1e-8)

  our_no_adj <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights,
    dimension_adj = FALSE
  )
  ref_no_adj <- independenceWeights::weighted_energy_stats(
    treatment,
    covariate_matrix,
    weights,
    dimension_adj = FALSE
  )$D_w
  expect_equal(our_no_adj, ref_no_adj, tolerance = 1e-8)
})

test_that("bal_energy dependence criterion ignores standardized", {
  skip_if_not_installed("independenceWeights")
  skip_on_cran()

  set.seed(2023)
  n <- 150
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rbinom(n, 1, 0.4)
  covariates <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  covariate_matrix <- as.matrix(covariates)
  treatment <- 0.5 * x1 - 0.3 * x3 + rnorm(n)
  weights <- runif(n, 0.2, 3)

  ref_adj <- independenceWeights::weighted_energy_stats(
    treatment,
    covariate_matrix,
    weights,
    dimension_adj = TRUE
  )$D_w

  # standardized is ignored for the dependence criterion: standardized = FALSE
  # must still return the dimension-adjusted D_w, both via the default criterion
  # and when the dependence criterion is named explicitly. This pins that a
  # standardized = FALSE call cannot be routed into the unstandardized dcor path.
  our_default_unstd <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights,
    standardized = FALSE
  )
  expect_equal(our_default_unstd, ref_adj, tolerance = 1e-8)

  our_dependence_unstd <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights,
    criterion = "dependence",
    standardized = FALSE
  )
  expect_equal(our_dependence_unstd, ref_adj, tolerance = 1e-8)
})

test_that("bal_energy dependence criterion with unit weights reduces to unweighted distance covariance", {
  skip_if_not_installed("independenceWeights")
  skip_on_cran()

  set.seed(11)
  n <- 120
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  covariate_matrix <- as.matrix(covariates)
  treatment <- rnorm(n)

  ref <- independenceWeights::weighted_energy_stats(
    treatment,
    covariate_matrix,
    rep(1, n),
    dimension_adj = TRUE
  )
  # With unit weights the marginal energy terms vanish and D_w collapses to the
  # unweighted distance covariance.
  expect_equal(ref$D_w, ref$distcov_unweighted, tolerance = 1e-8)

  our_unit <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = rep(1, n)
  )
  expect_equal(our_unit, ref$D_w, tolerance = 1e-8)

  our_null <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = NULL
  )
  expect_equal(our_null, ref$D_w, tolerance = 1e-8)
})

test_that("bal_energy dependence criterion is nonnegative across random weights", {
  skip_on_cran()

  set.seed(99)
  n <- 100
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rbinom(n, 1, 0.5)
  )
  treatment <- rnorm(n)

  for (i in seq_len(5)) {
    weights <- runif(n, 0.1, 5)
    result <- bal_energy(
      .covariates = covariates,
      .exposure = treatment,
      .weights = weights
    )
    expect_true(result >= 0)
  }
})

test_that("bal_energy dependence criterion counts marginal distortion under degenerate weights", {
  skip_if_not_installed("independenceWeights")
  skip_on_cran()

  set.seed(505)
  n <- 200
  x <- rnorm(n)
  covariates <- data.frame(x = x)
  covariate_matrix <- as.matrix(covariates)
  treatment <- x + rnorm(n, sd = 0.3)
  weights <- as.numeric(abs(x) < 0.2) + 1e-6

  ref <- independenceWeights::weighted_energy_stats(
    treatment,
    covariate_matrix,
    weights,
    dimension_adj = TRUE
  )
  our_default <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights
  )
  expect_equal(our_default, ref$D_w, tolerance = 1e-8)

  # The marginal-distortion terms keep D_w well above the weighted distance
  # covariance component alone, so degenerate weights are penalized.
  expect_gt(ref$D_w, ref$distcov_weighted)
  expect_gt(our_default, ref$distcov_weighted)
})

test_that("check_balance with continuous exposure and energy metric matches bal_energy default", {
  set.seed(2024)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    a = rnorm(n)
  )

  cb <- check_balance(df, c(x1, x2), a, .metrics = "energy")
  energy_estimate <- cb$estimate[
    cb$metric == "energy" & cb$method == "observed"
  ]

  direct <- bal_energy(
    .covariates = df[c("x1", "x2")],
    .exposure = df$a
  )
  expect_equal(energy_estimate, direct, tolerance = 1e-8)
})

test_that("bal_energy validates criterion and criterion-specific arguments", {
  set.seed(1)
  n <- 60
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  continuous_treatment <- rnorm(n)
  discrete_treatment <- rbinom(n, 1, 0.5)

  # Unknown criterion value is rejected.
  expect_error(
    bal_energy(
      .covariates = covariates,
      .exposure = continuous_treatment,
      criterion = "bogus"
    ),
    class = "halfmoon_arg_error"
  )

  # The dcor criterion is continuous-only.
  expect_error(
    bal_energy(
      .covariates = covariates,
      .exposure = discrete_treatment,
      criterion = "dcor"
    ),
    class = "halfmoon_arg_error"
  )

  # Non-default dimension_adj is continuous-only.
  expect_error(
    bal_energy(
      .covariates = covariates,
      .exposure = discrete_treatment,
      dimension_adj = FALSE
    ),
    class = "halfmoon_arg_error"
  )

  # A non-NULL estimand with a continuous exposure remains an error.
  expect_error(
    bal_energy(
      .covariates = covariates,
      .exposure = continuous_treatment,
      estimand = "ATE"
    ),
    class = "halfmoon_arg_error"
  )
})

test_that("bal_energy handles categorical covariates", {
  testthat::skip_if_not_installed("cobalt", minimum_version = "4.5.2")
  skip_on_cran()

  set.seed(789)
  n <- 100

  # Create test data with mixed types
  covariates <- data.frame(
    numeric1 = rnorm(n),
    numeric2 = rnorm(n),
    factor1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    character1 = sample(c("X", "Y"), n, replace = TRUE)
  )

  treatment <- rbinom(n, 1, 0.5)

  # Our implementation with categorical variables
  our_result <- bal_energy(
    .covariates = covariates,
    .exposure = treatment
  )

  # Cobalt with same data
  cobalt_result <- cobalt::bal.compute(
    x = covariates,
    treat = treatment,
    stat = "energy.dist"
  )

  # Should be very close (cobalt converts categoricals to dummies internally)
  expect_equal(our_result, cobalt_result, tolerance = 1e-4)

  # Test with weights
  weights <- runif(n, 0.5, 1.5)

  our_weighted <- bal_energy(
    .covariates = covariates,
    .exposure = treatment,
    .weights = weights
  )

  cobalt_weighted <- cobalt::bal.compute(
    x = covariates,
    treat = treatment,
    weights = weights,
    stat = "energy.dist"
  )

  expect_equal(our_weighted, cobalt_weighted, tolerance = 1e-4)
})

test_that("balance functions work seamlessly with psw objects from propensity package", {
  # This test ensures psw objects from the propensity package work throughout
  # the halfmoon package without requiring explicit conversion
  data(nhefs_weights)

  # Verify we have psw objects in the dataset
  expect_true(propensity::is_psw(nhefs_weights$w_cat_ate))
  expect_true(propensity::is_psw(nhefs_weights$w_cat_att_none))

  # Test that balance functions work directly with psw weights. The categorical
  # exposure and its weights both carry missing values, so na.rm = TRUE is
  # needed for a non-missing result.
  result_smd <- bal_smd(
    nhefs_weights$age,
    nhefs_weights$alcoholfreq_cat,
    .weights = nhefs_weights$w_cat_ate,
    na.rm = TRUE
  )
  expect_true(all(is.finite(result_smd)))

  result_vr <- bal_vr(
    nhefs_weights$wt71,
    nhefs_weights$alcoholfreq_cat,
    .weights = nhefs_weights$w_cat_att_none,
    na.rm = TRUE
  )
  expect_true(all(is.finite(result_vr) & result_vr > 0))

  result_ks <- bal_ks(
    nhefs_weights$age,
    nhefs_weights$alcoholfreq_cat,
    .weights = nhefs_weights$w_cat_ato,
    na.rm = TRUE
  )
  expect_true(all(is.finite(result_ks) & result_ks >= 0 & result_ks <= 1))

  # Test check_balance works with psw weights
  balance_results <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .weights = w_cat_ate,
    .metrics = "smd",
    include_observed = FALSE,
    na.rm = TRUE
  )
  expect_s3_class(balance_results, "data.frame")
  expect_true(nrow(balance_results) > 0)
  expect_true(all(is.finite(balance_results$estimate)))

  # Test weighted_quantile works with psw weights
  quantiles <- weighted_quantile(
    nhefs_weights$age,
    c(0.25, 0.5, 0.75),
    nhefs_weights$w_cat_ate
  )
  expect_length(quantiles, 3)
  expect_true(all(is.finite(quantiles)))
})

test_that("binary bal_* functions reject an invalid `.reference_level`", {
  set.seed(2024)
  x <- rnorm(100)
  g <- rep(c(0, 1), each = 50)

  expect_error(
    bal_vr(x, g, .reference_level = c(0, 1)),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_ks(x, g, .reference_level = c(0, 1)),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_vr(x, g, .reference_level = 1.5),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_ks(x, g, .reference_level = 1.5),
    class = "halfmoon_arg_error"
  )
})

test_that("binary bal_* functions resolve `.reference_level` by value first", {
  set.seed(2024)
  x <- rnorm(100)
  g <- rep(c(0, 1), each = 50)

  # 0 and 1 are level values, so they name levels rather than positions
  expect_equal(bal_vr(x, g, .reference_level = 0), bal_vr(x, g))
  expect_equal(
    bal_vr(x, g, .reference_level = 1),
    1 / bal_vr(x, g, .reference_level = 0)
  )

  # An integral index still resolves positionally when it is not a level value
  g_factor <- factor(g, levels = c(0, 1), labels = c("a", "b"))
  expect_equal(
    bal_vr(x, g_factor, .reference_level = 2),
    bal_vr(x, g_factor, .reference_level = "b")
  )
  expect_equal(
    bal_ks(x, g_factor, .reference_level = 1),
    bal_ks(x, g_factor, .reference_level = "a")
  )
})

test_that("categorical bal_* functions reject an invalid `.reference_level`", {
  x <- nhefs_weights$age
  g <- nhefs_weights$alcoholfreq_cat

  expect_error(
    bal_smd(x, g, .reference_level = c("none", "daily")),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_smd(x, g, .reference_level = 1.5),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_vr(x, g, .reference_level = 2.5),
    class = "halfmoon_arg_error"
  )
  expect_error(
    bal_ks(x, g, .reference_level = 2.5),
    class = "halfmoon_arg_error"
  )
})

test_that("categorical bal_* functions still accept an integral index", {
  x <- nhefs_weights$age
  g <- nhefs_weights$alcoholfreq_cat

  expect_equal(
    bal_smd(x, g, .reference_level = 2),
    bal_smd(x, g, .reference_level = "lt_12_per_year")
  )
})

# =============================================================================
# NA, ZERO-WEIGHT, AND UNUSED-LEVEL POLICY
# =============================================================================

test_that("bal_smd with na.rm = TRUE drops rows with missing weights", {
  set.seed(31)
  x <- rnorm(40)
  g <- rep(c(0, 1), each = 20)
  w <- runif(40, 0.5, 1.5)
  w[c(3, 25)] <- NA

  keep <- !is.na(w)
  expect_equal(
    bal_smd(x, g, .weights = w, na.rm = TRUE),
    bal_smd(x[keep], g[keep], .weights = w[keep], na.rm = TRUE)
  )

  psw_weights <- propensity::psw(w, estimand = "ate")
  expect_equal(
    bal_smd(x, g, .weights = psw_weights, na.rm = TRUE),
    bal_smd(x[keep], g[keep], .weights = w[keep], na.rm = TRUE)
  )
})

test_that("bal_smd with na.rm = TRUE drops rows with a missing exposure", {
  set.seed(32)
  x <- rnorm(30)
  g <- rep(c(0, 1), each = 15)
  g[c(2, 20)] <- NA

  keep <- !is.na(g)
  expect_equal(
    bal_smd(x, g, na.rm = TRUE),
    bal_smd(x[keep], g[keep])
  )
})

test_that("bal_vr and bal_ks return NA for a missing exposure by default", {
  x <- as.numeric(1:6)
  g <- c(0, 0, 0, 1, 1, NA)

  expect_true(is.na(bal_smd(x, g)))
  expect_true(is.na(bal_vr(x, g)))
  expect_true(is.na(bal_ks(x, g)))

  expect_true(is.finite(bal_smd(x, g, na.rm = TRUE)))
  expect_true(is.finite(bal_vr(x, g, na.rm = TRUE)))
  expect_true(is.finite(bal_ks(x, g, na.rm = TRUE)))
})

test_that("binary bal_* functions return NA when a group has no weight", {
  set.seed(33)
  x <- rnorm(40)
  g <- rep(c(0, 1), each = 20)
  x_binary <- rep(c(0, 1, 1, 0), times = 10)

  w_zero_comparison <- ifelse(g == 1, 0, 1)
  w_zero_reference <- ifelse(g == 0, 0, 1)

  # `smd:::n_mean_var()` reports a mean and variance of 0 for a group with no
  # weight, so bal_smd has to catch this itself rather than trust the estimate
  expect_true(is.na(bal_smd(x, g, .weights = w_zero_comparison)))
  expect_true(is.na(bal_smd(x, g, .weights = w_zero_reference)))
  expect_true(is.na(bal_vr(x, g, .weights = w_zero_comparison)))
  expect_true(is.na(bal_vr(x, g, .weights = w_zero_reference)))
  expect_true(is.na(bal_ks(x, g, .weights = w_zero_comparison)))
  expect_true(is.na(bal_ks(x, g, .weights = w_zero_reference)))

  expect_true(is.na(bal_smd(x_binary, g, .weights = w_zero_comparison)))
  expect_true(is.na(bal_vr(x_binary, g, .weights = w_zero_comparison)))
  expect_true(is.na(bal_ks(x_binary, g, .weights = w_zero_comparison)))

  psw_zero <- propensity::psw(w_zero_comparison, estimand = "ate")
  expect_true(is.na(bal_smd(x, g, .weights = psw_zero)))
  expect_true(is.na(bal_vr(x, g, .weights = psw_zero)))
  expect_true(is.na(bal_ks(x, g, .weights = psw_zero)))

  # Rows dropped by na.rm can empty a group's weight just as zeros can
  w_na <- ifelse(g == 1, NA_real_, 1)
  expect_true(is.na(bal_smd(x, g, .weights = w_na, na.rm = TRUE)))
})

test_that("categorical bal_* functions return NA for a level with no weight", {
  set.seed(37)
  x <- rnorm(90)
  g <- rep(c("low", "medium", "high"), each = 30)
  w <- ifelse(g == "medium", 0, 1)

  # The levels sort, so "high" is the reference and only the "medium"
  # comparison is undefined
  result <- bal_smd(x, g, .weights = w)
  expect_true(is.na(result[["medium_vs_high"]]))
  expect_true(is.finite(result[["low_vs_high"]]))

  expect_true(is.na(bal_vr(x, g, .weights = w)[["medium_vs_high"]]))
  expect_true(is.na(bal_ks(x, g, .weights = w)[["medium_vs_high"]]))
})

test_that("bal_corr returns NA when the weights sum to zero", {
  set.seed(34)
  x <- rnorm(30)
  y <- rnorm(30)

  expect_true(is.na(bal_corr(x, y, .weights = rep(0, 30))))

  psw_zero <- propensity::psw(rep(0, 30), estimand = "ate")
  expect_true(is.na(bal_corr(x, y, .weights = psw_zero)))

  # Only the zero-weight rows survive the complete-case filter
  w <- c(rep(0, 20), rep(NA, 10))
  x[21:30] <- NA
  expect_true(is.na(bal_corr(x, y, .weights = w, na.rm = TRUE)))
})

test_that("binary bal_* functions use observed levels, not declared ones", {
  set.seed(35)
  x <- rnorm(40)
  f3 <- factor(rep(c("a", "b"), each = 20), levels = c("a", "b", "c"))

  expect_equal(bal_smd(x, f3), bal_smd(x, droplevels(f3)))
  expect_equal(bal_vr(x, f3), bal_vr(x, droplevels(f3)))
  expect_equal(bal_ks(x, f3), bal_ks(x, droplevels(f3)))

  expect_true(is.finite(bal_smd(x, f3)))
})

test_that("binary bal_* functions reject a single observed level", {
  set.seed(36)
  x <- rnorm(20)
  f1 <- factor(rep("a", 20), levels = c("a", "b"))

  expect_error(bal_smd(x, f1), class = "halfmoon_group_error")
  expect_error(bal_vr(x, f1), class = "halfmoon_group_error")
  expect_error(bal_ks(x, f1), class = "halfmoon_group_error")
})

test_that("bal_energy takes an exposure_type that forces either path", {
  set.seed(88)
  n <- 90
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  many_valued <- sample(1:12, n, replace = TRUE)
  few_valued <- sample(1:8, n, replace = TRUE)

  # "auto" keeps the documented rule: more than ten unique numeric values are
  # read as continuous, ten or fewer as categorical
  expect_equal(
    bal_energy(covariates, many_valued),
    bal_energy(covariates, many_valued, exposure_type = "continuous"),
    tolerance = 1e-10
  )
  expect_equal(
    bal_energy(covariates, few_valued),
    bal_energy(covariates, few_valued, exposure_type = "categorical"),
    tolerance = 1e-10
  )

  # Forcing the categorical path is the same as calling factor() on the exposure
  expect_equal(
    bal_energy(covariates, many_valued, exposure_type = "categorical"),
    bal_energy(covariates, factor(many_valued)),
    tolerance = 1e-10
  )

  # Forcing the continuous path reads the same numbers as a continuous exposure
  expect_false(isTRUE(all.equal(
    bal_energy(covariates, few_valued, exposure_type = "continuous"),
    bal_energy(covariates, few_valued)
  )))

  # "binary" and "categorical" name the same path
  expect_equal(
    bal_energy(covariates, few_valued, exposure_type = "binary"),
    bal_energy(covariates, few_valued, exposure_type = "categorical"),
    tolerance = 1e-10
  )

  # A factor holds no numbers to read as continuous
  expect_error(
    bal_energy(
      covariates,
      factor(few_valued),
      exposure_type = "continuous"
    ),
    class = "halfmoon_type_error"
  )

  expect_error(
    bal_energy(covariates, few_valued, exposure_type = "ordinal"),
    class = "halfmoon_arg_error"
  )
})

test_that("bal_energy dcor criterion matches cobalt for weighted samples", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  set.seed(4)
  n <- 60
  covariates <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  treatment <- rnorm(n)
  weights <- runif(n, 0.2, 4)

  # cobalt scales the covariates and the standardizing denominator by the same
  # weights it uses in the quadratic form when they are supplied as sampling
  # weights, which is what bal_energy() does with its balancing weights
  expect_equal(
    bal_energy(
      covariates,
      treatment,
      .weights = weights,
      criterion = "dcor"
    ),
    cobalt::bal.compute(
      cobalt::bal.init(
        covariates,
        treat = treatment,
        s.weights = weights,
        stat = "distance.cor"
      )
    ),
    tolerance = 1e-8
  )
})

test_that("bal_energy dcor criterion reports zero for a non-positive distance covariance", {
  set.seed(66)
  n <- 40
  treatment <- rnorm(n)

  # A covariate with no variation leaves an all-zero double-centered distance
  # matrix, so the weighted distance covariance is exactly zero. cobalt's
  # distance.cor reports zero for a non-positive distance covariance and so does
  # bal_energy(), rather than an NaN from the standardizing denominator.
  expect_identical(
    bal_energy(
      data.frame(x = rep(2, n)),
      treatment,
      criterion = "dcor"
    ),
    0
  )
  expect_identical(
    bal_energy(
      data.frame(x = rep(2, n)),
      treatment,
      criterion = "dcor",
      standardized = FALSE
    ),
    0
  )
})

test_that("bal_energy dependence criterion expands a factor covariate", {
  skip_if_not_installed("independenceWeights")
  skip_on_cran()

  set.seed(3131)
  n <- 120
  f <- factor(sample(c("a", "b", "c"), n, replace = TRUE))
  g <- factor(sample(c("no", "yes"), n, replace = TRUE))
  covariates <- data.frame(x1 = rnorm(n), f = f, g = g)
  treatment <- rnorm(n)
  weights <- runif(n, 0.2, 3)

  # A multi-level factor becomes one indicator per level and a two-level factor
  # becomes a single 0/1 indicator, with the numeric columns kept first
  expanded <- cbind(
    x1 = covariates$x1,
    fa = as.numeric(f == "a"),
    fb = as.numeric(f == "b"),
    fc = as.numeric(f == "c"),
    g = as.numeric(g) - 1
  )

  expect_equal(
    bal_energy(covariates, treatment, .weights = weights),
    independenceWeights::weighted_energy_stats(
      treatment,
      expanded,
      weights,
      dimension_adj = TRUE
    )$D_w,
    tolerance = 1e-8
  )
})

test_that("check_balance reports the weighted dependence distance for a continuous exposure", {
  skip_if_not_installed("independenceWeights")
  skip_on_cran()

  set.seed(2028)
  n <- 130
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    a = rnorm(n),
    w1 = runif(n, 0.2, 3),
    w2 = runif(n, 0.5, 2)
  )
  covariate_matrix <- as.matrix(df[c("x1", "x2")])

  result <- check_balance(
    df,
    c(x1, x2),
    a,
    .weights = c(w1, w2),
    .metrics = "energy"
  )

  for (method in c("w1", "w2")) {
    expect_equal(
      result$estimate[result$method == method],
      independenceWeights::weighted_energy_stats(
        df$a,
        covariate_matrix,
        df[[method]],
        dimension_adj = TRUE
      )$D_w,
      tolerance = 1e-8
    )
  }

  expect_equal(
    result$estimate[result$method == "observed"],
    independenceWeights::weighted_energy_stats(
      df$a,
      covariate_matrix,
      rep(1, n),
      dimension_adj = TRUE
    )$D_w,
    tolerance = 1e-8
  )
})

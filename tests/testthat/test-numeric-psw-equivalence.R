# Tests for numeric vs psw weight equivalence and backward compatibility

test_that("backward compatibility: all functions work with numeric weights", {
  # Create numeric weight vectors
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  g <- sample(c(0, 1), n, replace = TRUE)
  numeric_weights <- runif(n, 0.5, 2.0)

  # Test all balance functions with numeric weights
  expect_no_error(smd_result <- bal_smd(x, g, .weights = numeric_weights))
  expect_true(is.finite(smd_result))

  expect_no_error(vr_result <- bal_vr(x, g, .weights = numeric_weights))
  expect_true(is.finite(vr_result) && vr_result > 0)

  expect_no_error(ks_result <- bal_ks(x, g, .weights = numeric_weights))
  expect_true(is.finite(ks_result) && ks_result >= 0 && ks_result <= 1)

  # Test correlation with numeric weights
  y <- 2 * x + rnorm(n, sd = 0.5)
  expect_no_error(corr_result <- bal_corr(x, y, .weights = numeric_weights))
  expect_true(is.finite(corr_result) && corr_result >= -1 && corr_result <= 1)

  # Test energy balance with numeric weights
  covariates <- data.frame(x1 = x, x2 = rnorm(n))
  expect_no_error(
    energy_result <- bal_energy(covariates, g, .weights = numeric_weights)
  )
  expect_true(is.finite(energy_result) && energy_result >= 0)

  # Test check_balance with numeric weights
  test_data <- data.frame(
    x = x,
    y = y,
    g = g,
    w = numeric_weights
  )
  expect_no_error(
    balance_results <- check_balance(
      test_data,
      c(x, y),
      g,
      .weights = w,
      .metrics = "smd"
    )
  )
  expect_s3_class(balance_results, "data.frame")
  expect_true(all(is.finite(balance_results$estimate)))
})

test_that("numeric and psw weights produce identical results", {
  skip_if_not_installed("propensity")

  # Create test data
  set.seed(42)
  n <- 100
  x <- rnorm(n)
  g <- sample(c(0, 1), n, replace = TRUE)

  # Create numeric weights
  numeric_weights <- runif(n, 0.5, 2.0)

  # Create equivalent psw objects
  psw_ate <- propensity::psw(numeric_weights, estimand = "ate")
  psw_att <- propensity::psw(numeric_weights, estimand = "att")
  psw_ato <- propensity::psw(numeric_weights, estimand = "ato")

  # Test bal_smd equivalence
  smd_numeric <- bal_smd(x, g, .weights = numeric_weights)
  smd_psw_ate <- bal_smd(x, g, .weights = psw_ate)
  smd_psw_att <- bal_smd(x, g, .weights = psw_att)
  smd_psw_ato <- bal_smd(x, g, .weights = psw_ato)

  expect_identical(smd_numeric, smd_psw_ate)
  expect_identical(smd_numeric, smd_psw_att)
  expect_identical(smd_numeric, smd_psw_ato)

  # Test bal_vr equivalence
  vr_numeric <- bal_vr(x, g, .weights = numeric_weights)
  vr_psw_ate <- bal_vr(x, g, .weights = psw_ate)
  vr_psw_att <- bal_vr(x, g, .weights = psw_att)
  vr_psw_ato <- bal_vr(x, g, .weights = psw_ato)

  expect_identical(vr_numeric, vr_psw_ate)
  expect_identical(vr_numeric, vr_psw_att)
  expect_identical(vr_numeric, vr_psw_ato)

  # Test bal_ks equivalence
  ks_numeric <- bal_ks(x, g, .weights = numeric_weights)
  ks_psw_ate <- bal_ks(x, g, .weights = psw_ate)
  ks_psw_att <- bal_ks(x, g, .weights = psw_att)
  ks_psw_ato <- bal_ks(x, g, .weights = psw_ato)

  expect_identical(ks_numeric, ks_psw_ate)
  expect_identical(ks_numeric, ks_psw_att)
  expect_identical(ks_numeric, ks_psw_ato)
})

test_that("numeric and psw weights produce identical results for correlation", {
  skip_if_not_installed("propensity")

  # Create test data
  set.seed(42)
  n <- 100
  x <- rnorm(n)
  y <- 2 * x + rnorm(n, sd = 0.5)

  # Create numeric weights
  numeric_weights <- runif(n, 0.5, 2.0)

  # Create equivalent psw object
  psw_weights <- propensity::psw(numeric_weights, estimand = "ate")

  # Test bal_corr equivalence
  corr_numeric <- bal_corr(x, y, .weights = numeric_weights)
  corr_psw <- bal_corr(x, y, .weights = psw_weights)

  expect_identical(corr_numeric, corr_psw)
})

test_that("numeric and psw weights produce identical results for energy balance", {
  skip_if_not_installed("propensity")

  # Create test data
  set.seed(42)
  n <- 100
  covariates <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  g <- sample(c(0, 1), n, replace = TRUE)

  # Create numeric weights
  numeric_weights <- runif(n, 0.5, 2.0)

  # Create equivalent psw object
  psw_weights <- propensity::psw(numeric_weights, estimand = "ate")

  # Test bal_energy equivalence
  energy_numeric <- bal_energy(covariates, g, .weights = numeric_weights)
  energy_psw <- bal_energy(covariates, g, .weights = psw_weights)

  expect_identical(energy_numeric, energy_psw)
})

test_that("check_balance works equivalently with numeric and psw weights", {
  skip_if_not_installed("propensity")

  # Use nhefs_weights dataset
  data(nhefs_weights)

  # Extract numeric data from existing psw weights for comparison
  w_ate_numeric <- vctrs::vec_data(nhefs_weights$w_ate)
  w_att_numeric <- vctrs::vec_data(nhefs_weights$w_att)

  # Create new psw objects from the same numeric data
  w_ate_psw <- propensity::psw(w_ate_numeric, estimand = "ate")
  w_att_psw <- propensity::psw(w_att_numeric, estimand = "att")

  # Add numeric weights to test data
  test_data <- nhefs_weights[1:100, ]
  test_data$w_ate_numeric <- w_ate_numeric[1:100]
  test_data$w_att_numeric <- w_att_numeric[1:100]
  test_data$w_ate_psw <- w_ate_psw[1:100]
  test_data$w_att_psw <- w_att_psw[1:100]

  # Test single weight comparison
  result_numeric <- check_balance(
    test_data,
    c(age, wt71),
    qsmk,
    .weights = w_ate_numeric,
    .metrics = "smd",
    include_observed = FALSE
  )

  result_psw <- check_balance(
    test_data,
    c(age, wt71),
    qsmk,
    .weights = w_ate_psw,
    .metrics = "smd",
    include_observed = FALSE
  )

  # Results should be identical except for method names
  expect_equal(result_numeric$estimate, result_psw$estimate)
  expect_equal(result_numeric$variable, result_psw$variable)
  # Method names will differ (w_ate_numeric vs w_ate_psw) but that's expected
})

test_that("mixed numeric and psw weights work in same function call", {
  skip_if_not_installed("propensity")

  # Use nhefs_weights dataset with mixed weight types
  data(nhefs_weights)
  test_data <- nhefs_weights[1:100, ]

  # Create a numeric version of one weight
  test_data$w_ate_numeric <- vctrs::vec_data(test_data$w_ate)

  # This should work without error - one numeric, one psw
  result <- check_balance(
    test_data,
    c(age, wt71),
    qsmk,
    .weights = c(w_ate_numeric, w_att), # Mix numeric and psw
    .metrics = "smd",
    include_observed = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(is.finite(result$estimate)))
  expect_true("w_ate_numeric" %in% result$method)
  expect_true("w_att" %in% result$method)
})

test_that("edge cases work identically for numeric and psw weights", {
  skip_if_not_installed("propensity")

  # Test with extreme weights
  set.seed(42)
  n <- 50
  x <- rnorm(n)
  g <- sample(c(0, 1), n, replace = TRUE)

  # Extreme numeric weights
  extreme_numeric <- c(rep(0.001, n / 2), rep(100, n / 2))
  extreme_psw <- propensity::psw(extreme_numeric, estimand = "ate")

  # Both should handle extreme weights the same way
  smd_numeric <- bal_smd(x, g, .weights = extreme_numeric)
  smd_psw <- bal_smd(x, g, .weights = extreme_psw)
  expect_identical(smd_numeric, smd_psw)

  # Test with uniform weights (should be close to unweighted)
  uniform_numeric <- rep(1, n)
  uniform_psw <- propensity::psw(uniform_numeric, estimand = "ate")

  smd_unweighted <- bal_smd(x, g)
  smd_uniform_numeric <- bal_smd(x, g, .weights = uniform_numeric)
  smd_uniform_psw <- bal_smd(x, g, .weights = uniform_psw)

  expect_identical(smd_uniform_numeric, smd_uniform_psw)
  expect_equal(smd_unweighted, smd_uniform_numeric, tolerance = 1e-10)
})

test_that("NA handling is identical for numeric and psw weights", {
  skip_if_not_installed("propensity")

  # Create test data with NAs
  set.seed(42)
  n <- 100
  x <- rnorm(n)
  x[1:5] <- NA # Add some NAs
  g <- sample(c(0, 1), n, replace = TRUE)

  # Create weights
  numeric_weights <- runif(n, 0.5, 2.0)
  psw_weights <- propensity::psw(numeric_weights, estimand = "ate")

  # Test na.rm = FALSE (should return NA)
  smd_numeric_na <- bal_smd(x, g, .weights = numeric_weights, na.rm = FALSE)
  smd_psw_na <- bal_smd(x, g, .weights = psw_weights, na.rm = FALSE)

  expect_identical(smd_numeric_na, smd_psw_na)
  expect_true(is.na(smd_numeric_na))

  # Test na.rm = TRUE
  smd_numeric_narm <- bal_smd(x, g, .weights = numeric_weights, na.rm = TRUE)
  smd_psw_narm <- bal_smd(x, g, .weights = psw_weights, na.rm = TRUE)

  expect_identical(smd_numeric_narm, smd_psw_narm)
  expect_true(is.finite(smd_numeric_narm))
})

test_that("all helper functions work with both numeric and psw weights", {
  skip_if_not_installed("propensity")

  # Test weighted_quantile
  set.seed(42)
  n <- 100
  x <- rnorm(n)

  numeric_weights <- runif(n, 0.5, 2.0)
  psw_weights <- propensity::psw(numeric_weights, estimand = "ate")

  q_numeric <- weighted_quantile(x, c(0.25, 0.5, 0.75), numeric_weights)
  q_psw <- weighted_quantile(x, c(0.25, 0.5, 0.75), psw_weights)

  expect_identical(q_numeric, q_psw)

  # Test ess function
  ess_numeric <- ess(numeric_weights)
  ess_psw <- ess(psw_weights)

  expect_identical(ess_numeric, ess_psw)
})

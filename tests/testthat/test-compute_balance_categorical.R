# Tests for categorical exposure balance functions

# Create test data with categorical exposure
create_test_data_categorical <- function(n = 300, seed = 123) {
  set.seed(seed)

  # Create a categorical exposure with 3 levels
  exposure <- sample(
    c("low", "medium", "high"),
    n,
    replace = TRUE,
    prob = c(0.3, 0.4, 0.3)
  )

  # Create covariates that vary by exposure
  age <- rnorm(
    n,
    mean = ifelse(exposure == "low", 45, ifelse(exposure == "medium", 50, 55)),
    sd = 10
  )

  income <- rnorm(
    n,
    mean = ifelse(
      exposure == "low",
      30000,
      ifelse(exposure == "medium", 45000, 60000)
    ),
    sd = 10000
  )

  # Binary covariate
  employed <- rbinom(
    n,
    1,
    prob = ifelse(
      exposure == "low",
      0.6,
      ifelse(exposure == "medium", 0.8, 0.9)
    )
  )

  # Create some weights
  ps_low <- 0.3
  ps_medium <- 0.4
  ps_high <- 0.3

  # Simple ATT-like weights (treat "high" as treatment)
  weights_att <- ifelse(
    exposure == "high",
    1,
    ifelse(exposure == "low", ps_high / ps_low, ps_high / ps_medium)
  )

  data.frame(
    exposure = exposure,
    age = age,
    income = income,
    employed = employed,
    weights_att = weights_att,
    stringsAsFactors = FALSE
  )
}

# Test bal_smd with categorical exposure
test_that("bal_smd works with categorical exposures", {
  data <- create_test_data_categorical()

  # Test basic functionality
  result <- bal_smd(data$age, data$exposure)

  expect_type(result, "double")
  expect_length(result, 2) # 3 levels - 1 reference = 2 comparisons
  expect_true(!is.null(names(result)))
  expect_true(all(!is.na(result)))

  # Check naming convention
  expect_true(all(grepl("_vs_", names(result))))

  # Test with explicit reference group
  result_ref <- bal_smd(data$age, data$exposure, reference_group = "low")
  expect_true(all(grepl("_vs_low$", names(result_ref))))
  expect_length(result_ref, 2) # medium_vs_low and high_vs_low

  # Test with weights
  result_weighted <- bal_smd(
    data$age,
    data$exposure,
    weights = data$weights_att
  )
  expect_length(result_weighted, 2)
  expect_true(all(!is.na(result_weighted)))

  # Check that weights are being used (results should differ from unweighted)
  # Using a less strict test since the effect might be small
  expect_false(identical(result, result_weighted))
})

test_that("bal_vr works with categorical exposures", {
  data <- create_test_data_categorical()

  # Test basic functionality
  result <- bal_vr(data$age, data$exposure)

  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(!is.null(names(result)))
  expect_true(all(!is.na(result)))
  expect_true(all(result > 0)) # Variance ratios should be positive

  # Test with binary covariate
  result_binary <- bal_vr(data$employed, data$exposure)
  expect_length(result_binary, 2)
  expect_true(all(!is.na(result_binary)))
  expect_true(all(result_binary > 0))
})

test_that("bal_ks works with categorical exposures", {
  data <- create_test_data_categorical()

  # Test basic functionality
  result <- bal_ks(data$age, data$exposure)

  expect_type(result, "double")
  expect_length(result, 2)
  expect_true(!is.null(names(result)))
  expect_true(all(!is.na(result)))
  expect_true(all(result >= 0 & result <= 1)) # KS statistic bounded [0,1]
})

test_that("check_balance integrates categorical exposures correctly", {
  data <- create_test_data_categorical()

  # Test with single metric
  result <- check_balance(data, age, exposure, .metrics = "smd")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2) # 2 comparisons for categorical
  expect_equal(unique(result$metric), "smd")
  expect_true(all(
    c("variable", "group_level", "method", "metric", "estimate") %in%
      names(result)
  ))

  # Test with multiple metrics
  result_multi <- check_balance(
    data,
    c(age, income),
    exposure,
    .metrics = c("smd", "vr", "ks")
  )

  expect_equal(nrow(result_multi), 2 * 3 * 2) # 2 vars × 3 metrics × 2 comparisons
  expect_equal(sort(unique(result_multi$metric)), c("ks", "smd", "vr"))

  # Test with weights
  result_weighted <- check_balance(
    data,
    age,
    exposure,
    .wts = weights_att,
    .metrics = "smd"
  )

  expect_equal(nrow(result_weighted), 4) # 2 comparisons × 2 methods (observed + weighted)
  expect_true(all(c("observed", "weights_att") %in% result_weighted$method))
})

test_that("categorical exposure validation works correctly", {
  data <- create_test_data_categorical()

  # Test is_categorical_exposure
  expect_true(is_categorical_exposure(data$exposure))
  expect_false(is_categorical_exposure(c(0, 1, 1, 0))) # Binary
  expect_false(is_categorical_exposure(c(1, 1, 1, 1))) # Single level

  # Test get_exposure_type
  expect_equal(get_exposure_type(data$exposure), "categorical")
  expect_equal(get_exposure_type(c(0, 1, 1, 0)), "binary")
  expect_error(get_exposure_type(c(1, 1, 1, 1)), "only one level")
})

test_that("categorical balance handles missing values correctly", {
  data <- create_test_data_categorical()

  # Add some missing values
  data$age[c(1, 5, 10)] <- NA
  data$exposure[c(2, 6)] <- NA

  # Test na.rm = FALSE (default)
  result_na <- bal_smd(data$age, data$exposure, na.rm = FALSE)
  expect_true(all(is.na(result_na)))

  # Test na.rm = TRUE
  result_no_na <- bal_smd(data$age, data$exposure, na.rm = TRUE)
  expect_false(any(is.na(result_no_na)))
  expect_length(result_no_na, 2)
})

test_that("categorical balance handles factor exposures", {
  data <- create_test_data_categorical()

  # Convert to factor
  data$exposure_factor <- factor(
    data$exposure,
    levels = c("low", "medium", "high")
  )

  # Use explicit reference group to ensure consistency
  result_char <- bal_smd(data$age, data$exposure, reference_group = "low")
  result_factor <- bal_smd(
    data$age,
    data$exposure_factor,
    reference_group = "low"
  )

  # Results should be identical when using same reference
  expect_equal(sort(names(result_char)), sort(names(result_factor)))
  expect_equal(
    result_char[sort(names(result_char))],
    result_factor[sort(names(result_factor))],
    tolerance = 1e-10
  )
})

test_that("reference group specification works correctly", {
  data <- create_test_data_categorical()

  # Test different ways of specifying reference group
  result_default <- bal_smd(data$age, data$exposure)
  result_low <- bal_smd(data$age, data$exposure, reference_group = "low")
  result_medium <- bal_smd(data$age, data$exposure, reference_group = "medium")
  result_high <- bal_smd(data$age, data$exposure, reference_group = "high")

  # Each should have 2 comparisons
  expect_length(result_default, 2)
  expect_length(result_low, 2)
  expect_length(result_medium, 2)
  expect_length(result_high, 2)

  # Check that reference groups are correct
  expect_true(all(grepl("_vs_low$", names(result_low))))
  expect_true(all(grepl("_vs_medium$", names(result_medium))))
  expect_true(all(grepl("_vs_high$", names(result_high))))

  # Test numeric reference group (by position)
  levels_sorted <- sort(unique(data$exposure))
  result_numeric <- bal_smd(data$age, data$exposure, reference_group = 1)
  expect_true(all(grepl(
    paste0("_vs_", levels_sorted[1], "$"),
    names(result_numeric)
  )))
})

test_that("categorical balance works with ordered factors", {
  data <- create_test_data_categorical()

  # Create ordered factor
  data$exposure_ordered <- ordered(
    data$exposure,
    levels = c("low", "medium", "high")
  )

  result <- bal_smd(data$age, data$exposure_ordered)
  expect_length(result, 2)
  expect_true(all(!is.na(result)))

  # Should maintain order in results
  expect_true(
    "medium_vs_low" %in% names(result) || "low_vs_medium" %in% names(result)
  )
})

# =============================================================================
# COBALT COMPARISON TESTS FOR CATEGORICAL EXPOSURES
# =============================================================================
#
# IMPORTANT: For continuous covariates, halfmoon and cobalt use different 
# variance calculations:
# - halfmoon (via smd package): uses population variance (dividing by n)
# - cobalt: uses sample variance with Bessel's correction (dividing by n-1)
# This results in a ~0.5-1% difference in SMD values for continuous covariates.
#
# For binary covariates with bin.vars=TRUE, both packages use the formula
# p(1-p) for variance, resulting in exact matches.
#
# =============================================================================

# Helper function to create binary indicators matching halfmoon's internal logic
create_binary_for_cobalt <- function(exposure, comp_level, ref_level) {
  binary <- rep(NA, length(exposure))
  binary[exposure == comp_level] <- 1
  binary[exposure == ref_level] <- 0
  binary
}

test_that("bal_smd categorical matches cobalt for 3-level exposure with binary covariate", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 456)

  # Halfmoon - using binary covariate for exact match
  hm_result <- bal_smd(
    covariate = data$employed,
    group = data$exposure,
    reference_group = "low"
  )

  # Cobalt - separate calculations matching halfmoon's internal logic
  # Medium vs Low: halfmoon codes as medium=1, low=0
  medium_low_indices <- data$exposure %in% c("medium", "low")
  # Create binary group exactly as halfmoon does internally
  medium_low_binary <- rep(NA, length(data$exposure))
  medium_low_binary[data$exposure == "medium"] <- 1
  medium_low_binary[data$exposure == "low"] <- 0
  
  cobalt_medium_vs_low <- cobalt::col_w_smd(
    matrix(data$employed[medium_low_indices], ncol = 1),
    treat = medium_low_binary[medium_low_indices],
    std = TRUE,
    bin.vars = TRUE
  )[1]

  # High vs Low: halfmoon codes as high=1, low=0
  high_low_indices <- data$exposure %in% c("high", "low")
  high_low_binary <- rep(NA, length(data$exposure))
  high_low_binary[data$exposure == "high"] <- 1
  high_low_binary[data$exposure == "low"] <- 0
  
  cobalt_high_vs_low <- cobalt::col_w_smd(
    matrix(data$employed[high_low_indices], ncol = 1),
    treat = high_low_binary[high_low_indices],
    std = TRUE,
    bin.vars = TRUE
  )[1]

  # Should match exactly for binary covariates
  expect_equal(
    abs(unname(hm_result["medium_vs_low"])),
    abs(unname(cobalt_medium_vs_low)),
    tolerance = 1e-10
  )
  expect_equal(
    abs(unname(hm_result["high_vs_low"])),
    abs(unname(cobalt_high_vs_low)),
    tolerance = 1e-10
  )
})

test_that("bal_smd categorical with continuous covariate shows expected variance difference", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 456)

  # Halfmoon - returns named vector with both comparisons
  hm_result <- bal_smd(
    covariate = data$age,
    group = data$exposure,
    reference_group = "low"
  )

  # Cobalt - separate calculations
  medium_low_indices <- data$exposure %in% c("medium", "low")
  medium_low_binary <- create_binary_for_cobalt(data$exposure, "medium", "low")
  
  cobalt_medium_vs_low <- cobalt::col_w_smd(
    matrix(data$age[medium_low_indices], ncol = 1),
    treat = medium_low_binary[medium_low_indices],
    std = TRUE
  )[1]

  # For continuous covariates, expect ~0.5-1% difference due to variance calculation
  # halfmoon uses population variance, cobalt uses sample variance
  expect_equal(
    abs(unname(hm_result["medium_vs_low"])),
    abs(unname(cobalt_medium_vs_low)),
    tolerance = 0.01  # 1% tolerance for variance difference
  )
})

test_that("bal_smd categorical matches cobalt with weights and binary covariate", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 400, seed = 789)

  # Halfmoon with weights - using binary covariate for exact match
  hm_result <- bal_smd(
    covariate = data$employed,
    group = data$exposure,
    weights = data$weights_att,
    reference_group = "high"
  )

  # Cobalt - separate calculations
  # Low vs High: halfmoon codes as low=1, high=0 (high is reference)
  low_high_indices <- data$exposure %in% c("low", "high")
  low_high_binary <- create_binary_for_cobalt(data$exposure, "low", "high")
  cobalt_low_vs_high <- cobalt::col_w_smd(
    matrix(data$employed[low_high_indices], ncol = 1),
    treat = low_high_binary[low_high_indices],
    weights = data$weights_att[low_high_indices],
    std = TRUE,
    bin.vars = TRUE
  )[1]

  # Medium vs High: halfmoon codes as medium=1, high=0 (high is reference)
  medium_high_indices <- data$exposure %in% c("medium", "high")
  medium_high_binary <- create_binary_for_cobalt(data$exposure, "medium", "high")
  cobalt_medium_vs_high <- cobalt::col_w_smd(
    matrix(data$employed[medium_high_indices], ncol = 1),
    treat = medium_high_binary[medium_high_indices],
    weights = data$weights_att[medium_high_indices],
    std = TRUE,
    bin.vars = TRUE
  )[1]

  # Should match exactly for binary covariates
  expect_equal(
    abs(unname(hm_result["low_vs_high"])),
    abs(unname(cobalt_low_vs_high)),
    tolerance = 1e-10
  )
  expect_equal(
    abs(unname(hm_result["medium_vs_high"])),
    abs(unname(cobalt_medium_vs_high)),
    tolerance = 1e-10
  )
})

test_that("bal_vr categorical matches cobalt for 3-level exposure", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 123)

  # Halfmoon - returns named vector
  hm_result <- bal_vr(
    covariate = data$age,
    group = data$exposure,
    reference_group = "low"
  )

  # Cobalt - separate calculations
  # Medium vs Low
  medium_low_indices <- data$exposure %in% c("medium", "low")
  medium_low_binary <- create_binary_for_cobalt(data$exposure, "medium", "low")
  cobalt_medium_vs_low <- cobalt::col_w_vr(
    matrix(data$age[medium_low_indices], ncol = 1),
    treat = medium_low_binary[medium_low_indices]
  )[1]

  # High vs Low
  high_low_indices <- data$exposure %in% c("high", "low")
  high_low_binary <- create_binary_for_cobalt(data$exposure, "high", "low")
  cobalt_high_vs_low <- cobalt::col_w_vr(
    matrix(data$age[high_low_indices], ncol = 1),
    treat = high_low_binary[high_low_indices]
  )[1]

  # Compare results
  expect_equal(
    unname(hm_result["medium_vs_low"]),
    unname(cobalt_medium_vs_low),
    tolerance = 1e-10
  )
  expect_equal(
    unname(hm_result["high_vs_low"]),
    unname(cobalt_high_vs_low),
    tolerance = 1e-10
  )
})

test_that("bal_vr categorical matches cobalt with binary covariate", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 456)

  # Halfmoon with binary covariate
  hm_result <- bal_vr(
    covariate = data$employed,
    group = data$exposure,
    reference_group = "medium"
  )

  # Cobalt - separate calculations with bin.vars = TRUE
  # Low vs Medium
  low_medium_indices <- data$exposure %in% c("low", "medium")
  low_medium_binary <- create_binary_for_cobalt(data$exposure, "low", "medium")
  cobalt_low_vs_medium <- cobalt::col_w_vr(
    matrix(data$employed[low_medium_indices], ncol = 1),
    treat = low_medium_binary[low_medium_indices],
    bin.vars = TRUE
  )[1]

  # High vs Medium
  high_medium_indices <- data$exposure %in% c("high", "medium")
  high_medium_binary <- create_binary_for_cobalt(data$exposure, "high", "medium")
  cobalt_high_vs_medium <- cobalt::col_w_vr(
    matrix(data$employed[high_medium_indices], ncol = 1),
    treat = high_medium_binary[high_medium_indices],
    bin.vars = TRUE
  )[1]

  # Compare results
  expect_equal(
    unname(hm_result["low_vs_medium"]),
    unname(cobalt_low_vs_medium),
    tolerance = 1e-10
  )
  expect_equal(
    unname(hm_result["high_vs_medium"]),
    unname(cobalt_high_vs_medium),
    tolerance = 1e-10
  )
})

test_that("bal_ks categorical matches cobalt for 3-level exposure", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 789)

  # Halfmoon - returns named vector
  hm_result <- bal_ks(
    covariate = data$income,
    group = data$exposure,
    reference_group = "low"
  )

  # Cobalt - separate calculations
  # Medium vs Low
  medium_low_indices <- data$exposure %in% c("medium", "low")
  medium_low_binary <- create_binary_for_cobalt(data$exposure, "medium", "low")
  cobalt_medium_vs_low <- cobalt::col_w_ks(
    matrix(data$income[medium_low_indices], ncol = 1),
    treat = medium_low_binary[medium_low_indices]
  )[1]

  # High vs Low
  high_low_indices <- data$exposure %in% c("high", "low")
  high_low_binary <- create_binary_for_cobalt(data$exposure, "high", "low")
  cobalt_high_vs_low <- cobalt::col_w_ks(
    matrix(data$income[high_low_indices], ncol = 1),
    treat = high_low_binary[high_low_indices]
  )[1]

  # Compare results
  expect_equal(
    unname(hm_result["medium_vs_low"]),
    unname(cobalt_medium_vs_low),
    tolerance = 1e-10
  )
  expect_equal(
    unname(hm_result["high_vs_low"]),
    unname(cobalt_high_vs_low),
    tolerance = 1e-10
  )
})

test_that("bal_ks categorical matches cobalt with weights and binary covariate", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 400, seed = 321)

  # Halfmoon with weights and binary covariate
  hm_result <- bal_ks(
    covariate = data$employed,
    group = data$exposure,
    weights = data$weights_att,
    reference_group = "high"
  )

  # Cobalt - separate calculations
  # Low vs High
  low_high_indices <- data$exposure %in% c("low", "high")
  low_high_binary <- create_binary_for_cobalt(data$exposure, "low", "high")
  cobalt_low_vs_high <- cobalt::col_w_ks(
    matrix(data$employed[low_high_indices], ncol = 1),
    treat = low_high_binary[low_high_indices],
    weights = data$weights_att[low_high_indices],
    bin.vars = TRUE
  )[1]

  # Medium vs High
  medium_high_indices <- data$exposure %in% c("medium", "high")
  medium_high_binary <- create_binary_for_cobalt(data$exposure, "medium", "high")
  cobalt_medium_vs_high <- cobalt::col_w_ks(
    matrix(data$employed[medium_high_indices], ncol = 1),
    treat = medium_high_binary[medium_high_indices],
    weights = data$weights_att[medium_high_indices],
    bin.vars = TRUE
  )[1]

  # Compare results
  expect_equal(
    unname(hm_result["low_vs_high"]),
    unname(cobalt_low_vs_high),
    tolerance = 1e-10
  )
  expect_equal(
    unname(hm_result["medium_vs_high"]),
    unname(cobalt_medium_vs_high),
    tolerance = 1e-10
  )
})

test_that("bal_energy categorical matches cobalt single overall statistic", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 987)

  # Create covariates matrix
  covariates <- data.frame(
    age = data$age,
    income = data$income,
    employed = data$employed
  )

  # Halfmoon - single overall statistic
  hm_result <- bal_energy(
    covariates = covariates,
    group = data$exposure
  )

  # Cobalt - also single overall statistic
  cobalt_result <- cobalt::bal.compute(
    x = covariates,
    treat = data$exposure,
    stat = "energy.dist"
  )

  # Both should be single values
  expect_length(hm_result, 1)
  expect_length(cobalt_result, 1)

  # Should be very close
  expect_equal(hm_result, cobalt_result, tolerance = 1e-4)
})

test_that("bal_energy categorical with weights matches cobalt", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 400, seed = 654)

  # Create covariates matrix
  covariates <- data.frame(
    age = data$age,
    income = data$income
  )

  # Halfmoon with weights
  hm_result <- bal_energy(
    covariates = covariates,
    group = data$exposure,
    weights = data$weights_att
  )

  # Cobalt with weights
  cobalt_result <- cobalt::bal.compute(
    x = covariates,
    treat = data$exposure,
    weights = data$weights_att,
    stat = "energy.dist"
  )

  # Should be very close
  expect_equal(hm_result, cobalt_result, tolerance = 1e-4)
})

test_that("categorical balance with NA values matches cobalt behavior", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  data <- create_test_data_categorical(n = 300, seed = 111)

  # Introduce missing values in binary covariate
  employed_na <- data$employed
  employed_na[sample(length(employed_na), 20)] <- NA

  # Test SMD with na.rm = TRUE
  hm_smd <- bal_smd(
    covariate = employed_na,
    group = data$exposure,
    reference_group = "low",
    na.rm = TRUE
  )

  # Cobalt equivalent - Medium vs Low
  medium_low_indices <- data$exposure %in% c("medium", "low")
  medium_low_binary <- create_binary_for_cobalt(data$exposure, "medium", "low")
  cobalt_medium_vs_low <- cobalt::col_w_smd(
    matrix(employed_na[medium_low_indices], ncol = 1),
    treat = medium_low_binary[medium_low_indices],
    na.rm = TRUE,
    std = TRUE,
    bin.vars = TRUE
  )[1]

  expect_equal(
    abs(unname(hm_smd["medium_vs_low"])),
    abs(unname(cobalt_medium_vs_low)),
    tolerance = 1e-10
  )
})

test_that("categorical balance with 4-level exposure matches cobalt pattern", {
  skip_if_not_installed("cobalt")
  skip_on_cran()

  # Create 4-level exposure data
  set.seed(222)
  n <- 300
  exposure_4 <- sample(c("A", "B", "C", "D"), n, replace = TRUE)
  # Binary covariate with different probabilities per group
  covariate_binary <- rbinom(n, 1, prob = ifelse(exposure_4 == "A", 0.3,
    ifelse(exposure_4 == "B", 0.5,
      ifelse(exposure_4 == "C", 0.7, 0.9)
    )
  ))

  # Halfmoon - should return 3 comparisons vs reference
  hm_result <- bal_smd(
    covariate = covariate_binary,
    group = exposure_4,
    reference_group = "A"
  )

  # Should have 3 comparisons: B_vs_A, C_vs_A, D_vs_A
  expect_length(hm_result, 3)
  expect_setequal(
    names(hm_result),
    c("B_vs_A", "C_vs_A", "D_vs_A")
  )

  # Verify one comparison matches cobalt exactly
  b_a_indices <- exposure_4 %in% c("B", "A")
  b_a_binary <- create_binary_for_cobalt(exposure_4, "B", "A")
  cobalt_b_vs_a <- cobalt::col_w_smd(
    matrix(covariate_binary[b_a_indices], ncol = 1),
    treat = b_a_binary[b_a_indices],
    std = TRUE,
    bin.vars = TRUE
  )[1]

  expect_equal(
    abs(unname(hm_result["B_vs_A"])),
    abs(unname(cobalt_b_vs_a)),
    tolerance = 1e-10
  )
})

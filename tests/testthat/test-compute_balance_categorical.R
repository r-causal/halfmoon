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

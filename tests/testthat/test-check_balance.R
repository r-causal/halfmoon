# Comprehensive tests for check_balance() using NHEFS data

# Test data setup using nhefs_weights from halfmoon package
get_nhefs_test_data <- function() {
  data(nhefs_weights, package = "halfmoon")

  # Create a subset with relevant variables for testing
  # Use factor variables and continuous variables
  nhefs_test <- nhefs_weights[
    1:200,
    c(
      "qsmk",
      "sex",
      "race",
      "education",
      "age",
      "wt71",
      "smokeintensity",
      "smokeyrs",
      "active",
      "exercise",
      "w_ate",
      "w_att",
      "w_atc"
    )
  ]

  # Add some additional test weights for comprehensive testing
  set.seed(123)
  nhefs_test$w_test1 <- runif(nrow(nhefs_test), 0.5, 2.0)
  nhefs_test$w_test2 <- rexp(nrow(nhefs_test), rate = 1)
  nhefs_test$w_extreme <- rep(c(0.1, 3.0), length.out = nrow(nhefs_test))

  return(nhefs_test)
}

# Helper function for backward compatibility in tests (uses old defaults)
check_balance_basic <- function(...) {
  check_balance(
    ...,
    make_dummy_vars = FALSE,
    squares = FALSE,
    cubes = FALSE,
    interactions = FALSE
  )
}

# =============================================================================
# BASIC FUNCTIONALITY TESTS
# =============================================================================

test_that("check_balance works with single variable and single metric", {
  data <- get_nhefs_test_data()

  # Test with SMD only
  result <- check_balance_basic(data, age, qsmk, .metrics = "smd")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1) # just age with basic function
  expect_equal(ncol(result), 5)
  expect_equal(
    names(result),
    c("variable", "group_level", "method", "metric", "estimate")
  )

  expect_equal(result$variable, "age")
  expect_equal(result$metric, "smd")
  expect_equal(result$method, "observed")
  expect_equal(result$group_level, "0") # qsmk is 0/1, reference = 1L -> first level = 0, but check_balance shows comparison group
  expect_true(is.numeric(result$estimate))
  expect_true(is.finite(result$estimate))
})

test_that("check_balance works with multiple metrics", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(
    data,
    age,
    qsmk,
    .metrics = c("smd", "variance_ratio", "ks")
  )

  expect_equal(nrow(result), 3) # 1 variable × 3 metrics
  expect_equal(sort(unique(result$metric)), c("ks", "smd", "variance_ratio"))
  expect_true(all(result$variable == "age"))
  expect_true(all(result$method == "observed"))
  expect_true(all(is.finite(result$estimate)))
})

test_that("check_balance works with multiple variables", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(data, c(age, wt71), qsmk, .metrics = "smd")

  expect_equal(nrow(result), 2) # 2 variables × 1 metric
  expect_equal(sort(unique(result$variable)), c("age", "wt71"))
  expect_true(all(result$metric == "smd"))
  expect_true(all(is.finite(result$estimate)))
})

# =============================================================================
# WEIGHTING TESTS
# =============================================================================

test_that("check_balance works with single weight", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_test1,
    .metrics = "smd"
  )

  expect_equal(nrow(result), 2) # observed + 1 weight
  expect_equal(sort(unique(result$method)), c("observed", "w_test1"))
  expect_true(all(result$variable == "age"))
  expect_true(all(result$metric == "smd"))
  expect_true(all(is.finite(result$estimate)))
})

test_that("check_balance works with multiple weights", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = c(w_test1, w_test2),
    .metrics = "smd"
  )

  expect_equal(nrow(result), 3) # observed + 2 weights
  expect_equal(sort(unique(result$method)), c("observed", "w_test1", "w_test2"))
  expect_true(all(result$variable == "age"))
  expect_true(all(result$metric == "smd"))
  expect_true(all(is.finite(result$estimate)))
})

test_that("check_balance respects include_observed = FALSE", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_test1,
    .metrics = "smd",
    include_observed = FALSE
  )

  expect_equal(nrow(result), 1) # only weighted
  expect_equal(result$method, "w_test1")
  expect_false("observed" %in% result$method)
})

# =============================================================================
# COMPREHENSIVE INTEGRATION TESTS
# =============================================================================

test_that("check_balance produces expected structure with multiple vars, weights, and metrics", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(
    data,
    c(age, wt71),
    qsmk,
    .wts = c(w_test1, w_test2),
    .metrics = c("smd", "ks")
  )

  # Should have: 2 variables × 3 methods (observed + 2 weights) × 2 metrics = 12 rows
  expect_equal(nrow(result), 12)
  expect_equal(length(unique(result$variable)), 2)
  expect_equal(length(unique(result$method)), 3)
  expect_equal(length(unique(result$metric)), 2)

  # Check structure
  expect_true(all(c("age", "wt71") %in% result$variable))
  expect_true(all(c("observed", "w_test1", "w_test2") %in% result$method))
  expect_true(all(c("smd", "ks") %in% result$metric))
})

# =============================================================================
# CONSISTENCY TESTS - VERIFY AGAINST INDIVIDUAL FUNCTIONS
# =============================================================================

test_that("check_balance SMD matches compute_smd", {
  data <- get_nhefs_test_data()

  # Get result from check_balance
  result <- check_balance_basic(data, age, qsmk, .metrics = "smd")
  balance_smd <- result$estimate[
    result$metric == "smd" & result$method == "observed"
  ]

  # Get result from compute_smd directly
  direct_smd <- compute_smd(
    covariate = data$age,
    group = data$qsmk,
    reference_group = 1L
  )

  expect_equal(balance_smd, direct_smd, tolerance = 1e-10)
})

test_that("check_balance variance_ratio matches compute_variance_ratio", {
  data <- get_nhefs_test_data()

  # Get result from check_balance
  result <- check_balance_basic(data, age, qsmk, .metrics = "variance_ratio")
  balance_vr <- result$estimate[
    result$metric == "variance_ratio" & result$method == "observed"
  ]

  # Get result from compute_variance_ratio directly
  # check_balance uses reference_group=1L which maps to first level (0) for compute_variance_ratio
  direct_vr <- compute_variance_ratio(
    covariate = data$age,
    group = data$qsmk,
    reference_group = 0 # First level of qsmk (group_levels[1])
  )

  expect_equal(balance_vr, direct_vr, tolerance = 1e-10)
})

test_that("check_balance KS matches compute_ks", {
  data <- get_nhefs_test_data()

  # Get result from check_balance
  result <- check_balance_basic(data, age, qsmk, .metrics = "ks")
  balance_ks <- result$estimate[
    result$metric == "ks" & result$method == "observed"
  ]

  # Get result from compute_ks directly
  direct_ks <- compute_ks(
    covariate = data$age,
    group = data$qsmk,
    reference_group = 0 # First level of qsmk
  )

  expect_equal(balance_ks, direct_ks, tolerance = 1e-10)
})

test_that("check_balance weighted results match individual weighted functions", {
  data <- get_nhefs_test_data()

  # Test weighted SMD
  result <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_test1,
    .metrics = "smd"
  )
  balance_smd <- result$estimate[
    result$metric == "smd" & result$method == "w_test1"
  ]

  direct_smd <- compute_smd(
    covariate = data$age,
    group = data$qsmk,
    weights = data$w_test1,
    reference_group = 1L
  )

  expect_equal(balance_smd, direct_smd, tolerance = 1e-10)

  # Test weighted variance ratio
  result_vr <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_test1,
    .metrics = "variance_ratio"
  )
  balance_vr <- result_vr$estimate[
    result_vr$metric == "variance_ratio" & result_vr$method == "w_test1"
  ]

  direct_vr <- compute_variance_ratio(
    covariate = data$age,
    group = data$qsmk,
    weights = data$w_test1,
    reference_group = 0 # First level (group_levels[1])
  )

  expect_equal(balance_vr, direct_vr, tolerance = 1e-10)
})

# =============================================================================
# REFERENCE GROUP TESTS
# =============================================================================

test_that("check_balance handles different reference groups correctly", {
  data <- get_nhefs_test_data()

  # Test with numeric reference group
  result1 <- check_balance_basic(
    data,
    age,
    qsmk,
    .metrics = "smd",
    reference_group = 0
  )
  result2 <- check_balance_basic(
    data,
    age,
    qsmk,
    .metrics = "smd",
    reference_group = 1
  )

  # SMDs should be negatives of each other
  expect_equal(result1$estimate, -result2$estimate, tolerance = 1e-10)

  # Group levels should be different
  expect_equal(result1$group_level, "1")
  expect_equal(result2$group_level, "0")
})

test_that("check_balance handles factor reference groups", {
  data <- get_nhefs_test_data()

  # Test with factor level reference group (sex is a factor)
  result1 <- check_balance_basic(
    data,
    age,
    sex,
    .metrics = "smd",
    reference_group = "0"
  )
  result2 <- check_balance_basic(
    data,
    age,
    sex,
    .metrics = "smd",
    reference_group = "1"
  )

  # SMDs should be negatives of each other
  expect_equal(result1$estimate, -result2$estimate, tolerance = 1e-10)

  # Group levels should be different
  expect_equal(result1$group_level, "1")
  expect_equal(result2$group_level, "0")
})

# =============================================================================
# MISSING VALUE HANDLING
# =============================================================================

test_that("check_balance handles missing values correctly", {
  data <- get_nhefs_test_data()

  # Introduce missing values
  data_na <- data
  data_na$age[1:20] <- NA

  # Test with na.rm = FALSE (should return NA)
  result_na_false <- check_balance_basic(
    data_na,
    age,
    qsmk,
    .metrics = "smd",
    na.rm = FALSE
  )
  expect_true(is.na(result_na_false$estimate))

  # Test with na.rm = TRUE (should work)
  result_na_true <- check_balance_basic(
    data_na,
    age,
    qsmk,
    .metrics = "smd",
    na.rm = TRUE
  )
  expect_true(is.finite(result_na_true$estimate))

  # Verify the na.rm = TRUE result matches direct computation
  direct_smd <- compute_smd(
    covariate = data_na$age,
    group = data_na$qsmk,
    reference_group = 1L,
    na.rm = TRUE
  )
  expect_equal(result_na_true$estimate, direct_smd, tolerance = 1e-10)
})

# =============================================================================
# ERROR HANDLING TESTS
# =============================================================================

test_that("check_balance validates inputs correctly", {
  data <- get_nhefs_test_data()

  # Test invalid data type
  expect_error(
    check_balance_basic("not_a_dataframe", age, qsmk),
    "must be a data frame"
  )

  # Test invalid group variable
  expect_error(
    check_balance_basic(data, age, nonexistent_group),
    "not found in data"
  )

  # Test invalid metrics
  expect_error(
    check_balance_basic(data, age, qsmk, .metrics = "invalid_metric"),
    "Invalid metrics"
  )

  # Test no variables selected
  expect_error(check_balance_basic(data, c(), qsmk), "No variables selected")

  # Test group with wrong number of levels
  data_bad_group <- data
  data_bad_group$bad_group <- rep(1, nrow(data))
  expect_error(
    check_balance_basic(data_bad_group, age, bad_group),
    "must have exactly two levels"
  )

  # Test no metrics to compute - should return empty tibble
  result_empty <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_test1,
    include_observed = FALSE,
    .metrics = character(0)
  )
  expect_equal(nrow(result_empty), 0)
  expect_s3_class(result_empty, "data.frame")
})

# =============================================================================
# EDGE CASES
# =============================================================================

test_that("check_balance handles binary variables correctly", {
  data <- get_nhefs_test_data()

  # Convert factors to numeric for binary testing
  data$qsmk_num <- as.numeric(data$qsmk) - 1 # Convert to 0/1
  data$sex_num <- as.numeric(data$sex) - 1 # Convert to 0/1

  # Test with qsmk as covariate (binary)
  result <- check_balance_basic(
    data,
    qsmk_num,
    sex_num,
    .metrics = c("smd", "variance_ratio", "ks")
  )

  expect_equal(nrow(result), 3)
  expect_true(all(is.finite(result$estimate)))

  # For binary variables, KS should equal absolute difference in proportions
  # Verify this matches the direct computation
  ks_result <- result$estimate[result$metric == "ks"]
  direct_ks <- compute_ks(data$qsmk_num, data$sex_num, reference_group = 0)
  expect_equal(ks_result, direct_ks, tolerance = 1e-10)
})

test_that("check_balance handles extreme weights", {
  data <- get_nhefs_test_data()

  # Test with extreme weights
  result <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_extreme,
    .metrics = "smd"
  )

  expect_equal(nrow(result), 2) # observed + weighted
  expect_true(all(is.finite(result$estimate)))

  # Verify it matches direct computation
  weighted_estimate <- result$estimate[result$method == "w_extreme"]
  direct_smd <- compute_smd(
    data$age,
    data$qsmk,
    weights = data$w_extreme,
    reference_group = 1L
  )
  expect_equal(weighted_estimate, direct_smd, tolerance = 1e-10)
})

test_that("check_balance output is properly arranged", {
  data <- get_nhefs_test_data()

  result <- check_balance_basic(
    data,
    c(age, wt71),
    qsmk,
    .wts = c(w_test1, w_test2),
    .metrics = c("smd", "ks")
  )

  # Check that results are arranged by variable, metric, method
  expect_true(all(result$variable[1:6] == "age"))
  expect_true(all(result$variable[7:12] == "wt71"))

  # Within each variable, should be arranged by metric
  age_metrics <- result$metric[result$variable == "age"]
  expect_true(all(age_metrics[1:3] == "ks"))
  expect_true(all(age_metrics[4:6] == "smd"))
})

# =============================================================================
# NHEFS-SPECIFIC TESTS
# =============================================================================

test_that("check_balance works with NHEFS factor variables", {
  data <- get_nhefs_test_data()

  # Convert factors to numeric for balance checking
  data$sex_num <- as.numeric(data$sex) - 1 # Convert to 0/1
  data$race_num <- as.numeric(data$race) - 1 # Convert to 0/1
  data$education_num <- as.numeric(data$education) # Keep factor levels as numbers

  # Test with converted factor covariates
  result <- check_balance_basic(
    data,
    c(sex_num, race_num, education_num),
    qsmk,
    .metrics = "smd"
  )

  expect_equal(nrow(result), 3) # 3 factor variables
  expect_true(all(is.finite(result$estimate)))
  expect_true(all(result$metric == "smd"))
})

test_that("check_balance works with mixed variable types from NHEFS", {
  data <- get_nhefs_test_data()

  # Convert factors to numeric for testing
  data$sex_num <- as.numeric(data$sex) - 1
  data$education_num <- as.numeric(data$education)

  # Test with mix of continuous and converted factor variables
  result <- check_balance_basic(
    data,
    c(age, wt71, sex_num, education_num, smokeintensity),
    qsmk,
    .metrics = c("smd", "variance_ratio")
  )

  expect_equal(nrow(result), 10) # 5 variables × 2 metrics
  expect_true(all(is.finite(result$estimate)))

  # All variables should be present
  expect_equal(length(unique(result$variable)), 5)
  expect_true(all(
    c("age", "wt71", "sex_num", "education_num", "smokeintensity") %in%
      result$variable
  ))
})

test_that("check_balance handles realistic smoking cessation balance assessment", {
  data <- get_nhefs_test_data()

  # Convert factor variables to numeric for balance checking
  data$sex_num <- as.numeric(data$sex) - 1 # Convert to 0/1
  data$race_num <- as.numeric(data$race) - 1 # Convert to 0/1
  data$education_num <- as.numeric(data$education) # Keep factor levels as numbers

  # Typical smoking cessation balance check with converted variables
  covariates <- c(
    "age",
    "wt71",
    "sex_num",
    "race_num",
    "education_num",
    "smokeintensity",
    "smokeyrs"
  )
  result <- check_balance_basic(
    data,
    all_of(covariates),
    qsmk,
    .wts = c(w_test1, w_test2),
    .metrics = c("smd", "ks")
  )

  # Should have: 7 variables × 3 methods × 2 metrics = 42 rows
  expect_equal(nrow(result), 42)
  expect_equal(length(unique(result$variable)), 7)
  expect_equal(length(unique(result$method)), 3)
  expect_equal(length(unique(result$metric)), 2)

  # All estimates should be finite
  expect_true(all(is.finite(result$estimate)))
})

# =============================================================================
# PERFORMANCE AND STRESS TESTS
# =============================================================================

test_that("check_balance handles full NHEFS dataset efficiently", {
  # Use full NHEFS data for stress testing
  data(nhefs_weights, package = "halfmoon")

  # Should complete without errors using existing weights
  expect_no_error({
    result <- check_balance_basic(
      nhefs_weights,
      c(age, wt71, smokeintensity),
      qsmk,
      .wts = c(w_ate, w_att),
      .metrics = c("smd", "variance_ratio", "ks")
    )
  })

  # Should have expected dimensions: 3 vars × 3 methods × 3 metrics = 27 rows
  expect_equal(nrow(result), 27)
  expect_true(all(is.finite(result$estimate)))
})

# =============================================================================
# TIDYSELECT FUNCTIONALITY TESTS
# =============================================================================

test_that("check_balance works with tidyselect helpers on NHEFS", {
  data <- get_nhefs_test_data()

  # Test with specific variables to avoid weight variable issues
  result1 <- check_balance_basic(
    data,
    c(age, wt71, smokeintensity, smokeyrs),
    qsmk,
    .metrics = "smd"
  )

  # Should include the specified numeric variables
  expected_vars <- c("age", "wt71", "smokeintensity", "smokeyrs")
  expect_equal(sort(unique(result1$variable)), sort(expected_vars))

  # Test with specific selection
  result2 <- check_balance_basic(
    data,
    starts_with("smoke"),
    qsmk,
    .metrics = "smd"
  )
  expect_true(all(grepl("^smoke", unique(result2$variable))))
})

test_that("check_balance works with real propensity score weights", {
  data <- get_nhefs_test_data()

  # Test with actual propensity score weights from nhefs_weights
  result <- check_balance_basic(
    data,
    c(age, wt71, smokeintensity),
    qsmk,
    .wts = c(w_ate, w_att, w_atc),
    .metrics = "smd"
  )

  # Should have: 3 variables × 4 methods (observed + 3 weights) × 1 metric = 12 rows
  expect_equal(nrow(result), 12)
  expect_equal(length(unique(result$variable)), 3)
  expect_equal(length(unique(result$method)), 4)
  expect_true(all(c("observed", "w_ate", "w_att", "w_atc") %in% result$method))
  expect_true(all(is.finite(result$estimate)))
})

# =============================================================================
# CORRELATION METRIC TESTS
# =============================================================================

test_that("check_balance works with correlation metric for continuous exposures", {
  data <- get_nhefs_test_data()

  # Test with correlation metric using continuous exposure (age)
  result <- check_balance_basic(
    data,
    c(wt71, smokeintensity),
    age,
    .metrics = "correlation"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 5)
  expect_equal(result$metric, rep("correlation", 2))
  expect_equal(result$group_level, rep("age", 2))
  expect_equal(result$method, rep("observed", 2))
  expect_true(all(result$estimate >= -1 & result$estimate <= 1))
  expect_true(all(is.finite(result$estimate)))
})

test_that("check_balance correlation works with weights", {
  data <- get_nhefs_test_data()

  # Test correlation with weights
  result <- check_balance_basic(
    data,
    c(wt71, smokeintensity),
    age,
    .wts = w_ate,
    .metrics = "correlation"
  )

  expect_equal(nrow(result), 4) # 2 variables × 2 methods (observed + w_ate)
  expect_true(all(c("observed", "w_ate") %in% result$method))
  expect_true(all(result$estimate >= -1 & result$estimate <= 1))
  expect_true(all(is.finite(result$estimate)))
})

test_that("check_balance correlation requires numeric group variable", {
  data <- get_nhefs_test_data()

  # Should error when using factor/binary variable with correlation
  expect_error(
    check_balance_basic(data, age, qsmk, .metrics = "correlation"),
    "Group variable must be numeric when using correlation metric"
  )
})

test_that("check_balance handles mixed metrics with correlation", {
  data <- get_nhefs_test_data()

  # Should error when mixing correlation with other metrics using binary group
  expect_error(
    check_balance_basic(
      data,
      age,
      qsmk,
      .metrics = c("smd", "correlation")
    ),
    "Group variable must be numeric when using correlation metric"
  )

  # But should work with continuous group when only using correlation
  result <- check_balance_basic(
    data,
    age,
    wt71,
    .metrics = "correlation"
  )
  expect_equal(nrow(result), 1)
  expect_equal(result$metric, "correlation")
})

test_that("check_balance correlation handles missing values", {
  data <- get_nhefs_test_data()

  # Introduce missing values
  data_na <- data
  data_na$age[1:10] <- NA
  data_na$wt71[5:15] <- NA

  # Should return NA when na.rm = FALSE
  result_na_false <- check_balance_basic(
    data_na,
    age,
    wt71,
    .metrics = "correlation",
    na.rm = FALSE
  )
  expect_true(is.na(result_na_false$estimate))

  # Should work when na.rm = TRUE
  result_na_true <- check_balance_basic(
    data_na,
    age,
    wt71,
    .metrics = "correlation",
    na.rm = TRUE
  )
  expect_true(is.finite(result_na_true$estimate))
})

test_that("check_balance supports both quoted and unquoted .wts", {
  data <- get_nhefs_test_data()

  # Test unquoted weight
  result_unquoted <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = w_ate,
    .metrics = "smd"
  )

  # Test quoted weight
  result_quoted <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = "w_ate",
    .metrics = "smd"
  )

  # Results should be identical
  expect_equal(result_unquoted, result_quoted)
})

test_that("check_balance supports multiple weight selection with c()", {
  data <- get_nhefs_test_data()

  # Test unquoted multiple weights
  result_unquoted <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = c(w_ate, w_att),
    .metrics = "smd"
  )

  # Test quoted multiple weights
  result_quoted <- check_balance_basic(
    data,
    age,
    qsmk,
    .wts = c("w_ate", "w_att"),
    .metrics = "smd"
  )

  # Results should be identical
  expect_equal(result_unquoted, result_quoted)

  # Check structure
  expect_equal(nrow(result_unquoted), 3) # observed + 2 weights
  expect_true(all(c("observed", "w_ate", "w_att") %in% result_unquoted$method))
})

# =============================================================================
# DATA TRANSFORMATION TESTS
# =============================================================================

test_that("make_dummy_vars works with categorical variables", {
  data <- get_nhefs_test_data()

  # Test with binary categorical variables (sex and race both have 2 levels)
  result <- check_balance(
    data,
    c(sex, race),
    qsmk,
    .metrics = "smd",
    make_dummy_vars = TRUE
  )

  # Binary variables should remain as single variables (not expanded)
  var_names <- unique(result$variable)
  expect_true("sex" %in% var_names)
  expect_true("race" %in% var_names)
  expect_equal(length(var_names), 2)

  # Test with mixed variable types including multi-level factor
  result_mixed <- check_balance(
    data,
    c(age, sex, education),
    qsmk,
    .metrics = "smd",
    make_dummy_vars = TRUE
  )

  # Should have age as numeric, sex as single variable, education as dummy variables
  var_names_mixed <- unique(result_mixed$variable)
  expect_true("age" %in% var_names_mixed)
  expect_true("sex" %in% var_names_mixed)
  
  # Education should be expanded to dummy variables
  education_vars <- var_names_mixed[grepl("education", var_names_mixed)]
  expect_true(length(education_vars) > 1)
  
  # Should have more variables than original 3 due to education expansion
  expect_true(length(var_names_mixed) > 3)
})

test_that("make_dummy_vars = FALSE preserves original variables", {
  data <- get_nhefs_test_data()

  # Test with categorical variables but no dummy transformation
  result <- check_balance(
    data,
    c(sex, race),
    qsmk,
    .metrics = "smd",
    make_dummy_vars = FALSE
  )

  # Should have exactly the original variables
  expect_equal(nrow(result), 2)
  expect_equal(sort(unique(result$variable)), c("race", "sex"))
})

test_that("squares argument works with numeric variables", {
  data <- get_nhefs_test_data()

  # Test with squares = TRUE (default)
  result_with_squares <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = TRUE,
    cubes = FALSE,
    interactions = FALSE
  )

  # Should have original variables plus squared versions
  var_names <- unique(result_with_squares$variable)
  expect_true("age" %in% var_names)
  expect_true("wt71" %in% var_names)
  expect_true("age_squared" %in% var_names)
  expect_true("wt71_squared" %in% var_names)
  expect_equal(nrow(result_with_squares), 4) # 2 original + 2 squared

  # Test with squares = FALSE
  result_no_squares <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = FALSE,
    cubes = FALSE,
    interactions = FALSE
  )

  # Should have only original variables
  expect_equal(nrow(result_no_squares), 2)
  expect_equal(sort(unique(result_no_squares$variable)), c("age", "wt71"))
})

test_that("cubes argument works with numeric variables", {
  data <- get_nhefs_test_data()

  # Test with cubes = TRUE (default)
  result_with_cubes <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = FALSE,
    cubes = TRUE,
    interactions = FALSE
  )

  # Should have original variables plus cubed versions
  var_names <- unique(result_with_cubes$variable)
  expect_true("age" %in% var_names)
  expect_true("wt71" %in% var_names)
  expect_true("age_cubed" %in% var_names)
  expect_true("wt71_cubed" %in% var_names)
  expect_equal(nrow(result_with_cubes), 4) # 2 original + 2 cubed

  # Test with cubes = FALSE
  result_no_cubes <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = FALSE,
    cubes = FALSE,
    interactions = FALSE
  )

  # Should have only original variables
  expect_equal(nrow(result_no_cubes), 2)
  expect_equal(sort(unique(result_no_cubes$variable)), c("age", "wt71"))
})

test_that("interactions argument works with numeric variables", {
  data <- get_nhefs_test_data()

  # Test with interactions = TRUE (default)
  result_with_interactions <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = FALSE,
    cubes = FALSE,
    interactions = TRUE
  )

  # Should have original variables plus interaction
  var_names <- unique(result_with_interactions$variable)
  expect_true("age" %in% var_names)
  expect_true("wt71" %in% var_names)
  expect_true("age_x_wt71" %in% var_names)
  expect_equal(nrow(result_with_interactions), 3) # 2 original + 1 interaction

  # Test with three variables
  result_three_vars <- check_balance(
    data,
    c(age, wt71, smokeintensity),
    qsmk,
    .metrics = "smd",
    squares = FALSE,
    cubes = FALSE,
    interactions = TRUE
  )

  # Should have 3 original + 3 interactions (age_x_wt71, age_x_smokeintensity, wt71_x_smokeintensity)
  expect_equal(nrow(result_three_vars), 6)

  # Test with interactions = FALSE
  result_no_interactions <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = FALSE,
    cubes = FALSE,
    interactions = FALSE
  )

  # Should have only original variables
  expect_equal(nrow(result_no_interactions), 2)
  expect_equal(sort(unique(result_no_interactions$variable)), c("age", "wt71"))
})

test_that("combined transformations work together", {
  data <- get_nhefs_test_data()

  # Test with all transformations
  result_all <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = "smd",
    squares = TRUE,
    cubes = TRUE,
    interactions = TRUE
  )

  # Should have:
  # - 2 original variables
  # - 2 squared variables
  # - 2 cubed variables
  # - 1 interaction between original variables (age_x_wt71)
  # Total: 7 variables
  expect_equal(nrow(result_all), 7)

  var_names <- unique(result_all$variable)
  expect_true("age" %in% var_names)
  expect_true("wt71" %in% var_names)
  expect_true("age_squared" %in% var_names)
  expect_true("wt71_squared" %in% var_names)
  expect_true("age_cubed" %in% var_names)
  expect_true("wt71_cubed" %in% var_names)
  expect_true("age_x_wt71" %in% var_names)
})

test_that("transformations work with dummy variables", {
  data <- get_nhefs_test_data()

  # Test combining dummy variables with squares/cubes/interactions
  result_mixed <- check_balance(
    data,
    c(age, sex),
    qsmk,
    .metrics = "smd",
    make_dummy_vars = TRUE,
    squares = TRUE,
    cubes = TRUE,
    interactions = TRUE
  )

  # Should have:
  # - age (numeric)
  # - sex dummy variables (categorical -> dummies)
  # - age_squared, age_cubed
  # - interactions between age and sex dummies

  var_names <- unique(result_mixed$variable)
  expect_true("age" %in% var_names)
  expect_true("age_squared" %in% var_names)
  expect_true("age_cubed" %in% var_names)
  expect_true(any(grepl("sex", var_names)))
  expect_true(any(grepl("age_x_sex", var_names)))

  # Should have more variables than just age and sex
  expect_true(nrow(result_mixed) > 2)
})

test_that("transformations work with weights", {
  data <- get_nhefs_test_data()

  # Test transformations with weights
  result_weighted <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .wts = w_test1,
    .metrics = "smd",
    squares = TRUE,
    cubes = FALSE,
    interactions = FALSE
  )

  # Should have transformed variables with both observed and weighted methods
  var_names <- unique(result_weighted$variable)
  expect_true("age" %in% var_names)
  expect_true("wt71" %in% var_names)
  expect_true("age_squared" %in% var_names)
  expect_true("wt71_squared" %in% var_names)

  methods <- unique(result_weighted$method)
  expect_true("observed" %in% methods)
  expect_true("w_test1" %in% methods)

  # Should have 4 variables × 2 methods = 8 rows
  expect_equal(nrow(result_weighted), 8)
})

test_that("transformations work with multiple metrics", {
  data <- get_nhefs_test_data()

  # Test transformations with multiple metrics
  result_multi_metrics <- check_balance(
    data,
    c(age, wt71),
    qsmk,
    .metrics = c("smd", "variance_ratio"),
    squares = TRUE,
    cubes = FALSE,
    interactions = FALSE
  )

  # Should have 4 variables × 2 metrics = 8 rows
  expect_equal(nrow(result_multi_metrics), 8)

  metrics <- unique(result_multi_metrics$metric)
  expect_equal(sort(metrics), c("smd", "variance_ratio"))
})

test_that("edge cases handled correctly", {
  data <- get_nhefs_test_data()

  # Test with single variable
  result_single <- check_balance(
    data,
    age,
    qsmk,
    .metrics = "smd",
    squares = TRUE,
    cubes = TRUE,
    interactions = TRUE # Should be ignored with single variable
  )

  # Should have age + age_squared + age_cubed (no interaction possible)
  expect_equal(nrow(result_single), 3)

  # Test with only categorical variables
  result_categorical <- check_balance(
    data,
    c(sex, race),
    qsmk,
    .metrics = "smd",
    make_dummy_vars = TRUE,
    squares = TRUE, # Should be ignored for categorical
    cubes = TRUE, # Should be ignored for categorical
    interactions = TRUE # Should work with dummy variables
  )

  # Should have dummy variables and their interactions
  var_names <- unique(result_categorical$variable)
  expect_true(any(grepl("sex", var_names)))
  expect_true(any(grepl("race", var_names)))
  expect_true(any(grepl("_x_", var_names))) # Interaction indicator
})

test_that("check_balance works with tidyselect helpers in .wts parameter", {
  data <- get_nhefs_test_data()
  
  # Test with starts_with()
  result1 <- check_balance_basic(
    data, 
    c(age, wt71), 
    qsmk, 
    .wts = starts_with("w_test")
  )
  
  expect_s3_class(result1, "data.frame")
  expect_gt(nrow(result1), 0)
  
  # Check that we got the correct weight methods
  weight_methods <- setdiff(unique(result1$method), "observed")
  expect_true(all(grepl("^w_test", weight_methods)))
  expect_equal(length(weight_methods), 2) # w_test1, w_test2
  
  # Test with ends_with()
  result2 <- check_balance_basic(
    data, 
    c(age, wt71), 
    qsmk, 
    .wts = ends_with("_test1")
  )
  
  expect_s3_class(result2, "data.frame")
  expect_true("w_test1" %in% unique(result2$method))
  
  # Test with contains()
  result3 <- check_balance_basic(
    data, 
    c(age, wt71), 
    qsmk, 
    .wts = contains("extreme")
  )
  
  expect_s3_class(result3, "data.frame")
  expect_true("w_extreme" %in% unique(result3$method))
})

test_that("check_balance new defaults work correctly", {
  data <- get_nhefs_test_data()
  
  # Test that make_dummy_vars = TRUE is now the default
  result_default <- check_balance(data, c(age, sex, race), qsmk, .metrics = "smd")
  
  # Should have age, sex, and race as single variables (sex and race are binary)
  var_names <- unique(result_default$variable)
  expect_true("age" %in% var_names)
  expect_true("sex" %in% var_names)
  expect_true("race" %in% var_names)
  
  # Should have exactly 3 variables (age + sex + race, no expansion for binary variables)
  expect_equal(length(var_names), 3)
  
  # Test that squares, cubes, interactions are FALSE by default
  result_minimal <- check_balance(data, c(age, wt71), qsmk, .metrics = "smd")
  var_names_minimal <- unique(result_minimal$variable)
  
  # Should not have squared, cubed, or interaction terms
  expect_false(any(grepl("_squared", var_names_minimal)))
  expect_false(any(grepl("_cubed", var_names_minimal)))
  expect_false(any(grepl("_x_", var_names_minimal)))
  
  # Should only have the original variables
  expect_equal(sort(var_names_minimal), c("age", "wt71"))
})

test_that("check_balance dummy variables include all levels for multi-level variables", {
  data <- get_nhefs_test_data()
  
  # Test with a factor that has multiple levels (education has 5 levels)
  result <- check_balance(data, education, qsmk, .metrics = "smd")
  
  # Should have dummy variables for all levels of education
  var_names <- unique(result$variable)
  education_dummies <- var_names[grepl("education", var_names)]
  
  # Should have all education levels (not dropping the first one)
  expect_gt(length(education_dummies), 1)
  
  # Check that we have the expected number of education levels
  n_education_levels <- length(levels(data$education))
  expect_equal(length(education_dummies), n_education_levels)
})

test_that("check_balance handles binary vs multi-level variables correctly", {
  data <- get_nhefs_test_data()
  
  # Test with multiple categorical variables
  result <- check_balance(data, c(sex, race, education), qsmk, .metrics = "smd")
  var_names <- unique(result$variable)
  
  # Check sex (binary variable - should remain as single variable)
  sex_vars <- var_names[grepl("^sex", var_names)]
  expect_equal(sex_vars, "sex")
  
  # Check race (binary variable - should remain as single variable)
  race_vars <- var_names[grepl("^race", var_names)]
  expect_equal(race_vars, "race")
  
  # Check education (multi-level variable - should have education1, education2, etc.)
  education_dummies <- var_names[grepl("^education", var_names)]
  expected_education <- paste0("education", levels(data$education))
  expect_equal(sort(education_dummies), sort(expected_education))
  
  # Test with exercise and active (3-level factors)
  result_multi <- check_balance(data, c(exercise, active), qsmk, .metrics = "smd")
  var_names_multi <- unique(result_multi$variable)
  
  # Check exercise (should have exercise0, exercise1, exercise2)
  exercise_dummies <- var_names_multi[grepl("^exercise", var_names_multi)]
  expected_exercise <- paste0("exercise", levels(data$exercise))
  expect_equal(sort(exercise_dummies), sort(expected_exercise))
  
  # Check active (should have active0, active1, active2)
  active_dummies <- var_names_multi[grepl("^active", var_names_multi)]
  expected_active <- paste0("active", levels(data$active))
  expect_equal(sort(active_dummies), sort(expected_active))
})

test_that("dummy variable creation handles edge cases correctly", {
  # Test with single-level factor (edge case)
  test_data <- data.frame(
    single_level = factor(rep("A", 10)),
    two_level = factor(c(rep("X", 5), rep("Y", 5))),
    qsmk = c(rep(0, 5), rep(1, 5))
  )
  
  result <- check_balance(test_data, c(single_level, two_level), qsmk, .metrics = "smd")
  var_names <- unique(result$variable)
  
  # Single level factor should create dummy (not binary)
  expect_true("single_levelA" %in% var_names)
  # Two level factor should remain as single variable (binary behavior)
  expect_true("two_level" %in% var_names)
  expect_false("two_levelX" %in% var_names)
  expect_false("two_levelY" %in% var_names)
  
  # Test with character variables
  test_char <- data.frame(
    char_var = c("cat", "dog", "cat", "bird", "dog", "cat", "bird", "dog", "cat", "dog"),
    qsmk = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  )
  
  result_char <- check_balance(test_char, char_var, qsmk, .metrics = "smd")
  var_names_char <- unique(result_char$variable)
  
  # Should create dummies for all unique character values (3 levels > 2)
  expect_true("char_varbird" %in% var_names_char)
  expect_true("char_varcat" %in% var_names_char)
  expect_true("char_vardog" %in% var_names_char)
})

test_that("dummy variables have correct values", {
  # Create simple test data
  test_data <- data.frame(
    x = factor(c("A", "B", "A", "C", "B", "A", "C", "B", "A", "B")),
    qsmk = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  )
  
  result <- check_balance(test_data, x, qsmk, .metrics = "smd")
  
  # Check that we have all three dummies
  var_names <- unique(result$variable)
  expect_equal(sort(var_names), c("xA", "xB", "xC"))
  
  # Check SMD values are reasonable (not NA, finite)
  expect_true(all(is.finite(result$estimate)))
  
  # Test that the transformation preserves information
  # A appears 4 times (in positions 1,3,6,9), B appears 4 times, C appears 2 times
  # So we should see different SMD values for each
  smd_values <- result$estimate[result$method == "observed" & result$metric == "smd"]
  expect_equal(length(unique(smd_values)), 3)  # Three different SMD values
})

test_that("binary variables are treated as single variables in main effects", {
  data <- get_nhefs_test_data()
  
  # Test with binary variables only
  result_binary <- check_balance(data, c(race, sex), qsmk, .metrics = "smd")
  var_names_binary <- unique(result_binary$variable)
  
  # Should have race and sex as single variables (not race0/race1, sex0/sex1)
  expect_equal(sort(var_names_binary), c("race", "sex"))
  expect_true(all(is.finite(result_binary$estimate)))
})

test_that("multi-level variables get dummy variables in main effects", {
  data <- get_nhefs_test_data()
  
  # Test with multi-level variables
  result_multi <- check_balance(data, c(education, exercise), qsmk, .metrics = "smd")
  var_names_multi <- unique(result_multi$variable)
  
  # Should have dummy variables for each level
  education_vars <- var_names_multi[grepl("^education", var_names_multi)]
  exercise_vars <- var_names_multi[grepl("^exercise", var_names_multi)]
  
  expect_equal(length(education_vars), 5)  # education1-5
  expect_equal(length(exercise_vars), 3)   # exercise0-2
  expect_true(all(is.finite(result_multi$estimate)))
})

test_that("mixed binary and multi-level variables work correctly", {
  data <- get_nhefs_test_data()
  
  # Test with mix of binary and multi-level
  result_mixed <- check_balance(data, c(race, sex, education, exercise), qsmk, .metrics = "smd")
  var_names_mixed <- unique(result_mixed$variable)
  
  # Should have binary variables as single variables
  expect_true("race" %in% var_names_mixed)
  expect_true("sex" %in% var_names_mixed)
  
  # Should have multi-level variables as dummy variables
  education_vars <- var_names_mixed[grepl("^education", var_names_mixed)]
  exercise_vars <- var_names_mixed[grepl("^exercise", var_names_mixed)]
  
  expect_equal(length(education_vars), 5)  # education1-5
  expect_equal(length(exercise_vars), 3)   # exercise0-2
  expect_true(all(is.finite(result_mixed$estimate)))
})

test_that("binary variables are expanded for interactions", {
  data <- get_nhefs_test_data()
  
  # Test interactions with binary variables
  result_interact <- check_balance(data, c(age, race, sex), qsmk, 
                                  interactions = TRUE, .metrics = "smd")
  var_names_interact <- unique(result_interact$variable)
  interact_vars <- var_names_interact[grepl("_x_", var_names_interact)]
  
  # Should have interactions with expanded binary variables
  expect_true("age_x_race0" %in% interact_vars)
  expect_true("age_x_race1" %in% interact_vars)
  expect_true("age_x_sex0" %in% interact_vars)
  expect_true("age_x_sex1" %in% interact_vars)
  expect_true("race0_x_sex0" %in% interact_vars)
  expect_true("race0_x_sex1" %in% interact_vars)
  expect_true("race1_x_sex0" %in% interact_vars)
  expect_true("race1_x_sex1" %in% interact_vars)
  
  # Should not have interactions between levels of same variable
  expect_false("race0_x_race1" %in% interact_vars)
  expect_false("sex0_x_sex1" %in% interact_vars)
  
  expect_true(all(is.finite(result_interact$estimate)))
})

test_that("no dummy variables when make_dummy_vars = FALSE", {
  data <- get_nhefs_test_data()
  
  # Test with dummy variables disabled
  result_no_dummy <- check_balance(data, c(race, sex, education), qsmk, 
                                  make_dummy_vars = FALSE, .metrics = "smd")
  var_names_no_dummy <- unique(result_no_dummy$variable)
  
  # Should have original variable names
  expect_equal(sort(var_names_no_dummy), c("education", "race", "sex"))
  
  # All should be NA because balance functions expect numeric variables
  expect_true(all(is.na(result_no_dummy$estimate)))
})

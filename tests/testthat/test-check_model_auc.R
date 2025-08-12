test_that("check_model_roc_curve works with basic inputs", {
  # Create simple test data
  set.seed(42)
  test_data <- tibble::tibble(
    truth = factor(c(rep("A", 50), rep("B", 50))),
    estimate = c(rnorm(50, 0.3, 0.2), rnorm(50, 0.7, 0.2)),
    weight1 = runif(100, 0.5, 2),
    weight2 = runif(100, 0.8, 1.5)
  )

  # Test unweighted ROC curve
  roc_basic <- check_model_roc_curve(
    test_data,
    truth,
    estimate,
    include_observed = TRUE
  )
  expect_s3_class(roc_basic, "tbl_df")
  expect_equal(unique(roc_basic$method), "observed")
  expect_equal(
    colnames(roc_basic),
    c("threshold", "sensitivity", "specificity", "method")
  )
  expect_true(all(roc_basic$sensitivity >= 0 & roc_basic$sensitivity <= 1))
  expect_true(all(roc_basic$specificity >= 0 & roc_basic$specificity <= 1))

  # Test with single weight
  roc_weighted <- check_model_roc_curve(test_data, truth, estimate, weight1)
  expect_equal(sort(unique(roc_weighted$method)), c("observed", "weight1"))

  # Test with multiple weights
  roc_multi <- check_model_roc_curve(
    test_data,
    truth,
    estimate,
    c(weight1, weight2)
  )
  expect_equal(
    sort(unique(roc_multi$method)),
    c("observed", "weight1", "weight2")
  )

  # Test without observed
  roc_no_obs <- check_model_roc_curve(
    test_data,
    truth,
    estimate,
    weight1,
    include_observed = FALSE
  )
  expect_equal(unique(roc_no_obs$method), "weight1")
})

test_that("check_auc computes correct AUC values", {
  # Create test data with known separation
  set.seed(123)
  test_data <- tibble::tibble(
    truth = factor(c(rep("A", 100), rep("B", 100))),
    # Perfect separation should give AUC near 1
    estimate_good = c(rnorm(100, 0, 0.1), rnorm(100, 1, 0.1)),
    # Random should give AUC near 0.5
    estimate_random = runif(200),
    weight1 = rep(1, 200)
  )

  # Test good separation
  auc_good <- check_model_auc(
    test_data,
    truth,
    estimate_good,
    include_observed = TRUE
  )
  expect_s3_class(auc_good, "tbl_df")
  expect_equal(colnames(auc_good), c("method", "auc"))
  # Note: AUC depends on which level is considered "positive"
  # For causal inference, we're less concerned with direction
  expect_true(abs(auc_good$auc[auc_good$method == "observed"] - 0.5) > 0.4)

  # Test random separation
  auc_random <- check_model_auc(
    test_data,
    truth,
    estimate_random,
    include_observed = TRUE
  )
  expect_true(abs(auc_random$auc[auc_random$method == "observed"] - 0.5) < 0.1)

  # Test with weights
  auc_weighted <- check_model_auc(test_data, truth, estimate_good, weight1)
  expect_equal(nrow(auc_weighted), 2) # observed + weight1
})

test_that("check_auc returns correct structure", {
  # Test basic functionality
  balance_check <- check_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att),
    include_observed = TRUE
  )

  expect_s3_class(balance_check, "tbl_df")
  expect_equal(colnames(balance_check), c("method", "auc"))
  expect_equal(nrow(balance_check), 3) # observed + w_ate + w_att
  expect_true(all(balance_check$auc >= 0 & balance_check$auc <= 1))

  # Test without observed
  balance_no_obs <- check_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate,
    include_observed = FALSE
  )
  expect_equal(nrow(balance_no_obs), 1)
  expect_equal(balance_no_obs$method, "w_ate")
})

test_that("functions handle edge cases correctly", {
  # Test with missing values
  test_data_na <- tibble::tibble(
    truth = factor(c(rep("A", 10), rep("B", 10), NA, NA)),
    estimate = c(runif(18), NA, NA, 0.5, 0.6),
    weight1 = runif(22)
  )

  # With na.rm = TRUE
  roc_na_rm <- suppressMessages(check_model_roc_curve(
    test_data_na,
    truth,
    estimate,
    weight1,
    na.rm = TRUE
  ))
  expect_s3_class(roc_na_rm, "tbl_df")

  # With na.rm = FALSE should error
  expect_halfmoon_error(
    check_model_roc_curve(
      test_data_na,
      truth,
      estimate,
      weight1,
      na.rm = FALSE
    ),
    "halfmoon_na_error"
  )

  # Test with constant estimates
  test_data_const <- tibble::tibble(
    truth = factor(rep(c("A", "B"), 10)),
    estimate = rep(0.5, 20),
    weight1 = rep(1, 20)
  )

  expect_warning(
    roc_const <- check_model_roc_curve(test_data_const, truth, estimate),
    class = "halfmoon_data_warning"
  )
  expect_equal(nrow(roc_const), 3) # Should have 3 points for degenerate curve

  # Test with zero weights
  test_data_zero <- tibble::tibble(
    truth = factor(rep(c("A", "B"), 10)),
    estimate = runif(20),
    weight_zero = c(rep(0, 5), runif(15))
  )

  expect_warning(
    roc_zero <- check_model_roc_curve(
      test_data_zero,
      truth,
      estimate,
      weight_zero
    ),
    class = "halfmoon_data_warning"
  )
  expect_s3_class(roc_zero, "tbl_df")

  # Test with negative weights
  test_data_neg <- tibble::tibble(
    truth = factor(rep(c("A", "B"), 10)),
    estimate = runif(20),
    weight_neg = c(rep(-1, 5), runif(15))
  )

  expect_warning(
    roc_neg <- check_model_roc_curve(
      test_data_neg,
      truth,
      estimate,
      weight_neg
    ),
    class = "halfmoon_data_warning"
  )
  expect_s3_class(roc_neg, "tbl_df")
})

test_that("functions handle different truth variable types", {
  set.seed(42)
  base_data <- tibble::tibble(
    estimate = runif(100),
    weight1 = runif(100, 0.5, 2)
  )

  # Test with character
  test_char <- dplyr::mutate(base_data, truth = rep(c("Yes", "No"), 50))
  roc_char <- check_model_roc_curve(test_char, truth, estimate)
  expect_s3_class(roc_char, "tbl_df")

  # Test with logical
  test_logical <- dplyr::mutate(base_data, truth = rep(c(TRUE, FALSE), 50))
  roc_logical <- check_model_roc_curve(test_logical, truth, estimate)
  expect_s3_class(roc_logical, "tbl_df")

  # Test with binary numeric
  test_numeric <- dplyr::mutate(base_data, truth = rep(c(0, 1), 50))
  roc_numeric <- check_model_roc_curve(test_numeric, truth, estimate)
  expect_s3_class(roc_numeric, "tbl_df")

  # Test with non-binary numeric (should error)
  test_multi <- dplyr::mutate(base_data, truth = rep(1:3, length.out = 100))
  expect_halfmoon_error(
    check_model_roc_curve(test_multi, truth, estimate),
    "halfmoon_group_error"
  )
})

test_that("error messages use proper cli formatting", {
  test_data <- tibble::tibble(
    truth = factor(rep(c("A", "B"), 10)),
    estimate = runif(20)
  )

  # Test .data not a data frame
  expect_halfmoon_error(
    check_model_roc_curve("not a data frame", truth, estimate),
    "halfmoon_type_error"
  )

  # Test non-numeric estimate
  test_data$estimate_char <- as.character(test_data$estimate)
  expect_halfmoon_error(
    check_model_roc_curve(test_data, truth, estimate_char),
    "halfmoon_type_error"
  )

  # Test multi-level truth
  test_data$truth_multi <- factor(rep(c("A", "B", "C"), length.out = 20))
  expect_halfmoon_error(
    check_model_roc_curve(test_data, truth_multi, estimate),
    "halfmoon_group_error"
  )
})

test_that("tidyselect works for weight selection", {
  test_data <- tibble::tibble(
    truth = factor(rep(c("A", "B"), 50)),
    estimate = runif(100),
    w_1 = runif(100),
    w_2 = runif(100),
    w_3 = runif(100),
    other_col = 1:100
  )

  # Test starts_with
  roc_starts <- check_model_roc_curve(
    test_data,
    truth,
    estimate,
    starts_with("w_")
  )
  expect_equal(
    sort(unique(roc_starts$method)),
    c("observed", "w_1", "w_2", "w_3")
  )

  # Test specific selection
  roc_specific <- check_model_roc_curve(test_data, truth, estimate, c(w_1, w_3))
  expect_equal(sort(unique(roc_specific$method)), c("observed", "w_1", "w_3"))
})

test_that("weighted ROC/AUC integrates with check_balance patterns", {
  # Compare output structure with check_balance
  balance_smd <- check_balance(
    nhefs_weights,
    c(age, wt71),
    qsmk,
    .wts = c(w_ate, w_att),
    .metrics = "smd"
  )

  balance_roc <- check_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )

  # Both should return tibbles
  expect_s3_class(balance_smd, "tbl_df")
  expect_s3_class(balance_roc, "tbl_df")

  # ROC balance should have simpler structure
  expect_equal(ncol(balance_roc), 2) # method, auc
  expect_true(all(c("method", "auc") %in% colnames(balance_roc)))
})

test_that("treatment_level parameter works correctly", {
  # Test with default (second level)
  roc_default <- check_model_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE
  )

  # Test with explicit treatment_level = "1" (same as default)
  roc_explicit <- check_model_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE,
    treatment_level = "1"
  )

  # Should be identical
  expect_equal(roc_default, roc_explicit)

  # Test with treatment_level = "0" (opposite)
  roc_opposite <- check_model_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE,
    treatment_level = "0"
  )

  # ROC curves should be different
  expect_false(identical(roc_default, roc_opposite))

  # Test with invalid treatment_level
  expect_halfmoon_error(
    check_model_roc_curve(
      nhefs_weights,
      qsmk,
      .fitted,
      treatment_level = "invalid"
    )
  )
})

test_that("compute_auc handles edge cases", {
  # Empty vectors
  expect_equal(compute_auc(numeric(0), numeric(0)), NA_real_)

  # Single point
  expect_equal(compute_auc(0.5, 0.5), 0.5)

  # Perfect ROC (should give AUC = 1)
  x <- c(0, 0, 1, 1)
  y <- c(0, 1, 1, 0)
  expect_equal(compute_auc(x, y), 1)

  # Worst ROC (should give AUC = 0)
  x <- c(0, 0, 1, 1)
  y <- c(0, 0, 0, 0)
  expect_equal(compute_auc(x, y), 0)

  # Random diagonal (should give AUC = 0.5)
  x <- c(0, 0.5, 1)
  y <- c(0, 0.5, 1)
  expect_equal(compute_auc(x, y), 0.5)
})

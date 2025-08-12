test_that("bal_model_roc_curve works with unweighted data", {
  roc_data <- bal_model_roc_curve(nhefs_weights, qsmk, .fitted)

  expect_s3_class(roc_data, "tbl_df")
  expect_named(roc_data, c("threshold", "sensitivity", "specificity"))

  # Check values are in valid range
  expect_true(all(roc_data$sensitivity >= 0 & roc_data$sensitivity <= 1))
  expect_true(all(roc_data$specificity >= 0 & roc_data$specificity <= 1))

  # Should have endpoints
  expect_true(any(roc_data$sensitivity == 1 & roc_data$specificity == 0))
  expect_true(any(roc_data$sensitivity == 0 & roc_data$specificity == 1))
})

test_that("bal_model_roc_curve works with weighted data", {
  roc_data <- bal_model_roc_curve(nhefs_weights, qsmk, .fitted, w_ate)

  expect_s3_class(roc_data, "tbl_df")
  expect_named(roc_data, c("threshold", "sensitivity", "specificity"))

  # Should be different from unweighted
  roc_unweighted <- bal_model_roc_curve(nhefs_weights, qsmk, .fitted)
  expect_false(identical(roc_data, roc_unweighted))
})

test_that("bal_model_roc_curve handles missing values", {
  # Create data with NAs
  nhefs_na <- nhefs_weights
  nhefs_na$.fitted[1:5] <- NA

  # With na.rm = TRUE
  roc_data <- bal_model_roc_curve(nhefs_na, qsmk, .fitted, na.rm = TRUE)
  expect_s3_class(roc_data, "tbl_df")
  expect_true(nrow(roc_data) > 0)

  # With na.rm = FALSE
  roc_na <- bal_model_roc_curve(nhefs_na, qsmk, .fitted, na.rm = FALSE)
  expect_true(all(is.na(roc_na$sensitivity)))
})

test_that("bal_model_roc_curve validates inputs", {
  expect_halfmoon_error(
    bal_model_roc_curve(nhefs_weights, nonexistent, .fitted),
    class = "halfmoon_arg_error"
  )

  expect_halfmoon_error(
    bal_model_roc_curve(nhefs_weights, qsmk, nonexistent),
    class = "halfmoon_arg_error"
  )

  # Multiple weights should error
  expect_halfmoon_error(
    bal_model_roc_curve(nhefs_weights, qsmk, .fitted, c(w_ate, w_att)),
    class = "halfmoon_arg_error"
  )
})

test_that("bal_model_roc_curve matches check_model_roc_curve for single method", {
  # Get single ROC curve
  roc_single <- bal_model_roc_curve(nhefs_weights, qsmk, .fitted, w_ate)

  # Get from check_model_roc_curve
  roc_check <- check_model_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate,
    include_observed = FALSE
  )

  # Should match the values (check_model_roc_curve has an additional method column)
  expect_equal(roc_single$threshold, roc_check$threshold)
  expect_equal(roc_single$sensitivity, roc_check$sensitivity)
  expect_equal(roc_single$specificity, roc_check$specificity)
})

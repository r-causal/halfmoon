test_that("bal_model_auc works with unweighted data", {
  auc_val <- bal_model_auc(nhefs_weights, qsmk, .fitted)

  expect_type(auc_val, "double")
  expect_length(auc_val, 1)
  expect_true(auc_val >= 0 && auc_val <= 1)

  # Should match check_model_auc for single method
  auc_check <- check_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE
  )
  expect_equal(auc_val, auc_check$auc[auc_check$method == "observed"])
})

test_that("bal_model_auc works with weighted data", {
  auc_val <- bal_model_auc(nhefs_weights, qsmk, .fitted, w_ate)

  expect_type(auc_val, "double")
  expect_length(auc_val, 1)
  expect_true(auc_val >= 0 && auc_val <= 1)

  # Should match check_model_auc for single weight
  auc_check <- check_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate,
    include_observed = FALSE
  )
  expect_equal(auc_val, auc_check$auc[1])
})

test_that("bal_model_auc handles missing values", {
  # Create data with NAs
  nhefs_na <- nhefs_weights
  nhefs_na$.fitted[1:5] <- NA

  # With na.rm = TRUE
  auc_val <- bal_model_auc(nhefs_na, qsmk, .fitted, na.rm = TRUE)
  expect_type(auc_val, "double")
  expect_false(is.na(auc_val))

  # With na.rm = FALSE
  auc_val_na <- bal_model_auc(nhefs_na, qsmk, .fitted, na.rm = FALSE)
  expect_true(is.na(auc_val_na))
})

test_that("bal_model_auc validates inputs", {
  expect_halfmoon_error(
    bal_model_auc(nhefs_weights, nonexistent, .fitted),
    class = "halfmoon_arg_error"
  )

  expect_halfmoon_error(
    bal_model_auc(nhefs_weights, qsmk, nonexistent),
    class = "halfmoon_arg_error"
  )

  # Multiple weights should error
  expect_halfmoon_error(
    bal_model_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att)),
    class = "halfmoon_arg_error"
  )
})

test_that("bal_model_auc works with different treatment levels", {
  # Default treatment level
  auc_default <- bal_model_auc(nhefs_weights, qsmk, .fitted)

  # Explicit treatment level
  auc_explicit <- bal_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    treatment_level = 1
  )

  # Should be different from opposite level
  auc_opposite <- bal_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    treatment_level = 0
  )
  expect_false(isTRUE(all.equal(auc_explicit, auc_opposite)))
})

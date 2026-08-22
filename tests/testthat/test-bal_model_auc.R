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
    class = "halfmoon_column_error"
  )

  expect_halfmoon_error(
    bal_model_auc(nhefs_weights, qsmk, nonexistent),
    class = "halfmoon_column_error"
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
    .focal_level = 1
  )

  # Should be different from opposite level
  auc_opposite <- bal_model_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    .focal_level = 0
  )
  expect_false(isTRUE(all.equal(auc_explicit, auc_opposite)))
  expect_equal(auc_default, auc_explicit)
  expect_equal(auc_opposite, 1 - auc_explicit, tolerance = 1e-10)
})

test_that("bal_model_auc defaults to the last observed level", {
  set.seed(20240119)
  n <- 60
  reordered <- tibble::tibble(
    truth = factor(
      c(rep("high", n / 2), rep("low", n / 2)),
      levels = c("low", "high")
    ),
    score = c(runif(n / 2, 0.4, 1), runif(n / 2, 0, 0.6))
  )

  auc_default <- bal_model_auc(reordered, truth, score)
  auc_last <- bal_model_auc(reordered, truth, score, .focal_level = "high")
  auc_first <- bal_model_auc(reordered, truth, score, .focal_level = "low")

  expect_equal(auc_default, auc_last)
  expect_equal(auc_first, 1 - auc_last, tolerance = 1e-10)
})

test_that("bal_model_auc requires exactly two observed exposure levels", {
  set.seed(20240120)
  three_level <- tibble::tibble(
    truth = factor(rep(c("a", "b", "c"), length.out = 60)),
    score = runif(60)
  )
  expect_error(
    bal_model_auc(three_level, truth, score),
    class = "halfmoon_group_error"
  )

  unused <- tibble::tibble(
    truth = factor(rep(c("a", "c"), 30), levels = c("a", "b", "c")),
    score = runif(60)
  )
  dropped <- unused
  dropped$truth <- droplevels(dropped$truth)
  expect_equal(
    bal_model_auc(unused, truth, score),
    bal_model_auc(dropped, truth, score)
  )
})

test_that("bal_model_auc drops zero and negative weights", {
  set.seed(11)
  n <- 40
  negative <- tibble::tibble(
    truth = factor(rep(c(0, 1), n / 2)),
    score = runif(n),
    weight = runif(n, 0.5, 2)
  )
  negative$weight[negative$truth == "0"][1:6] <- -2

  expect_warning(
    auc_bal <- bal_model_auc(negative, truth, score, weight),
    class = "halfmoon_data_warning"
  )
  auc_check <- suppressWarnings(
    check_model_auc(
      negative,
      truth,
      score,
      weight,
      include_observed = FALSE
    )$auc
  )
  expect_equal(auc_bal, auc_check, tolerance = 1e-10)
})

test_that("model diagnostic functions default to na.rm = TRUE", {
  expect_true(isTRUE(formals(bal_model_auc)$na.rm))
  expect_true(isTRUE(formals(bal_model_roc_curve)$na.rm))
  expect_true(isTRUE(formals(check_model_auc)$na.rm))
  expect_true(isTRUE(formals(check_model_roc_curve)$na.rm))
})

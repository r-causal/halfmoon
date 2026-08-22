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
    class = "halfmoon_column_error"
  )

  expect_halfmoon_error(
    bal_model_roc_curve(nhefs_weights, qsmk, nonexistent),
    class = "halfmoon_column_error"
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

test_that("bal_model_roc_curve requires exactly two observed exposure levels", {
  set.seed(20240120)
  three_level <- tibble::tibble(
    truth = factor(rep(c("a", "b", "c"), length.out = 60)),
    score = runif(60)
  )
  expect_error(
    bal_model_roc_curve(three_level, truth, score),
    class = "halfmoon_group_error"
  )

  unused <- tibble::tibble(
    truth = factor(rep(c("a", "c"), 30), levels = c("a", "b", "c")),
    score = runif(60)
  )
  dropped <- unused
  dropped$truth <- droplevels(dropped$truth)
  expect_equal(
    bal_model_roc_curve(unused, truth, score),
    bal_model_roc_curve(dropped, truth, score)
  )
})

test_that("bal_model_roc_curve drops zero and negative weights", {
  set.seed(11)
  n <- 40
  negative <- tibble::tibble(
    truth = factor(rep(c(0, 1), n / 2)),
    score = runif(n),
    weight = runif(n, 0.5, 2)
  )
  negative$weight[negative$truth == "0"][1:6] <- -2

  expect_warning(
    roc_negative <- bal_model_roc_curve(negative, truth, score, weight),
    class = "halfmoon_data_warning"
  )
  expect_true(all(
    roc_negative$specificity >= 0 & roc_negative$specificity <= 1
  ))
  expect_true(all(
    roc_negative$sensitivity >= 0 & roc_negative$sensitivity <= 1
  ))

  roc_check <- suppressWarnings(
    check_model_roc_curve(
      negative,
      truth,
      score,
      weight,
      include_observed = FALSE
    )
  )
  expect_equal(roc_negative$sensitivity, roc_check$sensitivity)
  expect_equal(roc_negative$specificity, roc_check$specificity)
})

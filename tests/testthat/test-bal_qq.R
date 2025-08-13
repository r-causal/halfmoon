test_that("bal_qq works with unweighted data", {
  qq_data <- bal_qq(nhefs_weights, age, qsmk)

  expect_s3_class(qq_data, "tbl_df")
  expect_named(
    qq_data,
    c("quantile", "exposed_quantiles", "unexposed_quantiles")
  )

  # Default should be 99 quantiles
  expect_equal(nrow(qq_data), 99)

  # Quantiles should be in order
  expect_true(all(diff(qq_data$exposed_quantiles) >= 0))
  expect_true(all(diff(qq_data$unexposed_quantiles) >= 0))
})

test_that("bal_qq works with weighted data", {
  qq_data <- bal_qq(nhefs_weights, age, qsmk, .weights = w_ate)

  expect_s3_class(qq_data, "tbl_df")
  expect_named(
    qq_data,
    c("quantile", "exposed_quantiles", "unexposed_quantiles")
  )

  # Should be different from unweighted
  qq_unweighted <- bal_qq(nhefs_weights, age, qsmk)
  expect_false(identical(
    qq_data$exposed_quantiles,
    qq_unweighted$exposed_quantiles
  ))
})

test_that("bal_qq handles custom quantiles", {
  custom_q <- seq(0.1, 0.9, 0.1)
  qq_data <- bal_qq(nhefs_weights, age, qsmk, quantiles = custom_q)

  expect_equal(nrow(qq_data), length(custom_q))
  expect_equal(qq_data$quantile, custom_q)
})

test_that("bal_qq handles missing values", {
  # Create data with NAs
  nhefs_na <- nhefs_weights
  nhefs_na$age[1:5] <- NA

  # With na.rm = TRUE
  qq_data <- bal_qq(nhefs_na, age, qsmk, na.rm = TRUE)
  expect_s3_class(qq_data, "tbl_df")
  expect_false(any(is.na(qq_data$exposed_quantiles)))

  # With na.rm = FALSE should error
  expect_halfmoon_error(
    bal_qq(nhefs_na, age, qsmk, na.rm = FALSE),
    class = "halfmoon_na_error"
  )
})

test_that("bal_qq validates inputs", {
  # Non-existent variable
  expect_halfmoon_error(
    bal_qq(nhefs_weights, nonexistent, qsmk),
    class = "halfmoon_column_error"
  )

  # Non-existent group
  expect_halfmoon_error(
    bal_qq(nhefs_weights, age, nonexistent),
    class = "halfmoon_column_error"
  )

  # Non-binary group
  expect_halfmoon_error(
    bal_qq(nhefs_weights, age, alcoholfreq_cat),
    class = "halfmoon_group_error"
  )

  # Multiple weights should error
  expect_halfmoon_error(
    bal_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att)),
    class = "halfmoon_arg_error"
  )
})

test_that("bal_qq works with different treatment levels", {
  # Default treatment level should be 1 (last level) - same as check_qq
  qq_default <- bal_qq(nhefs_weights, age, qsmk, quantiles = c(0.25, 0.5, 0.75))

  # Explicit treatment level = 1
  qq_1 <- bal_qq(
    nhefs_weights,
    age,
    qsmk,
    .reference_level = 1,
    quantiles = c(0.25, 0.5, 0.75)
  )

  # Default should match explicit treatment_level = 1
  expect_equal(qq_default$exposed_quantiles, qq_1$exposed_quantiles)
  expect_equal(qq_default$unexposed_quantiles, qq_1$unexposed_quantiles)

  # Explicit treatment level = 0
  qq_0 <- bal_qq(
    nhefs_weights,
    age,
    qsmk,
    .reference_level = 0,
    quantiles = c(0.25, 0.5, 0.75)
  )

  # Treated and untreated should swap
  expect_equal(qq_1$exposed_quantiles, qq_0$unexposed_quantiles)
  expect_equal(qq_1$unexposed_quantiles, qq_0$exposed_quantiles)

  # Invalid treatment level should error
  expect_halfmoon_error(
    bal_qq(nhefs_weights, age, qsmk, .reference_level = 2),
    class = "halfmoon_reference_error"
  )
})

test_that("bal_qq matches single method from check_qq", {
  # Test with default treatment_level (should use last/max level)
  qq_single <- bal_qq(nhefs_weights, age, qsmk, quantiles = seq(0.1, 0.9, 0.1))
  qq_check <- check_qq(
    nhefs_weights,
    age,
    qsmk,
    quantiles = seq(0.1, 0.9, 0.1),
    include_observed = TRUE
  )
  qq_check_observed <- qq_check[qq_check$method == "observed", ]

  expect_equal(qq_single$quantile, qq_check_observed$quantile)
  expect_equal(qq_single$exposed_quantiles, qq_check_observed$exposed_quantiles)
  expect_equal(
    qq_single$unexposed_quantiles,
    qq_check_observed$unexposed_quantiles
  )

  # Test with weighted data
  qq_single_wt <- bal_qq(nhefs_weights, age, qsmk, .weights = w_ate)
  qq_check_wt <- check_qq(
    nhefs_weights,
    age,
    qsmk,
    .weights = w_ate,
    include_observed = FALSE
  )

  expect_equal(qq_single_wt$quantile, qq_check_wt$quantile)
  expect_equal(qq_single_wt$exposed_quantiles, qq_check_wt$exposed_quantiles)
  expect_equal(
    qq_single_wt$unexposed_quantiles,
    qq_check_wt$unexposed_quantiles
  )
})

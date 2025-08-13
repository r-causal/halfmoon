test_that("check_qq computes basic quantiles", {
  result <- check_qq(nhefs_weights, age, qsmk)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 99) # 99 quantiles for observed only
  expect_equal(
    colnames(result),
    c("method", "quantile", "treated_quantiles", "untreated_quantiles")
  )
  expect_equal(unique(result$method), factor("observed"))
})

test_that("check_qq works with weights", {
  result <- check_qq(nhefs_weights, age, qsmk, .weights = w_ate)

  expect_equal(nrow(result), 198) # 99 quantiles * 2 methods
  expect_equal(levels(result$method), c("observed", "w_ate"))
})

test_that("check_qq works with multiple weights", {
  result <- check_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))

  expect_equal(nrow(result), 297) # 99 quantiles * 3 methods
  expect_equal(levels(result$method), c("observed", "w_ate", "w_att"))
})

test_that("check_qq works without observed", {
  result <- check_qq(
    nhefs_weights,
    age,
    qsmk,
    .weights = w_ate,
    include_observed = FALSE
  )

  expect_equal(nrow(result), 99) # 99 quantiles * 1 method
  expect_equal(unique(result$method), factor("w_ate"))
})

test_that("check_qq handles custom quantiles", {
  custom_q <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  result <- check_qq(nhefs_weights, age, qsmk, quantiles = custom_q)

  expect_equal(nrow(result), 5)
  expect_equal(unique(result$quantile), custom_q)
})

test_that("check_qq handles quoted column names", {
  result1 <- check_qq(nhefs_weights, age, qsmk)
  result2 <- check_qq(nhefs_weights, "age", "qsmk")

  expect_equal(result1, result2)
})

test_that("check_qq errors with missing columns", {
  expect_halfmoon_error(
    check_qq(nhefs_weights, missing_var, qsmk),
    "halfmoon_column_error"
  )

  expect_halfmoon_error(
    check_qq(nhefs_weights, age, missing_group),
    "halfmoon_column_error"
  )
})

test_that("check_qq errors with non-binary groups", {
  df <- nhefs_weights
  df$three_groups <- rep(1:3, length.out = nrow(df))

  expect_halfmoon_error(
    check_qq(df, age, three_groups),
    "halfmoon_group_error"
  )
})

test_that("check_qq handles NA values correctly", {
  df <- nhefs_weights
  df$age[1:10] <- NA

  # Should work with na.rm = TRUE
  result <- check_qq(df, age, qsmk, na.rm = TRUE)
  expect_false(any(is.na(result$treated_quantiles)))
  expect_false(any(is.na(result$untreated_quantiles)))

  # Should have NAs with na.rm = FALSE
  expect_halfmoon_error(check_qq(df, age, qsmk), "halfmoon_na_error")
})

test_that("check_qq handles NULL .reference_level correctly", {
  # Test with factor
  test_factor <- data.frame(
    x = 1:10,
    group = factor(rep(c("Control", "Treatment"), each = 5))
  )

  result_factor <- check_qq(test_factor, x, group, quantiles = 0.5)
  # Should use "Treatment" (last level) as reference
  expect_equal(as.numeric(result_factor$treated_quantiles), 8) # median of 6:10
  expect_equal(as.numeric(result_factor$untreated_quantiles), 3) # median of 1:5

  # Test with numeric
  test_numeric <- data.frame(
    x = 1:10,
    group = rep(c(0, 1), each = 5)
  )

  result_numeric <- check_qq(test_numeric, x, group, quantiles = 0.5)
  # Should use 1 (max value) as reference
  expect_equal(as.numeric(result_numeric$treated_quantiles), 8) # median of 6:10
  expect_equal(as.numeric(result_numeric$untreated_quantiles), 3) # median of 1:5
})

test_that("check_qq returns expected quantile values", {
  # Create simple test data
  set.seed(123)
  test_data <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(50, 1, 1)),
    group = rep(c("A", "B"), each = 50)
  )

  result <- check_qq(test_data, x, group, quantiles = c(0.25, 0.5, 0.75))

  # Check that we get 3 quantiles
  expect_equal(nrow(result), 3)

  # With default NULL .reference_level, B (last level) is reference group
  # So treated_quantiles are from B (higher values) and untreated_quantiles from A (lower values)
  expect_true(all(result$treated_quantiles > result$untreated_quantiles))

  # Test with explicit .reference_level = "A"
  result_explicit <- check_qq(
    test_data,
    x,
    group,
    quantiles = c(0.25, 0.5, 0.75),
    .reference_level = "A"
  )
  # Now A is reference, so treated_quantiles < untreated_quantiles
  expect_true(all(
    result_explicit$treated_quantiles < result_explicit$untreated_quantiles
  ))
})

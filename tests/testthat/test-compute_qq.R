test_that("qq computes basic quantiles", {
  result <- qq(nhefs_weights, age, qsmk)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 99) # 99 quantiles for observed only
  expect_equal(
    colnames(result),
    c("method", "quantile", "reference_quantile", "comparison_quantile")
  )
  expect_equal(unique(result$method), factor("observed"))
})

test_that("qq works with weights", {
  result <- qq(nhefs_weights, age, qsmk, .wts = w_ate)

  expect_equal(nrow(result), 198) # 99 quantiles * 2 methods
  expect_equal(levels(result$method), c("observed", "w_ate"))
})

test_that("qq works with multiple weights", {
  result <- qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))

  expect_equal(nrow(result), 297) # 99 quantiles * 3 methods
  expect_equal(levels(result$method), c("observed", "w_ate", "w_att"))
})

test_that("qq works without observed", {
  result <- qq(nhefs_weights, age, qsmk, .wts = w_ate, include_observed = FALSE)

  expect_equal(nrow(result), 99) # 99 quantiles * 1 method
  expect_equal(unique(result$method), factor("w_ate"))
})

test_that("qq handles custom quantiles", {
  custom_q <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  result <- qq(nhefs_weights, age, qsmk, quantiles = custom_q)

  expect_equal(nrow(result), 5)
  expect_equal(unique(result$quantile), custom_q)
})

test_that("qq handles quoted column names", {
  result1 <- qq(nhefs_weights, age, qsmk)
  result2 <- qq(nhefs_weights, "age", "qsmk")

  expect_equal(result1, result2)
})

test_that("qq errors with missing columns", {
  expect_error(
    qq(nhefs_weights, missing_var, qsmk),
    "not found in data"
  )

  expect_error(
    qq(nhefs_weights, age, missing_group),
    "not found in data"
  )
})

test_that("qq errors with non-binary groups", {
  df <- nhefs_weights
  df$three_groups <- rep(1:3, length.out = nrow(df))

  expect_error(
    qq(df, age, three_groups),
    "exactly 2 levels"
  )
})

test_that("qq handles NA values correctly", {
  df <- nhefs_weights
  df$age[1:10] <- NA

  # Should work with na.rm = TRUE
  result <- qq(df, age, qsmk, na.rm = TRUE)
  expect_false(any(is.na(result$reference_quantile)))
  expect_false(any(is.na(result$comparison_quantile)))

  # Should have NAs with na.rm = FALSE
  expect_error(qq(df, age, qsmk), "missing values")
})

test_that("qq returns expected quantile values", {
  # Create simple test data
  set.seed(123)
  test_data <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(50, 1, 1)),
    group = rep(c("A", "B"), each = 50)
  )

  result <- qq(test_data, x, group, quantiles = c(0.25, 0.5, 0.75))

  # Check that we get 3 quantiles
  expect_equal(nrow(result), 3)

  # Reference group (A) should have lower values than comparison group (B)
  expect_true(all(result$reference_quantile < result$comparison_quantile))
})

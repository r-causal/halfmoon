test_that("plot_qq creates basic QQ plot", {
  p <- plot_qq(nhefs_weights, age, qsmk)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 2) # points + abline
  expect_equal(p$labels$x, "age (qsmk = 0)")
  expect_equal(p$labels$y, "age (qsmk = 1)")
})

test_that("plot_qq works with weights", {
  p <- plot_qq(nhefs_weights, age, qsmk, .wts = w_ate)

  expect_s3_class(p, "ggplot")
  # Should have 3 layers: points + abline + scale_color_discrete
  expect_equal(length(p$layers), 2)

  # Check that data has both observed and weighted
  plot_data <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(plot_data), 198) # 99 quantiles * 2 methods
})

test_that("plot_qq works with multiple weights", {
  p <- plot_qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))

  expect_s3_class(p, "ggplot")

  # Check that data has observed + 2 weighted methods
  plot_data <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(plot_data), 297) # 99 quantiles * 3 methods
})

test_that("plot_qq works without observed", {
  p <- plot_qq(nhefs_weights, age, qsmk, .wts = w_ate, include_observed = FALSE)

  expect_s3_class(p, "ggplot")

  # Check that data has only weighted method
  plot_data <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(plot_data), 99) # 99 quantiles * 1 method
})

test_that("plot_qq handles quoted column names", {
  p1 <- plot_qq(nhefs_weights, age, qsmk)
  p2 <- plot_qq(nhefs_weights, "age", "qsmk")

  expect_equal(p1$data, p2$data)
})

test_that("plot_qq errors with missing columns", {
  expect_error(
    plot_qq(nhefs_weights, missing_var, qsmk),
    "not found in data"
  )

  expect_error(
    plot_qq(nhefs_weights, age, missing_group),
    "not found in data"
  )
})

test_that("plot_qq errors with non-binary groups", {
  # Create data with 3 groups
  df <- nhefs_weights
  df$three_groups <- rep(1:3, length.out = nrow(df))

  expect_error(
    plot_qq(df, age, three_groups),
    "exactly 2 levels"
  )
})

test_that("weighted_quantile works correctly", {
  # Test with simple data
  values <- 1:10
  weights <- rep(1, 10)
  quantiles <- c(0.25, 0.5, 0.75)

  result <- weighted_quantile(values, quantiles, .wts = weights)
  # With equal weights, should be close to regular quantiles
  # but not exactly equal due to interpolation method
  expected <- stats::quantile(values, quantiles)
  expect_equal(result, c(2.5, 5, 7.5))

  # Test with non-uniform weights
  weights <- c(rep(1, 5), rep(2, 5))
  result <- weighted_quantile(values, quantiles, .wts = weights)

  # Should be weighted towards higher values
  expect_true(all(result > c(2.5, 5, 7.5)))
})

test_that("plot_qq handles NA values", {
  # Add some NA values
  df <- nhefs_weights
  df$age[1:10] <- NA

  # Should work with na.rm = TRUE
  expect_no_error(plot_qq(df, age, qsmk, na.rm = TRUE))

  # Should error with na.rm = FALSE (default) when NAs are present
  expect_error(plot_qq(df, age, qsmk), "missing values")
})

# vdiffr tests
test_that("plot_qq visual regression tests", {
  # Basic QQ plot
  expect_doppelganger(
    "basic qq plot",
    plot_qq(nhefs_weights, age, qsmk)
  )

  # With single weight
  expect_doppelganger(
    "qq plot with weight",
    plot_qq(nhefs_weights, age, qsmk, .wts = w_ate)
  )

  # With multiple weights
  expect_doppelganger(
    "qq plot multiple weights",
    plot_qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))
  )

  # Without observed
  expect_doppelganger(
    "qq plot no observed",
    plot_qq(nhefs_weights, age, qsmk, .wts = w_ate, include_observed = FALSE)
  )

  # With propensity score
  expect_doppelganger(
    "qq plot propensity score",
    plot_qq(nhefs_weights, .fitted, qsmk, .wts = w_ate)
  )
})

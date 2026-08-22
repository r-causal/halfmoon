test_that("plot_qq creates basic QQ plot", {
  p <- plot_qq(nhefs_weights, age, qsmk)

  expect_s3_class(p, "ggplot")
  expect_equal(length(p$layers), 2) # points + abline
  # With NULL .reference_level, the first observed level (0) is the reference
  # group and goes on the x axis
  expect_equal(p$labels$x, "age (qsmk = 0)")
  expect_equal(p$labels$y, "age (qsmk = 1)")
})

test_that("plot_qq puts the reference group on the x axis", {
  p <- plot_qq(
    nhefs_weights,
    age,
    qsmk,
    .reference_level = 1,
    quantiles = c(0.25, 0.5, 0.75)
  )

  expect_equal(p$labels$x, "age (qsmk = 1)")
  expect_equal(p$labels$y, "age (qsmk = 0)")

  qq <- bal_qq(
    nhefs_weights,
    age,
    qsmk,
    .reference_level = 1,
    quantiles = c(0.25, 0.5, 0.75)
  )
  built <- ggplot2::layer_data(p, 1)
  expect_equal(built$x, qq$unexposed_quantiles)
  expect_equal(built$y, qq$exposed_quantiles)
})

test_that("plot_qq methods agree on point coordinates", {
  quantiles <- c(0.1, 0.5, 0.9)
  p_default <- plot_qq(nhefs_weights, age, qsmk, quantiles = quantiles)
  p_qq <- plot_qq(check_qq(nhefs_weights, age, qsmk, quantiles = quantiles))

  expect_equal(
    ggplot2::layer_data(p_default, 1)[, c("x", "y")],
    ggplot2::layer_data(p_qq, 2)[, c("x", "y")]
  )
})

test_that("plot_qq uses observed exposure levels", {
  df <- data.frame(
    x = c(1:10, 21:30),
    g = factor(rep(c("a", "b"), each = 10), levels = c("a", "b", "c"))
  )
  dropped <- df
  dropped$g <- droplevels(dropped$g)

  expect_equal(
    ggplot2::layer_data(plot_qq(df, x, g, quantiles = c(0.25, 0.75)), 1),
    ggplot2::layer_data(plot_qq(dropped, x, g, quantiles = c(0.25, 0.75)), 1)
  )

  one_level <- data.frame(
    x = 1:10,
    g = factor(rep("a", 10), levels = c("a", "b"))
  )
  expect_halfmoon_error(
    plot_qq(one_level, x, g),
    "halfmoon_group_error"
  )
})

test_that("plot_qq works with weights", {
  p <- plot_qq(nhefs_weights, age, qsmk, .weights = w_ate)

  expect_s3_class(p, "ggplot")
  # Should have 3 layers: points + abline + scale_color_discrete
  expect_equal(length(p$layers), 2)

  # Check that data has both observed and weighted
  plot_data <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(plot_data), 198) # 99 quantiles * 2 methods
})

test_that("plot_qq works with multiple weights", {
  p <- plot_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))

  expect_s3_class(p, "ggplot")

  # Check that data has observed + 2 weighted methods
  plot_data <- ggplot2::layer_data(p, 1)
  expect_equal(nrow(plot_data), 297) # 99 quantiles * 3 methods
})

test_that("plot_qq works without observed", {
  p <- plot_qq(
    nhefs_weights,
    age,
    qsmk,
    .weights = w_ate,
    include_observed = FALSE
  )

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

test_that("plot_qq validates missing arguments", {
  expect_halfmoon_error(
    plot_qq(nhefs_weights),
    "halfmoon_arg_error"
  )

  expect_halfmoon_error(
    plot_qq(nhefs_weights, age),
    "halfmoon_arg_error"
  )
})

test_that("plot_qq errors with missing columns", {
  expect_halfmoon_error(
    plot_qq(nhefs_weights, missing_var, qsmk),
    "halfmoon_column_error"
  )

  expect_halfmoon_error(
    plot_qq(nhefs_weights, age, missing_group),
    "halfmoon_column_error"
  )
})

test_that("plot_qq errors with non-binary groups", {
  # Create data with 3 groups
  df <- nhefs_weights
  df$three_groups <- rep(1:3, length.out = nrow(df))

  expect_halfmoon_error(
    plot_qq(df, age, three_groups),
    "halfmoon_group_error"
  )
})

test_that("plot_qq handles NA values", {
  # Add some NA values
  df <- nhefs_weights
  df$age[1:10] <- NA

  # Should work with na.rm = TRUE
  expect_no_error(plot_qq(df, age, qsmk, na.rm = TRUE))

  # Should error with na.rm = FALSE (default) when NAs are present
  expect_halfmoon_error(plot_qq(df, age, qsmk), "halfmoon_na_error")
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
    plot_qq(nhefs_weights, age, qsmk, .weights = w_ate)
  )

  # With multiple weights
  expect_doppelganger(
    "qq plot multiple weights",
    plot_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))
  )

  # Without observed
  expect_doppelganger(
    "qq plot no observed",
    plot_qq(
      nhefs_weights,
      age,
      qsmk,
      .weights = w_ate,
      include_observed = FALSE
    )
  )

  # With propensity score
  expect_doppelganger(
    "qq plot propensity score",
    plot_qq(nhefs_weights, .fitted, qsmk, .weights = w_ate)
  )
})

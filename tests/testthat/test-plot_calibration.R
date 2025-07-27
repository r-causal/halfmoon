library(ggplot2)

test_that("plot_calibration works with basic input", {
  # Basic functionality
  p <- plot_calibration(nhefs_weights, .fitted, qsmk)

  expect_s3_class(p, "ggplot")
  expect_gte(length(p$layers), 3) # calibration layers + abline

  # Check that the plot can be built
  expect_no_error(suppress_calibration_warnings(ggplot_build(p)))
})

test_that("plot_calibration works with quoted column names", {
  # Test with quoted column names
  p <- plot_calibration(nhefs_weights, ".fitted", "qsmk")

  expect_s3_class(p, "ggplot")
  expect_no_error(suppress_calibration_warnings(ggplot_build(p)))
})

test_that("plot_calibration works with different methods", {
  # Test breaks method
  p_breaks <- plot_calibration(nhefs_weights, .fitted, qsmk, method = "breaks")
  expect_s3_class(p_breaks, "ggplot")

  # Test logistic method
  p_logistic <- plot_calibration(
    nhefs_weights,
    .fitted,
    qsmk,
    method = "logistic"
  )
  expect_s3_class(p_logistic, "ggplot")

  # Test windowed method
  p_windowed <- plot_calibration(
    nhefs_weights,
    .fitted,
    qsmk,
    method = "windowed"
  )
  expect_s3_class(p_windowed, "ggplot")
})

test_that("plot_calibration works with treatment level specification", {
  # Test with explicit treatment level
  p <- plot_calibration(nhefs_weights, .fitted, qsmk, treatment_level = "1")

  expect_s3_class(p, "ggplot")
  expect_no_error(suppress_calibration_warnings(ggplot_build(p)))
})

test_that("plot_calibration works with rug option", {
  # Test with rug disabled (default)
  p_no_rug <- plot_calibration(
    nhefs_weights,
    .fitted,
    qsmk,
    include_rug = FALSE
  )
  expect_s3_class(p_no_rug, "ggplot")

  # Test with rug enabled
  p_rug <- plot_calibration(nhefs_weights, .fitted, qsmk, include_rug = TRUE)
  expect_s3_class(p_rug, "ggplot")

  # Rug plot should have one more layer
  expect_gt(length(p_rug$layers), length(p_no_rug$layers))
})

test_that("plot_calibration works with ribbon and points options", {
  # Test with ribbon disabled
  p_no_ribbon <- plot_calibration(
    nhefs_weights,
    .fitted,
    qsmk,
    include_ribbon = FALSE
  )
  expect_s3_class(p_no_ribbon, "ggplot")

  # Test with points disabled
  p_no_points <- plot_calibration(
    nhefs_weights,
    .fitted,
    qsmk,
    include_points = FALSE
  )
  expect_s3_class(p_no_points, "ggplot")

  # Test with both disabled
  p_minimal <- plot_calibration(
    nhefs_weights,
    .fitted,
    qsmk,
    include_ribbon = FALSE,
    include_points = FALSE
  )
  expect_s3_class(p_minimal, "ggplot")
})

test_that("plot_calibration works with different bin counts", {
  # Test with different bin counts
  p_5bins <- plot_calibration(nhefs_weights, .fitted, qsmk, bins = 5)
  expect_s3_class(p_5bins, "ggplot")

  p_15bins <- plot_calibration(nhefs_weights, .fitted, qsmk, bins = 15)
  expect_s3_class(p_15bins, "ggplot")
})

test_that("plot_calibration works with different confidence levels", {
  # Test with different confidence levels
  p_90 <- plot_calibration(nhefs_weights, .fitted, qsmk, conf_level = 0.90)
  expect_s3_class(p_90, "ggplot")

  p_99 <- plot_calibration(nhefs_weights, .fitted, qsmk, conf_level = 0.99)
  expect_s3_class(p_99, "ggplot")
})

test_that("plot_calibration handles NA values", {
  # Create test data with NAs
  test_data <- nhefs_weights[1:100, ]
  test_data$.fitted[1:5] <- NA
  test_data$qsmk[6:10] <- NA

  # Test with na.rm = TRUE
  p_na_rm <- plot_calibration(test_data, .fitted, qsmk, na.rm = TRUE)
  expect_s3_class(p_na_rm, "ggplot")
  expect_no_error(suppress_calibration_warnings(ggplot_build(p_na_rm)))

  # Test with na.rm = FALSE
  p_na_keep <- suppress_calibration_warnings(plot_calibration(test_data, .fitted, qsmk, na.rm = FALSE))
  expect_s3_class(p_na_keep, "ggplot")
  expect_no_error(suppressWarnings(ggplot_build(p_na_keep)))
})

test_that("plot_calibration has correct labels and theme", {
  p <- plot_calibration(nhefs_weights, .fitted, qsmk)

  # Check labels
  expect_equal(p$labels$x, "predicted probability")
  expect_equal(p$labels$y, "observed rate")

  # Check that it has coord_cartesian
  expect_true(any(
    sapply(p$layers, function(x) inherits(x, "CoordCartesian")) |
      inherits(p$coordinates, "CoordCartesian")
  ))
})

test_that("plot_calibration visual snapshot tests", {
  # Skip on CI to avoid platform-specific rendering differences
  skip_on_ci()

  # Basic plot
  p1 <- plot_calibration(nhefs_weights, .fitted, qsmk)
  expect_doppelganger("plot_calibration basic", p1)

  # With rug
  p2 <- plot_calibration(nhefs_weights, .fitted, qsmk, include_rug = TRUE)
  expect_doppelganger("plot_calibration with rug", p2)

  # Logistic method
  p3 <- plot_calibration(nhefs_weights, .fitted, qsmk, method = "logistic")
  expect_doppelganger("plot_calibration logistic", p3)

  # Windowed method
  p4 <- plot_calibration(nhefs_weights, .fitted, qsmk, method = "windowed")
  expect_doppelganger("plot_calibration windowed", p4)

  # With explicit treatment level
  p5 <- plot_calibration(nhefs_weights, .fitted, qsmk, treatment_level = "1")
  expect_doppelganger("plot_calibration explicit treatment", p5)
})

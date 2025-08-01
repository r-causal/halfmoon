library(ggplot2)

test_that("plot_mirror_distributions works with density plots", {
  # Basic density plot
  p_density <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    type = "density"
  )

  expect_s3_class(p_density, "ggplot")
  expect_doppelganger("basic density plot", p_density)
})

test_that("plot_mirror_distributions works with histograms", {
  # Basic histogram (default)
  p_hist <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    bins = 20
  )

  expect_s3_class(p_hist, "ggplot")
  expect_doppelganger("basic histogram", p_hist)
})

test_that("plot_mirror_distributions works with weights", {
  # Weighted histogram (default)
  p_weighted <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    .wts = w_ate,
    bins = 20
  )

  expect_s3_class(p_weighted, "ggplot")
  expect_doppelganger("weighted histogram default", p_weighted)

  # Weighted density
  p_weighted_density <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    .wts = w_ate,
    type = "density"
  )

  expect_doppelganger("weighted density", p_weighted_density)
})

test_that("plot_mirror_distributions works with multiple weights", {
  # Multiple weights
  p_multi <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    .wts = c(w_ate, w_att),
    bins = 20
  )

  expect_s3_class(p_multi, "ggplot")
  expect_doppelganger("multiple weights", p_multi)
})

test_that("plot_mirror_distributions works without unweighted", {
  # Without unweighted
  p_no_unweighted <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    .wts = w_ate,
    include_unweighted = FALSE,
    bins = 20
  )

  expect_s3_class(p_no_unweighted, "ggplot")
  expect_doppelganger("no unweighted", p_no_unweighted)
})

test_that("plot_mirror_distributions handles custom parameters", {
  # Custom bandwidth (density)
  p_custom_bw <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    type = "density",
    bw = 5,
    adjust = 1.5
  )

  expect_doppelganger("custom bandwidth", p_custom_bw)

  # Custom bins
  p_custom_bins <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    type = "histogram",
    binwidth = 5
  )

  expect_doppelganger("custom binwidth", p_custom_bins)
})

test_that("plot_mirror_distributions handles custom aesthetics", {
  # Custom alpha
  p_custom_aes <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    alpha = 0.8,
    bins = 20
  )

  expect_doppelganger("custom aesthetics", p_custom_aes)
})

test_that("plot_mirror_distributions handles mirror axis", {
  # Mirror on x-axis (horizontal)
  p_mirror_x <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    mirror_axis = "x",
    bins = 20
  )

  expect_doppelganger("mirror x-axis", p_mirror_x)
})

test_that("plot_mirror_distributions handles NA values", {
  # Create data with NA
  df_with_na <- nhefs_weights
  df_with_na$age[1:10] <- NA

  # Should error without na.rm
  expect_error(
    plot_mirror_distributions(df_with_na, age, qsmk),
    "missing values"
  )

  # Should work with na.rm
  p_na_rm <- plot_mirror_distributions(
    df_with_na,
    age,
    qsmk,
    na.rm = TRUE
  )

  expect_s3_class(p_na_rm, "ggplot")
})

test_that("plot_mirror_distributions validates inputs", {
  # Non-existent column
  expect_error(
    plot_mirror_distributions(nhefs_weights, nonexistent, qsmk),
    "not found in"
  )

  # Group with >2 levels
  expect_error(
    plot_mirror_distributions(nhefs_weights, age, education),
    "exactly two levels"
  )
})

test_that("plot_mirror_distributions works with quoted column names", {
  # Quoted names
  p_quoted <- plot_mirror_distributions(
    nhefs_weights,
    "age",
    "qsmk"
  )

  expect_s3_class(p_quoted, "ggplot")
})

test_that("plot_mirror_distributions works with different data types", {
  # Continuous variable
  p_continuous <- plot_mirror_distributions(
    nhefs_weights,
    .fitted,
    qsmk,
    type = "density"
  )

  expect_doppelganger("continuous variable", p_continuous)

  # Different bandwidth methods
  p_bw_method <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    type = "density",
    bw = "sj"
  )

  expect_doppelganger("bandwidth method sj", p_bw_method)
})

test_that("plot_mirror_distributions produces correct plot structure", {
  # Check basic plot structure
  p <- plot_mirror_distributions(nhefs_weights, age, qsmk, bins = 20)

  # Build the plot
  built <- ggplot_build(p)

  # Should have one layer (the mirrored density)
  expect_length(built$data, 1)

  # Check that y-axis uses absolute values
  y_labels <- built$layout$panel_params[[1]]$y$get_labels()
  expect_true(all(as.numeric(y_labels) >= 0))
})

test_that("plot_mirror_distributions works with faceting", {
  # With weights creates facets
  p_facet <- plot_mirror_distributions(
    nhefs_weights,
    age,
    qsmk,
    .wts = c(w_ate, w_att)
  )

  built <- ggplot_build(p_facet)

  # Should have multiple panels (facets)
  expect_true(length(built$layout$panel_params) > 1)
})

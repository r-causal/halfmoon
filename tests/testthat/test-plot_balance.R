test_that("plot_balance creates valid ggplot object", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "smd"
  )

  p <- plot_balance(balance_data)
  expect_s3_class(p, "ggplot")
})

test_that("plot_balance handles multiple metrics with faceting", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = c("smd", "vr", "ks")
  )

  p <- plot_balance(balance_data)
  expect_s3_class(p, "ggplot")

  # Check that faceting was applied
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_balance handles energy metric with NA variable", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "energy"
  )

  p <- plot_balance(balance_data)
  expect_s3_class(p, "ggplot")

  # Check that NA variable was replaced
  plot_data <- ggplot2::ggplot_build(p)$plot$data
  expect_false(any(is.na(plot_data$variable)))
  expect_true("overall (multivariate)" %in% plot_data$variable)
})

test_that("plot_balance applies absolute value to SMD when abs_smd = TRUE", {
  # Create data with negative SMD
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "smd"
  )

  p_abs <- plot_balance(balance_data, abs_smd = TRUE)
  p_no_abs <- plot_balance(balance_data, abs_smd = FALSE)

  # Extract the data from the plots
  data_abs <- ggplot2::ggplot_build(p_abs)$plot$data
  data_no_abs <- ggplot2::ggplot_build(p_no_abs)$plot$data

  # With abs_smd = TRUE, all SMD values should be non-negative
  expect_true(all(data_abs$estimate[data_abs$metric == "smd"] >= 0))

  # Without abs_smd, there might be negative values
  # (can't guarantee this in test data, but structure should differ)
  expect_s3_class(p_abs, "ggplot")
  expect_s3_class(p_no_abs, "ggplot")
})

test_that("plot_balance adds caption when abs_smd = TRUE", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "smd"
  )

  p_abs <- plot_balance(balance_data, abs_smd = TRUE)
  p_no_abs <- plot_balance(balance_data, abs_smd = FALSE)

  expect_equal(p_abs$labels$caption, "smd values shown as absolute values")
  expect_null(p_no_abs$labels$caption)
})

test_that("plot_balance handles different facet scales", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = c("smd", "vr")
  )

  p_free <- plot_balance(balance_data, facet_scales = "free")
  p_fixed <- plot_balance(balance_data, facet_scales = "fixed")

  expect_s3_class(p_free, "ggplot")
  expect_s3_class(p_fixed, "ggplot")

  # Check facet parameters
  expect_true(p_free$facet$params$free$x)
  expect_true(p_free$facet$params$free$y)
  expect_false(p_fixed$facet$params$free$x)
  expect_false(p_fixed$facet$params$free$y)
})

test_that("plot_balance adjusts x-axis for variance ratio", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "vr"
  )

  p <- plot_balance(balance_data)

  # Check that scale limits were adjusted
  # Can't directly test the limits, but ensure plot builds without error
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_balance validates input", {
  # Test with invalid input
  expect_error(
    plot_balance(data.frame(x = 1:5)),
    "Input must be output from check_balance"
  )

  # Test with non-data frame
  expect_error(
    plot_balance(list(variable = "x")),
    "must be a data frame"
  )
})

test_that("plot_balance passes geom_love parameters correctly", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "smd"
  )

  p <- plot_balance(
    balance_data,
    linewidth = 2,
    point_size = 5,
    vline_xintercept = 0.05,
    vline_color = "red",
    vlinewidth = 1.5
  )

  expect_s3_class(p, "ggplot")
  # The plot should build without error with custom parameters
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_balance handles multiple weighting methods", {
  balance_data <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = c(w_ate, w_att),
    .metrics = "smd"
  )

  p <- plot_balance(balance_data)
  expect_s3_class(p, "ggplot")

  # Check that multiple methods are represented
  plot_data <- ggplot2::ggplot_build(p)$plot$data
  expect_true(length(unique(plot_data$method)) > 1)
})

# Visual regression tests
test_that("plot_balance visual tests", {
  # Basic SMD plot
  balance_smd <- check_balance(
    nhefs_weights,
    c(age, education, race),
    qsmk,
    .wts = w_ate,
    .metrics = "smd"
  )

  expect_doppelganger(
    "balance-plot-basic-smd",
    plot_balance(balance_smd)
  )

  # Multiple metrics with faceting
  balance_multi <- check_balance(
    nhefs_weights,
    c(age, education, race),
    qsmk,
    .wts = c(w_ate, w_att),
    .metrics = c("smd", "vr", "ks")
  )

  expect_doppelganger(
    "balance-plot-multiple-metrics",
    plot_balance(balance_multi)
  )

  # Energy metric with multivariate label
  balance_energy <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = c("smd", "energy")
  )

  expect_doppelganger(
    "balance-plot-with-energy",
    plot_balance(balance_energy)
  )

  # Without absolute SMD
  expect_doppelganger(
    "balance-plot-no-abs-smd",
    plot_balance(balance_smd, abs_smd = FALSE)
  )

  # Fixed scales
  expect_doppelganger(
    "balance-plot-fixed-scales",
    plot_balance(balance_multi, facet_scales = "fixed")
  )

  # Variance ratio only (to test x-axis centering)
  balance_vr <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = "vr"
  )

  expect_doppelganger(
    "balance-plot-variance-ratio",
    plot_balance(balance_vr)
  )

  # Custom styling
  expect_doppelganger(
    "balance-plot-custom-style",
    plot_balance(
      balance_smd,
      linewidth = 1.5,
      point_size = 3,
      vline_xintercept = 0.05,
      vline_color = "red",
      vlinewidth = 1
    )
  )
})

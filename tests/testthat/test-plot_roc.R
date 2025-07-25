library(ggplot2)
test_that("plot_roc_curve works with basic inputs", {
  # Create test ROC data
  roc_data <- weighted_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )

  # Test basic plot
  p <- plot_roc_curve(roc_data)
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  # Check layers
  expect_true(length(p$layers) >= 2) # At least ROC curve and diagonal

  # Test without balance region
  p_no_region <- plot_roc_curve(roc_data, balance_region = FALSE)
  expect_s3_class(p_no_region, "gg")

  # Test single method
  roc_single <- weighted_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE
  )
  p_single <- plot_roc_curve(roc_single)
  expect_s3_class(p_single, "gg")
  # Single method should have no legend
  expect_equal(p_single$theme$legend.position, "none")
})

test_that("plot_roc_auc works with basic inputs", {
  # Create test AUC data
  auc_data <- weighted_roc_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )

  # Test basic plot
  p <- plot_roc_auc(auc_data)
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  # Check that balance_quality was added
  expect_true("balance_quality" %in% names(p$data))

  # Test without reference line
  p_no_ref <- plot_roc_auc(auc_data, ref_line = FALSE)
  expect_s3_class(p_no_ref, "gg")

  # Test with check_roc_balance output
  balance_data <- check_roc_balance(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )
  p_balance <- plot_roc_auc(balance_data)
  expect_s3_class(p_balance, "gg")
})

test_that("plot functions handle invalid inputs", {
  # Test with wrong data structure
  bad_data <- data.frame(x = 1:10, y = 1:10)

  expect_error(
    plot_roc_curve(bad_data),
    "must contain columns"
  )

  expect_error(
    plot_roc_auc(bad_data),
    "must contain columns"
  )

  # Test with non-data frame
  expect_error(
    plot_roc_curve("not a data frame"),
    "must be a data frame"
  )

  expect_error(
    plot_roc_auc("not a data frame"),
    "must be a data frame"
  )
})

test_that("geom_roc and stat_roc work", {
  # Basic usage
  p <- ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
    geom_roc()
  expect_s3_class(p, "gg")

  # With weights
  p_weighted <- ggplot(
    nhefs_weights,
    aes(x = .fitted, y = qsmk, weight = w_ate)
  ) +
    geom_roc()
  expect_s3_class(p_weighted, "gg")

  # Test stat_roc directly
  p_stat <- ggplot(
    nhefs_weights,
    aes(x = .fitted, y = qsmk)
  ) +
    stat_roc()
  expect_s3_class(p_stat, "gg")
})

test_that("plot_roc_curve customization works", {
  roc_data <- weighted_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate
  )

  # Test custom line width
  p_custom <- plot_roc_curve(
    roc_data,
    linewidth = 2,
    diagonal_color = "blue",
    diagonal_linetype = "solid",
    balance_alpha = 0.3
  )
  expect_s3_class(p_custom, "gg")
})

test_that("plot_roc_auc customization works", {
  auc_data <- weighted_roc_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate
  )

  # Test custom bar settings
  p_custom <- plot_roc_auc(
    auc_data,
    ref_color = "blue",
    bar_width = 0.8,
    bar_alpha = 0.5
  )
  expect_s3_class(p_custom, "gg")
})

test_that("balance quality categorization works correctly", {
  # Create data with known AUC values
  test_auc <- tibble::tibble(
    method = c("perfect", "good", "acceptable", "poor"),
    auc = c(0.5, 0.52, 0.58, 0.7)
  )

  p <- plot_roc_auc(test_auc)

  # Check balance quality assignments
  plot_data <- p$data
  expect_equal(plot_data$balance_quality[plot_data$auc == 0.5], "Good")
  expect_equal(plot_data$balance_quality[plot_data$auc == 0.52], "Good")
  expect_equal(plot_data$balance_quality[plot_data$auc == 0.58], "Acceptable")
  expect_equal(plot_data$balance_quality[plot_data$auc == 0.7], "Poor")
})

test_that("StatRoc handles edge cases", {
  # Test with non-binary outcome
  bad_data <- data.frame(
    x = runif(100),
    y = rep(c(1, 2, 3), length.out = 100) # Non-factor with 3 levels
  )

  # The error happens during build, not creation
  p_bad <- ggplot(bad_data, aes(x = x, y = y)) + stat_roc()
  expect_error(
    ggplot_build(p_bad),
    "must have exactly 2 unique values"
  )

  # Test with missing values
  na_data <- data.frame(
    x = c(runif(50), rep(NA, 10)),
    y = factor(c(rep("A", 30), rep("B", 30)))
  )

  # Should work with na.rm = TRUE (default)
  p_na <- ggplot(na_data, aes(x = x, y = y)) +
    stat_roc(na.rm = TRUE)
  expect_s3_class(p_na, "gg")
})

test_that("plot functions produce expected output structure", {
  # Generate plots
  roc_data <- weighted_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )
  auc_data <- weighted_roc_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )

  p_roc <- plot_roc_curve(roc_data)
  p_auc <- plot_roc_auc(auc_data)

  # Check axis labels
  expect_equal(p_roc$labels$x, "1 - Specificity (False Positive Rate)")
  expect_equal(p_roc$labels$y, "Sensitivity (True Positive Rate)")
  expect_equal(p_roc$labels$colour, "Method")

  expect_equal(p_auc$labels$x, "AUC")
  expect_equal(p_auc$labels$y, "Weighting Method")
  expect_equal(p_auc$labels$fill, "Balance Quality")

  # Check coordinate system for ROC plot
  expect_s3_class(p_roc$coordinates, "CoordFixed")

  # Check theme
  expect_s3_class(p_roc$theme, "theme")
  expect_s3_class(p_auc$theme, "theme")
})

# vdiffr tests
test_that("plot_roc_curve visual regression", {
  skip_on_ci()

  # Multiple methods
  roc_multi <- weighted_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att, w_atc)
  )

  expect_doppelganger(
    "roc-curve-multiple-methods",
    plot_roc_curve(roc_multi)
  )

  expect_doppelganger(
    "roc-curve-no-balance-region",
    plot_roc_curve(roc_multi, balance_region = FALSE)
  )

  # Single method
  roc_single <- weighted_roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE
  )

  expect_doppelganger(
    "roc-curve-single-method",
    plot_roc_curve(roc_single)
  )

  # Custom styling
  expect_doppelganger(
    "roc-curve-custom-style",
    plot_roc_curve(
      roc_multi,
      linewidth = 2,
      diagonal_color = "red",
      diagonal_linetype = "dotted",
      balance_alpha = 0.2
    )
  )
})

test_that("plot_roc_auc visual regression", {
  skip_on_ci()

  # Multiple methods
  auc_multi <- weighted_roc_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att, w_atc)
  )

  expect_doppelganger(
    "roc-auc-multiple-methods",
    plot_roc_auc(auc_multi)
  )

  expect_doppelganger(
    "roc-auc-no-reference",
    plot_roc_auc(auc_multi, ref_line = FALSE)
  )

  # Single method
  auc_single <- weighted_roc_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate
  )

  expect_doppelganger(
    "roc-auc-single-method",
    plot_roc_auc(auc_single)
  )

  # Custom styling
  expect_doppelganger(
    "roc-auc-custom-style",
    plot_roc_auc(
      auc_multi,
      ref_color = "blue",
      bar_width = 0.8,
      bar_alpha = 0.3
    )
  )

  # Test with varied AUC values for balance quality colors
  test_auc <- tibble::tibble(
    method = c("Good", "Acceptable", "Poor"),
    auc = c(0.51, 0.57, 0.75)
  )

  expect_doppelganger(
    "roc-auc-balance-colors",
    plot_roc_auc(test_auc)
  )
})

test_that("geom_roc visual regression", {
  skip_on_ci()

  # Basic geom_roc
  expect_doppelganger(
    "geom-roc-basic",
    ggplot(
      nhefs_weights,
      aes(x = .fitted, y = as.numeric(qsmk))
    ) +
      geom_roc() +
      theme_minimal()
  )

  # With weights
  expect_doppelganger(
    "geom-roc-weighted",
    ggplot(
      nhefs_weights,
      aes(x = .fitted, y = as.numeric(qsmk), weight = w_ate)
    ) +
      geom_roc(linewidth = 1.5, color = "blue") +
      theme_minimal()
  )

  # Multiple groups with different weights
  # First create long format data
  long_data <- tidyr::pivot_longer(
    nhefs_weights,
    cols = c(w_ate, w_att),
    names_to = "weight_type",
    values_to = "weight"
  )

  expect_doppelganger(
    "geom-roc-multiple-groups",
    ggplot(
      long_data,
      aes(
        x = .fitted,
        y = as.numeric(qsmk),
        weight = weight,
        color = weight_type
      )
    ) +
      geom_roc() +
      theme_minimal() +
      labs(color = "Weight Type")
  )
})

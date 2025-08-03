library(ggplot2)
test_that("plot_roc_curve works with basic inputs", {
  # Create test ROC data
  roc_data <- roc_curve(
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

  # Test single method
  roc_single <- roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    include_observed = TRUE
  )
  p_single <- plot_roc_curve(roc_single)
  expect_s3_class(p_single, "gg")
})

test_that("plot_roc_auc works with basic inputs", {
  # Create test AUC data
  auc_data <- check_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )

  # Test basic plot
  p <- plot_roc_auc(auc_data)
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  # Test without reference line
  p_no_ref <- plot_roc_auc(auc_data, ref_line = FALSE)
  expect_s3_class(p_no_ref, "gg")

  # Test with check_auc output
  balance_data <- check_auc(
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
    class = "halfmoon_column_error"
  )

  expect_error(
    plot_roc_auc(bad_data),
    class = "halfmoon_column_error"
  )

  # Test with non-data frame
  expect_error(
    plot_roc_curve("not a data frame"),
    class = "halfmoon_type_error"
  )

  expect_error(
    plot_roc_auc("not a data frame"),
    class = "halfmoon_type_error"
  )
})

test_that("plot_roc_curve customization works", {
  roc_data <- roc_curve(
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
    diagonal_linetype = "solid"
  )
  expect_s3_class(p_custom, "gg")
})

test_that("plot_roc_auc customization works", {
  auc_data <- check_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    w_ate
  )

  # Test custom point settings
  p_custom <- plot_roc_auc(
    auc_data,
    ref_color = "blue",
    point_size = 5,
    point_shape = 21
  )
  expect_s3_class(p_custom, "gg")
})


test_that("StatRoc handles edge cases", {
  # Test with non-binary outcome
  bad_data <- data.frame(
    x = runif(100),
    y = rep(c(1, 2, 3), length.out = 100) # Non-factor with 3 levels
  )

  # The error happens during build, not creation
  p_bad <- ggplot(bad_data, aes(estimate = x, truth = y)) + stat_roc()
  expect_error(
    ggplot_build(p_bad),
    class = "halfmoon_group_error"
  )

  # Test with missing values
  na_data <- data.frame(
    x = c(runif(50), rep(NA, 10)),
    y = factor(c(rep("A", 30), rep("B", 30)))
  )

  # Should work with na.rm = TRUE (default)
  p_na <- ggplot(na_data, aes(estimate = x, truth = y)) +
    stat_roc(na.rm = TRUE)
  expect_s3_class(p_na, "gg")
})

test_that("plot functions produce expected output structure", {
  # Generate plots
  roc_data <- roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )
  auc_data <- check_auc(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att)
  )

  p_roc <- plot_roc_curve(roc_data)
  p_auc <- plot_roc_auc(auc_data)

  # Check axis labels
  expect_equal(p_roc$labels$x, "1 - specificity")
  expect_equal(p_roc$labels$y, "sensitivity")
  expect_equal(p_roc$labels$colour, "method")

  expect_equal(p_auc$labels$x, "auc")
  expect_equal(p_auc$labels$y, "method")

  # Check coordinate system for ROC plot
  expect_s3_class(p_roc$coordinates, "CoordFixed")
})

# vdiffr tests
test_that("plot_roc_curve visual regression", {
  skip_on_ci()

  # Multiple methods
  roc_multi <- roc_curve(
    nhefs_weights,
    qsmk,
    .fitted,
    c(w_ate, w_att, w_atc)
  )

  expect_doppelganger(
    "roc-curve-multiple-methods",
    plot_roc_curve(roc_multi)
  )

  # Single method
  roc_single <- roc_curve(
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
      diagonal_linetype = "dotted"
    )
  )
})

test_that("plot_roc_auc visual regression", {
  skip_on_ci()

  # Multiple methods
  auc_multi <- check_auc(
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
  auc_single <- check_auc(
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
      point_size = 5,
      point_shape = 21
    )
  )

  # Test with varied AUC values
  test_auc <- tibble::tibble(
    method = c("Good", "Acceptable", "Poor"),
    auc = c(0.51, 0.57, 0.75)
  )

  expect_doppelganger(
    "roc-auc-balance-colors",
    plot_roc_auc(test_auc)
  )
})

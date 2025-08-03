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
  expect_halfmoon_error(
    plot_balance(data.frame(x = 1:5)),
    "halfmoon_column_error"
  )

  # Test with non-data frame
  expect_halfmoon_error(
    plot_balance(list(variable = "x")),
    "halfmoon_type_error"
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

test_that("plot_balance handles categorical exposures with facet_grid", {
  # Create categorical exposure balance data
  balance_cat <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = c("smd", "vr")
  )
  
  p <- plot_balance(balance_cat)
  expect_s3_class(p, "ggplot")
  
  # Check that facet_grid was applied for categorical exposure
  expect_s3_class(p$facet, "FacetGrid")
  
  # Check that both group_level and metric are in the facet
  facet_vars <- names(p$facet$params$rows)
  expect_true("group_level" %in% facet_vars)
  facet_vars_cols <- names(p$facet$params$cols)
  expect_true("metric" %in% facet_vars_cols)
})

test_that("plot_balance handles categorical exposures with single metric", {
  # Single metric should facet by group_level only
  balance_cat_single <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = "smd"
  )
  
  p <- plot_balance(balance_cat_single)
  expect_s3_class(p, "ggplot")
  
  # Should use facet_wrap for single metric
  expect_s3_class(p$facet, "FacetWrap")
})

test_that("plot_balance correctly identifies categorical vs binary exposures", {
  # Binary exposure should not trigger categorical behavior
  balance_binary <- check_balance(
    nhefs_weights,
    c(age, education),
    qsmk,
    .wts = w_ate,
    .metrics = c("smd", "vr")
  )
  
  p_binary <- plot_balance(balance_binary)
  
  # Should use facet_wrap for binary exposure
  expect_s3_class(p_binary$facet, "FacetWrap")
  
  # Categorical exposure
  balance_cat <- check_balance(
    nhefs_weights,
    c(age, education),
    alcoholfreq_cat,
    .metrics = c("smd", "vr")
  )
  
  p_cat <- plot_balance(balance_cat)
  
  # Should use facet_grid for categorical exposure with multiple metrics
  expect_s3_class(p_cat$facet, "FacetGrid")
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
  
  # Categorical exposure with multiple metrics
  balance_cat_multi <- check_balance(
    nhefs_weights,
    c(age, wt71, sex),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = c("smd", "vr", "ks")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-multi-metric",
    plot_balance(balance_cat_multi)
  )
  
  # Categorical exposure with single metric
  balance_cat_single <- check_balance(
    nhefs_weights,
    c(age, wt71, sex),
    alcoholfreq_cat,
    .wts = c(w_cat_ate, w_cat_att_2_3wk),
    .metrics = "smd"
  )
  
  expect_doppelganger(
    "balance-plot-categorical-single-metric",
    plot_balance(balance_cat_single)
  )
  
  # Categorical exposure without weights
  balance_cat_observed <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .metrics = c("smd", "vr")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-observed",
    plot_balance(balance_cat_observed)
  )
})

# Additional visual tests for categorical exposures
test_that("plot_balance visual tests - more categorical scenarios", {
  # Categorical with different reference group
  balance_cat_ref <- check_balance(
    nhefs_weights,
    c(age, wt71, education),
    alcoholfreq_cat,
    reference_group = "daily",
    .wts = w_cat_ate,
    .metrics = c("smd", "vr")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-ref-daily",
    plot_balance(balance_cat_ref)
  )
  
  # Categorical with energy metric included
  balance_cat_energy <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = c("smd", "energy")
  )
  
  # Suppress ggplot2's "Removed 2 rows containing missing values" warning
  # This happens when energy distance produces NA values for small sample sizes
  suppressWarnings(
    expect_doppelganger(
      "balance-plot-categorical-with-energy",
      plot_balance(balance_cat_energy)
    )
  )
  
  # Categorical with fixed scales
  balance_cat_fixed <- check_balance(
    nhefs_weights,
    c(age, wt71, sex),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = c("smd", "vr", "ks")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-fixed-scales",
    plot_balance(balance_cat_fixed, facet_scales = "fixed")
  )
  
  # Categorical without absolute SMD
  balance_cat_multi_test <- check_balance(
    nhefs_weights,
    c(age, wt71, sex),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = c("smd", "vr", "ks")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-no-abs-smd",
    plot_balance(balance_cat_multi_test, abs_smd = FALSE)
  )
  
  # Categorical with multiple ATT weights
  balance_cat_multi_att <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .wts = c(w_cat_att_none, w_cat_att_2_3wk, w_cat_att_daily),
    .metrics = "smd"
  )
  
  expect_doppelganger(
    "balance-plot-categorical-multi-att",
    plot_balance(balance_cat_multi_att)
  )
  
  # Categorical with only KS metric
  balance_cat_ks <- check_balance(
    nhefs_weights,
    c(age, wt71, smokeintensity),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = "ks"
  )
  
  expect_doppelganger(
    "balance-plot-categorical-ks-only",
    plot_balance(balance_cat_ks)
  )
  
  # Categorical with custom vline for SMD only
  balance_cat_vline <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = "smd"
  )
  
  expect_doppelganger(
    "balance-plot-categorical-custom-vline",
    plot_balance(balance_cat_vline, vline_xintercept = 0.05, vline_color = "red")
  )
  
  # Categorical with many variables to test scrolling/layout
  balance_cat_many_vars <- check_balance(
    nhefs_weights,
    c(age, wt71, sex, race, education, smokeintensity, smokeyrs, exercise, active),
    alcoholfreq_cat,
    .wts = w_cat_ate,
    .metrics = c("smd", "vr")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-many-vars",
    plot_balance(balance_cat_many_vars)
  )
  
  # Categorical comparing observed vs multiple weights
  balance_cat_compare <- check_balance(
    nhefs_weights,
    c(age, wt71),
    alcoholfreq_cat,
    .wts = c(w_cat_ate, w_cat_ato, w_cat_atm),
    .metrics = c("smd", "vr")
  )
  
  expect_doppelganger(
    "balance-plot-categorical-compare-weights",
    plot_balance(balance_cat_compare)
  )
})

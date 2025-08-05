test_that("plot_ess creates a ggplot object", {
  p <- plot_ess(nhefs_weights, .wts = w_ate)
  
  expect_s3_class(p, "ggplot")
  expect_s3_class(p, "gg")
})

test_that("plot_ess works with raw data", {
  p <- plot_ess(nhefs_weights, .wts = c(w_ate, w_att))
  
  expect_s3_class(p, "ggplot")
  # Check that data was computed
  expect_true("ess_pct" %in% names(p$data))
})

test_that("plot_ess works with pre-computed ESS data", {
  ess_data <- check_ess(nhefs_weights, .wts = c(w_ate, w_att))
  p <- plot_ess(ess_data)
  
  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), nrow(ess_data))
})

test_that("plot_ess works without groups", {
  p <- plot_ess(nhefs_weights, .wts = c(w_ate, w_att))
  
  # Should have single fill color (no group aesthetic)
  expect_true(!"fill" %in% names(p$mapping))
  
  # Check layers
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomCol"))))
})

test_that("plot_ess works with groups", {
  p <- plot_ess(nhefs_weights, .wts = c(w_ate, w_att), .group = qsmk)
  
  # Should have fill aesthetic for groups
  expect_true("fill" %in% names(p$mapping))
  
  # Check for dodged position
  col_layer <- p$layers[[which(sapply(p$layers, function(l) inherits(l$geom, "GeomCol")))]]
  expect_s3_class(col_layer$position, "PositionDodge")
})

test_that("plot_ess includes reference line", {
  p <- plot_ess(nhefs_weights, .wts = w_ate)
  
  # Check for horizontal line at 100
  hline_layer <- sapply(p$layers, function(l) inherits(l$geom, "GeomHline"))
  expect_true(any(hline_layer))
  
  # Check that line is at 100
  hline_idx <- which(hline_layer)[1]
  expect_equal(p$layers[[hline_idx]]$data$yintercept, 100)
})

test_that("plot_ess labels work correctly", {
  p_with_labels <- plot_ess(nhefs_weights, .wts = w_ate, show_labels = TRUE)
  p_without_labels <- plot_ess(nhefs_weights, .wts = w_ate, show_labels = FALSE)
  
  # Check for text layer
  text_layer_with <- sapply(p_with_labels$layers, function(l) inherits(l$geom, "GeomText"))
  text_layer_without <- sapply(p_without_labels$layers, function(l) inherits(l$geom, "GeomText"))
  
  expect_true(any(text_layer_with))
  expect_false(any(text_layer_without))
})

test_that("plot_ess y-axis uses percent scale", {
  p <- plot_ess(nhefs_weights, .wts = w_ate)
  
  # Check that y-axis has percent labels
  y_scale <- p$scales$get_scales("y")
  expect_true(is.function(y_scale$labels))
})

test_that("plot_ess handles continuous groups", {
  p <- plot_ess(nhefs_weights, .wts = w_ate, .group = age, n_tiles = 4)
  
  expect_s3_class(p, "ggplot")
  expect_true("fill" %in% names(p$mapping))
})

test_that("plot_ess customization works", {
  p <- plot_ess(
    nhefs_weights, 
    .wts = w_ate,
    fill_color = "red",
    alpha = 0.5,
    reference_line_color = "blue",
    reference_line_type = "solid"
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check fill color for non-grouped plot
  col_layer <- p$layers[[which(sapply(p$layers, function(l) inherits(l$geom, "GeomCol")))]]
  expect_equal(col_layer$aes_params$fill, "red")
  expect_equal(col_layer$aes_params$alpha, 0.5)
  
  # Check reference line
  hline_layer <- p$layers[[which(sapply(p$layers, function(l) inherits(l$geom, "GeomHline")))]]
  expect_equal(hline_layer$aes_params$colour, "blue")
  expect_equal(hline_layer$aes_params$linetype, "solid")
})

test_that("plot_ess has correct labels", {
  p <- plot_ess(nhefs_weights, .wts = w_ate)
  
  expect_equal(p$labels$x, "method")
  expect_equal(p$labels$y, "effective sample size (%)")
  expect_equal(p$labels$subtitle, "Higher values indicate less weight variability")
})

test_that("plot_ess y-limits are appropriate", {
  # Create data with low ESS
  test_df <- data.frame(
    wts = c(10, 0.1, 0.1, 0.1)  # Very unequal weights
  )
  
  p <- plot_ess(test_df, .wts = wts, include_observed = FALSE)
  
  # Y-axis should start at 0 and go above the max value
  y_scale <- p$scales$get_scales("y")
  expect_equal(y_scale$limits[1], 0)
  expect_true(y_scale$limits[2] >= 105) # At least to 105%
})

test_that("plot_ess snapshot tests", {
  # Basic plot
  expect_doppelganger(
    "plot_ess_basic",
    plot_ess(nhefs_weights, .wts = c(w_ate, w_att))
  )
  
  # Plot with groups
  expect_doppelganger(
    "plot_ess_groups",
    plot_ess(nhefs_weights, .wts = c(w_ate, w_att), .group = qsmk)
  )
  
  # Plot without labels
  expect_doppelganger(
    "plot_ess_no_labels",
    plot_ess(nhefs_weights, .wts = c(w_ate, w_att), show_labels = FALSE)
  )
  
  # Plot with continuous groups
  expect_doppelganger(
    "plot_ess_continuous",
    plot_ess(nhefs_weights, .wts = w_ate, .group = age, n_tiles = 3)
  )
})
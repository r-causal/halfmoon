library(ggplot2)

test_that("geom_mirrored_density works with basic usage", {
  p <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02
    ) +
    geom_mirror_density(
      aes(fill = qsmk, weight = w_ate),
      bw = 0.02,
      alpha = 0.5
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("layered (weighted and unweighted)", p)
})

test_that("geom_mirrored_density works with different kernels", {
  p_gaussian <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      kernel = "gaussian",
      bw = 0.02
    )

  p_epanechnikov <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      kernel = "epanechnikov",
      bw = 0.02
    )

  expect_doppelganger("gaussian kernel", p_gaussian)
  expect_doppelganger("epanechnikov kernel", p_epanechnikov)
})

test_that("geom_mirrored_density works with different bandwidth methods", {
  p_nrd0 <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = "nrd0"
    )

  p_sj <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = "sj"
    )

  expect_doppelganger("nrd0 bandwidth", p_nrd0)
  expect_doppelganger("SJ bandwidth", p_sj)
})

test_that("geom_mirrored_density works with adjust parameter", {
  p_adjust <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02,
      adjust = 2
    )

  expect_doppelganger("adjust = 2", p_adjust)
})

test_that("geom_mirrored_density works with trimming", {
  p_trim <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02,
      trim = TRUE
    )

  expect_doppelganger("trimmed density", p_trim)
})

test_that("geom_mirrored_density works with custom aesthetics", {
  p_custom <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, color = qsmk),
      bw = 0.02,
      fill = NA,
      linewidth = 1.5
    )

  expect_doppelganger("custom aesthetics", p_custom)
})

test_that("geom_mirrored_density handles NA values correctly", {
  df_with_na <- nhefs_weights
  df_with_na$.fitted[1:10] <- NA

  p_na <- ggplot(df_with_na, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02,
      na.rm = TRUE
    )

  expect_doppelganger("handles NA values", p_na)
})

test_that("geom_mirrored_density errors with 3+ groups", {
  edu_group <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = education),
      bw = 0.02
    )

  expect_error(
    ggplot_build(edu_group),
    class = "halfmoon_group_error"
  )
})

test_that("geom_mirrored_density errors with no group", {
  no_group <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(bw = 0.02)

  expect_error(
    ggplot_build(no_group),
    class = "halfmoon_aes_error"
  )
})

test_that("geom_mirrored_density respects different y scales", {
  p_count <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, y = after_stat(count)),
      bw = 0.02
    ) +
    scale_y_continuous(labels = abs)

  p_scaled <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, y = after_stat(scaled)),
      bw = 0.02
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("count scale", p_count)
  expect_doppelganger("scaled density", p_scaled)
})

test_that("geom_mirrored_density works with faceting", {
  p_facet <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02
    ) +
    facet_wrap(~sex) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("with faceting", p_facet)
})

test_that("geom_mirrored_density preserves group structure correctly", {
  # Test that group 1 gets negative values
  p <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02
    )

  # Build the plot to access the data
  built <- ggplot_build(p)
  layer_data <- built$data[[1]]

  # Check that group 1 has negative density values
  group1_data <- layer_data[layer_data$group == 1, ]
  group2_data <- layer_data[layer_data$group == 2, ]

  expect_true(all(group1_data$density <= 0))
  expect_true(all(group2_data$density >= 0))
})

test_that("geom_mirrored_density works with fill aesthetic", {
  # Basic fill by group
  p_fill <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk),
      bw = 0.02
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("fill by group", p_fill)
})

test_that("geom_mirrored_density works with fill and alpha", {
  # Fill with transparency
  p_alpha <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk),
      bw = 0.02,
      alpha = 0.7
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("fill with alpha", p_alpha)
})

test_that("geom_mirrored_density works with custom fill colors", {
  # Custom fill scale
  p_custom_fill <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk),
      bw = 0.02
    ) +
    scale_fill_manual(values = c("0" = "#2166ac", "1" = "#b2182b")) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("custom fill colors", p_custom_fill)
})

test_that("geom_mirrored_density works with fill and color aesthetics", {
  # Both fill and color
  p_fill_color <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk, color = qsmk),
      bw = 0.02,
      alpha = 0.5,
      linewidth = 1
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("fill and color", p_fill_color)
})

test_that("geom_mirrored_density works with position adjustments", {
  # Test with position dodge
  p_position <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk),
      bw = 0.02,
      position = "identity",
      alpha = 0.8
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("position identity", p_position)
})

test_that("geom_mirrored_density works with multiple layers and fill", {
  # Multiple layers with different fills
  p_multi_fill <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk),
      bw = 0.02,
      fill = "gray80"
    ) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk, weight = w_ate),
      bw = 0.02,
      alpha = 0.6
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("multiple layers with fill", p_multi_fill)
})

test_that("geom_mirrored_density handles fill NA correctly", {
  # No fill (outline only)
  p_no_fill <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, color = qsmk),
      bw = 0.02,
      fill = NA,
      linewidth = 1.2
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("no fill outline only", p_no_fill)
})

test_that("geom_mirrored_density works with theme modifications", {
  # Fill with theme adjustments
  p_theme <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk),
      bw = 0.02
    ) +
    scale_y_continuous(labels = abs) +
    theme_minimal() +
    theme(legend.position = "bottom")

  expect_doppelganger("fill with theme", p_theme)
})

test_that("geom_mirrored_density works with gradient fills", {
  # Use factor levels for gradient effect
  nhefs_gradient <- nhefs_weights
  nhefs_gradient$treatment_group <- factor(
    paste0(nhefs_gradient$qsmk, "_", nhefs_gradient$sex),
    levels = c("0_Male", "0_Female", "1_Male", "1_Female")
  )

  p_gradient <- ggplot(nhefs_gradient, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = treatment_group),
      bw = 0.02,
      alpha = 0.7
    ) +
    scale_fill_brewer(palette = "RdBu") +
    scale_y_continuous(labels = abs)

  expect_doppelganger("gradient fills", p_gradient)
})

test_that("geom_mirrored_density fill behavior matches documentation", {
  # Test that fill correctly follows group structure
  p <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(
      aes(group = qsmk, fill = qsmk),
      bw = 0.02
    )

  built <- ggplot_build(p)
  layer_data <- built$data[[1]]

  # Check that each group has the correct fill
  unique_fills <- unique(layer_data[, c("group", "fill")])
  expect_equal(nrow(unique_fills), 2)

  # Verify fills are properly assigned
  group1_fill <- unique(layer_data[layer_data$group == 1, "fill"])
  group2_fill <- unique(layer_data[layer_data$group == 2, "fill"])

  expect_length(group1_fill, 1)
  expect_length(group2_fill, 1)
  expect_false(group1_fill == group2_fill)
})

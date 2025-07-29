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
    print(edu_group),
    "Groups of three or greater not supported"
  )
})

test_that("geom_mirrored_density errors with no group", {
  no_group <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_density(bw = 0.02)

  expect_error(
    print(no_group),
    "No group detected"
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

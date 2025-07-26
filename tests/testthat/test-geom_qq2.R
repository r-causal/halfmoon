test_that("geom_qq2 creates basic QQ plot", {
  p <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(sample = age, treatment = qsmk)
  ) +
    geom_qq2()

  expect_s3_class(p, "ggplot")
  expect_s3_class(p$layers[[1]]$stat, "StatQq2")
  expect_s3_class(p$layers[[1]]$geom, "GeomPoint")
})

test_that("stat_qq2 computes correct values", {
  p <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(sample = age, treatment = qsmk)
  ) +
    stat_qq2(quantiles = c(0.25, 0.5, 0.75))

  # Build the plot to access computed data
  built <- ggplot2::ggplot_build(p)
  data <- built$data[[1]]

  # Should have 3 points (one for each quantile)
  expect_equal(nrow(data), 3)

  # Should have x and y coordinates
  expect_true(all(c("x", "y") %in% names(data)))
})

test_that("geom_qq2 works with weights", {
  p <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(sample = age, treatment = qsmk, weight = w_ate)
  ) +
    geom_qq2()

  expect_s3_class(p, "ggplot")

  # Build to check it computes without error
  built <- ggplot2::ggplot_build(p)
  expect_true(nrow(built$data[[1]]) > 0)
})

test_that("geom_qq2 works with color aesthetic for multiple weights", {
  # Create long format data
  long_data <- tidyr::pivot_longer(
    nhefs_weights,
    cols = c(w_ate, w_att),
    names_to = "weight_type",
    values_to = "weight"
  )

  p <- ggplot2::ggplot(
    long_data,
    ggplot2::aes(sample = age, treatment = qsmk, weight = weight)
  ) +
    geom_qq2(ggplot2::aes(color = weight_type))

  built <- ggplot2::ggplot_build(p)
  data <- built$data[[1]]

  # Should have data for both weight types
  expect_true("colour" %in% names(data))
  expect_equal(length(unique(data$group)), 2)
})

test_that("geom_qq2 respects custom quantiles", {
  custom_q <- c(0.1, 0.5, 0.9)

  p <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(sample = age, treatment = qsmk)
  ) +
    geom_qq2(quantiles = custom_q)

  built <- ggplot2::ggplot_build(p)
  data <- built$data[[1]]

  # Should have 3 points
  expect_equal(nrow(data), length(custom_q))
})

test_that("plot_qq and geom_qq2 produce equivalent results", {
  # Using plot_qq
  p1 <- plot_qq(nhefs_weights, age, qsmk, include_observed = TRUE)

  # Using geom_qq2 directly
  p2 <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(sample = age, treatment = qsmk)
  ) +
    geom_qq2() +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.8
    ) +
    ggplot2::labs(
      x = "0 quantiles",
      y = "1 quantiles"
    )

  # Extract the data
  built1 <- ggplot2::ggplot_build(p1)
  built2 <- ggplot2::ggplot_build(p2)

  # Compare point data (first layer in both)
  data1 <- built1$data[[1]][, c("x", "y")]
  data2 <- built2$data[[1]][, c("x", "y")]

  expect_equal(data1, data2, tolerance = 1e-10)
})

# vdiffr visual regression tests
test_that("geom_qq2 visual regression tests", {
  # Basic geom_qq2
  expect_doppelganger(
    "geom_qq2 basic",
    ggplot2::ggplot(
      nhefs_weights,
      ggplot2::aes(sample = age, treatment = qsmk)
    ) +
      geom_qq2() +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  )

  # With weight
  expect_doppelganger(
    "geom_qq2 weighted",
    ggplot2::ggplot(
      nhefs_weights,
      ggplot2::aes(sample = age, treatment = qsmk, weight = w_ate)
    ) +
      geom_qq2() +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  )

  # With color for multiple weights
  long_data <- tidyr::pivot_longer(
    nhefs_weights,
    cols = c(w_ate, w_att),
    names_to = "weight_type",
    values_to = "weight"
  )

  expect_doppelganger(
    "geom_qq2 multiple weights",
    ggplot2::ggplot(
      long_data,
      ggplot2::aes(sample = age, treatment = qsmk, weight = weight)
    ) +
      geom_qq2(ggplot2::aes(color = weight_type)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  )

  # Custom quantiles
  expect_doppelganger(
    "geom_qq2 custom quantiles",
    ggplot2::ggplot(
      nhefs_weights,
      ggplot2::aes(sample = age, treatment = qsmk)
    ) +
      geom_qq2(quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9), size = 3) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  )
})

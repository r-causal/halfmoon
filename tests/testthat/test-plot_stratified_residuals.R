test_that("plot_stratified_residuals works with model input", {
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  y <- 2 * treatment + x + rnorm(n)

  model <- lm(y ~ treatment + x)

  # Basic plot should work
  p <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment
  )
  expect_s3_class(p, "gg")

  # Different plot types
  p_facet <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment,
    plot_type = "facet"
  )
  expect_s3_class(p_facet, "gg")

  p_both <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment,
    plot_type = "both"
  )
  expect_s3_class(p_both, "gg")
})

test_that("plot_stratified_residuals works with separate vectors", {
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  y <- 2 * treatment + x + rnorm(n)

  model <- lm(y ~ treatment + x)
  resids <- residuals(model)
  fitted_vals <- fitted(model)

  p <- plot_stratified_residuals(
    .residuals = resids,
    .ps_or_fitted = fitted_vals,
    .treatment = treatment
  )
  expect_s3_class(p, "gg")
})

test_that("plot_stratified_residuals validates inputs correctly", {
  # Missing required inputs
  expect_error(
    plot_stratified_residuals(),
    "Either"
  )

  expect_error(
    plot_stratified_residuals(.residuals = 1:10),
    "Either"
  )

  expect_error(
    plot_stratified_residuals(
      .residuals = 1:10,
      .ps_or_fitted = 1:10
    ),
    ".treatment"
  )

  # Non-numeric inputs
  expect_error(
    plot_stratified_residuals(
      .residuals = letters[1:10],
      .ps_or_fitted = 1:10,
      .treatment = rep(0:1, 5)
    ),
    "must be numeric"
  )

  # Wrong number of treatment levels
  expect_error(
    plot_stratified_residuals(
      .residuals = 1:10,
      .ps_or_fitted = 1:10,
      .treatment = rep(1:3, length.out = 10)
    ),
    "exactly two levels"
  )

  # Mismatched lengths
  expect_error(
    plot_stratified_residuals(
      .residuals = 1:10,
      .ps_or_fitted = 1:5,
      .treatment = rep(0:1, 5)
    ),
    "same length"
  )
})

test_that("plot_stratified_residuals handles NA values correctly", {
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  y <- 2 * treatment + x + rnorm(n)

  # Add some NAs
  y[c(10, 20, 30)] <- NA

  model <- lm(y ~ treatment + x, na.action = na.exclude)

  # Should work with na.rm = TRUE
  p <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment,
    na.rm = TRUE
  )
  expect_s3_class(p, "gg")
})

test_that("plot_stratified_residuals customization options work", {
  set.seed(123)
  n <- 100
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  y <- 2 * treatment + x + rnorm(n)

  model <- lm(y ~ treatment + x)

  # Test smooth = FALSE
  p_no_smooth <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment,
    smooth = FALSE
  )
  expect_s3_class(p_no_smooth, "gg")

  # Test custom x_label
  p_custom_label <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment,
    x_label = "Propensity score"
  )
  expect_s3_class(p_custom_label, "gg")
  expect_equal(p_custom_label$labels$x, "Propensity score")

  # Test custom alpha
  p_alpha <- plot_stratified_residuals(
    .model = model,
    .treatment = treatment,
    alpha = 0.5
  )
  expect_s3_class(p_alpha, "gg")
})

test_that("plot_stratified_residuals visual regression tests", {
  skip_if_not_installed("vdiffr")

  set.seed(123)
  n <- 200
  x <- rnorm(n)
  ps <- plogis(x)
  treatment <- rbinom(n, 1, ps)
  y1 <- 0.5 * x + rnorm(n)
  y0 <- -0.5 * x + rnorm(n)
  y <- treatment * y1 + (1 - treatment) * y0

  # Misspecified model
  model_wrong <- lm(y ~ treatment + x)

  # Correct model
  model_correct <- lm(y ~ treatment * x)

  expect_doppelganger(
    "stratified residuals color misspecified",
    plot_stratified_residuals(
      .model = model_wrong,
      .treatment = treatment,
      plot_type = "color"
    )
  )

  expect_doppelganger(
    "stratified residuals facet misspecified",
    plot_stratified_residuals(
      .model = model_wrong,
      .treatment = treatment,
      plot_type = "facet"
    )
  )

  expect_doppelganger(
    "stratified residuals both misspecified",
    plot_stratified_residuals(
      .model = model_wrong,
      .treatment = treatment,
      plot_type = "both"
    )
  )

  expect_doppelganger(
    "stratified residuals correct model",
    plot_stratified_residuals(
      .model = model_correct,
      .treatment = treatment,
      plot_type = "both"
    )
  )
})

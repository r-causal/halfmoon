test_that("plot_stratified_residuals.lm works with model input", {
  set.seed(8)
  n <- 500
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  # Simple additive model for basic tests
  y <- 2 * treatment + x + rnorm(n)

  model <- lm(y ~ treatment + x)

  # Basic plot should work
  p <- plot_stratified_residuals(
    model,
    treatment = treatment
  )
  expect_s3_class(p, "gg")

  # Different plot types
  p_facet <- plot_stratified_residuals(
    model,
    treatment = treatment,
    plot_type = "facet"
  )
  expect_s3_class(p_facet, "gg")

  p_both <- plot_stratified_residuals(
    model,
    treatment = treatment,
    plot_type = "both"
  )
  expect_s3_class(p_both, "gg")

  # Test with propensity score model
  ps_model <- glm(treatment ~ x, family = binomial)
  p_ps <- plot_stratified_residuals(
    model,
    treatment = treatment,
    ps_model = ps_model
  )
  expect_s3_class(p_ps, "gg")
})

test_that("plot_stratified_residuals.data.frame works", {
  set.seed(8)
  n <- 500
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  # Simple additive model
  y <- 2 * treatment + x + rnorm(n)

  model <- lm(y ~ treatment + x)
  ps_model <- glm(treatment ~ x, family = binomial)

  # Create data frame with all needed columns
  plot_data <- data.frame(
    trt = treatment,
    resids = residuals(model),
    fitted_vals = fitted(model),
    ps = fitted(ps_model)
  )

  # Test with fitted values
  p <- plot_stratified_residuals(
    plot_data,
    treatment = trt,
    residuals = resids,
    x_var = fitted_vals
  )
  expect_s3_class(p, "gg")

  # Test with propensity scores
  p_ps <- plot_stratified_residuals(
    plot_data,
    treatment = trt,
    residuals = resids,
    x_var = ps
  )
  expect_s3_class(p_ps, "gg")
  expect_equal(p_ps$labels$x, "Propensity score")

  # Test with string column names
  p_string <- plot_stratified_residuals(
    plot_data,
    treatment = "trt",
    residuals = "resids",
    x_var = "ps"
  )
  expect_s3_class(p_string, "gg")
})

test_that("plot_stratified_residuals validates inputs correctly", {
  # Test model method validations
  model <- lm(mpg ~ wt, data = mtcars)

  # Missing treatment
  expect_error(
    plot_stratified_residuals(model),
    "missing"
  )

  # Wrong ps_model type
  expect_error(
    plot_stratified_residuals(
      model,
      treatment = rep(0:1, 16),
      ps_model = "not a model"
    ),
    "must be a glm or lm object"
  )

  # Test data frame method validations
  df <- data.frame(
    trt = rep(0:1, 5),
    resids = rnorm(10),
    x = rnorm(10)
  )

  # Missing required arguments - need to provide all required args
  expect_error(
    plot_stratified_residuals(df),
    class = "rlang_error"
  )

  expect_error(
    plot_stratified_residuals(
      df,
      treatment = trt,
      residuals = resids
    ),
    "`x_var` must be a column name"
  )

  # Non-existent column
  expect_error(
    plot_stratified_residuals(
      df,
      treatment = trt,
      residuals = resids,
      x_var = not_a_column
    ),
    "not found"
  )

  # Wrong number of treatment levels
  df_wrong <- df
  df_wrong$trt <- rep(1:3, length.out = 10)
  expect_error(
    plot_stratified_residuals(
      df_wrong,
      treatment = trt,
      residuals = resids,
      x_var = x
    ),
    "exactly two levels"
  )
})

test_that("plot_stratified_residuals handles NA values correctly", {
  set.seed(8)
  n <- 500
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  y <- 2 * treatment + x + rnorm(n)

  # Add some NAs
  y[c(10, 20, 30)] <- NA

  model <- lm(y ~ treatment + x, na.action = na.exclude)

  # Should work with na.rm = TRUE
  p <- plot_stratified_residuals(
    model,
    treatment = treatment,
    na.rm = TRUE
  )
  expect_s3_class(p, "gg")
})

test_that("plot_stratified_residuals customization options work", {
  set.seed(8)
  n <- 500
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  y <- 2 * treatment + x + rnorm(n)

  model <- lm(y ~ treatment + x)

  # Test smooth = FALSE
  p_no_smooth <- plot_stratified_residuals(
    model,
    treatment = treatment,
    smooth = FALSE
  )
  expect_s3_class(p_no_smooth, "gg")

  # Test custom alpha
  p_alpha <- plot_stratified_residuals(
    model,
    treatment = treatment,
    alpha = 0.5
  )
  expect_s3_class(p_alpha, "gg")

  # Test x_label with ps_model
  ps_model <- glm(treatment ~ x, family = binomial)
  p_ps <- plot_stratified_residuals(
    model,
    treatment = treatment,
    ps_model = ps_model
  )
  expect_s3_class(p_ps, "gg")
  expect_equal(p_ps$labels$x, "Propensity score")
})

test_that("plot_stratified_residuals visual regression tests", {
  skip_if_not_installed("vdiffr")

  # Use same simulation as in documentation example
  set.seed(8)
  n <- 1000
  x <- rnorm(n)
  treatment <- rbinom(n, 1, plogis(x))
  # Create treatment effect heterogeneity
  y1 <- 0.5 * x + rnorm(n)
  y0 <- -0.5 * x + rnorm(n)
  y <- treatment * y1 + (1 - treatment) * y0

  # Misspecified model (missing interaction)
  model_wrong <- lm(y ~ treatment + x)

  # Correct model with interaction
  model_correct <- lm(y ~ treatment * x)

  # Test model method
  expect_doppelganger(
    "sr lm color wrong",
    plot_stratified_residuals(
      model_wrong,
      treatment = treatment,
      plot_type = "color"
    )
  )

  expect_doppelganger(
    "sr lm facet wrong",
    plot_stratified_residuals(
      model_wrong,
      treatment = treatment,
      plot_type = "facet"
    )
  )

  expect_doppelganger(
    "sr lm both wrong",
    plot_stratified_residuals(
      model_wrong,
      treatment = treatment,
      plot_type = "both"
    )
  )

  expect_doppelganger(
    "sr lm correct",
    plot_stratified_residuals(
      model_correct,
      treatment = treatment,
      plot_type = "both"
    )
  )

  # Test with propensity scores
  ps_model <- glm(treatment ~ x, family = binomial)

  expect_doppelganger(
    "sr lm ps wrong",
    plot_stratified_residuals(
      model_wrong,
      treatment = treatment,
      ps_model = ps_model,
      plot_type = "color"
    )
  )

  # Test data frame method
  plot_df <- data.frame(
    trt = treatment,
    resids = residuals(model_wrong),
    fitted_vals = fitted(model_wrong),
    ps = fitted(ps_model)
  )

  expect_doppelganger(
    "sr df fitted wrong",
    plot_stratified_residuals(
      plot_df,
      treatment = trt,
      residuals = resids,
      x_var = fitted_vals,
      plot_type = "color"
    )
  )

  expect_doppelganger(
    "sr df ps facet wrong",
    plot_stratified_residuals(
      plot_df,
      treatment = trt,
      residuals = resids,
      x_var = ps,
      plot_type = "facet"
    )
  )
})

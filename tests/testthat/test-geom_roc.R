library(ggplot2)

test_that("geom_roc and stat_roc work", {
  # Basic usage with factor outcome (qsmk is already a factor)
  p <- ggplot(nhefs_weights, aes(estimate = .fitted, exposure = qsmk)) +
    geom_roc()
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))

  # Test with numeric outcome
  qsmk_numeric <- as.numeric(nhefs_weights$qsmk) - 1 # Convert to 0/1
  p_numeric <- ggplot(
    nhefs_weights,
    aes(estimate = .fitted, exposure = qsmk_numeric)
  ) +
    geom_roc()
  expect_s3_class(p_numeric, "gg")
  expect_no_error(ggplot_build(p_numeric))

  # With weights
  p_weighted <- ggplot(
    nhefs_weights,
    aes(estimate = .fitted, exposure = qsmk, weight = w_ate)
  ) +
    geom_roc()
  expect_s3_class(p_weighted, "gg")
  expect_no_error(ggplot_build(p_weighted))

  # Test stat_roc directly
  p_stat <- ggplot(
    nhefs_weights,
    aes(estimate = .fitted, exposure = qsmk)
  ) +
    stat_roc()
  expect_s3_class(p_stat, "gg")
  expect_no_error(ggplot_build(p_stat))

  # Test with .focal_level parameter
  p_treatment <- ggplot(nhefs_weights, aes(estimate = .fitted, exposure = qsmk)) +
    geom_roc(.focal_level = "1")
  expect_s3_class(p_treatment, "gg")
  expect_no_error(ggplot_build(p_treatment))

  # Test stat_roc with .focal_level
  p_stat_treatment <- ggplot(
    nhefs_weights,
    aes(estimate = .fitted, exposure = qsmk)
  ) +
    stat_roc(.focal_level = "0")
  expect_s3_class(p_stat_treatment, "gg")
  expect_no_error(ggplot_build(p_stat_treatment))
})

test_that("geom_roc visual regression", {
  skip_on_ci()

  # Basic geom_roc
  expect_doppelganger(
    "geom-roc-basic",
    ggplot(
      nhefs_weights,
      aes(estimate = .fitted, exposure = qsmk)
    ) +
      geom_roc()
  )

  # With weights
  expect_doppelganger(
    "geom-roc-weighted",
    ggplot(
      nhefs_weights,
      aes(estimate = .fitted, exposure = as.numeric(qsmk), weight = w_ate)
    ) +
      geom_roc(linewidth = 1.5, color = "blue")
  )

  # Multiple groups with different weights
  # Create long format data
  long_data <- nhefs_weights |>
    dplyr::mutate(
      w_ate_num = as.numeric(w_ate),
      w_att_num = as.numeric(w_att)
    ) |>
    tidyr::pivot_longer(
      cols = c(w_ate_num, w_att_num),
      names_to = "weight_type",
      values_to = "weight"
    )

  expect_doppelganger(
    "geom-roc-multiple-groups",
    ggplot(
      long_data,
      aes(
        estimate = .fitted,
        exposure = qsmk,
        weight = weight,
        color = weight_type
      )
    ) +
      geom_roc() +
      labs(color = "Weight Type")
  )

  # Test with .focal_level parameter
  expect_doppelganger(
    "geom-roc-treatment-level-1",
    ggplot(
      nhefs_weights,
      aes(estimate = .fitted, exposure = qsmk)
    ) +
      geom_roc(.focal_level = "1", color = "red") +
      labs(title = "ROC with .focal_level = '1'")
  )

  expect_doppelganger(
    "geom-roc-treatment-level-0",
    ggplot(
      nhefs_weights,
      aes(estimate = .fitted, exposure = qsmk)
    ) +
      geom_roc(.focal_level = "0", color = "blue") +
      labs(title = "ROC with .focal_level = '0'")
  )
})

test_that("geom_roc works with both numeric and factor outcomes - visual", {
  skip_on_ci()

  # Test with factor outcome (qsmk is already a factor)
  p_factor <- ggplot(nhefs_weights, aes(estimate = .fitted, exposure = qsmk)) +
    geom_roc() +
    labs(title = "ROC with factor outcome")

  # Test with numeric outcome
  qsmk_numeric <- as.numeric(nhefs_weights$qsmk) - 1 # Convert to 0/1
  p_numeric <- ggplot(
    nhefs_weights,
    aes(estimate = .fitted, exposure = qsmk_numeric)
  ) +
    geom_roc() +
    labs(title = "ROC with numeric outcome")

  # Visual tests
  expect_doppelganger("roc-factor-outcome", p_factor)
  expect_doppelganger("roc-numeric-outcome", p_numeric)
})

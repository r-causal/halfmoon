test_that("geom_roc and stat_roc work", {
  # Basic usage
  p <- ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
    geom_roc()
  expect_s3_class(p, "gg")
  expect_no_error(ggplot_build(p))

  # With weights
  p_weighted <- ggplot(
    nhefs_weights,
    aes(x = .fitted, y = qsmk, weight = w_ate)
  ) +
    geom_roc()
  expect_s3_class(p_weighted, "gg")
  expect_no_error(ggplot_build(p_weighted))

  # Test stat_roc directly
  p_stat <- ggplot(
    nhefs_weights,
    aes(x = .fitted, y = qsmk)
  ) +
    stat_roc()
  expect_s3_class(p_stat, "gg")
  expect_no_error(ggplot_build(p_stat))
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
      geom_roc()
  )

  # With weights
  expect_doppelganger(
    "geom-roc-weighted",
    ggplot(
      nhefs_weights,
      aes(x = .fitted, y = as.numeric(qsmk), weight = w_ate)
    ) +
      geom_roc(linewidth = 1.5, color = "blue")
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
      labs(color = "Weight Type")
  )
})

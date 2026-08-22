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
  p_treatment <- ggplot(
    nhefs_weights,
    aes(estimate = .fitted, exposure = qsmk)
  ) +
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

test_that("stat_roc keeps an explicit group aesthetic separate", {
  long_data <- nhefs_weights |>
    dplyr::mutate(dplyr::across(c(w_ate, w_att), as.numeric)) |>
    tidyr::pivot_longer(
      cols = c(w_ate, w_att),
      names_to = "weight_type",
      values_to = "weight"
    )

  grouped <- ggplot(
    long_data,
    aes(
      estimate = .fitted,
      exposure = qsmk,
      weight = weight,
      group = weight_type
    )
  ) +
    geom_roc()
  coloured <- ggplot(
    long_data,
    aes(
      estimate = .fitted,
      exposure = qsmk,
      weight = weight,
      colour = weight_type
    )
  ) +
    geom_roc()

  grouped_data <- layer_data(grouped, 1)
  coloured_data <- layer_data(coloured, 1)

  expect_equal(length(unique(grouped_data$group)), 2)
  expect_equal(grouped_data[, c("x", "y")], coloured_data[, c("x", "y")])

  # Each curve sees one weighting scheme, so no subject is counted twice
  ate_only <- layer_data(
    ggplot(
      dplyr::filter(long_data, weight_type == "w_ate"),
      aes(estimate = .fitted, exposure = qsmk, weight = weight)
    ) +
      geom_roc(),
    1
  )

  expect_equal(
    grouped_data[grouped_data$group == 1, c("x", "y")],
    ate_only[, c("x", "y")],
    ignore_attr = TRUE
  )
})

test_that("stat_roc renders an exposure with unused factor levels", {
  df <- nhefs_weights
  df$qsmk_extra <- factor(
    as.character(df$qsmk),
    levels = c(levels(df$qsmk), "never")
  )
  df$qsmk_observed <- droplevels(df$qsmk_extra)

  with_unused <- layer_data(
    ggplot(df, aes(estimate = .fitted, exposure = qsmk_extra)) + geom_roc(),
    1
  )
  observed_only <- layer_data(
    ggplot(df, aes(estimate = .fitted, exposure = qsmk_observed)) + geom_roc(),
    1
  )

  expect_gt(nrow(with_unused), 0)
  expect_equal(with_unused[, c("x", "y")], observed_only[, c("x", "y")])
})

test_that("stat_roc skips a group without two observed exposure levels", {
  df <- nhefs_weights
  df$grp <- "both"
  df$grp[df$qsmk == 1][1:50] <- "one"

  p <- ggplot(df, aes(estimate = .fitted, exposure = qsmk, group = grp)) +
    geom_roc()

  expect_warning(
    built <- layer_data(p, 1),
    class = "halfmoon_data_warning"
  )

  # the group that can be compared is still drawn
  both_only <- layer_data(
    ggplot(
      dplyr::filter(df, grp == "both"),
      aes(estimate = .fitted, exposure = qsmk)
    ) +
      geom_roc(),
    1
  )

  expect_equal(length(unique(built$group)), 1)
  expect_equal(
    built[, c("x", "y")],
    both_only[, c("x", "y")],
    ignore_attr = TRUE
  )
})

test_that("stat_roc validates .focal_level against the observed levels", {
  absent <- ggplot(nhefs_weights, aes(estimate = .fitted, exposure = qsmk)) +
    geom_roc(.focal_level = "2")

  expect_error(
    ggplot_build(absent),
    class = "halfmoon_reference_error"
  )

  df <- nhefs_weights
  df$qsmk_extra <- factor(
    as.character(df$qsmk),
    levels = c(levels(df$qsmk), "never")
  )

  unused <- ggplot(df, aes(estimate = .fitted, exposure = qsmk_extra)) +
    geom_roc(.focal_level = "never")

  expect_error(
    ggplot_build(unused),
    class = "halfmoon_reference_error"
  )

  observed <- ggplot(nhefs_weights, aes(estimate = .fitted, exposure = qsmk)) +
    geom_roc(.focal_level = "1")

  expect_no_error(ggplot_build(observed))
})

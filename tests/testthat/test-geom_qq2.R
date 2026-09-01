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

test_that("stat_qq2 keeps weighting when some weights are missing", {
  probs <- c(0.25, 0.5, 0.75)
  df <- nhefs_weights
  df$w <- as.numeric(df$w_ate)
  df$w[1:5] <- NA
  complete <- df[!is.na(df$w), ]

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(sample = age, treatment = qsmk, weight = w)
  ) +
    geom_qq2(quantiles = probs)
  built <- ggplot2::layer_data(p, 1)

  expected <- check_qq(
    complete,
    age,
    qsmk,
    .weights = w,
    include_observed = FALSE,
    quantiles = probs
  )
  expect_equal(built$x, expected$unexposed_quantiles)
  expect_equal(built$y, expected$exposed_quantiles)

  # the weights are actually used, rather than silently discarded
  unweighted <- check_qq(complete, age, qsmk, quantiles = probs)
  expect_false(isTRUE(all.equal(built$y, unweighted$exposed_quantiles)))

  # with na.rm = FALSE, dropped rows are reported
  p_reported <- ggplot2::ggplot(
    df,
    ggplot2::aes(sample = age, treatment = qsmk, weight = w)
  ) +
    geom_qq2(quantiles = probs, na.rm = FALSE)
  expect_warning(ggplot2::ggplot_build(p_reported), "Removed 5 rows")
})

test_that("stat_qq2 keeps an explicit group aesthetic separate", {
  probs <- c(0.25, 0.5, 0.75)
  long_data <- nhefs_weights |>
    dplyr::mutate(dplyr::across(c(w_ate, w_att), as.numeric)) |>
    tidyr::pivot_longer(
      cols = c(w_ate, w_att),
      names_to = "weight_type",
      values_to = "weight"
    )

  grouped <- ggplot2::ggplot(
    long_data,
    ggplot2::aes(
      sample = age,
      treatment = qsmk,
      weight = weight,
      group = weight_type
    )
  ) +
    geom_qq2(quantiles = probs)
  coloured <- ggplot2::ggplot(
    long_data,
    ggplot2::aes(
      sample = age,
      treatment = qsmk,
      weight = weight,
      colour = weight_type
    )
  ) +
    geom_qq2(quantiles = probs)

  grouped_data <- ggplot2::layer_data(grouped, 1)
  coloured_data <- ggplot2::layer_data(coloured, 1)

  expect_equal(length(unique(grouped_data$group)), 2)
  expect_equal(nrow(grouped_data), 2 * length(probs))
  expect_equal(grouped_data[, c("x", "y")], coloured_data[, c("x", "y")])
})

test_that("stat_qq2 requires two observed treatment levels", {
  df <- nhefs_weights
  df$three_groups <- rep(1:3, length.out = nrow(df))

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(sample = age, treatment = three_groups)
  ) +
    geom_qq2()

  expect_error(
    ggplot2::ggplot_build(p),
    class = "halfmoon_group_error"
  )
})

test_that("stat_qq2 skips a group without two treatment levels", {
  probs <- c(0.25, 0.75)
  df <- nhefs_weights
  df$grp <- "both"
  df$grp[df$qsmk == 1][1:50] <- "one"

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(sample = age, treatment = qsmk, group = grp)
  ) +
    geom_qq2(quantiles = probs)

  expect_warning(
    built <- ggplot2::layer_data(p, 1),
    class = "halfmoon_data_warning"
  )

  # the group that can be compared is still drawn
  expected <- check_qq(
    df[df$grp == "both", ],
    age,
    qsmk,
    quantiles = probs
  )
  expect_equal(nrow(built), length(probs))
  expect_equal(built$x, expected$unexposed_quantiles)
  expect_equal(built$y, expected$exposed_quantiles)
})

test_that("the QQ functions agree on identical input", {
  probs <- c(0.1, 0.5, 0.9)

  from_bal <- bal_qq(
    nhefs_weights,
    age,
    qsmk,
    .weights = w_ate,
    quantiles = probs
  )
  from_check <- check_qq(
    nhefs_weights,
    age,
    qsmk,
    .weights = w_ate,
    include_observed = FALSE,
    quantiles = probs
  )
  from_geom <- ggplot2::layer_data(
    ggplot2::ggplot(
      nhefs_weights,
      ggplot2::aes(sample = age, treatment = qsmk, weight = w_ate)
    ) +
      geom_qq2(quantiles = probs),
    1
  )
  from_plot <- ggplot2::layer_data(
    plot_qq(
      nhefs_weights,
      age,
      qsmk,
      .weights = w_ate,
      include_observed = FALSE,
      quantiles = probs
    ),
    1
  )

  expect_equal(from_bal$exposed_quantiles, from_check$exposed_quantiles)
  expect_equal(from_bal$unexposed_quantiles, from_check$unexposed_quantiles)

  # the reference (unexposed) group is on the x axis in every plot
  expect_equal(from_geom$x, from_bal$unexposed_quantiles)
  expect_equal(from_geom$y, from_bal$exposed_quantiles)
  expect_equal(from_plot$x, from_bal$unexposed_quantiles)
  expect_equal(from_plot$y, from_bal$exposed_quantiles)
})

# vdiffr visual regression tests
test_that("geom_qq2 visual regression tests", {
  withr::local_seed(123)
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
  long_data <- nhefs_weights |>
    dplyr::mutate(dplyr::across(c(w_ate, w_att), as.numeric)) |>
    tidyr::pivot_longer(
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

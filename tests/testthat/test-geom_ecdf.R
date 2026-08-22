library(ggplot2)
test_that("geom_ecdf works", {
  p_no_wts <- ggplot(
    nhefs_weights,
    aes(x = smokeyrs, color = qsmk)
  ) +
    geom_ecdf() +
    xlab("Smoking Years") +
    ylab("Proportion <= x")

  p_wts <- ggplot(
    nhefs_weights,
    aes(x = smokeyrs, color = qsmk)
  ) +
    geom_ecdf(aes(weights = w_ato)) +
    xlab("Smoking Years") +
    ylab("Proportion <= x")

  expect_doppelganger("ecdf (no weights)", p_no_wts)
  expect_doppelganger("ecdf (weights)", p_wts)
})

test_that("geom_ecdf computes a weighted ECDF", {
  # sorted x is 1, 2, 2, 3 with weights 2, 3, 4, 1, so the weight at each
  # unique value is 2, 7, 1 out of a total of 10
  df <- data.frame(x = c(3, 1, 2, 2), w = c(1, 2, 3, 4))

  built <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), pad = FALSE),
    1
  )

  expect_equal(built$x, c(1, 2, 3))
  expect_equal(built$y, cumsum(c(2, 7, 1)) / 10)
})

test_that("geom_ecdf treats psw weights as their numeric values", {
  df <- data.frame(x = c(3, 1, 2, 2))
  df$w <- propensity::psw(c(1, 2, 3, 4), estimand = "ate")
  df$bare <- as.numeric(df$w)

  weighted <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w)),
    1
  )
  bare <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = bare)),
    1
  )

  expect_equal(weighted[, c("x", "y")], bare[, c("x", "y")])
})

test_that("geom_ecdf honors pad and n when weights are mapped", {
  df <- data.frame(x = c(1, 2, 2, 5, 7), w = 1)

  padded <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), pad = TRUE),
    1
  )
  expect_equal(padded$x[1], -Inf)
  expect_equal(padded$x[nrow(padded)], Inf)

  unpadded <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), pad = FALSE),
    1
  )
  expect_true(all(is.finite(unpadded$x)))

  subsampled <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), n = 3, pad = FALSE),
    1
  )
  expect_equal(nrow(subsampled), 3)
  expect_equal(subsampled$x, seq(1, 7, length.out = 3))
})

test_that("geom_ecdf with unit weights matches the unweighted ECDF", {
  df <- data.frame(x = c(1, 2, 2, 5, 7), w = 1)

  for (pad in c(TRUE, FALSE)) {
    weighted <- layer_data(
      ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), pad = pad),
      1
    )
    unweighted <- layer_data(
      ggplot(df, aes(x = x)) + stat_ecdf(pad = pad),
      1
    )
    expect_equal(weighted[, c("x", "y")], unweighted[, c("x", "y")])
  }

  weighted_n <- layer_data(
    ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), n = 4, pad = FALSE),
    1
  )
  unweighted_n <- layer_data(
    ggplot(df, aes(x = x)) + stat_ecdf(n = 4, pad = FALSE),
    1
  )
  expect_equal(weighted_n[, c("x", "y")], unweighted_n[, c("x", "y")])
})

test_that("geom_ecdf drops rows with a missing weight", {
  df <- data.frame(x = 1:4, w = c(1, NA, 1, 1))

  silent <- layer_data(
    ggplot(df, aes(x = x)) +
      geom_ecdf(aes(weights = w), pad = FALSE, na.rm = TRUE),
    1
  )
  expect_equal(silent$x, c(1, 3, 4))
  expect_equal(silent$y, c(1, 2, 3) / 3)

  expect_warning(
    reported <- layer_data(
      ggplot(df, aes(x = x)) + geom_ecdf(aes(weights = w), pad = FALSE),
      1
    ),
    "Removed 1 row"
  )
  expect_equal(reported[, c("x", "y")], silent[, c("x", "y")])
})

test_that("geom_ecdf drops a group whose weights sum to zero", {
  df <- data.frame(
    x = rep(1:4, 2),
    w = c(rep(0, 4), rep(1, 4)),
    grp = rep(c("zero", "one"), each = 4)
  )

  p <- ggplot(df, aes(x = x, colour = grp)) +
    geom_ecdf(aes(weights = w), pad = FALSE)

  expect_warning(
    built <- layer_data(p, 1),
    class = "halfmoon_data_warning"
  )

  # the group that can be computed is still drawn
  expect_equal(length(unique(built$group)), 1)
  expect_equal(built$y, c(1, 2, 3, 4) / 4)
})

test_that("geom_ecdf draws the same weighted curve in either orientation", {
  upright <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(x = smokeyrs, color = qsmk)
  ) +
    geom_ecdf(ggplot2::aes(weights = w_ato))

  flipped <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(y = smokeyrs, color = qsmk)
  ) +
    geom_ecdf(ggplot2::aes(weights = w_ato), orientation = "y")

  upright_data <- ggplot2::ggplot_build(upright)$data[[1]]
  flipped_data <- ggplot2::ggplot_build(flipped)$data[[1]]

  expect_equal(upright_data$x, flipped_data$y)
  expect_equal(upright_data$y, flipped_data$x)
  expect_true(all(flipped_data$flipped_aes))
})

test_that("geom_ecdf refuses negative weights", {
  mixed_sign <- nhefs_weights
  mixed_sign$w_signed <- ifelse(
    seq_len(nrow(mixed_sign)) %% 2 == 0,
    -mixed_sign$w_ate,
    mixed_sign$w_ate
  )

  p <- ggplot2::ggplot(mixed_sign, ggplot2::aes(x = smokeyrs)) +
    geom_ecdf(ggplot2::aes(weights = w_signed))

  expect_error(
    ggplot2::ggplot_build(p),
    class = "halfmoon_range_error"
  )
})

test_that("compute_weighted_ecdf aggregates ties by value", {
  # The aggregation this replaced: group the weights by distinct value and sum
  # each group. It is the reference the current cumulative form must match.
  split_weighted_ecdf <- function(x, weights, n = NULL, pad = TRUE) {
    total <- sum(weights)
    ordered <- order(x)
    x <- x[ordered]
    weights <- weights[ordered]

    values <- unique(x)
    cumulative <- cumsum(vapply(
      split(weights, match(x, values)),
      sum,
      numeric(1)
    ))

    grid <- if (is.null(n)) values else seq(min(x), max(x), length.out = n)
    if (pad) {
      grid <- c(-Inf, grid, Inf)
    }

    ecdf <- if (length(values) == 1) {
      ifelse(grid < values, 0, 1)
    } else {
      stats::approxfun(
        values,
        cumulative / total,
        method = "constant",
        yleft = 0,
        yright = 1,
        f = 0,
        ties = "ordered"
      )(grid)
    }

    data.frame(x = grid, ecdf = ecdf)
  }

  withr::local_seed(2024)
  x <- stats::rnorm(200)
  weights <- stats::runif(200, 0, 3)
  tied <- round(x, 1)

  cases <- list(
    default = list(x = x, weights = weights),
    grid = list(x = x, weights = weights, n = 25),
    unpadded = list(x = x, weights = weights, pad = FALSE),
    heavy_ties = list(x = tied, weights = weights),
    heavy_ties_grid = list(x = tied, weights = weights, n = 25),
    zero_weights = list(
      x = tied,
      weights = replace(weights, seq(1, 200, by = 5), 0)
    ),
    single_value = list(x = rep(3, 10), weights = weights[1:10]),
    single_observation = list(x = 3, weights = 1)
  )

  for (nm in names(cases)) {
    expect_equal(
      do.call(compute_weighted_ecdf, cases[[nm]]),
      do.call(split_weighted_ecdf, cases[[nm]]),
      info = nm
    )
  }
})

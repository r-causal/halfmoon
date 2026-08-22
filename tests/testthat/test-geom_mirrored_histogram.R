library(ggplot2)
test_that("geom_mirrored_histogram works", {
  p <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_histogram(
      aes(group = qsmk),
      bins = 50
    ) +
    geom_mirror_histogram(
      aes(fill = qsmk, weight = w_ate),
      bins = 50,
      alpha = 0.5
    ) +
    scale_y_continuous(labels = abs)

  expect_doppelganger("layered (weighted and unweighted)", p)
})

test_that("geom_mirrored_histogram errors correctly", {
  # group of 3 or more
  edu_group <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_histogram(
      aes(group = education),
      bins = 50
    )

  expect_error(
    ggplot_build(edu_group),
    class = "halfmoon_group_error"
  )

  # no group
  no_group <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_histogram(bins = 50)

  expect_snapshot_warning(ggplot_build(no_group))
})

test_that("NO_GROUP is still -1", {
  skip_on_cran()
  expect_equal(asNamespace("ggplot2")$NO_GROUP, -1)
})

test_that("geom_mirror_histogram mirrors every computed statistic", {
  built <- ggplot2::layer_data(
    ggplot2::ggplot(nhefs_weights, ggplot2::aes(.fitted)) +
      geom_mirror_histogram(ggplot2::aes(group = qsmk), bins = 10),
    1
  )

  for (statistic in c("count", "density", "ncount", "ndensity")) {
    mirrored <- built[[statistic]][built$group == 1]
    upright <- built[[statistic]][built$group == 2]

    expect_true(all(mirrored <= 0), info = statistic)
    expect_true(any(mirrored < 0), info = statistic)
    expect_true(all(upright >= 0), info = statistic)
  }
})

test_that("geom_mirror_histogram mirrors a statistic chosen with after_stat()", {
  built <- ggplot2::layer_data(
    ggplot2::ggplot(nhefs_weights, ggplot2::aes(.fitted)) +
      geom_mirror_histogram(
        ggplot2::aes(group = qsmk, y = ggplot2::after_stat(density)),
        bins = 10
      ),
    1
  )

  expect_lt(min(built$ymin), 0)
  expect_gt(max(built$ymax), 0)
})

test_that("geom_mirror_histogram names the geom and the group count", {
  p <- ggplot2::ggplot(
    nhefs_weights,
    ggplot2::aes(.fitted, group = alcoholfreq_cat)
  ) +
    geom_mirror_histogram(bins = 20)

  expect_snapshot(error = TRUE, cnd_class = TRUE, ggplot2::ggplot_build(p))
})

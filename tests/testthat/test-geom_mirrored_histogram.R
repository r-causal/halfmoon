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

test_that("geom_mirrored_histogram errors/warns correctly", {
  # group of 3 or more
  edu_group <- ggplot(nhefs_weights, aes(.fitted)) +
    geom_mirror_histogram(
      aes(group = education),
      bins = 50
    )
  expect_snapshot_warning(print(edu_group))

  # no group
  no_group <- ggplot(nhefs_weights, aes(.fitted)) +
      geom_mirror_histogram(bins = 50)

  expect_snapshot_warning(print(no_group))
})

test_that("NO_GROUP is still -1", {
  skip_on_cran()
  expect_equal(asNamespace("ggplot2")$NO_GROUP, -1)
})

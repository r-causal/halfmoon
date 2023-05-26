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


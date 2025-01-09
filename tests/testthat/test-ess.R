test_that("ess returns correct result for equal weights", {
  # 5 observations, each weight = 2
  wts_equal <- rep(2, 5)
  # ESS should be 5
  expect_equal(ess(wts_equal), 5)
})

test_that("ess returns correct result for varied weights", {
  # 5 observations, each weight varies
  wts_equal <- runif(5, max = 5)
  # ESS should always be less than 5
  expect_lt(ess(wts_equal), 5)
})

test_that("ess handles one large weight", {
  # 5 observations, 1 large weight
  wts_big <- c(1000, rep(0, 4))
  # The sum is 1000, sum of squares is 1,000^2 = 1e6
  # ESS = (1000^2) / 1,000^2 = 1
  expect_equal(ess(wts_big), 1)
})

test_that("ess gives `NaN` if all weights are 0", {
  wts_zero <- rep(0, 5)
  # sum(wts) = 0, sum(wts^2) = 0 -> 0/0 is NaN
  expect_true(is.nan(ess(wts_zero)))
})

test_that("calculate_prop_ci works correctly", {
  # Normal case
  ci <- calculate_prop_ci(50, 100, 0.95)
  expect_true(is.list(ci))
  expect_named(ci, c("lower", "upper"))
  expect_true(ci$lower >= 0 && ci$lower <= 1)
  expect_true(ci$upper >= 0 && ci$upper <= 1)
  expect_true(ci$lower < ci$upper)

  # Edge cases
  ci_zero <- calculate_prop_ci(0, 100, 0.95)
  expect_equal(ci_zero$lower, 0)

  ci_all <- calculate_prop_ci(100, 100, 0.95)
  expect_equal(ci_all$upper, 1)

  # Different confidence levels
  ci_90 <- calculate_prop_ci(50, 100, 0.90)
  ci_99 <- calculate_prop_ci(50, 100, 0.99)
  expect_true(ci_90$upper - ci_90$lower < ci_99$upper - ci_99$lower)

  # Error handling (n = 0 should return NA)
  ci_error <- calculate_prop_ci(0, 0, 0.95)
  expect_true(is.na(ci_error$lower))
  expect_true(is.na(ci_error$upper))
})

test_that("calculate_normal_ci works correctly", {
  # Normal case
  ci <- calculate_normal_ci(0.5, 100, 0.95)
  expect_true(is.list(ci))
  expect_named(ci, c("lower", "upper"))
  expect_true(ci$lower >= 0 && ci$lower <= 1)
  expect_true(ci$upper >= 0 && ci$upper <= 1)
  expect_true(ci$lower < ci$upper)

  # Bounds are respected
  ci_low <- calculate_normal_ci(0.01, 100, 0.95)
  expect_true(ci_low$lower >= 0)

  ci_high <- calculate_normal_ci(0.99, 100, 0.95)
  expect_true(ci_high$upper <= 1)

  # Different sample sizes
  ci_small <- calculate_normal_ci(0.5, 10, 0.95)
  ci_large <- calculate_normal_ci(0.5, 1000, 0.95)
  expect_true(ci_small$upper - ci_small$lower > ci_large$upper - ci_large$lower)

  # Symmetric around rate
  expect_equal(ci$lower + ci$upper, 1, tolerance = 0.01)
})

test_that("get_z_score works correctly", {
  # Standard confidence levels
  expect_equal(get_z_score(0.95), qnorm(0.975))
  expect_equal(get_z_score(0.90), qnorm(0.95))
  expect_equal(get_z_score(0.99), qnorm(0.995))

  # Approximate values
  expect_equal(get_z_score(0.95), 1.96, tolerance = 0.01)
  expect_equal(get_z_score(0.90), 1.645, tolerance = 0.01)
  expect_equal(get_z_score(0.99), 2.576, tolerance = 0.01)

  # Different confidence levels produce ordered z-scores
  z_90 <- get_z_score(0.90)
  z_95 <- get_z_score(0.95)
  z_99 <- get_z_score(0.99)
  expect_true(z_90 < z_95)
  expect_true(z_95 < z_99)
})

test_that("bal_ess is equivalent to ess", {
  # Should be a simple wrapper
  weights <- nhefs_weights$w_ate

  ess_val <- ess(weights)
  bal_ess_val <- bal_ess(weights)

  expect_identical(ess_val, bal_ess_val)
})

test_that("bal_ess handles NA values", {
  weights_na <- c(1, 2, NA, 3, 4)

  # With na.rm = FALSE (default)
  expect_true(is.na(bal_ess(weights_na)))

  # With na.rm = TRUE
  ess_val <- bal_ess(weights_na, na.rm = TRUE)
  expect_type(ess_val, "double")
  expect_false(is.na(ess_val))
})

test_that("bal_ess works with different weight types", {
  # Numeric vector
  ess1 <- bal_ess(c(1, 1, 1, 1))
  expect_equal(ess1, 4)

  # Variable weights
  ess2 <- bal_ess(c(1, 2, 3, 4))
  expect_true(ess2 < 4)

  # PSW weights
  if (propensity::is_psw(nhefs_weights$w_ate)) {
    ess_psw <- bal_ess(nhefs_weights$w_ate)
    expect_type(ess_psw, "double")
    expect_true(ess_psw > 0)
  }
})

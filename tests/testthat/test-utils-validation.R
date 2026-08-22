test_that("validate_weights accepts numeric, `NULL`, and causal weight objects", {
  numeric_weights <- c(1, 2, 3)

  expect_silent(validate_weights(numeric_weights, 3))
  expect_silent(validate_weights(NULL, 3))

  psw_weights <- propensity::psw(numeric_weights, estimand = "ate")
  expect_silent(validate_weights(psw_weights, 3))

  # Balancing weights are a different causal weight class. `balancing` is not a
  # dependency, so the minimal object causalgenerics recognizes stands in for it.
  bw_weights <- causalgenerics::new_causal_wts(numeric_weights, subclass = "bw")
  expect_true(causalgenerics::is_causal_wt(bw_weights))
  expect_silent(validate_weights(bw_weights, 3))
})

test_that("validate_weights rejects weights that are not a causal weight or numeric", {
  expect_error(
    validate_weights(c("a", "b", "c"), 3),
    class = "halfmoon_type_error"
  )
  expect_halfmoon_error(
    validate_weights(c("a", "b", "c"), 3),
    "halfmoon_type_error"
  )
})

test_that("validate_weights rejects `NULL` when the caller requires weights", {
  expect_silent(validate_weights(NULL, 3))
  expect_error(
    validate_weights(NULL, allow_null = FALSE),
    class = "halfmoon_type_error"
  )
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    validate_weights("a", allow_null = FALSE)
  )
})

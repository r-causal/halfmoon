test_that("determine_reference_group rejects a reference level longer than one", {
  expect_error(
    determine_reference_group(c(0, 0, 1, 1), c(0, 1)),
    class = "halfmoon_arg_error"
  )
})

test_that("determine_reference_group rejects a fractional index", {
  expect_error(
    determine_reference_group(c(0, 0, 1, 1), 1.5),
    class = "halfmoon_arg_error"
  )
  expect_error(
    determine_reference_group(factor(c("a", "a", "b", "b")), 1.5),
    class = "halfmoon_arg_error"
  )
})

test_that("determine_reference_group rejects a missing reference level", {
  expect_error(
    determine_reference_group(c(0, 0, 1, 1), NA_real_),
    class = "halfmoon_arg_error"
  )
})

test_that("determine_reference_group keeps value matching ahead of indexing", {
  group <- c(0, 0, 1, 1)

  # A numeric that equals a level value is a value match, not an index
  expect_equal(determine_reference_group(group, 0), 0)
  expect_equal(determine_reference_group(group, 1), 1)

  # NULL means the first observed level
  expect_equal(determine_reference_group(group), 0)

  # An integral index still resolves positionally when it is not a level value
  letters_group <- factor(c("a", "a", "b", "b"))
  expect_equal(determine_reference_group(letters_group, 2), "b")
  expect_equal(determine_reference_group(letters_group, 1), "a")
})

test_that("determine_reference_group still reports out-of-bounds indices", {
  expect_error(
    determine_reference_group(factor(c("a", "a", "b", "b")), 10),
    class = "halfmoon_range_error"
  )
})

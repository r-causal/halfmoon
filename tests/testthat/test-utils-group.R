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

test_that("extract_group_levels reads the observed levels droplevels() would", {
  factors <- list(
    plain = factor(c("a", "a", "b")),
    unused_level = factor(c("a", "a", "b"), levels = c("a", "b", "c")),
    missing_value = factor(c("a", NA, "b")),
    used_na_level = addNA(factor(c("a", NA, "b"))),
    unused_na_level = factor(
      c("a", "b"),
      levels = c("a", "b", NA),
      exclude = NULL
    ),
    empty = factor(character(0)),
    empty_with_levels = factor(character(0), levels = c("a", "b")),
    all_missing = factor(c(NA, NA), levels = c("a", "b")),
    reordered = factor(c("a", "b", "c"), levels = c("c", "b", "a"))
  )

  for (nm in names(factors)) {
    expect_identical(
      extract_group_levels(factors[[nm]], require_binary = FALSE),
      levels(droplevels(factors[[nm]])),
      info = nm
    )
  }
})

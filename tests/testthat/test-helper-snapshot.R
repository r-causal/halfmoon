test_that("expect_halfmoon_error asserts the class it is given", {
  # A matching class passes and still records the snapshot.
  expect_halfmoon_error(
    abort("bad argument", error_class = "halfmoon_arg_error"),
    "halfmoon_arg_error"
  )

  # A mismatched class rethrows the original condition instead of passing.
  expect_error(
    expect_halfmoon_error(
      abort("bad argument", error_class = "halfmoon_arg_error"),
      "halfmoon_type_error"
    ),
    class = "halfmoon_arg_error"
  )
})

test_that("expect_halfmoon_warning asserts the class it is given", {
  expect_halfmoon_warning(
    warn("odd data", warning_class = "halfmoon_data_warning"),
    "halfmoon_data_warning"
  )

  # A mismatched class fails instead of passing. The failure is caught as the
  # condition it is, which also stops the helper before it records a snapshot
  # for a case that is not meant to have one. The unmatched warning is
  # re-emitted by testthat, so it is suppressed here.
  suppressWarnings(
    expect_error(
      expect_halfmoon_warning(
        warn("odd data", warning_class = "halfmoon_data_warning"),
        "halfmoon_type_warning"
      ),
      class = "expectation_failure"
    )
  )
})

test_that("the snapshot helpers can see objects local to the test", {
  local_message <- "locally defined message"
  expect_halfmoon_error(
    abort(local_message, error_class = "halfmoon_arg_error"),
    "halfmoon_arg_error"
  )
})

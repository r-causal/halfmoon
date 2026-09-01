test_that("get_column_name reports the user-facing call when a value is not a column name", {
  expect_halfmoon_error(
    check_qq(nhefs_weights, .var = c(1, 2), .exposure = qsmk),
    "halfmoon_type_error"
  )
})

test_that("get_column_name reports the user-facing call when the argument cannot be evaluated", {
  expect_halfmoon_error(
    check_qq(nhefs_weights, .var = no_such_function(), .exposure = qsmk),
    "halfmoon_type_error"
  )
})

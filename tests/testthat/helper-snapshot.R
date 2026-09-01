# Helper functions for consistent snapshot testing of errors and warnings
# These ensure we always capture both the message and the condition class

# `expect_snapshot()` has no quasiquotation, so the expression is spliced into a
# constructed call and evaluated in the caller's environment. That keeps
# test-local objects resolvable and records the real call in the snapshot
# instead of the literal token `expr`.
snapshot_condition <- function(expr_quo, ...) {
  snap_call <- rlang::call2(
    testthat::expect_snapshot,
    ...,
    rlang::get_expr(expr_quo)
  )
  rlang::eval_bare(snap_call, rlang::get_env(expr_quo))
}

# For testing errors - captures snapshot AND verifies error class if provided
# Usage:
#   expect_halfmoon_error(plot_balance(invalid_data))
#   expect_halfmoon_error(plot_balance(invalid_data), "halfmoon_type_error")
# When `class` is supplied the expression is evaluated twice: once to assert the
# class and once to record the snapshot.
expect_halfmoon_error <- function(expr, class = NULL) {
  expr_quo <- rlang::enquo(expr)
  if (!is.null(class)) {
    testthat::expect_error(rlang::eval_tidy(expr_quo), class = class)
  }
  snapshot_condition(expr_quo, error = TRUE, cnd_class = TRUE)
}

# For testing warnings - captures snapshot AND verifies warning class if provided
# Usage:
#   expect_halfmoon_warning(check_calibration(small_data))
#   expect_halfmoon_warning(check_calibration(small_data), "halfmoon_data_warning")
# When `class` is supplied the expression is evaluated twice: once to assert the
# class and once to record the snapshot.
expect_halfmoon_warning <- function(expr, class = NULL) {
  expr_quo <- rlang::enquo(expr)
  if (!is.null(class)) {
    testthat::expect_warning(rlang::eval_tidy(expr_quo), class = class)
  }
  snapshot_condition(expr_quo, cnd_class = TRUE)
}

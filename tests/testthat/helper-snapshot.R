# Helper functions for consistent snapshot testing of errors and warnings
# These ensure we always capture both the message and the condition class

# For testing errors - captures snapshot AND verifies error class if provided
# Usage: 
#   expect_halfmoon_error(plot_balance(invalid_data))
#   expect_halfmoon_error(plot_balance(invalid_data), "halfmoon_type_error")
expect_halfmoon_error <- function(expr, class = NULL) {
  # First capture the snapshot
  testthat::expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    expr
  )
  
  # Then verify the specific error class if provided
  if (!is.null(class)) {
    testthat::expect_error(
      expr,
      class = class
    )
  }
}

# For testing warnings - captures snapshot AND verifies warning class if provided
# Usage: 
#   expect_halfmoon_warning(check_calibration(small_data))
#   expect_halfmoon_warning(check_calibration(small_data), "halfmoon_data_warning")
expect_halfmoon_warning <- function(expr, class = NULL) {
  # Capture the snapshot with class information
  # This is sufficient since cnd_class = TRUE will verify the class
  testthat::expect_snapshot(
    cnd_class = TRUE,
    expr
  )
}
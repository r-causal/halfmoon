# Helper to suppress small sample size warnings in tests
# These warnings are informational and tested separately

suppress_calibration_warnings <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (
        grepl(
          "Small sample sizes or extreme proportions detected",
          conditionMessage(w)
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

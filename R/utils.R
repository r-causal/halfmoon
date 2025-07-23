# future implementations for interface
#
# be_quiet <- function() {
#   getOption("halfmoon.quiet", default = FALSE)
# }
#
# alert_info <- function(.message) {
#   if (!be_quiet()) {
#     cli::cli_alert_info(text = .message)
#   }
# }

abort <- function(.message) {
  cli::cli_abort(message = .message)
}

utils::globalVariables(c(
  "method",
  "metric",
  "variable",
  ".bin",
  "bin",
  "n_events",
  "n_total",
  "x",
  "x_var",
  "y",
  "y_var",
  "fitted_mean",
  "group_mean",
  ".fitted",
  ".group",
  "ymin",
  "ymax"
))

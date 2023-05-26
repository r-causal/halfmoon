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


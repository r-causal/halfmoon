# Exposure type announcements are user-facing information, not test output, so
# they are silenced for the whole suite. Tests that cover the announcement turn
# the option back on locally.
op <- options(halfmoon.quiet = TRUE)

withr::defer(options(op), teardown_env())

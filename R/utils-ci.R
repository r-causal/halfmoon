# Confidence interval helper functions

#' Calculate confidence interval using prop.test
#'
#' Wrapper around prop.test that handles errors and returns consistent output
#'
#' @param x Number of successes
#' @param n Number of trials
#' @param conf_level Confidence level (default 0.95)
#' @return Named list with lower and upper bounds, or NA values on error
#' @keywords internal
calculate_prop_ci <- function(x, n, conf_level = 0.95) {
  tryCatch(
    {
      suppressWarnings({
        ci <- stats::prop.test(x = x, n = n, conf.level = conf_level)$conf.int
      })
      list(lower = ci[1], upper = ci[2])
    },
    error = function(e) {
      list(lower = NA_real_, upper = NA_real_)
    }
  )
}

#' Calculate confidence interval using normal approximation
#'
#' @param rate The observed rate/proportion
#' @param n Sample size
#' @param conf_level Confidence level (default 0.95)
#' @return Named list with lower and upper bounds
#' @keywords internal
calculate_normal_ci <- function(rate, n, conf_level = 0.95) {
  alpha <- 1 - conf_level
  z_score <- stats::qnorm(1 - alpha / 2)
  se <- sqrt(rate * (1 - rate) / n)

  list(
    lower = max(0, rate - z_score * se),
    upper = min(1, rate + z_score * se)
  )
}

#' Get z-score for confidence level
#'
#' @param conf_level Confidence level (default 0.95)
#' @return z-score for the given confidence level
#' @keywords internal
get_z_score <- function(conf_level = 0.95) {
  alpha <- 1 - conf_level
  stats::qnorm(1 - alpha / 2)
}

#' Calculate Effective Sample Size for Single Weight Vector
#'
#' Computes the effective sample size (ESS) for a single weighting scheme.
#' This is a wrapper around [ess()] that follows the bal_*() naming convention
#' for API consistency.
#'
#' @details
#' The effective sample size (ESS) is calculated using the classical formula:
#' \eqn{ESS = (\sum w)^2 / \sum(w^2)}.
#'
#' ESS reflects how many observations you would have if all were equally weighted.
#' When weights vary substantially, the ESS can be much smaller than the actual
#' number of observations, indicating that a few observations carry
#' disproportionately large weights.
#'
#' **Diagnostic Value**:
#' * A large discrepancy between ESS and the actual sample size indicates that
#'   a few observations carry disproportionately large weights
#' * A small ESS signals that weighted estimates are more sensitive to a handful
#'   of observations, inflating the variance and standard errors
#' * If ESS is much lower than the total sample size, consider investigating
#'   why some weights are extremely large or small
#'
#' @param .weights A numeric vector of weights or a single weight column from a data frame.
#' @inheritParams balance_params
#'
#' @return A single numeric value representing the effective sample size.
#'
#' @family balance functions
#' @seealso [ess()] for the underlying implementation, [check_ess()] for
#'   computing ESS across multiple weighting schemes
#'
#' @examples
#' # ESS for ATE weights
#' bal_ess(nhefs_weights$w_ate)
#'
#' # ESS for ATT weights
#' bal_ess(nhefs_weights$w_att)
#'
#' # With missing values
#' weights_with_na <- nhefs_weights$w_ate
#' weights_with_na[1:5] <- NA
#' bal_ess(weights_with_na, na.rm = TRUE)
#'
#' @export
bal_ess <- function(.weights, na.rm = FALSE) {
  # Simply call the existing ess() function
  ess(.weights, na.rm = na.rm)
}

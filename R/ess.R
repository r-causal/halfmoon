#' Calculate the Effective Sample Size (ESS)
#'
#' This function computes the effective sample size (ESS) given a vector of
#' weights, using the classical \eqn{(\sum w)^2 / \sum(w^2)} formula (sometimes
#' referred to as "Kish's effective sample size").
#'
#' @param wts A numeric vector of weights (e.g., from survey or
#'   inverse-probability weighting).
#' @param na.rm Logical. Should missing values be removed? Default is FALSE.
#'
#' @return A single numeric value representing the effective sample size.
#'
#' @details The effective sample size (ESS) reflects how many observations you
#'   would have if all were equally weighted. If the weights vary substantially,
#'   the ESS can be much smaller than the actual number of observations.
#'   Formally:
#'
#' \deqn{
#'   \mathrm{ESS} = \frac{\left(\sum_i w_i\right)^2}{\sum_i w_i^2}.
#' }
#'
#' **Diagnostic Value**:
#' * **Indicator of Weight Concentration**: A large discrepancy between ESS
#'   and the actual sample size indicates that a few observations carry
#'   disproportionately large weights, effectively reducing the usable
#'   information in the dataset.
#' * **Variance Inflation**: A small ESS signals that weighted estimates are
#'   more sensitive to a handful of observations, inflating the variance and
#'   standard errors.
#' * **Practical Guidance**: If ESS is much lower than the total sample
#'   size, it is advisable to investigate why some weights are extremely large
#'   or small. Techniques like weight trimming or stabilized weights might be
#'   employed to mitigate the issue
#'
#' @examples
#' # Suppose we have five observations with equal weights
#' wts1 <- rep(1.2, 5)
#' # returns 5, because all weights are equal
#' ess(wts1)
#'
#' # If weights vary more, smaller than 5
#' wts2 <- c(0.5, 2, 2, 0.1, 0.8)
#' ess(wts2)
#'
#' @export
ess <- function(wts, na.rm = FALSE) {
  # Extract numeric data from psw weights if present
  wts <- extract_weight_data(wts)
  sum(wts, na.rm = na.rm)^2 / sum(wts^2, na.rm = na.rm)
}

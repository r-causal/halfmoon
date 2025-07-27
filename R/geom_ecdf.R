#' Calculate weighted and unweighted empirical cumulative distributions
#'
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualization of distribution. `geom_ecdf()` is similar to
#' [`ggplot2::stat_ecdf()`] but it can also calculate weighted ECDFs.
#'
#' @details
#' ECDF plots show the cumulative distribution function \eqn{F(x) = P(X \leq x)},
#' displaying what proportion of observations fall below each value. When comparing
#' treatment groups, overlapping ECDF curves indicate similar distributions and
#' thus good balance.
#'
#' ECDF plots are closely related to quantile-quantile (QQ) plots (see [`geom_qq2()`]).
#' While ECDF plots show \eqn{F(x)} for each group, QQ plots show the inverse relationship
#' by plotting \eqn{F_1^{-1}(p)} vs \eqn{F_2^{-1}(p)}. Both visualize the same distributional
#' information:
#' - ECDF plots: Compare cumulative probabilities at each value
#' - QQ plots: Compare values at each quantile
#'
#' Choose ECDF plots when you want to see the full cumulative distribution or when
#' comparing multiple groups simultaneously. Choose QQ plots when you want to directly
#' compare two groups with an easy-to-interpret 45-degree reference line.
#'
#' @section Aesthetics: In addition to the aesthetics for
#'   [`ggplot2::stat_ecdf()`], `geom_ecdf()` also accepts: \itemize{ \item
#'   weights }
#'
#' @inheritParams ggplot2::stat_ecdf
#'
#' @return a geom
#' @family ggplot2 functions
#'
#' @seealso
#' - [`geom_qq2()`] for an alternative visualization using quantile-quantile plots
#' - [`ggplot2::stat_ecdf()`] for the unweighted version
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(
#'   nhefs_weights,
#'   aes(x = smokeyrs, color = qsmk)
#' ) +
#'   geom_ecdf(aes(weights = w_ato)) +
#'   xlab("Smoking Years") +
#'   ylab("Proportion <= x")
#'
geom_ecdf <- function(
  mapping = NULL,
  data = NULL,
  geom = "step",
  position = "identity",
  ...,
  n = NULL,
  pad = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatWeightedECDF,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(n = n, pad = pad, na.rm = na.rm, ...)
  )
}

StatWeightedECDF <- ggplot2::ggproto(
  "StatWeightedECDF",
  ggplot2::StatEcdf,
  compute_group = function(data, scales, n = NULL, pad = NULL) {
    if ("weights" %in% names(data)) {
      data <- data[order(data$x), ]
      # ggplot2 3.4.1 changed this stat's name from `y` to `ecdf`
      if (packageVersion("ggplot2") >= "3.4.1") {
        data$ecdf <- cumsum(data$weights) / sum(data$weights)
      } else {
        data$y <- cumsum(data$weights) / sum(data$weights)
      }
      data
    } else {
      ggplot2::StatEcdf$compute_group(data, scales, n = n, pad = pad)
    }
  },
  required_aes = c("x"),
  optional_aes = "weights"
)

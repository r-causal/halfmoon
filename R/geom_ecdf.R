#' Calculate weighted and unweighted empirical cumulative distributions
#'
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualization of distribution. `geom_ecdf()` is similar to
#' [`ggplot2::stat_ecdf()`] but it can also calculate weighted ECDFs.
#'
#' @section Aesthetics: In addition to the aesthetics for
#'   [`ggplot2::stat_ecdf()`], `geom_ecdf()` also accepts: \itemize{ \item
#'   weights }
#'
#' @inheritParams ggplot2::stat_ecdf
#'
#' @return a geom
#' @export
#'
#' @examples
#' library(ggplot2)
#' nhefs_weights <- tidysmd::nhefs_weights
#'
#' ggplot(
#'   nhefs_weights,
#'   aes(x = smokeyrs, color = factor(qsmk))
#' ) +
#'   geom_ecdf(aes(weights = w_ato)) +
#'   xlab("Smoking Years") +
#'   ylab("Proportion <= x")
#'
geom_ecdf <- function(mapping = NULL, data = NULL, geom = "step", position = "identity",
                      ..., n = NULL, pad = TRUE, na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatWeightedECDF, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, pad = pad, na.rm = na.rm, ...)
  )
}

StatWeightedECDF <- ggplot2::ggproto(
  "StatWeightedECDF",
  ggplot2::StatEcdf,
  compute_group = function(data, scales, n = NULL, pad = NULL) {
    if ("weights" %in% names(data)) {
      data <- data[order(data$x), ]
      data$y <- cumsum(data$weights) / sum(data$weights)
      data
    } else {
      ggplot2::StatEcdf$compute_group(data, scales, n = n, pad = pad)
    }
  },
  required_aes = c("x"),
  optional_aes = "weights"
)

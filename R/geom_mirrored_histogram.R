#' Create mirrored histograms
#'
#' @inheritParams ggplot2::geom_histogram
#'
#' @return a geom
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(nhefs_weights, aes(.fitted)) +
#'   geom_mirror_histogram(
#'     aes(group = qsmk),
#'     bins = 50
#'   ) +
#'   geom_mirror_histogram(
#'     aes(fill = qsmk, weight = w_ate),
#'     bins = 50,
#'     alpha = 0.5
#'   ) +
#'   scale_y_continuous(labels = abs)
geom_mirror_histogram <- function(
    mapping = NULL,
    data = NULL,
    position = "stack",
    ...,
    binwidth = NULL,
    bins = NULL,
    na.rm = FALSE,
    orientation = NA,
    show.legend = NA,
    inherit.aes = TRUE
) {
  ggplot2::geom_histogram(
    mapping = mapping,
    data = data,
    stat = StatMirrorCount,
    position = position,
    ...,
    binwidth = binwidth,
    bins = bins,
    na.rm = na.rm,
    orientation = orientation,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

StatMirrorCount <- ggplot2::ggproto(
  "StatMirrorCount",
  ggplot2::StatBin,
  compute_group = function(data, scales, binwidth = NULL, bins = NULL,
                           center = NULL, boundary = NULL,
                           closed = c("right", "left"), pad = FALSE,
                           breaks = NULL, flipped_aes = FALSE,
                           origin = NULL, right = NULL, drop = NULL) {
    group <- unique(data$group)
    data <- ggplot2::StatBin$compute_group(data = data, scales = scales, binwidth = binwidth, bins = bins,
                                           center = center, boundary = boundary,
                                           closed = closed, pad = pad,
                                           breaks = breaks, flipped_aes = flipped_aes,
                                           origin = origin, right = right, drop = drop)
    if (group == 1) {
      data$count <- -data$count
    } else if (group > 2) {
      abort(
        "Groups of three or greater not supported in `geom_mirror_histogram()`"
      )
    } else if (group == -1) {
      abort(c(
        "No group detected.",
        "*" = "Do you need to use {.var aes(group = ...)}  \\
        with your grouping variable?"
      ))
    }
    data
  }
)

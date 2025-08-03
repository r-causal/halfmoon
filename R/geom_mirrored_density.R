#' Create mirrored density plots
#'
#' @inheritParams ggplot2::geom_density
#' @param stat The statistical transformation to use on the data for this layer.
#'   This should always be "density" (the default).
#'
#' @return a geom
#' @family ggplot2 functions
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(nhefs_weights, aes(.fitted)) +
#'   geom_mirror_density(
#'     aes(group = qsmk),
#'     bw = 0.02
#'   ) +
#'   geom_mirror_density(
#'     aes(fill = qsmk, weight = w_ate),
#'     bw = 0.02,
#'     alpha = 0.5
#'   ) +
#'   scale_y_continuous(labels = abs)
geom_mirror_density <- function(
  mapping = NULL,
  data = NULL,
  stat = "density",
  position = "identity",
  ...,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  outline.type = "upper"
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatMirrorDensity,
    geom = ggplot2::GeomArea,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      orientation = orientation,
      outline.type = outline.type,
      ...
    )
  )
}

StatMirrorDensity <- ggplot2::ggproto(
  "StatMirrorDensity",
  ggplot2::StatDensity,
  setup_data = function(data, params) {
    # Get unique groups in each panel
    panel_groups <- data |>
      dplyr::group_by(PANEL) |>
      dplyr::summarise(
        .panel_groups = list(sort(unique(group))),
        .n_groups = length(unique(group)),
        .groups = "drop"
      )
    
    # Check for panels with more than 2 groups
    if (any(panel_groups$.n_groups > 2)) {
      abort(
        "Groups of three or greater not supported in `geom_mirror_density()`"
      )
    }
    
    # Join back to get panel group info for each row
    data <- dplyr::left_join(data, panel_groups, by = "PANEL")
    
    # Mark which groups should be mirrored (first group in each panel)
    data$.should_mirror <- purrr::map2_lgl(data$group, data$.panel_groups, 
      ~ length(.y) == 2 && .x == .y[1]
    )
    
    # Clean up temporary columns
    data$.panel_groups <- NULL
    data$.n_groups <- NULL
    
    data
  },
  compute_group = function(
    data,
    scales,
    bw = "nrd0",
    adjust = 1,
    kernel = "gaussian",
    n = 512,
    trim = FALSE,
    na.rm = FALSE,
    bounds = c(-Inf, Inf),
    flipped_aes = FALSE
  ) {
    # Check for no group
    group <- unique(data$group)
    if (group == -1) {
      abort(c(
        "No group detected.",
        "*" = "Do you need to use {.var aes(group = ...)}  \\
        with your grouping variable?"
      ))
    }
    
    # Store mirroring flag
    should_mirror <- unique(data$.should_mirror)
    
    data <- ggplot2::StatDensity$compute_group(
      data = data,
      scales = scales,
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      bounds = bounds,
      flipped_aes = flipped_aes
    )
    
    # Apply mirroring if needed
    if (length(should_mirror) == 1 && should_mirror) {
      data$density <- -data$density
      data$count <- -data$count
      data$scaled <- -data$scaled
      data$ndensity <- -data$ndensity
    }
    
    data
  }
)

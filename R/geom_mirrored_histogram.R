#' Create mirrored histograms
#'
#' @inheritParams ggplot2::geom_histogram
#'
#' @return a geom
#' @family ggplot2 functions
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
        "Groups of three or greater not supported in `geom_mirror_histogram()`",
        error_class = "halfmoon_group_error"
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
    binwidth = NULL,
    bins = NULL,
    center = NULL,
    boundary = NULL,
    closed = c("right", "left"),
    pad = FALSE,
    breaks = NULL,
    flipped_aes = FALSE,
    origin = NULL,
    right = NULL,
    drop = NULL
  ) {
    # Check for no group
    group <- unique(data$group)
    if (group == -1) {
      abort(
        c(
          "No group detected.",
          "*" = "Do you need to use {.var aes(group = ...)}  \\
          with your grouping variable?"
        ),
        error_class = "halfmoon_aes_error"
      )
    }
    
    # Store mirroring flag
    should_mirror <- unique(data$.should_mirror)
    
    data <- ggplot2::StatBin$compute_group(
      data = data,
      scales = scales,
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      closed = closed,
      pad = pad,
      breaks = breaks,
      flipped_aes = flipped_aes,
      origin = origin,
      right = right,
      drop = drop
    )
    
    # Apply mirroring if needed
    if (length(should_mirror) == 1 && should_mirror) {
      data$count <- -data$count
    }
    
    data
  }
)

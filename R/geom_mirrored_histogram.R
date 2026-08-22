#' Create mirrored histograms
#'
#' @details
#' A mirrored histogram draws one group above the axis and the other below it,
#' so a panel holding three or more groups has no partial plot to fall back on.
#' That is an error, `halfmoon_group_error`, rather than a dropped group. It is
#' a deliberate difference from [geom_roc()] and [geom_qq2()], where each group
#' is drawn on its own and one that cannot be drawn is warned about and skipped.
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

    # A mirrored plot draws one group above the axis and one below, so there
    # is no partial rendering to fall back on and a third group is an error
    # rather than a dropped group
    if (any(panel_groups$.n_groups > 2)) {
      n_observed <- max(panel_groups$.n_groups)
      abort(
        c(
          "{.fun geom_mirror_histogram} draws at most two groups per panel, and a panel here has {n_observed}.",
          i = "One group is drawn above the axis and one below, so there is no partial plot to draw."
        ),
        error_class = "halfmoon_group_error"
      )
    }

    # Join back to get panel group info for each row
    data <- dplyr::left_join(data, panel_groups, by = "PANEL")

    # Mark which groups should be mirrored (first group in each panel)
    data$.should_mirror <- purrr::map2_lgl(
      data$group,
      data$.panel_groups,
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
    ...,
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

    # Extract numeric data from psw weights if present
    if ("weight" %in% names(data)) {
      data$weight <- extract_weight_data(data$weight)
    }

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
      ...,
      drop = drop
    )

    # Apply mirroring if needed
    if (length(should_mirror) == 1 && should_mirror) {
      data <- mirror_computed_stats(data)
    }

    data
  }
)

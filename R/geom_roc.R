#' ROC Curve Geom for Causal Inference
#'
#' A ggplot2 geom for plotting ROC curves with optional weighting.
#' Emphasizes the balance interpretation where AUC around 0.5 indicates good balance.
#'
#' @param mapping Set of aesthetic mappings. If specified, inherits from the plot.
#' @param data Data frame to use. If not specified, inherits from the plot.
#' @param stat Statistical transformation to use. Default is "roc".
#' @param position Position adjustment. Default is "identity".
#' @param na.rm Remove missing values? Default TRUE.
#' @param show.legend Show legend? Default NA.
#' @param inherit.aes Inherit aesthetics from plot? Default TRUE.
#' @param linewidth Width of the ROC curve line. Default is 0.5.
#' @param treatment_level The level of the outcome variable to consider as the treatment/event.
#'   Default is NULL, which uses the second level.
#' @param ... Additional arguments passed to the geom.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' # Basic usage
#' library(ggplot2)
#' ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
#'   geom_roc()
#'
#' # With grouping by weight
#' long_data <- tidyr::pivot_longer(
#'   nhefs_weights,
#'   cols = c(w_ate, w_att),
#'   names_to = "weight_type",
#'   values_to = "weight"
#' )
#'
#' ggplot(long_data, aes(x = .fitted, y = qsmk, weight = weight)) +
#'   geom_roc(aes(color = weight_type))
#'
#' @export
geom_roc <- function(
  mapping = NULL,
  data = NULL,
  stat = "roc",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  linewidth = 0.5,
  treatment_level = NULL,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linewidth = linewidth,
      treatment_level = treatment_level,
      ...
    )
  )
}

#' ROC Curve Stat
#'
#' Statistical transformation for ROC curves.
#'
#' @param mapping Set of aesthetic mappings.
#' @param data Data frame.
#' @param geom Geometric object to use. Default is "path".
#' @param position Position adjustment.
#' @param na.rm Remove missing values? Default TRUE.
#' @param show.legend Show legend? Default NA.
#' @param inherit.aes Inherit aesthetics? Default TRUE.
#' @param treatment_level The level of the outcome variable to consider as the treatment/event.
#'   Default is NULL, which uses the second level.
#' @param ... Additional arguments.
#'
#' @return A ggplot2 layer.
#' @export
stat_roc <- function(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  treatment_level = NULL,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatRoc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      treatment_level = treatment_level,
      ...
    )
  )
}

#' @rdname stat_roc
#' @format NULL
#' @usage NULL
#' @export
StatRoc <- ggplot2::ggproto(
  "StatRoc",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(weight = 1),

  compute_group = function(data, scales, na.rm = TRUE, treatment_level = NULL) {
    # Extract x (predictor) and y (truth)
    x <- data$x
    y <- data$y
    weights <- data$weight %||% rep(1, length(x))

    # Remove missing values if requested
    if (na.rm) {
      complete_cases <- stats::complete.cases(x, y, weights)
      x <- x[complete_cases]
      y <- y[complete_cases]
      weights <- weights[complete_cases]
    }

    # Handle y which could be:
    # 1. A factor that ggplot2 converted to numeric (1, 2)
    # 2. Already numeric (0, 1) or other values
    # 3. A character vector

    # First check if we have scale information for a factor
    if (!is.null(scales$y) && scales$y$is_discrete()) {
      # This means y was originally a factor
      # Get the original levels from the scale
      y_levels <- scales$y$get_breaks()
      if (length(y_levels) == 2 && all(y %in% c(1, 2))) {
        # Convert back to factor with original levels
        y <- factor(y_levels[y], levels = y_levels)
      }
    } else {
      # Not a discrete scale, so handle as numeric or create factor
      unique_y <- sort(unique(y))
      if (length(unique_y) != 2) {
        abort("{.arg y} must have exactly 2 unique values for ROC curve")
      }
      y <- factor(y, levels = unique_y)
    }

    roc_data <- compute_roc_curve_internal(
      y,
      x,
      weights,
      treatment_level = treatment_level
    )

    # Return data for ggplot2
    data.frame(
      x = 1 - roc_data$specificity,
      y = roc_data$sensitivity,
      group = data$group[1] # Preserve the group from input data
    )
  }
)
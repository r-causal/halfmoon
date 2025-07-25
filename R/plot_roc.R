#' Plot ROC Curves for Balance Assessment
#'
#' Creates a ggplot2 visualization of ROC curves for evaluating propensity score balance.
#' In causal inference, curves near the diagonal (AUC ≈ 0.5) indicate good balance.
#'
#' @param .data Output from `weighted_roc_curve()`.
#' @param linewidth Width of the ROC curve lines. Default is 1.
#' @param diagonal_color Color for the diagonal reference line. Default is "gray50".
#' @param diagonal_linetype Line type for the diagonal. Default is "dashed".
#' @param balance_region Show shaded region indicating good balance? Default is TRUE.
#' @param balance_alpha Alpha transparency for balance region. Default is 0.1.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Compute ROC curves
#' roc_data <- weighted_roc_curve(
#'   nhefs_weights,
#'   qsmk,
#'   .fitted,
#'   c(w_ate, w_att)
#' )
#'
#' # Create plot
#' plot_roc_curve(roc_data)
#'
#' # Without balance region
#' plot_roc_curve(roc_data, balance_region = FALSE)
#'
#' @export
plot_roc_curve <- function(
  .data,
  linewidth = 1,
  diagonal_color = "gray50",
  diagonal_linetype = "dashed",
  balance_region = TRUE,
  balance_alpha = 0.1
) {
  if (!inherits(.data, "tbl_df") && !inherits(.data, "data.frame")) {
    abort(
      "{.arg .data} must be a data frame or tibble from {.fn weighted_roc_curve}"
    )
  }

  required_cols <- c("threshold", "sensitivity", "specificity", "method")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    abort(
      "{.arg .data} must contain columns: {.val {required_cols}}. Missing: {.val {missing_cols}}"
    )
  }

  # Check if we have multiple methods
  has_multiple_methods <- length(unique(.data$method)) > 1

  # Base plot
  p <- ggplot2::ggplot(
    .data,
    ggplot2::aes(x = 1 - .data$specificity, y = .data$sensitivity)
  )

  # Add ROC curves
  if (has_multiple_methods) {
    p <- p +
      ggplot2::geom_path(
        ggplot2::aes(color = .data$method),
        linewidth = linewidth
      )
  } else {
    p <- p + ggplot2::geom_path(linewidth = linewidth)
  }

  # Add diagonal reference line
  p <- p +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      color = diagonal_color,
      linetype = diagonal_linetype,
      alpha = 0.7
    )

  # Add balance region if requested
  if (balance_region) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = 0.4,
        xmax = 0.6,
        ymin = 0.4,
        ymax = 0.6,
        alpha = balance_alpha,
        fill = "green"
      )
  }

  # Formatting
  p <- p +
    ggplot2::labs(
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      color = "Method"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = if (has_multiple_methods) "bottom" else "none"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

  p
}

#' Plot ROC AUC Values for Balance Assessment
#'
#' Creates a visualization of AUC values from weighted ROC analysis.
#' Values near 0.5 indicate good balance.
#'
#' @param .data Output from `weighted_roc_auc()` or `check_roc_balance()`.
#' @param ref_line Show reference line at AUC = 0.5? Default is TRUE.
#' @param ref_color Color for reference line. Default is "red".
#' @param bar_width Width of bars. Default is 0.6.
#' @param bar_alpha Alpha transparency for bars. Default is 0.7.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Compute AUC values
#' auc_data <- weighted_roc_auc(
#'   nhefs_weights,
#'   qsmk,
#'   .fitted,
#'   c(w_ate, w_att)
#' )
#'
#' # Create plot
#' plot_roc_auc(auc_data)
#'
#' @export
plot_roc_auc <- function(
  .data,
  ref_line = TRUE,
  ref_color = "red",
  bar_width = 0.6,
  bar_alpha = 0.7
) {
  if (!inherits(.data, "tbl_df") && !inherits(.data, "data.frame")) {
    abort(
      "{.arg .data} must be a data frame or tibble from {.fn weighted_roc_auc}"
    )
  }

  required_cols <- c("method", "auc")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    abort(
      "{.arg .data} must contain columns: {.val {required_cols}}. Missing: {.val {missing_cols}}"
    )
  }

  # Create balance interpretation based on AUC values
  .data <- dplyr::mutate(
    .data,
    balance_quality = dplyr::case_when(
      abs(.data$auc - 0.5) < 0.05 ~ "Good",
      abs(.data$auc - 0.5) < 0.1 ~ "Acceptable",
      TRUE ~ "Poor"
    )
  )

  # Create horizontal bar chart
  p <- ggplot2::ggplot(.data, ggplot2::aes(x = .data$auc, y = .data$method))

  # Add bars colored by balance quality
  p <- p +
    ggplot2::geom_col(
      ggplot2::aes(fill = .data$balance_quality),
      alpha = bar_alpha,
      width = bar_width
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$auc, 3)),
      hjust = -0.1,
      size = 3
    )

  # Add reference line at 0.5 if requested
  if (ref_line) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = 0.5,
        color = ref_color,
        linetype = "dashed",
        alpha = 0.7
      )
  }

  # Formatting
  p <- p +
    ggplot2::labs(
      x = "AUC",
      y = "Weighting Method",
      fill = "Balance Quality"
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        "Good" = "green",
        "Acceptable" = "yellow",
        "Poor" = "red"
      )
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, max(1, max(.data$auc) * 1.1)),
      expand = c(0, 0)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom"
    )

  p
}

#' ROC Curve Geom for Causal Inference
#'
#' A ggplot2 geom for plotting ROC curves with optional weighting.
#' Emphasizes the balance interpretation where AUC ≈ 0.5 indicates good balance.
#'
#' @param mapping Set of aesthetic mappings. If specified, inherits from the plot.
#' @param data Data frame to use. If not specified, inherits from the plot.
#' @param stat Statistical transformation to use. Default is "roc".
#' @param position Position adjustment. Default is "identity".
#' @param na.rm Remove missing values? Default TRUE.
#' @param show.legend Show legend? Default NA.
#' @param inherit.aes Inherit aesthetics from plot? Default TRUE.
#' @param linewidth Width of the ROC curve line. Default is 0.5.
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

  compute_group = function(data, scales, na.rm = TRUE) {
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
      # Create a factor
      y <- factor(y, levels = unique_y)
    }

    # Compute ROC curve using our internal function
    # Note: StatRoc doesn't have access to treatment_level parameter,
    # so it uses the default (second level)
    roc_data <- compute_roc_curve_internal(
      y,
      x,
      weights,
      treatment_level = NULL
    )

    # Return data for ggplot2
    data.frame(
      x = 1 - roc_data$specificity,
      y = roc_data$sensitivity,
      group = data$group[1] # Preserve the group from input data
    )
  }
)

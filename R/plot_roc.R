#' Plot ROC Curves for Balance Assessment
#'
#' Creates a ggplot2 visualization of ROC curves for evaluating propensity score balance.
#' In causal inference, curves near the diagonal (AUC ≈ 0.5) indicate good balance.
#'
#' @param .data Output from `weighted_roc_curve()`.
#' @param linewidth Width of the ROC curve lines. Default is 1.
#' @param diagonal_color Color for the diagonal reference line. Default is "gray50".
#' @param diagonal_linetype Line type for the diagonal. Default is "dashed".
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
#'
#' @export
plot_roc_curve <- function(
  .data,
  linewidth = 1,
  diagonal_color = "gray50",
  diagonal_linetype = "dashed"
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
#' @param point_size Size of the points. Default is 3.
#' @param point_shape Shape of the points. Default is 19 (filled circle).
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
  point_size = 3,
  point_shape = 19
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

  # Create dot plot
  p <- ggplot2::ggplot(.data, ggplot2::aes(x = .data$auc, y = .data$method))

  # Add points
  p <- p +
    ggplot2::geom_point(
      size = point_size,
      shape = point_shape
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(.data$auc, 3)),
      vjust = -1,
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
      y = "Weighting Method"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = c(0.02, 0.02)
    ) +
    ggplot2::theme_minimal()

  p
}


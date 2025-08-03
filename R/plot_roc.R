#' Plot weighted ROC Curves for Balance Assessment
#'
#' Creates a ggplot2 visualization of ROC curves for evaluating propensity score balance.
#' In causal inference, weighted curves near the diagonal (AUC around 0.5) indicate good balance.
#'
#' @details
#' ROC curves for balance assessment plot the true positive rate (sensitivity) against
#' the false positive rate (1 - specificity) when using propensity scores to classify
#' treatment assignment. When weights achieve perfect balance, the propensity score
#' distributions become identical between groups, yielding an ROC curve along the
#' diagonal (chance performance).
#'
#' Curves that deviate substantially from the diagonal indicate that propensity
#' scores can still discriminate between treatment groups after weighting, suggesting
#' residual imbalance. The closer the curve is to the diagonal, the better the
#' balance achieved by the weighting scheme.
#'
#' @param .data Output from [`roc_curve()`].
#' @param linewidth Width of the ROC curve lines. Default is 1.
#' @param diagonal_color Color for the diagonal reference line. Default is "gray50".
#' @param diagonal_linetype Line type for the diagonal. Default is "dashed".
#'
#' @return A ggplot2 object.
#'
#' @examples
#' roc_data <- roc_curve(
#'   nhefs_weights,
#'   qsmk,
#'   .fitted,
#'   c(w_ate, w_att)
#' )
#'
#' plot_roc_curve(roc_data)
#'
#'
#' @export
plot_roc_curve <- function(
  .data,
  linewidth = 0.5,
  diagonal_color = "gray50",
  diagonal_linetype = "dashed"
) {
  if (!inherits(.data, "tbl_df") && !inherits(.data, "data.frame")) {
    abort(
      "{.arg .data} must be a data frame or tibble from {.fn roc_curve}",
      error_class = "halfmoon_type_error"
    )
  }

  required_cols <- c("threshold", "sensitivity", "specificity", "method")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    abort(
      "{.arg .data} must contain columns: {.val {required_cols}}. Missing: {.val {missing_cols}}",
      error_class = "halfmoon_column_error"
    )
  }

  has_multiple_methods <- length(unique(.data$method)) > 1

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
      x = "1 - specificity",
      y = "sensitivity",
      color = "method"
    ) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0))

  p
}

#' Plot ROC AUC Values for Balance Assessment
#'
#' Creates a visualization of AUC values from weighted ROC analysis.
#' Values near 0.5 indicate good balance.
#'
#' @param .data Output from `check_auc()`.
#' @param ref_line Show reference line at AUC = 0.5? Default is TRUE.
#' @param ref_color Color for reference line. Default is "red".
#' @param point_size Size of the points. Default is 3.
#' @param point_shape Shape of the points. Default is 19 (filled circle).
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # Compute AUC values
#' auc_data <- check_auc(
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
      "{.arg .data} must be a data frame or tibble from {.fn check_auc}",
      error_class = "halfmoon_type_error"
    )
  }

  required_cols <- c("method", "auc")
  missing_cols <- setdiff(required_cols, names(.data))
  if (length(missing_cols) > 0) {
    abort(
      "{.arg .data} must contain columns: {.val {required_cols}}. Missing: {.val {missing_cols}}",
      error_class = "halfmoon_column_error"
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
      x = "auc",
      y = "method"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = c(0.02, 0.02)
    )

  p
}

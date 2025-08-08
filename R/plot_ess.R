#' Plot Effective Sample Size
#'
#' Creates a bar plot visualization of effective sample sizes (ESS) for different
#' weighting schemes. ESS values are shown as percentages of the actual sample size,
#' with a reference line at 100% indicating no loss of effective sample size.
#'
#' @details
#' This function visualizes the output of `check_ess()` or computes ESS directly
#' from the provided data. The plot shows how much "effective" sample size remains
#' after weighting, which is a key diagnostic for assessing weight variability.
#'
#' When `.group` is not provided, the function displays overall ESS for each
#' weighting method. When `.group` is provided, ESS is shown separately for each
#' group level using dodged bars.
#'
#' For continuous grouping variables, the function automatically creates quantile
#' groups (quartiles by default) to show how ESS varies across the distribution
#' of the continuous variable.
#'
#' Lower ESS percentages indicate:
#' - Greater weight variability
#' - More extreme weights
#' - Potentially unstable weighted estimates
#' - Need for weight trimming or alternative methods
#'
#' @param .data A data frame, either:
#'   - Output from `check_ess()` containing ESS calculations
#'   - Raw data to compute ESS from (requires `.wts` to be specified)
#' @inheritParams check_ess
#' @param fill_color Color for the bars when `.group` is not provided.
#'   Default is "#0172B1".
#' @param alpha Transparency level for the bars. Default is 0.8.
#' @param show_labels Logical. Show ESS percentage values as text labels on bars?
#'   Default is TRUE.
#' @param label_size Size of text labels. Default is 3.
#' @param percent_scale Logical. Display ESS as percentage of sample size (TRUE)
#'   or on original scale (FALSE)? Default is TRUE.
#' @param reference_line_color Color for the 100% reference line. Default is "gray50".
#' @param reference_line_type Line type for the reference line. Default is "dashed".
#'
#' @return A ggplot2 object.
#'
#' @seealso [check_ess()] for computing ESS values, [ess()] for the underlying calculation
#'
#' @examples
#' # Overall ESS for different weighting schemes
#' plot_ess(nhefs_weights, .wts = c(w_ate, w_att, w_atm))
#'
#' # ESS by treatment group (binary exposure)
#' plot_ess(nhefs_weights, .wts = c(w_ate, w_att), .group = qsmk)
#'
#' # ESS by treatment group (categorical exposure)
#' plot_ess(nhefs_weights, .wts = w_cat_ate, .group = alcoholfreq_cat)
#'
#' # ESS by age quartiles
#' plot_ess(nhefs_weights, .wts = w_ate, .group = age)
#'
#' # Customize quantiles for continuous variable
#' plot_ess(nhefs_weights, .wts = w_ate, .group = age,
#'          n_tiles = 5, tile_labels = c("Youngest", "Young", "Middle", "Older", "Oldest"))
#'
#' # Without percentage labels
#' plot_ess(nhefs_weights, .wts = c(w_ate, w_att), .group = qsmk,
#'          show_labels = FALSE)
#'
#' # Custom styling
#' plot_ess(nhefs_weights, .wts = c(w_ate, w_att), .group = qsmk,
#'          alpha = 0.6, fill_color = "steelblue",
#'          reference_line_color = "red")
#'
#' # Using pre-computed ESS data
#' ess_data <- check_ess(nhefs_weights, .wts = c(w_ate, w_att))
#' plot_ess(ess_data)
#'
#' # Show ESS on original scale instead of percentage
#' plot_ess(nhefs_weights, .wts = c(w_ate, w_att), percent_scale = FALSE)
#'
#' @export
plot_ess <- function(
  .data,
  .wts = NULL,
  .group = NULL,
  include_observed = TRUE,
  n_tiles = 4,
  tile_labels = NULL,
  fill_color = "#0172B1",
  alpha = 0.8,
  show_labels = TRUE,
  label_size = 3,
  percent_scale = TRUE,
  reference_line_color = "gray50",
  reference_line_type = "dashed"
) {
  # Check if .data is already ESS output
  is_ess_output <- all(c("method", "ess", "ess_pct", "n") %in% names(.data))

  if (!is_ess_output) {
    # Compute ESS from raw data
    .data <- check_ess(
      .data = .data,
      .wts = {{ .wts }},
      .group = {{ .group }},
      include_observed = include_observed,
      n_tiles = n_tiles,
      tile_labels = tile_labels
    )
  }

  # Determine if we have groups
  has_groups <- "group" %in% names(.data)

  # Choose which variable to plot
  y_var <- if (percent_scale) "ess_pct" else "ess"

  # Create the base plot
  if (has_groups) {
    # Plot with groups
    p <- ggplot2::ggplot(
      .data,
      ggplot2::aes(x = method, y = .data[[y_var]], fill = group)
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.9),
        alpha = alpha
      )

    if (show_labels) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = if (percent_scale) {
              scales::percent(ess_pct / 100, accuracy = 0.1)
            } else {
              scales::number(ess, accuracy = 0.1)
            }
          ),
          position = ggplot2::position_dodge(width = 0.9),
          vjust = -0.5,
          size = label_size
        )
    }
  } else {
    # Plot without groups
    p <- ggplot2::ggplot(
      .data,
      ggplot2::aes(x = method, y = .data[[y_var]])
    ) +
      ggplot2::geom_col(
        fill = fill_color,
        alpha = alpha
      )

    if (show_labels) {
      p <- p +
        ggplot2::geom_text(
          ggplot2::aes(
            label = if (percent_scale) {
              scales::percent(ess_pct / 100, accuracy = 0.1)
            } else {
              scales::number(ess, accuracy = 0.1)
            }
          ),
          vjust = -0.5,
          size = label_size
        )
    }
  }

  # Add reference line
  if (percent_scale) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = 100,
        color = reference_line_color,
        linetype = reference_line_type,
        alpha = 0.7
      )
  }

  # Determine y-axis limits
  if (percent_scale) {
    max_ess <- max(.data$ess_pct, na.rm = TRUE)
    y_upper <- ifelse(show_labels, max_ess * 1.1, max_ess * 1.05)
    y_upper <- max(y_upper, 105) # Ensure we show at least to 105%
  } else {
    max_ess <- max(.data$ess, na.rm = TRUE)
    y_upper <- ifelse(show_labels, max_ess * 1.1, max_ess * 1.05)
  }

  # Add labels and formatting
  p <- p +
    ggplot2::labs(
      x = "method",
      y = if (percent_scale) {
        "effective sample size (%)"
      } else {
        "effective sample size"
      }
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, y_upper),
      expand = c(0, 0),
      labels = if (percent_scale) {
        \(x) scales::percent(x / 100)
      } else {
        scales::number
      }
    )

  p
}

#' Create balance plot from check_balance output
#'
#' Create a Love plot-style visualization to assess balance across multiple
#' metrics computed by `check_balance()`. This function wraps `geom_love()`
#' to create a comprehensive balance assessment plot.
#'
#' @details
#' This function visualizes the output of `check_balance()`, creating a plot
#' that shows balance statistics across different variables, methods, and metrics.
#' The plot uses faceting to separate different metrics and displays the
#' absolute value of SMD by default (controlled by `abs_smd`).
#'
#' Different metrics have different interpretations:
#' \itemize{
#'   \item **SMD**: Standardized mean differences, where values near 0 indicate
#'     good balance. Often displayed as absolute values.
#'   \item **Variance Ratio**: Ratio of variances between groups, where values
#'     near 1 indicate similar variability.
#'   \item **KS**: Kolmogorov-Smirnov statistic, where smaller values indicate
#'     better distributional balance.
#'   \item **Correlation**: For continuous exposures, measures association with
#'     covariates.
#'   \item **Energy**: Multivariate balance metric applied to all variables
#'     simultaneously.
#' }
#'
#' @param .df A data frame produced by `check_balance()`
#' @param abs_smd Logical. Take the absolute value of SMD estimates? Defaults
#'   to TRUE. Does not affect other metrics which are already non-negative.
#' @param facet_scales Character. Scale specification for facets. Defaults to
#'   "free" to allow different scales for different metrics. Options are
#'   "fixed", "free_x", "free_y", or "free".
#' @inheritParams tidysmd::geom_love
#' @return A ggplot2 object
#' @family balance functions
#' @seealso [check_balance()] for computing balance metrics, [geom_love()] for
#'   the underlying geom
#' @examples
#' # Compute balance metrics
#' balance_data <- check_balance(
#'   nhefs_weights,
#'   c(age, education, race),
#'   qsmk,
#'   .wts = c(w_ate, w_att)
#' )
#'
#' # Create balance plot
#' plot_balance(balance_data)
#'
#' # Without absolute SMD values
#' plot_balance(balance_data, abs_smd = FALSE)
#'
#' # With fixed scales across facets
#' plot_balance(balance_data, facet_scales = "fixed")
#'
#' # Customize threshold lines
#' plot_balance(balance_data, vline_xintercept = 0.05)
#' @export
plot_balance <- function(
  .df,
  abs_smd = TRUE,
  facet_scales = "free",
  linewidth = 0.8,
  point_size = 1.85,
  vline_xintercept = 0.1,
  vline_color = "grey70",
  vlinewidth = 0.6
) {
  # Validate input
  validate_data_frame(.df)
  required_cols <- c("variable", "method", "metric", "estimate")
  missing_cols <- setdiff(required_cols, names(.df))
  if (length(missing_cols) > 0) {
    abort(
      paste0(
        "Input must be output from check_balance(). Missing columns: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # Take absolute value of SMD if requested
  if (abs_smd) {
    .df <- dplyr::mutate(
      .df,
      estimate = dplyr::if_else(
        metric == "smd",
        abs(estimate),
        estimate
      )
    )
  }

  # Improve variable labels for energy metric
  .df <- dplyr::mutate(
    .df,
    variable = dplyr::if_else(
      metric == "energy" & is.na(variable),
      "overall (multivariate)",
      variable
    )
  )

  # Create caption based on whether we're using absolute values
  caption_text <- if (abs_smd) {
    "smd values shown as absolute values"
  } else {
    NULL
  }

  # Create the base plot
  p <- ggplot2::ggplot(
    .df,
    ggplot2::aes(
      x = estimate,
      y = variable,
      group = method,
      color = method,
      fill = method
    )
  )
  
  # Compute unique metrics once to avoid redundant computation
  unique_metrics <- unique(.df$metric)
  
  # Determine if we should show vline (only for SMD when it's the only metric)
  show_vline <- "smd" %in% unique_metrics && length(unique_metrics) == 1
  
  # Add geom_love for non-energy metrics
  p <- p +
    geom_love(
      data = function(x) dplyr::filter(x, metric != "energy"),
      linewidth = linewidth,
      point_size = point_size,
      vline_xintercept = if (show_vline) vline_xintercept else NULL,
      vline_color = vline_color,
      vlinewidth = vlinewidth
    )
  
  # Add points for energy metric (no lines since only one point per method)
  if ("energy" %in% unique_metrics) {
    p <- p +
      ggplot2::geom_point(
        data = function(x) dplyr::filter(x, metric == "energy"),
        size = point_size
      )
  }
  
  p <- p +
    ggplot2::labs(
      x = "balance metric",
      y = "variable",
      color = "method",
      fill = "method",
      caption = caption_text
    )

  # Add faceting if multiple metrics
  n_metrics <- length(unique_metrics)
  if (n_metrics > 1) {
    p <- p +
      ggplot2::facet_wrap(
        ~metric,
        scales = facet_scales,
        labeller = ggplot2::labeller(
          metric = c(
            smd = "standardized mean difference",
            vr = "variance ratio",
            ks = "kolmogorov-smirnov",
            correlation = "correlation",
            energy = "energy distance"
          )
        )
      )
  }

  # Adjust x-axis limits based on metric
  # For variance ratio, center around 1
  if ("vr" %in% unique_metrics && n_metrics == 1) {
    max_dev <- max(abs(.df$estimate - 1), na.rm = TRUE)
    p <- p +
      ggplot2::scale_x_continuous(
        limits = c(1 - max_dev - 0.1, 1 + max_dev + 0.1)
      )
  }

  p
}

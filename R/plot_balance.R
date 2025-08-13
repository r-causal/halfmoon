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
#' For categorical exposures (>2 levels), the function automatically detects
#' multiple group level comparisons and uses `facet_grid()` to display each
#' comparison in a separate row, with metrics in columns. For binary exposures,
#' the standard `facet_wrap()` by metric is used.
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
#'   .weights = c(w_ate, w_att)
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
#'
#' # Categorical exposure example
#' # Automatically uses facet_grid to show each group comparison
#' balance_cat <- check_balance(
#'   nhefs_weights,
#'   c(age, wt71, sex),
#'   alcoholfreq_cat,
#'   .weights = w_cat_ate,
#'   .metrics = c("smd", "vr")
#' )
#' plot_balance(balance_cat)
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
      ),
      error_class = "halfmoon_column_error"
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

  # Detect if we have categorical exposure data (multiple group_level values)
  # Check if group_level column exists and has multiple unique non-NA values
  is_categorical <- FALSE
  unique_group_levels <- NULL

  if ("group_level" %in% names(.df)) {
    # Get unique group levels, excluding NA and continuous exposure indicators
    unique_group_levels <- unique(.df$group_level[!is.na(.df$group_level)])
    # Remove entries that are variable names (from correlation metric)
    unique_group_levels <- unique_group_levels[
      !unique_group_levels %in% names(.df)
    ]

    # Check if we have multiple group levels per variable/method/metric combination
    # This indicates categorical exposure
    if (length(unique_group_levels) > 1) {
      test_data <- .df |>
        dplyr::filter(!is.na(group_level), metric != "energy") |>
        dplyr::distinct(variable, method, metric, group_level)

      # If any variable/method/metric has multiple group levels, it's categorical
      grouped_counts <- test_data |>
        dplyr::group_by(variable, method, metric) |>
        dplyr::summarise(n_levels = dplyr::n(), .groups = "drop")

      is_categorical <- any(grouped_counts$n_levels > 1)
    }
  }

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

  # Add faceting based on exposure type and number of metrics
  n_metrics <- length(unique_metrics)

  # Define metric labels
  metric_labels <- c(
    smd = "standardized mean difference",
    vr = "variance ratio",
    ks = "kolmogorov-smirnov",
    correlation = "correlation",
    energy = "energy distance"
  )

  if (is_categorical) {
    # For categorical exposures, use facet_grid with group_level and metric
    if (n_metrics > 1) {
      p <- p +
        ggplot2::facet_grid(
          group_level ~ metric,
          scales = facet_scales,
          labeller = ggplot2::labeller(
            metric = metric_labels,
            .default = ggplot2::label_value
          )
        )
    } else {
      # Single metric, just facet by group_level
      p <- p +
        ggplot2::facet_wrap(
          ~group_level,
          scales = facet_scales
        )
    }
  } else {
    # For binary exposures, maintain current behavior
    if (n_metrics > 1) {
      p <- p +
        ggplot2::facet_wrap(
          ~metric,
          scales = facet_scales,
          labeller = ggplot2::labeller(metric = metric_labels)
        )
    }
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

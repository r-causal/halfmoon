#' Create mirrored distribution plots
#'
#' Create mirrored distribution plots (histograms or density plots) to compare
#' the distribution of variables between treatment groups before and after
#' weighting. This function helps assess covariate balance by visualizing
#' the distributions side-by-side with one group mirrored below the axis.
#'
#' @details
#' Mirrored distribution plots display the distribution of one group above the
#' x-axis and the other group below (mirrored). This makes it easy to compare
#' distributions and assess balance. The function supports both histogram and
#' density plot types.
#'
#' When using weights, the function can display both weighted and unweighted
#' distributions for comparison. Multiple weighting schemes can be compared
#' by providing multiple weight variables.
#'
#' @param .data A data frame containing the variables.
#' @param .var The variable to plot. Supports tidyselect syntax. Can be unquoted.
#' @param .group Column name of treatment/group variable. Supports tidyselect syntax. Can be unquoted.
#'   Must have exactly 2 levels.
#' @param .wts Optional weighting variable(s). Can be unquoted variable names, tidyselect syntax,
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes. Default is NULL (unweighted).
#' @param type Character; type of plot - "histogram" or "density". Default is "histogram".
#' @param mirror_axis Character; which axis to mirror - "y" (default) or "x".
#' @param bins Integer; number of bins for histogram. Only used when type = "histogram".
#' @param binwidth Numeric; width of bins for histogram. Only used when type = "histogram".
#'   If both bins and binwidth are specified, binwidth takes precedence.
#' @param bw Bandwidth for density estimation. Only used when type = "density".
#'   Can be numeric or character (e.g., "nrd0", "sj").
#' @param adjust Numeric; bandwidth adjustment factor for density. Only used when
#'   type = "density". Default is 1.
#' @param include_unweighted Logical. If using `.wts`, also show unweighted
#'   distribution? Defaults to TRUE.
#' @param alpha Numeric; transparency level for fills. Default is 0.6.
#' @param na.rm Logical; if TRUE, drop NA values before plotting.
#'
#' @return A ggplot2 object.
#'
#' @seealso
#' - [`geom_mirror_histogram()`] for the underlying histogram geom
#' - [`geom_mirror_density()`] for the underlying density geom
#' - [`plot_qq()`] for QQ plots, another distributional comparison
#' - [`geom_ecdf()`] for ECDF plots
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic histogram (unweighted)
#' plot_mirrored_distributions(nhefs_weights, age, qsmk)
#'
#' # Density plot instead of histogram
#' plot_mirrored_distributions(nhefs_weights, age, qsmk, type = "density")
#'
#' # With weighting
#' plot_mirrored_distributions(nhefs_weights, age, qsmk, .wts = w_ate)
#'
#' # Compare multiple weighting schemes
#' plot_mirrored_distributions(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))
#'
#' # Customize appearance
#' plot_mirrored_distributions(
#'   nhefs_weights, age, qsmk,
#'   .wts = w_ate,
#'   type = "density",
#'   alpha = 0.7
#' )
#'
#' # Without unweighted comparison
#' plot_mirrored_distributions(
#'   nhefs_weights, age, qsmk,
#'   .wts = w_ate,
#'   include_unweighted = FALSE
#' )
#'
#' @export
plot_mirrored_distributions <- function(
  .data,
  .var,
  .group,
  .wts = NULL,
  type = c("histogram", "density"),
  mirror_axis = "y",
  bins = NULL,
  binwidth = NULL,
  bw = "nrd0",
  adjust = 1,
  include_unweighted = TRUE,
  alpha = 0.6,
  na.rm = FALSE
) {
  # Match arguments
  type <- match.arg(type)

  # Handle quosures
  var_quo <- rlang::enquo(.var)
  group_quo <- rlang::enquo(.group)
  wts_quo <- rlang::enquo(.wts)

  # Get column names
  var_name <- get_column_name(var_quo, ".var")
  group_name <- get_column_name(group_quo, ".group")

  # Validate columns exist
  if (!var_name %in% names(.data)) {
    abort("Column {.code {var_name}} not found in data")
  }

  if (!group_name %in% names(.data)) {
    abort("Column {.code {group_name}} not found in data")
  }

  # Check for NA values
  if (!na.rm && any(is.na(.data[[var_name]]))) {
    abort("Variable contains missing values. Use `na.rm = TRUE` to drop them.")
  }

  # Validate group has exactly 2 levels
  group_var <- .data[[group_name]]
  group_levels <- if (is.factor(group_var)) {
    levels(group_var)
  } else {
    sort(unique(group_var[!is.na(group_var)]))
  }

  if (length(group_levels) != 2) {
    abort("Group variable must have exactly 2 levels")
  }


  # Handle weights
  if (!rlang::quo_is_null(wts_quo)) {
    # Get weight columns
    wts_cols <- tidyselect::eval_select(wts_quo, .data)
    wts_names <- names(wts_cols)

    # Create long format data
    if (include_unweighted) {
      # Add unweighted as a weight column with value 1
      .data$.unweighted <- 1
      wts_names <- c(".unweighted", wts_names)
    }

    plot_data <- tidyr::pivot_longer(
      .data,
      cols = dplyr::all_of(wts_names),
      names_to = "method",
      values_to = "weight"
    )

    # Clean up method names
    plot_data$method <- ifelse(
      plot_data$method == ".unweighted",
      "unweighted",
      plot_data$method
    )

    # Create faceted plot
    if (type == "density") {
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[var_name]])) +
        geom_mirror_density(
          ggplot2::aes(
            group = .data[[group_name]],
            fill = .data[[group_name]],
            weight = weight
          ),
          bw = bw,
          adjust = adjust,
          alpha = alpha,
          na.rm = na.rm
        ) +
        ggplot2::facet_wrap(~method, scales = "free_y")
    } else {
      # histogram
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[var_name]])) +
        geom_mirror_histogram(
          ggplot2::aes(
            group = .data[[group_name]],
            fill = .data[[group_name]],
            weight = weight
          ),
          bins = bins,
          binwidth = binwidth,
          alpha = alpha,
          na.rm = na.rm
        ) +
        ggplot2::facet_wrap(~method, scales = "free_y")
    }
  } else {
    # No weights - single plot
    if (type == "density") {
      p <- ggplot2::ggplot(.data, ggplot2::aes(x = .data[[var_name]])) +
        geom_mirror_density(
          ggplot2::aes(
            group = .data[[group_name]],
            fill = .data[[group_name]]
          ),
          bw = bw,
          adjust = adjust,
          alpha = alpha,
          na.rm = na.rm
        )
    } else {
      # histogram
      p <- ggplot2::ggplot(.data, ggplot2::aes(x = .data[[var_name]])) +
        geom_mirror_histogram(
          ggplot2::aes(
            group = .data[[group_name]],
            fill = .data[[group_name]]
          ),
          bins = bins,
          binwidth = binwidth,
          alpha = alpha,
          na.rm = na.rm
        )
    }
  }

  # Add styling and labels
  p <- p +
    ggplot2::scale_y_continuous(labels = abs) +
    ggplot2::labs(
      x = var_name,
      y = ifelse(type == "histogram", "Count", "Density")
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )

  # Mirror on x-axis if requested (rotate plot)
  if (mirror_axis == "x") {
    p <- p + ggplot2::coord_flip()
  }

  p
}

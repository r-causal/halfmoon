#' Create mirror distribution plots
#'
#' Create mirror distribution plots (histograms or density plots) to compare
#' the distribution of variables between treatment groups before and after
#' weighting. This function helps assess covariate balance by visualizing
#' the distributions side-by-side with one group mirrored below the axis.
#'
#' @details
#' Mirror distribution plots display the distribution of one group above the
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
#' plot_mirror_distributions(nhefs_weights, age, qsmk)
#'
#' # Density plot instead of histogram
#' plot_mirror_distributions(nhefs_weights, age, qsmk, type = "density")
#'
#' # With weighting
#' plot_mirror_distributions(nhefs_weights, age, qsmk, .wts = w_ate)
#'
#' # Compare multiple weighting schemes
#' plot_mirror_distributions(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))
#'
#' # Customize appearance
#' plot_mirror_distributions(
#'   nhefs_weights, age, qsmk,
#'   .wts = w_ate,
#'   type = "density",
#'   alpha = 0.7
#' )
#'
#' # Without unweighted comparison
#' plot_mirror_distributions(
#'   nhefs_weights, age, qsmk,
#'   .wts = w_ate,
#'   include_unweighted = FALSE
#' )
#'
#' @export
plot_mirror_distributions <- function(
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
  type <- match.arg(type)

  var_quo <- rlang::enquo(.var)
  group_quo <- rlang::enquo(.group)
  wts_quo <- rlang::enquo(.wts)

  validate_data_frame(.data)

  var_name <- get_column_name(var_quo, ".var")
  group_name <- get_column_name(group_quo, ".group")

  validate_column_exists(.data, var_name, ".var")
  validate_column_exists(.data, group_name, ".group")

  if (!na.rm && any(is.na(.data[[var_name]]))) {
    abort("Variable contains missing values. Use `na.rm = TRUE` to drop them.")
  }

  group_var <- .data[[group_name]]
  group_levels <- extract_group_levels(group_var, require_binary = TRUE)

  if (!rlang::quo_is_null(wts_quo)) {
    wts_cols <- tidyselect::eval_select(wts_quo, .data)
    wts_names <- names(wts_cols)

    if (include_unweighted) {
      .data$.observed <- 1
      wts_names <- c(".observed", wts_names)
    }

    plot_data <- tidyr::pivot_longer(
      .data,
      cols = dplyr::all_of(wts_names),
      names_to = "method",
      values_to = "weight"
    )

    plot_data$method <- ifelse(
      plot_data$method == ".observed",
      "observed",
      plot_data$method
    )

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

  p <- p +
    ggplot2::scale_y_continuous(labels = abs) +
    ggplot2::labs(
      x = var_name,
      y = ifelse(type == "histogram", "Count", "Density")
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )

  if (mirror_axis == "x") {
    p <- p + ggplot2::coord_flip()
  }

  p
}

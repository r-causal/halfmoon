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
#' For categorical exposures (>2 levels), the function creates a grid of
#' pairwise comparisons against a reference group. Each panel shows one
#' non-reference level compared to the reference level using the mirror
#' distribution format.
#'
#' When using weights, the function can display both weighted and unweighted
#' distributions for comparison. Multiple weighting schemes can be compared
#' by providing multiple weight variables.
#'
#' @param .data A data frame containing the variables.
#' @param .var The variable to plot. Supports tidyselect syntax. Can be unquoted.
#' @param .group Column name of treatment/group variable. Supports tidyselect syntax. Can be unquoted.
#'   For binary variables, must have exactly 2 levels. For categorical variables (>2 levels),
#'   creates pairwise comparisons against a reference group.
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
#' @param reference_group The reference group level for categorical exposures (>2 levels).
#'   Can be a string (group level) or numeric (position). Defaults to 1 (first level).
#'   Only used when .group has more than 2 levels.
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
#' # Categorical exposure - creates grid of comparisons
#' plot_mirror_distributions(
#'   nhefs_weights,
#'   age,
#'   alcoholfreq_cat,
#'   type = "density"
#' )
#'
#' # Categorical with weights
#' plot_mirror_distributions(
#'   nhefs_weights,
#'   wt71,
#'   alcoholfreq_cat,
#'   .wts = w_cat_ate,
#'   reference_group = "none"
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
  na.rm = FALSE,
  reference_group = 1L
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
    abort(
      "Variable contains missing values. Use `na.rm = TRUE` to drop them.",
      error_class = "halfmoon_na_error",
      call = rlang::current_env()
    )
  }

  group_var <- .data[[group_name]]

  # Check if we have a categorical exposure (>2 levels)
  # Always use actual unique values in the data, not factor levels
  group_levels <- sort(unique(group_var[!is.na(group_var)]))
  if (is.factor(group_var)) {
    # Convert to character to ensure proper comparison
    group_levels <- as.character(group_levels)
  }

  is_categorical <- length(group_levels) > 2

  if (is_categorical) {
    # Categorical exposure
    reference_group <- determine_reference_group(group_var, reference_group)

    # Create binary comparisons using purrr
    comparison_levels <- setdiff(group_levels, reference_group)

    .data <- purrr::map_dfr(comparison_levels, \(level) {
      comparison_df <- .data |>
        dplyr::filter(.data[[group_name]] %in% c(reference_group, level)) |>
        dplyr::mutate(comparison = paste0(level, " vs ", reference_group))
      # Update the group factor to only have the two levels
      comparison_df[[group_name]] <- factor(
        comparison_df[[group_name]],
        levels = c(reference_group, level)
      )
      comparison_df
    })
  } else if (length(group_levels) == 2) {
    # Binary exposure - no transformation needed
    group_levels <- extract_group_levels(group_var, require_binary = TRUE)
  } else {
    abort(
      "Group variable must have at least two levels",
      error_class = "halfmoon_group_error"
    )
  }

  if (!rlang::quo_is_null(wts_quo)) {
    wts_cols <- tidyselect::eval_select(wts_quo, .data)
    wts_names <- names(wts_cols)

    # Convert psw weight columns to numeric for compatibility with pivot_longer
    for (wts_name in wts_names) {
      .data[[wts_name]] <- extract_weight_data(.data[[wts_name]])
    }

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
        if (is_categorical) {
          ggplot2::facet_grid(comparison ~ method, scales = "free_y")
        } else {
          ggplot2::facet_wrap(~method, scales = "free_y")
        }
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
        if (is_categorical) {
          ggplot2::facet_grid(comparison ~ method, scales = "free_y")
        } else {
          ggplot2::facet_wrap(~method, scales = "free_y")
        }
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

    # Add faceting for categorical exposures without weights
    if (is_categorical) {
      p <- p + ggplot2::facet_wrap(~comparison, scales = "free_y")
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

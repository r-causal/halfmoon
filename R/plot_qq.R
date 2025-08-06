#' Create QQ plots for weighted and unweighted samples
#'
#' Create quantile-quantile (QQ) plots to compare the distribution of variables
#' between treatment groups before and after weighting. This function helps
#' assess covariate balance by visualizing how well the quantiles align between
#' groups.
#'
#' @details
#' QQ plots display the quantiles of one distribution against the quantiles of
#' another. Perfect distributional balance appears as points along the 45-degree
#' line (y = x). This function automatically adds this reference line and
#' appropriate axis labels.
#'
#' For an alternative visualization of the same information, see [`geom_ecdf()`],
#' which shows the empirical cumulative distribution functions directly.
#'
#' @param .data A data frame containing the variables.
#' @param .var Variable to plot. Can be unquoted (e.g., `age`) or quoted (e.g., `"age"`).
#' @param .group Column name of treatment/group variable. Can be unquoted (e.g., `qsmk`) or quoted (e.g., `"qsmk"`).
#' @param .wts Optional weighting variable(s). Can be unquoted variable names,
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes. Default is NULL (unweighted).
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param include_observed Logical. If using `.wts`, also show observed
#'   (unweighted) QQ plot? Defaults to TRUE.
#' @param treatment_level The reference treatment level to use for comparisons.
#'   If `NULL` (default), uses the last level for factors or the maximum value for numeric variables.
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#'
#' @return A ggplot2 object.
#'
#' @seealso
#' - [`geom_ecdf()`] for ECDF plots, an alternative distributional visualization
#' - [`geom_qq2()`] for the underlying geom used by this function
#' - [`qq()`] for computing QQ data without plotting
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic QQ plot (observed)
#' plot_qq(nhefs_weights, age, qsmk)
#'
#' # With weighting
#' plot_qq(nhefs_weights, age, qsmk, .wts = w_ate)
#'
#' # Compare multiple weighting schemes
#' plot_qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))
#'
#' # For propensity scores
#' plot_qq(nhefs_weights, .fitted, qsmk, .wts = w_ate)
#'
#' # Without observed comparison
#' plot_qq(nhefs_weights, age, qsmk, .wts = w_ate, include_observed = FALSE)
#'
#' @export
plot_qq <- function(
  .data,
  .var,
  .group,
  .wts = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  treatment_level = NULL,
  na.rm = FALSE
) {
  # Basic validation
  var_quo <- rlang::enquo(.var)
  group_quo <- rlang::enquo(.group)

  var_name <- get_column_name(var_quo, ".var")
  group_name <- get_column_name(group_quo, ".group")

  if (!var_name %in% names(.data)) {
    abort(
      "Column {.code {var_name}} not found in data",
      error_class = "halfmoon_column_error"
    )
  }

  if (!group_name %in% names(.data)) {
    abort(
      "Column {.code {group_name}} not found in data",
      error_class = "halfmoon_column_error"
    )
  }

  # Check for NA values
  if (!na.rm && any(is.na(.data[[var_name]]))) {
    abort(
      "Variable contains missing values. Use `na.rm = TRUE` to drop them.",
      error_class = "halfmoon_na_error"
    )
  }

  group_var <- .data[[group_name]]
  group_levels <- if (is.factor(group_var)) {
    levels(group_var)
  } else {
    sort(unique(group_var[!is.na(group_var)]))
  }

  if (length(group_levels) != 2) {
    abort(
      "Group variable must have exactly 2 levels",
      error_class = "halfmoon_group_error"
    )
  }

  # Handle NULL treatment_level
  if (is.null(treatment_level)) {
    if (is.factor(group_var)) {
      # For factors, use the last level
      treatment_level <- group_levels[length(group_levels)]
    } else {
      # For numeric, use the maximum value
      treatment_level <- max(group_levels)
    }
  }

  # Validate treatment_level exists
  if (!treatment_level %in% group_levels) {
    abort(
      "{.arg treatment_level} '{treatment_level}' not found in {.arg .group} levels: {.val {group_levels}}",
      error_class = "halfmoon_reference_error"
    )
  }

  ref_group <- treatment_level
  comp_group <- setdiff(group_levels, treatment_level)

  # Get variable name for labels
  var_name <- get_column_name(var_quo, ".var")

  # Prepare data in long format
  wts_quo <- rlang::enquo(.wts)

  if (!rlang::quo_is_null(wts_quo)) {
    # Get weight columns
    wts_cols <- tidyselect::eval_select(wts_quo, .data)
    wts_names <- names(wts_cols)

    # Create long format data
    # Convert psw weight columns to numeric for compatibility with pivot_longer
    for (wts_name in wts_names) {
      .data[[wts_name]] <- extract_weight_data(.data[[wts_name]])
    }

    if (include_observed) {
      # Add observed as a weight column with value 1
      .data$.observed <- 1
      wts_names <- c(".observed", wts_names)
    }

    plot_data <- tidyr::pivot_longer(
      .data,
      cols = dplyr::all_of(wts_names),
      names_to = "method",
      values_to = "weight"
    )

    # Clean up method names
    plot_data$method <- ifelse(
      plot_data$method == ".observed",
      "observed",
      plot_data$method
    )

    # Create plot with color aesthetic
    p <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        sample = {{ .var }},
        treatment = {{ .group }},
        weight = weight
      )
    ) +
      geom_qq2(
        ggplot2::aes(color = method),
        quantiles = quantiles,
        treatment_level = treatment_level,
        na.rm = na.rm
      )
  } else {
    # No weights - just use geom_qq2 directly
    p <- ggplot2::ggplot(
      .data,
      ggplot2::aes(sample = {{ .var }}, treatment = {{ .group }})
    ) +
      geom_qq2(
        quantiles = quantiles,
        treatment_level = treatment_level,
        na.rm = na.rm
      )
  }

  # Add reference line and labels
  p +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.8
    ) +
    ggplot2::labs(
      x = paste0(var_name, " (", group_name, " = ", ref_group, ")"),
      y = paste0(var_name, " (", group_name, " = ", comp_group, ")")
    )
}

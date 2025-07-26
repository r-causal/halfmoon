#' Create QQ plots for weighted and unweighted samples
#'
#' Create quantile-quantile (QQ) plots to compare the distribution of variables
#' between treatment groups before and after weighting. This function helps
#' assess covariate balance by visualizing how well the quantiles align between
#' groups.
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
#' @param reference_group The reference group level to use for comparisons.
#'   Defaults to 1 (first level).
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#'
#' @return A ggplot2 object.
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
  reference_group = 1L,
  na.rm = FALSE
) {
  qq_data <- qq(
    .data = .data,
    .var = {{ .var }},
    .group = {{ .group }},
    .wts = {{ .wts }},
    quantiles = quantiles,
    include_observed = include_observed,
    reference_group = reference_group,
    na.rm = na.rm
  )

  # Extract group levels for axis labels
  group_quo <- rlang::enquo(.group)
  group_name <- get_column_name(group_quo, ".group")

  group_levels <- if (is.factor(.data[[group_name]])) {
    levels(.data[[group_name]])
  } else {
    sort(unique(.data[[group_name]]))
  }

  ref_group <- group_levels[reference_group]
  comp_group <- group_levels[-reference_group]

  # Create plot
  p <- ggplot2::ggplot(
    qq_data,
    ggplot2::aes(x = reference_quantile, y = comparison_quantile)
  )

  # Use color aesthetic for multiple methods
  if (length(unique(qq_data$method)) > 1) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(color = method),
        alpha = 0.7,
        size = 1.5
      ) +
      ggplot2::scale_color_discrete(name = "method")
  } else {
    p <- p + ggplot2::geom_point(alpha = 0.7, size = 1.5)
  }

  p <- p +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.8
    ) +
    ggplot2::labs(
      x = paste0(ref_group, " quantiles"),
      y = paste0(comp_group, " quantiles")
    )

  p
}

#' Compute quantiles for a single method
#'
#' Internal function to compute quantiles for one method (observed or weighted).
#'
#' @param method Character string indicating the method ("observed" or weight column name)
#' @param .data Data frame
#' @param var_name Variable name to compute quantiles for
#' @param group_name Group variable name
#' @param ref_group Reference group level
#' @param comp_group Comparison group level
#' @param quantiles Numeric vector of quantiles
#' @param na.rm Logical indicating whether to remove NAs
#'
#' @return A tibble with quantile data
#'
#' @noRd
compute_method_quantiles <- function(
  method,
  .data,
  var_name,
  group_name,
  ref_group,
  comp_group,
  quantiles,
  na.rm
) {
  # Filter data by group
  ref_data <- .data[.data[[group_name]] == ref_group, ]
  comp_data <- .data[.data[[group_name]] == comp_group, ]

  if (na.rm) {
    ref_data <- ref_data[!is.na(ref_data[[var_name]]), ]
    comp_data <- comp_data[!is.na(comp_data[[var_name]]), ]
  }

  # Get values and weights
  ref_vals <- ref_data[[var_name]]
  comp_vals <- comp_data[[var_name]]

  if (method == "observed") {
    # Standard quantiles
    ref_q <- stats::quantile(ref_vals, probs = quantiles, na.rm = FALSE)
    comp_q <- stats::quantile(comp_vals, probs = quantiles, na.rm = FALSE)
  } else {
    # Weighted quantiles
    if (!method %in% names(ref_data) || !method %in% names(comp_data)) {
      abort("Weight column {.code {method}} not found in data")
    }

    ref_wts <- ref_data[[method]]
    comp_wts <- comp_data[[method]]

    if (na.rm) {
      ref_wts <- ref_wts[!is.na(ref_data[[var_name]])]
      comp_wts <- comp_wts[!is.na(comp_data[[var_name]])]
    }

    ref_q <- weighted_quantile(ref_vals, quantiles, .wts = ref_wts)
    comp_q <- weighted_quantile(comp_vals, quantiles, .wts = comp_wts)
  }

  dplyr::tibble(
    method = method,
    quantile = quantiles,
    reference_quantile = ref_q,
    comparison_quantile = comp_q
  )
}

#' Compute weighted quantiles
#'
#' Calculate quantiles of a numeric vector with associated weights. This function
#' sorts the values and computes weighted cumulative distribution before
#' interpolating the requested quantiles.
#'
#' @param values Numeric vector of values to compute quantiles for.
#' @param quantiles Numeric vector of probabilities with values between 0 and 1.
#' @param .wts Numeric vector of non-negative weights, same length as `values`.
#'
#' @return Numeric vector of weighted quantiles corresponding to the requested probabilities.
#'
#' @examples
#' # Equal weights (same as regular quantiles)
#' weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10))
#'
#' # Weighted towards higher values
#' weighted_quantile(1:10, c(0.25, 0.5, 0.75), 1:10)
#'
#' @export
weighted_quantile <- function(values, quantiles, .wts) {
  # Remove NA values if present
  na_idx <- is.na(values) | is.na(.wts)
  if (any(na_idx)) {
    values <- values[!na_idx]
    .wts <- .wts[!na_idx]
  }

  # Sort values and weights
  sorted <- order(values)
  values <- values[sorted]
  weights <- .wts[sorted]

  # Compute cumulative weights
  cumsum_weights <- cumsum(weights)
  total_weight <- cumsum_weights[length(cumsum_weights)]

  # Normalize to [0, 1]
  normed <- cumsum_weights / total_weight

  # Interpolate quantiles
  stats::approx(normed, values, xout = quantiles, rule = 2)$y
}

#' Compute QQ plot data for weighted and unweighted samples
#'
#' Calculate quantile-quantile data comparing the distribution of a variable
#' between treatment groups. This function computes the quantiles for both
#' groups and returns a tidy data frame suitable for plotting or further analysis.
#'
#' @details
#' This function computes the data needed for quantile-quantile plots by calculating
#' corresponding quantiles from two distributions. The computation uses the inverse
#' of the empirical cumulative distribution function (ECDF). For weighted data,
#' it first computes the weighted ECDF and then inverts it to obtain quantiles.
#'
#' @param .data A data frame containing the variables.
#' @param .var Variable to compute quantiles for. Supports tidyselect syntax.
#' @param .group Column name of treatment/group variable. Supports tidyselect syntax.
#' @param .wts Optional weighting variable(s). Can be unquoted variable names (supports tidyselect syntax.),
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes. Default is NULL (unweighted).
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param include_observed Logical. If using `.wts`, also compute observed
#'   (unweighted) quantiles? Defaults to TRUE.
#' @param treatment_level The reference treatment level to use for comparisons.
#'   If `NULL` (default), uses the last level for factors or the maximum value for numeric variables.
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#'
#' @return A tibble with columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{quantile}{Numeric. The quantile probability (0-1).}
#'   \item{treated_quantiles}{Numeric. The quantile value for the treatment group.}
#'   \item{untreated_quantiles}{Numeric. The quantile value for the control group.}
#'
#' @examples
#' # Basic QQ data (observed only)
#' qq(nhefs_weights, age, qsmk)
#'
#' # With weighting
#' qq(nhefs_weights, age, qsmk, .wts = w_ate)
#'
#' # Compare multiple weighting schemes
#' qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))
#' 
#' @export
qq <- function(
  .data,
  .var,
  .group,
  .wts = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  treatment_level = NULL,
  na.rm = FALSE
) {
  # Handle both quoted and unquoted column names
  var_quo <- rlang::enquo(.var)
  group_quo <- rlang::enquo(.group)
  wts_quo <- rlang::enquo(.wts)

  var_name <- get_column_name(var_quo, ".var")
  group_name <- get_column_name(group_quo, ".group")

  # Validate inputs
  if (!var_name %in% names(.data)) {
    abort("Column {.code {var_name}} not found in data")
  }

  if (!group_name %in% names(.data)) {
    abort("Column {.code {group_name}} not found in data")
  }

  # Get weight column names using tidyselect
  wt_names <- if (!rlang::quo_is_null(wts_quo)) {
    names(tidyselect::eval_select(wts_quo, .data))
  } else {
    character(0)
  }

  # Get group levels
  group_var <- .data[[group_name]]
  group_levels <- if (is.factor(group_var)) {
    levels(group_var)
  } else {
    sort(unique(group_var[!is.na(group_var)]))
  }

  if (length(group_levels) != 2) {
    abort("Group variable must have exactly 2 levels")
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
    abort("{.arg treatment_level} '{treatment_level}' not found in {.arg .group} levels: {.val {group_levels}}")
  }

  # Determine reference and comparison groups
  ref_group <- treatment_level
  comp_group <- setdiff(group_levels, treatment_level)

  # Create list of methods to compute
  methods <- character(0)
  if (include_observed || length(wt_names) == 0) {
    methods <- c(methods, "observed")
  }
  if (length(wt_names) > 0) {
    methods <- c(methods, wt_names)
  }

  # Compute quantiles for each method
  qq_data <- purrr::map_df(
    methods,
    compute_method_quantiles,
    .data = .data,
    var_name = var_name,
    group_name = group_name,
    ref_group = ref_group,
    comp_group = comp_group,
    quantiles = quantiles,
    na.rm = na.rm
  )

  # Format method labels
  qq_data$method <- factor(qq_data$method, levels = methods)

  qq_data
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
    treated_quantiles = ref_q,
    untreated_quantiles = comp_q
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

#' Check QQ Data for Multiple Weights
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
#' @param .exposure Column name of treatment/group variable. Supports tidyselect syntax.
#' @param .weights Optional weighting variable(s). Can be unquoted variable names (supports tidyselect syntax),
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes. Default is NULL (unweighted).
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param include_observed Logical. If using `.weights`, also compute observed
#'   (unweighted) quantiles? Defaults to TRUE.
#' @param .reference_level The reference treatment level to use for comparisons.
#'   If `NULL` (default), uses the last level for factors or the maximum value for numeric variables.
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#'
#' @return A tibble with class "halfmoon_qq" containing columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{quantile}{Numeric. The quantile probability (0-1).}
#'   \item{exposed_quantiles}{Numeric. The quantile value for the exposed group.}
#'   \item{unexposed_quantiles}{Numeric. The quantile value for the unexposed group.}
#'
#' @family balance functions
#' @seealso [bal_qq()] for single weight QQ data, [plot_qq()] for visualization
#' @examples
#' # Basic QQ data (observed only)
#' check_qq(nhefs_weights, age, qsmk)
#'
#' # With weighting
#' check_qq(nhefs_weights, age, qsmk, .weights = w_ate)
#'
#' # Compare multiple weighting schemes
#' check_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))
#'
#' @export
check_qq <- function(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  .reference_level = NULL,
  na.rm = FALSE
) {
  # Handle both quoted and unquoted column names
  var_quo <- rlang::enquo(.var)
  exposure_quo <- rlang::enquo(.exposure)
  wts_quo <- rlang::enquo(.weights)

  var_name <- get_column_name(var_quo, ".var")
  exposure_name <- get_column_name(exposure_quo, ".exposure")

  # Validate inputs
  if (!var_name %in% names(.data)) {
    abort(
      "Column {.code {var_name}} not found in data",
      error_class = "halfmoon_column_error"
    )
  }

  if (!exposure_name %in% names(.data)) {
    abort(
      "Column {.code {exposure_name}} not found in data",
      error_class = "halfmoon_column_error"
    )
  }

  # Get weight column names using tidyselect
  wt_names <- if (!rlang::quo_is_null(wts_quo)) {
    names(tidyselect::eval_select(wts_quo, .data))
  } else {
    character(0)
  }

  # Get group levels
  exposure_var <- .data[[exposure_name]]
  exposure_levels <- if (is.factor(exposure_var)) {
    levels(exposure_var)
  } else {
    sort(unique(exposure_var[!is.na(exposure_var)]))
  }

  if (length(exposure_levels) != 2) {
    abort(
      "Exposure variable must have exactly 2 levels",
      error_class = "halfmoon_group_error"
    )
  }

  # Check for missing values if na.rm = FALSE
  if (!na.rm) {
    var_data <- .data[[var_name]]
    if (any(is.na(var_data))) {
      abort(
        "Variable {.code {var_name}} contains missing values and {.arg na.rm = FALSE}",
        error_class = "halfmoon_na_error"
      )
    }
    if (any(is.na(exposure_var))) {
      abort(
        "Exposure variable {.code {exposure_name}} contains missing values and {.arg na.rm = FALSE}",
        error_class = "halfmoon_na_error"
      )
    }
  }

  # Handle NULL .reference_level
  if (is.null(.reference_level)) {
    if (is.factor(exposure_var)) {
      # For factors, use the last level
      .reference_level <- exposure_levels[length(exposure_levels)]
    } else {
      # For numeric, use the maximum value
      .reference_level <- max(exposure_levels)
    }
  }

  # Validate .reference_level exists
  if (!.reference_level %in% exposure_levels) {
    abort(
      "{.arg .reference_level} '{(.reference_level)}' not found in {.arg .group} levels: {.val {exposure_levels}}",
      error_class = "halfmoon_reference_error"
    )
  }

  # Determine reference and comparison groups
  ref_group <- .reference_level
  comp_group <- setdiff(exposure_levels, .reference_level)

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
    exposure_name = exposure_name,
    ref_group = ref_group,
    comp_group = comp_group,
    quantiles = quantiles,
    na.rm = na.rm
  )

  # Format method labels
  qq_data$method <- factor(qq_data$method, levels = methods)

  # Add halfmoon_qq class
  class(qq_data) <- c("halfmoon_qq", class(qq_data))

  qq_data
}

#' Compute quantiles for a single method
#'
#' Internal function to compute quantiles for one method (observed or weighted).
#'
#' @param method Character string indicating the method ("observed" or weight column name)
#' @param .data Data frame
#' @param var_name Variable name to compute quantiles for
#' @param exposure_name Group variable name
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
  exposure_name,
  ref_group,
  comp_group,
  quantiles,
  na.rm
) {
  # Filter data by group
  ref_data <- .data[.data[[exposure_name]] == ref_group, ]
  comp_data <- .data[.data[[exposure_name]] == comp_group, ]

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
      abort(
        "Weight column {.code {method}} not found in data",
        error_class = "halfmoon_column_error"
      )
    }

    ref_wts <- ref_data[[method]]
    comp_wts <- comp_data[[method]]

    if (na.rm) {
      ref_wts <- ref_wts[!is.na(ref_data[[var_name]])]
      comp_wts <- comp_wts[!is.na(comp_data[[var_name]])]
    }

    ref_q <- weighted_quantile(ref_vals, quantiles, .weights = ref_wts)
    comp_q <- weighted_quantile(comp_vals, quantiles, .weights = comp_wts)
  }

  dplyr::tibble(
    method = method,
    quantile = quantiles,
    exposed_quantiles = ref_q,
    unexposed_quantiles = comp_q
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
#' @param .weights Numeric vector of non-negative weights, same length as `values`.
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
weighted_quantile <- function(values, quantiles, .weights) {
  # Extract numeric data from weights (handles both numeric and psw objects)
  .wts <- extract_weight_data(.weights)

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

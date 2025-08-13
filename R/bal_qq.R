#' Compute QQ Data for Single Variable and Weight
#'
#' Calculate quantile-quantile data comparing the distribution of a variable
#' between treatment groups for a single weighting scheme (or unweighted).
#' This function computes the quantiles for both groups and returns a data frame
#' suitable for plotting or further analysis.
#'
#' @details
#' This function computes the data needed for quantile-quantile plots by calculating
#' corresponding quantiles from two distributions. The computation uses the inverse
#' of the empirical cumulative distribution function (ECDF). For weighted data,
#' it first computes the weighted ECDF and then inverts it to obtain quantiles.
#'
#' When the distributions of a variable are similar between treatment groups
#' (indicating good balance), the QQ plot points will lie close to the diagonal
#' line y = x.
#'
#' @param .data A data frame containing the variables.
#' @param .var Variable to compute quantiles for (unquoted).
#' @param .exposure Column name of treatment/group variable (unquoted).
#' @param .weights Optional single weight variable (unquoted). If NULL, computes
#'   unweighted quantiles.
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @inheritParams balance_params
#' @inheritParams treatment_param
#'
#' @return A tibble with columns:
#'   \item{quantile}{Numeric. The quantile probability (0-1).}
#'   \item{exposed_quantiles}{Numeric. The quantile value for the exposed group.}
#'   \item{unexposed_quantiles}{Numeric. The quantile value for the unexposed group.}
#'
#' @family balance functions
#' @seealso [check_qq()] for computing QQ data across multiple weights,
#'   [plot_qq()] for visualization
#'
#' @examples
#' # Unweighted QQ data
#' bal_qq(nhefs_weights, age, qsmk)
#'
#' # Weighted QQ data
#' bal_qq(nhefs_weights, age, qsmk, .weights = w_ate)
#'
#' # Custom quantiles
#' bal_qq(nhefs_weights, age, qsmk, .weights = w_ate,
#'        quantiles = seq(0.1, 0.9, 0.1))
#'
#' @export
bal_qq <- function(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  .reference_level = NULL,
  na.rm = FALSE
) {
  # Handle column names
  var_quo <- rlang::enquo(.var)
  exposure_quo <- rlang::enquo(.exposure)
  wts_quo <- rlang::enquo(.weights)

  var_name <- get_column_name(var_quo, ".var")
  exposure_name <- get_column_name(exposure_quo, ".exposure")

  # Validate inputs
  validate_data_frame(.data, call = rlang::caller_env())
  validate_column_exists(.data, var_name, ".var", call = rlang::caller_env())
  validate_column_exists(
    .data,
    exposure_name,
    ".exposure",
    call = rlang::caller_env()
  )

  # Get weight column if provided
  wt_name <- NULL
  if (!rlang::quo_is_null(wts_quo)) {
    wt_names <- names(tidyselect::eval_select(wts_quo, .data))
    if (length(wt_names) != 1) {
      abort(
        "{.arg .weights} must select exactly one variable or be NULL",
        error_class = "halfmoon_arg_error",
        call = rlang::current_env()
      )
    }
    wt_name <- wt_names[1]
  }

  # Get exposure levels
  exposure_var <- .data[[exposure_name]]
  exposure_levels <- extract_group_levels(exposure_var)

  # Validate binary exposure
  validate_binary_group(
    exposure_levels,
    exposure_name,
    call = rlang::caller_env()
  )

  # Check for missing values if na.rm = FALSE
  if (!na.rm) {
    var_data <- .data[[var_name]]
    if (any(is.na(var_data))) {
      abort(
        "Variable {.code {var_name}} contains missing values and {.arg na.rm = FALSE}",
        error_class = "halfmoon_na_error",
        call = rlang::current_env()
      )
    }
    if (any(is.na(exposure_var))) {
      abort(
        "Exposure variable {.code {exposure_name}} contains missing values and {.arg na.rm = FALSE}",
        error_class = "halfmoon_na_error",
        call = rlang::current_env()
      )
    }
  }

  # Handle NULL .reference_level - use same logic as check_qq
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
      "{.arg .reference_level} '{(.reference_level)}' not found in {.arg .exposure} levels: {.val {exposure_levels}}",
      error_class = "halfmoon_reference_error",
      call = rlang::current_env()
    )
  }

  # Use .reference_level as reference group (same as check_qq)
  ref_group <- .reference_level
  comp_group <- setdiff(exposure_levels, .reference_level)

  # Filter data by group
  ref_data <- .data[exposure_var == ref_group, ]
  comp_data <- .data[exposure_var == comp_group, ]

  if (na.rm) {
    ref_data <- ref_data[!is.na(ref_data[[var_name]]), ]
    comp_data <- comp_data[!is.na(comp_data[[var_name]]), ]
  }

  # Get values
  ref_vals <- ref_data[[var_name]]
  comp_vals <- comp_data[[var_name]]

  # Compute quantiles
  if (is.null(wt_name)) {
    # Standard quantiles
    ref_q <- stats::quantile(ref_vals, probs = quantiles, na.rm = FALSE)
    comp_q <- stats::quantile(comp_vals, probs = quantiles, na.rm = FALSE)
  } else {
    # Weighted quantiles
    ref_wts <- extract_weight_data(ref_data[[wt_name]])
    comp_wts <- extract_weight_data(comp_data[[wt_name]])

    if (na.rm) {
      ref_wts <- ref_wts[!is.na(ref_data[[var_name]])]
      comp_wts <- comp_wts[!is.na(comp_data[[var_name]])]
    }

    ref_q <- weighted_quantile(ref_vals, quantiles, .weights = ref_wts)
    comp_q <- weighted_quantile(comp_vals, quantiles, .weights = comp_wts)
  }

  # Return tibble
  tibble::tibble(
    quantile = quantiles,
    exposed_quantiles = ref_q,
    unexposed_quantiles = comp_q
  )
}

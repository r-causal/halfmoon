#' Calculate Single ROC Curve for Model Balance Assessment
#'
#' Computes the Receiver Operating Characteristic (ROC) curve for a single
#' weighting scheme or unweighted data. In causal inference, an ROC curve
#' near the diagonal indicates good balance between treatment groups.
#'
#' @details
#' The ROC curve plots sensitivity (true positive rate) against 1-specificity
#' (false positive rate) across all possible threshold values. When propensity
#' scores achieve perfect balance, the ROC curve should lie close to the diagonal
#' line from (0,0) to (1,1), indicating that the propensity scores have no
#' discriminatory ability between treatment groups.
#'
#' ROC curves that bow significantly above the diagonal indicate that the
#' propensity scores can still distinguish between treatment groups, suggesting
#' inadequate balance.
#'
#' @param .data A data frame containing the variables.
#' @param .exposure The treatment/outcome variable (unquoted).
#' @param .estimate The propensity score or fitted values (unquoted).
#' @param .weights Optional single weight variable (unquoted). If NULL, computes
#'   unweighted ROC curve.
#' @inheritParams balance_params
#' @inheritParams treatment_param
#'
#' @return A tibble with columns:
#'   \item{threshold}{Numeric. The decision threshold.}
#'   \item{sensitivity}{Numeric. True positive rate at the threshold.}
#'   \item{specificity}{Numeric. True negative rate at the threshold.}
#'
#' @family balance functions
#' @seealso [check_model_roc_curve()] for computing ROC curves across multiple weights,
#'   [bal_model_auc()] for the area under the curve summary
#'
#' @examples
#' # Unweighted ROC curve
#' bal_model_roc_curve(nhefs_weights, qsmk, .fitted)
#'
#' # Weighted ROC curve
#' bal_model_roc_curve(nhefs_weights, qsmk, .fitted, w_ate)
#'
#' @export
bal_model_roc_curve <- function(
  .data,
  .exposure,
  .estimate,
  .weights = NULL,
  na.rm = TRUE,
  .focal_level = NULL
) {
  validate_data_frame(.data, call = rlang::caller_env())

  exposure_quo <- rlang::enquo(.exposure)
  estimate_quo <- rlang::enquo(.estimate)
  wts_quo <- rlang::enquo(.weights)

  # Extract column names
  exposure_name <- names(tidyselect::eval_select(exposure_quo, .data))
  estimate_name <- names(tidyselect::eval_select(estimate_quo, .data))

  if (length(exposure_name) != 1) {
    abort(
      "{.arg .exposure} must select exactly one variable",
      error_class = "halfmoon_arg_error",
      call = rlang::current_env()
    )
  }
  if (length(estimate_name) != 1) {
    abort(
      "{.arg .estimate} must select exactly one variable",
      error_class = "halfmoon_arg_error",
      call = rlang::current_env()
    )
  }

  # Extract data
  exposure <- .data[[exposure_name]]
  estimate <- .data[[estimate_name]]

  # Handle weights if provided
  weights <- NULL
  if (!rlang::quo_is_null(wts_quo)) {
    weight_vars <- names(tidyselect::eval_select(wts_quo, .data))
    if (length(weight_vars) != 1) {
      abort(
        "{.arg .weights} must select exactly one variable or be NULL",
        error_class = "halfmoon_arg_error",
        call = rlang::current_env()
      )
    }
    weights <- extract_weight_data(.data[[weight_vars[1]]])
  }

  # Handle missing values
  if (na.rm) {
    if (is.null(weights)) {
      complete_cases <- stats::complete.cases(exposure, estimate)
    } else {
      complete_cases <- stats::complete.cases(exposure, estimate, weights)
    }
    exposure <- exposure[complete_cases]
    estimate <- estimate[complete_cases]
    if (!is.null(weights)) {
      weights <- weights[complete_cases]
    }
  } else {
    if (is.null(weights)) {
      na_present <- any(is.na(exposure)) || any(is.na(estimate))
    } else {
      na_present <- any(is.na(exposure)) ||
        any(is.na(estimate)) ||
        any(is.na(weights))
    }
    if (na_present) {
      return(tibble::tibble(
        threshold = NA_real_,
        sensitivity = NA_real_,
        specificity = NA_real_
      ))
    }
  }

  # Compute and return ROC curve
  compute_roc_curve_imp(
    exposure,
    estimate,
    weights = weights,
    .focal_level = .focal_level,
    call = rlang::current_env()
  )
}

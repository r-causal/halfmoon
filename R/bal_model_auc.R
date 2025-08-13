#' Calculate Single AUC for Model Balance Assessment
#'
#' Computes the Area Under the ROC Curve (AUC) for a single weighting scheme
#' or unweighted data. In causal inference, an AUC around 0.5 indicates good
#' balance between treatment groups.
#'
#' @details
#' The AUC provides a single metric for assessing propensity score balance.
#' When propensity scores achieve perfect balance, the weighted distribution
#' of scores should be identical between treatment groups, resulting in an
#' AUC of 0.5 (chance performance).
#'
#' AUC values significantly different from 0.5 indicate systematic differences
#' in propensity score distributions between groups, suggesting inadequate
#' balance.
#'
#' @param .data A data frame containing the variables.
#' @param .exposure The treatment/outcome variable (unquoted).
#' @param .estimate The propensity score or fitted values (unquoted).
#' @param .wts Optional single weight variable (unquoted). If NULL, computes
#'   unweighted AUC.
#' @inheritParams balance_params
#' @inheritParams treatment_param
#'
#' @return A numeric value representing the AUC. Values around 0.5 indicate
#'   good balance, while values closer to 0 or 1 indicate poor balance.
#'
#' @family balance functions
#' @seealso [check_model_auc()] for computing AUC across multiple weights,
#'   [bal_model_roc_curve()] for the full ROC curve
#'
#' @examples
#' # Unweighted AUC
#' bal_model_auc(nhefs_weights, qsmk, .fitted)
#'
#' # Weighted AUC
#' bal_model_auc(nhefs_weights, qsmk, .fitted, w_ate)
#'
#' @export
bal_model_auc <- function(
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
      return(NA_real_)
    }
  }

  # Compute ROC curve
  roc_data <- compute_roc_curve_imp(
    exposure,
    estimate,
    weights = weights,
    .focal_level = .focal_level,
    call = rlang::current_env()
  )

  # Calculate AUC using trapezoidal rule
  fpr <- 1 - roc_data$specificity
  tpr <- roc_data$sensitivity

  compute_auc(fpr, tpr)
}

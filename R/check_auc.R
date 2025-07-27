#' Check Balance Using Weighted ROC Curves
#'
#' Computes weighted ROC curves and AUC for evaluating propensity score balance.
#' In causal inference, a weighted ROC curve near the diagonal (AUC around 0.5)
#' indicates good balance between treatment groups.
#'
#' @param .data A data frame containing the variables.
#' @param .truth The treatment/outcome variable.
#' @param .estimate The propensity score or fitted values.
#' @param .wts Weighting variables (supports tidyselect).
#' @param include_observed Include unweighted results? Default `TRUE`.
#' @param na.rm Remove missing values? Default `TRUE`.
#' @param treatment_level The level of `.truth` to consider as the treatment/event.
#'   Default is NULL, which uses the second level.
#'
#' @return A tibble with columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{auc}{Numeric. The ROC AUC value.}
#'
#' @examples
#' # Check balance for propensity scores
#' check_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))
#'
#' # Without observed results
#' check_auc(nhefs_weights, qsmk, .fitted, w_ate, include_observed = FALSE)
#'
#' @export
check_auc <- function(
  .data,
  .truth,
  .estimate,
  .wts,
  include_observed = TRUE,
  na.rm = TRUE,
  treatment_level = NULL
) {
  if (!is.data.frame(.data)) {
    abort("{.arg .data} must be a data frame")
  }

  roc_data <- roc_curve(
    .data,
    {{ .truth }},
    {{ .estimate }},
    {{ .wts }},
    include_observed,
    na.rm,
    treatment_level
  )

  # Calculate AUC for each method
  methods <- unique(roc_data$method)
  auc_results <- purrr::map_dfr(methods, function(method) {
    method_data <- dplyr::filter(roc_data, .data$method == !!method)

    # Calculate AUC using trapezoidal rule
    fpr <- 1 - method_data$specificity
    tpr <- method_data$sensitivity
    auc_val <- compute_auc(fpr, tpr)

    tibble::tibble(
      method = method,
      auc = auc_val
    )
  })

  auc_results
}


#' ROC Curve for Causal Inference
#'
#' Computes ROC curves (weighted or unweighted) for evaluating propensity score balance.
#' In causal inference, an ROC curve near the diagonal (AUC around 0.5)
#' indicates good balance between treatment groups.
#'
#' @param .data A data frame containing the variables.
#' @param .truth The treatment/outcome variable (unquoted).
#' @param .estimate The propensity score or covariate (unquoted).
#' @param .wts Optional weighting variables (unquoted, can be multiple).
#' @param include_observed Include unweighted results? Default TRUE.
#' @param na.rm Remove missing values? Default TRUE.
#' @param treatment_level The level of `.truth` to consider as the treatment/event.
#'   Default is NULL, which uses the second level.
#'
#' @return A tibble with ROC curve data.
#' @export
roc_curve <- function(
  .data,
  .truth,
  .estimate,
  .wts = NULL,
  include_observed = TRUE,
  na.rm = TRUE,
  treatment_level = NULL
) {
  truth_quo <- rlang::enquo(.truth)
  estimate_quo <- rlang::enquo(.estimate)
  wts_quo <- rlang::enquo(.wts)

  if (!is.data.frame(.data)) {
    abort("{.arg .data} must be a data frame")
  }

  # Extract column names
  truth_name <- names(tidyselect::eval_select(truth_quo, .data))
  estimate_name <- names(tidyselect::eval_select(estimate_quo, .data))

  if (length(truth_name) != 1) {
    abort("{.arg .truth} must select exactly one variable")
  }
  if (length(estimate_name) != 1) {
    abort("{.arg .estimate} must select exactly one variable")
  }

  # Handle weights
  if (!rlang::quo_is_null(wts_quo)) {
    weight_vars <- names(tidyselect::eval_select(wts_quo, .data))
  } else {
    weight_vars <- character(0)
  }

  # Extract data
  truth <- .data[[truth_name]]
  estimate <- .data[[estimate_name]]

  # Convert truth to factor if needed
  if (!is.factor(truth)) {
    if (is.character(truth) || is.logical(truth)) {
      truth <- as.factor(truth)
    } else if (is.numeric(truth)) {
      unique_vals <- unique(truth[!is.na(truth)])
      if (length(unique_vals) == 2) {
        truth <- factor(truth, levels = sort(unique_vals))
      } else {
        abort("{.arg .truth} must have exactly 2 unique values")
      }
    } else {
      abort(
        "{.arg .truth} must be a factor, character, logical, or binary numeric"
      )
    }
  }

  if (length(levels(truth)) != 2) {
    abort("{.arg .truth} must have exactly 2 levels")
  }

  if (!is.numeric(estimate)) {
    abort("{.arg .estimate} must be numeric")
  }

  if (na.rm) {
    complete_cases <- stats::complete.cases(truth, estimate)
    if (!all(complete_cases)) {
      n_missing <- sum(!complete_cases)
      cli::cli_inform("Removed {n_missing} rows with missing values")
    }
    truth <- truth[complete_cases]
    estimate <- estimate[complete_cases]
    .data <- .data[complete_cases, , drop = FALSE]
  } else {
    if (any(is.na(truth)) || any(is.na(estimate))) {
      abort("Missing values found and {.code na.rm = FALSE}")
    }
  }

  # Results list
  results <- list()

  # Add observed (unweighted) results
  if (include_observed) {
    observed_curve <- compute_roc_curve_imp(
      truth,
      estimate,
      weights = NULL,
      treatment_level = treatment_level
    )
    observed_curve$method <- "observed"
    results$observed <- observed_curve
  }

  # Add weighted results
  for (wt_name in weight_vars) {
    weights <- .data[[wt_name]]

    # Validate weights
    if (!is.numeric(weights)) {
      warn("Skipping non-numeric weight variable: {wt_name}")
      next
    }

    # Handle zero and negative weights
    if (any(weights <= 0, na.rm = TRUE)) {
      n_zero_neg <- sum(weights <= 0, na.rm = TRUE)
      cli::cli_inform(
        "Removing {n_zero_neg} observations with zero or negative weights from {wt_name}"
      )

      valid_weights <- weights > 0 & !is.na(weights)
      truth_wt <- truth[valid_weights]
      estimate_wt <- estimate[valid_weights]
      weights_wt <- weights[valid_weights]
    } else {
      truth_wt <- truth
      estimate_wt <- estimate
      weights_wt <- weights
    }

    weighted_curve <- compute_roc_curve_imp(
      truth_wt,
      estimate_wt,
      weights = weights_wt,
      treatment_level = treatment_level
    )
    weighted_curve$method <- wt_name
    results[[wt_name]] <- weighted_curve
  }

  # Combine results
  if (length(results) == 0) {
    abort("No valid results to return")
  }

  dplyr::bind_rows(results)
}

compute_roc_curve_imp <- function(
  truth,
  estimate,
  weights = NULL,
  treatment_level = NULL
) {
  if (length(truth) == 0) {
    return(tibble::tibble(
      threshold = numeric(0),
      sensitivity = numeric(0),
      specificity = numeric(0)
    ))
  }

  # Check for constant estimates
  if (length(unique(estimate)) == 1) {
    warn("Estimate variable is constant; ROC curve will be degenerate")
    return(tibble::tibble(
      threshold = c(-Inf, unique(estimate), Inf),
      sensitivity = c(1, 0.5, 0),
      specificity = c(0, 0.5, 1)
    ))
  }

  # Set default weights
  if (is.null(weights)) {
    weights <- rep(1, length(truth))
  }

  # Convert to binary (1 = event, 0 = non-event)
  # Determine which level is the treatment/event
  if (is.factor(truth)) {
    truth_levels <- levels(truth)
    if (!is.null(treatment_level)) {
      # User specified treatment level
      if (!treatment_level %in% truth_levels) {
        abort(
          "{.arg treatment_level} '{treatment_level}' not found in {.arg truth} levels: {.val {truth_levels}}"
        )
      }
      event_level <- treatment_level
    } else {
      # Default: use second level as event
      event_level <- truth_levels[[2]]
    }
    truth_binary <- as.integer(truth == event_level)
  } else {
    # For non-factors, determine event level
    unique_vals <- sort(unique(truth))
    if (!is.null(treatment_level)) {
      if (!treatment_level %in% unique_vals) {
        abort(
          "{.arg treatment_level} '{treatment_level}' not found in {.arg truth} values: {.val {unique_vals}}"
        )
      }
      event_level <- treatment_level
    } else {
      # Default: use second unique value as event
      event_level <- unique_vals[[2]]
    }
    truth_binary <- as.integer(truth == event_level)
  }

  # Sort by decreasing estimate
  order_idx <- order(estimate, decreasing = TRUE)
  truth_sorted <- truth_binary[order_idx]
  estimate_sorted <- estimate[order_idx]
  weights_sorted <- weights[order_idx]

  # Get unique thresholds
  unique_idx <- !duplicated(estimate_sorted, fromLast = TRUE)
  thresholds <- estimate_sorted[unique_idx]

  # Calculate weighted TP and FP
  weights_events <- truth_sorted * weights_sorted
  weights_non_events <- (1 - truth_sorted) * weights_sorted

  # Cumulative sums
  tp <- cumsum(weights_events)[unique_idx]
  fp <- cumsum(weights_non_events)[unique_idx]

  # Calculate totals
  total_tp <- sum(weights_events, na.rm = TRUE)
  total_fp <- sum(weights_non_events, na.rm = TRUE)

  # Handle edge cases
  if (is.na(total_tp) || total_tp == 0) {
    warn("No events found in truth variable")
    sensitivity <- rep(0, length(tp))
  } else {
    sensitivity <- tp / total_tp
  }

  if (is.na(total_fp) || total_fp == 0) {
    warn("No non-events found in truth variable")
    specificity <- rep(1, length(fp))
  } else {
    specificity <- 1 - fp / total_fp
  }

  # Add endpoints for complete curve
  thresholds <- c(-Inf, rev(thresholds), Inf)
  sensitivity <- c(1, rev(sensitivity), 0)
  specificity <- c(0, rev(specificity), 1)

  tibble::tibble(
    threshold = thresholds,
    sensitivity = sensitivity,
    specificity = specificity
  )
}

compute_auc <- function(x, y) {
  # Handle edge cases
  if (length(x) != length(y)) {
    abort("Length mismatch: x ({length(x)}) and y ({length(y)})")
  }

  if (length(x) == 0) {
    return(NA_real_)
  }

  if (length(x) == 1) {
    return(0.5) # Single point, assume balanced
  }

  # Remove missing values
  complete_cases <- stats::complete.cases(x, y)
  x <- x[complete_cases]
  y <- y[complete_cases]

  if (length(x) < 2) {
    return(NA_real_)
  }

  # Ensure x is sorted
  if (is.unsorted(x)) {
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
  }

  # Trapezoidal rule
  n <- length(x)
  dx <- x[-1] - x[-n]
  height <- (y[-n] + y[-1]) / 2

  auc <- sum(height * dx, na.rm = TRUE)

  # Ensure AUC is in [0, 1]
  auc <- max(0, min(1, auc))

  auc
}

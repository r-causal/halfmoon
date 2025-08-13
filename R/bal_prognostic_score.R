#' Compute Prognostic Scores for Balance Assessment
#'
#' Calculates prognostic scores by fitting an outcome model on the control group
#' and generating predictions for all observations. Prognostic scores represent
#' the expected outcome under control conditions and are useful for assessing
#' whether balance on treatment predictors ensures balance on outcome-relevant
#' variables.
#'
#' @details
#' The prognostic score method, introduced by Stuart et al. (2013), provides a
#' way to assess balance that focuses on variables predictive of the outcome
#' rather than just the treatment assignment. The procedure:
#'
#' 1. Fits an outcome model using only control group observations
#' 2. Generates predictions (prognostic scores) for all observations
#' 3. Returns these scores for balance assessment
#'
#' This approach is particularly useful when:
#' - The outcome model includes non-linearities or interactions
#' - You want to ensure balance on outcome-relevant variables
#' - Traditional propensity score balance checks may miss important imbalances
#'
#' The returned prognostic scores can be used with existing balance functions
#' like `bal_smd()`, `bal_vr()`, or `check_balance()` to assess balance between
#' treatment groups.
#'
#' @param .data A data frame containing the variables for analysis
#' @param outcome The outcome variable. Can be specified as a bare name or
#'   character string. Ignored if `formula` is provided.
#' @param .covariates Variables to include in the outcome model. Defaults to
#'   `everything()` which includes all variables except the outcome and treatment.
#'   Supports tidyselect syntax. Ignored if `formula` is provided.
#' @param .exposure The treatment variable. Can be specified as a bare name or
#'   character string.
#' @param formula Optional formula for the outcome model. If provided, `outcome`
#'   and `.covariates` arguments are ignored. The formula should not include the
#'   treatment variable.
#' @param .reference_level The level of treatment that represents the control group.
#'   If NULL (default), the first level is used as control.
#' @param family A family object for the GLM. Defaults to `gaussian()` for
#'   continuous outcomes. Use `binomial()` for binary outcomes.
#' @param .weights Optional weights for the outcome model. Can be a numeric vector
#'   or bare name of a variable in `.data`.
#' @param na.rm Logical. Should missing values be removed? Defaults to FALSE.
#' @param ... Additional arguments passed to `glm()`.
#'
#' @return A numeric vector of prognostic scores with the same length as the
#'   number of rows in `.data` (after NA removal if `na.rm = TRUE`).
#'
#' @references
#' Stuart EA, Lee BK, Leacy FP. Prognostic score-based balance measures can be
#' a useful diagnostic for propensity score methods in comparative effectiveness
#' research. J Clin Epidemiol. 2013;66(8):S84-S90.
#'
#' @examples
#' # Using tidyselect interface
#' prog_scores <- bal_prognostic_score(
#'   nhefs_weights,
#'   outcome = wt82_71,
#'   .exposure = qsmk,
#'   .covariates = c(age, sex, race, wt71)
#' )
#'
#' # Using formula interface
#' prog_scores_formula <- bal_prognostic_score(
#'   nhefs_weights,
#'   .exposure = qsmk,
#'   formula = wt82_71 ~ age + sex + race + wt71 + I(age^2)
#' )
#'
#' # Add to data and check balance
#' nhefs_with_prog <- nhefs_weights
#' nhefs_with_prog$prog_score <- prog_scores
#' check_balance(nhefs_with_prog, prog_score, qsmk, .weights = w_ate)
#'
#' # For binary outcome
#' prog_scores_binary <- bal_prognostic_score(
#'   nhefs_weights,
#'   outcome = death,
#'   .exposure = qsmk,
#'   family = binomial()
#' )
#'
#' @family balance
#' @importFrom stats gaussian
#' @export
bal_prognostic_score <- function(
  .data,
  outcome = NULL,
  .covariates = everything(),
  .exposure,
  formula = NULL,
  .reference_level = NULL,
  family = gaussian(),
  .weights = NULL,
  na.rm = FALSE,
  ...
) {
  validate_data_frame(.data)

  exposure_quo <- rlang::enquo(.exposure)
  exposure_var <- get_column_name(exposure_quo, ".exposure")
  validate_column_exists(.data, exposure_var)

  # Handle formula vs tidyselect interface
  if (!is.null(formula)) {
    # Formula interface
    if (!inherits(formula, "formula")) {
      abort(
        "`formula` must be a formula object.",
        error_class = "halfmoon_formula_error",
        call = rlang::current_env()
      )
    }

    # Extract variables from formula
    formula_vars <- all.vars(formula)
    outcome_var <- formula_vars[1] # LHS of formula

    # Check that treatment is not in the formula
    if (exposure_var %in% formula_vars) {
      abort(
        paste0(
          "The treatment variable '",
          exposure_var,
          "' should not be included in the outcome model formula."
        ),
        error_class = "halfmoon_formula_error",
        call = rlang::current_env()
      )
    }

    # Validate outcome exists
    validate_column_exists(.data, outcome_var)

    model_formula <- formula
  } else {
    # Tidyselect interface
    # Handle outcome using get_column_name helper
    outcome_quo <- rlang::enquo(outcome)

    # Check if outcome was provided using quo_is_null
    if (rlang::quo_is_null(outcome_quo)) {
      abort(
        "Either `outcome` or `formula` must be provided.",
        error_class = "halfmoon_arg_error",
        call = rlang::current_env()
      )
    }

    outcome_var <- get_column_name(outcome_quo, "outcome")
    validate_column_exists(.data, outcome_var)

    # Get covariate names
    covariate_enquo <- rlang::enquo(.covariates)
    all_vars <- names(.data)
    # Remove outcome and exposure from available variables for selection
    available_vars <- setdiff(all_vars, c(outcome_var, exposure_var))
    covariate_names <- names(tidyselect::eval_select(
      covariate_enquo,
      .data[available_vars]
    ))

    if (length(covariate_names) == 0) {
      abort(
        "No covariates selected for the outcome model.",
        error_class = "halfmoon_empty_error",
        call = rlang::current_env()
      )
    }

    # Create formula
    formula_str <- paste0(
      outcome_var,
      " ~ ",
      paste(covariate_names, collapse = " + ")
    )
    model_formula <- stats::as.formula(formula_str)
  }

  # Handle weights
  weights_enquo <- rlang::enquo(.weights)
  if (!rlang::quo_is_null(weights_enquo)) {
    # Try to get column name first (handles both quoted and unquoted)
    weights_var <- tryCatch(
      {
        get_column_name(weights_enquo, ".weights")
      },
      error = function(e) NULL
    )

    if (!is.null(weights_var) && weights_var %in% names(.data)) {
      # It's a column reference
      validate_column_exists(.data, weights_var)
      model_weights <- .data[[weights_var]]
    } else {
      # It's a numeric vector
      model_weights <- rlang::eval_tidy(weights_enquo)
      validate_weights(model_weights, nrow(.data))
    }
  } else {
    model_weights <- NULL
  }

  # Determine control group
  exposure_levels <- extract_group_levels(.data[[exposure_var]])
  control_level <- determine_reference_group(
    exposure_levels,
    .reference_level
  )

  # Create control group indicator
  is_control <- .data[[exposure_var]] == control_level

  # Handle NAs if requested
  if (na.rm) {
    # Get all variables used in the model
    model_vars <- unique(c(all.vars(model_formula), exposure_var))
    complete_idx <- stats::complete.cases(.data[model_vars])

    if (!is.null(model_weights)) {
      complete_idx <- complete_idx & !is.na(model_weights)
    }

    .data <- .data[complete_idx, ]
    is_control <- is_control[complete_idx]

    if (!is.null(model_weights)) {
      model_weights <- model_weights[complete_idx]
    }
  }

  # Check we have control observations
  n_control <- sum(is_control, na.rm = TRUE)
  if (n_control == 0) {
    abort(
      paste0(
        "No control observations found. ",
        "Control level '",
        control_level,
        "' not present in treatment variable."
      ),
      error_class = "halfmoon_reference_error"
    )
  }

  # Fit prognostic model on control group only
  prognostic_scores <- bal_prognostic_fit_model(
    .data = .data,
    model_formula = model_formula,
    is_control = is_control,
    family = family,
    .weights = model_weights,
    ...
  )

  prognostic_scores
}

bal_prognostic_fit_model <- function(
  .data,
  model_formula,
  is_control,
  family,
  .weights = NULL,
  ...
) {
  # Subset to control group for fitting
  control_data <- .data[is_control, ]

  if (!is.null(.weights)) {
    .weights <- .weights[is_control]
  }

  model <- tryCatch(
    {
      if (!is.null(.weights)) {
        stats::glm(
          formula = model_formula,
          data = control_data,
          family = family,
          weights = .weights,
          ...
        )
      } else {
        stats::glm(
          formula = model_formula,
          data = control_data,
          family = family,
          ...
        )
      }
    },
    error = function(e) {
      abort(
        paste0(
          "Failed to fit prognostic score model: ",
          e$message
        ),
        error_class = "halfmoon_formula_error"
      )
    }
  )

  if (!model$converged) {
    warn(
      "Prognostic score model did not converge. Results may be unreliable.",
      warning_class = "halfmoon_convergence_warning"
    )
  }

  # Generate predictions for all observations
  predictions <- tryCatch(
    {
      stats::predict(model, newdata = .data, type = "response")
    },
    error = function(e) {
      abort(
        paste0(
          "Failed to generate prognostic score predictions: ",
          e$message
        ),
        error_class = "halfmoon_formula_error"
      )
    }
  )

  as.numeric(predictions)
}

#' Check Balance Across Multiple Metrics
#'
#' Computes balance statistics for multiple variables across different groups and
#' optional weighting schemes. This function generalizes balance checking by
#' supporting multiple metrics (SMD, variance ratio, Kolmogorov-Smirnov, weighted correlation) and
#' returns results in a tidy format.
#'
#' @param .data A data frame containing the variables to analyze.
#' @param .vars Variables for which to calculate balance metrics. Can be unquoted
#'   variable names, a character vector, or a tidyselect expression.
#' @param .group Grouping variable, e.g., treatment or exposure group.
#' @param .wts Optional weighting variables. Can be unquoted variable names,
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes.
#' @param .metrics Character vector specifying which metrics to compute.
#'   Available options: "smd" (standardized mean difference), "vr" (variance ratio),
#'   "ks" (Kolmogorov-Smirnov), "correlation" (for continuous exposures),
#'   "energy" (multivariate energy distance). Defaults to c("smd", "vr", "ks", "energy").
#' @param include_observed Logical. If using `.wts`, also calculate observed
#'   metrics? Defaults to TRUE.
#' @param reference_group The reference group level to use for comparisons.
#'   Defaults to 1 (first level).
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. Defaults to FALSE.
#' @param make_dummy_vars Logical. Transform categorical variables to dummy
#'   variables using `model.matrix()`? Defaults to TRUE. When TRUE, categorical
#'   variables are expanded into separate binary indicators for each level.
#' @param squares Logical. Include squared terms for continuous variables?
#'   Defaults to FALSE. When TRUE, adds squared versions of numeric variables.
#' @param cubes Logical. Include cubed terms for continuous variables?
#'   Defaults to FALSE. When TRUE, adds cubed versions of numeric variables.
#' @param interactions Logical. Include all pairwise interactions between
#'   variables? Defaults to FALSE. When TRUE, creates interaction terms for
#'   all variable pairs, excluding interactions between levels of the same
#'   categorical variable and between squared/cubed terms.
#'
#' @return A tibble with columns:
#'   \item{variable}{Character. The variable name being analyzed.}
#'   \item{group_level}{Character. The non-reference group level.}
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{metric}{Character. The balance metric computed ("smd", "vr", "ks").}
#'   \item{estimate}{Numeric. The computed balance statistic.}
#'
#' @examples
#' # Basic usage with all metrics
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = c(w_ate, w_att))
#'
#' # With specific metrics only
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .metrics = c("smd", "energy"))
#'
#' # Exclude observed results
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = w_ate, include_observed = FALSE)
#'
#' # Use correlation for continuous exposure
#' check_balance(mtcars, c(mpg, hp), disp, .metrics = c("correlation", "energy"))
#'
#' # With dummy variables for categorical variables (default behavior)
#' check_balance(nhefs_weights, c(age, sex, race), qsmk)
#'
#' # Without dummy variables for categorical variables
#' check_balance(nhefs_weights, c(age, sex, race), qsmk, make_dummy_vars = FALSE)
#' @export
check_balance <- function(
  .data,
  .vars,
  .group,
  .wts = NULL,
  .metrics = c("smd", "vr", "ks", "energy"),
  include_observed = TRUE,
  reference_group = 1L,
  na.rm = FALSE,
  make_dummy_vars = TRUE,
  squares = FALSE,
  cubes = FALSE,
  interactions = FALSE
) {
  if (!is.data.frame(.data)) {
    stop("Argument '.data' must be a data frame")
  }

  # Convert inputs to character vectors for consistent handling
  group_var <- rlang::as_name(rlang::enquo(.group))
  var_names <- names(tidyselect::eval_select(rlang::enquo(.vars), .data))

  if (length(var_names) == 0) {
    stop("No variables selected for '.vars'")
  }

  transformed_data <- .data
  original_vars <- var_names

  if (make_dummy_vars || squares || cubes || interactions) {
    # Extract just the variables we're working with
    vars_data <- dplyr::select(.data, dplyr::all_of(var_names))

    # Create dummy variables if requested
    if (make_dummy_vars) {
      vars_data <- create_dummy_variables(vars_data, binary_as_single = TRUE)
    }

    # Add squared terms if requested
    if (squares) {
      numeric_vars <- purrr::map_lgl(vars_data, is.numeric)
      if (any(numeric_vars)) {
        numeric_data <- dplyr::select(vars_data, dplyr::where(is.numeric))
        squared_data <- dplyr::mutate(
          numeric_data,
          dplyr::across(everything(), ~ .x^2, .names = "{.col}_squared")
        )
        vars_data <- dplyr::bind_cols(
          vars_data,
          dplyr::select(squared_data, dplyr::ends_with("_squared"))
        )
      }
    }

    # Add cubed terms if requested
    if (cubes) {
      numeric_vars <- purrr::map_lgl(vars_data, is.numeric)
      if (any(numeric_vars)) {
        numeric_data <- dplyr::select(vars_data, dplyr::where(is.numeric))
        # Only cube original variables, not squared ones
        original_numeric <- dplyr::select(
          numeric_data,
          -dplyr::ends_with("_squared")
        )
        if (ncol(original_numeric) > 0) {
          cubed_data <- dplyr::mutate(
            original_numeric,
            dplyr::across(everything(), ~ .x^3, .names = "{.col}_cubed")
          )
          vars_data <- dplyr::bind_cols(
            vars_data,
            dplyr::select(cubed_data, dplyr::ends_with("_cubed"))
          )
        }
      }
    }

    # Add interaction terms if requested
    if (interactions) {
      numeric_vars <- purrr::map_lgl(vars_data, is.numeric)
      if (sum(numeric_vars) > 1) {
        numeric_data <- dplyr::select(vars_data, dplyr::where(is.numeric))
        # Only interact original variables, not squared/cubed ones
        original_numeric <- dplyr::select(
          numeric_data,
          -dplyr::ends_with("_squared"),
          -dplyr::ends_with("_cubed")
        )

        if (ncol(original_numeric) > 1) {
          # For interactions with binary categorical variables, we need to expand them
          # Get the original data to check for binary categoricals
          original_vars_data <- dplyr::select(.data, dplyr::all_of(var_names))

          # Identify which numeric variables were originally binary categoricals
          binary_categorical_names <- character()
          if (make_dummy_vars) {
            categorical_check <- purrr::map_lgl(
              original_vars_data,
              function(x) is.factor(x) || is.character(x)
            )

            if (any(categorical_check)) {
              categorical_cols <- original_vars_data[categorical_check]
              binary_check <- purrr::map_lgl(categorical_cols, function(x) {
                n_levels <- if (is.factor(x)) length(levels(x)) else
                  length(unique(x))
                n_levels == 2
              })
              binary_categorical_names <- names(categorical_cols)[binary_check]
            }
          }

          # Prepare variables for interactions
          interaction_vars <- purrr::imap(
            original_numeric,
            function(var_data, var_name) {
              # Check if this was originally a binary categorical
              if (var_name %in% binary_categorical_names) {
                # Need to expand binary to both levels for interactions
                original_var <- original_vars_data[[var_name]]
                levels_to_use <- if (is.factor(original_var)) {
                  levels(original_var)
                } else {
                  sort(unique(original_var))
                }

                # Create dummy for each level
                purrr::map(levels_to_use, function(level) {
                  dummy_name <- paste0(var_name, level)
                  dummy_values <- as.numeric(original_var == level)
                  stats::setNames(list(dummy_values), dummy_name)
                }) |>
                  purrr::flatten()
              } else {
                # Continuous or already expanded multi-level variable
                stats::setNames(list(var_data), var_name)
              }
            }
          ) |>
            purrr::flatten()

          # Now create interactions between all pairs
          var_combinations <- utils::combn(
            names(interaction_vars),
            2,
            simplify = FALSE
          )

          # Filter out same-variable dummy interactions (e.g., sex0 x sex1)
          valid_combinations <- purrr::keep(var_combinations, function(combo) {
            var1 <- combo[1]
            var2 <- combo[2]

            # Extract base variable names (before dummy suffixes)
            base1 <- sub("^([^0-9]+).*", "\\1", var1)
            base2 <- sub("^([^0-9]+).*", "\\1", var2)

            # Only keep interactions between different base variables
            base1 != base2
          })

          # Create interaction terms using functional programming
          interaction_terms <- purrr::map(valid_combinations, function(combo) {
            var1 <- combo[1]
            var2 <- combo[2]
            interaction_name <- paste(var1, var2, sep = "_x_")

            # Return a named list with the interaction term
            interaction_value <- interaction_vars[[var1]] *
              interaction_vars[[var2]]
            stats::setNames(list(interaction_value), interaction_name)
          })

          # Flatten the list and add to vars_data
          interaction_terms <- purrr::flatten(interaction_terms)
          vars_data <- c(vars_data, interaction_terms)
        }
      }
    }

    # Replace the variables in the transformed data
    transformed_data <- transformed_data[,
      !names(transformed_data) %in% original_vars,
      drop = FALSE
    ]
    transformed_data <- dplyr::bind_cols(transformed_data, vars_data)

    # Update var_names to include all transformed variables
    var_names <- names(vars_data)
  }

  # Handle weights using proper NSE - capture quosure and check if null
  .wts <- rlang::enquo(.wts)
  if (!rlang::quo_is_null(.wts)) {
    wts_names <- names(tidyselect::eval_select(.wts, .data))
  } else {
    wts_names <- NULL
  }

  # Validate group variable
  if (!group_var %in% names(transformed_data)) {
    stop("Group variable '", group_var, "' not found in data")
  }

  # Get group levels in proper order
  # For correlation, we allow continuous group variables
  using_correlation <- "correlation" %in% .metrics

  if (is.factor(transformed_data[[group_var]])) {
    group_levels <- levels(transformed_data[[group_var]])
  } else {
    group_levels <- sort(unique(stats::na.omit(transformed_data[[group_var]])))
  }

  # Check group variable requirements based on metrics
  binary_only_metrics <- setdiff(.metrics, c("correlation", "energy"))
  only_correlation <- length(setdiff(.metrics, "correlation")) == 0

  if (length(binary_only_metrics) > 0 && length(group_levels) != 2) {
    stop(
      "Group variable must have exactly two levels for metrics: ",
      paste(binary_only_metrics, collapse = ", "),
      ". Got ",
      length(group_levels),
      " levels."
    )
  }

  if (using_correlation && !is.numeric(transformed_data[[group_var]])) {
    stop(
      "Group variable must be numeric when using correlation metric"
    )
  }

  # Validate metrics
  available_metrics <- c("smd", "vr", "ks", "correlation", "energy")
  invalid_metrics <- setdiff(.metrics, available_metrics)
  if (length(invalid_metrics) > 0) {
    stop(
      "Invalid metrics: ",
      paste(invalid_metrics, collapse = ", "),
      ". Available metrics: ",
      paste(available_metrics, collapse = ", ")
    )
  }

  # Create metric function mapping
  metric_functions <- list(
    smd = bal_smd,
    vr = bal_vr,
    ks = bal_ks,
    correlation = bal_corr,
    energy = bal_energy
  )

  # Determine which methods to include
  methods <- character(0)
  if (include_observed) {
    methods <- c(methods, "observed")
  }
  if (!is.null(wts_names)) {
    methods <- c(methods, wts_names)
  }

  if (length(methods) == 0) {
    stop(
      "No methods to compute. Either set include_observed = TRUE or provide .wts"
    )
  }

  # Separate energy metric from other metrics since it's multivariate
  energy_metrics <- intersect(.metrics, "energy")
  univariate_metrics <- setdiff(.metrics, "energy")

  # Create combinations for univariate metrics
  univariate_combinations <- if (length(univariate_metrics) > 0) {
    tidyr::expand_grid(
      variable = var_names,
      method = methods,
      metric = univariate_metrics
    )
  } else {
    tibble::tibble()
  }

  # Create combinations for energy metric (one per method)
  energy_combinations <- if (length(energy_metrics) > 0) {
    tidyr::expand_grid(
      variable = NA_character_,
      method = methods,
      metric = energy_metrics
    )
  } else {
    tibble::tibble()
  }

  # Combine all combinations
  combinations <- dplyr::bind_rows(univariate_combinations, energy_combinations)

  # Use purrr to compute all balance statistics
  results <- purrr::pmap_dfr(combinations, function(variable, method, metric) {
    # Handle weights
    if (method == "observed") {
      weights_data <- NULL
    } else {
      weights_data <- .data[[method]]
    }

    # Get the appropriate function and compute
    compute_fn <- metric_functions[[metric]]

    # Compute the statistic
    tryCatch(
      {
        if (metric == "energy") {
          # For energy, use all variables
          group_data <- transformed_data[[group_var]]
          covariates_data <- transformed_data[var_names]

          estimate <- compute_fn(
            covariates = covariates_data,
            group = group_data,
            weights = weights_data,
            na.rm = na.rm
          )
        } else if (metric == "correlation") {
          # For correlation, use the group variable as the second variable
          var_data <- transformed_data[[variable]]
          group_data <- transformed_data[[group_var]]

          estimate <- compute_fn(
            x = var_data,
            y = group_data,
            weights = weights_data,
            na.rm = na.rm
          )
        } else {
          # For other metrics (smd, vr, ks)
          var_data <- transformed_data[[variable]]
          group_data <- transformed_data[[group_var]]
          # Handle reference group parameter based on the function
          ref_group_param <- if (metric == "smd") {
            # bal_smd accepts both indices and group values
            reference_group
          } else {
            # bal_vr and bal_ks expect actual group values
            # Always treat numeric reference_group as index for consistency
            if (is.numeric(reference_group) && length(reference_group) == 1) {
              group_levels[reference_group]
            } else {
              reference_group
            }
          }

          estimate <- compute_fn(
            covariate = var_data,
            group = group_data,
            weights = weights_data,
            reference_group = ref_group_param,
            na.rm = na.rm
          )
        }

        # Determine the group level for reporting
        if (metric == "correlation") {
          # For correlation, report the group variable name since it's continuous
          group_level <- group_var
        } else if (metric == "energy") {
          # For energy, use NA since it's multivariate
          group_level <- NA_character_
        } else {
          # For other metrics, determine the non-reference group level
          if (reference_group %in% group_levels) {
            ref_level <- reference_group
          } else {
            # If reference_group is numeric index, get the corresponding level
            ref_level <- group_levels[reference_group]
          }
          group_level <- setdiff(group_levels, ref_level)[1]
        }

        tibble::tibble(
          variable = variable,
          group_level = as.character(group_level),
          method = method,
          metric = metric,
          estimate = estimate
        )
      },
      error = function(e) {
        # Return NA for failed computations but preserve structure
        error_group_level <- if (metric == "correlation") {
          group_var
        } else if (metric == "energy") {
          NA_character_
        } else {
          setdiff(
            group_levels,
            if (reference_group %in% group_levels) reference_group else
              group_levels[reference_group]
          )[1]
        }

        tibble::tibble(
          variable = variable,
          group_level = as.character(error_group_level),
          method = method,
          metric = metric,
          estimate = NA_real_
        )
      }
    )
  })

  # Arrange results for better readability
  if (nrow(results) > 0) {
    results <- dplyr::arrange(results, variable, metric, method)
  }

  return(results)
}

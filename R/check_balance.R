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
#'   Available options: "smd" (standardized mean difference), "variance_ratio",
#'   "ks" (Kolmogorov-Smirnov), "correlation" (for continuous exposures).
#'   Defaults to the first three.
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
#'   \item{metric}{Character. The balance metric computed ("smd", "variance_ratio", "ks").}
#'   \item{estimate}{Numeric. The computed balance statistic.}
#'
#' @examples
#' # Basic usage with all metrics
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = c(w_ate, w_att))
#'
#' # With specific metrics only
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .metrics = c("smd", "ks"))
#'
#' # Exclude observed results
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = w_ate, include_observed = FALSE)
#'
#' # Use correlation for continuous exposure
#' check_balance(mtcars, c(mpg, hp), disp, .metrics = "correlation")
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
  .metrics = c("smd", "variance_ratio", "ks"),
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
  var_names <- names(dplyr::select(.data, {{ .vars }}))

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
      # Identify categorical variables (factors and character variables)
      categorical_vars <- purrr::map_lgl(
        vars_data,
        function(x) is.factor(x) || is.character(x)
      )

      if (any(categorical_vars)) {
        # Create dummy variables for categorical variables
        categorical_data <- dplyr::select(
          vars_data,
          dplyr::where(function(x) is.factor(x) || is.character(x))
        )

        # Create dummy variables using functional programming to ensure all levels are included
        if (ncol(categorical_data) > 0) {
          # Create dummy variables, treating binary variables differently
          dummy_vars <- purrr::imap(categorical_data, function(col_data, col_name) {
            # Get levels based on variable type
            levels_to_use <- if (is.factor(col_data)) {
              levels(col_data)
            } else {
              sort(unique(col_data))
            }
            
            # For binary variables (2 levels), keep as single variable (like cobalt)
            # For multi-level variables (3+ levels), create dummy for each level
            if (length(levels_to_use) == 2) {
              # Binary variable: keep as single variable, convert to 0/1 numeric
              # Use the factor levels directly, converting to numeric 0/1
              dummy_values <- as.numeric(col_data) - 1  # Convert to 0/1 from 1/2
              stats::setNames(list(dummy_values), col_name)
            } else {
              # Multi-level variable: create dummy for each level
              dummy_names <- paste0(col_name, levels_to_use)
              dummy_values <- purrr::map(levels_to_use, ~ as.numeric(col_data == .x))
              stats::setNames(dummy_values, dummy_names)
            }
          })
          
          # Flatten and convert to tibble
          dummy_df <- dplyr::as_tibble(purrr::flatten(dummy_vars)) 

          # Remove original categorical variables and add dummy variables
          vars_data <- dplyr::select(
            vars_data,
            -dplyr::where(function(x) is.factor(x) || is.character(x))
          )
          vars_data <- dplyr::bind_cols(vars_data, dummy_df)
        }
      }
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
          # For interactions, we need to handle binary variables specially
          # Binary variables need to be expanded to dummy variables for interactions
          interaction_vars <- list()
          
          # Process each variable for interactions
          for (var_name in names(original_numeric)) {
            var_data <- original_numeric[[var_name]]
            
            # Check if this was originally a binary categorical variable
            # (we can identify these by checking if they're in the original categorical data)
            if (make_dummy_vars && exists("categorical_data") && var_name %in% names(categorical_data)) {
              original_var_data <- categorical_data[[var_name]]
              
              # Get levels from original categorical data
              levels_to_use <- if (is.factor(original_var_data)) {
                levels(original_var_data)
              } else {
                sort(unique(original_var_data))
              }
              
              # If binary (2 levels), expand to dummy variables for interactions
              if (length(levels_to_use) == 2) {
                for (level in levels_to_use) {
                  dummy_name <- paste0(var_name, level)
                  dummy_values <- as.numeric(original_var_data == level)
                  interaction_vars[[dummy_name]] <- dummy_values
                }
              } else {
                # Multi-level variables were already expanded, use as-is
                interaction_vars[[var_name]] <- var_data
              }
            } else {
              # Continuous variable, use as-is
              interaction_vars[[var_name]] <- var_data
            }
          }
          
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
            interaction_value <- interaction_vars[[var1]] * interaction_vars[[var2]]
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
    wts_names <- names(dplyr::select(.data, {{ .wts }}))
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
  non_correlation_metrics <- setdiff(.metrics, "correlation")
  only_correlation <- length(non_correlation_metrics) == 0

  if (length(non_correlation_metrics) > 0 && length(group_levels) != 2) {
    stop(
      "Group variable must have exactly two levels for non-correlation metrics, got ",
      length(group_levels)
    )
  }

  if (using_correlation && !is.numeric(transformed_data[[group_var]])) {
    stop(
      "Group variable must be numeric when using correlation metric"
    )
  }

  # Validate metrics
  available_metrics <- c("smd", "variance_ratio", "ks", "correlation")
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
    smd = compute_smd,
    variance_ratio = compute_variance_ratio,
    ks = compute_ks,
    correlation = compute_correlation
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

  # Create all combinations for computation
  combinations <- tidyr::expand_grid(
    variable = var_names,
    method = methods,
    metric = .metrics
  )

  # Use purrr to compute all balance statistics
  results <- purrr::pmap_dfr(combinations, function(variable, method, metric) {
    # Extract variable data
    var_data <- transformed_data[[variable]]
    group_data <- transformed_data[[group_var]]

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
        if (metric == "correlation") {
          # For correlation, use the group variable as the second variable
          estimate <- compute_fn(
            x = var_data,
            y = group_data,
            weights = weights_data,
            na.rm = na.rm
          )
        } else {
          # Handle reference group parameter based on the function
          ref_group_param <- if (metric == "smd") {
            # compute_smd accepts both indices and group values
            reference_group
          } else {
            # compute_variance_ratio and compute_ks expect actual group values
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

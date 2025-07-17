#' Check Balance Across Multiple Metrics
#'
#' Computes balance statistics for multiple variables across different groups and
#' optional weighting schemes. This function generalizes balance checking by
#' supporting multiple metrics (SMD, variance ratio, Kolmogorov-Smirnov) and
#' returns results in a tidy, long-format tibble similar to `tidysmd::tidy_smd()`.
#'
#' @param .data A data frame containing the variables to analyze.
#' @param .vars Variables for which to calculate balance metrics. Can be unquoted
#'   variable names, a character vector, or a tidyselect expression.
#' @param .group Grouping variable. Can be unquoted or quoted. Must have exactly
#'   two unique levels.
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
#' @param na_rm A logical value indicating whether to remove missing values
#'   before computation. Defaults to FALSE.
#' @param make_dummy_vars Logical. Transform categorical variables to dummy
#'   variables using `model.matrix()`? Defaults to FALSE. When TRUE, categorical
#'   variables are expanded into separate binary indicators for each level.
#' @param squares Logical. Include squared terms for continuous variables?
#'   Defaults to TRUE. When TRUE, adds squared versions of numeric variables.
#' @param cubes Logical. Include cubed terms for continuous variables?
#'   Defaults to TRUE. When TRUE, adds cubed versions of numeric variables.
#' @param interactions Logical. Include all pairwise interactions between
#'   variables? Defaults to TRUE. When TRUE, creates interaction terms for
#'   all variable pairs.
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
#' check_balance(nhefs_weights, c(age, wt71, smokeintensity), qsmk)
#'
#' # With specific metrics only
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .metrics = c("smd", "ks"))
#'
#' # With multiple weights
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = c(w_ate, w_att))
#'
#' # Exclude observed results
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .wts = w_ate, include_observed = FALSE)
#'
#' # Use correlation for continuous exposure
#' check_balance(mtcars, c(mpg, hp), disp, .metrics = "correlation")
#'
#' # With dummy variables for categorical variables
#' check_balance(nhefs_weights, c(age, sex, race), qsmk, make_dummy_vars = TRUE)
#'
#' # With higher-order terms
#' check_balance(nhefs_weights, c(age, wt71), qsmk, squares = TRUE, cubes = TRUE)
#'
#' # With interactions
#' check_balance(nhefs_weights, c(age, wt71), qsmk, interactions = TRUE)
#'
#' @export
check_balance <- function(
  .data,
  .vars,
  .group,
  .wts = NULL,
  .metrics = c("smd", "variance_ratio", "ks"),
  include_observed = TRUE,
  reference_group = 1L,
  na_rm = FALSE,
  make_dummy_vars = FALSE,
  squares = TRUE,
  cubes = TRUE,
  interactions = TRUE
) {
  # Input validation
  if (!is.data.frame(.data)) {
    stop("Argument '.data' must be a data frame")
  }

  # Convert inputs to character vectors for consistent handling
  group_var <- rlang::as_name(rlang::enquo(.group))
  var_names <- names(dplyr::select(.data, {{ .vars }}))

  if (length(var_names) == 0) {
    stop("No variables selected for '.vars'")
  }

  # Transform data based on arguments
  transformed_data <- .data

  # Store original variable names for later reference
  original_vars <- var_names

  # Apply data transformations
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

        # Use model.matrix to create dummy variables, remove intercept
        if (ncol(categorical_data) > 0) {
          dummy_matrix <- stats::model.matrix(~., data = categorical_data)
          dummy_df <- dplyr::as_tibble(dummy_matrix[, -1, drop = FALSE]) # Remove intercept

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
          # Create all pairwise interactions
          var_combinations <- utils::combn(
            names(original_numeric),
            2,
            simplify = FALSE
          )

          for (combo in var_combinations) {
            var1 <- combo[1]
            var2 <- combo[2]
            interaction_name <- paste(var1, var2, sep = "_x_")
            vars_data[[interaction_name]] <- original_numeric[[var1]] *
              original_numeric[[var2]]
          }
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
    wts_names <- names(dplyr::select(.data, !!.wts))
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
            na_rm = na_rm
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
            na_rm = na_rm
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

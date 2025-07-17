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
#' check_balance(mtcars, c(mpg, hp, wt), vs)
#'
#' # With specific metrics only
#' check_balance(mtcars, c(mpg, hp), vs, .metrics = c("smd", "ks"))
#'
#' # With multiple weights
#' check_balance(mtcars, c(mpg, hp), vs, .wts = c(gear, carb))
#'
#' # Exclude observed results
#' check_balance(mtcars, c(mpg, hp), vs, .wts = gear, include_observed = FALSE)
#'
#' # Use correlation for continuous exposure
#' check_balance(mtcars, c(mpg, hp), disp, .metrics = "correlation")
#'
#' @importFrom dplyr select arrange
#' @importFrom tidyr expand_grid
#' @importFrom purrr pmap_dfr
#' @importFrom tibble tibble
#' @importFrom rlang as_name enquo
#' @export
check_balance <- function(
  .data,
  .vars,
  .group,
  .wts = NULL,
  .metrics = c("smd", "variance_ratio", "ks"),
  include_observed = TRUE,
  reference_group = 1L,
  na_rm = FALSE
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

  # Handle weights using proper NSE - capture quosure and check if null
  .wts <- rlang::enquo(.wts)
  if (!rlang::quo_is_null(.wts)) {
    wts_names <- names(dplyr::select(.data, !!.wts))
  } else {
    wts_names <- NULL
  }

  # Validate group variable
  if (!group_var %in% names(.data)) {
    stop("Group variable '", group_var, "' not found in data")
  }

  # Get group levels in proper order
  # For correlation, we allow continuous group variables
  using_correlation <- "correlation" %in% .metrics

  if (is.factor(.data[[group_var]])) {
    group_levels <- levels(.data[[group_var]])
  } else {
    group_levels <- sort(unique(stats::na.omit(.data[[group_var]])))
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

  if (using_correlation && !is.numeric(.data[[group_var]])) {
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
    var_data <- .data[[variable]]
    group_data <- .data[[group_var]]

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

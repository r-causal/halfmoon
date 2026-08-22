#' Check Balance Across Multiple Metrics
#'
#' Computes balance statistics for multiple variables across different groups and
#' optional weighting schemes. This function generalizes balance checking by
#' supporting multiple metrics (SMD, variance ratio, Kolmogorov-Smirnov, weighted correlation) and
#' returns results in a tidy format.
#'
#' @details
#' This function serves as a comprehensive balance assessment tool by computing multiple
#' balance metrics simultaneously. It automatically handles different variable types and
#' can optionally transform variables (dummy coding, polynomial terms, interactions)
#' before computing balance statistics.
#'
#' The function supports several balance metrics:
#' \itemize{
#'   \item **SMD (Standardized Mean Difference)**: Measures effect size between groups,
#'     with values around 0.1 or smaller generally indicating good balance
#'   \item **Variance Ratio**: Compares group variances, with values near 1.0 indicating
#'     similar variability between groups
#'   \item **Kolmogorov-Smirnov**: Tests distributional differences between groups,
#'     with smaller values indicating better balance
#'   \item **Correlation**: For continuous exposures, measures linear association
#'     between covariate and exposure
#'   \item **Energy Distance**: Multivariate test comparing entire distributions
#' }
#'
#' When multiple weighting schemes are provided, the function computes balance
#' for each method, enabling comparison of different approaches (e.g., ATE vs ATT weights).
#' The `include_observed` parameter controls whether unweighted ("observed") balance
#' is included in the results.
#'
#' The metrics that apply depend on the type of the exposure. Binary and
#' categorical exposures compare groups, so they take the standardized mean
#' difference, the variance ratio, the Kolmogorov-Smirnov statistic, and the
#' energy distance. A continuous exposure has no groups to compare, so it takes
#' the weighted correlation and the energy distance. `exposure_type` decides
#' which set applies, and `.metrics = NULL` fills in that whole set. Asking for
#' a metric that does not apply to the exposure type is an error.
#'
#' By default the exposure type is read from the exposure itself: two observed
#' values are binary, a factor or character vector with more than two values is
#' categorical, and a numeric vector is categorical when its distinct values
#' cover less than a fifth of its observations and continuous otherwise. A
#' numeric exposure with many repeated values, such as a change score on a
#' bounded count, therefore reads as categorical; pass
#' `exposure_type = "continuous"` when that is not what you mean. The detected
#' type is reported once per call, which `options(halfmoon.quiet = TRUE)`
#' silences.
#'
#' @inheritParams check_params
#' @param .metrics Character vector specifying which metrics to compute.
#'   Available options: "smd" (standardized mean difference), "vr" (variance ratio),
#'   "ks" (Kolmogorov-Smirnov), "correlation" (for continuous exposures),
#'   "energy" (multivariate energy distance). Defaults to `NULL`, which uses
#'   every metric that applies to the exposure type: c("smd", "vr", "ks",
#'   "energy") for a binary or categorical exposure and c("correlation",
#'   "energy") for a continuous one.
#' @param exposure_type The type of exposure `.exposure` holds: one of "binary",
#'   "categorical", or "continuous". Defaults to "auto", which detects the type
#'   from the data and reports what it found.
#' @param .reference_level The level of `.exposure` the other levels are
#'   compared against. If `NULL` (default), the first observed level. A value
#'   that matches a level is taken as that level; a numeric value that matches
#'   no level is taken as a position among the observed levels. Ignored for a
#'   continuous exposure.
#' @inheritParams balance_params
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
#' @family balance functions
#' @seealso [bal_smd()], [bal_vr()], [bal_ks()], [bal_corr()], [bal_energy()] for individual metric functions,
#'   [plot_balance()] for visualization
#'
#' @examples
#' # Basic usage with binary exposure
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .weights = c(w_ate, w_att))
#'
#' # With specific metrics only
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .metrics = c("smd", "energy"))
#'
#' # Categorical exposure
#' check_balance(nhefs_weights, c(age, wt71), alcoholfreq_cat,
#'               .weights = c(w_cat_ate, w_cat_att_2_3wk))
#'
#' # Specify reference group for categorical exposure
#' check_balance(nhefs_weights, c(age, wt71, sex), alcoholfreq_cat,
#'               .reference_level = "daily", .metrics = c("smd", "vr"))
#'
#' # Exclude observed results
#' check_balance(nhefs_weights, c(age, wt71), qsmk, .weights = w_ate,
#'               include_observed = FALSE)
#'
#' # Use correlation for continuous exposure
#' check_balance(mtcars, c(mpg, hp), disp, .metrics = c("correlation", "energy"))
#'
#' # The metrics that apply to the exposure type are the default
#' check_balance(mtcars, c(mpg, hp), disp, exposure_type = "continuous")
#'
#' # A numeric exposure with many repeated values reads as categorical, so say
#' # so when you mean it to be continuous
#' check_balance(nhefs_weights, c(age, wt71), smokeintensity,
#'               exposure_type = "continuous")
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
  .exposure,
  .weights = NULL,
  .metrics = NULL,
  exposure_type = c("auto", "binary", "categorical", "continuous"),
  include_observed = TRUE,
  .reference_level = NULL,
  na.rm = FALSE,
  make_dummy_vars = TRUE,
  squares = FALSE,
  cubes = FALSE,
  interactions = FALSE
) {
  validate_data_frame(.data)

  # Grouping would add the grouping variables to every selection, so the
  # groups are dropped and the data is read as a plain data frame
  .data <- dplyr::ungroup(.data)

  # Convert inputs to character vectors for consistent handling
  exposure_var <- rlang::as_name(rlang::enquo(.exposure))

  # A selection can rename what it selects, so the columns to read and the
  # names to report are tracked separately
  vars_selection <- tidyselect::eval_select(rlang::enquo(.vars), .data)
  var_cols <- names(.data)[vars_selection]
  var_names <- names(vars_selection)

  if (length(var_names) == 0) {
    abort(
      "No variables selected for {.arg .vars}",
      error_class = "halfmoon_empty_error",
      call = rlang::current_env()
    )
  }

  # The selected covariates under the names the results will report. A logical
  # covariate is a 0/1 indicator, so it is read as one rather than refused by
  # the balance functions.
  selected_data <- .data[vars_selection]
  names(selected_data) <- var_names
  selected_data <- purrr::modify_if(selected_data, is.logical, as.numeric)

  vars_data <- selected_data

  if (make_dummy_vars || squares || cubes || interactions) {
    # Track variable origins for interaction filtering
    dummy_var_mapping <- list()

    # Create dummy variables if requested
    if (make_dummy_vars) {
      dummy_result <- create_dummy_variables(
        vars_data,
        binary_as_single = TRUE,
        return_mapping = TRUE
      )
      vars_data <- dummy_result$data
      dummy_var_mapping <- dummy_result$mapping
    }

    # Add squared terms if requested
    if (squares) {
      numeric_vars <- purrr::map_lgl(vars_data, is.numeric)
      if (any(numeric_vars)) {
        numeric_data <- dplyr::select(vars_data, dplyr::where(is.numeric))
        # Only square non-binary numeric variables
        non_binary_numeric <- dplyr::select(
          numeric_data,
          dplyr::where(\(x) !is_binary(x))
        )
        if (ncol(non_binary_numeric) > 0) {
          squared_data <- dplyr::mutate(
            non_binary_numeric,
            dplyr::across(everything(), \(x) x^2, .names = "{.col}_squared")
          )
          vars_data <- dplyr::bind_cols(
            vars_data,
            dplyr::select(squared_data, dplyr::ends_with("_squared"))
          )
        }
      }
    }

    # Add cubed terms if requested
    if (cubes) {
      numeric_vars <- purrr::map_lgl(vars_data, is.numeric)
      if (any(numeric_vars)) {
        numeric_data <- dplyr::select(vars_data, dplyr::where(is.numeric))
        # Only cube original non-binary variables, not squared ones
        original_numeric <- dplyr::select(
          numeric_data,
          -dplyr::ends_with("_squared")
        )
        # Filter out binary variables
        non_binary_original <- dplyr::select(
          original_numeric,
          dplyr::where(\(x) !is_binary(x))
        )
        if (ncol(non_binary_original) > 0) {
          cubed_data <- dplyr::mutate(
            non_binary_original,
            dplyr::across(everything(), \(x) x^3, .names = "{.col}_cubed")
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
          original_vars_data <- selected_data

          # Identify which numeric variables were originally binary categoricals
          binary_categorical_names <- character()
          if (make_dummy_vars) {
            categorical_check <- purrr::map_lgl(
              original_vars_data,
              \(x) is.factor(x) || is.character(x)
            )

            if (any(categorical_check)) {
              categorical_cols <- original_vars_data[categorical_check]
              binary_check <- purrr::map_lgl(categorical_cols, \(x) {
                n_levels <- if (is.factor(x)) {
                  nlevels(x)
                } else {
                  length(unique(x))
                }
                n_levels == 2
              })
              binary_categorical_names <- names(categorical_cols)[binary_check]
            }
          }

          # Prepare variables for interactions
          interaction_vars_list <- purrr::imap(
            original_numeric,
            prepare_interaction_variable,
            binary_categorical_names = binary_categorical_names,
            original_vars_data = original_vars_data
          )

          # Extract the variables and update mapping for expanded binaries
          interaction_vars <- purrr::flatten(interaction_vars_list)

          # Update mapping for any expanded binary categoricals
          for (i in seq_along(interaction_vars_list)) {
            var_result <- interaction_vars_list[[i]]
            var_name <- names(original_numeric)[i]

            # Check if this variable was expanded (binary categorical)
            if (var_name %in% binary_categorical_names) {
              # The result is already flattened by prepare_interaction_variable
              # Get the names of the expanded dummies
              expanded_names <- names(var_result)
              for (expanded_name in expanded_names) {
                # Track that this expanded dummy came from the original variable
                dummy_var_mapping[[expanded_name]] <- var_name
              }
            }
          }

          # Now create interactions between all pairs
          var_combinations <- utils::combn(
            names(interaction_vars),
            2,
            simplify = FALSE
          )

          # Filter out same-variable dummy interactions (e.g., sex0 x sex1)
          valid_combinations <- purrr::keep(
            var_combinations,
            \(combo) is_valid_interaction_combo(combo, dummy_var_mapping)
          )

          # Create interaction terms using functional programming
          interaction_terms <- purrr::map(
            valid_combinations,
            create_interaction_term,
            interaction_vars = interaction_vars
          )

          # Flatten the list and convert to data frame
          interaction_terms <- purrr::flatten(interaction_terms)
          if (length(interaction_terms) > 0) {
            interaction_df <- dplyr::as_tibble(interaction_terms)
            vars_data <- dplyr::bind_cols(vars_data, interaction_df)
          }
        }
      }
    }
  }

  # Replace the selected columns with the working copies, which carry the names
  # the results report and any transformations
  transformed_data <- .data[, !names(.data) %in% var_cols, drop = FALSE]
  transformed_data <- dplyr::bind_cols(transformed_data, vars_data)

  # Update var_names to include all transformed variables
  var_names <- names(vars_data)

  # Handle weights using proper NSE - capture quosure and check if null. A
  # renamed selection names the method, so the column it reads is kept with it.
  .weights <- rlang::enquo(.weights)
  if (!rlang::quo_is_null(.weights)) {
    wts_selection <- tidyselect::eval_select(.weights, .data)
    wts_names <- names(wts_selection)
    weight_columns <- stats::setNames(names(.data)[wts_selection], wts_names)
  } else {
    wts_names <- NULL
    weight_columns <- character(0)
  }

  # Validate exposure variable
  validate_column_exists(transformed_data, exposure_var, "data")

  exposure_type <- causalgenerics::match_exposure_type(
    exposure_type,
    transformed_data[[exposure_var]],
    arg = ".exposure",
    announce = !be_quiet(),
    call = rlang::current_env()
  )

  .metrics <- resolve_metrics(.metrics, exposure_type)

  # A continuous exposure has no groups to compare, so neither the levels nor
  # the reference level are computed or needed
  if (exposure_type == "continuous") {
    group_levels <- NULL
    reference_level <- NULL
  } else {
    group_levels <- extract_group_levels(
      transformed_data[[exposure_var]],
      require_binary = FALSE,
      call = rlang::current_env()
    )

    # Check for single-level groups
    only_correlation <- length(setdiff(.metrics, "correlation")) == 0
    if (length(group_levels) < 2 && !only_correlation) {
      abort(
        "Exposure variable must have at least two levels for metrics: {.val {setdiff(.metrics, 'correlation')}}. Got {length(group_levels)} level{?s}.",
        error_class = "halfmoon_group_error"
      )
    }

    # Resolve the reference once, so that every metric and every label in the
    # results describes the same comparison
    reference_level <- if (length(group_levels) >= 2) {
      determine_reference_group(
        transformed_data[[exposure_var]],
        .reference_level,
        call = rlang::current_env()
      )
    } else {
      NULL
    }
  }

  # A continuous reading needs a numeric exposure whatever the metrics;
  # energy on a factor would otherwise measure its integer codes
  if (
    exposure_type == "continuous" &&
      !is.numeric(transformed_data[[exposure_var]])
  ) {
    abort(
      "Exposure variable must be numeric when treated as continuous",
      error_class = "halfmoon_type_error"
    )
  }

  # Create metric function mapping
  metric_functions <- list(
    smd = bal_smd,
    vr = bal_vr,
    ks = bal_ks,
    correlation = bal_corr,
    energy = energy_metric(exposure_type)
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
    abort(
      "No methods to compute. Either set {.arg include_observed = TRUE} or provide {.arg .weights}",
      error_class = "halfmoon_arg_error"
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
  results <- purrr::pmap_dfr(
    combinations,
    compute_single_balance_metric,
    .data = .data,
    metric_functions = metric_functions,
    transformed_data = transformed_data,
    exposure_var = exposure_var,
    na.rm = na.rm,
    reference_level = reference_level,
    group_levels = group_levels,
    var_names = var_names,
    weight_columns = weight_columns
  )

  # Arrange results for better readability
  if (nrow(results) > 0) {
    report_failed_combinations(results)
    results$.failed <- NULL
    results <- dplyr::arrange(results, variable, metric, method)
  }

  # Add halfmoon_balance class
  class(results) <- c("halfmoon_balance", class(results))

  return(results)
}

# The metrics check_balance() can compute, either overall or for one exposure
# type
balance_metrics <- function(exposure_type = NULL) {
  if (is.null(exposure_type)) {
    c("smd", "vr", "ks", "correlation", "energy")
  } else if (exposure_type == "continuous") {
    c("correlation", "energy")
  } else {
    c("smd", "vr", "ks", "energy")
  }
}

# Supply the metrics that suit the exposure type, or check the requested ones
# against it
resolve_metrics <- function(
  .metrics,
  exposure_type,
  call = rlang::caller_env()
) {
  type_metrics <- balance_metrics(exposure_type)

  if (is.null(.metrics)) {
    return(type_metrics)
  }

  available_metrics <- balance_metrics()
  invalid_metrics <- setdiff(.metrics, available_metrics)
  if (length(invalid_metrics) > 0) {
    abort(
      c(
        "Invalid metric{?s}: {.val {invalid_metrics}}",
        i = "Available metrics: {.val {available_metrics}}"
      ),
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  mismatched_metrics <- setdiff(.metrics, type_metrics)
  if (length(mismatched_metrics) > 0) {
    abort(
      c(
        "{cli::qty(mismatched_metrics)}Metric{?s} {.val {mismatched_metrics}} cannot be computed for a {.val {exposure_type}} exposure.",
        i = "Metrics for a {.val {exposure_type}} exposure: {.val {type_metrics}}.",
        i = "Set {.arg exposure_type} to read {.arg .exposure} as another type."
      ),
      error_class = "halfmoon_metric_type_error",
      call = call
    )
  }

  .metrics
}

# `bal_energy()` reads the exposure to choose between its continuous and its
# discrete branch. Within check_balance() the resolved exposure type makes that
# choice instead, so that every metric describes the same exposure.
energy_metric <- function(exposure_type) {
  function(.covariates, .exposure, .weights, na.rm) {
    bal_energy_impl(
      .covariates = .covariates,
      .exposure = .exposure,
      .weights = .weights,
      na.rm = na.rm,
      exposure_type = exposure_type
    )
  }
}

# Compute a single balance metric for a variable/method/metric combination
compute_single_balance_metric <- function(
  variable,
  method,
  metric,
  .data,
  metric_functions,
  transformed_data,
  exposure_var,
  na.rm,
  reference_level,
  group_levels,
  var_names,
  weight_columns
) {
  # Handle weights
  if (method == "observed") {
    weights_data <- NULL
  } else {
    weights_data <- .data[[weight_columns[[method]]]]
  }

  # Get the appropriate function and compute
  compute_fn <- metric_functions[[metric]]

  # Compute the statistic
  tryCatch(
    {
      if (metric == "energy") {
        # For energy, use all variables
        group_data <- transformed_data[[exposure_var]]
        covariates_data <- transformed_data[var_names]

        estimate <- compute_fn(
          .covariates = covariates_data,
          .exposure = group_data,
          .weights = weights_data,
          na.rm = na.rm
        )
      } else if (metric == "correlation") {
        # For correlation, use the exposure variable as the second variable
        var_data <- transformed_data[[variable]]
        group_data <- transformed_data[[exposure_var]]

        estimate <- compute_fn(
          .x = var_data,
          .y = group_data,
          .weights = weights_data,
          na.rm = na.rm
        )
      } else {
        # For other metrics (smd, vr, ks). The reference level is already
        # resolved to a level value, so each function is given the same group.
        var_data <- transformed_data[[variable]]
        group_data <- transformed_data[[exposure_var]]

        estimate <- compute_fn(
          .covariate = var_data,
          .exposure = group_data,
          .weights = weights_data,
          .reference_level = reference_level,
          na.rm = na.rm
        )
      }

      # Check if we have categorical results (named vector)
      if (
        is.numeric(estimate) &&
          length(estimate) > 1 &&
          !is.null(names(estimate))
      ) {
        # Categorical exposure - expand results
        tibble::tibble(
          variable = variable,
          group_level = strip_comparison_suffix(
            names(estimate),
            reference_level
          ),
          method = method,
          metric = metric,
          estimate = unname(estimate),
          .failed = FALSE
        )
      } else {
        # Binary exposure - single result
        # Determine the group level for reporting
        if (metric == "correlation") {
          # For correlation, report the exposure variable name since it's continuous
          group_level <- exposure_var
        } else if (metric == "energy") {
          # For energy, use NA since it's multivariate
          group_level <- NA_character_
        } else {
          # For other metrics, report the group compared against the reference
          group_level <- setdiff(group_levels, reference_level)[1]
        }

        tibble::tibble(
          variable = variable,
          group_level = as.character(group_level),
          method = method,
          metric = metric,
          estimate = estimate,
          .failed = FALSE
        )
      }
    },
    error = \(e) {
      # Return NA for failed computations but preserve structure. A categorical
      # exposure has one comparison per non-reference level, so it takes one
      # row each rather than a single row standing for all of them.
      error_group_level <- if (metric == "correlation") {
        exposure_var
      } else if (metric == "energy") {
        NA_character_
      } else {
        setdiff(group_levels, reference_level)
      }

      tibble::tibble(
        variable = variable,
        group_level = as.character(error_group_level),
        method = method,
        metric = metric,
        estimate = NA_real_,
        .failed = TRUE
      )
    }
  )
}

# Categorical results are named "{comparison}_vs_{reference}". A reference level
# can itself contain "_vs_", so the suffix is removed by its own length rather
# than matched with a pattern.
strip_comparison_suffix <- function(comparison_names, reference_level) {
  if (is.null(reference_level)) {
    return(as.character(comparison_names))
  }

  suffix <- paste0("_vs_", reference_level)
  stripped <- endsWith(comparison_names, suffix)
  comparison_names[stripped] <- substr(
    comparison_names[stripped],
    1L,
    nchar(comparison_names[stripped]) - nchar(suffix)
  )

  as.character(comparison_names)
}

# Statistics that no data can support are reported as NA rather than as an
# error, so the combinations that failed are named once for the whole call
report_failed_combinations <- function(results, call = rlang::caller_env()) {
  failed <- results$.failed
  if (!any(failed)) {
    return(invisible(results))
  }

  n_failed <- sum(failed)
  failed_metrics <- unique(results$metric[failed])
  failed_variables <- unique(results$variable[failed])
  failed_variables <- failed_variables[!is.na(failed_variables)]

  bullets <- c(
    "Could not compute {n_failed} balance combination{?s}, reported as {.code NA}.",
    i = "{cli::qty(failed_metrics)}Affected metric{?s}: {.val {failed_metrics}}."
  )
  if (length(failed_variables) > 0) {
    bullets <- c(
      bullets,
      i = "{cli::qty(failed_variables)}Affected variable{?s}: {.val {failed_variables}}."
    )
  }

  warn(
    bullets,
    warning_class = "halfmoon_data_warning",
    call = call,
    .envir = rlang::current_env()
  )

  invisible(results)
}

# Prepare a variable for interaction terms
prepare_interaction_variable <- function(
  var_data,
  var_name,
  binary_categorical_names,
  original_vars_data
) {
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
    purrr::map(levels_to_use, \(level) {
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

# Check if an interaction combination is valid (not between same variable dummies)
is_valid_interaction_combo <- function(combo, variable_mapping = NULL) {
  var1 <- combo[1]
  var2 <- combo[2]

  # If we have a mapping, use it to determine if variables come from same source
  if (!is.null(variable_mapping)) {
    # Get the original variable for each dummy (or the variable itself if not a dummy)
    origin1 <- variable_mapping[[var1]] %||% var1
    origin2 <- variable_mapping[[var2]] %||% var2

    # Only keep interactions between different original variables
    return(origin1 != origin2)
  }

  # Fallback to the old regex approach if no mapping provided
  # Extract base variable names (before dummy suffixes)
  base1 <- sub("^([^0-9]+).*", "\\1", var1)
  base2 <- sub("^([^0-9]+).*", "\\1", var2)

  # Only keep interactions between different base variables
  base1 != base2
}

# Create an interaction term between two variables
create_interaction_term <- function(combo, interaction_vars) {
  var1 <- combo[1]
  var2 <- combo[2]
  interaction_name <- paste(var1, var2, sep = "_x_")

  # Return a named list with the interaction term
  interaction_value <- interaction_vars[[var1]] *
    interaction_vars[[var2]]
  stats::setNames(list(interaction_value), interaction_name)
}

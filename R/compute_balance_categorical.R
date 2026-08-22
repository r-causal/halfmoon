# Internal functions for computing balance statistics for categorical exposures
#' Internal: Calculate SMD for categorical exposures
#'
#' @param covariate The covariate vector
#' @param group The categorical exposure vector (>2 levels)
#' @param weights Optional weights vector
#' @param reference_group Reference group level
#' @param na.rm Remove missing values?
#' @param call The calling environment, used for error reporting
#'
#' @return Named vector of SMD values for each non-reference category
#' @noRd
.bal_smd_categorical <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE,
  call = rlang::caller_env()
) {
  compute_categorical_balance(
    covariate = covariate,
    group = group,
    weights = weights,
    reference_group = reference_group,
    na.rm = na.rm,
    call = call,
    balance_fn = bal_smd
  )
}

#' Internal: Calculate variance ratios for categorical exposures
#'
#' @param covariate The covariate vector
#' @param group The categorical exposure vector (>2 levels)
#' @param weights Optional weights vector
#' @param reference_group Reference group level
#' @param na.rm Remove missing values?
#' @param call The calling environment, used for error reporting
#'
#' @return Named vector of variance ratio values for each non-reference category
#' @noRd
.bal_vr_categorical <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE,
  call = rlang::caller_env()
) {
  compute_categorical_balance(
    covariate = covariate,
    group = group,
    weights = weights,
    reference_group = reference_group,
    na.rm = na.rm,
    call = call,
    balance_fn = bal_vr
  )
}

#' Internal: Calculate KS statistics for categorical exposures
#'
#' @param covariate The covariate vector
#' @param group The categorical exposure vector (>2 levels)
#' @param weights Optional weights vector
#' @param reference_group Reference group level
#' @param na.rm Remove missing values?
#' @param call The calling environment, used for error reporting
#'
#' @return Named vector of KS values for each non-reference category
#' @noRd
.bal_ks_categorical <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE,
  call = rlang::caller_env()
) {
  compute_categorical_balance(
    covariate = covariate,
    group = group,
    weights = weights,
    reference_group = reference_group,
    na.rm = na.rm,
    call = call,
    balance_fn = bal_ks
  )
}

#' Internal: Generic function to compute balance for categorical exposures
#'
#' @param covariate The covariate vector
#' @param group The categorical exposure vector (>2 levels)
#' @param weights Optional weights vector
#' @param reference_group Reference group level
#' @param na.rm Remove missing values?
#' @param balance_fn The balance function to use (bal_smd, bal_vr, bal_ks)
#' @param call The calling environment, used for error reporting
#'
#' @return Named vector of balance values for each non-reference category
#' @noRd
compute_categorical_balance <- function(
  covariate,
  group,
  weights,
  reference_group,
  na.rm,
  balance_fn,
  call = rlang::caller_env()
) {
  # Get group levels
  group_levels <- extract_group_levels(
    group,
    require_binary = FALSE,
    call = call
  )

  if (length(group_levels) <= 2) {
    abort(
      "Internal error: compute_categorical_balance called with non-categorical group",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  # Determine reference group
  ref_group <- determine_reference_group_categorical(
    group_levels,
    reference_group,
    call = call
  )

  # Get non-reference levels
  comparison_levels <- setdiff(group_levels, ref_group)
  result_names <- paste0(comparison_levels, "_vs_", ref_group)

  # Subsetting to a pair of levels drops missing exposures silently, so missing
  # data has to be caught before the pairwise comparisons
  if (!na.rm) {
    has_missing <- anyNA(group) ||
      anyNA(covariate) ||
      (!is.null(weights) && anyNA(extract_weight_data(weights)))

    if (has_missing) {
      return(stats::setNames(
        rep(NA_real_, length(comparison_levels)),
        result_names
      ))
    }
  }

  # Calculate balance statistic for each comparison level vs reference
  results <- purrr::map_dbl(
    comparison_levels,
    compute_pairwise_balance,
    covariate = covariate,
    group = group,
    weights = weights,
    ref_group = ref_group,
    na.rm = na.rm,
    balance_fn = balance_fn
  )

  # Name the results
  names(results) <- result_names
  results
}

# Helper functions --------------------------------------------------------

compute_pairwise_balance <- function(
  comp_level,
  covariate,
  group,
  weights,
  ref_group,
  na.rm,
  balance_fn
) {
  # Create binary indicator for this comparison
  binary_group <- create_binary_comparison(group, comp_level, ref_group)

  # Keep only observations in these two groups
  keep_idx <- group %in% c(comp_level, ref_group)

  # Calculate balance statistic using provided function
  if (is.null(weights)) {
    balance_fn(
      .covariate = covariate[keep_idx],
      .exposure = binary_group[keep_idx],
      .weights = NULL,
      .reference_level = 0, # ref_group mapped to 0
      na.rm = na.rm
    )
  } else {
    balance_fn(
      .covariate = covariate[keep_idx],
      .exposure = binary_group[keep_idx],
      .weights = weights[keep_idx],
      .reference_level = 0,
      na.rm = na.rm
    )
  }
}

create_binary_comparison <- function(group, comp_level, ref_group) {
  binary_group <- numeric(length(group))
  binary_group[group == comp_level] <- 1
  binary_group[group == ref_group] <- 0
  binary_group
}

# Determine reference group for categorical exposures
determine_reference_group_categorical <- function(
  group_levels,
  reference_group = NULL,
  call = rlang::caller_env()
) {
  if (is.null(reference_group)) {
    # Default to first level
    return(group_levels[1])
  }

  validate_reference_group_scalar(reference_group, call = call)

  # Check if the value exists in the group levels
  if (reference_group %in% group_levels) {
    return(reference_group)
  }

  # If numeric, treat as index
  if (is.numeric(reference_group)) {
    validate_reference_group_index(
      reference_group,
      length(group_levels),
      call = call
    )
    return(group_levels[reference_group])
  }

  # Otherwise, it's an invalid reference group
  abort(
    "{.arg reference_group} {.val {reference_group}} not found in grouping variable",
    error_class = "halfmoon_reference_error",
    call = call
  )
}

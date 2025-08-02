# Internal functions for computing balance statistics for categorical exposures

#' Internal: Calculate SMD for categorical exposures
#'
#' @param covariate The covariate vector
#' @param group The categorical exposure vector (>2 levels)
#' @param weights Optional weights vector
#' @param reference_group Reference group level
#' @param na.rm Remove missing values?
#'
#' @return Named vector of SMD values for each non-reference category
#' @noRd
.bal_smd_categorical <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE
) {
  compute_categorical_balance(
    covariate = covariate,
    group = group,
    weights = weights,
    reference_group = reference_group,
    na.rm = na.rm,
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
#'
#' @return Named vector of variance ratio values for each non-reference category
#' @noRd
.bal_vr_categorical <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE
) {
  compute_categorical_balance(
    covariate = covariate,
    group = group,
    weights = weights,
    reference_group = reference_group,
    na.rm = na.rm,
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
#'
#' @return Named vector of KS values for each non-reference category
#' @noRd
.bal_ks_categorical <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE
) {
  compute_categorical_balance(
    covariate = covariate,
    group = group,
    weights = weights,
    reference_group = reference_group,
    na.rm = na.rm,
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
#'
#' @return Named vector of balance values for each non-reference category
#' @noRd
compute_categorical_balance <- function(
  covariate,
  group,
  weights,
  reference_group,
  na.rm,
  balance_fn
) {
  # Get group levels
  group_levels <- extract_group_levels(group, require_binary = FALSE)

  if (length(group_levels) <= 2) {
    abort(
      "Internal error: compute_categorical_balance called with non-categorical group"
    )
  }

  # Determine reference group
  ref_group <- determine_reference_group_categorical(
    group_levels,
    reference_group
  )

  # Get non-reference levels
  comparison_levels <- setdiff(group_levels, ref_group)

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
  names(results) <- paste0(comparison_levels, "_vs_", ref_group)
  results
}

#' Internal: Expand categorical balance results to tidy format
#'
#' @param cat_results Named vector of balance statistics from categorical functions
#' @param variable_name Name of the variable
#' @param metric_name Name of the metric (smd, vr, ks)
#' @param method_name Name of the method (observed or weight name)
#'
#' @return Data frame with expanded results
#' @noRd
.expand_categorical_results <- function(
  cat_results,
  variable_name,
  metric_name,
  method_name
) {
  # Extract comparison info from names
  comparison_info <- stringr::str_match(
    names(cat_results),
    "^(.+)_vs_(.+)$"
  )

  data.frame(
    variable = variable_name,
    group_level = comparison_info[, 2],
    reference_level = comparison_info[, 3],
    method = method_name,
    metric = metric_name,
    estimate = unname(cat_results),
    stringsAsFactors = FALSE
  )
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
      covariate = covariate[keep_idx],
      group = binary_group[keep_idx],
      weights = NULL,
      reference_group = 0, # ref_group mapped to 0
      na.rm = na.rm
    )
  } else {
    balance_fn(
      covariate = covariate[keep_idx],
      group = binary_group[keep_idx],
      weights = weights[keep_idx],
      reference_group = 0,
      na.rm = na.rm
    )
  }
}

create_binary_comparison <- function(group, comp_level, ref_group) {
  binary_group <- as.numeric(group == comp_level | group == ref_group)
  binary_group[group == comp_level] <- 1
  binary_group[group == ref_group] <- 0
  binary_group
}

# Determine reference group for categorical exposures
determine_reference_group_categorical <- function(
  group_levels,
  reference_group = NULL
) {
  if (is.null(reference_group)) {
    # Default to first level
    return(group_levels[1])
  }

  # Check if the value exists in the group levels
  if (reference_group %in% group_levels) {
    return(reference_group)
  }

  # If numeric, treat as index
  if (is.numeric(reference_group) && length(reference_group) == 1) {
    if (reference_group > length(group_levels) || reference_group < 1) {
      abort("Reference group index {reference_group} out of bounds")
    }
    return(group_levels[reference_group])
  }

  # Otherwise, it's an invalid reference group
  abort(
    "{.arg reference_group} {.val {reference_group}} not found in grouping variable"
  )
}

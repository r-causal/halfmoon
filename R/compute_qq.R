#' Compute QQ plot data for weighted and unweighted samples
#'
#' Calculate quantile-quantile data comparing the distribution of a variable
#' between treatment groups. This function computes the quantiles for both
#' groups and returns a tidy data frame suitable for plotting or further analysis.
#'
#' @param .data A data frame containing the variables.
#' @param .var Variable to compute quantiles for. Can be unquoted (e.g., `age`)
#'   or quoted (e.g., `"age"`).
#' @param .group Column name of treatment/group variable. Can be unquoted
#'   (e.g., `qsmk`) or quoted (e.g., `"qsmk"`).
#' @param .wts Optional weighting variable(s). Can be unquoted variable names,
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes. Default is NULL (unweighted).
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param include_observed Logical. If using `.wts`, also compute observed
#'   (unweighted) quantiles? Defaults to TRUE.
#' @param treatment_level The reference treatment level to use for comparisons.
#'   If `NULL` (default), uses the last level for factors or the maximum value for numeric variables.
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#'
#' @return A tibble with columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{quantile}{Numeric. The quantile probability (0-1).}
#'   \item{x_quantiles}{Numeric. The quantile value for the reference group.}
#'   \item{y_quantiles}{Numeric. The quantile value for the comparison group.}
#'
#' @examples
#' # Basic QQ data (observed only)
#' qq(nhefs_weights, age, qsmk)
#'
#' # With weighting
#' qq(nhefs_weights, age, qsmk, .wts = w_ate)
#'
#' # Compare multiple weighting schemes
#' qq(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))
#'
#' # Custom quantiles
#' qq(nhefs_weights, age, qsmk,
#'    .wts = w_ate,
#'    quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
#'
#' @export
qq <- function(
  .data,
  .var,
  .group,
  .wts = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  treatment_level = NULL,
  na.rm = FALSE
) {
  # Handle both quoted and unquoted column names
  var_quo <- rlang::enquo(.var)
  group_quo <- rlang::enquo(.group)
  wts_quo <- rlang::enquo(.wts)

  var_name <- get_column_name(var_quo, ".var")
  group_name <- get_column_name(group_quo, ".group")

  # Validate inputs
  if (!var_name %in% names(.data)) {
    abort("Column {.code {var_name}} not found in data")
  }

  if (!group_name %in% names(.data)) {
    abort("Column {.code {group_name}} not found in data")
  }

  # Get weight column names using tidyselect
  wt_names <- if (!rlang::quo_is_null(wts_quo)) {
    names(tidyselect::eval_select(wts_quo, .data))
  } else {
    character(0)
  }

  # Get group levels
  group_var <- .data[[group_name]]
  group_levels <- if (is.factor(group_var)) {
    levels(group_var)
  } else {
    sort(unique(group_var[!is.na(group_var)]))
  }

  if (length(group_levels) != 2) {
    abort("Group variable must have exactly 2 levels")
  }

  # Handle NULL treatment_level
  if (is.null(treatment_level)) {
    if (is.factor(group_var)) {
      # For factors, use the last level
      treatment_level <- group_levels[length(group_levels)]
    } else {
      # For numeric, use the maximum value
      treatment_level <- max(group_levels)
    }
  }

  # Validate treatment_level exists
  if (!treatment_level %in% group_levels) {
    abort("{.arg treatment_level} '{treatment_level}' not found in {.arg .group} levels: {.val {group_levels}}")
  }

  # Determine reference and comparison groups
  ref_group <- treatment_level
  comp_group <- setdiff(group_levels, treatment_level)

  # Create list of methods to compute
  methods <- character(0)
  if (include_observed || length(wt_names) == 0) {
    methods <- c(methods, "observed")
  }
  if (length(wt_names) > 0) {
    methods <- c(methods, wt_names)
  }

  # Compute quantiles for each method
  qq_data <- purrr::map_df(
    methods,
    compute_method_quantiles,
    .data = .data,
    var_name = var_name,
    group_name = group_name,
    ref_group = ref_group,
    comp_group = comp_group,
    quantiles = quantiles,
    na.rm = na.rm
  )

  # Format method labels
  qq_data$method <- factor(qq_data$method, levels = methods)

  qq_data
}

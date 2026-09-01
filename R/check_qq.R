#' Check QQ Data for Multiple Weights
#'
#' Calculate quantile-quantile data comparing the distribution of a variable
#' between treatment groups. This function computes the quantiles for both
#' groups and returns a tidy data frame suitable for plotting or further analysis.
#'
#' @details
#' This function computes the data needed for quantile-quantile plots by
#' calculating corresponding quantiles from two distributions. Unweighted
#' quantiles come from [stats::quantile()]; weighted quantiles come from
#' [weighted_quantile()], which uses the same definition, so a constant weight
#' reproduces the observed quantiles.
#'
#' @param .data A data frame containing the variables.
#' @param .var Variable to compute quantiles for. Supports tidyselect syntax.
#' @param .exposure Column name of treatment/group variable. Supports tidyselect syntax.
#' @param .weights Optional weighting variable(s). Can be unquoted variable names (supports tidyselect syntax),
#'   a character vector, or NULL. Multiple weights can be provided to compare
#'   different weighting schemes. Default is NULL (unweighted).
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param include_observed Logical. If using `.weights`, also compute observed
#'   (unweighted) quantiles? Defaults to TRUE.
#' @param .reference_level The level of `.exposure` to treat as the reference,
#'   the unexposed group whose quantiles are returned in `unexposed_quantiles`.
#'   Either a level of `.exposure` or its position among the observed levels.
#'   If `NULL` (default), the first observed level is used.
#' @param na.rm Logical. If `FALSE` (default), missing values in `.var`,
#'   `.exposure`, or any weight raise an error. If `TRUE`, rows with missing
#'   values are dropped before computation.
#'
#' @return A tibble with class "halfmoon_qq" containing columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{quantile}{Numeric. The quantile probability (0-1).}
#'   \item{exposed_quantiles}{Numeric. The quantile value for the exposed group,
#'     the level of `.exposure` that is not the reference level.}
#'   \item{unexposed_quantiles}{Numeric. The quantile value for the unexposed
#'     group, the reference level of `.exposure`.}
#'
#' @family balance functions
#' @seealso [bal_qq()] for single weight QQ data, [plot_qq()] for visualization
#' @examples
#' # Basic QQ data (observed only)
#' check_qq(nhefs_weights, age, qsmk)
#'
#' # With weighting
#' check_qq(nhefs_weights, age, qsmk, .weights = w_ate)
#'
#' # Compare multiple weighting schemes
#' check_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))
#'
#' @export
check_qq <- function(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  .reference_level = NULL,
  na.rm = FALSE
) {
  # Handle both quoted and unquoted column names
  var_quo <- rlang::enquo(.var)
  exposure_quo <- rlang::enquo(.exposure)
  wts_quo <- rlang::enquo(.weights)

  var_name <- get_column_name(var_quo, ".var")
  exposure_name <- get_column_name(exposure_quo, ".exposure")

  # Validate inputs
  if (!var_name %in% names(.data)) {
    abort(
      "Column {.code {var_name}} not found in data",
      error_class = "halfmoon_column_error"
    )
  }

  if (!exposure_name %in% names(.data)) {
    abort(
      "Column {.code {exposure_name}} not found in data",
      error_class = "halfmoon_column_error"
    )
  }

  # A renaming selection names the method, so the column it reads is tracked
  # alongside the name the result reports
  wt_columns <- if (!rlang::quo_is_null(wts_quo)) {
    wts_selection <- tidyselect::eval_select(wts_quo, .data)
    stats::setNames(names(.data)[wts_selection], names(wts_selection))
  } else {
    character(0)
  }
  wt_names <- names(wt_columns)
  validate_method_labels(wt_names, call = rlang::current_env())

  # Get group levels. Only observed levels count, so a factor that declares
  # levels no observation takes is still binary input.
  exposure_var <- .data[[exposure_name]]
  exposure_levels <- extract_group_levels(exposure_var)

  # Check for missing values if na.rm = FALSE
  if (!na.rm) {
    validate_qq_complete(
      .data,
      var_name = var_name,
      exposure_name = exposure_name,
      wt_names = unname(wt_columns)
    )
  }

  # The reference level is the unexposed group; the other level is exposed
  ref_group <- determine_reference_group(exposure_var, .reference_level)
  comp_group <- setdiff(exposure_levels, ref_group)

  # Create list of methods to compute
  methods <- character(0)
  if (include_observed || length(wt_names) == 0) {
    methods <- c(methods, "observed")
  }
  if (length(wt_names) > 0) {
    methods <- c(methods, wt_names)
  }

  # Compute quantiles for each method. "observed" reads no column, so it maps
  # to `NA` and the unweighted branch takes over.
  method_columns <- unname(wt_columns[methods])

  qq_data <- purrr::map2_df(
    methods,
    method_columns,
    compute_method_quantiles,
    .data = .data,
    var_name = var_name,
    exposure_name = exposure_name,
    ref_group = ref_group,
    comp_group = comp_group,
    quantiles = quantiles,
    na.rm = na.rm
  )

  # Format method labels
  qq_data$method <- factor(qq_data$method, levels = methods)

  # Add halfmoon_qq class
  class(qq_data) <- c("halfmoon_qq", class(qq_data))

  qq_data
}

#' Error on missing values in the columns a QQ computation reads
#'
#' @param .data Data frame
#' @param var_name Variable name to compute quantiles for
#' @param exposure_name Group variable name
#' @param wt_names Character vector of weight column names
#'
#' @return `TRUE`, invisibly
#'
#' @noRd
validate_qq_complete <- function(
  .data,
  var_name,
  exposure_name,
  wt_names = character(0),
  call = rlang::caller_env()
) {
  if (anyNA(.data[[var_name]])) {
    abort(
      "Variable {.code {var_name}} contains missing values and {.arg na.rm = FALSE}",
      error_class = "halfmoon_na_error",
      call = call
    )
  }

  if (anyNA(.data[[exposure_name]])) {
    abort(
      "Exposure variable {.code {exposure_name}} contains missing values and {.arg na.rm = FALSE}",
      error_class = "halfmoon_na_error",
      call = call
    )
  }

  for (wt_name in wt_names) {
    if (anyNA(extract_weight_data(.data[[wt_name]]))) {
      abort(
        "Weight variable {.code {wt_name}} contains missing values and {.arg na.rm = FALSE}",
        error_class = "halfmoon_na_error",
        call = call
      )
    }
  }

  invisible(TRUE)
}

#' Compute quantiles for a single method
#'
#' Internal function to compute quantiles for one method (observed or weighted).
#'
#' @param method The name the result reports this method under
#' @param wt_col The column the weights are read from, or `NA` for the
#'   unweighted method. A renaming selection makes this differ from `method`.
#' @param .data Data frame
#' @param var_name Variable name to compute quantiles for
#' @param exposure_name Group variable name
#' @param ref_group Reference group level, the unexposed group
#' @param comp_group Comparison group level, the exposed group
#' @param quantiles Numeric vector of quantiles
#' @param na.rm Logical indicating whether to remove NAs
#'
#' @return A tibble with quantile data
#'
#' @noRd
compute_method_quantiles <- function(
  method,
  wt_col,
  .data,
  var_name,
  exposure_name,
  ref_group,
  comp_group,
  quantiles,
  na.rm
) {
  # Filter data by group
  ref_data <- .data[.data[[exposure_name]] == ref_group, ]
  comp_data <- .data[.data[[exposure_name]] == comp_group, ]

  if (is.na(wt_col)) {
    if (na.rm) {
      ref_data <- ref_data[!is.na(ref_data[[var_name]]), ]
      comp_data <- comp_data[!is.na(comp_data[[var_name]]), ]
    }

    # Standard quantiles
    ref_q <- stats::quantile(
      ref_data[[var_name]],
      probs = quantiles,
      na.rm = FALSE
    )
    comp_q <- stats::quantile(
      comp_data[[var_name]],
      probs = quantiles,
      na.rm = FALSE
    )
  } else {
    # Weighted quantiles
    if (!wt_col %in% names(ref_data) || !wt_col %in% names(comp_data)) {
      abort(
        "Weight column {.code {wt_col}} not found in data",
        error_class = "halfmoon_column_error"
      )
    }

    ref_wts <- extract_weight_data(ref_data[[wt_col]])
    comp_wts <- extract_weight_data(comp_data[[wt_col]])

    if (na.rm) {
      # A row is only usable when both the variable and its weight are observed
      ref_keep <- !is.na(ref_data[[var_name]]) & !is.na(ref_wts)
      comp_keep <- !is.na(comp_data[[var_name]]) & !is.na(comp_wts)
      ref_data <- ref_data[ref_keep, ]
      comp_data <- comp_data[comp_keep, ]
      ref_wts <- ref_wts[ref_keep]
      comp_wts <- comp_wts[comp_keep]
    }

    # Rows with a missing value or weight have already been refused or
    # filtered above, so nothing is left for `na.rm` to drop here
    ref_q <- weighted_quantile(
      ref_data[[var_name]],
      quantiles,
      .weights = ref_wts,
      na.rm = TRUE
    )
    comp_q <- weighted_quantile(
      comp_data[[var_name]],
      quantiles,
      .weights = comp_wts,
      na.rm = TRUE
    )
  }

  dplyr::tibble(
    method = method,
    quantile = quantiles,
    exposed_quantiles = unname(comp_q),
    unexposed_quantiles = unname(ref_q)
  )
}

#' Compute weighted quantiles
#'
#' Calculate quantiles of a numeric vector with associated weights, using the
#' weighted generalization of the default definition in [stats::quantile()].
#'
#' @details
#' [stats::quantile()] with `type = 7`, its default, places the value of rank
#' `i` among `n` values at probability `(i - 1) / (n - 1)` and interpolates
#' linearly between them. `weighted_quantile()` generalizes those positions:
#' each distinct value spans the probabilities implied by the total weight of
#' the observations that take it, shortened at each end by half the average
#' weight of those observations, and the positions are rescaled so that the
#' smallest value sits at 0 and the largest at 1.
#'
#' The definition has the properties you would expect of one:
#'
#' - With a constant positive weight, the result is identical to
#'   `stats::quantile(values, quantiles)`, ties included.
#' - Multiplying every weight by a constant leaves the result unchanged.
#' - The result does not depend on the order of `values`.
#' - The result is monotone in `quantiles`.
#'
#' Observations with zero weight contribute nothing and are excluded. This
#' matters for matching weights, which are 0 or 1: the quantiles of a matched
#' sample are the quantiles of the matched observations alone.
#'
#' @param values Numeric vector of values to compute quantiles for.
#' @param quantiles Numeric vector of probabilities with values between 0 and 1.
#' @param .weights Numeric vector of non-negative weights, same length as `values`.
#' @param na.rm Logical. If `FALSE` (default), a missing value or a missing
#'   weight makes every quantile missing. If `TRUE`, an observation with either
#'   one missing is dropped and the quantiles are computed from the rest.
#'
#' @return Numeric vector of weighted quantiles corresponding to the requested
#'   probabilities. Fewer than two observations with a positive weight leave the
#'   quantiles undefined, and the result is `NA_real_`, as does `na.rm = FALSE`
#'   with a missing value or a missing weight.
#'
#' @examples
#' # Equal weights (same as regular quantiles)
#' weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10))
#' quantile(1:10, c(0.25, 0.5, 0.75))
#'
#' # Weighted towards higher values
#' weighted_quantile(1:10, c(0.25, 0.5, 0.75), 1:10)
#'
#' @export
weighted_quantile <- function(values, quantiles, .weights, na.rm = FALSE) {
  validate_numeric(values, "values")
  validate_numeric(quantiles, "quantiles")
  validate_flag(na.rm, "na.rm")

  if (anyNA(quantiles) || any(quantiles < 0 | quantiles > 1)) {
    abort(
      "{.arg quantiles} must be between 0 and 1",
      error_class = "halfmoon_range_error"
    )
  }

  validate_weights(.weights, length(values))
  .wts <- extract_weight_data(.weights)

  # A missing value or a missing weight cannot be placed in the weighted
  # distribution, so it either makes the quantiles missing or is dropped
  incomplete <- is.na(values) | is.na(.wts)
  if (!na.rm && any(incomplete)) {
    return(rep(NA_real_, length(quantiles)))
  }

  # Zero-weight observations are not part of the weighted distribution
  keep <- !incomplete & .wts > 0
  values <- values[keep]
  .wts <- .wts[keep]

  if (length(values) < 2) {
    return(rep(NA_real_, length(quantiles)))
  }

  ordered <- order(values)
  positions <- weighted_quantile_positions(values[ordered], .wts[ordered])

  stats::approx(
    positions$probs,
    positions$values,
    xout = quantiles,
    rule = 2
  )$y
}

#' Plotting positions for a weighted type 7 quantile
#'
#' @param values Numeric vector of values, sorted, with positive weights.
#' @param weights Numeric vector of positive weights, sorted alongside `values`.
#'
#' @return A list with the distinct `values` and the `probs` they span. Both are
#'   sorted and have the same length, and `probs` runs from 0 to 1.
#'
#' @noRd
weighted_quantile_positions <- function(values, weights) {
  # Index of the last observation taking each distinct value
  last <- c(values[-1] != values[-length(values)], TRUE)

  cumulative_wt <- cumsum(weights)
  upper_wt <- cumulative_wt[last]
  value_wt <- diff(c(0, upper_wt))
  lower_wt <- upper_wt - value_wt
  n_values <- diff(c(0, which(last)))
  mean_wt <- value_wt / n_values

  # Each distinct value spans its own weight, less half of the average weight
  # of its observations at each end, and the whole range is shifted so that the
  # smallest value starts at zero. Under a constant weight, consecutive values
  # then sit one weight apart, which is the (i - 1) / (n - 1) spacing of
  # stats::quantile(type = 7).
  lower <- lower_wt + (mean_wt - mean_wt[1]) / 2
  upper <- upper_wt - (mean_wt + mean_wt[1]) / 2
  span <- upper[length(upper)]

  probs <- c(rbind(lower, upper)) / span
  values <- rep(values[last], each = 2)

  # A value taken by a single observation spans no probability at all, so it
  # only needs one of its two endpoints. The test has to be equality of the
  # endpoints rather than the observation count behind them: the two are
  # algebraically equal for a singleton but reach that value through different
  # expressions, so they can land an ulp apart and both be needed. Comparing
  # the endpoints is also what keeps duplicates away from `stats::approx()`
  # when a wide spread of weights collapses the interleaving.
  distinct <- !duplicated(probs)

  list(probs = probs[distinct], values = values[distinct])
}

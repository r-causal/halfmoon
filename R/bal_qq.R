#' Compute QQ Data for Single Variable and Weight
#'
#' Calculate quantile-quantile data comparing the distribution of a variable
#' between treatment groups for a single weighting scheme (or unweighted).
#' This function computes the quantiles for both groups and returns a data frame
#' suitable for plotting or further analysis.
#'
#' @details
#' This function computes the data needed for quantile-quantile plots by
#' calculating corresponding quantiles from two distributions. Unweighted
#' quantiles come from [stats::quantile()]; weighted quantiles come from
#' [weighted_quantile()], which uses the same definition, so a constant weight
#' reproduces the observed quantiles.
#'
#' When the distributions of a variable are similar between treatment groups
#' (indicating good balance), the QQ plot points will lie close to the diagonal
#' line y = x.
#'
#' @param .data A data frame containing the variables.
#' @param .var Variable to compute quantiles for (unquoted).
#' @param .exposure Column name of treatment/group variable (unquoted).
#' @param .weights Optional single weight variable (unquoted). If NULL, computes
#'   unweighted quantiles.
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param .reference_level The level of `.exposure` to treat as the reference,
#'   the unexposed group whose quantiles are returned in `unexposed_quantiles`.
#'   Either a level of `.exposure` or its position among the observed levels.
#'   If `NULL` (default), the first observed level is used.
#' @param na.rm Logical. If `FALSE` (default), missing values in `.var`,
#'   `.exposure`, or `.weights` raise an error. If `TRUE`, rows with missing
#'   values are dropped before computation.
#'
#' @return A tibble with columns:
#'   \item{quantile}{Numeric. The quantile probability (0-1).}
#'   \item{exposed_quantiles}{Numeric. The quantile value for the exposed group,
#'     the level of `.exposure` that is not the reference level.}
#'   \item{unexposed_quantiles}{Numeric. The quantile value for the unexposed
#'     group, the reference level of `.exposure`.}
#'
#' @family balance functions
#' @seealso [check_qq()] for computing QQ data across multiple weights,
#'   [plot_qq()] for visualization
#'
#' @examples
#' # Unweighted QQ data
#' bal_qq(nhefs_weights, age, qsmk)
#'
#' # Weighted QQ data
#' bal_qq(nhefs_weights, age, qsmk, .weights = w_ate)
#'
#' # Custom quantiles
#' bal_qq(nhefs_weights, age, qsmk, .weights = w_ate,
#'        quantiles = seq(0.1, 0.9, 0.1))
#'
#' @export
bal_qq <- function(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  .reference_level = NULL,
  na.rm = FALSE
) {
  # Handle column names
  var_quo <- rlang::enquo(.var)
  exposure_quo <- rlang::enquo(.exposure)
  wts_quo <- rlang::enquo(.weights)

  var_name <- get_column_name(var_quo, ".var")
  exposure_name <- get_column_name(exposure_quo, ".exposure")

  # Validate inputs. The helpers default to the calling function's frame, so
  # the error reports bal_qq() rather than whatever called it.
  validate_data_frame(.data)
  validate_column_exists(.data, var_name, ".var")
  validate_column_exists(.data, exposure_name, ".exposure")

  # Get weight column if provided. A renaming selection names the method, so
  # the column it reads is tracked separately from that name.
  wt_name <- NULL
  wt_col <- NA_character_
  if (!rlang::quo_is_null(wts_quo)) {
    wts_selection <- tidyselect::eval_select(wts_quo, .data)
    wt_names <- names(wts_selection)
    if (length(wt_names) != 1) {
      abort(
        "{.arg .weights} must select exactly one variable or be NULL",
        error_class = "halfmoon_arg_error",
        call = rlang::current_env()
      )
    }
    validate_method_labels(wt_names, call = rlang::current_env())
    wt_name <- wt_names[[1]]
    wt_col <- names(.data)[wts_selection][[1]]
  }

  # Get exposure levels. Only observed levels count, so a factor that declares
  # levels no observation takes is still binary input.
  exposure_var <- .data[[exposure_name]]
  exposure_levels <- extract_group_levels(exposure_var)

  # Check for missing values if na.rm = FALSE
  if (!na.rm) {
    validate_qq_complete(
      .data,
      var_name = var_name,
      exposure_name = exposure_name,
      wt_names = if (is.na(wt_col)) character(0) else wt_col
    )
  }

  # The reference level is the unexposed group; the other level is exposed
  ref_group <- determine_reference_group(exposure_var, .reference_level)
  comp_group <- setdiff(exposure_levels, ref_group)

  compute_method_quantiles(
    method = wt_name %||% "observed",
    wt_col = wt_col,
    .data = .data,
    var_name = var_name,
    exposure_name = exposure_name,
    ref_group = ref_group,
    comp_group = comp_group,
    quantiles = quantiles,
    na.rm = na.rm
  )[, c("quantile", "exposed_quantiles", "unexposed_quantiles")]
}

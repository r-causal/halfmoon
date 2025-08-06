#' Check Effective Sample Size
#'
#' Computes the effective sample size (ESS) for one or more weighting schemes,
#' optionally stratified by treatment groups. ESS reflects how many observations
#' you would have if all were equally weighted.
#'
#' @details
#' The effective sample size (ESS) is calculated using the classical formula:
#' \eqn{ESS = (\sum w)^2 / \sum(w^2)}.
#'
#' When weights vary substantially, the ESS can be much smaller than the actual
#' number of observations, indicating that a few observations carry
#' disproportionately large weights.
#'
#' When `.group` is provided, ESS is calculated separately for each group level:
#' - For binary/categorical exposures: ESS is computed within each treatment level
#' - For continuous exposures: The variable is divided into quantiles (using
#'   `dplyr::ntile()`) and ESS is computed within each quantile
#'
#' The function returns results in a tidy format suitable for plotting or
#' further analysis.
#'
#' @inheritParams check_params
#' @param .group Optional grouping variable. When provided, ESS is calculated
#'   separately for each group level. For continuous variables, groups are
#'   created using quantiles.
#' @param n_tiles For continuous `.group` variables, the number of quantile
#'   groups to create. Default is 4 (quartiles).
#' @param tile_labels Optional character vector of labels for the quantile groups
#'   when `.group` is continuous. If NULL, uses "Q1", "Q2", etc.
#'
#' @return A tibble with columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{group}{Character. The group level (if `.group` is provided).}
#'   \item{n}{Integer. The number of observations in the group.}
#'   \item{ess}{Numeric. The effective sample size.}
#'   \item{ess_pct}{Numeric. ESS as a percentage of the actual sample size.}
#'
#' @family balance functions
#' @seealso [ess()] for the underlying ESS calculation, [plot_ess()] for visualization
#'
#' @examples
#' # Overall ESS for different weighting schemes
#' check_ess(nhefs_weights, .wts = c(w_ate, w_att, w_atm))
#'
#' # ESS by treatment group (binary exposure)
#' check_ess(nhefs_weights, .wts = c(w_ate, w_att), .group = qsmk)
#'
#' # ESS by treatment group (categorical exposure)
#' check_ess(nhefs_weights, .wts = w_cat_ate, .group = alcoholfreq_cat)
#'
#' # ESS by quartiles of a continuous variable
#' check_ess(nhefs_weights, .wts = w_ate, .group = age, n_tiles = 4)
#'
#' # Custom labels for continuous groups
#' check_ess(nhefs_weights, .wts = w_ate, .group = age,
#'           n_tiles = 3, tile_labels = c("Young", "Middle", "Older"))
#'
#' # Without unweighted comparison
#' check_ess(nhefs_weights, .wts = w_ate, .group = qsmk,
#'           include_observed = FALSE)
#'
#' @export
check_ess <- function(
  .data,
  .wts = NULL,
  .group = NULL,
  include_observed = TRUE,
  n_tiles = 4,
  tile_labels = NULL
) {
  # Validate inputs
  validate_data_frame(.data)

  # Handle group variable
  group_quo <- rlang::enquo(.group)
  has_group <- !rlang::quo_is_null(group_quo)

  if (has_group) {
    group_name <- get_column_name(group_quo, ".group")
    validate_column_exists(.data, group_name, ".group")
    group_var <- .data[[group_name]]

    # Check if continuous (numeric and more than 10 unique values)
    is_continuous <- is.numeric(group_var) &&
      length(unique(stats::na.omit(group_var))) > 10

    if (is_continuous) {
      # Create quantile groups
      if (!is.null(tile_labels) && length(tile_labels) != n_tiles) {
        abort(
          "Length of {.arg tile_labels} must equal {.arg n_tiles}",
          error_class = "halfmoon_length_error"
        )
      }

      # Create tile groups
      .data$.ess_group <- dplyr::ntile(group_var, n_tiles)

      # Apply labels
      if (is.null(tile_labels)) {
        tile_labels <- paste0("Q", seq_len(n_tiles))
      }
      .data$.ess_group <- factor(
        .data$.ess_group,
        levels = seq_len(n_tiles),
        labels = tile_labels
      )
      group_col <- ".ess_group"
    } else {
      group_col <- group_name
    }
  }

  # Handle weights
  wts_quo <- rlang::enquo(.wts)

  if (rlang::quo_is_null(wts_quo)) {
    # No weights provided, just use observed
    wts_names <- character()
  } else {
    wts_cols <- tidyselect::eval_select(wts_quo, .data)
    wts_names <- names(wts_cols)

    # Convert psw weight columns to numeric
    for (wts_name in wts_names) {
      .data[[wts_name]] <- extract_weight_data(.data[[wts_name]])
    }
  }

  # Add observed if requested
  if (include_observed || length(wts_names) == 0) {
    .data$.observed <- 1
    wts_names <- c(".observed", wts_names)
  }

  # Reshape to long format
  plot_data <- tidyr::pivot_longer(
    .data,
    cols = dplyr::all_of(wts_names),
    names_to = "method",
    values_to = "weight"
  )

  # Clean up method names
  plot_data$method <- ifelse(
    plot_data$method == ".observed",
    "observed",
    plot_data$method
  )

  # Calculate ESS
  if (has_group) {
    # Group-wise ESS
    ess_data <- plot_data |>
      dplyr::group_by(method, .data[[group_col]]) |>
      dplyr::summarise(
        n = dplyr::n(),
        ess = ess(weight, na.rm = TRUE),
        ess_pct = ess / n * 100,
        .groups = "drop"
      ) |>
      dplyr::rename(group = !!group_col)
  } else {
    # Overall ESS
    ess_data <- plot_data |>
      dplyr::group_by(method) |>
      dplyr::summarise(
        n = dplyr::n(),
        ess = ess(weight, na.rm = TRUE),
        ess_pct = ess / n * 100,
        .groups = "drop"
      )
  }

  # Clean up temporary columns
  if (has_group && is_continuous && ".ess_group" %in% names(ess_data)) {
    ess_data <- dplyr::select(ess_data, -.ess_group)
  }

  ess_data
}

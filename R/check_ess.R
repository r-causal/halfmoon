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
#' When `.exposure` is provided, ESS is calculated separately for each exposure level:
#' - For binary/categorical exposures: ESS is computed within each treatment level
#' - For continuous exposures: The variable is divided into quantiles (using
#'   `dplyr::ntile()`) and ESS is computed within each quantile
#'
#' A missing weight is a weight of unknown size, so with `na.rm = FALSE`, the
#' default, `ess` and `ess_pct` are `NA` for a weighting method that has any
#' missing weight. With `na.rm = TRUE`, the observations with a missing weight
#' are dropped. Either way, `n` counts the observations whose weight is not
#' missing, which is what `ess` is a share of, so `ess_pct` compares the
#' effective sample size against the sample it was computed from. `n` therefore
#' differs across weighting methods when they are missing for different
#' observations.
#'
#' The function returns results in a tidy format suitable for plotting or
#' further analysis.
#'
#' @inheritParams check_params
#' @param .exposure Optional exposure variable. When provided, ESS is calculated
#'   separately for each exposure level. For continuous variables, groups are
#'   created using quantiles.
#' @param n_tiles For continuous `.exposure` variables, the number of quantile
#'   groups to create. Default is 4 (quartiles).
#' @param tile_labels Optional character vector of labels for the quantile groups
#'   when `.exposure` is continuous. If NULL, uses "Q1", "Q2", etc.
#' @param na.rm Logical. If `FALSE` (default), a missing weight makes the
#'   effective sample size for that weighting method `NA`. If `TRUE`,
#'   observations with a missing weight are dropped before computation.
#'
#' @return A tibble with columns:
#'   \item{method}{Character. The weighting method ("observed" or weight variable name).}
#'   \item{group}{The exposure level, present only when `.exposure` is
#'     provided. It keeps the type of `.exposure`, so a factor exposure gives a
#'     factor and a numeric one gives a numeric. A continuous exposure is cut
#'     into quantile groups, which gives a factor of `tile_labels`.}
#'   \item{n}{Integer. The number of observations in the group whose weight is
#'     not missing.}
#'   \item{ess}{Numeric. The effective sample size.}
#'   \item{ess_pct}{Numeric. ESS as a percentage of `n`.}
#'
#' @family balance functions
#' @seealso [ess()] for the underlying ESS calculation, [plot_ess()] for visualization
#'
#' @examples
#' # Overall ESS for different weighting schemes
#' check_ess(nhefs_weights, .weights = c(w_ate, w_att, w_atm))
#'
#' # ESS by treatment group (binary exposure)
#' check_ess(nhefs_weights, .weights = c(w_ate, w_att), .exposure = qsmk)
#'
#' # ESS by treatment group (categorical exposure)
#' check_ess(nhefs_weights, .weights = w_cat_ate, .exposure = alcoholfreq_cat)
#'
#' # ESS by quartiles of a continuous variable
#' check_ess(nhefs_weights, .weights = w_ate, .exposure = age, n_tiles = 4)
#'
#' # Custom labels for continuous groups
#' check_ess(nhefs_weights, .weights = w_ate, .exposure = age,
#'           n_tiles = 3, tile_labels = c("Young", "Middle", "Older"))
#'
#' # Without unweighted comparison
#' check_ess(nhefs_weights, .weights = w_ate, .exposure = qsmk,
#'           include_observed = FALSE)
#'
#' # Drop observations with a missing weight
#' weights_with_na <- nhefs_weights
#' weights_with_na$w_ate[1:5] <- NA
#' check_ess(weights_with_na, .weights = w_ate, na.rm = TRUE)
#'
#' @export
check_ess <- function(
  .data,
  .weights = NULL,
  .exposure = NULL,
  include_observed = TRUE,
  n_tiles = 4,
  tile_labels = NULL,
  na.rm = FALSE
) {
  # Validate inputs
  validate_data_frame(.data)

  # Handle exposure variable
  group_quo <- rlang::enquo(.exposure)
  has_group <- !rlang::quo_is_null(group_quo)

  if (has_group) {
    group_name <- get_column_name(group_quo, ".exposure")
    validate_column_exists(.data, group_name, ".exposure")
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
      if (is.null(tile_labels)) {
        tile_labels <- paste0("Q", seq_len(n_tiles))
      }
      group_values <- factor(
        dplyr::ntile(group_var, n_tiles),
        levels = seq_len(n_tiles),
        labels = tile_labels
      )
    } else {
      group_values <- group_var
    }
  }

  # Handle weights
  wts_quo <- rlang::enquo(.weights)

  if (rlang::quo_is_null(wts_quo)) {
    # No weights provided, just use observed
    wts_names <- character()
    wts_columns <- character()
  } else {
    wts_cols <- tidyselect::eval_select(wts_quo, .data)
    wts_names <- names(wts_cols)
    # A renaming selection names the method, so the column it reads is tracked
    # alongside the name the result reports
    wts_columns <- names(.data)[wts_cols]
  }

  validate_method_labels(wts_names, call = rlang::current_env())

  # Convert psw weight columns to numeric
  wts_values <- lapply(
    wts_columns,
    function(nm) extract_weight_data(.data[[nm]])
  )

  # Each selected column must hold usable weights before any of them are
  # summarized, so a non-numeric column fails with a halfmoon error rather than
  # somewhere inside the effective sample size calculation
  for (i in seq_along(wts_values)) {
    validate_weights(
      wts_values[[i]],
      n = nrow(.data),
      arg_name = wts_columns[[i]],
      allow_null = FALSE,
      call = rlang::current_env()
    )
  }

  # Add observed if requested
  if (include_observed || length(wts_names) == 0) {
    wts_values <- c(list(rep(1, nrow(.data))), wts_values)
    wts_names <- c("observed", wts_names)
  }

  # Reshape to long format. The reshaped frame holds only internal names, so a
  # column of `.data` named "method" or "weight" cannot collide with the
  # reshaped columns, and a selected column with either name keeps its own
  # meaning in the returned tibble.
  wts_ids <- paste0(".ess_weight_", seq_along(wts_names))
  names(wts_values) <- wts_ids
  ess_input <- tibble::new_tibble(wts_values, nrow = nrow(.data))

  if (has_group) {
    ess_input[[".ess_group"]] <- group_values
  }

  plot_data <- tidyr::pivot_longer(
    ess_input,
    cols = dplyr::all_of(wts_ids),
    names_to = ".ess_method",
    values_to = ".ess_weight"
  )

  # Restore the user-facing method names
  plot_data$.ess_method <- wts_names[match(plot_data$.ess_method, wts_ids)]

  # Calculate ESS
  if (has_group) {
    # Group-wise ESS
    ess_data <- plot_data |>
      dplyr::group_by(.data$.ess_method, .data$.ess_group) |>
      dplyr::summarise(
        n = sum(!is.na(.data$.ess_weight)),
        ess = ess(.data$.ess_weight, na.rm = na.rm),
        ess_pct = ess / n * 100,
        .groups = "drop"
      ) |>
      dplyr::rename(method = ".ess_method", group = ".ess_group")
  } else {
    # Overall ESS
    ess_data <- plot_data |>
      dplyr::group_by(.data$.ess_method) |>
      dplyr::summarise(
        n = sum(!is.na(.data$.ess_weight)),
        ess = ess(.data$.ess_weight, na.rm = na.rm),
        ess_pct = ess / n * 100,
        .groups = "drop"
      ) |>
      dplyr::rename(method = ".ess_method")
  }

  # Add halfmoon_ess class
  class(ess_data) <- c("halfmoon_ess", class(ess_data))

  ess_data
}

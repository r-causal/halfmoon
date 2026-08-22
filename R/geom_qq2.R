#' Create 2-dimensional QQ geometries
#'
#' `geom_qq2()` is a geom for creating quantile-quantile plots with support for
#' weighted comparisons. QQ plots compare the quantiles of two distributions,
#' making them useful for assessing distributional balance in causal inference.
#' As opposed to `geom_qq()`, this geom does not compare a variable against a
#' theoretical distribution, but rather against two group's distributions, e.g.,
#' treatment vs. control.
#'
#' @details
#' Quantile-quantile (QQ) plots visualize how the distributions of a variable
#' differ between treatment groups by plotting corresponding quantiles against
#' each other. If the distributions are identical, points fall on the 45-degree
#' line (y = x). Deviations from this line indicate differences in the distributions.
#'
#' QQ plots are closely related to empirical cumulative distribution function
#' (ECDF) plots (see [`geom_ecdf()`]). While ECDF plots show \eqn{F(x) = P(X \leq x)}
#' for each group, QQ plots show \eqn{F_1^{-1}(p)} vs \eqn{F_2^{-1}(p)}, essentially the inverse
#' relationship. Both approaches visualize the same information about distributional
#' differences, but QQ plots make it easier to spot deviations through a 45-degree
#' reference line.
#'
#' The reference (unexposed) group is on the x axis and the exposed group, the
#' treatment level that is not the reference, is on the y axis.
#'
#' @param mapping Set of aesthetic mappings. Required aesthetics are `sample` (variable)
#'   and `treatment` (group). The `treatment` aesthetic can be a factor, character, or numeric.
#'   Optional aesthetics include `weight` for weighting.
#' @param data Data frame to use. If not specified, inherits from the plot.
#' @param stat Statistical transformation to use. Default is "qq2".
#' @param position Position adjustment. Default is "identity".
#' @inheritParams ggplot2_params
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param .reference_level The level of the `treatment` aesthetic to treat as
#'   the reference, the unexposed group plotted on the x axis. Either a level of
#'   `treatment` or its position among the observed levels. If `NULL` (default),
#'   the first observed level is used.
#'
#' @return A ggplot2 layer.
#' @family ggplot2 functions
#'
#' @seealso
#' - [`geom_ecdf()`] for an alternative visualization of distributional differences
#' - [`plot_qq()`] for a complete plotting function with reference line and labels
#' - [`check_qq()`] for the underlying data computation
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic QQ plot
#' ggplot(nhefs_weights, aes(sample = age, treatment = qsmk)) +
#'   geom_qq2() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed")
#'
#' # With weighting
#' ggplot(nhefs_weights, aes(sample = age, treatment = qsmk, weight = w_ate)) +
#'   geom_qq2() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed")
#'
#' # Compare multiple weights using long format
#' long_data <- tidyr::pivot_longer(
#'   nhefs_weights,
#'   cols = c(w_ate, w_att),
#'   names_to = "weight_type",
#'   values_to = "weight"
#' )
#'
#' ggplot(long_data, aes(color = weight_type)) +
#'   geom_qq2(aes(sample = age, treatment = qsmk, weight = weight)) +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed")
#'
#' @export
geom_qq2 <- function(
  mapping = NULL,
  data = NULL,
  stat = "qq2",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  quantiles = seq(0.01, 0.99, 0.01),
  .reference_level = NULL,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      quantiles = quantiles,
      .reference_level = .reference_level,
      ...
    )
  )
}

#' QQ2 Plot Stat
#'
#' Statistical transformation for QQ plots.
#'
#' @param mapping Set of aesthetic mappings.
#' @param data Data frame.
#' @param geom Geometric object to use. Default is "point".
#' @param position Position adjustment.
#' @param na.rm Remove missing values? Default TRUE.
#' @param show.legend Show legend? Default NA.
#' @param inherit.aes Inherit aesthetics? Default TRUE.
#' @param quantiles Numeric vector of quantiles to compute.
#' @param .reference_level The level of the `treatment` aesthetic to treat as
#'   the reference, the unexposed group plotted on the x axis. Either a level of
#'   `treatment` or its position among the observed levels. If `NULL` (default),
#'   the first observed level is used.
#' @param include_observed For compatibility with qq(). When weights are present,
#'   this determines if an additional "observed" group is added. Default FALSE
#'   for stat_qq2 to avoid duplication when using facets/colors.
#' @param ... Additional arguments.
#'
#' @return A ggplot2 layer.
#' @export
stat_qq2 <- function(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  quantiles = seq(0.01, 0.99, 0.01),
  .reference_level = NULL,
  include_observed = FALSE,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatQq2,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      quantiles = quantiles,
      .reference_level = .reference_level,
      include_observed = include_observed,
      ...
    )
  )
}

#' Resolve the reference level of the treatment aesthetic
#'
#' Levels are the OBSERVED levels, so a declared factor level that no
#' observation takes is never chosen as the default and never accepted as a
#' supplied value.
#'
#' @param treatment The treatment aesthetic for the whole layer
#' @param .reference_level The supplied reference level, or NULL
#'
#' @return The reference level, or `NULL` when no treatment value is observed,
#'   which leaves the empty result to the caller.
#' @noRd
resolve_qq_reference_level <- function(
  treatment,
  .reference_level = NULL,
  call = rlang::caller_env()
) {
  observed_levels <- extract_group_levels(
    treatment,
    require_binary = FALSE,
    call = call
  )

  if (length(observed_levels) == 0) {
    return(NULL)
  }

  if (length(observed_levels) != 2) {
    abort(
      c(
        "{.field treatment} must have exactly two observed levels, not {length(observed_levels)}",
        i = "Observed: {.val {observed_levels}}"
      ),
      error_class = "halfmoon_group_error",
      call = call
    )
  }

  determine_reference_group(treatment, .reference_level, call = call)
}

#' Compute QQ data for one curve
#'
#' @param curve_data Data frame of the rows making up a single curve
#' @param .reference_level The treatment level to use as reference
#' @param quantiles Numeric vector of quantiles to compute
#' @param na.rm Logical whether to remove NA values
#'
#' @return A tibble of quantiles for the curve, or `NULL` when the curve holds
#'   fewer than two treatment levels and there is nothing to compare.
#' @noRd
compute_qq_curve <- function(curve_data, .reference_level, quantiles, na.rm) {
  # One curve compares two groups, so a curve that holds a single treatment
  # level is dropped on its own rather than taken to check_qq(), where it would
  # fail and cost every other curve in the panel
  observed_levels <- extract_group_levels(
    curve_data$treatment,
    require_binary = FALSE
  )

  if (length(observed_levels) < 2) {
    warn(
      c(
        "Dropping a group that does not have two observed treatment levels",
        i = "Observed: {.val {observed_levels}}"
      ),
      warning_class = "halfmoon_data_warning",
      call = quote(geom_qq2())
    )

    return(NULL)
  }

  # The reference level becomes group 1, so check_qq() reads the reference
  # group as unexposed and the other level as exposed
  temp_data <- data.frame(
    .var = curve_data$sample,
    .group = as.integer(curve_data$treatment == .reference_level),
    stringsAsFactors = FALSE
  )

  if (!is.null(curve_data$weight)) {
    temp_data$.wts <- extract_weight_data(curve_data$weight)
    wts_arg <- ".wts"
  } else {
    wts_arg <- NULL
  }

  check_qq(
    .data = temp_data,
    .var = .var,
    .exposure = .group,
    .weights = if (!is.null(wts_arg)) rlang::sym(wts_arg) else NULL,
    quantiles = quantiles,
    .reference_level = 1L, # We already converted to 0/1
    na.rm = na.rm,
    include_observed = FALSE
  )
}

#' Process groups with matching aesthetic signatures
#'
#' Internal function to compute QQ data for groups that share the same
#' aesthetic values (e.g., same color).
#'
#' @param sig The aesthetic signature identifying which groups to process
#' @param groups List of data frames, one per group
#' @param group_signatures Character vector mapping groups to signatures
#' @param unique_signatures Character vector of all unique signatures
#' @param aes_cols Character vector of aesthetic column names
#' @param .reference_level The treatment level to use as reference
#' @param quantiles Numeric vector of quantiles to compute
#' @param na.rm Logical whether to remove NA values
#'
#' @return A data frame with QQ results
#' @noRd
process_aesthetic_group <- function(
  sig,
  groups,
  group_signatures,
  unique_signatures,
  aes_cols,
  .reference_level,
  quantiles,
  na.rm
) {
  # Combine all groups with this signature
  matching_groups <- names(groups)[group_signatures == sig]
  combined_data <- do.call(rbind, groups[matching_groups])

  qq_result <- compute_qq_curve(
    combined_data,
    .reference_level = .reference_level,
    quantiles = quantiles,
    na.rm = na.rm
  )

  if (is.null(qq_result)) {
    return(NULL)
  }

  # Build result data frame preserving aesthetics
  result_df <- data.frame(
    exposed_quantiles = qq_result$exposed_quantiles,
    unexposed_quantiles = qq_result$unexposed_quantiles,
    group = which(unique_signatures == sig),
    PANEL = combined_data$PANEL[1]
  )

  # Preserve aesthetic mappings
  for (col in aes_cols) {
    if (col %in% names(combined_data)) {
      result_df[[col]] <- combined_data[[col]][1]
    }
  }

  result_df
}

#' @rdname stat_qq2
#' @format NULL
#' @usage NULL
#' @export
StatQq2 <- ggplot2::ggproto(
  "StatQq2",
  ggplot2::Stat,
  required_aes = c("sample", "treatment"),
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(unexposed_quantiles),
    y = ggplot2::after_stat(exposed_quantiles),
    weight = NULL
  ),
  dropped_aes = "weight",

  setup_params = function(data, params) {
    # `setup_params()` sees the whole layer before it is split into panels, so
    # the reference level is resolved once here and carried into every panel.
    # Resolving it per panel would let a panel holding a single treatment level
    # take that level as the reference.
    if (!is.null(data$treatment)) {
      params$.reference_level <- resolve_qq_reference_level(
        data$treatment,
        params$.reference_level,
        call = quote(geom_qq2())
      )
    }

    params
  },

  setup_data = function(data, params) {
    # Rows that are missing the variable, the treatment, or the weight cannot
    # enter the quantiles. Dropping them here keeps the weights of the rows
    # that remain, rather than discarding the weighting altogether.
    ggplot2::remove_missing(
      data,
      na.rm = params$na.rm %||% TRUE,
      vars = intersect(c("sample", "treatment", "weight"), names(data)),
      name = "stat_qq2"
    )
  },

  # Override compute_panel instead of compute_group to work with all data at once
  # For QQ plots, we need all the data to compute quantiles properly
  # So we work at the panel level, not the group level
  compute_panel = function(
    data,
    scales,
    quantiles = seq(0.01, 0.99, 0.01),
    .reference_level = NULL,
    na.rm = TRUE,
    include_observed = FALSE
  ) {
    # Nothing observed to compare
    empty_result <- data.frame(
      exposed_quantiles = numeric(0),
      unexposed_quantiles = numeric(0),
      group = integer(0)
    )

    if (is.null(.reference_level) || nrow(data) == 0) {
      return(empty_result)
    }

    # If we have multiple groups, identify which ones should be merged
    # Groups that differ only by treatment level should be processed together
    if ("group" %in% names(data) && length(unique(data$group)) > 1) {
      # Split by group
      groups <- split(data, data$group)

      # Identify aesthetic columns (exclude data and panel columns)
      aes_cols <- setdiff(
        names(data),
        c("sample", "treatment", "weight", "PANEL", "group", "x", "y")
      )

      # Create signatures for each group based on aesthetic values
      # Groups with the same signature should be merged
      group_signatures <- purrr::imap_chr(
        groups,
        function(group_data, group_id) {
          # ggplot2 builds the default group from every discrete aesthetic,
          # the treatment included, so groups that differ only by treatment
          # level are two halves of one curve. A group that already holds both
          # treatment levels comes from an explicit `group` aesthetic and is a
          # curve in its own right.
          if (length(unique(group_data$treatment)) > 1) {
            paste0("group_", group_id)
          } else {
            create_group_signature(group_data, aes_cols)
          }
        }
      )

      # Process each unique signature
      unique_signatures <- unique(group_signatures)
      results <- purrr::map_df(
        unique_signatures,
        process_aesthetic_group,
        groups = groups,
        group_signatures = group_signatures,
        unique_signatures = unique_signatures,
        aes_cols = aes_cols,
        .reference_level = .reference_level,
        quantiles = quantiles,
        na.rm = na.rm
      )

      if (nrow(results) == 0) {
        return(empty_result)
      }

      return(results)
    } else {
      # No groups, process all data together
      qq_result <- compute_qq_curve(
        data,
        .reference_level = .reference_level,
        quantiles = quantiles,
        na.rm = na.rm
      )

      if (is.null(qq_result)) {
        return(empty_result)
      }

      # Return data frame without x and y; let `after_stat()` handle that
      data.frame(
        exposed_quantiles = qq_result$exposed_quantiles,
        unexposed_quantiles = qq_result$unexposed_quantiles,
        PANEL = data$PANEL[1],
        group = 1
      )
    }
  }
)

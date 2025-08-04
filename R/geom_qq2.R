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
#' @param mapping Set of aesthetic mappings. Required aesthetics are `sample` (variable)
#'   and `treatment` (group). The `treatment` aesthetic can be a factor, character, or numeric.
#'   Optional aesthetics include `weight` for weighting.
#' @param data Data frame to use. If not specified, inherits from the plot.
#' @param stat Statistical transformation to use. Default is "qq2".
#' @param position Position adjustment. Default is "identity".
#' @inheritParams ggplot2_params
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @inheritParams treatment_param
#'
#' @return A ggplot2 layer.
#' @family ggplot2 functions
#'
#' @seealso
#' - [`geom_ecdf()`] for an alternative visualization of distributional differences
#' - [`plot_qq()`] for a complete plotting function with reference line and labels
#' - [`qq()`] for the underlying data computation
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
#' # TODO: Remove vec_data() workaround once propensity implements vctrs methods
#' # Extract numeric data from psw objects first
#' nhefs_for_pivot <- nhefs_weights
#' nhefs_for_pivot$w_ate <- vctrs::vec_data(nhefs_weights$w_ate)
#' nhefs_for_pivot$w_att <- vctrs::vec_data(nhefs_weights$w_att)
#' long_data <- tidyr::pivot_longer(
#'   nhefs_for_pivot,
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
  treatment_level = NULL,
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
      treatment_level = treatment_level,
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
#' @param treatment_level The reference treatment level to use for comparisons.
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
  treatment_level = NULL,
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
      treatment_level = treatment_level,
      include_observed = include_observed,
      ...
    )
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
#' @param treatment_level The treatment level to use as reference
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
  treatment_level,
  quantiles,
  na.rm
) {
  # Combine all groups with this signature
  matching_groups <- names(groups)[group_signatures == sig]
  combined_data <- do.call(rbind, groups[matching_groups])

  # Create temporary data frame with binary treatment
  temp_data <- data.frame(
    .var = combined_data$sample,
    .group = as.integer(combined_data$treatment == treatment_level),
    stringsAsFactors = FALSE
  )

  # Add weight if present
  if (!is.null(combined_data$weight) && all(!is.na(combined_data$weight))) {
    temp_data$.wts <- extract_weight_data(combined_data$weight)
    wts_arg <- ".wts"
  } else {
    wts_arg <- NULL
  }

  qq_result <- qq(
    .data = temp_data,
    .var = .var,
    .group = .group,
    .wts = if (!is.null(wts_arg)) rlang::sym(wts_arg) else NULL,
    quantiles = quantiles,
    treatment_level = 1L, # We already converted to 0/1
    na.rm = na.rm,
    include_observed = FALSE
  )

  # Build result data frame preserving aesthetics
  result_df <- data.frame(
    treated_quantiles = qq_result$treated_quantiles,
    untreated_quantiles = qq_result$untreated_quantiles,
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
    x = ggplot2::after_stat(treated_quantiles),
    y = ggplot2::after_stat(untreated_quantiles),
    weight = NULL
  ),
  dropped_aes = "weight",

  # Override compute_panel instead of compute_group to work with all data at once
  # For QQ plots, we need all the data to compute quantiles properly
  # So we work at the panel level, not the group level
  compute_panel = function(
    data,
    scales,
    quantiles = seq(0.01, 0.99, 0.01),
    treatment_level = NULL,
    na.rm = TRUE,
    include_observed = FALSE
  ) {
    # Handle NULL treatment_level
    if (is.null(treatment_level)) {
      if (is.factor(data$treatment)) {
        # Factor - use the last level
        treatment_level <- levels(data$treatment)[length(levels(
          data$treatment
        ))]
      } else {
        # Numeric or character
        treatment_values <- unique(data$treatment[!is.na(data$treatment)])
        if (length(treatment_values) == 0) {
          treatment_level <- 1 # Default for empty data
        } else {
          treatment_level <- max(treatment_values)
        }
      }
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
      group_signatures <- purrr::map_chr(
        groups,
        create_group_signature,
        aes_cols = aes_cols
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
        treatment_level = treatment_level,
        quantiles = quantiles,
        na.rm = na.rm
      )

      return(results)
    } else {
      # No groups, process all data together
      temp_data <- data.frame(
        .var = data$sample,
        .group = as.integer(data$treatment == treatment_level),
        stringsAsFactors = FALSE
      )

      # Add weight if present
      if (!is.null(data$weight) && all(!is.na(data$weight))) {
        temp_data$.wts <- extract_weight_data(data$weight)
        wts_arg <- ".wts"
      } else {
        wts_arg <- NULL
      }

      qq_result <- qq(
        .data = temp_data,
        .var = .var,
        .group = .group,
        .wts = if (!is.null(wts_arg)) rlang::sym(wts_arg) else NULL,
        quantiles = quantiles,
        treatment_level = 1L, # We already converted to 0/1
        na.rm = na.rm,
        include_observed = FALSE
      )

      # Return data frame without x and y; let `after_stat()` handle that
      data.frame(
        treated_quantiles = qq_result$treated_quantiles,
        untreated_quantiles = qq_result$untreated_quantiles,
        PANEL = data$PANEL[1],
        group = 1
      )
    }
  }
)

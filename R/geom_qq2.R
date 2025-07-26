#' QQ Plot Geom
#'
#' A ggplot2 geom for creating quantile-quantile plots with support for
#' weighted comparisons.
#'
#' @param mapping Set of aesthetic mappings. Required aesthetics are `sample` (variable)
#'   and `treatment` (group). The `treatment` aesthetic must be numeric or converted to numeric
#'   using `as.numeric()` if it's a factor. Optional aesthetics include `weight`
#'   for weighting.
#' @param data Data frame to use. If not specified, inherits from the plot.
#' @param stat Statistical transformation to use. Default is "qq2".
#' @param position Position adjustment. Default is "identity".
#' @param na.rm Remove missing values? Default TRUE.
#' @param show.legend Show legend? Default NA.
#' @param inherit.aes Inherit aesthetics from plot? Default TRUE.
#' @param quantiles Numeric vector of quantiles to compute. Default is
#'   `seq(0.01, 0.99, 0.01)` for 99 quantiles.
#' @param reference_group The reference group level to use for comparisons.
#'   Defaults to 1 (first level).
#' @param ... Additional arguments passed to the geom.
#'
#' @return A ggplot2 layer.
#'
#' @examples
#' library(ggplot2)
#'
#' # Basic QQ plot (note: treatment must be numeric)
#' ggplot(nhefs_weights, aes(sample = age, treatment = as.numeric(qsmk))) +
#'   geom_qq2()
#'
#' # With weighting
#' ggplot(nhefs_weights, aes(sample = age, treatment = as.numeric(qsmk), weight = w_ate)) +
#'   geom_qq2()
#'
#' # Compare multiple weights using long format
#' long_data <- tidyr::pivot_longer(
#'   nhefs_weights,
#'   cols = c(w_ate, w_att),
#'   names_to = "weight_type",
#'   values_to = "weight"
#' )
#'
#' ggplot(long_data, aes(sample = age, treatment = as.numeric(qsmk), weight = weight)) +
#'   geom_qq2(aes(color = weight_type)) +
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
  reference_group = 1L,
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
      reference_group = reference_group,
      ...
    )
  )
}

#' QQ Plot Stat
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
#' @param reference_group The reference group level to use for comparisons.
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
  reference_group = 1L,
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
      reference_group = reference_group,
      include_observed = include_observed,
      ...
    )
  )
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
    x = ggplot2::after_stat(x_quantiles),
    y = ggplot2::after_stat(y_quantiles),
    weight = NULL
  ),
  dropped_aes = "weight",

  # Override compute_panel instead of compute_group to work with all data at once
  compute_panel = function(
    data,
    scales,
    quantiles = seq(0.01, 0.99, 0.01),
    reference_group = 1L,
    na.rm = TRUE,
    include_observed = FALSE
  ) {
    # For QQ plots, we need all the data to compute quantiles properly
    # So we work at the panel level, not the group level

    # Split by any grouping aesthetics (like color)
    if ("group" %in% names(data) && length(unique(data$group)) > 1) {
      # We have groups (e.g., from color aesthetic)
      # Process each group separately
      groups <- split(data, data$group)

      results <- purrr::map_df(names(groups), function(g) {
        group_data <- groups[[g]]

        # Create temporary data frame
        temp_data <- data.frame(
          .var = group_data$sample,
          .group = group_data$treatment,
          stringsAsFactors = FALSE
        )

        # Add weight if present
        if (!is.null(group_data$weight) && all(!is.na(group_data$weight))) {
          temp_data$.wts <- group_data$weight
          wts_arg <- ".wts"
        } else {
          wts_arg <- NULL
        }

        # Compute QQ data
        qq_result <- qq(
          .data = temp_data,
          .var = .var,
          .group = .group,
          .wts = if (!is.null(wts_arg)) rlang::sym(wts_arg) else NULL,
          quantiles = quantiles,
          reference_group = reference_group,
          na.rm = na.rm,
          include_observed = FALSE
        )

        # Build result data frame preserving aesthetics
        # Don't include x and y - let after_stat handle that
        result_df <- data.frame(
          x_quantiles = qq_result$x_quantiles,
          y_quantiles = qq_result$y_quantiles,
          group = as.numeric(g),
          PANEL = group_data$PANEL[1]
        )

        # Preserve any aesthetic mappings (like color)
        aes_cols <- setdiff(
          names(group_data),
          c("sample", "treatment", "weight", "PANEL", "group")
        )
        for (col in aes_cols) {
          if (length(unique(group_data[[col]])) == 1) {
            result_df[[col]] <- group_data[[col]][1]
          }
        }

        result_df
      })

      return(results)
    } else {
      # No groups, process all data together
      temp_data <- data.frame(
        .var = data$sample,
        .group = data$treatment,
        stringsAsFactors = FALSE
      )

      # Add weight if present
      if (!is.null(data$weight) && all(!is.na(data$weight))) {
        temp_data$.wts <- data$weight
        wts_arg <- ".wts"
      } else {
        wts_arg <- NULL
      }

      # Compute QQ data
      qq_result <- qq(
        .data = temp_data,
        .var = .var,
        .group = .group,
        .wts = if (!is.null(wts_arg)) rlang::sym(wts_arg) else NULL,
        quantiles = quantiles,
        reference_group = reference_group,
        na.rm = na.rm,
        include_observed = FALSE
      )

      # Return data frame without x and y - let after_stat handle that
      data.frame(
        x_quantiles = qq_result$x_quantiles,
        y_quantiles = qq_result$y_quantiles,
        PANEL = data$PANEL[1],
        group = 1
      )
    }
  }
)

#' ROC Curve Geom for Causal Inference
#'
#' A ggplot2 geom for plotting ROC curves with optional weighting.
#' Emphasizes the balance interpretation where AUC around 0.5 indicates good balance.
#'
#' @param mapping Set of aesthetic mappings. Must include `estimate` (propensity scores/predictions)
#'   and `exposure` (treatment/outcome variable). If specified, inherits from the plot.
#' @param data Data frame to use. If not specified, inherits from the plot.
#' @param stat Statistical transformation to use. Default is "roc".
#' @param position Position adjustment. Default is "identity".
#' @inheritParams ggplot2_params
#' @param linewidth Width of the ROC curve line. Default is 0.5.
#' @param .focal_level The level of the `exposure` aesthetic to treat as the
#'   event. Must be a level the data actually takes; a declared factor level
#'   that no observation takes is not accepted. If `NULL` (default), the last
#'   observed level is used, which is the maximum value for numeric exposures.
#'
#' @details
#' A curve compares two exposure levels, so each curve is drawn from the rows
#' that share an aesthetic signature, with the exposure level excluded from that
#' signature. Mapping `group` explicitly makes each group a curve of its own,
#' which keeps long data holding several weighting schemes from being pooled
#' into a single curve. A group that holds only one observed exposure level is
#' dropped with a warning, and the remaining curves are still drawn.
#'
#' @return A ggplot2 layer.
#' @family ggplot2 functions
#' @seealso [check_model_auc()] for computing AUC values, [stat_roc()] for the underlying stat
#'
#' @examples
#' # Basic usage
#' library(ggplot2)
#' ggplot(nhefs_weights, aes(estimate = .fitted, exposure = qsmk)) +
#'   geom_roc() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed")
#'
#' # With grouping by weight
#' long_data <- tidyr::pivot_longer(
#'   nhefs_weights,
#'   cols = c(w_ate, w_att),
#'   names_to = "weight_type",
#'   values_to = "weight"
#' )
#'
#' ggplot(long_data, aes(estimate = .fitted, exposure = qsmk, weight = weight)) +
#'   geom_roc(aes(color = weight_type)) +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed")
#'
#' @export
geom_roc <- function(
  mapping = NULL,
  data = NULL,
  stat = "roc",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  linewidth = 0.5,
  .focal_level = NULL,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      linewidth = linewidth,
      .focal_level = .focal_level,
      ...
    )
  )
}

#' ROC Curve Stat
#'
#' Statistical transformation for ROC curves.
#'
#' @inheritParams ggplot2_params
#' @param geom Geometric object to use. Default is "path".
#' @param .focal_level The level of the `exposure` aesthetic to treat as the
#'   event. Must be a level the data actually takes; a declared factor level
#'   that no observation takes is not accepted. If `NULL` (default), the last
#'   observed level is used, which is the maximum value for numeric exposures.
#'
#' @return A ggplot2 layer.
#' @export
stat_roc <- function(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  .focal_level = NULL,
  ...
) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatRoc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      .focal_level = .focal_level,
      ...
    )
  )
}

#' Resolve the focal level of the exposure aesthetic
#'
#' Levels are the OBSERVED levels, so a declared factor level that no
#' observation takes is never chosen as the default and never accepted as a
#' supplied value.
#'
#' @param exposure The exposure aesthetic for the whole layer
#' @param .focal_level The supplied focal level, or NULL
#'
#' @return The focal level, or `NULL` when no exposure value is observed, which
#'   leaves the empty result to the caller.
#' @noRd
resolve_roc_focal_level <- function(
  exposure,
  .focal_level = NULL,
  call = rlang::caller_env()
) {
  observed_levels <- extract_group_levels(
    exposure,
    require_binary = FALSE,
    call = call
  )

  # An exposure with nothing observed leaves no curve to draw. The layer is
  # empty either way, but silently so would look like a plot with no data
  # rather than a plot whose exposure is missing.
  if (length(observed_levels) == 0) {
    warn(
      c(
        "Drawing no ROC curve: {.field exposure} has no observed levels",
        i = "Every value of {.field exposure} is missing."
      ),
      warning_class = "halfmoon_data_warning",
      call = call
    )

    return(NULL)
  }

  if (length(observed_levels) != 2) {
    abort(
      c(
        "{.field exposure} must have exactly two observed levels, not {length(observed_levels)}",
        i = "Observed: {.val {observed_levels}}"
      ),
      error_class = "halfmoon_group_error",
      call = call
    )
  }

  if (is.null(.focal_level)) {
    return(observed_levels[[2]])
  }

  if (!as.character(.focal_level) %in% as.character(observed_levels)) {
    abort(
      c(
        "{.arg .focal_level} {.val {(.focal_level)}} is not an observed level of {.field exposure}",
        i = "Observed: {.val {observed_levels}}"
      ),
      error_class = "halfmoon_reference_error",
      call = call
    )
  }

  .focal_level
}

#' @rdname stat_roc
#' @format NULL
#' @usage NULL
#' @export
StatRoc <- ggplot2::ggproto(
  "StatRoc",
  ggplot2::Stat,
  required_aes = c("estimate", "exposure"),
  non_missing_aes = "weight",
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(fpr), # 1 - specificity
    y = ggplot2::after_stat(tpr), # sensitivity
    weight = 1
  ),
  dropped_aes = "weight", # Tell ggplot2 to drop weight after computation

  setup_params = function(data, params) {
    # `setup_params()` sees the whole layer before it is split into panels, so
    # the focal level is resolved once here and carried into every panel.
    # Resolving it per panel would let a panel holding a single exposure level
    # take that level as the focal one.
    if (!is.null(data$exposure)) {
      params$.focal_level <- resolve_roc_focal_level(
        data$exposure,
        params$.focal_level,
        call = quote(geom_roc())
      )
    }

    params
  },

  compute_panel = function(data, scales, .focal_level = NULL) {
    # Nothing observed to compare
    empty_result <- data.frame(
      fpr = numeric(0),
      tpr = numeric(0),
      group = integer(0)
    )

    if (is.null(.focal_level) || nrow(data) == 0) {
      return(empty_result)
    }

    # If we have multiple groups, identify which ones should be merged
    if ("group" %in% names(data) && length(unique(data$group)) > 1) {
      groups <- split(data, data$group)

      # Create signatures for each group based on aesthetic values
      # We want to merge groups that differ only by exposure factor levels
      # but preserve groups that differ by other aesthetics like colour
      aes_cols <- setdiff(
        names(data),
        c(
          "estimate",
          "exposure",
          "weight",
          "PANEL",
          "group",
          "x",
          "y",
          "fpr",
          "tpr"
        )
      )

      group_signatures <- purrr::imap_chr(
        groups,
        function(group_data, group_id) {
          # ggplot2 builds the default group from every discrete aesthetic, the
          # exposure included, so groups that differ only by exposure level are
          # two halves of one curve. A group that already holds both exposure
          # levels comes from an explicit `group` aesthetic and is a curve in
          # its own right.
          if (length(unique(group_data$exposure)) > 1) {
            paste0("group_", group_id)
          } else {
            create_group_signature(group_data, aes_cols)
          }
        }
      )

      # Process groups with the same signature together
      unique_signatures <- unique(group_signatures)
      results <- purrr::map_df(unique_signatures, \(sig) {
        matching_groups <- names(groups)[group_signatures == sig]
        combined_data <- do.call(rbind, groups[matching_groups])

        # Use the first matching group's group ID
        group_id <- groups[[matching_groups[1]]]$group[1]

        # Process the combined data
        compute_roc_for_group(combined_data, .focal_level, group_id)
      })

      if (nrow(results) == 0) {
        return(empty_result)
      }

      results
    } else {
      # Single group or no groups
      result <- compute_roc_for_group(data, .focal_level, data$group[1])
      result %||% empty_result
    }
  }
)

# Helper function to compute ROC for a single curve. Returns `NULL` when the
# curve holds a single observed exposure level, so that the curves that can be
# computed are still drawn.
compute_roc_for_group <- function(data, .focal_level, group_id) {
  # Extract estimate (predictor) and exposure
  estimate <- data$estimate
  exposure <- data$exposure
  weights <- data$weight %||% rep(1, length(estimate))

  # One curve separates two exposure levels, so a curve that holds a single
  # level is dropped on its own rather than costing every other curve in the
  # panel
  observed_levels <- extract_group_levels(exposure, require_binary = FALSE)

  if (length(observed_levels) < 2) {
    warn(
      c(
        "Dropping a group that does not have two observed exposure levels",
        i = "Observed: {.val {observed_levels}}"
      ),
      warning_class = "halfmoon_data_warning",
      call = quote(geom_roc())
    )

    return(NULL)
  }

  # Handle both factor and non-factor exposure variables
  if (is.factor(exposure)) {
    # For factors, ensure we're comparing as character to handle numeric-looking levels
    exposure_binary <- as.integer(
      as.character(exposure) == as.character(.focal_level)
    )
  } else {
    exposure_binary <- as.integer(exposure == .focal_level)
  }

  # Create a factor for compute_roc_curve_imp
  exposure_factor <- factor(exposure_binary, levels = c(0, 1))

  roc_data <- compute_roc_curve_imp(
    exposure_factor,
    estimate,
    weights,
    .focal_level = "1" # We've already converted to binary
  )

  # Get aesthetic columns to preserve (like colour, linetype, etc.)
  aes_cols <- setdiff(
    names(data),
    c("estimate", "exposure", "weight", "PANEL", "group", "x", "y")
  )

  # Create base result
  result <- data.frame(
    fpr = 1 - roc_data$specificity,
    tpr = roc_data$sensitivity,
    PANEL = data$PANEL[1],
    group = group_id
  )

  # Add aesthetic columns if they exist
  if (length(aes_cols) > 0) {
    # Use the first row's values since they should be constant within the group
    for (col in aes_cols) {
      result[[col]] <- data[[col]][1]
    }
  }

  result
}

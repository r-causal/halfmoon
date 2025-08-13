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
#' @inheritParams treatment_param
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
#' @inheritParams treatment_param
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

#' @rdname stat_roc
#' @format NULL
#' @usage NULL
#' @export
StatRoc <- ggplot2::ggproto(
  "StatRoc",
  ggplot2::Stat,
  required_aes = c("estimate", "exposure"),
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(fpr), # 1 - specificity
    y = ggplot2::after_stat(tpr), # sensitivity
    weight = 1
  ),
  dropped_aes = "weight", # Tell ggplot2 to drop weight after computation

  compute_panel = function(data, scales, na.rm = TRUE, .focal_level = NULL) {
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

      group_signatures <- purrr::map_chr(
        groups,
        create_group_signature,
        aes_cols = aes_cols
      )

      # Process groups with the same signature together
      unique_signatures <- unique(group_signatures)
      results <- purrr::map_df(unique_signatures, \(sig) {
        matching_groups <- names(groups)[group_signatures == sig]
        combined_data <- do.call(rbind, groups[matching_groups])

        # Use the first matching group's group ID
        group_id <- groups[[matching_groups[1]]]$group[1]

        # Process the combined data
        compute_roc_for_group(combined_data, na.rm, .focal_level, group_id)
      })

      results
    } else {
      # Single group or no groups
      compute_roc_for_group(data, na.rm, .focal_level, data$group[1])
    }
  }
)

# Helper function to compute ROC for a single group
compute_roc_for_group <- function(data, na.rm, .focal_level, group_id) {
  # Extract estimate (predictor) and exposure
  estimate <- data$estimate
  exposure <- data$exposure
  weights <- data$weight %||% rep(1, length(estimate))

  # Remove missing values if requested
  if (na.rm) {
    complete_cases <- stats::complete.cases(estimate, exposure, weights)
    estimate <- estimate[complete_cases]
    exposure <- exposure[complete_cases]
    weights <- weights[complete_cases]
  }

  # Check that exposure has exactly 2 unique values
  unique_exposure <- if (is.factor(exposure)) {
    levels(exposure)
  } else {
    unique(exposure[!is.na(exposure)])
  }

  if (length(unique_exposure) != 2) {
    abort(
      "exposure must have exactly 2 unique values for ROC curve",
      error_class = "halfmoon_group_error"
    )
  }

  # Convert exposure to binary
  if (is.null(.focal_level)) {
    .focal_level <- if (is.factor(exposure)) {
      levels(exposure)[length(levels(exposure))]
    } else {
      max(unique_exposure)
    }
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

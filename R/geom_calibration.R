#' Compute calibration data for binary outcomes
#'
#' This function summarizes predicted probabilities and observed outcomes into bins,
#' computing mean prediction, observed rate, counts, and confidence intervals.
#'
#' @param data A data frame or tibble containing the data.
#' @param x Unquoted column name or string of predicted probabilities (numeric between 0 and 1).
#' @param y Unquoted column name or string of observed binary outcomes (0/1).
#' @param bins Integer >1; number of bins.
#' @param binning_method "equal_width" or "quantile" for bin creation.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param na.rm Logical; if TRUE, drop NA x or y before summarizing.
#'
#' @return A tibble with columns:
#'   - .bin: integer bin index
#'   - x_mean: mean predicted probability in bin
#'   - y_mean: observed outcome rate in bin
#'   - count: number of observations in bin
#'   - lower: lower bound of CI for y_mean
#'   - upper: upper bound of CI for y_mean
#' @export
check_calibration <- function(data, x, y,
                              bins = 10,
                              binning_method = c("equal_width", "quantile"),
                              conf_level = 0.95,
                              na.rm = FALSE) {
  binning_method <- match.arg(binning_method)
  if (!is.numeric(bins) || bins < 2 || bins != round(bins)) {
    stop("`bins` must be an integer > 1.")
  }

  # Extract column names handling different input types
  if (is.character(x)) {
    x_name <- x
  } else {
    x_name <- rlang::as_name(rlang::enquo(x))
  }

  if (is.character(y)) {
    y_name <- y
  } else {
    y_name <- rlang::as_name(rlang::enquo(y))
  }

  # Create a new data frame with only the needed columns and standardized names
  df <- data.frame(
    x_var = data[[x_name]],
    y_var = data[[y_name]]
  )

  if (na.rm) {
    df <- df[!is.na(df$x_var) & !is.na(df$y_var), ]
  }

  if (nrow(df) == 0) {
    return(tibble::tibble(
      .bin = integer(0),
      x_mean = numeric(0),
      y_mean = numeric(0),
      count = integer(0),
      lower = numeric(0),
      upper = numeric(0)
    ))
  }

  # Determine breaks
  xs <- df$x_var
  if (binning_method == "equal_width") {
    brks <- seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE), length.out = bins + 1)
  } else {
    probs <- seq(0, 1, length.out = bins + 1)
    brks <- unique(stats::quantile(xs, probs = probs, na.rm = TRUE))
    if (length(brks) <= 2) {
      brks <- seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE), length.out = bins + 1)
    }
  }

  # Assign bins - make sure it's numeric
  df$.bin <- as.integer(cut(xs, breaks = brks, include.lowest = TRUE, labels = FALSE))

  # Use base R aggregation for more direct control
  bin_summary <- stats::aggregate(
    cbind(x_var, y_var) ~ .bin,
    data = df,
    FUN = mean,
    na.rm = TRUE
  )
  names(bin_summary)[names(bin_summary) == "x_var"] <- "x_mean"
  names(bin_summary)[names(bin_summary) == "y_var"] <- "y_mean"

  # Count observations per bin
  bin_counts <- stats::aggregate(
    y_var ~ .bin,
    data = df,
    FUN = length
  )
  names(bin_counts)[names(bin_counts) == "y_var"] <- "count"

  # Merge summaries
  result <- merge(bin_summary, bin_counts, by = ".bin")

  # Sort by bin
  result <- result[order(result$.bin), ]

  # Compute CIs
  cis <- lapply(1:nrow(result), function(i) {
    n <- result$count[i]
    successes <- round(result$y_mean[i] * n)

    # Handle edge cases where prop.test might fail
    if (n == 0 || successes < 0 || successes > n) {
      return(c(NA_real_, NA_real_))
    }

    tryCatch({
      ci <- stats::prop.test(
        x = successes,
        n = n,
        conf.level = conf_level,
        correct = FALSE
      )$conf.int
      return(ci)
    }, error = function(e) {
      return(c(NA_real_, NA_real_))
    })
  })

  # Extract the CIs
  result$lower <- sapply(cis, function(ci) ci[1])
  result$upper <- sapply(cis, function(ci) ci[2])

  # Convert to tibble for consistency with other functions
  tibble::as_tibble(result)
}

#' Stat for calibration geom (bins and summarizes by panel/group)
#' @noRd
StatCalibration <- ggplot2::ggproto(
  "StatCalibration", ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(),

  compute_panel = function(data, scales, bins = 10,
                           binning_method = "equal_width",
                           conf_level = 0.95, na.rm = FALSE) {
    # Create a data frame for check_calibration
    cal <- check_calibration(
      data = data,
      x = "x",
      y = "y",
      bins = bins,
      binning_method = binning_method,
      conf_level = conf_level,
      na.rm = na.rm
    )

    # Make sure x and y are available for ggplot aesthetics
    cal$x <- cal$x_mean
    cal$y <- cal$y_mean

    return(cal)
  }
)

#' Geom for calibration plot with CIs
#'
#' @inheritParams check_calibration
#' @param mapping Aesthetic mapping (must supply x and y if not inherited).
#' @param data Data frame or tibble; if NULL, uses ggplot default.
#' @param position Position adjustment.
#' @param show_line Logical; draw line between points.
#' @param show_points Logical; draw points at each bin.
#' @param show_ci Logical; draw error bars for CIs.
#' @param reference_line Logical; add y=x diagonal.
#' @param show.legend Logical; include in legend.
#' @param inherit.aes Logical; inherit aesthetics from ggplot.
#' @param ... Additional parameters passed to geoms.
#' @return A ggplot2 layer or list of layers
#' @export
geom_calibration <- function(mapping = NULL, data = NULL,
                             position = "identity",
                             bins = 10,
                             binning_method = c("equal_width", "quantile"),
                             conf_level = 0.95,
                             show_line = TRUE,
                             show_points = TRUE,
                             show_ci = TRUE,
                             reference_line = FALSE,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             ...) {
  binning_method <- match.arg(binning_method)
  if (!is.numeric(bins) || bins < 2 || bins != round(bins)) {
    stop("`bins` must be an integer > 1.")
  }

  # Define NULL coalescing operator if not available
  `%||%` <- function(x, y) if (is.null(x)) y else x

  # Collection of layers to be returned
  layers <- list()

  # Add reference line if requested
  if (reference_line) {
    layers <- c(layers, list(ggplot2::geom_abline(
      slope = 1, intercept = 0, linetype = "dashed", color = "gray50"
    )))
  }

  # Common layer parameters
  common_params <- list(
    stat = StatCalibration,
    data = data,
    mapping = mapping,
    position = position,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binning_method = binning_method,
      conf_level = conf_level,
      na.rm = na.rm
    )
  )

  # Add error bars for confidence intervals
  if (show_ci) {
    errorbar_params <- common_params
    errorbar_params$geom <- "errorbar"
    errorbar_params$show.legend <- FALSE  # No need for error bars in legend
    errorbar_params$params$width <- 0.1   # Default width for error bars

    # Create the layer
    ci_layer <- do.call(ggplot2::layer, errorbar_params)

    # Add the error bar mapping
    ci_layer$mapping <- utils::modifyList(
      ci_layer$mapping %||% ggplot2::aes(),
      ggplot2::aes(ymin = after_stat(lower), ymax = after_stat(upper))
    )

    layers <- c(layers, list(ci_layer))
  }

  # Add the line connecting points
  if (show_line) {
    line_params <- common_params
    line_params$geom <- "line"
    line_params$show.legend <- show.legend

    # Create the layer
    line_layer <- do.call(ggplot2::layer, line_params)

    layers <- c(layers, list(line_layer))
  }

  # Add the points at each bin
  if (show_points) {
    point_params <- common_params
    point_params$geom <- "point"
    point_params$show.legend <- show.legend

    # Create the layer
    point_layer <- do.call(ggplot2::layer, point_params)

    layers <- c(layers, list(point_layer))
  }

  # Return a single layer or a list of layers
  if (length(layers) == 1) layers[[1]] else layers
}

#' Compute calibration data for binary outcomes
#'
#' This function summarizes predicted probabilities and observed outcomes into bins,
#' computing mean prediction, observed rate, counts, and confidence intervals.
#'
#' @param data A data frame or tibble containing the data.
#' @param x Unquoted column name or string of predicted probabilities (numeric between 0 and 1).
#' @param y Unquoted column name or string of observed binary outcomes (0/1).
#' @param bins Integer >1; number of bins for the "breaks" method.
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
#' @importFrom stats prop.test quantile aggregate
#' @importFrom tibble as_tibble tibble
#' @export
check_calibration <- function(
  data,
  x,
  y,
  bins = 10,
  binning_method = c("equal_width", "quantile"),
  conf_level = 0.95,
  na.rm = FALSE
) {
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
    brks <- seq(
      min(xs, na.rm = TRUE),
      max(xs, na.rm = TRUE),
      length.out = bins + 1
    )
  } else {
    probs <- seq(0, 1, length.out = bins + 1)
    brks <- unique(stats::quantile(xs, probs = probs, na.rm = TRUE))
    if (length(brks) <= 2) {
      brks <- seq(
        min(xs, na.rm = TRUE),
        max(xs, na.rm = TRUE),
        length.out = bins + 1
      )
    }
  }

  # Assign bins - make sure it's numeric
  df$.bin <- as.integer(cut(
    xs,
    breaks = brks,
    include.lowest = TRUE,
    labels = FALSE
  ))

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

    tryCatch(
      {
        ci <- stats::prop.test(
          x = successes,
          n = n,
          conf.level = conf_level,
          correct = FALSE
        )$conf.int
        return(ci)
      },
      error = function(e) {
        return(c(NA_real_, NA_real_))
      }
    )
  })

  # Extract the CIs
  result$lower <- sapply(cis, function(ci) ci[1])
  result$upper <- sapply(cis, function(ci) ci[2])

  # Convert to tibble for consistency with other functions
  tibble::as_tibble(result)
}

# Define NULL coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Stat for computing calibration statistics
StatCalibration <- ggplot2::ggproto(
  "StatCalibration",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(alpha = 0.3),

  setup_params = function(data, params) {
    # Set default parameters
    params$method <- params$method %||% "breaks"
    params$bins <- params$bins %||% 10
    params$smooth <- params$smooth %||% TRUE
    params$conf_level <- params$conf_level %||% 0.95
    params$window_size <- params$window_size %||% 0.1
    params$step_size <- params$step_size %||% (params$window_size / 2)
    params
  },

  compute_group = function(
    data,
    scales,
    method = "breaks",
    bins = 10,
    smooth = TRUE,
    conf_level = 0.95,
    window_size = 0.1,
    step_size = window_size / 2
  ) {
    if (method == "breaks") {
      compute_calibration_breaks(data, bins, conf_level)
    } else if (method == "logistic") {
      compute_calibration_logistic(data, smooth, conf_level)
    } else if (method == "windowed") {
      compute_calibration_windowed(data, window_size, step_size, conf_level)
    } else {
      stop("Method must be 'breaks', 'logistic', or 'windowed'")
    }
  }
)

# Helper function for breaks method
compute_calibration_breaks <- function(data, bins, conf_level) {
  # Create breaks from 0 to 1
  breaks <- seq(0, 1, length.out = bins + 1)

  # Assign each prediction to a bin
  bin_assignments <- cut(data$x, breaks = breaks, include.lowest = TRUE)

  # Calculate observed event rates for each bin using base R
  bin_data <- data.frame(
    x = data$x,
    y = data$y,
    bin = bin_assignments
  )

  # Remove rows with NA bins
  bin_data <- bin_data[!is.na(bin_data$bin), ]

  # Summarize by bin using base R
  bin_levels <- levels(bin_assignments)
  bin_summary <- data.frame(
    bin = factor(bin_levels, levels = bin_levels),
    n_events = numeric(length(bin_levels)),
    n_total = numeric(length(bin_levels)),
    event_rate = numeric(length(bin_levels)),
    x = numeric(length(bin_levels)),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(bin_levels)) {
    bin_subset <- bin_data[bin_data$bin == bin_levels[i], ]
    if (nrow(bin_subset) > 0) {
      bin_summary$n_events[i] <- sum(bin_subset$y)
      bin_summary$n_total[i] <- nrow(bin_subset)
      bin_summary$event_rate[i] <- bin_summary$n_events[i] /
        bin_summary$n_total[i]
      bin_summary$x[i] <- mean(bin_subset$x)
    }
  }

  # Remove empty bins
  bin_summary <- bin_summary[bin_summary$n_total > 0, ]

  # Add confidence intervals using prop.test
  alpha <- 1 - conf_level

  bin_summary$ymin <- NA
  bin_summary$ymax <- NA

  for (i in seq_len(nrow(bin_summary))) {
    if (
      bin_summary$n_total[i] > 0 &&
        bin_summary$n_events[i] > 0 &&
        bin_summary$n_events[i] < bin_summary$n_total[i]
    ) {
      prop_test <- prop.test(
        bin_summary$n_events[i],
        bin_summary$n_total[i],
        conf.level = conf_level
      )
      bin_summary$ymin[i] <- prop_test$conf.int[1]
      bin_summary$ymax[i] <- prop_test$conf.int[2]
    } else {
      # For edge cases, use simple approximation
      se <- sqrt(
        bin_summary$event_rate[i] *
          (1 - bin_summary$event_rate[i]) /
          bin_summary$n_total[i]
      )
      bin_summary$ymin[i] <- max(
        0,
        bin_summary$event_rate[i] - qnorm(1 - alpha / 2) * se
      )
      bin_summary$ymax[i] <- min(
        1,
        bin_summary$event_rate[i] + qnorm(1 - alpha / 2) * se
      )
    }
  }

  data.frame(
    x = bin_summary$x,
    y = bin_summary$event_rate,
    ymin = bin_summary$ymin,
    ymax = bin_summary$ymax
  )
}

# Helper function for logistic method
compute_calibration_logistic <- function(data, smooth, conf_level) {
  # Fit model
  if (smooth) {
    rlang::check_installed("mgcv", "for GAM smoothing")
    model <- mgcv::gam(y ~ s(x, k = 10), data = data, family = binomial())
  } else {
    model <- glm(y ~ x, data = data, family = binomial())
  }

  # Create prediction sequence
  pred_seq <- seq(min(data$x), max(data$x), length.out = 100)
  new_data <- data.frame(x = pred_seq)

  # Get predictions with confidence intervals
  preds <- predict(model, new_data, se.fit = TRUE)
  pred_probs <- plogis(preds$fit)

  # Calculate confidence intervals
  alpha <- 1 - conf_level
  z_score <- qnorm(1 - alpha / 2)
  ymin <- plogis(preds$fit - z_score * preds$se.fit)
  ymax <- plogis(preds$fit + z_score * preds$se.fit)

  data.frame(
    x = pred_seq,
    y = pred_probs,
    ymin = ymin,
    ymax = ymax
  )
}

# Helper function for windowed method
compute_calibration_windowed <- function(
  data,
  window_size,
  step_size,
  conf_level
) {
  # Create window centers
  steps <- seq(0, 1, by = step_size)

  # Initialize results
  window_results <- list()

  for (i in seq_along(steps)) {
    # Define window boundaries
    lower_bound <- max(0, steps[i] - (window_size / 2))
    upper_bound <- min(1, steps[i] + (window_size / 2))

    # Find observations in this window
    in_window <- data$x >= lower_bound & data$x <= upper_bound

    if (sum(in_window) > 0) {
      # Calculate statistics for this window
      n_events <- sum(data$y[in_window])
      n_total <- sum(in_window)
      event_rate <- n_events / n_total

      # Calculate confidence intervals
      alpha <- 1 - conf_level
      if (n_events > 0 && n_events < n_total) {
        prop_test <- prop.test(n_events, n_total, conf.level = conf_level)
        lower_ci <- prop_test$conf.int[1]
        upper_ci <- prop_test$conf.int[2]
      } else {
        # For edge cases, use normal approximation
        se <- sqrt(event_rate * (1 - event_rate) / n_total)
        z_score <- qnorm(1 - alpha / 2)
        lower_ci <- max(0, event_rate - z_score * se)
        upper_ci <- min(1, event_rate + z_score * se)
      }

      # Store results
      window_results[[i]] <- data.frame(
        x = steps[i],
        y = event_rate,
        ymin = lower_ci,
        ymax = upper_ci
      )
    }
  }

  # Combine all windows
  do.call(rbind, window_results)
}

# Geom for calibration line
GeomCalibrationLine <- ggplot2::ggproto(
  "GeomCalibrationLine",
  ggplot2::GeomLine,
  default_aes = ggplot2::aes(
    colour = "blue",
    linewidth = 0.5,
    linetype = 1,
    alpha = NA
  )
)

# Geom for calibration ribbon
GeomCalibrationRibbon <- ggplot2::ggproto(
  "GeomCalibrationRibbon",
  ggplot2::GeomRibbon,
  default_aes = ggplot2::aes(
    colour = NA,
    fill = "blue",
    linewidth = 0.5,
    linetype = 1,
    alpha = 0.3
  )
)

# Geom for calibration points
GeomCalibrationPoint <- ggplot2::ggproto(
  "GeomCalibrationPoint",
  ggplot2::GeomPoint,
  default_aes = ggplot2::aes(
    colour = "blue",
    size = 1.5,
    alpha = NA,
    shape = 19,
    fill = NA,
    stroke = 0.5
  )
)

#' Geom for calibration plot with confidence intervals
#'
#' This geom creates calibration plots to assess the agreement between predicted
#' probabilities and observed binary outcomes. It supports three methods:
#' binning ("breaks"), logistic regression ("logistic"), and windowed ("windowed").
#'
#' @param mapping Aesthetic mapping (must supply x and y if not inherited).
#' @param data Data frame or tibble; if NULL, uses ggplot default.
#' @param method Character; calibration method - "breaks", "logistic", or "windowed".
#' @param bins Integer >1; number of bins for the "breaks" method.
#' @param smooth Logical; for "logistic" method, use GAM smoothing if available.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param window_size Numeric; size of each window for "windowed" method.
#' @param step_size Numeric; distance between window centers for "windowed" method.
#' @param show_ribbon Logical; show confidence interval ribbon.
#' @param show_points Logical; show points (only for "breaks" and "windowed" methods).
#' @param position Position adjustment.
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#' @param show.legend Logical; include in legend.
#' @param inherit.aes Logical; inherit aesthetics from ggplot.
#' @param ... Additional parameters passed to geoms.
#' @return A ggplot2 layer or list of layers
#' @importFrom stats glm binomial predict plogis qnorm
#' @export
geom_calibration <- function(
  mapping = NULL,
  data = NULL,
  method = "breaks",
  bins = 10,
  smooth = TRUE,
  conf_level = 0.95,
  window_size = 0.1,
  step_size = window_size / 2,
  show_ribbon = TRUE,
  show_points = TRUE,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layers <- list()

  # Add ribbon first (so it's behind the line)
  if (show_ribbon) {
    ribbon_layer <- ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatCalibration,
      geom = GeomCalibrationRibbon,
      position = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        method = method,
        bins = bins,
        smooth = smooth,
        conf_level = conf_level,
        window_size = window_size,
        step_size = step_size,
        na.rm = na.rm
      )
    )
    layers <- c(layers, list(ribbon_layer))
  }

  # Add the main line
  cal_stat <- ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatCalibration,
    geom = GeomCalibrationLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      bins = bins,
      smooth = smooth,
      conf_level = conf_level,
      window_size = window_size,
      step_size = step_size,
      na.rm = na.rm,
      ...
    )
  )
  layers <- c(layers, list(cal_stat))

  # Add points if requested (only for breaks and windowed methods)
  if (show_points && method %in% c("breaks", "windowed")) {
    point_layer <- ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatCalibration,
      geom = GeomCalibrationPoint,
      position = position,
      show.legend = FALSE,
      inherit.aes = inherit.aes,
      params = list(
        method = method,
        bins = bins,
        smooth = smooth,
        conf_level = conf_level,
        window_size = window_size,
        step_size = step_size,
        na.rm = na.rm
      )
    )
    layers <- c(layers, list(point_layer))
  }

  layers
}

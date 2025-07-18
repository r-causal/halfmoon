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

  # Compute CIs - optimized version
  n_rows <- nrow(result)
  result$lower <- numeric(n_rows)
  result$upper <- numeric(n_rows)

  # Vectorized computation where possible
  n_total <- result$count
  n_events <- round(result$y_mean * n_total)

  # Handle edge cases up front
  valid_mask <- n_total > 0 &
    n_events >= 0 &
    n_events <= n_total &
    n_events > 0 &
    n_events < n_total

  # For valid cases, compute CIs using purrr
  valid_indices <- which(valid_mask)
  if (length(valid_indices) > 0) {
    ci_results <- purrr::map(
      valid_indices,
      ~ {
        tryCatch(
          {
            ci <- stats::prop.test(
              x = n_events[.x],
              n = n_total[.x],
              conf.level = conf_level
            )$conf.int
            list(lower = ci[1], upper = ci[2])
          },
          error = function(e) {
            list(lower = NA_real_, upper = NA_real_)
          }
        )
      }
    )

    # Extract results
    result$lower[valid_indices] <- purrr::map_dbl(ci_results, ~ .x$lower)
    result$upper[valid_indices] <- purrr::map_dbl(ci_results, ~ .x$upper)
  }

  # For edge cases, use normal approximation with purrr
  edge_cases <- which(!valid_mask & n_total > 0)
  if (length(edge_cases) > 0) {
    alpha <- 1 - conf_level
    z_score <- stats::qnorm(1 - alpha / 2)

    edge_results <- purrr::map(
      edge_cases,
      ~ {
        rate <- result$y_mean[.x]
        se <- sqrt(rate * (1 - rate) / n_total[.x])
        list(
          lower = max(0, rate - z_score * se),
          upper = min(1, rate + z_score * se)
        )
      }
    )

    result$lower[edge_cases] <- purrr::map_dbl(edge_results, ~ .x$lower)
    result$upper[edge_cases] <- purrr::map_dbl(edge_results, ~ .x$upper)
  }

  # Set NA for completely invalid cases
  invalid_mask <- n_total == 0
  result$lower[invalid_mask] <- NA_real_
  result$upper[invalid_mask] <- NA_real_

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

  # Summarize by bin using tapply for better performance
  bin_indices <- as.integer(bin_data$bin)
  unique_bins <- unique(bin_indices)
  unique_bins <- unique_bins[!is.na(unique_bins)]

  # Use purrr for vectorized computation
  bin_list <- split(bin_data, bin_indices)
  n_events <- purrr::map_dbl(bin_list, ~ sum(.x$y))
  n_total <- purrr::map_int(bin_list, ~ nrow(.x))
  x_mean <- purrr::map_dbl(bin_list, ~ mean(.x$x))

  # Create summary data frame
  bin_summary <- data.frame(
    bin_idx = as.integer(names(bin_list)),
    n_events = n_events,
    n_total = n_total,
    x = x_mean,
    stringsAsFactors = FALSE
  )

  # Calculate event rates vectorized
  bin_summary$event_rate <- bin_summary$n_events / bin_summary$n_total

  # Filter out empty bins
  bin_summary <- bin_summary[bin_summary$n_total > 0, ]

  # Add confidence intervals - optimized version
  alpha <- 1 - conf_level
  z_score <- stats::qnorm(1 - alpha / 2)

  # Pre-allocate vectors
  bin_summary$ymin <- numeric(nrow(bin_summary))
  bin_summary$ymax <- numeric(nrow(bin_summary))

  # Identify valid cases for prop.test
  valid_cases <- bin_summary$n_total > 0 &
    bin_summary$n_events > 0 &
    bin_summary$n_events < bin_summary$n_total

  # For valid cases, use prop.test with purrr
  valid_indices <- which(valid_cases)
  if (length(valid_indices) > 0) {
    ci_results <- purrr::map(
      valid_indices,
      ~ {
        tryCatch(
          {
            prop_test <- stats::prop.test(
              bin_summary$n_events[.x],
              bin_summary$n_total[.x],
              conf.level = conf_level
            )
            list(ymin = prop_test$conf.int[1], ymax = prop_test$conf.int[2])
          },
          error = function(e) {
            # Fallback to normal approximation
            se <- sqrt(
              bin_summary$event_rate[.x] *
                (1 - bin_summary$event_rate[.x]) /
                bin_summary$n_total[.x]
            )
            list(
              ymin = max(0, bin_summary$event_rate[.x] - z_score * se),
              ymax = min(1, bin_summary$event_rate[.x] + z_score * se)
            )
          }
        )
      }
    )

    bin_summary$ymin[valid_indices] <- purrr::map_dbl(ci_results, ~ .x$ymin)
    bin_summary$ymax[valid_indices] <- purrr::map_dbl(ci_results, ~ .x$ymax)
  }

  # For edge cases, use normal approximation vectorized
  edge_cases <- which(!valid_cases & bin_summary$n_total > 0)
  if (length(edge_cases) > 0) {
    rates <- bin_summary$event_rate[edge_cases]
    se <- sqrt(rates * (1 - rates) / bin_summary$n_total[edge_cases])
    bin_summary$ymin[edge_cases] <- pmax(0, rates - z_score * se)
    bin_summary$ymax[edge_cases] <- pmin(1, rates + z_score * se)
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

# Helper function for windowed method - optimized
compute_calibration_windowed <- function(
  data,
  window_size,
  step_size,
  conf_level
) {
  # Create window centers
  steps <- seq(0, 1, by = step_size)
  n_steps <- length(steps)

  # Pre-calculate constants
  alpha <- 1 - conf_level
  z_score <- stats::qnorm(1 - alpha / 2)
  half_window <- window_size / 2

  # Sort data once for more efficient window lookups
  data_x <- data$x
  data_y <- data$y

  # Process each window using purrr
  window_results <- purrr::map(
    steps,
    ~ {
      # Define window boundaries
      lower_bound <- max(0, .x - half_window)
      upper_bound <- min(1, .x + half_window)

      # Find observations in this window
      in_window <- data_x >= lower_bound & data_x <= upper_bound
      n_total <- sum(in_window)

      if (n_total > 0) {
        # Calculate statistics for this window
        n_events <- sum(data_y[in_window])
        event_rate <- n_events / n_total

        # Calculate confidence intervals
        if (n_events > 0 && n_events < n_total) {
          tryCatch(
            {
              prop_test <- stats::prop.test(
                n_events,
                n_total,
                conf.level = conf_level
              )
              list(
                x = .x,
                y = event_rate,
                ymin = prop_test$conf.int[1],
                ymax = prop_test$conf.int[2],
                valid = TRUE
              )
            },
            error = function(e) {
              # Fallback to normal approximation
              se <- sqrt(event_rate * (1 - event_rate) / n_total)
              list(
                x = .x,
                y = event_rate,
                ymin = max(0, event_rate - z_score * se),
                ymax = min(1, event_rate + z_score * se),
                valid = TRUE
              )
            }
          )
        } else {
          # For edge cases, use normal approximation
          se <- sqrt(event_rate * (1 - event_rate) / n_total)
          list(
            x = .x,
            y = event_rate,
            ymin = max(0, event_rate - z_score * se),
            ymax = min(1, event_rate + z_score * se),
            valid = TRUE
          )
        }
      } else {
        # Invalid window
        list(valid = FALSE)
      }
    }
  )

  # Filter to valid windows only
  valid_results <- purrr::keep(window_results, ~ .x$valid)

  # Return only valid windows
  if (length(valid_results) > 0) {
    data.frame(
      x = purrr::map_dbl(valid_results, ~ .x$x),
      y = purrr::map_dbl(valid_results, ~ .x$y),
      ymin = purrr::map_dbl(valid_results, ~ .x$ymin),
      ymax = purrr::map_dbl(valid_results, ~ .x$ymax)
    )
  } else {
    # Return empty data frame with correct structure
    data.frame(
      x = numeric(0),
      y = numeric(0),
      ymin = numeric(0),
      ymax = numeric(0)
    )
  }
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

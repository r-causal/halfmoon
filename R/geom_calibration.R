#' Compute calibration data for binary outcomes
#'
#' This function summarizes predicted probabilities and observed outcomes into bins,
#' computing mean prediction, observed rate, counts, and confidence intervals.
#'
#' @param data A data frame or tibble containing the data.
#' @param .fitted Column name of predicted probabilities (numeric between 0 and 1).
#'   Can be unquoted (e.g., `.fitted`) or quoted (e.g., `".fitted"`).
#' @param .group Column name of treatment/group variable.
#'   Can be unquoted (e.g., `qsmk`) or quoted (e.g., `"qsmk"`).
#' @param treatment_level Value indicating which level of `.group` represents treatment.
#'   If NULL (default), uses the last level for factors or max value for numeric.
#' @param bins Integer >1; number of bins for the "breaks" method.
#' @param binning_method "equal_width" or "quantile" for bin creation.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param na.rm Logical; if TRUE, drop NA values before summarizing.
#'
#' @return A tibble with columns:
#'   - .bin: integer bin index
#'   - fitted_mean: mean predicted probability in bin
#'   - group_mean: observed treatment rate in bin
#'   - count: number of observations in bin
#'   - lower: lower bound of CI for group_mean
#'   - upper: upper bound of CI for group_mean
#' @examples
#' # Use the included nhefs_weights dataset
#' # .fitted contains propensity scores, qsmk is the treatment variable
#' check_calibration(nhefs_weights, .fitted, qsmk)
#'
#' # Both quoted and unquoted column names work
#' check_calibration(nhefs_weights, ".fitted", "qsmk")
#'
#' # Specify treatment level explicitly (useful for multi-level factors)
#' check_calibration(nhefs_weights, .fitted, qsmk, treatment_level = "1")
#'
#' # Different binning methods
#' check_calibration(nhefs_weights, .fitted, qsmk, binning_method = "quantile")
#' @importFrom stats prop.test quantile
#' @export
check_calibration <- function(
  data,
  .fitted,
  .group,
  treatment_level = NULL,
  bins = 10,
  binning_method = c("equal_width", "quantile"),
  conf_level = 0.95,
  na.rm = FALSE
) {
  binning_method <- match.arg(binning_method)
  if (!is.numeric(bins) || bins < 2 || bins != round(bins)) {
    stop("`bins` must be an integer > 1.")
  }

  # Extract column names using rlang - handle both quoted and unquoted
  fitted_quo <- rlang::enquo(.fitted)
  group_quo <- rlang::enquo(.group)

  # Function to extract column name from quosure
  get_column_name <- function(quo, arg_name) {
    # First try as_name (works for symbols and strings)
    tryCatch(
      {
        rlang::as_name(quo)
      },
      error = function(e) {
        # If as_name fails, try to evaluate the quosure
        val <- tryCatch(
          {
            rlang::eval_tidy(quo)
          },
          error = function(e2) {
            stop(paste0(
              "`",
              arg_name,
              "` must be a column name (quoted or unquoted)"
            ))
          }
        )

        # Handle different types of evaluated values
        if (is.character(val) && length(val) == 1) {
          val
        } else if (is.symbol(val)) {
          as.character(val)
        } else {
          stop(paste0(
            "`",
            arg_name,
            "` must be a column name (quoted or unquoted)"
          ))
        }
      }
    )
  }

  fitted_name <- get_column_name(fitted_quo, ".fitted")
  group_name <- get_column_name(group_quo, ".group")

  # Validate that columns exist
  if (!fitted_name %in% names(data)) {
    stop(paste0("Column '", fitted_name, "' not found in data"))
  }
  if (!group_name %in% names(data)) {
    stop(paste0("Column '", group_name, "' not found in data"))
  }

  # Extract and validate treatment variable
  group_var <- data[[group_name]]
  unique_levels <- unique(group_var[!is.na(group_var)])
  
  # Determine treatment level
  if (is.null(treatment_level)) {
    # Default: use last level for factors, max value for numeric
    if (is.factor(group_var)) {
      treatment_level <- levels(group_var)[length(levels(group_var))]
    } else {
      if (length(unique_levels) == 0) {
        treatment_level <- 1  # Default for empty data
      } else {
        treatment_level <- max(unique_levels, na.rm = TRUE)
      }
    }
  }
  
  # Validate treatment level exists (skip for empty data)
  if (length(unique_levels) > 0 && !treatment_level %in% unique_levels) {
    stop(paste0("treatment_level '", treatment_level, "' not found in .group variable"))
  }
  
  # Create binary treatment indicator (1 = treatment, 0 = control)
  treatment_indicator <- as.numeric(group_var == treatment_level)
  
  # Create a tibble with only the needed columns and standardized names
  df <- tibble::tibble(
    x_var = data[[fitted_name]],
    y_var = treatment_indicator
  )

  if (na.rm) {
    df <- df |>
      dplyr::filter(!is.na(x_var) & !is.na(y_var))
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

  # Use dplyr for aggregation
  result <- df |>
    dplyr::group_by(.bin) |>
    dplyr::summarise(
      fitted_mean = mean(x_var, na.rm = TRUE),
      group_mean = mean(y_var, na.rm = TRUE),
      count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.bin)

  # Compute CIs - optimized version
  n_rows <- nrow(result)
  result$lower <- numeric(n_rows)
  result$upper <- numeric(n_rows)

  # Vectorized computation where possible
  n_total <- result$count
  n_events <- round(result$group_mean * n_total)

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
        rate <- result$group_mean[.x]
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
    params$treatment_level <- params$treatment_level %||% NULL
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
    step_size = window_size / 2,
    treatment_level = NULL
  ) {
    # Process treatment level to create binary indicator
    if (!is.null(treatment_level)) {
      unique_levels <- unique(data$y[!is.na(data$y)])
      if (!treatment_level %in% unique_levels) {
        stop(paste0("treatment_level '", treatment_level, "' not found in y variable"))
      }
      data$y <- as.numeric(data$y == treatment_level)
    } else {
      # Default behavior: assume y is already binary or use max value
      if (is.factor(data$y)) {
        # For factors, use the last level as treatment
        treatment_level <- levels(data$y)[length(levels(data$y))]
        data$y <- as.numeric(data$y == treatment_level)
      } else {
        # For numeric, use max value as treatment
        unique_levels <- unique(data$y[!is.na(data$y)])
        treatment_level <- max(unique_levels, na.rm = TRUE)
        data$y <- as.numeric(data$y == treatment_level)
      }
    }
    
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

  # Use dplyr for cleaner aggregation
  bin_summary <- tibble::tibble(
    x = data$x,
    y = data$y,
    bin = bin_assignments
  ) |>
    dplyr::filter(!is.na(bin)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(
      n_events = sum(y),
      n_total = dplyr::n(),
      x = mean(x),
      .groups = "drop"
    ) |>
    dplyr::filter(n_total > 0) |>
    dplyr::mutate(event_rate = n_events / n_total)

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
#'   x should be propensity scores/predicted probabilities, y should be treatment variable.
#' @param data Data frame or tibble; if NULL, uses ggplot default.
#' @param method Character; calibration method - "breaks", "logistic", or "windowed".
#' @param bins Integer >1; number of bins for the "breaks" method.
#' @param smooth Logical; for "logistic" method, use GAM smoothing if available.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param window_size Numeric; size of each window for "windowed" method.
#' @param step_size Numeric; distance between window centers for "windowed" method.
#' @param treatment_level Value indicating which level of y represents treatment.
#'   If NULL (default), uses the last level for factors or max value for numeric.
#' @param show_ribbon Logical; show confidence interval ribbon.
#' @param show_points Logical; show points (only for "breaks" and "windowed" methods).
#' @param position Position adjustment.
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#' @param show.legend Logical; include in legend.
#' @param inherit.aes Logical; inherit aesthetics from ggplot.
#' @param ... Additional parameters passed to geoms.
#' @return A ggplot2 layer or list of layers
#' @examples
#' library(ggplot2)
#' 
#' # Basic calibration plot using nhefs_weights dataset
#' # .fitted contains propensity scores, qsmk is the treatment variable
#' ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
#'   geom_calibration() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
#'
#' # Using different methods
#' ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
#'   geom_calibration(method = "logistic") +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
#'
#' # Specify treatment level explicitly
#' ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
#'   geom_calibration(treatment_level = "1") +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
#'
#' # Windowed method with custom parameters
#' ggplot(nhefs_weights, aes(x = .fitted, y = qsmk)) +
#'   geom_calibration(method = "windowed", window_size = 0.2, step_size = 0.1) +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
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
  treatment_level = NULL,
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
        treatment_level = treatment_level,
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
        treatment_level = treatment_level,
        na.rm = na.rm
      )
    )
    layers <- c(layers, list(point_layer))
  }

  layers
}

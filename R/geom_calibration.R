#' Compute calibration data for binary outcomes
#'
#' `check_calibration()` summarizes predicted probabilities and observed outcomes,
#' computing mean prediction, observed rate, counts, and confidence intervals.
#' Calibration represents the agreement between predicted probabilities and observed outcomes.
#' Supports multiple methods for calibration assessment.
#'
#' @param data A data frame containing the data.
#' @param .fitted Column name of predicted probabilities (numeric between 0 and 1).
#'   Can be unquoted (e.g., `p`) or quoted (e.g., `"p"`).
#' @param .group Column name of treatment/group variable.
#'   Can be unquoted (e.g., `g`) or quoted (e.g., `"g"`).
#' @param treatment_level Value indicating which level of `.group` represents treatment.
#'   If `NULL` (default), uses the last level for factors or max value for numeric.
#' @param method Character; calibration method. One of: "breaks", "logistic", or "windowed".
#' @param bins Integer > 1; number of bins for the "breaks" method.
#' @param binning_method "equal_width" or "quantile" for bin creation (breaks method only).
#' @param smooth Logical; for "logistic" method, use GAM smoothing via the mgcv package.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param window_size Numeric; size of each window for "windowed" method.
#' @param step_size Numeric; distance between window centers for "windowed" method.
#' @param k Integer; the basis dimension for GAM smoothing when method = "logistic" and smooth = `TRUE.` Default is 10.
#' @param na.rm Logical; if `TRUE`, drop `NA` values before summarizing.
#'
#' @return A tibble with columns:
#'   - For "breaks" method:
#'     - `.bin`: integer bin index
#'     - `fitted_mean`: mean predicted probability in bin
#'     - `group_mean`: observed treatment rate in bin
#'     - `count`: number of observations in bin
#'     - `lower`: lower bound of CI for `group_mean`
#'     - `upper`: upper bound of CI for `group_mean`
#'   - For "logistic" and "windowed" methods:
#'     - `fitted_mean`: predicted probability values
#'     - `group_mean`: calibrated outcome rate
#'     - `lower`: lower bound of CI
#'     - `upper`: upper bound of CI
#' @examples
#' # Using the included `nhefs_weights` dataset
#' # `.fitted` contains propensity scores, and `qsmk` is the treatment variable
#' check_calibration(nhefs_weights, .fitted, qsmk)
#'
#' # Logistic method with smoothing
#' check_calibration(nhefs_weights, .fitted, qsmk, method = "logistic")
#'
#' # Windowed method
#' check_calibration(nhefs_weights, .fitted, qsmk, method = "windowed")
#'
#' @importFrom stats prop.test quantile glm binomial predict plogis qnorm
#' @export
check_calibration <- function(
  data,
  .fitted,
  .group,
  treatment_level = NULL,
  method = c("breaks", "logistic", "windowed"),
  bins = 10,
  binning_method = c("equal_width", "quantile"),
  smooth = TRUE,
  conf_level = 0.95,
  window_size = 0.1,
  step_size = window_size / 2,
  k = 10,
  na.rm = FALSE
) {
  method <- rlang::arg_match(method)
  binning_method <- rlang::arg_match(binning_method)
  bins_are_not_correctly_specified <- (method == "breaks" &&
    (!is.numeric(bins) || bins < 2 || !isTRUE(all.equal(bins, round(bins)))))

  if (bins_are_not_correctly_specified) {
    abort("{.code bins} must be an integer > 1.")
  }

  fitted_quo <- rlang::enquo(.fitted)
  group_quo <- rlang::enquo(.group)

  fitted_name <- get_column_name(fitted_quo, ".fitted")
  group_name <- get_column_name(group_quo, ".group")

  group_var <- data[[group_name]]
  
  check_columns(data, fitted_name, group_name, treatment_level)

  treatment_indicator <- check_treatment_level(group_var, treatment_level)

  df <- tibble::tibble(
    x_var = data[[fitted_name]],
    y_var = treatment_indicator
  )

  if (isTRUE(na.rm)) {
    df <- df |>
      dplyr::filter(!is.na(x_var) & !is.na(y_var))
  }

  if (nrow(df) == 0) {
    return(empty_calibration(method))
  }

  result <- if (method == "breaks") {
    compute_calibration_breaks_imp(df, bins, binning_method, conf_level)
  } else if (method == "logistic") {
    compute_calibration_logistic_imp(df, smooth, conf_level, k = k)
  } else if (method == "windowed") {
    compute_calibration_windowed_imp(
      df,
      window_size,
      step_size,
      conf_level
    )
  }

  result
}

empty_calibration <- function(method = "breaks") {
  if (method == "breaks") {
    tibble::tibble(
      .bin = integer(0),
      fitted_mean = numeric(0),
      group_mean = numeric(0),
      count = integer(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  } else {
    tibble::tibble(
      fitted_mean = numeric(0),
      group_mean = numeric(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  }
}

check_treatment_level <- function(group_var, treatment_level) {
  unique_levels <- unique(group_var[!is.na(group_var)])
  # Default: use last level for factors, max value for numeric
  if (is.null(treatment_level)) {
    if (is.factor(group_var)) {
      treatment_level <- levels(group_var)[length(levels(group_var))]
    } else {
      if (length(unique_levels) == 0) {
        # Default for empty data
        treatment_level <- 1
      } else {
        treatment_level <- max(unique_levels, na.rm = TRUE)
      }
    }
  }

  # Validate treatment level exists (skip for empty data)
  if (length(unique_levels) > 0 && !treatment_level %in% unique_levels) {
    abort(
      "{.code treatment_level} {.code {treatment_level}} not found in {.code .group} variable"
    )
  }
  
  # Create binary treatment indicator (1 = treatment, 0 = control)
  # Handle both factor and non-factor variables
  if (is.factor(group_var)) {
    # For factors, ensure we're comparing as character to handle numeric-looking levels
    treatment_indicator <- as.numeric(
      as.character(group_var) == as.character(treatment_level)
    )
  } else {
    treatment_indicator <- as.numeric(group_var == treatment_level)
  }

  treatment_indicator
}

check_columns <- function(data, fitted_name, group_name, treatment_level) {
  if (is.null(treatment_level)) {
    if (!fitted_name %in% names(data)) {
      abort("Column {.code {fitted_name}} not found in data")
    }

    if (!group_name %in% names(data)) {
      abort("Column {.code {group_name}} not found in data")
    }
  }
}

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
          abort("{.code arg_name} must be a column name (quoted or unquoted)")
        }
      )

      # Handle different types of evaluated values
      if (is.character(val) && length(val) == 1) {
        val
      } else if (is.symbol(val)) {
        as.character(val)
      } else {
        abort("{.code arg_name} must be a column name (quoted or unquoted)")
      }
    }
  )
}

# Internal helper function for breaks method
compute_calibration_breaks_imp <- function(
  df,
  bins,
  binning_method,
  conf_level
) {
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

  result <- df |>
    dplyr::mutate(.bin = as.integer(cut(
    xs,
    breaks = brks,
    include.lowest = TRUE,
    labels = FALSE
  ))) |>
    dplyr::group_by(.bin) |>
    dplyr::summarise(
      fitted_mean = mean(x_var, na.rm = TRUE),
      group_mean = mean(y_var, na.rm = TRUE),
      count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.bin)

  n_total <- result$count
  n_events <- round(result$group_mean * n_total)

  # Handle edge cases up front:
  # - total count must be positive
  # - number of events must be non-negative
  # - events cannot exceed total count 
  # - events must be greater than 0 (no empty bins)
  # - events must be less than total (no full bins)
  valid_mask <- n_total > 0 &
    n_events >= 0 &
    n_events <= n_total &
    n_events > 0 &
    n_events < n_total

  # Check for small cell sizes that might cause warnings
  small_cells <- n_total < 10
  extreme_props <- (n_events <= 2) | (n_events >= n_total - 2)
  warning_mask <- small_cells | extreme_props

  if (any(warning_mask, na.rm = TRUE)) {
    warning_bins <- result$.bin[warning_mask]
    warning_counts <- n_total[warning_mask]
    warning(
      "Small sample sizes or extreme proportions detected in bins ",
      paste(warning_bins, collapse = ", "),
      " (n = ",
      paste(warning_counts, collapse = ", "),
      "). ",
      "Confidence intervals may be unreliable. ",
      "Consider using fewer bins or a different calibration method.",
      call. = FALSE
    )
  }

  n_rows <- nrow(result)
  result$lower <- numeric(n_rows)
  result$upper <- numeric(n_rows)

  valid_indices <- which(valid_mask)
  if (length(valid_indices) > 0) {
    ci_results <- purrr::map(
      valid_indices,
      ~ {
        tryCatch(
          {
            suppressWarnings({
              ci <- stats::prop.test(
                x = n_events[.x],
                n = n_total[.x],
                conf.level = conf_level
              )$conf.int
            })
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

  result
}

compute_calibration_logistic_imp <- function(
  df,
  smooth,
  conf_level,
  k = 10
) {
  # Fit model
  if (smooth) {
    rlang::check_installed("mgcv", "for GAM smoothing")
    model <- mgcv::gam(y_var ~ s(x_var, k = k), data = df, family = binomial())
  } else {
    model <- glm(y_var ~ x_var, data = df, family = binomial())
  }

  # Create prediction sequence
  pred_seq <- seq(min(df$x_var), max(df$x_var), length.out = 100)
  new_data <- data.frame(x_var = pred_seq)

  # Get predictions with confidence intervals
  preds <- predict(model, new_data, se.fit = TRUE)
  pred_probs <- plogis(preds$fit)

  # Calculate confidence intervals
  alpha <- 1 - conf_level
  z_score <- qnorm(1 - alpha / 2)
  lower <- plogis(preds$fit - z_score * preds$se.fit)
  upper <- plogis(preds$fit + z_score * preds$se.fit)

  tibble::tibble(
    fitted_mean = pred_seq,
    group_mean = pred_probs,
    lower = lower,
    upper = upper
  )
}

compute_calibration_windowed_imp <- function(
  df,
  window_size,
  step_size,
  conf_level
) {
  steps <- seq(0, 1, by = step_size)
  n_steps <- length(steps)
  alpha <- 1 - conf_level
  z_score <- stats::qnorm(1 - alpha / 2)
  half_window <- window_size / 2

  window_results <- purrr::map(
    steps,
    calculate_window_statistics,
    data_x = df$x_var,
    data_y = df$y_var,
    half_window = half_window,
    z_score = z_score
  )

  # Filter to valid windows only
  valid_results <- purrr::keep(window_results, ~ .x$valid)

  # Check for small cell sizes that might cause unreliable estimates
  if (length(valid_results) > 0) {
    sample_sizes <- purrr::map_dbl(valid_results, ~ .x$n_total)
    event_counts <- purrr::map_dbl(valid_results, ~ .x$n_events)
    window_centers <- purrr::map_dbl(valid_results, ~ .x$fitted_mean)

    small_cells <- sample_sizes < 10
    extreme_props <- (event_counts <= 2) | (event_counts >= sample_sizes - 2)
    warning_mask <- small_cells | extreme_props

    if (any(warning_mask, na.rm = TRUE)) {
      warning_windows <- window_centers[warning_mask]
      warning_counts <- sample_sizes[warning_mask]
      warning(
        "Small sample sizes or extreme proportions detected in windows centered at ",
        paste(round(warning_windows, 3), collapse = ", "),
        " (n = ",
        paste(warning_counts, collapse = ", "),
        "). ",
        "Confidence intervals may be unreliable. ",
        "Consider using a larger window size or a different calibration method.",
        call. = FALSE
      )
    }
  }

  # Return only valid windows
  if (length(valid_results) > 0) {
    tibble::tibble(
      fitted_mean = purrr::map_dbl(valid_results, ~ .x$fitted_mean),
      group_mean = purrr::map_dbl(valid_results, ~ .x$group_mean),
      lower = purrr::map_dbl(valid_results, ~ .x$lower),
      upper = purrr::map_dbl(valid_results, ~ .x$upper)
    )
  } else {
    # Return empty data frame with correct structure
    tibble::tibble(
      fitted_mean = numeric(0),
      group_mean = numeric(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  }
}

calculate_window_statistics <- function(.x, data_x, data_y, half_window, z_score) {
  {
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
              suppressWarnings({
                prop_test <- stats::prop.test(
                  n_events,
                  n_total,
                  conf.level = conf_level
                )
              })
              list(
                fitted_mean = .x,
                group_mean = event_rate,
                lower = prop_test$conf.int[1],
                upper = prop_test$conf.int[2],
                valid = TRUE,
                n_total = n_total,
                n_events = n_events
              )
            },
            error = function(e) {
              # Fallback to normal approximation
              se <- sqrt(event_rate * (1 - event_rate) / n_total)
              list(
                fitted_mean = .x,
                group_mean = event_rate,
                lower = max(0, event_rate - z_score * se),
                upper = min(1, event_rate + z_score * se),
                valid = TRUE,
                n_total = n_total,
                n_events = n_events
              )
            }
          )
        } else {
          # For edge cases, use normal approximation
          se <- sqrt(event_rate * (1 - event_rate) / n_total)
          list(
            fitted_mean = .x,
            group_mean = event_rate,
            lower = max(0, event_rate - z_score * se),
            upper = min(1, event_rate + z_score * se),
            valid = TRUE,
            n_total = n_total,
            n_events = n_events
          )
        }
      } else {
        # Invalid window
        list(valid = FALSE)
      }
    }
}

# Stat for computing calibration statistics
StatCalibration <- ggplot2::ggproto(
  "StatCalibration",
  ggplot2::Stat,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(alpha = 0.3),
  dropped_aes = c("y"),
  setup_params = function(data, params) {
    # Set default parameters
    params$method <- params$method %||% "breaks"
    params$bins <- params$bins %||% 10
    params$smooth <- params$smooth %||% TRUE
    params$conf_level <- params$conf_level %||% 0.95
    params$window_size <- params$window_size %||% 0.1
    params$step_size <- params$step_size %||% (params$window_size / 2)
    params$treatment_level <- params$treatment_level %||% NULL
    params$k <- params$k %||% 10
    params
  },
  compute_panel = function(
    data,
    scales,
    method = "breaks",
    bins = 10,
    smooth = TRUE,
    conf_level = 0.95,
    window_size = 0.1,
    step_size = window_size / 2,
    treatment_level = NULL,
    k = 10
  ) {

    calibration_result <- check_calibration(
      data = data,
      .fitted = x,
      .group = y,
      treatment_level = treatment_level,
      method = method,
      bins = bins,
      smooth = smooth,
      conf_level = conf_level,
      window_size = window_size,
      step_size = step_size,
      k = k
    )

    result <- data.frame(
      x = calibration_result$fitted_mean,
      y = calibration_result$group_mean,
      ymin = calibration_result$lower,
      ymax = calibration_result$upper
    )

    # Preserve required ggplot2 columns
    result$PANEL <- data$PANEL[1]
    result$group <- data$group[1]

    result
  }
)

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
#'   For factors with numeric-looking levels (e.g., "0", "1"), this parameter
#'   works as expected. For factors with non-numeric levels (e.g., "Control",
#'   "Treatment"), the function will attempt to use the higher level as treatment
#'   but may not always correctly identify the intended level. In such cases,
#'   consider converting the factor to numeric before plotting.
#' @param k Integer; the basis dimension for GAM smoothing when method = "logistic" and smooth = TRUE. Default is 10.
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
  k = 10,
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
        k = k,
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
      treatment_level = treatment_level,
      k = k,
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
        k = k,
        na.rm = na.rm
      )
    )
    layers <- c(layers, list(point_layer))
  }

  layers
}

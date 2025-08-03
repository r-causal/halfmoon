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
#' @inheritParams treatment_param
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
#'     - `predicted_rate`: mean predicted probability in bin
#'     - `observed_rate`: observed treatment rate in bin
#'     - `count`: number of observations in bin
#'     - `lower`: lower bound of CI for `observed_rate`
#'     - `upper`: upper bound of CI for `observed_rate`
#'   - For "logistic" and "windowed" methods:
#'     - `predicted_rate`: predicted probability values
#'     - `observed_rate`: calibrated outcome rate
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
    abort(
      "{.code bins} must be an integer > 1.",
      error_class = "halfmoon_arg_error"
    )
  }

  fitted_quo <- rlang::enquo(.fitted)
  group_quo <- rlang::enquo(.group)

  fitted_name <- get_column_name(fitted_quo, ".fitted")
  group_name <- get_column_name(group_quo, ".group")

  group_var <- data[[group_name]]

  check_columns(data, fitted_name, group_name, treatment_level, call = rlang::current_env())

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
    compute_calibration_breaks_imp(df, bins, binning_method, conf_level, call = rlang::current_env())
  } else if (method == "logistic") {
    compute_calibration_logistic_imp(df, smooth, conf_level, k = k, call = rlang::current_env())
  } else if (method == "windowed") {
    compute_calibration_windowed_imp(
      df,
      window_size,
      step_size,
      conf_level,
      call = rlang::current_env()
    )
  }

  result
}

empty_calibration <- function(method = "breaks") {
  if (method == "breaks") {
    tibble::tibble(
      .bin = integer(0),
      predicted_rate = numeric(0),
      observed_rate = numeric(0),
      count = integer(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  } else {
    tibble::tibble(
      predicted_rate = numeric(0),
      observed_rate = numeric(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  }
}

check_treatment_level <- function(group_var, treatment_level) {
  # Validate treatment level exists if provided
  if (!is.null(treatment_level)) {
    unique_levels <- unique(group_var[!is.na(group_var)])
    if (length(unique_levels) > 0 && !treatment_level %in% unique_levels) {
      abort(
        "{.code treatment_level} {.code {treatment_level}} not found in {.code .group} variable",
        error_class = "halfmoon_reference_error"
      )
    }
  }

  # Use the helper function to create treatment indicator
  create_treatment_indicator(group_var, treatment_level)
}

check_columns <- function(data, fitted_name, group_name, treatment_level, call = rlang::caller_env()) {
  if (is.null(treatment_level)) {
    if (!fitted_name %in% names(data)) {
      abort(
        "Column {.code {fitted_name}} not found in data",
        error_class = "halfmoon_column_error",
        call = call
      )
    }

    if (!group_name %in% names(data)) {
      abort(
        "Column {.code {group_name}} not found in data",
        error_class = "halfmoon_column_error",
        call = call
      )
    }
  }
}


# Internal helper function for breaks method
compute_calibration_breaks_imp <- function(
  df,
  bins,
  binning_method,
  conf_level,
  call = rlang::caller_env()
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
    dplyr::mutate(
      .bin = as.integer(cut(
        xs,
        breaks = brks,
        include.lowest = TRUE,
        labels = FALSE
      ))
    ) |>
    dplyr::group_by(.bin) |>
    dplyr::summarise(
      predicted_rate = mean(x_var, na.rm = TRUE),
      observed_rate = mean(y_var, na.rm = TRUE),
      count = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.bin)

  n_total <- result$count
  n_events <- round(result$observed_rate * n_total)

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
    bins_list <- paste(warning_bins, collapse = ", ")
    counts_list <- paste(warning_counts, collapse = ", ")
    warn(
      "Small sample sizes or extreme proportions detected in bins {bins_list} (n = {counts_list}). Confidence intervals may be unreliable. Consider using fewer bins or a different calibration method.",
      warning_class = "halfmoon_data_warning",
      call = call
    )
  }

  n_rows <- nrow(result)
  result$lower <- numeric(n_rows)
  result$upper <- numeric(n_rows)

  valid_indices <- which(valid_mask)
  if (length(valid_indices) > 0) {
    ci_results <- purrr::map(
      valid_indices,
      \(x) calculate_prop_ci(n_events[x], n_total[x], conf_level)
    )

    # Extract results
    result$lower[valid_indices] <- purrr::map_dbl(ci_results, \(x) x$lower)
    result$upper[valid_indices] <- purrr::map_dbl(ci_results, \(x) x$upper)
  }

  # For edge cases, use normal approximation with purrr
  edge_cases <- which(!valid_mask & n_total > 0)
  if (length(edge_cases) > 0) {
    edge_results <- purrr::map(
      edge_cases,
      \(x) calculate_normal_ci(result$observed_rate[x], n_total[x], conf_level)
    )

    result$lower[edge_cases] <- purrr::map_dbl(edge_results, \(x) x$lower)
    result$upper[edge_cases] <- purrr::map_dbl(edge_results, \(x) x$upper)
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
  k = 10,
  call = rlang::caller_env()
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
  z_score <- get_z_score(conf_level)
  lower <- plogis(preds$fit - z_score * preds$se.fit)
  upper <- plogis(preds$fit + z_score * preds$se.fit)

  tibble::tibble(
    predicted_rate = pred_seq,
    observed_rate = pred_probs,
    lower = lower,
    upper = upper
  )
}

compute_calibration_windowed_imp <- function(
  df,
  window_size,
  step_size,
  conf_level,
  call = rlang::caller_env()
) {
  steps <- seq(0, 1, by = step_size)
  n_steps <- length(steps)
  z_score <- get_z_score(conf_level)
  half_window <- window_size / 2

  window_results <- purrr::map(
    steps,
    calculate_window_statistics,
    data_x = df$x_var,
    data_y = df$y_var,
    half_window = half_window,
    z_score = z_score,
    conf_level = conf_level
  )

  # Filter to valid windows only
  valid_results <- purrr::keep(window_results, \(x) x$valid)

  # Check for small cell sizes that might cause unreliable estimates
  if (length(valid_results) > 0) {
    sample_sizes <- purrr::map_dbl(valid_results, \(x) x$n_total)
    event_counts <- purrr::map_dbl(valid_results, \(x) x$n_events)
    window_centers <- purrr::map_dbl(valid_results, \(x) x$predicted_rate)

    small_cells <- sample_sizes < 10
    extreme_props <- (event_counts <= 2) | (event_counts >= sample_sizes - 2)
    warning_mask <- small_cells | extreme_props

    if (any(warning_mask, na.rm = TRUE)) {
      warning_windows <- window_centers[warning_mask]
      warning_counts <- sample_sizes[warning_mask]
      windows_list <- paste(round(warning_windows, 3), collapse = ", ")
      counts_list <- paste(warning_counts, collapse = ", ")
      warn(
        "Small sample sizes or extreme proportions detected in windows centered at {windows_list} (n = {counts_list}). Confidence intervals may be unreliable. Consider using a larger window size or a different calibration method.",
        warning_class = "halfmoon_data_warning",
        call = call
      )
    }
  }

  # Return only valid windows
  if (length(valid_results) > 0) {
    tibble::tibble(
      predicted_rate = purrr::map_dbl(valid_results, \(x) x$predicted_rate),
      observed_rate = purrr::map_dbl(valid_results, \(x) x$observed_rate),
      lower = purrr::map_dbl(valid_results, \(x) x$lower),
      upper = purrr::map_dbl(valid_results, \(x) x$upper)
    )
  } else {
    # Return empty data frame with correct structure
    tibble::tibble(
      predicted_rate = numeric(0),
      observed_rate = numeric(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  }
}

calculate_window_statistics <- function(
  .x,
  data_x,
  data_y,
  half_window,
  z_score,
  conf_level
) {
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
        ci <- calculate_prop_ci(n_events, n_total, conf_level)
        list(
          predicted_rate = .x,
          observed_rate = event_rate,
          lower = ci$lower,
          upper = ci$upper,
          valid = TRUE,
          n_total = n_total,
          n_events = n_events
        )
      } else {
        # For edge cases, use normal approximation
        ci <- calculate_normal_ci(event_rate, n_total, conf_level)
        list(
          predicted_rate = .x,
          observed_rate = event_rate,
          lower = ci$lower,
          upper = ci$upper,
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
  required_aes = c("estimate", "truth"),
  default_aes = ggplot2::aes(
    x = ggplot2::after_stat(predicted_rate),
    y = ggplot2::after_stat(observed_rate),
    ymin = ggplot2::after_stat(lower),
    ymax = ggplot2::after_stat(upper),
    alpha = 0.3
  ),
  dropped_aes = c("truth"),
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
    k = 10,
    binning_method = "equal_width",
    na.rm = FALSE
  ) {
    # If we have multiple groups, handle smart group merging
    if ("group" %in% names(data) && length(unique(data$group)) > 1) {
      groups <- split(data, data$group)

      # Create signatures for each group based on aesthetic values
      # We want to merge groups that differ only by truth factor levels
      aes_cols <- setdiff(
        names(data),
        c("estimate", "truth", "weight", "PANEL", "group", "x", "y")
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
        compute_calibration_for_group(
          combined_data,
          treatment_level,
          method,
          bins,
          binning_method,
          smooth,
          conf_level,
          window_size,
          step_size,
          k,
          na.rm,
          group_id,
          call = rlang::current_env()
        )
      })

      results
    } else {
      # Single group or no groups
      compute_calibration_for_group(
        data,
        treatment_level,
        method,
        bins,
        binning_method,
        smooth,
        conf_level,
        window_size,
        step_size,
        k,
        na.rm,
        data$group[1],
        call = rlang::current_env()
      )
    }
  }
)

# Helper function to compute calibration for a single group
compute_calibration_for_group <- function(
  data,
  treatment_level,
  method,
  bins,
  binning_method,
  smooth,
  conf_level,
  window_size,
  step_size,
  k,
  na.rm,
  group_id,
  call = rlang::caller_env()
) {
  # Convert to binary using helper function
  truth <- data$truth
  treatment_indicator <- create_treatment_indicator(truth, treatment_level)

  # Create data frame for calibration computation
  df <- tibble::tibble(
    x_var = data$estimate,
    y_var = treatment_indicator
  )

  if (isTRUE(na.rm)) {
    df <- df |>
      dplyr::filter(!is.na(x_var) & !is.na(y_var))
  }

  if (nrow(df) == 0) {
    return(data.frame(
      predicted_rate = numeric(0),
      observed_rate = numeric(0),
      lower = numeric(0),
      upper = numeric(0),
      PANEL = data$PANEL[1],
      group = group_id
    ))
  }

  # Compute calibration based on method
  calibration_result <- if (method == "breaks") {
    compute_calibration_breaks_imp(df, bins, binning_method, conf_level, call = call)
  } else if (method == "logistic") {
    compute_calibration_logistic_imp(df, smooth, conf_level, k = k, call = call)
  } else if (method == "windowed") {
    compute_calibration_windowed_imp(
      df,
      window_size,
      step_size,
      conf_level,
      call = call
    )
  } else {
    # Invalid method - warn and return empty result
    warn(
      "Invalid calibration method: {method}",
      warning_class = "halfmoon_method_warning",
      call = call
    )
    tibble::tibble(
      predicted_rate = numeric(0),
      observed_rate = numeric(0),
      lower = numeric(0),
      upper = numeric(0)
    )
  }

  # Return with after_stat names
  result <- data.frame(
    predicted_rate = calibration_result$predicted_rate,
    observed_rate = calibration_result$observed_rate,
    lower = calibration_result$lower,
    upper = calibration_result$upper
  )

  # Preserve required ggplot2 columns (only if we have results)
  if (nrow(result) > 0) {
    result$PANEL <- data$PANEL[1]
    result$group <- group_id
  }

  result
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
#' `geom_calibration()` creates calibration plots to assess the agreement between predicted
#' probabilities and observed binary outcomes. It supports three methods:
#' binning ("breaks"), logistic regression ("logistic"), and windowed ("windowed"), all computed with [`check_calibration()`].
#'
#' @details
#' This geom provides a ggplot2 layer for creating calibration plots with confidence
#' intervals. The geom automatically computes calibration statistics using the
#' specified method and renders appropriate geometric elements (points, lines, ribbons)
#' to visualize the relationship between predicted and observed rates.
#'
#' The three methods offer different approaches to calibration assessment:
#' \itemize{
#'   \item **"breaks"**: Discrete binning approach, useful for understanding calibration
#'     across prediction ranges with sufficient sample sizes
#'   \item **"logistic"**: Regression-based approach that can include smoothing for
#'     continuous calibration curves
#'   \item **"windowed"**: Sliding window approach providing smooth curves without
#'     requiring additional packages
#' }
#'
#' @param mapping Aesthetic mapping (must supply `estimate` and `truth` if not inherited).
#'   `estimate` should be propensity scores/predicted probabilities, `truth` should be treatment variable.
#' @param data Data frame or tibble; if NULL, uses ggplot default.
#' @param method Character; calibration method - "breaks", "logistic", or "windowed".
#' @param bins Integer >1; number of bins for the "breaks" method.
#' @param binning_method "equal_width" or "quantile" for bin creation (breaks method only).
#' @param smooth Logical; for "logistic" method, use GAM smoothing if available.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param window_size Numeric; size of each window for "windowed" method.
#' @param step_size Numeric; distance between window centers for "windowed" method.
#' @inheritParams treatment_param
#' @param k Integer; the basis dimension for GAM smoothing when method = "logistic" and smooth = TRUE. Default is 10.
#' @param show_ribbon Logical; show confidence interval ribbon.
#' @param show_points Logical; show points (only for "breaks" and "windowed" methods).
#' @inheritParams ggplot2_params
#' @return A ggplot2 layer or list of layers
#' @family ggplot2 functions
#' @seealso [check_calibration()] for computing calibration statistics
#' @examples
#' library(ggplot2)
#'
#' # Basic calibration plot using nhefs_weights dataset
#' # .fitted contains propensity scores, qsmk is the treatment variable
#' ggplot(nhefs_weights, aes(estimate = .fitted, truth = qsmk)) +
#'   geom_calibration() +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
#'
#' # Using different methods
#' ggplot(nhefs_weights, aes(estimate = .fitted, truth = qsmk)) +
#'   geom_calibration(method = "logistic") +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
#'
#' # Specify treatment level explicitly
#' ggplot(nhefs_weights, aes(estimate = .fitted, truth = qsmk)) +
#'   geom_calibration(treatment_level = "1") +
#'   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#'   labs(x = "Propensity Score", y = "Observed Treatment Rate")
#'
#' # Windowed method with custom parameters
#' ggplot(nhefs_weights, aes(estimate = .fitted, truth = qsmk)) +
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
  binning_method = "equal_width",
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
        binning_method = binning_method,
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
        binning_method = binning_method,
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

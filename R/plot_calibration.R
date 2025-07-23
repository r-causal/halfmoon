#' Create calibration plot
#'
#' Create a calibration plot to assess the agreement between predicted
#' probabilities and observed treatment rates. This function wraps
#' `geom_calibration()` to provide a convenient plotting interface
#' in the style of the probably package.
#'
#' @param .data A data frame containing the variables.
#' @param .fitted Column name of predicted probabilities (propensity scores).
#'   Can be unquoted (e.g., `.fitted`) or quoted (e.g., `".fitted"`).
#' @param .group Column name of treatment/group variable.
#'   Can be unquoted (e.g., `qsmk`) or quoted (e.g., `"qsmk"`).
#' @param treatment_level Value indicating which level of `.group` represents treatment.
#'   If NULL (default), uses the last level for factors or max value for numeric.
#' @param method Character; calibration method - "breaks", "logistic", or "windowed".
#' @param bins Integer >1; number of bins for the "breaks" method.
#' @param smooth Logical; for "logistic" method, use GAM smoothing if available.
#' @param conf_level Numeric in (0,1); confidence level for CIs (default = 0.95).
#' @param window_size Numeric; size of each window for "windowed" method.
#' @param step_size Numeric; distance between window centers for "windowed" method.
#' @param k Integer; the basis dimension for GAM smoothing when method = "logistic" and smooth = TRUE. Default is 10.
#' @param include_rug Logical; add rug plot showing distribution of predicted probabilities.
#' @param include_ribbon Logical; show confidence interval ribbon.
#' @param include_points Logical; show points (only for "breaks" and "windowed" methods).
#' @param na.rm Logical; if TRUE, drop NA values before computation.
#' @param ... Additional parameters passed to `geom_calibration()`.
#' @return A ggplot2 object.
#' @examples
#' library(ggplot2)
#'
#' # Basic calibration plot
#' plot_calibration(nhefs_weights, .fitted, qsmk)
#'
#' # With rug plot
#' plot_calibration(nhefs_weights, .fitted, qsmk, include_rug = TRUE)
#'
#' # Different methods
#' plot_calibration(nhefs_weights, .fitted, qsmk, method = "logistic")
#' plot_calibration(nhefs_weights, .fitted, qsmk, method = "windowed")
#'
#' # Specify treatment level explicitly
#' plot_calibration(nhefs_weights, .fitted, qsmk, treatment_level = "1")
#' @export
plot_calibration <- function(
  .data,
  .fitted,
  .group,
  treatment_level = NULL,
  method = "breaks",
  bins = 10,
  smooth = TRUE,
  conf_level = 0.95,
  window_size = 0.1,
  step_size = window_size / 2,
  k = 10,
  include_rug = FALSE,
  include_ribbon = TRUE,
  include_points = TRUE,
  na.rm = FALSE,
  ...
) {
  # Handle both quoted and unquoted column names using the same logic as check_calibration
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

  # Run check_calibration once to generate appropriate warnings
  tryCatch(
    {
      check_calibration(
        .data,
        !!fitted_quo,
        !!group_quo,
        treatment_level = treatment_level,
        method = method,
        bins = bins,
        smooth = smooth,
        conf_level = conf_level,
        window_size = window_size,
        step_size = step_size,
        k = k,
        na.rm = na.rm
      )
    },
    error = function(e) {
      # If there's an error, it will be caught later by geom_calibration
      NULL
    }
  )

  # Convert factor to numeric for proper y-axis scale
  # This ensures the y-axis shows calibration rates (0-1) not factor labels
  if (is.factor(.data[[group_name]])) {
    # Determine which level is treatment
    if (!is.null(treatment_level)) {
      # User specified treatment level
      .data$.y_numeric <- as.numeric(.data[[group_name]] == treatment_level)
    } else {
      # Use default logic - last level for factors
      levels <- levels(.data[[group_name]])
      if (all(levels %in% c("0", "1"))) {
        # For 0/1 factors, use "1" as treatment
        .data$.y_numeric <- as.numeric(.data[[group_name]] == "1")
      } else {
        # Use last level as treatment
        .data$.y_numeric <- as.numeric(
          .data[[group_name]] == levels[length(levels)]
        )
      }
    }
    y_aes <- ggplot2::aes(y = .data$.y_numeric)
  } else {
    # Not a factor, use as-is
    y_aes <- ggplot2::aes(y = .data[[group_name]])
  }

  # Create the base plot
  p <- ggplot2::ggplot(
    .data,
    ggplot2::aes(x = .data[[fitted_name]])
  ) +
    suppressWarnings({
      geom_calibration(
        y_aes,
        method = method,
        bins = bins,
        smooth = smooth,
        conf_level = conf_level,
        window_size = window_size,
        step_size = step_size,
        treatment_level = treatment_level,
        k = k,
        show_ribbon = include_ribbon,
        show_points = include_points,
        na.rm = na.rm,
        ...
      )
    }) +
    # Add perfect calibration line
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "gray50",
      alpha = 0.8
    ) +
    # Add labels
    ggplot2::labs(
      x = "predicted probability",
      y = "observed rate"
    ) +
    # Set x limits
    ggplot2::xlim(0, 1) +
    # Set y limits using coord_cartesian to avoid scale issues
    ggplot2::coord_cartesian(ylim = c(0, 1))

  # Add rug if requested
  if (include_rug) {
    if (is.factor(.data[[group_name]])) {
      # Use the numeric version for consistency
      p <- p +
        ggplot2::geom_rug(
          ggplot2::aes(color = factor(.data$.y_numeric)),
          alpha = 0.5,
          sides = "b"
        ) +
        ggplot2::scale_color_discrete(
          name = group_name,
          labels = c("Control", "Treatment")
        )
    } else {
      p <- p +
        ggplot2::geom_rug(
          ggplot2::aes(color = factor(.data[[group_name]])),
          alpha = 0.5,
          sides = "b"
        ) +
        ggplot2::scale_color_discrete(name = group_name)
    }
  }

  p
}

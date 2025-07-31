#' Create calibration plot
#'
#' Create a calibration plot to assess the agreement between predicted
#' probabilities and observed treatment rates. This function wraps
#' `geom_calibration()`.
#'
#' @details
#' Calibration plots visualize how well predicted probabilities match observed
#' outcome rates. Since outcomes are binary (0/1), the "observed rate" represents
#' the proportion of units with outcome = 1 within each prediction group. For
#' example, among all units with predicted probability around 0.3, we expect
#' approximately 30% to actually have the outcome. Perfect calibration occurs
#' when predicted probabilities equal observed rates (points fall on the 45-degree
#' line).
#'
#' The plot supports three calibration assessment methods:
#' \itemize{
#'   \item **"breaks"**: Bins predictions into groups and compares mean prediction
#'     vs observed rate within each bin
#'   \item **"logistic"**: Fits a logistic regression of outcomes on predictions;
#'     perfect calibration yields slope=1, intercept=0
#'   \item **"windowed"**: Uses sliding windows across the prediction range for
#'     smooth calibration curves
#' }
#'
#' The function supports two approaches:
#' - For regression models (lm/glm): Extracts fitted values and observed outcomes automatically
#' - For data frames: Uses specified columns for fitted values and treatment group
#'
#' @param x Either a fitted model object (lm or glm) or a data frame
#' @param ... Additional arguments passed to methods
#' @return A ggplot2 object.
#' @examples
#' library(ggplot2)
#'
#' # Method 1: Using data frame
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
#'
#' # Method 2: Using model objects
#' # Fit a propensity score model
#' ps_model <- glm(qsmk ~ age + sex + race + education,
#'                 data = nhefs_weights,
#'                 family = binomial())
#'
#' # Plot calibration from the model
#' plot_calibration(ps_model)
#'
#' @seealso
#' - [`geom_calibration()`] for the underlying geom
#' - [`check_calibration()`] for numerical calibration metrics
#' - [`plot_stratified_residuals()`] for residual diagnostic plots
#' - [`plot_roc_curve()`] for ROC curves
#' - [`plot_qq()`] for QQ plots
#'
#' @export
plot_calibration <- function(x, ...) {
  UseMethod("plot_calibration")
}

#' @rdname plot_calibration
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
#' @export
plot_calibration.data.frame <- function(
  x,
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

  fitted_name <- get_column_name(fitted_quo, ".fitted")
  group_name <- get_column_name(group_quo, ".group")

  # Create the base plot with new aesthetics
  p <- ggplot2::ggplot(
    x,
    ggplot2::aes(estimate = .data[[fitted_name]], truth = .data[[group_name]])
  ) +
    geom_calibration(
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
    ) +
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
    p <- p +
      ggplot2::geom_rug(
        ggplot2::aes(color = .data[[group_name]]),
        alpha = 0.5,
        sides = "b"
      )

    # Add appropriate color scale based on variable type
    if (is.factor(x[[group_name]])) {
      p <- p + ggplot2::scale_color_discrete(name = group_name)
    } else {
      p <- p + ggplot2::scale_color_continuous(name = group_name)
    }
  }

  p
}

#' @rdname plot_calibration
#' @export
plot_calibration.glm <- function(
  x,
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
  # Get the model frame to access the original data
  model_frame <- stats::model.frame(x)

  # Extract fitted values
  .fitted <- stats::fitted(x)

  # For GLM/LM models, the response is the first column of the model frame
  .group <- model_frame[[1]]

  # Create a data frame for plotting
  plot_data <- data.frame(
    .fitted = .fitted,
    .group = .group
  )

  # Call the data frame method
  plot_calibration.data.frame(
    plot_data,
    .fitted = .fitted,
    .group = .group,
    treatment_level = treatment_level,
    method = method,
    bins = bins,
    smooth = smooth,
    conf_level = conf_level,
    window_size = window_size,
    step_size = step_size,
    k = k,
    include_rug = include_rug,
    include_ribbon = include_ribbon,
    include_points = include_points,
    na.rm = na.rm,
    ...
  )
}

#' @rdname plot_calibration
#' @export
plot_calibration.lm <- plot_calibration.glm

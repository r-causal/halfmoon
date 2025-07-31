#' Create stratified residual diagnostic plots
#'
#' Create diagnostic plots to assess differences between treatment group after adjustment.
#' This function plots residuals
#' from an outcome model against propensity scores (or fitted values),
#' stratified by treatment group, to reveal model mis-specification.
#'
#' @details
#' This diagnostic plot was originally suggested by Rosenbaum and Rubin (1983)
#' and revisited by D'Agostino McGowan, D'Agostino, and D'Agostino (2023).
#' The key idea is that plotting residuals against propensity scores
#' or fitted values by treatment group can reveal non-linear relationships or
#' heterogeneous treatment effects that might be obscured in standard
#' residuals-vs-fitted plots.
#'
#' The function supports two approaches:
#' - For regression models (lm/glm): Extracts residuals and fitted values automatically
#' - For data frames: Uses specified columns for residuals, treatment, and x-axis values
#'
#' @param x Either a fitted model object (lm or glm) or a data frame
#' @param ... Additional arguments passed to methods
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Simulate data with treatment effect heterogeneity
#' set.seed(8)
#' n <- 1000
#' x <- rnorm(n)
#' ps <- plogis(x)  # True propensity score
#' treatment <- rbinom(n, 1, ps)
#' y1 <- 0.5 * x + rnorm(n)
#' y0 <- -0.5 * x + rnorm(n)
#' y <- treatment * y1 + (1 - treatment) * y0
#'
#' # Method 1: Using model objects
#' # Fit misspecified model (missing interaction)
#' model_wrong <- lm(y ~ treatment + x)
#'
#' # Plot with fitted values
#' plot_stratified_residuals(
#'   model_wrong,
#'   treatment = treatment,
#'   plot_type = "both"
#' )
#'
#' # Plot with propensity scores
#' ps_model <- glm(treatment ~ x, family = binomial)
#'
#' plot_stratified_residuals(
#'   model_wrong,
#'   treatment = treatment,
#'   ps_model = ps_model,
#'   plot_type = "color"
#' )
#'
#' # Method 2: Using data frame
#' library(dplyr)
#' plot_data <- data.frame(
#'   treatment = treatment,
#'   residuals = residuals(model_wrong),
#'   fitted_values = fitted(model_wrong),
#'   propensity_score = fitted(ps_model)
#' )
#'
#' plot_stratified_residuals(
#'   plot_data,
#'   treatment = treatment,
#'   residuals = residuals,
#'   x_var = propensity_score,
#'   plot_type = "facet"
#' )
#' }
#'
#' @references
#' - Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of the
#'   propensity score in observational studies for causal effects.
#'   Biometrika, 70(1), 41-55.
#' - D'Agostino McGowan, L., D'Agostino, R. B, Sr., & D'Agostino, R. B, Jr. (2023).
#'   A Visual Diagnostic Tool for Causal Inference. Observational Studies,
#'   9(1), 87-95.
#'
#' @seealso
#' - [`plot_calibration()`] for calibration plots
#' - [`plot_roc_curve()`] for ROC curves
#' - [`plot_qq()`] for QQ plots
#'
#' @export
plot_stratified_residuals <- function(x, ...) {
  UseMethod("plot_stratified_residuals")
}

#' @rdname plot_stratified_residuals
#' @param treatment A vector indicating treatment group membership.
#'   Must have exactly two unique levels. For data frames, can be
#'   an unquoted column name.
#' @param ps_model Optional propensity score model (glm object).
#'   If provided, uses propensity scores instead of fitted values.
#' @param plot_type Character; type of plot - "color" (default), "facet", or "both".
#'   - "color": Single plot with points colored by treatment
#'   - "facet": Separate facets for each treatment group
#'   - "both": Both color and faceting
#' @param smooth Logical; whether to add loess smoothing curves. Default is TRUE.
#' @param smooth_span Numeric; span parameter for loess smoothing. Default is 1.
#' @param alpha Numeric; transparency level for points. Default is 0.25.
#' @param na.rm Logical; if TRUE, remove missing values before plotting.
#' @export
plot_stratified_residuals.lm <- function(
  x,
  treatment,
  ps_model = NULL,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
) {
  plot_type <- rlang::arg_match(plot_type)

  # Extract residuals from the outcome model
  .residuals <- stats::residuals(x)

  # Determine x-axis values and label
  if (!is.null(ps_model)) {
    # Use propensity scores if PS model provided
    if (!inherits(ps_model, c("glm", "lm"))) {
      abort("{.arg ps_model} must be a glm or lm object")
    }
    .ps_or_fitted <- stats::fitted(ps_model)
    x_label <- "Propensity score"
  } else {
    # Use fitted values from outcome model
    .ps_or_fitted <- stats::fitted(x)
    x_label <- "Fitted values"
  }

  # Call the internal plotting function
  plot_stratified_residuals_impl(
    .residuals = .residuals,
    .ps_or_fitted = .ps_or_fitted,
    .treatment = treatment,
    plot_type = plot_type,
    smooth = smooth,
    smooth_span = smooth_span,
    alpha = alpha,
    x_label = x_label,
    na.rm = na.rm
  )
}

#' @rdname plot_stratified_residuals
#' @export
plot_stratified_residuals.glm <- plot_stratified_residuals.lm

#' @rdname plot_stratified_residuals
#' @param residuals Column containing residuals. Supports tidyselect syntax.
#' @param x_var Column for x-axis values (fitted values or propensity scores).
#'   Supports tidyselect syntax.
#' @export
plot_stratified_residuals.data.frame <- function(
  x,
  treatment,
  residuals,
  x_var,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
) {
  plot_type <- match.arg(plot_type)

  # Validate data frame
  validate_data_frame(x)

  # Extract column names using tidyselect
  treatment_quo <- rlang::enquo(treatment)
  residuals_quo <- rlang::enquo(residuals)
  x_var_quo <- rlang::enquo(x_var)

  # Get treatment column
  treatment_name <- get_column_name(treatment_quo, "treatment")
  validate_column_exists(x, treatment_name, "treatment")
  .treatment <- x[[treatment_name]]

  # Get residuals column
  residuals_name <- get_column_name(residuals_quo, "residuals")
  validate_column_exists(x, residuals_name, "residuals")
  .residuals <- x[[residuals_name]]

  # Get x-axis column
  x_var_name <- get_column_name(x_var_quo, "x_var")
  validate_column_exists(x, x_var_name, "x_var")
  .ps_or_fitted <- x[[x_var_name]]

  # Infer label from column name
  x_label <- if (grepl("propensity|ps", x_var_name, ignore.case = TRUE)) {
    "Propensity score"
  } else {
    x_var_name
  }

  # Call the internal plotting function
  plot_stratified_residuals_impl(
    .residuals = .residuals,
    .ps_or_fitted = .ps_or_fitted,
    .treatment = .treatment,
    plot_type = plot_type,
    smooth = smooth,
    smooth_span = smooth_span,
    alpha = alpha,
    x_label = x_label,
    na.rm = na.rm
  )
}

# Internal implementation function
plot_stratified_residuals_impl <- function(
  .residuals,
  .ps_or_fitted,
  .treatment,
  plot_type,
  smooth,
  smooth_span,
  alpha,
  x_label,
  na.rm
) {
  # Validate inputs
  validate_numeric(.residuals, ".residuals")
  validate_numeric(.ps_or_fitted, ".ps_or_fitted")

  # Validate treatment has exactly 2 levels
  validate_binary_group(.treatment, ".treatment")

  # Check lengths
  validate_equal_length(
    .residuals,
    .ps_or_fitted,
    ".residuals",
    ".ps_or_fitted"
  )
  validate_equal_length(.residuals, .treatment, ".residuals", ".treatment")

  # Create data frame for plotting
  plot_data <- data.frame(
    residuals = .residuals,
    x_var = .ps_or_fitted,
    treatment = as.factor(.treatment)
  )

  # Handle missing values
  if (na.rm) {
    plot_data <- plot_data[stats::complete.cases(plot_data), ]
  }

  # Base plot
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$x_var, y = .data$residuals)
  )

  # Add points with color based on plot type
  if (plot_type %in% c("color", "both")) {
    p <- p +
      ggplot2::geom_point(
        ggplot2::aes(color = .data$treatment),
        alpha = alpha
      )
  } else {
    p <- p + ggplot2::geom_point(alpha = alpha)
  }

  # Add smoothing if requested
  if (smooth) {
    if (plot_type %in% c("color", "both")) {
      p <- p +
        ggplot2::geom_smooth(
          ggplot2::aes(color = .data$treatment),
          method = "loess",
          formula = y ~ x,
          se = FALSE,
          span = smooth_span
        )
    } else {
      p <- p +
        ggplot2::geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = FALSE,
          span = smooth_span
        )
    }
  }

  # Add faceting if requested
  if (plot_type %in% c("facet", "both")) {
    p <- p + ggplot2::facet_wrap(~treatment)
  }

  # Styling
  p <- p +
    ggplot2::labs(
      x = x_label,
      y = "residuals"
    )

  # Add horizontal reference line at y = 0
  p <- p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)

  p
}

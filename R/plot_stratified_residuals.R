#' Create stratified residual diagnostic plots
#'
#' Create diagnostic plots to assess whether covariate adjustment adequately
#' controls confounding in observational studies. This function plots residuals
#' from an outcome model against propensity scores (or fitted values),
#' stratified by treatment group, to reveal model mis-specification.
#'
#' @details
#' This diagnostic plot was originally suggested by Rosenbaum and Rubin (1983)
#' and revisited by McGowan-D'Agostino, D'Agostino, and D'Agostino (2023).
#' The key idea is that plotting residuals against propensity scores
#' (rather than fitted values) can reveal non-linear relationships or
#' heterogeneous treatment effects that might be obscured in standard
#' residuals-vs-fitted plots.
#'
#' The function accepts either:
#' - A fitted model object, from which it extracts residuals and fitted values
#' - Separate vectors of residuals and propensity scores/fitted values
#'
#' @param .model A fitted model object (e.g., from `lm()` or `glm()`).
#'   If provided, residuals and fitted values are extracted automatically.
#' @param .residuals A numeric vector of residuals. Required if `.model` is NULL.
#' @param .ps_or_fitted A numeric vector of propensity scores or fitted values
#'   to plot on the x-axis. Required if `.model` is NULL.
#' @param .treatment A vector indicating treatment group membership.
#'   Must have exactly two unique levels.
#' @param plot_type Character; type of plot - "color" (default), "facet", or "both".
#'   - "color": Single plot with points colored by treatment
#'   - "facet": Separate facets for each treatment group
#'   - "both": Both color and faceting
#' @param smooth Logical; whether to add loess smoothing curves. Default is TRUE.
#' @param smooth_span Numeric; span parameter for loess smoothing. Default is 1.
#' @param alpha Numeric; transparency level for points. Default is 0.25.
#' @param x_label Character; label for x-axis. Default is "Propensity score"
#'   if using propensity scores, or "Fitted values" otherwise.
#' @param na.rm Logical; if TRUE, remove missing values before plotting.
#'
#' @return A ggplot2 object.
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
#' # Fit misspecified model (missing interaction)
#' model_wrong <- lm(y ~ treatment + x)
#'
#' # Create diagnostic plot using fitted model
#' plot_stratified_residuals(
#'   .model = model_wrong,
#'   .treatment = treatment,
#'   plot_type = "both"
#' )
#'
#' # Fit correct model with interaction
#' model_correct <- lm(y ~ treatment * x)
#'
#' # Compare plots
#' plot_stratified_residuals(
#'   .model = model_correct,
#'   .treatment = treatment,
#'   plot_type = "both"
#' )
#'
#' # Using propensity scores instead of fitted values
#' ps_model <- glm(treatment ~ x, family = binomial)
#' ps_fitted <- fitted(ps_model)
#'
#' plot_stratified_residuals(
#'   .residuals = residuals(model_wrong),
#'   .ps_or_fitted = ps_fitted,
#'   .treatment = treatment,
#'   plot_type = "color",
#'   x_label = "Propensity score"
#' )
#'
#' # The propensity score plot often reveals patterns more clearly
#' plot_stratified_residuals(
#'   .residuals = residuals(model_wrong),
#'   .ps_or_fitted = ps_fitted,
#'   .treatment = treatment,
#'   plot_type = "facet",
#'   x_label = "Propensity score"
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
plot_stratified_residuals <- function(
  .model = NULL,
  .residuals = NULL,
  .ps_or_fitted = NULL,
  .treatment = NULL,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  x_label = NULL,
  na.rm = FALSE
) {
  plot_type <- match.arg(plot_type)

  # Extract residuals and fitted values from model if provided
  if (!is.null(.model)) {
    if (is.null(.residuals)) {
      .residuals <- stats::residuals(.model)
    }
    if (is.null(.ps_or_fitted)) {
      .ps_or_fitted <- stats::fitted(.model)
    }
  }

  # Validate inputs
  if (is.null(.residuals) || is.null(.ps_or_fitted)) {
    abort(
      "Either {.arg .model} must be provided, or both {.arg .residuals} and {.arg .ps_or_fitted} must be provided."
    )
  }

  if (is.null(.treatment)) {
    abort("{.arg .treatment} must be provided.")
  }

  # Validate numeric inputs
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

  # Set x-axis label if not provided
  if (is.null(x_label)) {
    x_label <- "Fitted values"
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

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_stratified_residuals.R
\name{plot_stratified_residuals}
\alias{plot_stratified_residuals}
\alias{plot_stratified_residuals.lm}
\alias{plot_stratified_residuals.glm}
\alias{plot_stratified_residuals.data.frame}
\title{Create stratified residual diagnostic plots}
\usage{
plot_stratified_residuals(x, ...)

\method{plot_stratified_residuals}{lm}(
  x,
  treatment,
  ps_model = NULL,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
)

\method{plot_stratified_residuals}{glm}(
  x,
  treatment,
  ps_model = NULL,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
)

\method{plot_stratified_residuals}{data.frame}(
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
)
}
\arguments{
\item{x}{Either a fitted model object (lm or glm) or a data frame}

\item{...}{Additional arguments passed to methods}

\item{treatment}{A vector indicating treatment group membership.
Must have exactly two unique levels. For data frames, can be
an unquoted column name.}

\item{ps_model}{Optional propensity score model (glm object).
If provided, uses propensity scores instead of fitted values.}

\item{plot_type}{Character; type of plot - "color" (default), "facet", or "both".
\itemize{
\item "color": Single plot with points colored by treatment
\item "facet": Separate facets for each treatment group
\item "both": Both color and faceting
}}

\item{smooth}{Logical; whether to add loess smoothing curves. Default is TRUE.}

\item{smooth_span}{Numeric; span parameter for loess smoothing. Default is 1.}

\item{alpha}{Numeric; transparency level for points. Default is 0.25.}

\item{na.rm}{Logical; if TRUE, remove missing values before plotting.}

\item{residuals}{Column containing residuals. Supports tidyselect syntax.}

\item{x_var}{Column for x-axis values (fitted values or propensity scores).
Supports tidyselect syntax.}
}
\value{
A ggplot2 object
}
\description{
Create diagnostic plots to assess differences between treatment group after adjustment.
This function plots residuals
from an outcome model against propensity scores (or fitted values),
stratified by treatment group, to reveal model mis-specification.
}
\details{
This diagnostic plot was originally suggested by Rosenbaum and Rubin (1983)
and revisited by D'Agostino McGowan, D'Agostino, and D'Agostino (2023).
The key idea is that plotting residuals against propensity scores
or fitted values by treatment group can reveal non-linear relationships or
heterogeneous treatment effects that might be obscured in standard
residuals-vs-fitted plots.

The function supports two approaches:
\itemize{
\item For regression models (lm/glm): Extracts residuals and fitted values automatically
\item For data frames: Uses specified columns for residuals, treatment, and x-axis values
}
}
\examples{
\dontrun{
library(ggplot2)

# Simulate data with treatment effect heterogeneity
set.seed(8)
n <- 1000
x <- rnorm(n)
ps <- plogis(x)  # True propensity score
treatment <- rbinom(n, 1, ps)
y1 <- 0.5 * x + rnorm(n)
y0 <- -0.5 * x + rnorm(n)
y <- treatment * y1 + (1 - treatment) * y0

# Method 1: Using model objects
# Fit misspecified model (missing interaction)
model_wrong <- lm(y ~ treatment + x)

# Plot with fitted values
plot_stratified_residuals(
  model_wrong,
  treatment = treatment,
  plot_type = "both"
)

# Plot with propensity scores
ps_model <- glm(treatment ~ x, family = binomial)

plot_stratified_residuals(
  model_wrong,
  treatment = treatment,
  ps_model = ps_model,
  plot_type = "color"
)

# Method 2: Using data frame
library(dplyr)
plot_data <- data.frame(
  treatment = treatment,
  residuals = residuals(model_wrong),
  fitted_values = fitted(model_wrong),
  propensity_score = fitted(ps_model)
)

plot_stratified_residuals(
  plot_data,
  treatment = treatment,
  residuals = residuals,
  x_var = propensity_score,
  plot_type = "facet"
)
}

}
\references{
\itemize{
\item Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of the
propensity score in observational studies for causal effects.
Biometrika, 70(1), 41-55.
\item D'Agostino McGowan, L., D'Agostino, R. B, Sr., & D'Agostino, R. B, Jr. (2023).
A Visual Diagnostic Tool for Causal Inference. Observational Studies,
9(1), 87-95.
}
}
\seealso{
\itemize{
\item \code{\link[=plot_calibration]{plot_calibration()}} for calibration plots
\item \code{\link[=plot_roc_curve]{plot_roc_curve()}} for ROC curves
\item \code{\link[=plot_qq]{plot_qq()}} for QQ plots
}
}

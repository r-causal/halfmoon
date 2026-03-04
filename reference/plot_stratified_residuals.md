# Create stratified residual diagnostic plots

Create diagnostic plots to assess differences between .exposure group
after adjustment. This function plots residuals from an outcome model
against propensity scores (or fitted values), stratified by .exposure
group, to reveal model mis-specification.

## Usage

``` r
plot_stratified_residuals(x, ...)

# S3 method for class 'lm'
plot_stratified_residuals(
  x,
  .exposure,
  ps_model = NULL,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
)

# S3 method for class 'glm'
plot_stratified_residuals(
  x,
  .exposure,
  ps_model = NULL,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
)

# S3 method for class 'data.frame'
plot_stratified_residuals(
  x,
  .exposure,
  residuals,
  x_var,
  plot_type = c("color", "facet", "both"),
  smooth = TRUE,
  smooth_span = 1,
  alpha = 0.25,
  na.rm = FALSE,
  ...
)
```

## Arguments

- x:

  Either a fitted model object (lm or glm) or a data frame

- ...:

  Additional arguments passed to methods

- .exposure:

  A vector indicating .exposure group membership. Must have exactly two
  unique levels. For data frames, can be an unquoted column name.

- ps_model:

  Optional propensity score model (glm object). If provided, uses
  propensity scores instead of fitted values.

- plot_type:

  Character; type of plot - "color" (default), "facet", or "both".

  - "color": Single plot with points colored by .exposure

  - "facet": Separate facets for each .exposure group

  - "both": Both color and faceting

- smooth:

  Logical; whether to add loess smoothing curves. Default is TRUE.

- smooth_span:

  Numeric; span parameter for loess smoothing. Default is 1.

- alpha:

  Numeric; transparency level for points. Default is 0.25.

- na.rm:

  Logical; if TRUE, remove missing values before plotting.

- residuals:

  Column containing residuals. Supports tidyselect syntax.

- x_var:

  Column for x-axis values (fitted values or propensity scores).
  Supports tidyselect syntax.

## Value

A ggplot2 object

## Details

This diagnostic plot was originally suggested by Rosenbaum and Rubin
(1983) and revisited by D'Agostino McGowan, D'Agostino, and D'Agostino
(2023). The key idea is that plotting residuals against propensity
scores or fitted values by .exposure group can reveal non-linear
relationships or heterogeneous .exposure effects that might be obscured
in standard residuals-vs-fitted plots.

The function supports two approaches:

- For regression models (lm/glm): Extracts residuals and fitted values
  automatically

- For data frames: Uses specified columns for residuals, .exposure, and
  x-axis values

## References

- Rosenbaum, P. R., & Rubin, D. B. (1983). The central role of the
  propensity score in observational studies for causal effects.
  Biometrika, 70(1), 41-55.

- D'Agostino McGowan, L., D'Agostino, R. B, Sr., & D'Agostino, R. B, Jr.
  (2023). A Visual Diagnostic Tool for Causal Inference. Observational
  Studies, 9(1), 87-95.

## See also

- [`plot_model_calibration()`](https://r-causal.github.io/halfmoon/reference/plot_model_calibration.md)
  for calibration plots

- [`plot_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/plot_model_roc_curve.md)
  for ROC curves

- [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  for QQ plots

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)

# Simulate data with .exposure effect heterogeneity
set.seed(8)
n <- 1000
x <- rnorm(n)
ps <- plogis(x)  # True propensity score
.exposure <- rbinom(n, 1, ps)
y1 <- 0.5 * x + rnorm(n)
y0 <- -0.5 * x + rnorm(n)
y <- .exposure * y1 + (1 - .exposure) * y0

# Method 1: Using model objects
# Fit misspecified model (missing interaction)
model_wrong <- lm(y ~ .exposure + x)

# Plot with fitted values
plot_stratified_residuals(
  model_wrong,
  .exposure = .exposure,
  plot_type = "both"
)

# Plot with propensity scores
ps_model <- glm(.exposure ~ x, family = binomial)

plot_stratified_residuals(
  model_wrong,
  .exposure = .exposure,
  ps_model = ps_model,
  plot_type = "color"
)

# Method 2: Using data frame
library(dplyr)
plot_data <- data.frame(
  .exposure = .exposure,
  residuals = residuals(model_wrong),
  fitted_values = fitted(model_wrong),
  propensity_score = fitted(ps_model)
)

plot_stratified_residuals(
  plot_data,
  .exposure = .exposure,
  residuals = residuals,
  x_var = propensity_score,
  plot_type = "facet"
)
} # }
```

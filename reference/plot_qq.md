# Create QQ plots for weighted and unweighted samples

Create quantile-quantile (QQ) plots to compare the distribution of
variables between treatment groups before and after weighting. This
function helps assess covariate balance by visualizing how well the
quantiles align between groups.

## Usage

``` r
plot_qq(.data, ...)

# Default S3 method
plot_qq(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  .reference_level = NULL,
  na.rm = FALSE,
  ...
)

# S3 method for class 'halfmoon_qq'
plot_qq(.data, ...)
```

## Arguments

- .data:

  A data frame containing the variables or a halfmoon_qq object.

- ...:

  Arguments passed to methods (see Methods section).

- .var:

  Variable to plot. Can be unquoted (e.g., `age`) or quoted (e.g.,
  `"age"`).

- .exposure:

  Column name of treatment/exposure variable. Can be unquoted (e.g.,
  `qsmk`) or quoted (e.g., `"qsmk"`).

- .weights:

  Optional weighting variable(s). Can be unquoted variable names, a
  character vector, or NULL. Multiple weights can be provided to compare
  different weighting schemes. Default is NULL (unweighted).

- quantiles:

  Numeric vector of quantiles to compute. Default is
  `seq(0.01, 0.99, 0.01)` for 99 quantiles.

- include_observed:

  Logical. If using `.weights`, also show observed (unweighted) QQ plot?
  Defaults to TRUE.

- .reference_level:

  The reference treatment level to use for comparisons. If `NULL`
  (default), uses the last level for factors or the maximum value for
  numeric variables.

- na.rm:

  Logical; if TRUE, drop NA values before computation.

## Value

A ggplot2 object.

## Details

QQ plots display the quantiles of one distribution against the quantiles
of another. Perfect distributional balance appears as points along the
45-degree line (y = x). This function automatically adds this reference
line and appropriate axis labels.

For an alternative visualization of the same information, see
[`geom_ecdf()`](https://r-causal.github.io/halfmoon/reference/geom_ecdf.md),
which shows the empirical cumulative distribution functions directly.

## Methods

- `plot_qq.default`:

  For data frames. Accepts all documented parameters.

- `plot_qq.halfmoon_qq`:

  For halfmoon_qq objects from
  [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md).
  Only uses `.data` and `...` parameters.

## See also

- [`geom_ecdf()`](https://r-causal.github.io/halfmoon/reference/geom_ecdf.md)
  for ECDF plots, an alternative distributional visualization

- [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  for the underlying geom used by this function

- [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md)
  for computing QQ data without plotting

## Examples

``` r
library(ggplot2)

# Basic QQ plot (observed)
plot_qq(nhefs_weights, age, qsmk)


# With weighting
plot_qq(nhefs_weights, age, qsmk, .weights = w_ate)


# Compare multiple weighting schemes
plot_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))


# For propensity scores
plot_qq(nhefs_weights, .fitted, qsmk, .weights = w_ate)


# Without observed comparison
plot_qq(nhefs_weights, age, qsmk, .weights = w_ate, include_observed = FALSE)

```

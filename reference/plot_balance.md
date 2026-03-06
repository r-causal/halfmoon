# Create balance plot from check_balance output

Create a Love plot-style visualization to assess balance across multiple
metrics computed by
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md).
This function wraps
[`geom_love()`](https://r-causal.github.io/tidysmd/reference/geom_love.html)
to create a comprehensive balance assessment plot.

## Usage

``` r
plot_balance(
  .df,
  abs_smd = TRUE,
  facet_scales = "free",
  linewidth = 0.8,
  point_size = 1.85,
  vline_xintercept = 0.1,
  vline_color = "grey70",
  vlinewidth = 0.6
)
```

## Arguments

- .df:

  A data frame produced by
  [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)

- abs_smd:

  Logical. Take the absolute value of SMD estimates? Defaults to TRUE.
  Does not affect other metrics which are already non-negative.

- facet_scales:

  Character. Scale specification for facets. Defaults to "free" to allow
  different scales for different metrics. Options are "fixed", "free_x",
  "free_y", or "free".

- linewidth:

  The line size, passed to
  [`ggplot2::geom_line()`](https://ggplot2.tidyverse.org/reference/geom_path.html).

- point_size:

  The point size, passed to
  [`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html).

- vline_xintercept:

  The X intercept, passed to
  [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- vline_color:

  The vertical line color, passed to
  [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

- vlinewidth:

  The vertical line size, passed to
  [`ggplot2::geom_vline()`](https://ggplot2.tidyverse.org/reference/geom_abline.html).

## Value

A ggplot2 object

## Details

This function visualizes the output of
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
creating a plot that shows balance statistics across different
variables, methods, and metrics. The plot uses faceting to separate
different metrics and displays the absolute value of SMD by default
(controlled by `abs_smd`).

For categorical exposures (\>2 levels), the function automatically
detects multiple group level comparisons and uses
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
to display each comparison in a separate row, with metrics in columns.
For binary exposures, the standard
[`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
by metric is used.

Different metrics have different interpretations:

- **SMD**: Standardized mean differences, where values near 0 indicate
  good balance. Often displayed as absolute values.

- **Variance Ratio**: Ratio of variances between groups, where values
  near 1 indicate similar variability.

- **KS**: Kolmogorov-Smirnov statistic, where smaller values indicate
  better distributional balance.

- **Correlation**: For continuous exposures, measures association with
  covariates.

- **Energy**: Multivariate balance metric applied to all variables
  simultaneously.

## See also

[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
for computing balance metrics,
[`geom_love()`](https://r-causal.github.io/tidysmd/reference/geom_love.html)
for the underlying geom

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
[`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md),
[`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
[`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
[`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
[`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md),
[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md)

## Examples

``` r
# Compute balance metrics
balance_data <- check_balance(
  nhefs_weights,
  c(age, education, race),
  qsmk,
  .weights = c(w_ate, w_att)
)

# Create balance plot
plot_balance(balance_data)


# Without absolute SMD values
plot_balance(balance_data, abs_smd = FALSE)


# With fixed scales across facets
plot_balance(balance_data, facet_scales = "fixed")


# Customize threshold lines
plot_balance(balance_data, vline_xintercept = 0.05)


# Categorical exposure example
# Automatically uses facet_grid to show each group comparison
balance_cat <- check_balance(
  nhefs_weights,
  c(age, wt71, sex),
  alcoholfreq_cat,
  .weights = w_cat_ate,
  .metrics = c("smd", "vr")
)
plot_balance(balance_cat)
```

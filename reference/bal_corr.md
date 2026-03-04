# Balance Weighted or Unweighted Pearson Correlation

Calculates the Pearson correlation coefficient between two numeric
vectors, with optional case weights. Uses the standard correlation
formula for unweighted data and weighted covariance for weighted data.

## Usage

``` r
bal_corr(.x, .y, .weights = NULL, na.rm = FALSE)
```

## Arguments

- .x:

  A numeric vector containing the first variable.

- .y:

  A numeric vector containing the second variable. Must have the same
  length as `.x`.

- .weights:

  An optional numeric vector of case weights. If provided, must have the
  same length as `.x` and `.y`. All weights must be non-negative.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values result in `NA`
  output.

## Value

A numeric value representing the correlation coefficient between -1
and 1. Returns `NA` if either variable has zero variance.

## See also

[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
for computing multiple balance metrics at once

Other balance functions:
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
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
bal_corr(nhefs_weights$age, nhefs_weights$wt71)
#> [1] 0.02219162
```

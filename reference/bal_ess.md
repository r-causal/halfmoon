# Calculate Effective Sample Size for Single Weight Vector

Computes the effective sample size (ESS) for a single weighting scheme.
This is a wrapper around
[`ess()`](https://r-causal.github.io/halfmoon/reference/ess.md) that
follows the bal\_\*() naming convention for API consistency.

## Usage

``` r
bal_ess(.weights, na.rm = FALSE)
```

## Arguments

- .weights:

  A numeric vector of weights or a single weight column from a data
  frame.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.

## Value

A single numeric value representing the effective sample size.

## Details

The effective sample size (ESS) is calculated using the classical
formula: \\ESS = (\sum w)^2 / \sum(w^2)\\.

ESS reflects how many observations you would have if all were equally
weighted. When weights vary substantially, the ESS can be much smaller
than the actual number of observations, indicating that a few
observations carry disproportionately large weights.

**Diagnostic Value**:

- A large discrepancy between ESS and the actual sample size indicates
  that a few observations carry disproportionately large weights

- A small ESS signals that weighted estimates are more sensitive to a
  handful of observations, inflating the variance and standard errors

- If ESS is much lower than the total sample size, consider
  investigating why some weights are extremely large or small

## See also

[`ess()`](https://r-causal.github.io/halfmoon/reference/ess.md) for the
underlying implementation,
[`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
for computing ESS across multiple weighting schemes

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
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
# ESS for ATE weights
bal_ess(nhefs_weights$w_ate)
#> [1] 1013.312

# ESS for ATT weights
bal_ess(nhefs_weights$w_att)
#> [1] 1069.587

# With missing values
weights_with_na <- nhefs_weights$w_ate
weights_with_na[1:5] <- NA
bal_ess(weights_with_na, na.rm = TRUE)
#> [1] 1010.012
```

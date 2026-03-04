# Calculate Single AUC for Model Balance Assessment

Computes the Area Under the ROC Curve (AUC) for a single weighting
scheme or unweighted data. In causal inference, an AUC around 0.5
indicates good balance between treatment groups.

## Usage

``` r
bal_model_auc(
  .data,
  .exposure,
  .fitted,
  .weights = NULL,
  na.rm = TRUE,
  .focal_level = NULL
)
```

## Arguments

- .data:

  A data frame containing the variables.

- .exposure:

  The treatment/outcome variable (unquoted).

- .fitted:

  The propensity score or fitted values (unquoted).

- .weights:

  Optional single weight variable (unquoted). If NULL, computes
  unweighted AUC.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.

- .focal_level:

  The level of the outcome variable to consider as the treatment/event.
  If `NULL` (default), uses the last level for factors or the maximum
  value for numeric variables.

## Value

A numeric value representing the AUC. Values around 0.5 indicate good
balance, while values closer to 0 or 1 indicate poor balance.

## Details

The AUC provides a single metric for assessing propensity score balance.
When propensity scores achieve perfect balance, the weighted
distribution of scores should be identical between treatment groups,
resulting in an AUC of 0.5 (chance performance).

AUC values significantly different from 0.5 indicate systematic
differences in propensity score distributions between groups, suggesting
inadequate balance.

## See also

[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
for computing AUC across multiple weights,
[`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md)
for the full ROC curve

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
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
# Unweighted AUC
bal_model_auc(nhefs_weights, qsmk, .fitted)
#> [1] 0.6626473

# Weighted AUC
bal_model_auc(nhefs_weights, qsmk, .fitted, w_ate)
#> [1] 0.5017247
```

# Calculate Single ROC Curve for Model Balance Assessment

Computes the Receiver Operating Characteristic (ROC) curve for a single
weighting scheme or unweighted data. In causal inference, an ROC curve
near the diagonal indicates good balance between treatment groups.

## Usage

``` r
bal_model_roc_curve(
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
  unweighted ROC curve.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.

- .focal_level:

  The level of the outcome variable to consider as the treatment/event.
  If `NULL` (default), uses the last level for factors or the maximum
  value for numeric variables.

## Value

A tibble with columns:

- threshold:

  Numeric. The decision threshold.

- sensitivity:

  Numeric. True positive rate at the threshold.

- specificity:

  Numeric. True negative rate at the threshold.

## Details

The ROC curve plots sensitivity (true positive rate) against
1-specificity (false positive rate) across all possible threshold
values. When propensity scores achieve perfect balance, the ROC curve
should lie close to the diagonal line from (0,0) to (1,1), indicating
that the propensity scores have no discriminatory ability between
treatment groups.

ROC curves that bow significantly above the diagonal indicate that the
propensity scores can still distinguish between treatment groups,
suggesting inadequate balance.

## See also

[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
for computing ROC curves across multiple weights,
[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md)
for the area under the curve summary

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
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
# Unweighted ROC curve
bal_model_roc_curve(nhefs_weights, qsmk, .fitted)
#> # A tibble: 1,568 × 3
#>    threshold sensitivity specificity
#>        <dbl>       <dbl>       <dbl>
#>  1 -Inf            1        0       
#>  2    0.0510       1        0       
#>  3    0.0527       1        0.000860
#>  4    0.0558       1        0.00172 
#>  5    0.0559       1        0.00258 
#>  6    0.0567       1        0.00344 
#>  7    0.0595       1        0.00430 
#>  8    0.0599       1        0.00516 
#>  9    0.0601       0.998    0.00516 
#> 10    0.0608       0.995    0.00516 
#> # ℹ 1,558 more rows

# Weighted ROC curve
bal_model_roc_curve(nhefs_weights, qsmk, .fitted, w_ate)
#> # A tibble: 1,568 × 3
#>    threshold sensitivity specificity
#>        <dbl>       <dbl>       <dbl>
#>  1 -Inf            1        0       
#>  2    0.0510       1        0       
#>  3    0.0527       1        0.000673
#>  4    0.0558       1        0.00135 
#>  5    0.0559       1        0.00202 
#>  6    0.0567       1        0.00270 
#>  7    0.0595       1        0.00338 
#>  8    0.0599       1        0.00406 
#>  9    0.0601       0.989    0.00406 
#> 10    0.0608       0.979    0.00406 
#> # ℹ 1,558 more rows
```

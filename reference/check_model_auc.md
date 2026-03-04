# Check Balance Using Weighted ROC Curves

Computes weighted ROC curves and AUC for evaluating propensity score
balance. In causal inference, a weighted ROC curve near the diagonal
(AUC around 0.5) indicates good balance between treatment groups.

## Usage

``` r
check_model_auc(
  .data,
  .exposure,
  .fitted,
  .weights,
  include_observed = TRUE,
  na.rm = TRUE,
  .focal_level = NULL
)
```

## Arguments

- .data:

  A data frame containing the variables.

- .exposure:

  The treatment/outcome variable.

- .fitted:

  The propensity score or fitted values.

- .weights:

  Weighting variables (supports tidyselect).

- include_observed:

  Logical. If using `.weights`, also calculate observed (unweighted)
  metrics? Defaults to TRUE.

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

- method:

  Character. The weighting method ("observed" or weight variable name).

- auc:

  Numeric. The ROC AUC value.

## Details

The Area Under the ROC Curve (AUC) provides a single metric for
assessing propensity score balance. When propensity scores achieve
perfect balance, the weighted distribution of scores should be identical
between treatment groups, resulting in an AUC of 0.5 (chance
performance).

AUC values significantly different from 0.5 indicate systematic
differences in propensity score distributions between groups, suggesting
inadequate balance. Values closer to 0.5 indicate better balance
achieved by the weighting scheme.

This approach complements traditional balance diagnostics by focusing
specifically on the propensity score overlap and balance.

## See also

[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md)
for single AUC values,
[`plot_model_auc()`](https://r-causal.github.io/halfmoon/reference/plot_model_auc.md)
for visualization,
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
for other balance metrics

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
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Check balance for propensity scores
check_model_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))
#> # A tibble: 3 × 2
#>   method     auc
#>   <chr>    <dbl>
#> 1 observed 0.663
#> 2 w_ate    0.502
#> 3 w_att    0.495

# Without observed results
check_model_auc(nhefs_weights, qsmk, .fitted, w_ate, include_observed = FALSE)
#> # A tibble: 1 × 2
#>   method   auc
#>   <chr>  <dbl>
#> 1 w_ate  0.502
```

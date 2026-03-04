# Check ROC Curves for Multiple Weights

Computes ROC curves (weighted or unweighted) for evaluating propensity
score balance. In causal inference, an ROC curve near the diagonal (AUC
around 0.5) indicates good balance between treatment groups.

## Usage

``` r
check_model_roc_curve(
  .data,
  .exposure,
  .fitted,
  .weights = NULL,
  include_observed = TRUE,
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

  The propensity score or covariate (unquoted).

- .weights:

  Optional weighting variables (unquoted, can be multiple).

- include_observed:

  Include unweighted results? Default TRUE.

- na.rm:

  Remove missing values? Default TRUE.

- .focal_level:

  The level of `.exposure` to consider as the treatment/event. Default
  is NULL, which uses the second level.

## Value

A tibble with class "halfmoon_roc" containing ROC curve data.

## See also

[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
for AUC summaries,
[`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md)
for single ROC curves

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
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Check ROC curves for propensity scores with multiple weights
roc_data <- check_model_roc_curve(
  nhefs_weights,
  qsmk,
  .fitted,
  c(w_ate, w_att)
)

# Check ROC curve for a single weight without observed
check_model_roc_curve(
  nhefs_weights,
  qsmk,
  .fitted,
  w_ate,
  include_observed = FALSE
)
#> # A tibble: 1,568 × 4
#>    threshold sensitivity specificity method
#>        <dbl>       <dbl>       <dbl> <chr> 
#>  1 -Inf            1        0        w_ate 
#>  2    0.0510       1        0        w_ate 
#>  3    0.0527       1        0.000673 w_ate 
#>  4    0.0558       1        0.00135  w_ate 
#>  5    0.0559       1        0.00202  w_ate 
#>  6    0.0567       1        0.00270  w_ate 
#>  7    0.0595       1        0.00338  w_ate 
#>  8    0.0599       1        0.00406  w_ate 
#>  9    0.0601       0.989    0.00406  w_ate 
#> 10    0.0608       0.979    0.00406  w_ate 
#> # ℹ 1,558 more rows

# Specify a different focal level
check_model_roc_curve(
  nhefs_weights,
  qsmk,
  .fitted,
  w_ate,
  .focal_level = 0  # Use 0 as the treatment level instead of 1
)
#> # A tibble: 3,136 × 4
#>    threshold sensitivity specificity method  
#>        <dbl>       <dbl>       <dbl> <chr>   
#>  1 -Inf            1         0       observed
#>  2    0.0510       1         0       observed
#>  3    0.0527       0.999     0       observed
#>  4    0.0558       0.998     0       observed
#>  5    0.0559       0.997     0       observed
#>  6    0.0567       0.997     0       observed
#>  7    0.0595       0.996     0       observed
#>  8    0.0599       0.995     0       observed
#>  9    0.0601       0.995     0.00248 observed
#> 10    0.0608       0.995     0.00496 observed
#> # ℹ 3,126 more rows
```

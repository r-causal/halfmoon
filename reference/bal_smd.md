# Balance Standardized Mean Difference (SMD)

Calculates the standardized mean difference between two groups using the
smd package. This is a common measure of effect size for comparing group
differences while accounting for variability.

## Usage

``` r
bal_smd(
  .covariate,
  .exposure,
  .weights = NULL,
  .reference_level = NULL,
  na.rm = FALSE
)
```

## Arguments

- .covariate:

  A numeric vector containing the covariate values to compare.

- .exposure:

  A vector (factor or numeric) indicating group membership. Must have
  exactly two unique levels.

- .weights:

  An optional numeric vector of case weights. If provided, must have the
  same length as other input vectors. All weights must be non-negative.

- .reference_level:

  The reference group level for comparisons. Can be either a group level
  value or a numeric index. If `NULL` (default), uses the first level.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.

## Value

A numeric value representing the standardized mean difference. Positive
values indicate the comparison group has a higher mean than the
reference group.

## Details

The standardized mean difference (SMD) is calculated as: \$\$SMD =
\frac{\bar{x}\_1 - \bar{x}\_0}{\sqrt{(s_1^2 + s_0^2)/2}}\$\$ where
\\\bar{x}\_1\\ and \\\bar{x}\_0\\ are the means of the treatment and
control groups, and \\s_1^2\\ and \\s_0^2\\ are their variances.

In causal inference, SMD values of 0.1 or smaller are often considered
indicative of good balance between treatment groups.

## See also

[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
for computing multiple balance metrics at once

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
[`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md),
[`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
[`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
[`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md),
[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Binary exposure
bal_smd(nhefs_weights$age, nhefs_weights$qsmk)
#> [1] -0.2822208

# With weights
bal_smd(nhefs_weights$wt71, nhefs_weights$qsmk,
        .weights = nhefs_weights$w_ate)
#> [1] 0.009030294

# Categorical exposure (returns named vector)
bal_smd(nhefs_weights$age, nhefs_weights$alcoholfreq_cat)
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>             -0.4101980             -0.6853829             -0.6122164 
#>          daily_vs_none 
#>             -0.3337888 

# Specify reference level
bal_smd(nhefs_weights$age, nhefs_weights$alcoholfreq_cat,
        .reference_level = "daily")
#>           none_vs_daily lt_12_per_year_vs_daily  1_4_per_month_vs_daily 
#>             -0.33378882              0.07250964              0.32898774 
#>   2_3_per_week_vs_daily 
#>             -0.26514253 

# With categorical weights
bal_smd(nhefs_weights$wt71, nhefs_weights$alcoholfreq_cat,
        .weights = nhefs_weights$w_cat_ate)
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>            -0.07400341            -0.08935937            -0.06519465 
#>          daily_vs_none 
#>            -0.04164903 
```

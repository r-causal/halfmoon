# Balance Kolmogorov-Smirnov (KS) Statistic for Two Groups

Computes the two-sample KS statistic comparing empirical cumulative
distribution functions (CDFs) between two groups. For binary variables,
returns the absolute difference in proportions. For continuous
variables, computes the maximum difference between empirical CDFs.

## Usage

``` r
bal_ks(
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

A numeric value representing the KS statistic. Values range from 0 to 1,
with 0 indicating identical distributions and 1 indicating completely
separate distributions.

## Details

The Kolmogorov-Smirnov statistic measures the maximum difference between
empirical cumulative distribution functions of two groups: \$\$KS =
\max_x \|F_1(x) - F_0(x)\|\$\$ where \\F_1(x)\\ and \\F_0(x)\\ are the
empirical CDFs of the treatment and control groups.

For binary variables, this reduces to the absolute difference in
proportions. For continuous variables, the statistic captures
differences in the entire distribution shape, not just means or
variances.

The KS statistic ranges from 0 (identical distributions) to 1
(completely separate distributions). Smaller values indicate better
distributional balance between groups.

## See also

[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
for computing multiple balance metrics at once

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
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
# Binary exposure
bal_ks(nhefs_weights$age, nhefs_weights$qsmk)
#> [1] 0.1295593

# With weights
bal_ks(nhefs_weights$wt71, nhefs_weights$qsmk,
       .weights = nhefs_weights$w_ate)
#> [1] 0.03583763

# Categorical exposure (returns named vector)
bal_ks(nhefs_weights$age, nhefs_weights$alcoholfreq_cat)
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>              0.1956535              0.2913630              0.2637864 
#>          daily_vs_none 
#>              0.1774359 

# Specify reference level
bal_ks(nhefs_weights$age, nhefs_weights$alcoholfreq_cat,
       .reference_level = "none")
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>              0.1956535              0.2913630              0.2637864 
#>          daily_vs_none 
#>              0.1774359 

# With categorical weights
bal_ks(nhefs_weights$wt71, nhefs_weights$alcoholfreq_cat,
       .weights = nhefs_weights$w_cat_ate)
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>             0.07483474             0.05665006             0.07013520 
#>          daily_vs_none 
#>             0.06446345 
```

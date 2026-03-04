# Balance Variance Ratio for Two Groups

Calculates the ratio of variances between two groups: var(comparison) /
var(reference). For binary variables, uses the p\*(1-p) variance
formula. For continuous variables, uses Bessel's correction for weighted
sample variance.

## Usage

``` r
bal_vr(
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

A numeric value representing the variance ratio. Values greater than 1
indicate the comparison group has higher variance than the reference
group.

## Details

The variance ratio compares the variability of a covariate between
treatment groups. It is calculated as: \$\$VR = \frac{s_1^2}{s_0^2}\$\$
where \\s_1^2\\ and \\s_0^2\\ are the variances of the treatment and
control groups.

For binary variables (0/1), variance is computed as \\p(1-p)\\ where
\\p\\ is the proportion of 1s in each group. For continuous variables,
the weighted sample variance is used with Bessel's correction when
weights are provided.

Values close to 1.0 indicate similar variability between groups, which
is desirable for balance. Values substantially different from 1.0
suggest imbalanced variance.

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
[`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
[`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md),
[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Binary exposure
bal_vr(nhefs_weights$age, nhefs_weights$qsmk)
#> [1] 1.073075

# With weights
bal_vr(nhefs_weights$wt71, nhefs_weights$qsmk,
       .weights = nhefs_weights$w_ate)
#> [1] 1.000901

# Categorical exposure (returns named vector)
bal_vr(nhefs_weights$age, nhefs_weights$alcoholfreq_cat)
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>               1.271705               1.223574               1.295637 
#>          daily_vs_none 
#>               1.267799 

# Specify reference level
bal_vr(nhefs_weights$age, nhefs_weights$alcoholfreq_cat,
       .reference_level = "2_3_per_week")
#>           none_vs_2_3_per_week lt_12_per_year_vs_2_3_per_week 
#>                      0.7718213                      0.9815290 
#>  1_4_per_month_vs_2_3_per_week          daily_vs_2_3_per_week 
#>                      0.9443805                      0.9785144 

# With categorical weights
bal_vr(nhefs_weights$wt71, nhefs_weights$alcoholfreq_cat,
       .weights = nhefs_weights$w_cat_ate)
#> lt_12_per_year_vs_none  1_4_per_month_vs_none   2_3_per_week_vs_none 
#>              0.8819113              0.8610861              0.8629089 
#>          daily_vs_none 
#>              0.8990640 
```

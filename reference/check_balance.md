# Check Balance Across Multiple Metrics

Computes balance statistics for multiple variables across different
groups and optional weighting schemes. This function generalizes balance
checking by supporting multiple metrics (SMD, variance ratio,
Kolmogorov-Smirnov, weighted correlation) and returns results in a tidy
format.

## Usage

``` r
check_balance(
  .data,
  .vars,
  .exposure,
  .weights = NULL,
  .metrics = c("smd", "vr", "ks", "energy"),
  include_observed = TRUE,
  .reference_level = 1L,
  na.rm = FALSE,
  make_dummy_vars = TRUE,
  squares = FALSE,
  cubes = FALSE,
  interactions = FALSE
)
```

## Arguments

- .data:

  A data frame containing the variables to analyze.

- .vars:

  Variables for which to calculate metrics. Can be unquoted variable
  names, a character vector, or a tidyselect expression.

- .exposure:

  Grouping variable, e.g., treatment or exposure group.

- .weights:

  Optional weighting variables. Can be unquoted variable names, a
  character vector, or NULL. Multiple weights can be provided to compare
  different weighting schemes.

- .metrics:

  Character vector specifying which metrics to compute. Available
  options: "smd" (standardized mean difference), "vr" (variance ratio),
  "ks" (Kolmogorov-Smirnov), "correlation" (for continuous exposures),
  "energy" (multivariate energy distance). Defaults to c("smd", "vr",
  "ks", "energy").

- include_observed:

  Logical. If using `.weights`, also calculate observed (unweighted)
  metrics? Defaults to TRUE.

- .reference_level:

  The reference group level to use for comparisons. Defaults to 1 (first
  level).

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.

- make_dummy_vars:

  Logical. Transform categorical variables to dummy variables using
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)?
  Defaults to TRUE. When TRUE, categorical variables are expanded into
  separate binary indicators for each level.

- squares:

  Logical. Include squared terms for continuous variables? Defaults to
  FALSE. When TRUE, adds squared versions of numeric variables.

- cubes:

  Logical. Include cubed terms for continuous variables? Defaults to
  FALSE. When TRUE, adds cubed versions of numeric variables.

- interactions:

  Logical. Include all pairwise interactions between variables? Defaults
  to FALSE. When TRUE, creates interaction terms for all variable pairs,
  excluding interactions between levels of the same categorical variable
  and between squared/cubed terms.

## Value

A tibble with columns:

- variable:

  Character. The variable name being analyzed.

- group_level:

  Character. The non-reference group level.

- method:

  Character. The weighting method ("observed" or weight variable name).

- metric:

  Character. The balance metric computed ("smd", "vr", "ks").

- estimate:

  Numeric. The computed balance statistic.

## Details

This function serves as a comprehensive balance assessment tool by
computing multiple balance metrics simultaneously. It automatically
handles different variable types and can optionally transform variables
(dummy coding, polynomial terms, interactions) before computing balance
statistics.

The function supports several balance metrics:

- **SMD (Standardized Mean Difference)**: Measures effect size between
  groups, with values around 0.1 or smaller generally indicating good
  balance

- **Variance Ratio**: Compares group variances, with values near 1.0
  indicating similar variability between groups

- **Kolmogorov-Smirnov**: Tests distributional differences between
  groups, with smaller values indicating better balance

- **Correlation**: For continuous exposures, measures linear association
  between covariate and exposure

- **Energy Distance**: Multivariate test comparing entire distributions

When multiple weighting schemes are provided, the function computes
balance for each method, enabling comparison of different approaches
(e.g., ATE vs ATT weights). The `include_observed` parameter controls
whether unweighted ("observed") balance is included in the results.

## See also

[`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
[`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
for individual metric functions,
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)
for visualization

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
[`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md),
[`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
[`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
[`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
[`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md),
[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Basic usage with binary exposure
check_balance(nhefs_weights, c(age, wt71), qsmk, .weights = c(w_ate, w_att))
#> # A tibble: 21 × 5
#>    variable group_level method   metric estimate
#>    <chr>    <chr>       <chr>    <chr>     <dbl>
#>  1 age      0           observed ks      0.130  
#>  2 age      0           w_ate    ks      0.0293 
#>  3 age      0           w_att    ks      0.0362 
#>  4 age      0           observed smd     0.282  
#>  5 age      0           w_ate    smd     0.00585
#>  6 age      0           w_att    smd     0.0122 
#>  7 age      0           observed vr      1.07   
#>  8 age      0           w_ate    vr      1.01   
#>  9 age      0           w_att    vr      1.01   
#> 10 wt71     0           observed ks      0.0700 
#> # ℹ 11 more rows

# With specific metrics only
check_balance(nhefs_weights, c(age, wt71), qsmk, .metrics = c("smd", "energy"))
#> # A tibble: 3 × 5
#>   variable group_level method   metric estimate
#>   <chr>    <chr>       <chr>    <chr>     <dbl>
#> 1 age      0           observed smd      0.282 
#> 2 wt71     0           observed smd      0.133 
#> 3 NA       NA          observed energy   0.0503

# Categorical exposure
check_balance(nhefs_weights, c(age, wt71), alcoholfreq_cat,
              .weights = c(w_cat_ate, w_cat_att_2_3wk))
#> # A tibble: 75 × 5
#>    variable group_level    method          metric estimate
#>    <chr>    <chr>          <chr>           <chr>     <dbl>
#>  1 age      lt_12_per_year observed        ks       0.196 
#>  2 age      1_4_per_month  observed        ks       0.291 
#>  3 age      2_3_per_week   observed        ks       0.264 
#>  4 age      daily          observed        ks       0.177 
#>  5 age      lt_12_per_year w_cat_ate       ks       0.0696
#>  6 age      1_4_per_month  w_cat_ate       ks       0.0455
#>  7 age      2_3_per_week   w_cat_ate       ks       0.0503
#>  8 age      daily          w_cat_ate       ks       0.0476
#>  9 age      lt_12_per_year w_cat_att_2_3wk ks       0.164 
#> 10 age      1_4_per_month  w_cat_att_2_3wk ks       0.137 
#> # ℹ 65 more rows

# Specify reference group for categorical exposure
check_balance(nhefs_weights, c(age, wt71, sex), alcoholfreq_cat,
              .reference_level = "daily", .metrics = c("smd", "vr"))
#> # A tibble: 24 × 5
#>    variable group_level    method   metric estimate
#>    <chr>    <chr>          <chr>    <chr>     <dbl>
#>  1 age      none           observed smd     -0.334 
#>  2 age      lt_12_per_year observed smd      0.0725
#>  3 age      1_4_per_month  observed smd      0.329 
#>  4 age      2_3_per_week   observed smd     -0.265 
#>  5 age      none           observed vr       0.789 
#>  6 age      lt_12_per_year observed vr       1.00  
#>  7 age      1_4_per_month  observed vr       0.965 
#>  8 age      2_3_per_week   observed vr       1.02  
#>  9 sex      none           observed smd     -0.494 
#> 10 sex      lt_12_per_year observed smd     -0.826 
#> # ℹ 14 more rows

# Exclude observed results
check_balance(nhefs_weights, c(age, wt71), qsmk, .weights = w_ate,
              include_observed = FALSE)
#> # A tibble: 7 × 5
#>   variable group_level method metric estimate
#>   <chr>    <chr>       <chr>  <chr>     <dbl>
#> 1 age      0           w_ate  ks      0.0293 
#> 2 age      0           w_ate  smd     0.00585
#> 3 age      0           w_ate  vr      1.01   
#> 4 wt71     0           w_ate  ks      0.0358 
#> 5 wt71     0           w_ate  smd    -0.00903
#> 6 wt71     0           w_ate  vr      1.00   
#> 7 NA       NA          w_ate  energy  0.00217

# Use correlation for continuous exposure
check_balance(mtcars, c(mpg, hp), disp, .metrics = c("correlation", "energy"))
#> # A tibble: 3 × 5
#>   variable group_level method   metric      estimate
#>   <chr>    <chr>       <chr>    <chr>          <dbl>
#> 1 hp       disp        observed correlation    0.791
#> 2 mpg      disp        observed correlation   -0.848
#> 3 NA       NA          observed energy         0.882

# With dummy variables for categorical variables (default behavior)
check_balance(nhefs_weights, c(age, sex, race), qsmk)
#> # A tibble: 10 × 5
#>    variable group_level method   metric estimate
#>    <chr>    <chr>       <chr>    <chr>     <dbl>
#>  1 age      0           observed ks       0.130 
#>  2 age      0           observed smd      0.282 
#>  3 age      0           observed vr       1.07  
#>  4 race     0           observed ks       0.0568
#>  5 race     0           observed smd     -0.177 
#>  6 race     0           observed vr       0.652 
#>  7 sex      0           observed ks       0.0799
#>  8 sex      0           observed smd     -0.160 
#>  9 sex      0           observed vr       0.996 
#> 10 NA       NA          observed energy   0.0641

# Without dummy variables for categorical variables
check_balance(nhefs_weights, c(age, sex, race), qsmk, make_dummy_vars = FALSE)
#> # A tibble: 10 × 5
#>    variable group_level method   metric estimate
#>    <chr>    <chr>       <chr>    <chr>     <dbl>
#>  1 age      0           observed ks       0.130 
#>  2 age      0           observed smd      0.282 
#>  3 age      0           observed vr       1.07  
#>  4 race     0           observed ks      NA     
#>  5 race     0           observed smd     NA     
#>  6 race     0           observed vr      NA     
#>  7 sex      0           observed ks      NA     
#>  8 sex      0           observed smd     NA     
#>  9 sex      0           observed vr      NA     
#> 10 NA       NA          observed energy   0.0641
```

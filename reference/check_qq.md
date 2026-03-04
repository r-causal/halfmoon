# Check QQ Data for Multiple Weights

Calculate quantile-quantile data comparing the distribution of a
variable between treatment groups. This function computes the quantiles
for both groups and returns a tidy data frame suitable for plotting or
further analysis.

## Usage

``` r
check_qq(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  include_observed = TRUE,
  .reference_level = NULL,
  na.rm = FALSE
)
```

## Arguments

- .data:

  A data frame containing the variables.

- .var:

  Variable to compute quantiles for. Supports tidyselect syntax.

- .exposure:

  Column name of treatment/group variable. Supports tidyselect syntax.

- .weights:

  Optional weighting variable(s). Can be unquoted variable names
  (supports tidyselect syntax), a character vector, or NULL. Multiple
  weights can be provided to compare different weighting schemes.
  Default is NULL (unweighted).

- quantiles:

  Numeric vector of quantiles to compute. Default is
  `seq(0.01, 0.99, 0.01)` for 99 quantiles.

- include_observed:

  Logical. If using `.weights`, also compute observed (unweighted)
  quantiles? Defaults to TRUE.

- .reference_level:

  The reference treatment level to use for comparisons. If `NULL`
  (default), uses the last level for factors or the maximum value for
  numeric variables.

- na.rm:

  Logical; if TRUE, drop NA values before computation.

## Value

A tibble with class "halfmoon_qq" containing columns:

- method:

  Character. The weighting method ("observed" or weight variable name).

- quantile:

  Numeric. The quantile probability (0-1).

- exposed_quantiles:

  Numeric. The quantile value for the exposed group.

- unexposed_quantiles:

  Numeric. The quantile value for the unexposed group.

## Details

This function computes the data needed for quantile-quantile plots by
calculating corresponding quantiles from two distributions. The
computation uses the inverse of the empirical cumulative distribution
function (ECDF). For weighted data, it first computes the weighted ECDF
and then inverts it to obtain quantiles.

## See also

[`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md)
for single weight QQ data,
[`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
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
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
[`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md),
[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Basic QQ data (observed only)
check_qq(nhefs_weights, age, qsmk)
#> # A tibble: 99 × 4
#>    method   quantile exposed_quantiles unexposed_quantiles
#>    <fct>       <dbl>             <dbl>               <dbl>
#>  1 observed     0.01                25                25  
#>  2 observed     0.02                25                25  
#>  3 observed     0.03                26                25  
#>  4 observed     0.04                26                25.5
#>  5 observed     0.05                27                26  
#>  6 observed     0.06                27                26  
#>  7 observed     0.07                28                26  
#>  8 observed     0.08                28                27  
#>  9 observed     0.09                29                27  
#> 10 observed     0.1                 29                28  
#> # ℹ 89 more rows

# With weighting
check_qq(nhefs_weights, age, qsmk, .weights = w_ate)
#> # A tibble: 198 × 4
#>    method   quantile exposed_quantiles unexposed_quantiles
#>    <fct>       <dbl>             <dbl>               <dbl>
#>  1 observed     0.01                25                25  
#>  2 observed     0.02                25                25  
#>  3 observed     0.03                26                25  
#>  4 observed     0.04                26                25.5
#>  5 observed     0.05                27                26  
#>  6 observed     0.06                27                26  
#>  7 observed     0.07                28                26  
#>  8 observed     0.08                28                27  
#>  9 observed     0.09                29                27  
#> 10 observed     0.1                 29                28  
#> # ℹ 188 more rows

# Compare multiple weighting schemes
check_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))
#> # A tibble: 297 × 4
#>    method   quantile exposed_quantiles unexposed_quantiles
#>    <fct>       <dbl>             <dbl>               <dbl>
#>  1 observed     0.01                25                25  
#>  2 observed     0.02                25                25  
#>  3 observed     0.03                26                25  
#>  4 observed     0.04                26                25.5
#>  5 observed     0.05                27                26  
#>  6 observed     0.06                27                26  
#>  7 observed     0.07                28                26  
#>  8 observed     0.08                28                27  
#>  9 observed     0.09                29                27  
#> 10 observed     0.1                 29                28  
#> # ℹ 287 more rows
```

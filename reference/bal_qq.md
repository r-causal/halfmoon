# Compute QQ Data for Single Variable and Weight

Calculate quantile-quantile data comparing the distribution of a
variable between treatment groups for a single weighting scheme (or
unweighted). This function computes the quantiles for both groups and
returns a data frame suitable for plotting or further analysis.

## Usage

``` r
bal_qq(
  .data,
  .var,
  .exposure,
  .weights = NULL,
  quantiles = seq(0.01, 0.99, 0.01),
  .reference_level = NULL,
  na.rm = FALSE
)
```

## Arguments

- .data:

  A data frame containing the variables.

- .var:

  Variable to compute quantiles for (unquoted).

- .exposure:

  Column name of treatment/group variable (unquoted).

- .weights:

  Optional single weight variable (unquoted). If NULL, computes
  unweighted quantiles.

- quantiles:

  Numeric vector of quantiles to compute. Default is
  `seq(0.01, 0.99, 0.01)` for 99 quantiles.

- .reference_level:

  The reference group level for comparisons. Can be either a group level
  value or a numeric index. If `NULL` (default), uses the first level.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values in the input will
  produce `NA` in the output.

## Value

A tibble with columns:

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

When the distributions of a variable are similar between treatment
groups (indicating good balance), the QQ plot points will lie close to
the diagonal line y = x.

## See also

[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md)
for computing QQ data across multiple weights,
[`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
for visualization

Other balance functions:
[`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md),
[`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md),
[`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
[`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
[`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md),
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
# Unweighted QQ data
bal_qq(nhefs_weights, age, qsmk)
#> # A tibble: 99 × 3
#>    quantile exposed_quantiles unexposed_quantiles
#>       <dbl>             <dbl>               <dbl>
#>  1     0.01                25                25  
#>  2     0.02                25                25  
#>  3     0.03                26                25  
#>  4     0.04                26                25.5
#>  5     0.05                27                26  
#>  6     0.06                27                26  
#>  7     0.07                28                26  
#>  8     0.08                28                27  
#>  9     0.09                29                27  
#> 10     0.1                 29                28  
#> # ℹ 89 more rows

# Weighted QQ data
bal_qq(nhefs_weights, age, qsmk, .weights = w_ate)
#> # A tibble: 99 × 3
#>    quantile exposed_quantiles unexposed_quantiles
#>       <dbl>             <dbl>               <dbl>
#>  1     0.01              25                    25
#>  2     0.02              25                    25
#>  3     0.03              25.3                  25
#>  4     0.04              26                    26
#>  5     0.05              26                    26
#>  6     0.06              26                    26
#>  7     0.07              27                    27
#>  8     0.08              27                    27
#>  9     0.09              28                    27
#> 10     0.1               28                    28
#> # ℹ 89 more rows

# Custom quantiles
bal_qq(nhefs_weights, age, qsmk, .weights = w_ate,
       quantiles = seq(0.1, 0.9, 0.1))
#> # A tibble: 9 × 3
#>   quantile exposed_quantiles unexposed_quantiles
#>      <dbl>             <dbl>               <dbl>
#> 1      0.1              28                    28
#> 2      0.2              32                    31
#> 3      0.3              34.1                  35
#> 4      0.4              39                    39
#> 5      0.5              43                    43
#> 6      0.6              47                    47
#> 7      0.7              51                    50
#> 8      0.8              55                    54
#> 9      0.9              60                    60
```

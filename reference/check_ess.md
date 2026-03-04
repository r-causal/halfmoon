# Check Effective Sample Size

Computes the effective sample size (ESS) for one or more weighting
schemes, optionally stratified by treatment groups. ESS reflects how
many observations you would have if all were equally weighted.

## Usage

``` r
check_ess(
  .data,
  .weights = NULL,
  .exposure = NULL,
  include_observed = TRUE,
  n_tiles = 4,
  tile_labels = NULL
)
```

## Arguments

- .data:

  A data frame containing the variables to analyze.

- .weights:

  Optional weighting variables. Can be unquoted variable names, a
  character vector, or NULL. Multiple weights can be provided to compare
  different weighting schemes.

- .exposure:

  Optional exposure variable. When provided, ESS is calculated
  separately for each exposure level. For continuous variables, groups
  are created using quantiles.

- include_observed:

  Logical. If using `.weights`, also calculate observed (unweighted)
  metrics? Defaults to TRUE.

- n_tiles:

  For continuous `.exposure` variables, the number of quantile groups to
  create. Default is 4 (quartiles).

- tile_labels:

  Optional character vector of labels for the quantile groups when
  `.exposure` is continuous. If NULL, uses "Q1", "Q2", etc.

## Value

A tibble with columns:

- method:

  Character. The weighting method ("observed" or weight variable name).

- group:

  Character. The exposure level (if `.exposure` is provided).

- n:

  Integer. The number of observations in the group.

- ess:

  Numeric. The effective sample size.

- ess_pct:

  Numeric. ESS as a percentage of the actual sample size.

## Details

The effective sample size (ESS) is calculated using the classical
formula: \\ESS = (\sum w)^2 / \sum(w^2)\\.

When weights vary substantially, the ESS can be much smaller than the
actual number of observations, indicating that a few observations carry
disproportionately large weights.

When `.exposure` is provided, ESS is calculated separately for each
exposure level:

- For binary/categorical exposures: ESS is computed within each
  treatment level

- For continuous exposures: The variable is divided into quantiles
  (using
  [`dplyr::ntile()`](https://dplyr.tidyverse.org/reference/ntile.html))
  and ESS is computed within each quantile

The function returns results in a tidy format suitable for plotting or
further analysis.

## See also

[`ess()`](https://r-causal.github.io/halfmoon/reference/ess.md) for the
underlying ESS calculation,
[`plot_ess()`](https://r-causal.github.io/halfmoon/reference/plot_ess.md)
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
[`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
[`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md),
[`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
[`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

## Examples

``` r
# Overall ESS for different weighting schemes
check_ess(nhefs_weights, .weights = c(w_ate, w_att, w_atm))
#> # A tibble: 4 × 4
#>   method       n   ess ess_pct
#>   <chr>    <int> <dbl>   <dbl>
#> 1 observed  1566 1566    100  
#> 2 w_ate     1566 1013.    64.7
#> 3 w_atm     1566 1089.    69.5
#> 4 w_att     1566 1070.    68.3

# ESS by treatment group (binary exposure)
check_ess(nhefs_weights, .weights = c(w_ate, w_att), .exposure = qsmk)
#> # A tibble: 6 × 5
#>   method   group     n   ess ess_pct
#>   <chr>    <fct> <int> <dbl>   <dbl>
#> 1 observed 0      1163 1163    100  
#> 2 observed 1       403  403    100  
#> 3 w_ate    0      1163 1129.    97.0
#> 4 w_ate    1       403  326.    80.9
#> 5 w_att    0      1163  796.    68.4
#> 6 w_att    1       403  403    100  

# ESS by treatment group (categorical exposure)
check_ess(nhefs_weights, .weights = w_cat_ate, .exposure = alcoholfreq_cat)
#> # A tibble: 12 × 5
#>    method    group              n   ess ess_pct
#>    <chr>     <fct>          <int> <dbl>   <dbl>
#>  1 observed  none             195 195     100  
#>  2 observed  lt_12_per_year   328 328     100  
#>  3 observed  1_4_per_month    494 494     100  
#>  4 observed  2_3_per_week     219 219     100  
#>  5 observed  daily            325 325     100  
#>  6 observed  NA                 5   5     100  
#>  7 w_cat_ate none             195  98.4    50.4
#>  8 w_cat_ate lt_12_per_year   328 251.     76.7
#>  9 w_cat_ate 1_4_per_month    494 452.     91.6
#> 10 w_cat_ate 2_3_per_week     219 181.     82.8
#> 11 w_cat_ate daily            325 246.     75.7
#> 12 w_cat_ate NA                 5 NaN     NaN  

# ESS by quartiles of a continuous variable
check_ess(nhefs_weights, .weights = w_ate, .exposure = age, n_tiles = 4)
#> # A tibble: 8 × 5
#>   method   group     n   ess ess_pct
#>   <chr>    <fct> <int> <dbl>   <dbl>
#> 1 observed Q1      392  392    100  
#> 2 observed Q2      392  392    100  
#> 3 observed Q3      391  391    100  
#> 4 observed Q4      391  391    100  
#> 5 w_ate    Q1      392  212.    54.1
#> 6 w_ate    Q2      392  230.    58.6
#> 7 w_ate    Q3      391  281.    71.9
#> 8 w_ate    Q4      391  317.    81.0

# Custom labels for continuous groups
check_ess(nhefs_weights, .weights = w_ate, .exposure = age,
          n_tiles = 3, tile_labels = c("Young", "Middle", "Older"))
#> # A tibble: 6 × 5
#>   method   group      n   ess ess_pct
#>   <chr>    <fct>  <int> <dbl>   <dbl>
#> 1 observed Young    522  522    100  
#> 2 observed Middle   522  522    100  
#> 3 observed Older    522  522    100  
#> 4 w_ate    Young    522  293.    56.1
#> 5 w_ate    Middle   522  326.    62.4
#> 6 w_ate    Older    522  417.    80.0

# Without unweighted comparison
check_ess(nhefs_weights, .weights = w_ate, .exposure = qsmk,
          include_observed = FALSE)
#> # A tibble: 2 × 5
#>   method group     n   ess ess_pct
#>   <chr>  <fct> <int> <dbl>   <dbl>
#> 1 w_ate  0      1163 1129.    97.0
#> 2 w_ate  1       403  326.    80.9
```

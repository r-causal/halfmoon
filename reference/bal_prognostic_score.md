# Compute Prognostic Scores for Balance Assessment

Calculates prognostic scores by fitting an outcome model on the control
group and generating predictions for all observations. Prognostic scores
represent the expected outcome under control conditions and are useful
for assessing whether balance on treatment predictors ensures balance on
outcome-relevant variables.

## Usage

``` r
bal_prognostic_score(
  .data,
  outcome = NULL,
  .covariates = everything(),
  .exposure,
  formula = NULL,
  .reference_level = NULL,
  family = gaussian(),
  .weights = NULL,
  na.rm = FALSE,
  ...
)
```

## Arguments

- .data:

  A data frame containing the variables for analysis

- outcome:

  The outcome variable. Can be specified as a bare name or character
  string. Ignored if `formula` is provided.

- .covariates:

  Variables to include in the outcome model. Defaults to
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
  which includes all variables except the outcome and treatment.
  Supports tidyselect syntax. Ignored if `formula` is provided.

- .exposure:

  The treatment variable. Can be specified as a bare name or character
  string.

- formula:

  Optional formula for the outcome model. If provided, `outcome` and
  `.covariates` arguments are ignored. The formula should not include
  the treatment variable.

- .reference_level:

  The level of treatment that represents the control group. If NULL
  (default), the first level is used as control.

- family:

  A family object for the GLM. Defaults to
  [`gaussian()`](https://rdrr.io/r/stats/family.html) for continuous
  outcomes. Use [`binomial()`](https://rdrr.io/r/stats/family.html) for
  binary outcomes.

- .weights:

  Optional weights for the outcome model. Can be a numeric vector or
  bare name of a variable in `.data`.

- na.rm:

  Logical. Should missing values be removed? Defaults to FALSE.

- ...:

  Additional arguments passed to
  [`glm()`](https://rdrr.io/r/stats/glm.html).

## Value

A numeric vector of prognostic scores with the same length as the number
of rows in `.data` (after NA removal if `na.rm = TRUE`).

## Details

The prognostic score method, introduced by Stuart et al. (2013),
provides a way to assess balance that focuses on variables predictive of
the outcome rather than just the treatment assignment. The procedure:

1.  Fits an outcome model using only control group observations

2.  Generates predictions (prognostic scores) for all observations

3.  Returns these scores for balance assessment

This approach is particularly useful when:

- The outcome model includes non-linearities or interactions

- You want to ensure balance on outcome-relevant variables

- Traditional propensity score balance checks may miss important
  imbalances

The returned prognostic scores can be used with existing balance
functions like
[`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
[`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
or
[`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
to assess balance between treatment groups.

## References

Stuart EA, Lee BK, Leacy FP. Prognostic score-based balance measures can
be a useful diagnostic for propensity score methods in comparative
effectiveness research. J Clin Epidemiol. 2013;66(8):S84-S90.

## Examples

``` r
# Using tidyselect interface
prog_scores <- bal_prognostic_score(
  nhefs_weights,
  outcome = wt82_71,
  .exposure = qsmk,
  .covariates = c(age, sex, race, wt71)
)

# Using formula interface
prog_scores_formula <- bal_prognostic_score(
  nhefs_weights,
  .exposure = qsmk,
  formula = wt82_71 ~ age + sex + race + wt71 + I(age^2)
)

# Add to data and check balance
nhefs_with_prog <- nhefs_weights
nhefs_with_prog$prog_score <- prog_scores
check_balance(nhefs_with_prog, prog_score, qsmk, .weights = w_ate)
#> # A tibble: 8 × 5
#>   variable   group_level method   metric  estimate
#>   <chr>      <chr>       <chr>    <chr>      <dbl>
#> 1 prog_score 0           observed ks      0.124   
#> 2 prog_score 0           w_ate    ks      0.0316  
#> 3 prog_score 0           observed smd    -0.267   
#> 4 prog_score 0           w_ate    smd     0.00305 
#> 5 prog_score 0           observed vr      1.06    
#> 6 prog_score 0           w_ate    vr      1.01    
#> 7 NA         NA          observed energy  0.0442  
#> 8 NA         NA          w_ate    energy  0.000954

# For binary outcome
prog_scores_binary <- bal_prognostic_score(
  nhefs_weights,
  outcome = death,
  .exposure = qsmk,
  family = binomial()
)
#> Warning: prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
```

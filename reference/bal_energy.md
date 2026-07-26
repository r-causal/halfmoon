# Balance Energy Distance

Computes the energy distance as a multivariate measure of covariate
balance between groups. Energy distance captures the similarity between
distributions across the entire joint distribution of .covariates,
making it more comprehensive than univariate balance measures.

## Usage

``` r
bal_energy(
  .covariates,
  .exposure,
  .weights = NULL,
  estimand = NULL,
  .focal_level = NULL,
  use_improved = TRUE,
  standardized = TRUE,
  criterion = c("dependence", "dcor"),
  dimension_adj = TRUE,
  na.rm = FALSE
)
```

## Arguments

- .covariates:

  A data frame or matrix containing the .covariates to compare.

- .exposure:

  A vector (factor or numeric) indicating group membership. For binary
  and multi-category treatments, must have 2+ unique levels. For
  continuous treatments, should be numeric.

- .weights:

  An optional numeric vector of weights. If provided, must have the same
  length as rows in `.covariates`. All weights must be non-negative.

- estimand:

  Character string specifying the estimand. Options are:

  - NULL (default): Pure between-group energy distance comparing
    distributions

  - "ATE": Energy distance weighted to reflect balance for estimating
    average treatment effects across the entire population

  - "ATT": Energy distance weighted to reflect balance for the treated
    .exposure, measuring how well controls match the treated
    distribution

  - "ATC": Energy distance weighted to reflect balance for the control
    .exposure, measuring how well treated units match the control
    distribution For continuous treatments, only NULL is supported.

- .focal_level:

  The treatment level for ATT/ATC. If `NULL` (default), automatically
  determined based on estimand.

- use_improved:

  Logical. Use improved energy distance for ATE? Default is TRUE. When
  TRUE, adds pairwise treatment comparisons for better group separation.

- standardized:

  Logical. Only used when `criterion = "dcor"` for a continuous
  exposure, where `TRUE` (default) returns the standardized distance
  correlation and `FALSE` returns the unstandardized square-root
  distance covariance. Ignored for `criterion = "dependence"`.

- criterion:

  Character string selecting the continuous-exposure statistic.
  `"dependence"` (default) returns the weighted dependence distance
  \\D(w)\\ of Huling, Greifer, and Chen (2023); `"dcor"` returns
  cobalt's `distance.cor` balance statistic. Binary and multi-category
  exposures always use the energy distance and accept only the default;
  supplying `"dcor"` with a non-continuous exposure is an error.

- dimension_adj:

  Logical. For `criterion = "dependence"`, weight the two marginal
  energy terms by a dimension adjustment (`TRUE`, default) so that the
  covariate term and the treatment term contribute comparably regardless
  of the number of covariates, or split them evenly (`FALSE`). Ignored
  when `criterion = "dcor"`. Binary and multi-category exposures accept
  only the default; `dimension_adj = FALSE` with a non-continuous
  exposure is an error.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values result in an error
  (energy distance cannot be computed with missing data).

## Value

A numeric value. For binary and multi-category exposures, the energy
distance between groups, where lower values indicate better balance and
0 indicates identical distributions. For a continuous exposure with
`criterion = "dependence"`, the weighted dependence distance \\D(w)\\,
which is 0 if and only if the weighted joint distribution of the
exposure and covariates factorizes into their unweighted marginals;
smaller values indicate better balance, and the statistic is not bounded
above by 1. For a continuous exposure with `criterion = "dcor"`,
cobalt's `distance.cor` balance statistic.

## Details

Energy distance is based on the energy statistics framework (Székely &
Rizzo, 2004) and implemented following Huling & Mak (2024) and Huling et
al. (2024). The calculation uses a quadratic form: \\w^T P w + q^T w +
k\\, where the components depend on the estimand.

For binary variables in the .covariates, variance is calculated as
p(1-p) rather than sample variance to prevent over-weighting.

For a continuous exposure, `criterion = "dependence"` returns the
weighted dependence distance \\D(w)\\ of Huling, Greifer, and Chen
(2023, eq. 7), \$\$D(w) = \mathrm{dCov}\_w(A, X) + E_w(A) + E_w(X),\$\$
the weighted distance covariance between the exposure \\A\\ and the
covariates \\X\\ plus dimension-adjusted energy distances \\E_w(A)\\ and
\\E_w(X)\\ between the weighted and unweighted marginals of the exposure
and of the covariates. All three terms are computed on unscaled
Euclidean distance matrices with the weights normalized to mean 1. The
weighted distance covariance alone has a false converse, since weights
can shrink it while distorting the marginals, so by their Theorem 3.2 it
is the full \\D(w)\\, not the distance covariance, that is 0 exactly
when the weights make the exposure and covariates independent without
distorting their unweighted marginal distributions. The `dimension_adj`
argument controls the relative weighting of the two marginal energy
terms.

`criterion = "dcor"` instead returns cobalt's `distance.cor` balance
statistic, a weighted-variance-scaled distance correlation (or, with
`standardized = FALSE`, the corresponding square-root distance
covariance). This is a descriptive balance summary rather than a measure
of weighted dependence.

## References

Huling, J. D., & Mak, S. (2024). Energy Balancing of Covariate
Distributions. Journal of Causal Inference, 12(1) . Huling, J. D.,
Greifer, N., & Chen, G. (2023). Independence weights for causal
inference with continuous treatments. *Journal of the American
Statistical Association*, 0(ja), 1–25.
[doi:10.1080/01621459.2023.2213485](https://doi.org/10.1080/01621459.2023.2213485)

Székely, G. J., & Rizzo, M. L. (2004). Testing for equal distributions
in high dimension. InterStat, 5.

## Examples

``` r
# Binary treatment
bal_energy(
  .covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
  .exposure = nhefs_weights$qsmk
)
#> [1] 0.05159867

# With weights
bal_energy(
  .covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
  .exposure = nhefs_weights$qsmk,
  .weights = nhefs_weights$w_ate
)
#> [1] 0.002998391

# ATT estimand
bal_energy(
  .covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
  .exposure = nhefs_weights$qsmk,
  .weights = nhefs_weights$w_att,
  estimand = "ATT"
)
#> [1] 0.003318796
```

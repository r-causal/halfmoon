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

  Logical. For continuous treatments, return standardized distance
  correlation? Default is TRUE.

- na.rm:

  A logical value indicating whether to remove missing values before
  computation. If `FALSE` (default), missing values result in an error
  (energy distance cannot be computed with missing data).

## Value

A numeric value representing the energy distance between groups. Lower
values indicate better balance, with 0 indicating perfect balance
(identical distributions). For continuous treatments, returns the
distance correlation coefficient (0 = independence, 1 = perfect
dependence).

## Details

Energy distance is based on the energy statistics framework (Székely &
Rizzo, 2004) and implemented following Huling & Mak (2024) and Huling et
al. (2024). The calculation uses a quadratic form: \\w^T P w + q^T w +
k\\, where the components depend on the estimand.

For binary variables in the .covariates, variance is calculated as
p(1-p) rather than sample variance to prevent over-weighting.

For continuous treatments, the function uses distance correlation
instead of traditional energy distance, measuring independence between
treatment and .covariates.

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

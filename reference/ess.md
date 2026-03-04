# Calculate the Effective Sample Size (ESS)

This function computes the effective sample size (ESS) given a vector of
weights, using the classical \\(\sum w)^2 / \sum(w^2)\\ formula
(sometimes referred to as "Kish's effective sample size").

## Usage

``` r
ess(wts, na.rm = FALSE)
```

## Arguments

- wts:

  A numeric vector of weights (e.g., from survey or inverse-probability
  weighting).

- na.rm:

  Logical. Should missing values be removed? Default is FALSE.

## Value

A single numeric value representing the effective sample size.

## Details

The effective sample size (ESS) reflects how many observations you would
have if all were equally weighted. If the weights vary substantially,
the ESS can be much smaller than the actual number of observations.
Formally:

\$\$ \mathrm{ESS} = \frac{\left(\sum_i w_i\right)^2}{\sum_i w_i^2}. \$\$

**Diagnostic Value**:

- **Indicator of Weight Concentration**: A large discrepancy between ESS
  and the actual sample size indicates that a few observations carry
  disproportionately large weights, effectively reducing the usable
  information in the dataset.

- **Variance Inflation**: A small ESS signals that weighted estimates
  are more sensitive to a handful of observations, inflating the
  variance and standard errors.

- **Practical Guidance**: If ESS is much lower than the total sample
  size, it is advisable to investigate why some weights are extremely
  large or small. Techniques like weight trimming or stabilized weights
  might be employed to mitigate the issue

## Examples

``` r
# Suppose we have five observations with equal weights
wts1 <- rep(1.2, 5)
# returns 5, because all weights are equal
ess(wts1)
#> [1] 5

# If weights vary more, smaller than 5
wts2 <- c(0.5, 2, 2, 0.1, 0.8)
ess(wts2)
#> [1] 3.276404
```

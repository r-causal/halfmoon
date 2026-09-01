# Compute weighted quantiles

Calculate quantiles of a numeric vector with associated weights, using
the weighted generalization of the default definition in
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

## Usage

``` r
weighted_quantile(values, quantiles, .weights, na.rm = FALSE)
```

## Arguments

- values:

  Numeric vector of values to compute quantiles for.

- quantiles:

  Numeric vector of probabilities with values between 0 and 1.

- .weights:

  Numeric vector of non-negative weights, same length as `values`.

- na.rm:

  Logical. If `FALSE` (default), a missing value or a missing weight
  makes every quantile missing. If `TRUE`, an observation with either
  one missing is dropped and the quantiles are computed from the rest.

## Value

Numeric vector of weighted quantiles corresponding to the requested
probabilities. Fewer than two observations with a positive weight leave
the quantiles undefined, and the result is `NA_real_`, as does
`na.rm = FALSE` with a missing value or a missing weight.

## Details

[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) with
`type = 7`, its default, places the value of rank `i` among `n` values
at probability `(i - 1) / (n - 1)` and interpolates linearly between
them. `weighted_quantile()` generalizes those positions: each distinct
value spans the probabilities implied by the total weight of the
observations that take it, shortened at each end by half the average
weight of those observations, and the positions are rescaled so that the
smallest value sits at 0 and the largest at 1.

The definition has the properties you would expect of one:

- With a constant positive weight, the result is identical to
  `stats::quantile(values, quantiles)`, ties included.

- Multiplying every weight by a constant leaves the result unchanged.

- The result does not depend on the order of `values`.

- The result is monotone in `quantiles`.

Observations with zero weight contribute nothing and are excluded. This
matters for matching weights, which are 0 or 1: the quantiles of a
matched sample are the quantiles of the matched observations alone.

## Examples

``` r
# Equal weights (same as regular quantiles)
weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10))
#> [1] 3.25 5.50 7.75
quantile(1:10, c(0.25, 0.5, 0.75))
#>  25%  50%  75% 
#> 3.25 5.50 7.75 

# Weighted towards higher values
weighted_quantile(1:10, c(0.25, 0.5, 0.75), 1:10)
#> [1] 5.068182 7.100000 8.661765
```

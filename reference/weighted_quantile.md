# Compute weighted quantiles

Calculate quantiles of a numeric vector with associated weights. This
function sorts the values and computes weighted cumulative distribution
before interpolating the requested quantiles.

## Usage

``` r
weighted_quantile(values, quantiles, .weights)
```

## Arguments

- values:

  Numeric vector of values to compute quantiles for.

- quantiles:

  Numeric vector of probabilities with values between 0 and 1.

- .weights:

  Numeric vector of non-negative weights, same length as `values`.

## Value

Numeric vector of weighted quantiles corresponding to the requested
probabilities.

## Examples

``` r
# Equal weights (same as regular quantiles)
weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10))
#> [1] 2.5 5.0 7.5

# Weighted towards higher values
weighted_quantile(1:10, c(0.25, 0.5, 0.75), 1:10)
#> [1] 4.750000 6.928571 8.583333
```

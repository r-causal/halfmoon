# Calculate confidence interval using prop.test

Wrapper around prop.test that handles errors and returns consistent
output

## Usage

``` r
calculate_prop_ci(x, n, conf_level = 0.95)
```

## Arguments

- x:

  Number of successes

- n:

  Number of trials

- conf_level:

  Confidence level (default 0.95)

## Value

Named list with lower and upper bounds, or NA values on error

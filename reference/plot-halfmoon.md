# Plot Methods for halfmoon Objects

These methods provide standard plot generation for halfmoon data
objects. They create the plot using autoplot() and then print it.

## Usage

``` r
# S3 method for class 'halfmoon_balance'
plot(x, ...)

# S3 method for class 'halfmoon_ess'
plot(x, ...)

# S3 method for class 'halfmoon_calibration'
plot(x, ...)

# S3 method for class 'halfmoon_roc'
plot(x, ...)

# S3 method for class 'halfmoon_auc'
plot(x, ...)

# S3 method for class 'halfmoon_qq'
plot(x, ...)
```

## Arguments

- x:

  A halfmoon data object with appropriate class

- ...:

  Additional arguments passed to autoplot()

## Value

Invisibly returns the ggplot2 object after printing

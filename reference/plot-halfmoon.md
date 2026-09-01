# Plot Methods for halfmoon Objects

These methods provide standard plot generation for halfmoon data
objects. Each method builds the plot with
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and returns it. The plot is drawn when the returned object is printed,
which happens automatically at the top level of an interactive session.

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

A ggplot2 object

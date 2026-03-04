# Autoplot Methods for halfmoon Objects

These methods provide automatic plot generation for halfmoon data
objects using ggplot2's autoplot interface. Each method dispatches to
the appropriate plot\_\*() function as follows:

## Usage

``` r
# S3 method for class 'halfmoon_balance'
autoplot(object, ...)

# S3 method for class 'halfmoon_ess'
autoplot(object, ...)

# S3 method for class 'halfmoon_calibration'
autoplot(object, ...)

# S3 method for class 'halfmoon_roc'
autoplot(object, ...)

# S3 method for class 'halfmoon_auc'
autoplot(object, ...)

# S3 method for class 'halfmoon_qq'
autoplot(object, ...)
```

## Arguments

- object:

  A halfmoon data object with appropriate class

- ...:

  Additional arguments passed to the underlying plot\_\*() function

## Value

A ggplot2 object

## Details

- `autoplot.halfmoon_balance` calls
  [`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)

- `autoplot.halfmoon_ess` calls
  [`plot_ess()`](https://r-causal.github.io/halfmoon/reference/plot_ess.md)

- `autoplot.halfmoon_calibration` calls
  [`plot_model_calibration()`](https://r-causal.github.io/halfmoon/reference/plot_model_calibration.md)

- `autoplot.halfmoon_roc` calls
  [`plot_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/plot_model_roc_curve.md)

- `autoplot.halfmoon_auc` calls
  [`plot_model_auc()`](https://r-causal.github.io/halfmoon/reference/plot_model_auc.md)

- `autoplot.halfmoon_qq` calls
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)

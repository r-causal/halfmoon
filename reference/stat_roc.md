# ROC Curve Stat

Statistical transformation for ROC curves.

## Usage

``` r
stat_roc(
  mapping = NULL,
  data = NULL,
  geom = "path",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  .focal_level = NULL,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot.

- data:

  The data to be displayed in this layer. If `NULL`, the default, the
  data is inherited from the plot data as specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- geom:

  Geometric object to use. Default is "path".

- position:

  Position adjustment.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  Logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them.

- .focal_level:

  The level of the outcome variable to consider as the treatment/event.
  If `NULL` (default), uses the last level for factors or the maximum
  value for numeric variables.

- ...:

  Other arguments passed on to layer().

## Value

A ggplot2 layer.

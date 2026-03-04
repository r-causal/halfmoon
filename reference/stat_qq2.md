# QQ2 Plot Stat

Statistical transformation for QQ plots.

## Usage

``` r
stat_qq2(
  mapping = NULL,
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = TRUE,
  show.legend = NA,
  inherit.aes = TRUE,
  quantiles = seq(0.01, 0.99, 0.01),
  .reference_level = NULL,
  include_observed = FALSE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings.

- data:

  Data frame.

- geom:

  Geometric object to use. Default is "point".

- position:

  Position adjustment.

- na.rm:

  Remove missing values? Default TRUE.

- show.legend:

  Show legend? Default NA.

- inherit.aes:

  Inherit aesthetics? Default TRUE.

- quantiles:

  Numeric vector of quantiles to compute.

- .reference_level:

  The reference treatment level to use for comparisons.

- include_observed:

  For compatibility with qq(). When weights are present, this determines
  if an additional "observed" group is added. Default FALSE for stat_qq2
  to avoid duplication when using facets/colors.

- ...:

  Additional arguments.

## Value

A ggplot2 layer.

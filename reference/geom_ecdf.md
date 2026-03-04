# Calculate weighted and unweighted empirical cumulative distributions

The empirical cumulative distribution function (ECDF) provides an
alternative visualization of distribution. `geom_ecdf()` is similar to
[`ggplot2::stat_ecdf()`](https://ggplot2.tidyverse.org/reference/stat_ecdf.html)
but it can also calculate weighted ECDFs.

## Usage

``` r
geom_ecdf(
  mapping = NULL,
  data = NULL,
  geom = "step",
  position = "identity",
  ...,
  n = NULL,
  pad = TRUE,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- n:

  if NULL, do not interpolate. If not NULL, this is the number of points
  to interpolate with.

- pad:

  If `TRUE`, pad the ecdf with additional points (-Inf, 0) and (Inf, 1)

- na.rm:

  If `FALSE` (the default), removes missing values with a warning. If
  `TRUE` silently removes missing values.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

a geom

## Details

ECDF plots show the cumulative distribution function \\F(x) = P(X \leq
x)\\, displaying what proportion of observations fall below each value.
When comparing treatment groups, overlapping ECDF curves indicate
similar distributions and thus good balance.

ECDF plots are closely related to quantile-quantile (QQ) plots (see
[`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)).
While ECDF plots show \\F(x)\\ for each group, QQ plots show the inverse
relationship by plotting \\F_1^{-1}(p)\\ vs \\F_2^{-1}(p)\\. Both
visualize the same distributional information:

- ECDF plots: Compare cumulative probabilities at each value

- QQ plots: Compare values at each quantile

Choose ECDF plots when you want to see the full cumulative distribution
or when comparing multiple groups simultaneously. Choose QQ plots when
you want to directly compare two groups with an easy-to-interpret
45-degree reference line.

## Aesthetics

In addition to the aesthetics for
[`ggplot2::stat_ecdf()`](https://ggplot2.tidyverse.org/reference/stat_ecdf.html),
`geom_ecdf()` also accepts:

- weights

## See also

- [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  for an alternative visualization using quantile-quantile plots

- [`ggplot2::stat_ecdf()`](https://ggplot2.tidyverse.org/reference/stat_ecdf.html)
  for the unweighted version

Other ggplot2 functions:
[`geom_calibration()`](https://r-causal.github.io/halfmoon/reference/geom_calibration.md),
[`geom_mirror_density()`](https://r-causal.github.io/halfmoon/reference/geom_mirror_density.md),
[`geom_mirror_histogram()`](https://r-causal.github.io/halfmoon/reference/geom_mirror_histogram.md),
[`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md),
[`geom_roc()`](https://r-causal.github.io/halfmoon/reference/geom_roc.md)

## Examples

``` r
library(ggplot2)

ggplot(
  nhefs_weights,
  aes(x = smokeyrs, color = qsmk)
) +
  geom_ecdf(aes(weights = w_ato)) +
  xlab("Smoking Years") +
  ylab("Proportion <= x")

```

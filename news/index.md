# Changelog

## halfmoon (development version)

- [`plot_model_calibration()`](https://r-causal.github.io/halfmoon/reference/plot_model_calibration.md)
  no longer applies
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  to a `halfmoon_calibration` object. Both methods now leave the theme
  to the user, as
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  and the other halfmoon plotting functions do.

- [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md)
  and
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md)
  read the column a renaming selection points at and label the method
  with the new name, so `.weights = c(ate = w_ate)` works as it does in
  [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md).
  It previously looked for a column named after the method and failed to
  find it.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  raises `halfmoon_empty_error` naming `.data` when the data has no rows
  or no columns, rather than failing inside the covariate selection with
  an error about a column that does not exist.

- [`geom_ecdf()`](https://r-causal.github.io/halfmoon/reference/geom_ecdf.md)
  supports a flipped orientation. Mapping the variable to `y`, or
  passing `orientation = "y"`, computes the same weighted curve and
  draws it across the panel. It previously ignored the request and drew
  the curve as if the variable had been mapped to `x`.

- [`geom_ecdf()`](https://r-causal.github.io/halfmoon/reference/geom_ecdf.md)
  raises `halfmoon_range_error` for a negative weight, which made the
  cumulative distribution non-monotone, consistent with the weight
  validation everywhere else in the package.

- [`geom_roc()`](https://r-causal.github.io/halfmoon/reference/geom_roc.md)
  emits a `halfmoon_data_warning` when the exposure has no observed
  levels, rather than drawing an empty layer without comment.

- [`geom_mirror_histogram()`](https://r-causal.github.io/halfmoon/reference/geom_mirror_histogram.md)
  and
  [`geom_mirror_density()`](https://r-causal.github.io/halfmoon/reference/geom_mirror_density.md)
  name the geom and the observed group count when a panel holds three or
  more groups. This is still an error, deliberately: a mirrored plot
  draws one group above the axis and one below, so there is no partial
  plot to fall back on.

- [`weighted_quantile()`](https://r-causal.github.io/halfmoon/reference/weighted_quantile.md)
  gains `na.rm`. With the default `FALSE` a missing value or a missing
  weight now makes every quantile `NA_real_`, matching the policy the
  `bal_*()` functions follow; `na.rm = TRUE` drops the observation and
  computes the quantiles from the rest, which is what the function
  always did.
  [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md),
  and
  [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  apply their own missing-value policy before they call it, so their
  results are unchanged.

- [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  no longer applies
  [`ggplot2::theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  to a `halfmoon_qq` object. Both methods now leave the theme to the
  user, as the other halfmoon plotting functions do.

- [`plot_model_calibration()`](https://r-causal.github.io/halfmoon/reference/plot_model_calibration.md)
  gains a documented `binning_method` argument, which was previously
  reachable only through `...`.

- [`plot_model_calibration()`](https://r-causal.github.io/halfmoon/reference/plot_model_calibration.md)
  raises `halfmoon_type_error` for a `glm` or `lm` whose response does
  not take exactly two observed values. A calibration curve reads the
  response as the event indicator, so a model of anything else produced
  a curve with no meaning.

- [`bal_prognostic_score()`](https://r-causal.github.io/halfmoon/reference/bal_prognostic_score.md)
  raises `halfmoon_group_error`, rather than `halfmoon_reference_error`,
  when no control observations are left to fit the prognostic model on,
  which `na.rm = TRUE` can cause by dropping them all.

- [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  and
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  raise `halfmoon_type_error` naming a selected weight column that does
  not hold weights. They previously warned and left that method out of
  the result, which reported fewer methods than the call asked for.

- `bal_energy(criterion = "dcor")` lets the weights enter only the
  quadratic form that evaluates the dependence. The variances the
  distances are scaled by and the denominator that standardizes them are
  properties of the sample and are now computed unweighted. Weighted
  values change, and they now match
  `cobalt::bal.compute(cobalt::bal.init(x, treat, stat = "distance.cor"), weights = w)`
  rather than the `s.weights` initialization, which reweighted the scale
  along with the sample. The unweighted value is unchanged.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
  [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  and
  [`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
  refuse a weight method named `"observed"`, whether the column carries
  that name or the selection renames one to it, with
  `halfmoon_arg_error`. That label names the unweighted rows, so the
  selection previously produced a duplicate set of unweighted results
  instead of the weighted ones.

- [`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
  validates every selected weight column before it summarizes any of
  them, so a non-numeric column raises `halfmoon_type_error` naming the
  column. It also reads the column a renaming selection points at, where
  it previously looked for a column named after the method.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  raises `halfmoon_arg_error` when `.focal_level` is supplied with an
  `estimand` other than `"ATT"` or `"ATC"`. The focal level had no
  target to name there and was silently ignored.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  raises `halfmoon_arg_error` for an `exposure_type` that is not one of
  its documented values, matching every other option argument in the
  package.

- [`weighted_quantile()`](https://r-causal.github.io/halfmoon/reference/weighted_quantile.md)
  computes the weighted generalization of the definition
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) uses by
  default, `type = 7`. A constant positive weight now reproduces
  `stats::quantile(values, quantiles)` exactly, the result is invariant
  to the order of `values` and to the scale of the weights, and it is
  monotone in `quantiles`. It previously inverted the weighted empirical
  cumulative distribution function, which returned lower values than
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) did for
  the same data. Every weighted QQ value changes, and the weighted and
  observed quantiles in
  [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md),
  and
  [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  are now computed the same way.

- [`weighted_quantile()`](https://r-causal.github.io/halfmoon/reference/weighted_quantile.md)
  excludes observations with zero weight instead of averaging them into
  the neighboring quantiles. Matching weights are 0 or 1, so the
  quantiles of a matched sample are now the quantiles of the matched
  observations, where the unmatched observations previously pulled them
  toward the whole sample. Fewer than two observations with a positive
  weight leave the quantiles undefined and return `NA_real_`, where two
  observations with one zero weight previously raised an error from
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html).

- [`weighted_quantile()`](https://r-causal.github.io/halfmoon/reference/weighted_quantile.md)
  validates its arguments. A probability outside `[0, 1]` or a negative
  weight raises `halfmoon_range_error`, a non-numeric argument raises
  `halfmoon_type_error`, and weights whose length does not match
  `values` raise `halfmoon_length_error`. Each previously produced a
  quiet wrong answer or an error about interpolation.

- [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md),
  and
  [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  resolve `.reference_level` the way the rest of the package does: it
  names the reference group, which is the unexposed group, and defaults
  to the first observed level. A level can be given by value or by
  position. The default output is unchanged, since the exposed group is
  still the level that is not the reference, but an explicit
  `.reference_level` now names the opposite group from before.

- [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  and
  [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  put the reference (unexposed) group on the x axis and the exposed
  group on the y axis.
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  on a data frame previously transposed the plot that
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  on a
  [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md)
  result drew. Both methods now use
  [`coord_equal()`](https://ggplot2.tidyverse.org/reference/coord_fixed.html).

- [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  and
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  count only the observed levels of the exposure. A factor that declares
  a level no observation takes is valid binary input, and an exposure
  with one observed level raises `halfmoon_group_error` rather than
  returning a column of missing values.

- [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  and
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  treat missing weights the way they treat a missing variable or
  exposure: with `na.rm = FALSE` they raise `halfmoon_na_error` naming
  the weight column, and with `na.rm = TRUE` they drop the rows. Missing
  weights were previously dropped without comment under either setting.
  [`plot_qq()`](https://r-causal.github.io/halfmoon/reference/plot_qq.md)
  also raises `halfmoon_na_error` for a missing exposure, which is new:
  it previously checked `.var` alone and dropped rows with a missing
  exposure without comment.

- [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  requires the `treatment` aesthetic to have exactly two observed levels
  and raises `halfmoon_group_error` otherwise. A treatment with three or
  more levels previously pooled every level other than the reference
  into a single group and drew that comparison without comment.

- [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  drops a group that holds a single treatment level, with a warning
  naming the group’s observed level, and draws the rest of the panel.
  Such a group previously discarded every curve in the panel.

- [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  keeps the weighting when some weights are missing. A single missing
  weight previously discarded the weights of every observation and drew
  the unweighted QQ plot.

- [`geom_qq2()`](https://r-causal.github.io/halfmoon/reference/geom_qq2.md)
  draws one curve per group when `group` is mapped explicitly. The
  groups were previously pooled into a single curve, which counted each
  observation once per group.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  resolves `.reference_level` once for the whole call and uses the
  result for every metric and every label. It previously resolved the
  argument one way for the standardized mean difference and another way
  for the variance ratio and the Kolmogorov-Smirnov statistic, so a
  single table could compare against two different reference groups and
  label the rows with the wrong one. On a 0/1 exposure the default now
  references the level `0` for every metric, where the standardized mean
  difference previously referenced the level `1`: those rows change sign
  relative to the previous output, and the `group_level` column reports
  `1` rather than `0`. The default is now `NULL`, documented as the
  first observed level, and a value that matches a level is taken as
  that level, so `.reference_level = 0` on a 0/1 exposure means the
  level `0` for every metric.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  validates `.reference_level` before computing anything. A value that
  names no group raises `halfmoon_reference_error` and an index out of
  range raises `halfmoon_range_error`, where both previously produced
  rows of missing values without comment for the variance ratio and the
  Kolmogorov-Smirnov statistic.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  warns once, naming the affected metrics and variables, when it reports
  a combination it could not compute as `NA`. Missing values that
  `na.rm = FALSE` asks it to keep are not such a combination and stay
  silent.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  honors a renamed selection. `.weights = c(myw = w_ate)` now weights by
  `w_ate` and reports the method as `myw`, where it previously reported
  unweighted estimates under the new name, and a renamed `.vars`
  selection now reports the covariate under its new name instead of
  failing or returning `NA`.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  drops the grouping of a grouped data frame instead of adding the
  grouping variables to `.vars` and repairing the duplicated names.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  reads a logical covariate as a 0/1 indicator, where every metric
  previously reported `NA` for it.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  labels the comparison groups of a categorical exposure by removing the
  reference level from the names the balance functions return, rather
  than by splitting those names on `_vs_`. A reference level that itself
  contains `_vs_` no longer truncates the labels. A metric that fails
  for a categorical exposure now reports one missing row per comparison
  level instead of a single row labeled with one of them.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  forms the distance matrix behind the energy metric once per call and
  evaluates every weight column against it, rather than rebuilding it
  for each column. The estimates are unchanged.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  compares the weighted groups against an unweighted target population
  for `estimand = "ATE"`. It previously used the balancing weights for
  both sides of the comparison, which understated the energy distance
  when the weights improved balance and overstated it when they did not.
  Every weighted ATE energy distance changes, and the values now agree
  with `cobalt::bal.compute(stat = "energy.dist", estimand = "ATE")`.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  compares against the unweighted focal group for `estimand = "ATT"` and
  `estimand = "ATC"`. Weighting the focal group as well left the focal
  target with no effect, so `"ATT"` and `"ATC"` both returned the
  between-group energy distance whatever the weights were. They now
  differ from each other and from the between-group distance, and agree
  with cobalt. With uniform weights all three statistics still coincide,
  as the definitions require.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  defaults `.focal_level` to the last observed level of the exposure for
  `estimand = "ATT"` and the first for `estimand = "ATC"`, and validates
  a supplied value against the levels the exposure takes. It previously
  coerced the levels to numbers to find the focal group, so a
  text-valued exposure selected no group at all and returned a negative
  energy distance alongside a coercion warning, as did a `.focal_level`
  that named no group. A value that names no group now raises
  `halfmoon_reference_error`.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  reads the levels a factor exposure actually takes, so a declared level
  with no observations no longer divides by a group size of zero.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  returns `NA_real_` when `na.rm = FALSE` and the covariates, the
  exposure, or the weights contain missing values, where it previously
  raised `halfmoon_na_error`. This follows the convention that the
  atomic balance functions report missing values rather than refusing
  them, and it means
  [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  reports an energy row of `NA` without a warning instead of treating
  the metric as one it could not compute.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  gains an `exposure_type` argument, mirroring the one in
  [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
  so the continuous or the categorical statistic can be asked for
  directly. The default `"auto"` keeps the existing rule, which is now
  documented: a numeric exposure taking more than ten unique values is
  treated as continuous. The argument sits before `na.rm` in the
  signature, so a fully positional call that passed `na.rm` in that slot
  now raises `halfmoon_arg_error` rather than reading a logical as an
  exposure type.

- [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  raises `halfmoon_arg_error` for an option argument that is not a
  single value. `estimand = character(0)`, `criterion = character(0)`,
  and a length-2 `na.rm` previously produced a base R error, carrying no
  halfmoon class, about a condition of length zero.

- [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md)
  now reports the comparison group minus the reference group, so a
  positive value means the comparison group has the higher mean or
  proportion. This is the convention the documentation has always
  described and the one
  [`cobalt::col_w_smd()`](https://ngreifer.github.io/cobalt/reference/balance-summary.html)
  uses, but the estimate previously carried the opposite sign. Every
  standardized mean difference, from
  [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
  [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md),
  and `plot_balance(abs_smd = FALSE)`, changes sign. Categorical results
  follow the same convention: `X_vs_ref` is level `X` minus the
  reference level.

- [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md)
  resolves `.reference_level` against the levels of the exposure rather
  than the order in which those levels first appear in the data.
  Estimates no longer depend on the row order, and a `.reference_level`
  that names no group now raises a halfmoon error rather than passing
  through to the smd package.

- Functions that require a binary exposure count the levels an exposure
  actually takes rather than the levels a factor declares. A factor with
  unused levels and two observed groups is now valid input for binary
  [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
  [`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  and
  [`plot_mirror_distributions()`](https://r-causal.github.io/halfmoon/reference/plot_mirror_distributions.md),
  all of which previously rejected it for declaring too many levels. An
  exposure with a single observed group now raises
  `halfmoon_group_error` in
  [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
  and
  [`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md),
  instead of failing inside the smd package or returning `NA` without
  comment, and in
  [`bal_qq()`](https://r-causal.github.io/halfmoon/reference/bal_qq.md),
  which previously returned a table pairing the observed group’s
  quantiles with a column of missing values.
  [`bal_prognostic_score()`](https://r-causal.github.io/halfmoon/reference/bal_prognostic_score.md)
  reports that same error for an exposure with one observed group, where
  it previously reported a missing control level.

- [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
  and
  [`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md)
  treat a missing exposure the way they treat a missing covariate or
  weight: with `na.rm = FALSE` the result is `NA`, and with
  `na.rm = TRUE` the affected rows are dropped.
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md)
  and
  [`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md)
  previously dropped rows with a missing exposure without being asked,
  and the categorical versions of all three did the same. A categorical
  exposure with missing data now returns an all-`NA` named vector by
  default, so calls on `nhefs_weights$alcoholfreq_cat` need
  `na.rm = TRUE`.

- `bal_smd(na.rm = TRUE)` drops rows with missing weights instead of
  failing inside the smd package, which only removes missing covariate
  values.

- [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md),
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
  and
  [`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md)
  return `NA` when a group carries no weight. Zero weights are valid
  input, so a group they empty has no mean, variance, or distribution to
  report.
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md)
  and
  [`bal_ks()`](https://r-causal.github.io/halfmoon/reference/bal_ks.md)
  previously failed with a base R error about a missing value or an
  interpolation with no points, and
  [`bal_smd()`](https://r-causal.github.io/halfmoon/reference/bal_smd.md)
  was worse: the smd package treats a group with no weight as having a
  mean and variance of zero, so an undefined statistic came back as a
  plausible number.

- [`bal_corr()`](https://r-causal.github.io/halfmoon/reference/bal_corr.md)
  returns `NA` when the weights sum to zero, rather than failing with a
  base R error about a missing value.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  gains an `exposure_type` argument, one of `"binary"`, `"categorical"`,
  or `"continuous"`. It defaults to `"auto"`, which reads the type from
  `.exposure` and reports what it found.
  `options(halfmoon.quiet = TRUE)` silences that report.

- `.metrics` in
  [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  now defaults to `NULL`, which computes every metric that applies to
  the exposure type: the standardized mean difference, the variance
  ratio, the Kolmogorov-Smirnov statistic, and the energy distance for a
  binary or categorical exposure, and the weighted correlation and the
  energy distance for a continuous one. Results for binary and
  categorical exposures are unchanged. Asking for a metric that does not
  apply to the exposure type is now an error, so a continuous exposure
  no longer produces a standardized mean difference for every distinct
  value it takes, and a binary exposure no longer produces a
  correlation.

- [`check_balance()`](https://r-causal.github.io/halfmoon/reference/check_balance.md)
  computes the energy distance for the exposure type it resolved rather
  than from the count of distinct exposure values. A numeric exposure
  with many repeated values, such as a change score on a bounded count,
  reads as categorical and now contributes a between-group energy
  distance instead of a continuous one. The change runs the other way
  too: a numeric exposure with few distinct values in a small sample,
  such as eight distinct values across twenty rows, reads as continuous
  and now contributes a continuous energy distance instead of a
  between-group one. Pass `exposure_type` explicitly for the previous
  behavior. A direct call to
  [`bal_energy()`](https://r-causal.github.io/halfmoon/reference/bal_energy.md)
  is unchanged.

- [`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)
  marks the reference for the correlation metric at 0.

- [`ess()`](https://r-causal.github.io/causalgenerics/reference/ess.html)
  is now a re-export of the generic of the same name from
  causalgenerics. Attaching halfmoon alongside another package that
  re-exports that same generic no longer produces a masking conflict,
  because both packages export the one object. A package that defines
  its own unrelated
  [`ess()`](https://r-causal.github.io/causalgenerics/reference/ess.html)
  still masks, as before. The calculation is unchanged for numeric
  weights.

- Because the generic names its first argument `x`,
  [`ess()`](https://r-causal.github.io/causalgenerics/reference/ess.html)
  no longer accepts the argument name `wts`. Pass the weights
  positionally, as in `ess(w)`.

- [`ess()`](https://r-causal.github.io/causalgenerics/reference/ess.html),
  and
  [`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md)
  through it, now error on non-numeric input instead of returning a
  meaningless number. Previously `ess(NULL)` and `bal_ess(NULL)`
  returned `NaN`, and factors, logicals, data frames, dates, time
  differences, and complex vectors each produced a value:
  `bal_ess(factor("a"))` returned `1.8`. `ess(rep(0, 5))` and
  `ess(numeric(0))` still return `NaN`.

- An argument that is neither a column name nor something that evaluates
  to one now reports the function the user called, such as
  [`check_qq()`](https://r-causal.github.io/halfmoon/reference/check_qq.md),
  rather than the internal handler frame `value[[3L]](cond)`.

- `.reference_level` must name a single group. A value longer than one,
  or `NA`, is now a `halfmoon_arg_error` instead of the base R error
  `the condition has length > 1`.

- A `.reference_level` used as a position must be a whole number.
  Previously `bal_vr(x, g, .reference_level = 1.5)` silently truncated
  to the first level and returned that answer; it is now a
  `halfmoon_arg_error`. A numeric that equals one of the exposure’s
  level values is still read as that value rather than as a position, so
  `.reference_level = 0` on a 0/1 exposure still means the level `0`.

- Errors raised while resolving the exposure levels or the reference
  level now report the function the user called, such as
  [`bal_vr()`](https://r-causal.github.io/halfmoon/reference/bal_vr.md),
  rather than the internal helper `split_by_group()`.

- `.weights` is now validated with
  [`causalgenerics::is_causal_wt()`](https://r-causal.github.io/causalgenerics/reference/causal-weights.html),
  so any causal weight object is accepted rather than only the `psw`
  objects from propensity. The error message names a causal weight
  object instead of a `psw` object.

- [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  and
  [`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md)
  integrate the ROC curve in the order the curve is drawn. The points
  were previously re-sorted by false positive rate, which reversed each
  vertical run of the curve and paired the trapezoids with the wrong
  corners whenever two points shared a false positive rate. Every AUC
  can change slightly, and an AUC computed from a small sample or from
  scores with many ties can change materially. The values now equal the
  weighted Mann-Whitney concordance, so
  [`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md)
  on truth `(1, 0, 1, 0)` with scores `(4, 3, 2, 1)` reports `0.75`
  rather than `0.625`.

- [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  and
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  apply `na.rm` to the weight columns as well as to the exposure and the
  fitted values. A missing weight previously survived into the
  cumulative sums and produced a curve of missing values, from which the
  AUC was computed over the surviving fragment. With `na.rm = TRUE` the
  rows with a missing weight are dropped for that weight column, and
  with `na.rm = FALSE` a missing weight raises `halfmoon_na_error`
  naming the column.

- [`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md)
  and
  [`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md)
  require an exposure with exactly two observed levels, as
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  and
  [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  already did. A three-level exposure previously compared one level
  against the rest without comment, and a factor whose second declared
  level had no observations picked that empty level as the event and
  returned an AUC near zero. Unused declared levels are dropped, so a
  factor with two observed levels is valid input and gives the same
  answer as the same factor with its levels dropped.

- [`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md)
  and
  [`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md)
  drop observations with zero or negative weights and warn with
  `halfmoon_data_warning`, matching
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  and
  [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md).
  Such weights previously passed through and made the cumulative totals
  non-monotone, which sent sensitivity and specificity outside `[0, 1]`
  and left the two layers reporting different AUCs for the same input.

- `.focal_level` defaults to the last observed level of the exposure, or
  the maximum value of a numeric exposure, in the ROC and AUC functions.
  This is what the documentation has always described; the code used the
  second level, which is the same level for the binary input these
  functions accept.

- [`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
  [`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md),
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
  and
  [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  report a `.exposure`, `.fitted`, or `.weights` column that does not
  exist as `halfmoon_column_error` naming the argument, rather than
  passing through the `vctrs` subscript error from tidyselect. A
  `.focal_level` that names no level of the exposure now reports
  [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  rather than the internal `compute_roc_curve_imp()`, and a condition
  raised from
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  reports
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  rather than the
  [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  it delegates to.

- The `na.rm` documentation for
  [`bal_model_auc()`](https://r-causal.github.io/halfmoon/reference/bal_model_auc.md),
  [`bal_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/bal_model_roc_curve.md),
  and
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md)
  records the actual default of `TRUE`. The inherited text described a
  default of `FALSE`.

- [`check_model_calibration()`](https://r-causal.github.io/halfmoon/reference/check_model_calibration.md)
  now defaults to `na.rm = TRUE`, matching
  [`check_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/check_model_roc_curve.md)
  and
  [`check_model_auc()`](https://r-causal.github.io/halfmoon/reference/check_model_auc.md),
  and raises `halfmoon_na_error` when `na.rm = FALSE` and `.fitted` or
  `.exposure` holds a missing value. The `"windowed"` and `"logistic"`
  methods previously failed with a base R error on such input.

- [`check_model_calibration()`](https://r-causal.github.io/halfmoon/reference/check_model_calibration.md)
  requires an exposure with exactly two observed levels and raises
  `halfmoon_group_error` otherwise. An exposure that took a single value
  previously became its own focal level and reported an observed rate of
  1 in every bin, and an exposure with more than two levels reported one
  level against all the others without comment.

- [`check_model_calibration()`](https://r-causal.github.io/halfmoon/reference/check_model_calibration.md)
  and
  [`plot_model_calibration()`](https://r-causal.github.io/halfmoon/reference/plot_model_calibration.md)
  resolve the default `.focal_level` to the last OBSERVED level of the
  exposure, and validate a supplied value against the observed levels. A
  factor with an unused trailing declared level previously took that
  absent level as the event and reported an observed rate of 0 in every
  bin. A `.focal_level` that names no observed level now raises
  `halfmoon_reference_error` with the levels it could have named, where
  the message previously failed to format.

- [`check_model_calibration()`](https://r-causal.github.io/halfmoon/reference/check_model_calibration.md)
  reports a `.fitted` or `.exposure` column that does not exist as
  `halfmoon_column_error`. The check was previously skipped whenever
  `.focal_level` was supplied, which left a base R error from deeper in
  the summary.

- [`check_model_calibration()`](https://r-causal.github.io/halfmoon/reference/check_model_calibration.md)
  and
  [`geom_calibration()`](https://r-causal.github.io/halfmoon/reference/geom_calibration.md)
  return a single calibration bin, with a `halfmoon_data_warning`, when
  `.fitted` is constant. Both binning methods previously failed with
  `'breaks' are not unique`.

- The `"breaks"` method counts the observations whose outcome is known,
  so the bin count, the observed rate, and the confidence interval share
  a denominator. The interval was previously computed from a count
  inflated by rows whose outcome was missing.

- [`geom_calibration()`](https://r-causal.github.io/halfmoon/reference/geom_calibration.md)
  passes `binning_method` to its line layer, so the line now follows the
  same bins as the points and the ribbon rather than always using
  equal-width bins.

- [`geom_calibration()`](https://r-causal.github.io/halfmoon/reference/geom_calibration.md)
  renders an empty layer for a group with nothing left to summarize,
  rather than reporting `Computation failed in stat_calibration()`.
  Missing values follow the ggplot2 convention for a stat: they are
  dropped, and `na.rm = FALSE` reports how many rows went.

- [`geom_calibration()`](https://r-causal.github.io/halfmoon/reference/geom_calibration.md)
  resolves the default `.focal_level` once from the whole layer, before
  it is split into panels, and validates a supplied value against the
  levels the exposure takes. A facet holding a single group previously
  took that group as the event and reported an observed rate of 1
  throughout, so a numeric exposure and the same values as a factor
  disagreed on the same panel.

- [`check_model_calibration()`](https://r-causal.github.io/halfmoon/reference/check_model_calibration.md)
  classes an empty result `halfmoon_calibration`, so
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  dispatch on a result with no rows the way they do on every other
  result.

- The `"breaks"` and `"windowed"` calibration methods report a real
  confidence interval for a bin or window in which every observation is
  an event, or none is. Such bins are routine for a well separated
  propensity model, and both methods previously fell back to a normal
  approximation whose standard error is exactly zero at a rate of 0 or
  1, so they reported `[0, 0]` or `[1, 1]` and asserted certainty they
  had no basis for. A bin of 0 events out of 116 now reports
  `[0, 0.038]`, as
  [`prop.test()`](https://rdrr.io/r/stats/prop.test.html) gives it.
  Interval values change for those bins only.

- [`bal_prognostic_score()`](https://r-causal.github.io/halfmoon/reference/bal_prognostic_score.md)
  resolves `.reference_level` against the exposure itself rather than
  against its levels in sorted order, so a factor keeps its declared
  order. On a factor whose declared control level was not the
  alphabetically first, the outcome model was previously fit on the
  treated group.

- [`bal_prognostic_score()`](https://r-causal.github.io/halfmoon/reference/bal_prognostic_score.md)
  reports an exposure whose declared control level is never observed as
  such, naming the absent level, rather than as a bare count of levels.

- [`bal_prognostic_score()`](https://r-causal.github.io/halfmoon/reference/bal_prognostic_score.md)
  now defaults to `na.rm = TRUE` and raises `halfmoon_na_error` when
  `na.rm = FALSE` and a model variable, the exposure, or a weight is
  missing. It previously returned a score vector with missing values
  that later models would carry silently.

- [`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
  and
  [`plot_ess()`](https://r-causal.github.io/halfmoon/reference/plot_ess.md)
  work on a data frame that has a column named `method` or `weight`.
  Reshaping the data previously collided with the names of the columns
  [`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
  builds, so any such data frame raised `Names must be unique`. A weight
  or exposure column with either name may also be selected, and the
  returned tibble still names its columns `method` and `group`.

- [`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
  and
  [`plot_ess()`](https://r-causal.github.io/halfmoon/reference/plot_ess.md)
  gain `na.rm`, which defaults to `FALSE` as it does in
  [`ess()`](https://r-causal.github.io/causalgenerics/reference/ess.html)
  and
  [`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md).
  A missing weight now produces an `NA` effective sample size for that
  weighting method rather than being dropped without notice. Pass
  `na.rm = TRUE` for the previous behavior.

- [`check_ess()`](https://r-causal.github.io/halfmoon/reference/check_ess.md)
  reports `n`, and therefore `ess_pct`, against the observations whose
  weight is not missing. Weights of 90 ones and 10 missing values
  previously reported an effective sample size of 90 as 90% of 100
  observations. The 90 observations it was computed from are all
  weighted equally, so it is now reported as 100%.

- [`add_ess_header()`](https://r-causal.github.io/halfmoon/reference/add_ess_header.md)
  gives the group columns of a table with an overall column from
  [`gtsummary::add_overall()`](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html)
  the denominator they have without one, the sum of the group effective
  sample sizes. ESS is not additive, so dividing a group ESS by the ESS
  of the whole sample could exceed 1: the header of the `qsmk = 0`
  column of `nhefs_weights` weighted by `w_ate` reported `p` as 1.11.
  The group percentages now sum to 100%, and the overall column reports
  a `p` of 1.

- [`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md)
  validates `.weights` itself, so an invalid weight reports
  [`bal_ess()`](https://r-causal.github.io/halfmoon/reference/bal_ess.md)
  and `.weights` with a halfmoon condition class. `bal_ess(NULL)`
  previously raised a `causalgenerics` error about an argument named `x`
  in a call to `ess.default()`. A negative weight is now a
  `halfmoon_range_error`, where `bal_ess(c(1, -1))` previously returned
  0.

- [`geom_roc()`](https://r-causal.github.io/halfmoon/reference/geom_roc.md)
  and
  [`stat_roc()`](https://r-causal.github.io/halfmoon/reference/stat_roc.md)
  draw one curve per group when `group` is mapped explicitly. Long data
  holding several weighting schemes previously collapsed into a single
  curve that counted each subject once per scheme, so mapping `group`
  gave a different answer from mapping `colour`.

- [`geom_roc()`](https://r-causal.github.io/halfmoon/reference/geom_roc.md)
  and
  [`stat_roc()`](https://r-causal.github.io/halfmoon/reference/stat_roc.md)
  count only the observed levels of the `exposure` aesthetic. A factor
  that declares a level no observation takes is valid binary input,
  where it previously failed to compute, and the default `.focal_level`
  is the last observed level rather than the last declared one. A group
  that holds a single observed level is dropped with a warning and the
  rest of the panel is still drawn.

- [`geom_roc()`](https://r-causal.github.io/halfmoon/reference/geom_roc.md)
  and
  [`stat_roc()`](https://r-causal.github.io/halfmoon/reference/stat_roc.md)
  validate `.focal_level` against the observed exposure values and raise
  `halfmoon_reference_error` for a value the data does not take. An
  absent focal level previously drew a curve spanning the full range of
  the panel, computed from no events at all.

- [`geom_roc()`](https://r-causal.github.io/halfmoon/reference/geom_roc.md)
  and
  [`stat_roc()`](https://r-causal.github.io/halfmoon/reference/stat_roc.md)
  drop rows with a missing weight the way they drop rows with a missing
  estimate or exposure: silently under `na.rm = TRUE`, and with a report
  of the number of rows removed under `na.rm = FALSE`. A missing weight
  previously spread through the curve under `na.rm = FALSE`.

- [`plot_model_roc_curve()`](https://r-causal.github.io/halfmoon/reference/plot_model_roc_curve.md)
  labels the color aesthetic only when it maps one, so a plot of a
  single method no longer reports that it is ignoring an unknown label.

- [`geom_ecdf()`](https://r-causal.github.io/halfmoon/reference/geom_ecdf.md)
  honors `pad` and `n` when weights are mapped. The weighted curve
  previously ignored both, so it neither reached the edges of the panel
  nor could be interpolated onto a grid, and it stepped once per
  observation rather than once per distinct value, which placed the
  steps of tied values wrongly.

- [`geom_ecdf()`](https://r-causal.github.io/halfmoon/reference/geom_ecdf.md)
  drops rows with a missing weight, silently under `na.rm = TRUE` and
  with a report of the number of rows removed under `na.rm = FALSE`. A
  single missing weight previously turned the whole curve into missing
  values. A group whose weights sum to zero is dropped with a warning,
  where it previously returned `NaN` for every point.

- [`geom_mirror_histogram()`](https://r-causal.github.io/halfmoon/reference/geom_mirror_histogram.md)
  and
  [`geom_mirror_density()`](https://r-causal.github.io/halfmoon/reference/geom_mirror_density.md)
  mirror every statistic their stat computes, so `after_stat(density)`,
  `after_stat(ncount)`, `after_stat(ndensity)`, and
  `after_stat(wdensity)` are drawn below the axis for the mirrored
  group. Only `count` was mirrored for histograms, and `wdensity` was
  left upright for densities, so asking for any of the others drew both
  groups above the axis.

- [`plot_balance()`](https://r-causal.github.io/halfmoon/reference/plot_balance.md)
  draws the SMD threshold in the SMD facet whether or not other metrics
  are shown. `vline_xintercept`, `vline_color`, and `vlinewidth`
  previously did nothing unless SMD was the only metric plotted, which
  made the documented example a no-op. `vline_xintercept = NULL` now
  leaves the line out instead of adding an empty layer to the plot.

## halfmoon 0.2.0

CRAN release: 2026-03-04

## halfmoon 0.1.0.9000

- Added a `NEWS.md` file to track changes to the package.

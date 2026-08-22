# halfmoon (development version)

* `check_balance()` resolves `.reference_level` once for the whole call and uses
  the result for every metric and every label. It previously resolved the
  argument one way for the standardized mean difference and another way for the
  variance ratio and the Kolmogorov-Smirnov statistic, so a single table could
  compare against two different reference groups and label the rows with the
  wrong one. On a 0/1 exposure the default now references the level `0` for
  every metric, where the standardized mean difference previously referenced the
  level `1`: those rows change sign relative to the previous output, and the
  `group_level` column reports `1` rather than `0`. The default is now `NULL`,
  documented as the first observed level, and a value that matches a level is
  taken as that level, so `.reference_level = 0` on a 0/1 exposure means the
  level `0` for every metric.

* `check_balance()` validates `.reference_level` before computing anything. A
  value that names no group raises `halfmoon_reference_error` and an index out
  of range raises `halfmoon_range_error`, where both previously produced rows of
  missing values without comment for the variance ratio and the
  Kolmogorov-Smirnov statistic.

* `check_balance()` warns once, naming the affected metrics and variables, when
  it reports a combination it could not compute as `NA`. Missing values that
  `na.rm = FALSE` asks it to keep are not such a combination and stay silent.

* `check_balance()` honors a renamed selection. `.weights = c(myw = w_ate)` now
  weights by `w_ate` and reports the method as `myw`, where it previously
  reported unweighted estimates under the new name, and a renamed `.vars`
  selection now reports the covariate under its new name instead of failing or
  returning `NA`.

* `check_balance()` drops the grouping of a grouped data frame instead of adding
  the grouping variables to `.vars` and repairing the duplicated names.

* `check_balance()` reads a logical covariate as a 0/1 indicator, where every
  metric previously reported `NA` for it.

* `check_balance()` labels the comparison groups of a categorical exposure by
  removing the reference level from the names the balance functions return,
  rather than by splitting those names on `_vs_`. A reference level that itself
  contains `_vs_` no longer truncates the labels. A metric that fails for a
  categorical exposure now reports one missing row per comparison level instead
  of a single row labeled with one of them.

* `check_balance()` forms the distance matrix behind the energy metric once per
  call and evaluates every weight column against it, rather than rebuilding it
  for each column. The estimates are unchanged.

* `bal_energy()` compares the weighted groups against an unweighted target
  population for `estimand = "ATE"`. It previously used the balancing weights
  for both sides of the comparison, which understated the energy distance when
  the weights improved balance and overstated it when they did not. Every
  weighted ATE energy distance changes, and the values now agree with
  `cobalt::bal.compute(stat = "energy.dist", estimand = "ATE")`.

* `bal_energy()` compares against the unweighted focal group for
  `estimand = "ATT"` and `estimand = "ATC"`. Weighting the focal group as well
  left the focal target with no effect, so `"ATT"` and `"ATC"` both returned the
  between-group energy distance whatever the weights were. They now differ from
  each other and from the between-group distance, and agree with cobalt. With
  uniform weights all three statistics still coincide, as the definitions
  require.

* `bal_energy()` defaults `.focal_level` to the last observed level of the
  exposure for `estimand = "ATT"` and the first for `estimand = "ATC"`, and
  validates a supplied value against the levels the exposure takes. It
  previously coerced the levels to numbers to find the focal group, so a
  text-valued exposure selected no group at all and returned a negative energy
  distance alongside a coercion warning, as did a `.focal_level` that named no
  group. A value that names no group now raises `halfmoon_reference_error`.

* `bal_energy()` reads the levels a factor exposure actually takes, so a
  declared level with no observations no longer divides by a group size of
  zero.

* `bal_energy()` returns `NA_real_` when `na.rm = FALSE` and the covariates, the
  exposure, or the weights contain missing values, where it previously raised
  `halfmoon_na_error`. This follows the convention that the atomic balance
  functions report missing values rather than refusing them, and it means
  `check_balance()` reports an energy row of `NA` without a warning instead of
  treating the metric as one it could not compute.

* `bal_energy()` gains an `exposure_type` argument, mirroring the one in
  `check_balance()`, so the continuous or the categorical statistic can be
  asked for directly. The default `"auto"` keeps the existing rule, which is now
  documented: a numeric exposure taking more than ten unique values is treated
  as continuous. The argument sits before `na.rm` in the signature, so a fully
  positional call that passed `na.rm` in that slot now raises
  `halfmoon_arg_error` rather than reading a logical as an exposure type.

* `bal_energy()` raises `halfmoon_arg_error` for an option argument that is not
  a single value. `estimand = character(0)`, `criterion = character(0)`, and
  a length-2 `na.rm` previously produced a base R error, carrying no halfmoon
  class, about a condition of length zero.

* `bal_smd()` now reports the comparison group minus the reference group, so a
  positive value means the comparison group has the higher mean or proportion.
  This is the convention the documentation has always described and the one
  `cobalt::col_w_smd()` uses, but the estimate previously carried the opposite
  sign. Every standardized mean difference, from `bal_smd()`, `check_balance()`,
  and `plot_balance(abs_smd = FALSE)`, changes sign. Categorical results follow
  the same convention: `X_vs_ref` is level `X` minus the reference level.

* `bal_smd()` resolves `.reference_level` against the levels of the exposure
  rather than the order in which those levels first appear in the data.
  Estimates no longer depend on the row order, and a `.reference_level` that
  names no group now raises a halfmoon error rather than passing through to the
  smd package.

* Functions that require a binary exposure count the levels an exposure
  actually takes rather than the levels a factor declares. A factor with unused
  levels and two observed groups is now valid input for binary `bal_smd()`,
  `bal_vr()`, `bal_ks()`, `bal_qq()`, and `plot_mirror_distributions()`, all of
  which previously rejected it for declaring too many levels. An exposure with
  a single observed group now raises `halfmoon_group_error` in `bal_smd()`,
  `bal_vr()`, and `bal_ks()`, instead of failing inside the smd package or
  returning `NA` without comment, and in `bal_qq()`, which previously returned a
  table pairing the observed group's quantiles with a column of missing values.
  `bal_prognostic_score()` reports that same error for an exposure with one
  observed group, where it previously reported a missing control level.

* `bal_smd()`, `bal_vr()`, and `bal_ks()` treat a missing exposure the way they
  treat a missing covariate or weight: with `na.rm = FALSE` the result is `NA`,
  and with `na.rm = TRUE` the affected rows are dropped. `bal_vr()` and
  `bal_ks()` previously dropped rows with a missing exposure without being
  asked, and the categorical versions of all three did the same. A categorical
  exposure with missing data now returns an all-`NA` named vector by default,
  so calls on `nhefs_weights$alcoholfreq_cat` need `na.rm = TRUE`.

* `bal_smd(na.rm = TRUE)` drops rows with missing weights instead of failing
  inside the smd package, which only removes missing covariate values.

* `bal_smd()`, `bal_vr()`, and `bal_ks()` return `NA` when a group carries no
  weight. Zero weights are valid input, so a group they empty has no mean,
  variance, or distribution to report. `bal_vr()` and `bal_ks()` previously
  failed with a base R error about a missing value or an interpolation with no
  points, and `bal_smd()` was worse: the smd package treats a group with no
  weight as having a mean and variance of zero, so an undefined statistic came
  back as a plausible number.

* `bal_corr()` returns `NA` when the weights sum to zero, rather than failing
  with a base R error about a missing value.

* `check_balance()` gains an `exposure_type` argument, one of `"binary"`,
  `"categorical"`, or `"continuous"`. It defaults to `"auto"`, which reads the
  type from `.exposure` and reports what it found.
  `options(halfmoon.quiet = TRUE)` silences that report.

* `.metrics` in `check_balance()` now defaults to `NULL`, which computes every
  metric that applies to the exposure type: the standardized mean difference,
  the variance ratio, the Kolmogorov-Smirnov statistic, and the energy distance
  for a binary or categorical exposure, and the weighted correlation and the
  energy distance for a continuous one. Results for binary and categorical
  exposures are unchanged. Asking for a metric that does not apply to the
  exposure type is now an error, so a continuous exposure no longer produces a
  standardized mean difference for every distinct value it takes, and a binary
  exposure no longer produces a correlation.

* `check_balance()` computes the energy distance for the exposure type it
  resolved rather than from the count of distinct exposure values. A numeric
  exposure with many repeated values, such as a change score on a bounded
  count, reads as categorical and now contributes a between-group energy
  distance instead of a continuous one. Pass `exposure_type = "continuous"` for
  the previous behavior. A direct call to `bal_energy()` is unchanged.

* `plot_balance()` marks the reference for the correlation metric at 0.

* `ess()` is now a re-export of the generic of the same name from
  causalgenerics. Attaching halfmoon alongside another package that re-exports
  that same generic no longer produces a masking conflict, because both
  packages export the one object. A package that defines its own unrelated
  `ess()` still masks, as before. The calculation is unchanged for numeric
  weights.

* Because the generic names its first argument `x`, `ess()` no longer accepts
  the argument name `wts`. Pass the weights positionally, as in `ess(w)`.

* `ess()`, and `bal_ess()` through it, now error on non-numeric input instead
  of returning a meaningless number. Previously `ess(NULL)` and `bal_ess(NULL)`
  returned `NaN`, and factors, logicals, data frames, dates, time differences,
  and complex vectors each produced a value: `bal_ess(factor("a"))` returned
  `1.8`. `ess(rep(0, 5))` and `ess(numeric(0))` still return `NaN`.

* An argument that is neither a column name nor something that evaluates to one
  now reports the function the user called, such as `check_qq()`, rather than
  the internal handler frame `value[[3L]](cond)`.

* `.reference_level` must name a single group. A value longer than one, or `NA`,
  is now a `halfmoon_arg_error` instead of the base R error
  `the condition has length > 1`.

* A `.reference_level` used as a position must be a whole number. Previously
  `bal_vr(x, g, .reference_level = 1.5)` silently truncated to the first level
  and returned that answer; it is now a `halfmoon_arg_error`. A numeric that
  equals one of the exposure's level values is still read as that value rather
  than as a position, so `.reference_level = 0` on a 0/1 exposure still means
  the level `0`.

* Errors raised while resolving the exposure levels or the reference level now
  report the function the user called, such as `bal_vr()`, rather than the
  internal helper `split_by_group()`.

* `.weights` is now validated with `causalgenerics::is_causal_wt()`, so any
  causal weight object is accepted rather than only the `psw` objects from
  propensity. The error message names a causal weight object instead of a
  `psw` object.

* `check_model_auc()` and `bal_model_auc()` integrate the ROC curve in the order
  the curve is drawn. The points were previously re-sorted by false positive
  rate, which reversed each vertical run of the curve and paired the trapezoids
  with the wrong corners whenever two points shared a false positive rate. Every
  AUC can change slightly, and an AUC computed from a small sample or from
  scores with many ties can change materially. The values now equal the weighted
  Mann-Whitney concordance, so `bal_model_auc()` on truth `(1, 0, 1, 0)` with
  scores `(4, 3, 2, 1)` reports `0.75` rather than `0.625`.

* `check_model_roc_curve()` and `check_model_auc()` apply `na.rm` to the weight
  columns as well as to the exposure and the fitted values. A missing weight
  previously survived into the cumulative sums and produced a curve of missing
  values, from which the AUC was computed over the surviving fragment. With
  `na.rm = TRUE` the rows with a missing weight are dropped for that weight
  column, and with `na.rm = FALSE` a missing weight raises `halfmoon_na_error`
  naming the column.

* `bal_model_auc()` and `bal_model_roc_curve()` require an exposure with exactly
  two observed levels, as `check_model_auc()` and `check_model_roc_curve()`
  already did. A three-level exposure previously compared one level against the
  rest without comment, and a factor whose second declared level had no
  observations picked that empty level as the event and returned an AUC near
  zero. Unused declared levels are dropped, so a factor with two observed levels
  is valid input and gives the same answer as the same factor with its levels
  dropped.

* `bal_model_auc()` and `bal_model_roc_curve()` drop observations with zero or
  negative weights and warn with `halfmoon_data_warning`, matching
  `check_model_auc()` and `check_model_roc_curve()`. Such weights previously
  passed through and made the cumulative totals non-monotone, which sent
  sensitivity and specificity outside `[0, 1]` and left the two layers reporting
  different AUCs for the same input.

* `.focal_level` defaults to the last observed level of the exposure, or the
  maximum value of a numeric exposure, in the ROC and AUC functions. This is
  what the documentation has always described; the code used the second level,
  which is the same level for the binary input these functions accept.

* `bal_model_auc()`, `bal_model_roc_curve()`, `check_model_auc()`, and
  `check_model_roc_curve()` report a `.exposure`, `.fitted`, or `.weights`
  column that does not exist as `halfmoon_column_error` naming the argument,
  rather than passing through the `vctrs` subscript error from tidyselect. A
  `.focal_level` that names no level of the exposure now reports
  `check_model_roc_curve()` rather than the internal `compute_roc_curve_imp()`,
  and a condition raised from `check_model_auc()` reports `check_model_auc()`
  rather than the `check_model_roc_curve()` it delegates to.

* The `na.rm` documentation for `bal_model_auc()`, `bal_model_roc_curve()`, and
  `check_model_auc()` records the actual default of `TRUE`. The inherited text
  described a default of `FALSE`.

* `check_model_calibration()` now defaults to `na.rm = TRUE`, matching
  `check_model_roc_curve()` and `check_model_auc()`, and raises
  `halfmoon_na_error` when `na.rm = FALSE` and `.fitted` or `.exposure` holds a
  missing value. The `"windowed"` and `"logistic"` methods previously failed
  with a base R error on such input.

* `check_model_calibration()` requires an exposure with exactly two observed
  levels and raises `halfmoon_group_error` otherwise. An exposure that took a
  single value previously became its own focal level and reported an observed
  rate of 1 in every bin, and an exposure with more than two levels reported
  one level against all the others without comment.

* `check_model_calibration()` and `plot_model_calibration()` resolve the default
  `.focal_level` to the last OBSERVED level of the exposure, and validate a
  supplied value against the observed levels. A factor with an unused trailing
  declared level previously took that absent level as the event and reported an
  observed rate of 0 in every bin. A `.focal_level` that names no observed level
  now raises `halfmoon_reference_error` with the levels it could have named,
  where the message previously failed to format.

* `check_model_calibration()` reports a `.fitted` or `.exposure` column that does
  not exist as `halfmoon_column_error`. The check was previously skipped
  whenever `.focal_level` was supplied, which left a base R error from deeper in
  the summary.

* `check_model_calibration()` and `geom_calibration()` return a single
  calibration bin, with a `halfmoon_data_warning`, when `.fitted` is constant.
  Both binning methods previously failed with `'breaks' are not unique`.

* The `"breaks"` method counts the observations whose outcome is known, so the
  bin count, the observed rate, and the confidence interval share a denominator.
  The interval was previously computed from a count inflated by rows whose
  outcome was missing.

* `geom_calibration()` passes `binning_method` to its line layer, so the line
  now follows the same bins as the points and the ribbon rather than always
  using equal-width bins.

* `geom_calibration()` renders an empty layer for a group with nothing left to
  summarize, rather than reporting `Computation failed in stat_calibration()`.
  Missing values follow the ggplot2 convention for a stat: they are dropped, and
  `na.rm = FALSE` reports how many rows went.

* `geom_calibration()` resolves the default `.focal_level` once from the whole
  layer, before it is split into panels, and validates a supplied value against
  the levels the exposure takes. A facet holding a single group previously took
  that group as the event and reported an observed rate of 1 throughout, so a
  numeric exposure and the same values as a factor disagreed on the same panel.

* `check_model_calibration()` classes an empty result `halfmoon_calibration`, so
  `plot()` and `autoplot()` dispatch on a result with no rows the way they do on
  every other result.

* `bal_prognostic_score()` resolves `.reference_level` against the exposure
  itself rather than against its levels in sorted order, so a factor keeps its
  declared order. On a factor whose declared control level was not the
  alphabetically first, the outcome model was previously fit on the treated
  group.

* `bal_prognostic_score()` reports an exposure whose declared control level is
  never observed as such, naming the absent level, rather than as a bare count
  of levels.

* `bal_prognostic_score()` now defaults to `na.rm = TRUE` and raises
  `halfmoon_na_error` when `na.rm = FALSE` and a model variable, the exposure,
  or a weight is missing. It previously returned a score vector with missing
  values that later models would carry silently.

# halfmoon 0.2.0

# halfmoon 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

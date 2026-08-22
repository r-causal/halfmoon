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

# halfmoon 0.2.0

# halfmoon 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

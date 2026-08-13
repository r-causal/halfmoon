# Changelog

## halfmoon (development version)

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
  distance instead of a continuous one. Pass
  `exposure_type = "continuous"` for the previous behavior. A direct
  call to
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

## halfmoon 0.2.0

CRAN release: 2026-03-04

## halfmoon 0.1.0.9000

- Added a `NEWS.md` file to track changes to the package.

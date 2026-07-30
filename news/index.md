# Changelog

## halfmoon (development version)

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

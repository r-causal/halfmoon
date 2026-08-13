# Regression fixture for the default output of `check_balance()` with binary and
# categorical exposures. The saved list pins the results a default call produced
# before `check_balance()` resolved metrics from the exposure type, so the
# accompanying test can show that type resolution leaves those results untouched.
# Regenerate only when a change to the default output is intended.

pkgload::load_all(quiet = TRUE)

check_balance_defaults <- list(
  binary_observed = check_balance(
    nhefs_weights,
    c(age, wt71, smokeintensity, race, education),
    qsmk
  ),
  binary_weighted = check_balance(
    nhefs_weights,
    c(age, wt71, smokeintensity, race, education),
    qsmk,
    .weights = c(w_ate, w_att)
  ),
  categorical_observed = check_balance(
    nhefs_weights,
    c(age, wt71, sex),
    alcoholfreq_cat
  ),
  categorical_weighted = check_balance(
    nhefs_weights,
    c(age, wt71, sex),
    alcoholfreq_cat,
    .weights = c(w_cat_ate, w_cat_att_2_3wk)
  )
)

saveRDS(
  check_balance_defaults,
  testthat::test_path("fixtures", "check-balance-defaults.rds"),
  version = 3
)

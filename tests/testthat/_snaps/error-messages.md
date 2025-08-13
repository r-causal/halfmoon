# error messages show user-facing function names

    Code
      plot_mirror_distributions(nhefs_weights, age, alcoholfreq_cat,
        .reference_level = "invalid")
    Condition <halfmoon_reference_error>
      Error in `plot_mirror_distributions()`:
      ! `.reference_level` "invalid" not found in grouping variable

---

    Code
      plot_mirror_distributions(nhefs_weights, age, alcoholfreq_cat,
        .reference_level = 10)
    Condition <halfmoon_range_error>
      Error in `plot_mirror_distributions()`:
      ! .reference_level index 10 out of bounds

---

    Code
      plot_mirror_distributions(nhefs_weights)
    Condition <halfmoon_arg_error>
      Error in `plot_mirror_distributions()`:
      ! Argument `.var` is required

---

    Code
      plot_mirror_distributions(nhefs_weights, missing_column, qsmk)
    Condition <halfmoon_column_error>
      Error in `plot_mirror_distributions()`:
      ! Column `missing_column` not found in `.var`

---

    Code
      plot_qq(nhefs_weights, age, qsmk, .reference_level = "invalid")
    Condition <halfmoon_reference_error>
      Error in `plot_qq()`:
      ! `.reference_level` 'invalid' not found in `.group` levels: "0" and "1"

---

    Code
      plot_stratified_residuals(model)
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `.exposure` is required

---

    Code
      check_balance(nhefs_weights, .vars = age, .group = rep(1, nrow(nhefs_weights)))
    Condition <simpleError>
      Error in `check_balance()`:
      ! unused argument (.group = rep(1, nrow(nhefs_weights)))

---

    Code
      bal_prognostic_score(nhefs_weights, .exposure = qsmk, formula = wt82_71 ~ age +
        qsmk + wt71)
    Condition <halfmoon_formula_error>
      Error in `bal_prognostic_score()`:
      ! The treatment variable 'qsmk' should not be included in the outcome model formula.

# validation errors show correct function context

    Code
      check_balance(nhefs_weights, .vars = age, .group = "not_numeric")
    Condition <simpleError>
      Error in `check_balance()`:
      ! unused argument (.group = "not_numeric")

---

    Code
      check_balance(data.frame(), .vars = age, .group = qsmk)
    Condition <simpleError>
      Error in `check_balance()`:
      ! unused argument (.group = qsmk)

# errors have correct custom classes

    Code
      expr
    Condition <halfmoon_reference_error>
      Error in `plot_mirror_distributions()`:
      ! `.reference_level` "invalid" not found in grouping variable

---

    Code
      expr
    Condition <halfmoon_range_error>
      Error in `plot_mirror_distributions()`:
      ! .reference_level index 10 out of bounds

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_mirror_distributions()`:
      ! Argument `.var` is required

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_mirror_distributions()`:
      ! Column `missing_column` not found in `.var`

---

    Code
      expr
    Condition <halfmoon_formula_error>
      Error in `bal_prognostic_score()`:
      ! The treatment variable 'qsmk' should not be included in the outcome model formula.


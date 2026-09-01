# bal_prognostic_score validates treatment not in formula

    Code
      bal_prognostic_score(nhefs_weights, .exposure = qsmk, formula = wt82_71 ~ age +
        qsmk + wt71)
    Condition <halfmoon_formula_error>
      Error in `bal_prognostic_score()`:
      ! The treatment variable 'qsmk' should not be included in the outcome model formula.

# bal_prognostic_score errors with no control observations

    Code
      bal_prognostic_score(treated_only, outcome = wt82_71, .exposure = qsmk,
        .covariates = c(age, sex))
    Condition <halfmoon_group_error>
      Error in `bal_prognostic_score()`:
      ! No control observations found in `.exposure` (`qsmk`).
      x Declared level "0" never observed.
      i A prognostic score is fit on the control group, so both groups must be present.

# bal_prognostic_score errors when required arguments missing

    Code
      bal_prognostic_score(nhefs_weights, .exposure = qsmk, .covariates = c(age, sex))
    Condition <halfmoon_arg_error>
      Error in `bal_prognostic_score()`:
      ! Either `outcome` or `formula` must be provided.

---

    Code
      bal_prognostic_score(nhefs_weights, .exposure = qsmk, formula = "not a formula")
    Condition <halfmoon_formula_error>
      Error in `bal_prognostic_score()`:
      ! `formula` must be a formula object.

# bal_prognostic_score names the control level that is absent

    Code
      bal_prognostic_score(treated_only, outcome = y, .exposure = g, .covariates = x)
    Condition <halfmoon_group_error>
      Error in `bal_prognostic_score()`:
      ! No control observations found in `.exposure` (`g`).
      x Declared level "ctrl" never observed.
      i A prognostic score is fit on the control group, so both groups must be present.


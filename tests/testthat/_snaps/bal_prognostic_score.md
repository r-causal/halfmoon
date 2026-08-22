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
    Condition <halfmoon_reference_error>
      Error in `bal_prognostic_score()`:
      ! No control observations found. Control level '0' not present in treatment variable.

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


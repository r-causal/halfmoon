# bal_prognostic_score validates treatment not in formula

    Code
      expr
    Condition <halfmoon_formula_error>
      Error in `bal_prognostic_score()`:
      ! The treatment variable 'qsmk' should not be included in the outcome model formula.

# bal_prognostic_score errors with no control observations

    Code
      expr
    Condition <halfmoon_reference_error>
      Error in `bal_prognostic_score()`:
      ! No control observations found. Control level '0' not present in treatment variable.

# bal_prognostic_score errors when required arguments missing

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `bal_prognostic_score()`:
      ! Either `outcome` or `formula` must be provided.

---

    Code
      expr
    Condition <halfmoon_formula_error>
      Error in `bal_prognostic_score()`:
      ! `formula` must be a formula object.


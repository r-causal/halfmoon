# functions handle edge cases correctly

    Code
      expr
    Condition <halfmoon_na_error>
      Error in `check_model_roc_curve()`:
      ! Missing values found and `na.rm = FALSE`

# functions handle different truth variable types

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `check_model_roc_curve()`:
      ! `.exposure` must have exactly 2 unique values

# error messages use proper cli formatting

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_model_roc_curve()`:
      ! `.data` must be a data frame

---

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_model_roc_curve()`:
      ! `.estimate` must be numeric, got <character>

---

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `check_model_roc_curve()`:
      ! `.exposure` must have exactly 2 levels

# treatment_level parameter works correctly

    Code
      expr
    Condition <halfmoon_reference_error>
      Error in `compute_roc_curve_imp()`:
      ! `.focal_level` 'invalid' not found in `truth` levels: "0" and "1"


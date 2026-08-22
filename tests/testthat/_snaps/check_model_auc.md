# functions handle edge cases correctly

    Code
      check_model_roc_curve(test_data_na, truth, estimate, weight1, na.rm = FALSE)
    Condition <halfmoon_na_error>
      Error in `check_model_roc_curve()`:
      ! Missing values found and `na.rm = FALSE`

# functions handle different truth variable types

    Code
      check_model_roc_curve(test_multi, truth, estimate)
    Condition <halfmoon_group_error>
      Error in `check_model_roc_curve()`:
      ! `.exposure` must have exactly 2 unique values

# error messages use proper cli formatting

    Code
      check_model_roc_curve("not a data frame", truth, estimate)
    Condition <halfmoon_type_error>
      Error in `check_model_roc_curve()`:
      ! `.data` must be a data frame

---

    Code
      check_model_roc_curve(test_data, truth, estimate_char)
    Condition <halfmoon_type_error>
      Error in `check_model_roc_curve()`:
      ! `.fitted` must be numeric, got <character>

---

    Code
      check_model_roc_curve(test_data, truth_multi, estimate)
    Condition <halfmoon_group_error>
      Error in `check_model_roc_curve()`:
      ! `.exposure` must have exactly 2 levels

# .focal_level parameter works correctly

    Code
      check_model_roc_curve(nhefs_weights, qsmk, .fitted, .focal_level = "invalid")
    Condition <halfmoon_reference_error>
      Error in `compute_roc_curve_imp()`:
      ! `.focal_level` 'invalid' not found in `truth` levels: "0" and "1"


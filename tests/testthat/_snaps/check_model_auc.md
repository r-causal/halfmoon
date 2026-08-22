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
      Error in `check_model_roc_curve()`:
      ! `.focal_level` 'invalid' not found in `truth` levels: "0" and "1"

# check_model_roc_curve rejects missing weights with na.rm = FALSE

    Code
      check_model_roc_curve(nhefs_na, qsmk, .fitted, weight, na.rm = FALSE)
    Condition <halfmoon_na_error>
      Error in `check_model_roc_curve()`:
      ! Missing values found in `weight` and `na.rm = FALSE`

# check_model_* report a missing column against the call the user made

    Code
      check_model_roc_curve(nhefs_weights, nonexistent, .fitted)
    Condition <halfmoon_column_error>
      Error in `check_model_roc_curve()`:
      ! `.exposure` must name a column in `.data`
      x Column `nonexistent` does not exist

---

    Code
      check_model_roc_curve(nhefs_weights, qsmk, .fitted, nonexistent)
    Condition <halfmoon_column_error>
      Error in `check_model_roc_curve()`:
      ! `.weights` must name a column in `.data`
      x Column `nonexistent` does not exist

---

    Code
      check_model_auc(nhefs_weights, qsmk, nonexistent, w_ate)
    Condition <halfmoon_column_error>
      Error in `check_model_auc()`:
      ! `.fitted` must name a column in `.data`
      x Column `nonexistent` does not exist


# bal_model_roc_curve validates inputs

    Code
      bal_model_roc_curve(nhefs_weights, nonexistent, .fitted)
    Condition <vctrs_error_subscript_oob>
      Error in `bal_model_roc_curve()`:
      ! Can't select columns that don't exist.
      x Column `nonexistent` doesn't exist.

---

    Code
      bal_model_roc_curve(nhefs_weights, qsmk, nonexistent)
    Condition <vctrs_error_subscript_oob>
      Error in `bal_model_roc_curve()`:
      ! Can't select columns that don't exist.
      x Column `nonexistent` doesn't exist.

---

    Code
      bal_model_roc_curve(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))
    Condition <halfmoon_arg_error>
      Error in `bal_model_roc_curve()`:
      ! `.weights` must select exactly one variable or be NULL


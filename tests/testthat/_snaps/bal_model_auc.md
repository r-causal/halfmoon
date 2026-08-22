# bal_model_auc validates inputs

    Code
      bal_model_auc(nhefs_weights, nonexistent, .fitted)
    Condition <vctrs_error_subscript_oob>
      Error in `bal_model_auc()`:
      ! Can't select columns that don't exist.
      x Column `nonexistent` doesn't exist.

---

    Code
      bal_model_auc(nhefs_weights, qsmk, nonexistent)
    Condition <vctrs_error_subscript_oob>
      Error in `bal_model_auc()`:
      ! Can't select columns that don't exist.
      x Column `nonexistent` doesn't exist.

---

    Code
      bal_model_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))
    Condition <halfmoon_arg_error>
      Error in `bal_model_auc()`:
      ! `.weights` must select exactly one variable or be NULL


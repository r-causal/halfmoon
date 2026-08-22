# bal_model_auc validates inputs

    Code
      bal_model_auc(nhefs_weights, nonexistent, .fitted)
    Condition <halfmoon_column_error>
      Error in `bal_model_auc()`:
      ! `.exposure` must name a column in `.data`
      x Column `nonexistent` does not exist

---

    Code
      bal_model_auc(nhefs_weights, qsmk, nonexistent)
    Condition <halfmoon_column_error>
      Error in `bal_model_auc()`:
      ! `.fitted` must name a column in `.data`
      x Column `nonexistent` does not exist

---

    Code
      bal_model_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))
    Condition <halfmoon_arg_error>
      Error in `bal_model_auc()`:
      ! `.weights` must select exactly one variable or be NULL


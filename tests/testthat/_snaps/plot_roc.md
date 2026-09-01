# plot functions handle invalid inputs

    Code
      plot_model_roc_curve(bad_data)
    Condition <halfmoon_column_error>
      Error in `plot_model_roc_curve()`:
      ! `.data` must contain columns: "threshold", "sensitivity", "specificity", and "method". Missing: "threshold", "sensitivity", "specificity", and "method"

---

    Code
      plot_model_auc(bad_data)
    Condition <halfmoon_column_error>
      Error in `plot_model_auc()`:
      ! `.data` must contain columns: "method" and "auc". Missing: "method" and "auc"

---

    Code
      plot_model_roc_curve("not a data frame")
    Condition <halfmoon_type_error>
      Error in `plot_model_roc_curve()`:
      ! `.data` must be a data frame or tibble from `check_model_roc_curve()`

---

    Code
      plot_model_auc("not a data frame")
    Condition <halfmoon_type_error>
      Error in `plot_model_auc()`:
      ! `.data` must be a data frame or tibble from `check_model_auc()`


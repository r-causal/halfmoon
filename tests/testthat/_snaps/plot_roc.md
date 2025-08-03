# plot functions handle invalid inputs

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_roc_curve()`:
      ! `.data` must contain columns: "threshold", "sensitivity", "specificity", and "method". Missing: "threshold", "sensitivity", "specificity", and "method"

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_roc_auc()`:
      ! `.data` must contain columns: "method" and "auc". Missing: "method" and "auc"

---

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `plot_roc_curve()`:
      ! `.data` must be a data frame or tibble from `roc_curve()`

---

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `plot_roc_auc()`:
      ! `.data` must be a data frame or tibble from `check_auc()`


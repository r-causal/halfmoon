# plot_balance validates input

    Code
      plot_balance(data.frame(x = 1:5))
    Condition <halfmoon_column_error>
      Error in `plot_balance()`:
      ! Input must be output from check_balance(). Missing columns: variable, method, metric, estimate

---

    Code
      plot_balance(list(variable = "x"))
    Condition <halfmoon_type_error>
      Error in `plot_balance()`:
      ! `.data` must be a data frame


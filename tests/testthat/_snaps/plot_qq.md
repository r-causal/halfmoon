# plot_qq validates missing arguments

    Code
      plot_qq(nhefs_weights)
    Condition <halfmoon_arg_error>
      Error in `plot_qq()`:
      ! Argument `.var` is required

---

    Code
      plot_qq(nhefs_weights, age)
    Condition <halfmoon_arg_error>
      Error in `plot_qq()`:
      ! Argument `.exposure` is required

# plot_qq errors with missing columns

    Code
      plot_qq(nhefs_weights, missing_var, qsmk)
    Condition <halfmoon_column_error>
      Error in `plot_qq()`:
      ! Column `missing_var` not found in data

---

    Code
      plot_qq(nhefs_weights, age, missing_group)
    Condition <halfmoon_column_error>
      Error in `plot_qq()`:
      ! Column `missing_group` not found in data

# plot_qq errors with non-binary groups

    Code
      plot_qq(df, age, three_groups)
    Condition <halfmoon_group_error>
      Error in `plot_qq()`:
      ! Exposure variable must have exactly 2 levels

# plot_qq handles NA values

    Code
      plot_qq(df, age, qsmk)
    Condition <halfmoon_na_error>
      Error in `plot_qq()`:
      ! Variable contains missing values. Use `na.rm = TRUE` to drop them.


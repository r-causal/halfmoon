# plot_qq validates missing arguments

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_qq()`:
      ! Argument `.var` is required

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_qq()`:
      ! Argument `.group` is required

# plot_qq errors with missing columns

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_qq()`:
      ! Column `missing_var` not found in data

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_qq()`:
      ! Column `missing_group` not found in data

# plot_qq errors with non-binary groups

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `plot_qq()`:
      ! Group variable must have exactly 2 levels

# plot_qq handles NA values

    Code
      expr
    Condition <halfmoon_na_error>
      Error in `plot_qq()`:
      ! Variable contains missing values. Use `na.rm = TRUE` to drop them.


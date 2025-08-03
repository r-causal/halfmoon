# plot_mirror_distributions handles NA values

    Code
      expr
    Condition <halfmoon_na_error>
      Error in `plot_mirror_distributions()`:
      ! Variable contains missing values. Use `na.rm = TRUE` to drop them.

# plot_mirror_distributions validates inputs

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_mirror_distributions()`:
      ! Argument `.var` is required

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_mirror_distributions()`:
      ! Argument `.group` is required

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_mirror_distributions()`:
      ! Column `nonexistent` not found in `.var`

---

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `plot_mirror_distributions()`:
      ! Group variable must have at least two levels

# plot_mirror_distributions validates categorical reference group

    Code
      expr
    Condition <halfmoon_reference_error>
      Error in `plot_mirror_distributions()`:
      ! `reference_group` "invalid" not found in grouping variable

---

    Code
      expr
    Condition <halfmoon_range_error>
      Error in `plot_mirror_distributions()`:
      ! Reference group index 10 out of bounds


# plot_mirror_distributions handles NA values

    Code
      plot_mirror_distributions(df_with_na, age, qsmk)
    Condition <halfmoon_na_error>
      Error in `plot_mirror_distributions()`:
      ! Variable contains missing values. Use `na.rm = TRUE` to drop them.

# plot_mirror_distributions validates inputs

    Code
      plot_mirror_distributions(nhefs_weights)
    Condition <halfmoon_arg_error>
      Error in `plot_mirror_distributions()`:
      ! Argument `.var` is required

---

    Code
      plot_mirror_distributions(nhefs_weights, age)
    Condition <halfmoon_arg_error>
      Error in `plot_mirror_distributions()`:
      ! Argument `.exposure` is required

---

    Code
      plot_mirror_distributions(nhefs_weights, nonexistent, qsmk)
    Condition <halfmoon_column_error>
      Error in `plot_mirror_distributions()`:
      ! Column `nonexistent` not found in `.var`

---

    Code
      plot_mirror_distributions(df_one_level, age, qsmk)
    Condition <halfmoon_group_error>
      Error in `plot_mirror_distributions()`:
      ! Exposure variable must have at least two levels

# plot_mirror_distributions validates categorical reference group

    Code
      plot_mirror_distributions(nhefs_weights, age, alcoholfreq_cat,
        .reference_level = "invalid")
    Condition <halfmoon_reference_error>
      Error in `plot_mirror_distributions()`:
      ! `.reference_level` "invalid" not found in grouping variable

---

    Code
      plot_mirror_distributions(nhefs_weights, age, alcoholfreq_cat,
        .reference_level = 10)
    Condition <halfmoon_range_error>
      Error in `plot_mirror_distributions()`:
      ! .reference_level index 10 out of bounds


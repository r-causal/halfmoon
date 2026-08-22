# check_qq errors with missing columns

    Code
      check_qq(nhefs_weights, missing_var, qsmk)
    Condition <halfmoon_column_error>
      Error in `check_qq()`:
      ! Column `missing_var` not found in data

---

    Code
      check_qq(nhefs_weights, age, missing_group)
    Condition <halfmoon_column_error>
      Error in `check_qq()`:
      ! Column `missing_group` not found in data

# check_qq errors with non-binary groups

    Code
      check_qq(df, age, three_groups)
    Condition <halfmoon_group_error>
      Error in `check_qq()`:
      ! Exposure variable must have exactly 2 levels

# check_qq handles NA values correctly

    Code
      check_qq(df, age, qsmk)
    Condition <halfmoon_na_error>
      Error in `check_qq()`:
      ! Variable `age` contains missing values and `na.rm = FALSE`


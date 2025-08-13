# check_qq errors with missing columns

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `check_qq()`:
      ! Column `missing_var` not found in data

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `check_qq()`:
      ! Column `missing_group` not found in data

# check_qq errors with non-binary groups

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `check_qq()`:
      ! Exposure variable must have exactly 2 levels

# check_qq handles NA values correctly

    Code
      expr
    Condition <halfmoon_na_error>
      Error in `check_qq()`:
      ! Variable `age` contains missing values and `na.rm = FALSE`


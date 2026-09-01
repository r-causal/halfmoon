# bal_qq handles missing values

    Code
      bal_qq(nhefs_na, age, qsmk, na.rm = FALSE)
    Condition <halfmoon_na_error>
      Error in `bal_qq()`:
      ! Variable `age` contains missing values and `na.rm = FALSE`

# bal_qq validates inputs

    Code
      bal_qq(nhefs_weights, nonexistent, qsmk)
    Condition <halfmoon_column_error>
      Error in `bal_qq()`:
      ! Column `nonexistent` not found in `.var`

---

    Code
      bal_qq(nhefs_weights, age, nonexistent)
    Condition <halfmoon_column_error>
      Error in `bal_qq()`:
      ! Column `nonexistent` not found in `.exposure`

---

    Code
      bal_qq(nhefs_weights, age, alcoholfreq_cat)
    Condition <halfmoon_group_error>
      Error in `bal_qq()`:
      ! Exposure variable must have exactly two levels, got 5

---

    Code
      bal_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))
    Condition <halfmoon_arg_error>
      Error in `bal_qq()`:
      ! `.weights` must select exactly one variable or be NULL

# bal_qq works with different treatment levels

    Code
      bal_qq(nhefs_weights, age, qsmk, .reference_level = "invalid")
    Condition <halfmoon_reference_error>
      Error in `bal_qq()`:
      ! `.reference_level` "invalid" not found in grouping variable

---

    Code
      bal_qq(nhefs_weights, age, qsmk, .reference_level = 3)
    Condition <halfmoon_range_error>
      Error in `bal_qq()`:
      ! .reference_level index 3 out of bounds

# bal_qq validates missing weights

    Code
      bal_qq(df, x, g, .weights = w)
    Condition <halfmoon_na_error>
      Error in `bal_qq()`:
      ! Weight variable `w` contains missing values and `na.rm = FALSE`


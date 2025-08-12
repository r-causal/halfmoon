# bal_qq handles missing values

    Code
      expr
    Condition <halfmoon_na_error>
      Error in `bal_qq()`:
      ! Variable `age` contains missing values and `na.rm = FALSE`

# bal_qq validates inputs

    Code
      expr
    Condition <halfmoon_column_error>
      Error:
      ! Column `nonexistent` not found in `.var`

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error:
      ! Column `nonexistent` not found in `.group`

---

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `bal_qq()`:
      ! Group variable must have exactly two levels, got 5

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `bal_qq()`:
      ! `.wts` must select exactly one variable or be NULL

# bal_qq works with different treatment levels

    Code
      expr
    Condition <halfmoon_reference_error>
      Error in `bal_qq()`:
      ! `treatment_level` '2' not found in `.group` levels: "0" and "1"


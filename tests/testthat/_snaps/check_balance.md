# check_balance validates inputs correctly

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_balance()`:
      ! `.data` must be a data frame

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `check_balance()`:
      ! Column `nonexistent_group` not found in `data`

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `check_balance()`:
      ! Invalid metric: "invalid_metric"
      i Available metrics: "smd", "vr", "ks", "correlation", and "energy"

---

    Code
      expr
    Condition <halfmoon_empty_error>
      Error in `check_balance()`:
      ! No variables selected for `.vars`

---

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `check_balance()`:
      ! Exposure variable must have at least two levels for metrics: "smd", "vr", "ks", and "energy". Got 1 level.

# check_balance correlation requires numeric group variable

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_balance()`:
      ! Exposure variable must be numeric when using correlation metric

# check_balance handles mixed metrics with correlation

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `check_balance()`:
      ! Exposure variable must be numeric when using correlation metric


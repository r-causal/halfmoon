# check_ess validates inputs

    Code
      check_ess("not a data frame")
    Condition <halfmoon_type_error>
      Error in `check_ess()`:
      ! `.data` must be a data frame

---

    Code
      check_ess(nhefs_weights, .exposure = "not_a_column")
    Condition <halfmoon_column_error>
      Error in `check_ess()`:
      ! Column `not_a_column` not found in `.exposure`

---

    Code
      check_ess(nhefs_weights, .weights = w_ate, .exposure = age, n_tiles = 3,
        tile_labels = c("Too", "Few"))
    Condition <halfmoon_length_error>
      Error in `check_ess()`:
      ! Length of `tile_labels` must equal `n_tiles`


# bal_smd error handling

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `bal_smd()`:
      ! Group variable must have exactly two levels, got 1

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_smd()`:
      ! `.covariate` and `.exposure` must have the same length

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_smd()`:
      ! `.weights` must have length 100, got 50

# bal_vr error handling

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `split_by_group()`:
      ! Group variable must have exactly two levels, got 1

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_vr()`:
      ! `.covariate` and `.exposure` must have the same length

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_vr()`:
      ! `.weights` must have length 100, got 50

# bal_ks error handling

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `split_by_group()`:
      ! Group variable must have exactly two levels, got 1

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_ks()`:
      ! `.covariate` and `.exposure` must have the same length

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_ks()`:
      ! `.weights` must have length 100, got 50

# bal_corr handles edge cases

    Code
      expr
    Condition <simpleWarning>
      Warning in `stats::cor()`:
      the standard deviation is zero

---

    Code
      expr
    Condition <simpleWarning>
      Warning in `stats::cor()`:
      the standard deviation is zero

# bal_corr error handling

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_corr()`:
      ! `.x` and `.y` must have the same length

---

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_corr()`:
      ! `.weights` must have length 100, got 50

# bal_energy handles continuous treatments

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `bal_energy()`:
      ! For continuous treatments, `estimand` must be `NULL`

# bal_energy handles missing values

    Code
      expr
    Condition <halfmoon_na_error>
      Error in `bal_energy()`:
      ! Energy distance cannot be computed with missing values in `.covariates`. Set `na.rm = TRUE` or remove missing values.

# bal_energy error handling

    Code
      expr
    Condition <halfmoon_length_error>
      Error in `bal_energy()`:
      ! `.exposure` and `.covariates` must have the same length

---

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `bal_energy()`:
      ! Group variable must have at least two levels

---

    Code
      expr
    Condition <halfmoon_range_error>
      Error in `bal_energy()`:
      ! `.weights` cannot contain negative values


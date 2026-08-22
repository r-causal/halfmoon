# Error if `x` is not a tbl_svysummary

    Code
      add_ess_header(1)
    Condition <halfmoon_type_error>
      Error in `add_ess_header()`:
      ! Argument `x` must be class <tbl_svysummary> and typically created with `gtsummary::tbl_svysummary()`.

# Error if `header` is not a string

    Code
      add_ess_header(tbl, header = 123)
    Condition <halfmoon_type_error>
      Error in `add_ess_header()`:
      ! Argument `header` must be a string.


# get_column_name reports the user-facing call when a value is not a column name

    Code
      check_qq(nhefs_weights, .var = c(1, 2), .exposure = qsmk)
    Condition <halfmoon_type_error>
      Error in `check_qq()`:
      ! `.var` must be a column name (quoted or unquoted)

# get_column_name reports the user-facing call when the argument cannot be evaluated

    Code
      check_qq(nhefs_weights, .var = no_such_function(), .exposure = qsmk)
    Condition <halfmoon_type_error>
      Error in `check_qq()`:
      ! `.var` must be a column name (quoted or unquoted)


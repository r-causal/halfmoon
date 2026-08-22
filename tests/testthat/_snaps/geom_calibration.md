# check_model_calibration provides clear error messages for missing columns

    Code
      check_model_calibration(test_data, "nonexistent", "obs")
    Condition <halfmoon_column_error>
      Error in `check_model_calibration()`:
      ! Column `nonexistent` not found in data

---

    Code
      check_model_calibration(test_data, "pred", "nonexistent")
    Condition <halfmoon_column_error>
      Error in `check_model_calibration()`:
      ! Column `nonexistent` not found in data

# check_model_calibration method parameter validation

    Code
      check_model_calibration(test_data, pred, obs, method = "invalid")
    Condition <rlang_error>
      Error in `check_model_calibration()`:
      ! `method` must be one of "breaks", "logistic", or "windowed", not "invalid".

# check_model_calibration validates input parameters

    Code
      check_model_calibration(test_data, pred, obs, method = "breaks", bins = 1)
    Condition <halfmoon_arg_error>
      Error in `check_model_calibration()`:
      ! `bins` must be an integer > 1.

---

    Code
      check_model_calibration(test_data, pred, obs, method = "breaks", bins = 2.5)
    Condition <halfmoon_arg_error>
      Error in `check_model_calibration()`:
      ! `bins` must be an integer > 1.

---

    Code
      check_model_calibration(test_data, nonexistent, obs)
    Condition <halfmoon_column_error>
      Error in `check_model_calibration()`:
      ! Column `nonexistent` not found in data

# check_model_calibration provides helpful warnings for small cell sizes

    Code
      check_model_calibration(test_data, pred, obs, method = "breaks", bins = 10)
    Condition <halfmoon_data_warning>
      Warning in `check_model_calibration()`:
      Small sample sizes or extreme proportions detected in bins 6, 10 (n = 2, 10). Confidence intervals may be unreliable. Consider using fewer bins or a different calibration method.
    Output
      # A tibble: 7 x 6
         .bin predicted_rate observed_rate count  lower upper
        <int>          <dbl>         <dbl> <int>  <dbl> <dbl>
      1     1         0.0493         0.438    16 0.208  0.694
      2     2         0.145          0.222    18 0.0737 0.481
      3     3         0.236          0.167    18 0.0441 0.423
      4     4         0.340          0.2      15 0.0531 0.486
      5     5         0.434          0.333    21 0.155  0.569
      6     6         0.495          0         2 0      0.802
      7    10         0.941          0.2      10 0.0354 0.558

# check_model_calibration provides helpful warnings for extreme proportions

    Code
      check_model_calibration(test_data, pred, obs, method = "breaks", bins = 10)
    Condition <halfmoon_data_warning>
      Warning in `check_model_calibration()`:
      Small sample sizes or extreme proportions detected in bins 1, 2, 3, 8, 9, 10 (n = 17, 16, 17, 16, 17, 17). Confidence intervals may be unreliable. Consider using fewer bins or a different calibration method.
    Output
      # A tibble: 6 x 6
         .bin predicted_rate observed_rate count lower upper
        <int>          <dbl>         <dbl> <int> <dbl> <dbl>
      1     1         0.0567             0    17 0     0.229
      2     2         0.154              0    16 0     0.241
      3     3         0.257              0    17 0     0.229
      4     8         0.746              1    16 0.759 1    
      5     9         0.840              1    17 0.771 1    
      6    10         0.938              1    17 0.771 1    

# check_model_calibration windowed method provides helpful warnings

    Code
      check_model_calibration(test_data, pred, obs, method = "windowed", window_size = 0.05,
        step_size = 0.1)
    Condition <halfmoon_data_warning>
      Warning in `check_model_calibration()`:
      Small sample sizes or extreme proportions detected in windows centered at 0.4, 0.5, 0.6 (n = 3, 6, 6). Confidence intervals may be unreliable. Consider using a larger window size or a different calibration method.
    Output
      # A tibble: 3 x 4
        predicted_rate observed_rate  lower upper
                 <dbl>         <dbl>  <dbl> <dbl>
      1            0.4         0.333 0.0177 0.875
      2            0.5         0.667 0.241  0.940
      3            0.6         0.667 0.241  0.940

# check_model_calibration errors with invalid bins

    Code
      check_model_calibration(cal_data, pred, obs, bins = 1)
    Condition <halfmoon_arg_error>
      Error in `check_model_calibration()`:
      ! `bins` must be an integer > 1.

---

    Code
      check_model_calibration(cal_data, pred, obs, bins = 2.5)
    Condition <halfmoon_arg_error>
      Error in `check_model_calibration()`:
      ! `bins` must be an integer > 1.

# check_model_calibration reports an absent focal level clearly

    Code
      check_model_calibration(test_data, pred, obs, .focal_level = 99)
    Condition <halfmoon_reference_error>
      Error in `check_model_calibration()`:
      ! `.focal_level` 99 not found in `.exposure`
      i Observed levels: 0 and 1


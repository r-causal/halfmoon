# bal_smd error handling

    Code
      bal_smd(.covariate = data$x_cont, .exposure = rep(1, 100))
    Condition <halfmoon_group_error>
      Error in `bal_smd()`:
      ! Exposure variable must have exactly two levels, got 1

---

    Code
      bal_smd(.covariate = data$x_cont[1:50], .exposure = data$g_balanced)
    Condition <halfmoon_length_error>
      Error in `bal_smd()`:
      ! `.covariate` and `.exposure` must have the same length

---

    Code
      bal_smd(.covariate = data$x_cont, .exposure = data$g_balanced, .weights = data$
        w_uniform[1:50])
    Condition <halfmoon_length_error>
      Error in `bal_smd()`:
      ! `.weights` must have length 100, got 50

# bal_vr error handling

    Code
      bal_vr(.covariate = data$x_cont, .exposure = rep(1, 100))
    Condition <halfmoon_group_error>
      Error in `bal_vr()`:
      ! Exposure variable must have exactly two levels, got 1

---

    Code
      bal_vr(.covariate = data$x_cont[1:50], .exposure = data$g_balanced)
    Condition <halfmoon_length_error>
      Error in `bal_vr()`:
      ! `.covariate` and `.exposure` must have the same length

---

    Code
      bal_vr(.covariate = data$x_cont, .exposure = data$g_balanced, .weights = data$
        w_uniform[1:50])
    Condition <halfmoon_length_error>
      Error in `bal_vr()`:
      ! `.weights` must have length 100, got 50

# bal_ks error handling

    Code
      bal_ks(.covariate = data$x_cont, .exposure = rep(1, 100))
    Condition <halfmoon_group_error>
      Error in `bal_ks()`:
      ! Exposure variable must have exactly two levels, got 1

---

    Code
      bal_ks(.covariate = data$x_cont[1:50], .exposure = data$g_balanced)
    Condition <halfmoon_length_error>
      Error in `bal_ks()`:
      ! `.covariate` and `.exposure` must have the same length

---

    Code
      bal_ks(.covariate = data$x_cont, .exposure = data$g_balanced, .weights = data$
        w_uniform[1:50])
    Condition <halfmoon_length_error>
      Error in `bal_ks()`:
      ! `.weights` must have length 100, got 50

# bal_corr handles edge cases

    Code
      cor_zero <- bal_corr(x_zero, y_normal)
    Condition <simpleWarning>
      Warning in `stats::cor()`:
      the standard deviation is zero

---

    Code
      cor_both_zero <- bal_corr(x_zero, y_zero)
    Condition <simpleWarning>
      Warning in `stats::cor()`:
      the standard deviation is zero

# bal_corr error handling

    Code
      bal_corr(data$x_cont[1:50], data$x_skewed)
    Condition <halfmoon_length_error>
      Error in `bal_corr()`:
      ! `.x` and `.y` must have the same length

---

    Code
      bal_corr(data$x_cont, data$x_skewed, .weights = data$w_uniform[1:50])
    Condition <halfmoon_length_error>
      Error in `bal_corr()`:
      ! `.weights` must have length 100, got 50

# bal_energy handles continuous treatments

    Code
      bal_energy(.covariates = covs, .exposure = continuous_treatment, estimand = "ATE")
    Condition <halfmoon_arg_error>
      Error in `bal_energy()`:
      ! For continuous treatments, `estimand` must be `NULL`

# bal_energy error handling

    Code
      bal_energy(.covariates = data.frame(x = data$x_cont[1:50]), .exposure = data$
        g_balanced)
    Condition <halfmoon_length_error>
      Error in `bal_energy()`:
      ! `.exposure` and `.covariates` must have the same length

---

    Code
      bal_energy(.covariates = data.frame(x = data$x_cont), .exposure = rep(1, 100))
    Condition <halfmoon_group_error>
      Error in `bal_energy()`:
      ! Exposure variable must have at least two levels

---

    Code
      bal_energy(.covariates = data.frame(x = data$x_cont), .exposure = data$
        g_balanced, .weights = c(-1, rep(1, 99)))
    Condition <halfmoon_range_error>
      Error in `bal_energy()`:
      ! `.weights` cannot contain negative values


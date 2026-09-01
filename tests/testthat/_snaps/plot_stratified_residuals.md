# plot_stratified_residuals validates inputs correctly

    Code
      plot_stratified_residuals(model)
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `.exposure` is required

---

    Code
      plot_stratified_residuals(model, .exposure = rep(0:1, 16), ps_model = "not a model")
    Condition <halfmoon_type_error>
      Error in `plot_stratified_residuals()`:
      ! `ps_model` must be a glm or lm object

---

    Code
      plot_stratified_residuals(df)
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `.exposure` is required

---

    Code
      plot_stratified_residuals(df, .exposure = trt, residuals = resids)
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `x_var` is required

---

    Code
      plot_stratified_residuals(df, .exposure = trt, residuals = resids, x_var = not_a_column)
    Condition <halfmoon_column_error>
      Error in `plot_stratified_residuals()`:
      ! Column `not_a_column` not found in `x_var`

---

    Code
      plot_stratified_residuals(df_wrong, .exposure = trt, residuals = resids, x_var = x)
    Condition <halfmoon_group_error>
      Error in `plot_stratified_residuals()`:
      ! `.exposure` must have exactly two levels, got 3


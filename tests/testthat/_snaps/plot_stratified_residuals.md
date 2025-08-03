# plot_stratified_residuals validates inputs correctly

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `treatment` is required

---

    Code
      expr
    Condition <halfmoon_type_error>
      Error in `plot_stratified_residuals()`:
      ! `ps_model` must be a glm or lm object

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `treatment` is required

---

    Code
      expr
    Condition <halfmoon_arg_error>
      Error in `plot_stratified_residuals()`:
      ! Argument `x_var` is required

---

    Code
      expr
    Condition <halfmoon_column_error>
      Error in `plot_stratified_residuals()`:
      ! Column `not_a_column` not found in `x_var`

---

    Code
      expr
    Condition <halfmoon_group_error>
      Error in `plot_stratified_residuals_impl()`:
      ! `.treatment` must have exactly two levels, got 3


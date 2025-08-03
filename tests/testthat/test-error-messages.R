
test_that("error messages show user-facing function names", {
  # Test plot_mirror_distributions with invalid reference group
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    plot_mirror_distributions(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      reference_group = "invalid"
    )
  )
  
  # Test with numeric out of bounds reference group
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    plot_mirror_distributions(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      reference_group = 10
    )
  )
  
  # Test missing required arguments
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    plot_mirror_distributions(nhefs_weights)
  )
  
  # Test with missing column
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    plot_mirror_distributions(
      nhefs_weights,
      missing_column,
      qsmk
    )
  )
  
  # Test plot_qq with invalid treatment level
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    plot_qq(
      nhefs_weights,
      age,
      qsmk,
      treatment_level = "invalid"
    )
  )
  
  # Test plot_stratified_residuals with missing treatment
  model <- lm(mpg ~ wt, data = mtcars)
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    plot_stratified_residuals(model)
  )
  
  # Test check_balance with wrong group levels
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    check_balance(
      nhefs_weights,
      .vars = age,
      .group = rep(1, nrow(nhefs_weights))
    )
  )
  
  # Test bal_prognostic_score with treatment in formula
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    bal_prognostic_score(
      nhefs_weights,
      treatment = qsmk,
      formula = wt82_71 ~ age + qsmk + wt71
    )
  )
})

test_that("validation errors show correct function context", {
  # Test numeric validation
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    check_balance(
      nhefs_weights,
      .vars = age,
      .group = "not_numeric"
    )
  )
  
  # Test empty data frame
  expect_snapshot(
    error = TRUE,
    cnd_class = TRUE,
    check_balance(
      data.frame(),
      .vars = age,
      .group = qsmk
    )
  )
})

test_that("errors have correct custom classes", {
  # Test reference error class
  expect_halfmoon_error(
    plot_mirror_distributions(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      reference_group = "invalid"
    ),
    class = "halfmoon_reference_error"
  )
  
  # Test range error class
  expect_halfmoon_error(
    plot_mirror_distributions(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      reference_group = 10
    ),
    class = "halfmoon_range_error"
  )
  
  # Test arg error class
  expect_halfmoon_error(
    plot_mirror_distributions(nhefs_weights),
    class = "halfmoon_arg_error"
  )
  
  # Test column error class
  expect_halfmoon_error(
    plot_mirror_distributions(
      nhefs_weights,
      missing_column,
      qsmk
    ),
    class = "halfmoon_column_error"
  )
  
  # Test formula error class
  expect_halfmoon_error(
    bal_prognostic_score(
      nhefs_weights,
      treatment = qsmk,
      formula = wt82_71 ~ age + qsmk + wt71
    ),
    class = "halfmoon_formula_error"
  )
})


test_that("error messages show user-facing function names", {
  # Test plot_mirror_distributions with invalid reference group
  expect_snapshot(
    error = TRUE,
    plot_mirror_distributions(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      reference_group = "invalid"
    )
  )
  
  # Test with numeric out of bounds reference group
  expect_snapshot_error(
    plot_mirror_distributions(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      reference_group = 10
    )
  )
  
  # Test missing required arguments
  expect_snapshot_error(
    plot_mirror_distributions(nhefs_weights)
  )
  
  # Test with missing column
  expect_snapshot_error(
    plot_mirror_distributions(
      nhefs_weights,
      missing_column,
      qsmk
    )
  )
  
  # Test plot_qq with invalid treatment level
  expect_snapshot_error(
    plot_qq(
      nhefs_weights,
      age,
      qsmk,
      treatment_level = "invalid"
    )
  )
  
  # Test plot_stratified_residuals with missing treatment
  model <- lm(mpg ~ wt, data = mtcars)
  expect_snapshot_error(
    plot_stratified_residuals(model)
  )
  
  # Test check_balance with wrong group levels
  expect_snapshot_error(
    check_balance(
      nhefs_weights,
      qsmk,
      age,
      .group = rep(1, nrow(nhefs_weights))
    )
  )
  
  # Test bal_prognostic_score with treatment in formula
  expect_snapshot_error(
    bal_prognostic_score(
      nhefs_weights,
      treatment = qsmk,
      formula = wt82_71 ~ age + qsmk + wt71
    )
  )
})

test_that("validation errors show correct function context", {
  # Test numeric validation
  expect_snapshot_error(
    check_balance(
      nhefs_weights,
      qsmk,
      age,
      .group = "not_numeric"
    )
  )
  
  # Test weight validation with wrong length
  # Create a separate data frame to avoid modifying nhefs_weights
  test_df <- nhefs_weights[1:100, ]
  bad_weights <- rep(1, 10)
  expect_snapshot_error(
    check_balance(
      test_df,
      .vars = age,
      .group = qsmk,
      .wts = list(bad_wts = bad_weights)
    )
  )
  
  # Test empty data frame
  expect_snapshot_error(
    check_balance(
      data.frame(),
      qsmk,
      age
    )
  )
})


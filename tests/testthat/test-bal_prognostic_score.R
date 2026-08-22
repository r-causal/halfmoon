test_that("bal_prognostic_score works with tidyselect interface", {
  # Test with unquoted column names
  scores <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex, wt71)
  )

  expect_type(scores, "double")
  expect_length(scores, nrow(nhefs_weights))
  expect_false(anyNA(scores[!is.na(nhefs_weights$wt82_71)]))

  # Test with quoted column names
  scores_quoted <- bal_prognostic_score(
    nhefs_weights,
    outcome = "wt82_71",
    .exposure = "qsmk",
    .covariates = c("age", "sex", "wt71")
  )

  expect_equal(scores, scores_quoted)

  # Test with tidyselect helpers
  scores2 <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = matches("^(age|sex|race|education)$")
  )

  expect_type(scores2, "double")
  expect_length(scores2, nrow(nhefs_weights))
})

test_that("bal_prognostic_score works with formula interface", {
  # Formula interface
  scores_formula <- bal_prognostic_score(
    nhefs_weights,
    .exposure = qsmk,
    formula = wt82_71 ~ age + sex + wt71
  )

  expect_type(scores_formula, "double")
  expect_length(scores_formula, nrow(nhefs_weights))
})

test_that("bal_prognostic_score validates treatment not in formula", {
  expect_halfmoon_error(
    bal_prognostic_score(
      nhefs_weights,
      .exposure = qsmk,
      formula = wt82_71 ~ age + qsmk + wt71
    ),
    "halfmoon_formula_error"
  )
})

test_that("bal_prognostic_score handles different families", {
  # Binary outcome
  scores_binary <- bal_prognostic_score(
    nhefs_weights,
    outcome = death,
    .exposure = qsmk,
    .covariates = c(age, sex),
    family = binomial()
  )

  expect_type(scores_binary, "double")
  expect_true(all(scores_binary >= 0 & scores_binary <= 1, na.rm = TRUE))
})

test_that("bal_prognostic_score handles weights", {
  scores_weighted <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex),
    .weights = w_ate
  )

  expect_type(scores_weighted, "double")
  expect_length(scores_weighted, nrow(nhefs_weights))

  # Also test with quoted weights
  scores_weighted2 <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex),
    .weights = "w_ate"
  )

  expect_equal(scores_weighted, scores_weighted2)
})

test_that("bal_prognostic_score handles .reference_level parameter", {
  # Specify control level explicitly
  scores_ref <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex),
    .reference_level = 0
  )

  expect_type(scores_ref, "double")
  expect_length(scores_ref, nrow(nhefs_weights))
})

test_that("bal_prognostic_score handles na.rm parameter correctly", {
  # Create data with NAs in specific rows
  data_with_na <- nhefs_weights
  na_indices <- c(1, 5, 10, 15, 20) # Specific rows to set as NA
  data_with_na$wt82_71[na_indices] <- NA

  # With na.rm - should exclude rows with NA outcomes
  scores_na_rm <- bal_prognostic_score(
    data_with_na,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex),
    na.rm = TRUE
  )

  expect_type(scores_na_rm, "double")
  # Should have fewer rows (excluded the NA rows)
  expect_length(scores_na_rm, nrow(data_with_na) - length(na_indices))

  # Test with NAs in covariates
  data_with_na_cov <- nhefs_weights
  cov_na_indices <- c(2, 7, 12)
  data_with_na_cov$age[cov_na_indices] <- NA

  # With na.rm - should exclude rows with NA in any model variable
  scores_cov_na_rm <- bal_prognostic_score(
    data_with_na_cov,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex),
    na.rm = TRUE
  )
  expect_length(
    scores_cov_na_rm,
    nrow(data_with_na_cov) - length(cov_na_indices)
  )
})

test_that("bal_prognostic_score errors with no control observations", {
  # Create data with only treated units
  treated_only <- nhefs_weights[nhefs_weights$qsmk == 1, ]

  expect_halfmoon_error(
    bal_prognostic_score(
      treated_only,
      outcome = wt82_71,
      .exposure = qsmk,
      .covariates = c(age, sex)
    ),
    "halfmoon_group_error"
  )
})

test_that("bal_prognostic_score errors when required arguments missing", {
  # No outcome or formula
  expect_halfmoon_error(
    bal_prognostic_score(
      nhefs_weights,
      .exposure = qsmk,
      .covariates = c(age, sex)
    ),
    "halfmoon_arg_error"
  )

  # Invalid formula
  expect_halfmoon_error(
    bal_prognostic_score(
      nhefs_weights,
      .exposure = qsmk,
      formula = "not a formula"
    ),
    "halfmoon_formula_error"
  )
})

test_that("bal_prognostic_score handles everything() selector", {
  # Select all variables except outcome and treatment
  scores_all <- bal_prognostic_score(
    nhefs_weights[, c("wt82_71", "qsmk", "age", "sex", "wt71")],
    outcome = wt82_71,
    .exposure = qsmk
  )

  expect_type(scores_all, "double")
  expect_length(scores_all, nrow(nhefs_weights))
})

test_that("bal_prognostic_score integrates with balance functions", {
  # Compute prognostic scores
  prog_scores <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex, wt71)
  )

  # Add to data
  test_data <- nhefs_weights
  test_data$prog_score <- prog_scores

  # Check balance using existing functions
  balance_smd <- bal_smd(
    .covariate = test_data$prog_score,
    .exposure = test_data$qsmk,
    .weights = test_data$w_ate
  )

  expect_type(balance_smd, "double")

  # Check with check_balance
  balance_check <- check_balance(
    test_data,
    prog_score,
    qsmk,
    .weights = w_ate
  )

  expect_s3_class(balance_check, "tbl_df")
  expect_true("prog_score" %in% balance_check$variable)
})

test_that("bal_prognostic_score produces reasonable predictions", {
  scores <- bal_prognostic_score(
    nhefs_weights,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex, wt71)
  )

  # Scores should be in reasonable range for weight change
  expect_true(all(scores > -50 & scores < 50, na.rm = TRUE))

  # Should have variation
  expect_true(sd(scores, na.rm = TRUE) > 0)
})

test_that("bal_prognostic_score handles formula with transformations", {
  # Formula with polynomial and interaction terms
  scores_complex <- bal_prognostic_score(
    nhefs_weights,
    .exposure = qsmk,
    formula = wt82_71 ~ age + I(age^2) + sex + wt71 + age:sex
  )

  expect_type(scores_complex, "double")
  expect_length(scores_complex, nrow(nhefs_weights))

  # Should produce different results than simple model
  scores_simple <- bal_prognostic_score(
    nhefs_weights,
    .exposure = qsmk,
    formula = wt82_71 ~ age + sex + wt71
  )

  expect_false(all(scores_complex == scores_simple))
})

test_that("bal_prognostic_score respects the declared order of exposure levels", {
  set.seed(123)
  n <- 60
  reversed <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    g = factor(
      rep(c("treat", "ctrl"), length.out = n),
      levels = c("treat", "ctrl")
    )
  )

  scores <- bal_prognostic_score(
    reversed,
    outcome = y,
    .exposure = g,
    .covariates = x
  )

  fit_on_first_declared <- stats::glm(
    y ~ x,
    data = reversed[reversed$g == "treat", ]
  )
  expected <- as.numeric(stats::predict(
    fit_on_first_declared,
    newdata = reversed,
    type = "response"
  ))

  expect_equal(scores, expected)
})

test_that("bal_prognostic_score names the control level that is absent", {
  set.seed(123)
  n <- 40
  treated_only <- data.frame(
    y = rnorm(n),
    x = rnorm(n),
    g = factor(rep("treat", n), levels = c("ctrl", "treat"))
  )

  expect_halfmoon_error(
    bal_prognostic_score(
      treated_only,
      outcome = y,
      .exposure = g,
      .covariates = x
    ),
    "halfmoon_group_error"
  )
})

test_that("bal_prognostic_score drops missing values by default", {
  expect_identical(formals(bal_prognostic_score)$na.rm, TRUE)

  data_with_na <- nhefs_weights
  na_indices <- c(1, 5, 10, 15, 20)
  data_with_na$wt82_71[na_indices] <- NA

  scores <- bal_prognostic_score(
    data_with_na,
    outcome = wt82_71,
    .exposure = qsmk,
    .covariates = c(age, sex)
  )

  expect_length(scores, nrow(data_with_na) - length(na_indices))
  expect_false(anyNA(scores))
})

test_that("bal_prognostic_score refuses missing values when na.rm is FALSE", {
  data_with_na <- nhefs_weights
  data_with_na$wt82_71[c(1, 5, 10)] <- NA

  expect_error(
    bal_prognostic_score(
      data_with_na,
      outcome = wt82_71,
      .exposure = qsmk,
      .covariates = c(age, sex),
      na.rm = FALSE
    ),
    class = "halfmoon_na_error"
  )

  data_with_na_weights <- nhefs_weights
  data_with_na_weights$w_ate[1:3] <- NA

  expect_error(
    bal_prognostic_score(
      data_with_na_weights,
      outcome = wt82_71,
      .exposure = qsmk,
      .covariates = c(age, sex),
      .weights = w_ate,
      na.rm = FALSE
    ),
    class = "halfmoon_na_error"
  )
})

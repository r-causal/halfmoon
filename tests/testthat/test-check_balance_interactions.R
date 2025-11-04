test_that("check_balance does not create interactions between levels of same variable", {
  # Create test data with various factor types
  test_data <- data.frame(
    # Binary factors with text levels
    sex = factor(c(rep("male", 50), rep("female", 50))),
    race = factor(c(rep("white", 40), rep("black", 60))),
    employment = factor(c(rep("employed", 45), rep("unemployed", 55))),
    # Binary factor with numeric-like levels
    treatment = factor(c(rep("0", 50), rep("1", 50))),
    # Continuous variable
    age = rnorm(100, 50, 10),
    # Multi-level factor
    education = factor(sample(c("high", "medium", "low"), 100, replace = TRUE)),
    # Outcome
    outcome = c(rep(0, 50), rep(1, 50))
  )

  # Run check_balance with interactions
  result <- check_balance(
    test_data,
    c(sex, race, employment, age, education),
    outcome,
    interactions = TRUE,
    .metrics = "smd"
  )

  # Get all interaction variables
  interaction_vars <- unique(result$variable)[grepl(
    "_x_",
    unique(result$variable)
  )]

  # Check that no self-interactions exist

  # Sex should not interact with itself
  sex_self <- interaction_vars[grepl("sex.*_x_sex", interaction_vars)]
  expect_length(sex_self, 0)

  # Race should not interact with itself
  race_self <- interaction_vars[grepl("race.*_x_race", interaction_vars)]
  expect_length(race_self, 0)

  # Employment should not interact with itself
  employment_self <- interaction_vars[grepl(
    "employ.*_x_employ",
    interaction_vars
  )]
  expect_length(employment_self, 0)

  # Education (multi-level) should not interact with itself
  education_self <- interaction_vars[grepl(
    "education.*_x_education",
    interaction_vars
  )]
  expect_length(education_self, 0)

  # But different variables should interact
  expect_true(any(grepl("age_x_sex", interaction_vars)))
  expect_true(any(grepl("age_x_race", interaction_vars)))
  expect_true(any(grepl("sex.*_x_race", interaction_vars)))
  expect_true(any(grepl("age_x_education", interaction_vars)))
})

test_that("is_valid_interaction_combo works with mapping", {
  # Direct test of the helper function
  # Note: This requires access to the internal function
  check_combo <- halfmoon:::is_valid_interaction_combo

  # Test with mapping (the primary approach)
  mapping <- list(
    sex0 = "sex",
    sex1 = "sex",
    sexmale = "sex",
    sexfemale = "sex",
    race0 = "race",
    race1 = "race",
    racewhite = "race",
    raceblack = "race",
    employedemployed = "employed",
    employedunemployed = "employed",
    marriedmarried = "married",
    marriedsingle = "married",
    sex_male = "sex",
    sex_female = "sex",
    race_white = "race",
    race_black = "race",
    age = "age",
    educationhigh = "education",
    var0 = "var0",
    var1 = "var1"
  )

  # Same variable - should return FALSE with mapping
  expect_false(check_combo(c("sex0", "sex1"), mapping))
  expect_false(check_combo(c("race0", "race1"), mapping))
  expect_false(check_combo(c("sexmale", "sexfemale"), mapping))
  expect_false(check_combo(c("racewhite", "raceblack"), mapping))
  expect_false(check_combo(
    c("employedemployed", "employedunemployed"),
    mapping
  ))
  expect_false(check_combo(c("marriedmarried", "marriedsingle"), mapping))
  expect_false(check_combo(c("sex_male", "sex_female"), mapping))
  expect_false(check_combo(c("race_white", "race_black"), mapping))

  # Different variables - should return TRUE with mapping
  expect_true(check_combo(c("var0", "var1"), mapping))
  expect_true(check_combo(c("age", "sex0"), mapping))
  expect_true(check_combo(c("sexmale", "racewhite"), mapping))
  expect_true(check_combo(c("age", "educationhigh"), mapping))
  expect_true(check_combo(c("sex0", "race1"), mapping))
  expect_true(check_combo(c("employedemployed", "sexmale"), mapping))

  # Test fallback to regex when no mapping provided
  # The regex approach is less sophisticated and only handles numeric suffixes
  expect_false(check_combo(c("sex0", "sex1"), NULL))
  expect_false(check_combo(c("race0", "race1"), NULL))
})

test_that("interaction creation handles edge cases correctly", {
  # Test with variables that might confuse the pattern matching
  tricky_data <- data.frame(
    # Variable names that contain suffixes within them
    female_headed = factor(c(rep("yes", 50), rep("no", 50))),
    male_presence = factor(c(rep("present", 60), rep("absent", 40))),
    # Variable that ends with a number
    var1 = rnorm(100),
    var2 = rnorm(100),
    # Outcome
    y = rbinom(100, 1, 0.5)
  )

  result <- check_balance(
    tricky_data,
    c(female_headed, male_presence, var1, var2),
    y,
    interactions = TRUE,
    .metrics = "smd"
  )

  interaction_vars <- unique(result$variable)[grepl(
    "_x_",
    unique(result$variable)
  )]

  # These should create interactions because they're different variables
  expect_true(any(grepl("female_headed.*_x_male_presence", interaction_vars)))
  expect_true(any(grepl("var1_x_var2", interaction_vars)))

  # But not self-interactions
  expect_false(any(grepl("female_headed.*_x_female_headed", interaction_vars)))
  expect_false(any(grepl("male_presence.*_x_male_presence", interaction_vars)))
})

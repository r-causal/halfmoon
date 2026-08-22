test_that("check_qq computes basic quantiles", {
  result <- check_qq(nhefs_weights, age, qsmk)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 99) # 99 quantiles for observed only
  expect_equal(
    colnames(result),
    c("method", "quantile", "exposed_quantiles", "unexposed_quantiles")
  )
  expect_equal(unique(result$method), factor("observed"))
})

test_that("check_qq works with weights", {
  result <- check_qq(nhefs_weights, age, qsmk, .weights = w_ate)

  expect_equal(nrow(result), 198) # 99 quantiles * 2 methods
  expect_equal(levels(result$method), c("observed", "w_ate"))
})

test_that("check_qq works with multiple weights", {
  result <- check_qq(nhefs_weights, age, qsmk, .weights = c(w_ate, w_att))

  expect_equal(nrow(result), 297) # 99 quantiles * 3 methods
  expect_equal(levels(result$method), c("observed", "w_ate", "w_att"))
})

test_that("check_qq works without observed", {
  result <- check_qq(
    nhefs_weights,
    age,
    qsmk,
    .weights = w_ate,
    include_observed = FALSE
  )

  expect_equal(nrow(result), 99) # 99 quantiles * 1 method
  expect_equal(unique(result$method), factor("w_ate"))
})

test_that("check_qq handles custom quantiles", {
  custom_q <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  result <- check_qq(nhefs_weights, age, qsmk, quantiles = custom_q)

  expect_equal(nrow(result), 5)
  expect_equal(unique(result$quantile), custom_q)
})

test_that("check_qq handles quoted column names", {
  result1 <- check_qq(nhefs_weights, age, qsmk)
  result2 <- check_qq(nhefs_weights, "age", "qsmk")

  expect_equal(result1, result2)
})

test_that("check_qq errors with missing columns", {
  expect_halfmoon_error(
    check_qq(nhefs_weights, missing_var, qsmk),
    "halfmoon_column_error"
  )

  expect_halfmoon_error(
    check_qq(nhefs_weights, age, missing_group),
    "halfmoon_column_error"
  )
})

test_that("check_qq errors with non-binary groups", {
  df <- nhefs_weights
  df$three_groups <- rep(1:3, length.out = nrow(df))

  expect_halfmoon_error(
    check_qq(df, age, three_groups),
    "halfmoon_group_error"
  )
})

test_that("check_qq handles NA values correctly", {
  df <- nhefs_weights
  df$age[1:10] <- NA

  # Should work with na.rm = TRUE
  result <- check_qq(df, age, qsmk, na.rm = TRUE)
  expect_false(anyNA(result$exposed_quantiles))
  expect_false(anyNA(result$unexposed_quantiles))

  # Should have NAs with na.rm = FALSE
  expect_halfmoon_error(check_qq(df, age, qsmk), "halfmoon_na_error")
})

test_that("check_qq handles NULL .reference_level correctly", {
  # Test with factor
  test_factor <- data.frame(
    x = 1:10,
    group = factor(rep(c("Control", "Treatment"), each = 5))
  )

  result_factor <- check_qq(test_factor, x, group, quantiles = 0.5)
  # "Control" (first level) is the reference, so the exposed quantiles come
  # from "Treatment"
  expect_equal(as.numeric(result_factor$exposed_quantiles), 8) # median of 6:10
  expect_equal(as.numeric(result_factor$unexposed_quantiles), 3) # median of 1:5

  # Test with numeric
  test_numeric <- data.frame(
    x = 1:10,
    group = rep(c(0, 1), each = 5)
  )

  result_numeric <- check_qq(test_numeric, x, group, quantiles = 0.5)
  # 0 (minimum value) is the reference
  expect_equal(as.numeric(result_numeric$exposed_quantiles), 8) # median of 6:10
  expect_equal(as.numeric(result_numeric$unexposed_quantiles), 3) # median of 1:5
})

test_that("check_qq names the reference group with .reference_level", {
  test_data <- data.frame(
    x = 1:10,
    group = factor(rep(c("Control", "Treatment"), each = 5))
  )

  # The reference group supplies the unexposed quantiles
  result <- check_qq(
    test_data,
    x,
    group,
    quantiles = 0.5,
    .reference_level = "Treatment"
  )
  expect_equal(as.numeric(result$exposed_quantiles), 3) # median of 1:5
  expect_equal(as.numeric(result$unexposed_quantiles), 8) # median of 6:10

  # A numeric reference level is matched by value before position
  numeric_data <- data.frame(x = 1:10, group = rep(c(0, 1), each = 5))
  by_value <- check_qq(
    numeric_data,
    x,
    group,
    quantiles = 0.5,
    .reference_level = 1
  )
  expect_equal(as.numeric(by_value$exposed_quantiles), 3)
  expect_equal(as.numeric(by_value$unexposed_quantiles), 8)

  # A level that is neither a value nor a valid position errors
  expect_error(
    check_qq(test_data, x, group, .reference_level = "nope"),
    class = "halfmoon_reference_error"
  )
  expect_error(
    check_qq(test_data, x, group, .reference_level = 5),
    class = "halfmoon_range_error"
  )
})

test_that("check_qq uses observed exposure levels", {
  df <- data.frame(
    x = c(1:10, 21:30),
    g = factor(rep(c("a", "b"), each = 10), levels = c("a", "b", "c"))
  )
  dropped <- df
  dropped$g <- droplevels(dropped$g)

  expect_equal(
    check_qq(df, x, g, quantiles = c(0.25, 0.75)),
    check_qq(dropped, x, g, quantiles = c(0.25, 0.75))
  )

  one_level <- data.frame(
    x = 1:10,
    g = factor(rep("a", 10), levels = c("a", "b"))
  )
  expect_error(
    check_qq(one_level, x, g),
    class = "halfmoon_group_error"
  )
})

test_that("check_qq validates missing weights", {
  df <- data.frame(
    x = c(1:10, 21:30),
    g = rep(0:1, each = 10),
    w = c(NA, rep(1, 19))
  )

  expect_halfmoon_error(
    check_qq(df, x, g, .weights = w),
    "halfmoon_na_error"
  )

  # na.rm = TRUE drops the rows with missing weights
  result <- check_qq(
    df,
    x,
    g,
    .weights = w,
    na.rm = TRUE,
    include_observed = FALSE,
    quantiles = c(0.25, 0.75)
  )
  complete <- df[!is.na(df$w), ]
  expect_equal(
    result$unexposed_quantiles,
    unname(stats::quantile(complete$x[complete$g == 0], c(0.25, 0.75)))
  )
})

test_that("check_qq agrees with the observed quantiles under unit weights", {
  df <- nhefs_weights
  df$unit_wt <- 1
  probs <- seq(0.05, 0.95, 0.05)

  result <- check_qq(df, wt71, qsmk, .weights = unit_wt, quantiles = probs)
  observed <- result[result$method == "observed", ]
  weighted <- result[result$method == "unit_wt", ]

  expect_equal(weighted$exposed_quantiles, observed$exposed_quantiles)
  expect_equal(weighted$unexposed_quantiles, observed$unexposed_quantiles)
})

test_that("weighted_quantile reproduces stats::quantile(type = 7)", {
  withr::local_seed(2024)
  probs <- c(0, 0.01, 0.25, 0.5, 0.75, 0.99, 1)

  for (n in c(3, 5, 50)) {
    values <- stats::rnorm(n)
    expected <- unname(stats::quantile(values, probs, type = 7))

    expect_equal(weighted_quantile(values, probs, rep(1, n)), expected)
    # weights are scale invariant
    expect_equal(weighted_quantile(values, probs, rep(2, n)), expected)
    expect_equal(weighted_quantile(values, probs, rep(0.5, n)), expected)
  }

  # tied values are handled the same way as stats::quantile()
  tied <- c(1, 1, 2, 2, 2, 5)
  expect_equal(
    weighted_quantile(tied, probs, rep(1, length(tied))),
    unname(stats::quantile(tied, probs, type = 7))
  )

  # the documented example is exact
  expect_equal(
    weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10)),
    unname(stats::quantile(1:10, c(0.25, 0.5, 0.75)))
  )
})

test_that("weighted_quantile excludes zero-weight observations", {
  values <- c(1, 2, 3, 4, 100)
  weights <- c(1, 1, 1, 1, 0)
  probs <- c(0.25, 0.5, 0.75)

  expect_equal(
    weighted_quantile(values, probs, weights),
    unname(stats::quantile(values[weights > 0], probs))
  )

  # matching weights are 0/1, so the weighted quantiles are the quantiles of
  # the matched subset
  withr::local_seed(9)
  matching_wts <- stats::rbinom(nrow(nhefs_weights), 1, 0.5)
  expect_equal(
    weighted_quantile(nhefs_weights$age, probs, matching_wts),
    unname(stats::quantile(nhefs_weights$age[matching_wts == 1], probs))
  )
})

test_that("weighted_quantile guards degenerate weights", {
  # a single positive weight is not enough to interpolate
  expect_equal(weighted_quantile(c(1, 2), 0.5, c(1, 0)), NA_real_)
  expect_equal(
    weighted_quantile(c(1, 2), c(0.1, 0.9), c(0, 0)),
    rep(NA_real_, 2)
  )
  expect_equal(weighted_quantile(numeric(0), 0.5, numeric(0)), NA_real_)

  # a constant variable has constant quantiles
  expect_equal(weighted_quantile(c(5, 5, 5), c(0.1, 0.9), c(1, 2, 3)), c(5, 5))
})

test_that("weighted_quantile validates its arguments", {
  expect_error(
    weighted_quantile(1:10, c(-0.5, 0.5), rep(1, 10)),
    class = "halfmoon_range_error"
  )
  expect_error(
    weighted_quantile(1:10, 1.5, rep(1, 10)),
    class = "halfmoon_range_error"
  )
  expect_error(
    weighted_quantile(1:10, "a", rep(1, 10)),
    class = "halfmoon_type_error"
  )
  expect_error(
    weighted_quantile(1:10, 0.5, rep(-1, 10)),
    class = "halfmoon_range_error"
  )
  expect_error(
    weighted_quantile(1:10, 0.5, rep(1, 3)),
    class = "halfmoon_length_error"
  )
})

test_that("weighted_quantile reflects the weights", {
  # hand-computed: distinct values 1, 2, 3 with weights 1, 1, 2 sit at
  # probabilities 0, 0.4, and 1, so the median interpolates between 2 and 3
  expect_equal(weighted_quantile(1:3, 0.5, c(1, 1, 2)), 2 + 1 / 6)

  # quantiles are monotone in the probabilities
  withr::local_seed(11)
  values <- stats::rnorm(30)
  weights <- stats::runif(30, 0, 5)
  result <- weighted_quantile(values, seq(0, 1, 0.01), weights)
  expect_true(all(diff(result) >= 0))

  # upweighting the larger values pulls the quantiles up
  heavy <- weighted_quantile(1:10, 0.5, c(rep(1, 5), rep(5, 5)))
  expect_gt(heavy, stats::median(1:10))
})

test_that("check_qq returns expected quantile values", {
  # Create simple test data
  set.seed(123)
  test_data <- data.frame(
    x = c(rnorm(50, 0, 1), rnorm(50, 1, 1)),
    group = rep(c("A", "B"), each = 50)
  )

  result <- check_qq(test_data, x, group, quantiles = c(0.25, 0.5, 0.75))

  # Check that we get 3 quantiles
  expect_equal(nrow(result), 3)

  # With default NULL .reference_level, A (first level) is the reference group,
  # so exposed_quantiles are from B (higher values) and unexposed_quantiles
  # from A (lower values)
  expect_true(all(result$exposed_quantiles > result$unexposed_quantiles))

  # Test with explicit .reference_level = "B"
  result_explicit <- check_qq(
    test_data,
    x,
    group,
    quantiles = c(0.25, 0.5, 0.75),
    .reference_level = "B"
  )
  # Now B is the reference, so exposed_quantiles < unexposed_quantiles
  expect_true(all(
    result_explicit$exposed_quantiles < result_explicit$unexposed_quantiles
  ))
})

test_that("check_qq rejects a weight method labeled observed", {
  collided <- dplyr::rename(nhefs_weights, observed = w_ate)

  expect_error(
    check_qq(collided, age, qsmk, .weights = observed),
    class = "halfmoon_arg_error"
  )

  expect_error(
    check_qq(nhefs_weights, age, qsmk, .weights = c(observed = w_ate)),
    class = "halfmoon_arg_error"
  )
})

test_that("weighted_quantile applies the two-tier na.rm policy", {
  values <- c(1:9, NA_real_)
  weights <- rep(1, 10)

  expect_equal(
    weighted_quantile(values, c(0.25, 0.5, 0.75), weights),
    rep(NA_real_, 3)
  )

  expect_equal(
    weighted_quantile(values, c(0.25, 0.5, 0.75), weights, na.rm = TRUE),
    unname(stats::quantile(1:9, c(0.25, 0.5, 0.75)))
  )

  # A missing weight is missing data too
  missing_weight <- c(rep(1, 9), NA_real_)
  expect_equal(
    weighted_quantile(1:10, c(0.25, 0.5, 0.75), missing_weight),
    rep(NA_real_, 3)
  )
  expect_equal(
    weighted_quantile(
      1:10,
      c(0.25, 0.5, 0.75),
      missing_weight,
      na.rm = TRUE
    ),
    unname(stats::quantile(1:9, c(0.25, 0.5, 0.75)))
  )

  # Complete data is unaffected by either setting
  expect_equal(
    weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10)),
    weighted_quantile(1:10, c(0.25, 0.5, 0.75), rep(1, 10), na.rm = TRUE)
  )

  expect_error(
    weighted_quantile(1:10, 0.5, rep(1, 10), na.rm = "yes"),
    class = "halfmoon_arg_error"
  )
})

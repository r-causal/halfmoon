# Tests for exposure type resolution in check_balance()

# A numeric exposure with many repeated values, the shape a change score on a
# bounded count takes. The 20% rule reads it as categorical even though it has
# more than ten distinct values.
sparse_numeric_data <- function(n = 1000) {
  withr::local_seed(20260813)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  tibble::tibble(
    z1 = z1,
    z2 = z2,
    exposure = sample(0:14, n, replace = TRUE),
    wts = stats::runif(n, 0.5, 2)
  )
}

continuous_data <- function(n = 400) {
  withr::local_seed(20260814)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  tibble::tibble(
    z1 = z1,
    z2 = z2,
    exposure = 0.5 * z1 - 0.3 * z2 + stats::rnorm(n),
    wts = stats::runif(n, 0.5, 2)
  )
}

# =============================================================================
# ANNOUNCEMENT
# =============================================================================

test_that("check_balance announces the exposure type it detected", {
  withr::local_options(halfmoon.quiet = FALSE)

  expect_message(
    check_balance(nhefs_weights, age, qsmk, .metrics = "smd"),
    "Treating.+as binary"
  )

  expect_message(
    check_balance(nhefs_weights, age, alcoholfreq_cat, .metrics = "smd"),
    "Treating.+as categorical"
  )

  expect_message(
    check_balance(nhefs_weights, age, wt71, .metrics = "correlation"),
    "Treating.+as continuous"
  )
})

test_that("check_balance announces nothing when halfmoon.quiet is TRUE", {
  withr::local_options(halfmoon.quiet = TRUE)

  expect_no_message(
    check_balance(nhefs_weights, age, qsmk, .metrics = "smd")
  )
})

test_that("check_balance announces nothing when exposure_type is supplied", {
  withr::local_options(halfmoon.quiet = FALSE)

  expect_no_message(
    check_balance(
      nhefs_weights,
      age,
      qsmk,
      .metrics = "smd",
      exposure_type = "binary"
    )
  )
})

test_that("check_balance rejects an unknown exposure_type", {
  expect_error(
    check_balance(nhefs_weights, age, qsmk, exposure_type = "ordinal"),
    "ordinal"
  )
})

# =============================================================================
# DEFAULT METRICS BY EXPOSURE TYPE
# =============================================================================

test_that("check_balance defaults to discrete metrics for a binary exposure", {
  result <- check_balance(nhefs_weights, c(age, wt71), qsmk)

  expect_setequal(unique(result$metric), c("smd", "vr", "ks", "energy"))
})

test_that("check_balance defaults to discrete metrics for a categorical exposure", {
  result <- check_balance(nhefs_weights, c(age, wt71), alcoholfreq_cat)

  expect_setequal(unique(result$metric), c("smd", "vr", "ks", "energy"))
})

test_that("check_balance defaults to continuous metrics for a continuous exposure", {
  data <- continuous_data()

  result <- check_balance(data, c(z1, z2), exposure)

  expect_setequal(unique(result$metric), c("correlation", "energy"))
})

test_that("check_balance reads a sparse numeric exposure as categorical", {
  data <- sparse_numeric_data()
  withr::local_options(halfmoon.quiet = FALSE)

  expect_message(
    result <- check_balance(data, c(z1, z2), exposure, .metrics = "smd"),
    "Treating.+as categorical"
  )

  # every non-reference level gets its own comparison
  expect_length(unique(result$group_level), 14L)
  expect_equal(nrow(result), 28L)
})

test_that("check_balance honors an explicit continuous type for a sparse numeric exposure", {
  data <- sparse_numeric_data()

  result <- check_balance(
    data,
    c(z1, z2),
    exposure,
    exposure_type = "continuous"
  )

  expect_setequal(unique(result$metric), c("correlation", "energy"))
})

# =============================================================================
# METRIC AND EXPOSURE TYPE COMPATIBILITY
# =============================================================================

test_that("check_balance rejects discrete metrics for a continuous exposure", {
  data <- continuous_data()

  expect_halfmoon_error(
    check_balance(data, c(z1, z2), exposure, .metrics = c("smd", "energy")),
    "halfmoon_metric_type_error"
  )
})

test_that("check_balance rejects correlation for a binary exposure", {
  expect_halfmoon_error(
    check_balance(nhefs_weights, age, qsmk, .metrics = "correlation"),
    "halfmoon_metric_type_error"
  )
})

test_that("check_balance rejects correlation for a categorical exposure", {
  expect_halfmoon_error(
    check_balance(
      nhefs_weights,
      age,
      alcoholfreq_cat,
      .metrics = c("correlation", "energy")
    ),
    "halfmoon_metric_type_error"
  )
})

test_that("check_balance requires a numeric exposure treated as continuous", {
  # `exposure_type` clears the metric compatibility check, so the lower level
  # guard is what rejects the factor. Energy alone must hit it too: computing
  # distances on a factor would silently measure its integer codes
  expect_halfmoon_error(
    check_balance(
      nhefs_weights,
      age,
      qsmk,
      .metrics = "correlation",
      exposure_type = "continuous"
    ),
    "halfmoon_type_error"
  )
  expect_halfmoon_error(
    check_balance(
      nhefs_weights,
      age,
      qsmk,
      .metrics = "energy",
      exposure_type = "continuous"
    ),
    "halfmoon_type_error"
  )
})

test_that("check_balance still rejects unknown metric names", {
  expect_halfmoon_error(
    check_balance(nhefs_weights, age, qsmk, .metrics = "invalid"),
    "halfmoon_arg_error"
  )
})

# =============================================================================
# CONTINUOUS RESULTS
# =============================================================================

test_that("check_balance shapes continuous results by variable and method", {
  data <- continuous_data()

  result <- check_balance(data, c(z1, z2), exposure, .weights = wts)

  correlations <- dplyr::filter(result, metric == "correlation")
  energies <- dplyr::filter(result, metric == "energy")

  # two variables by two methods, plus one energy row per method
  expect_equal(nrow(correlations), 4L)
  expect_equal(nrow(energies), 2L)
  expect_equal(unique(correlations$group_level), "exposure")
  expect_true(all(is.na(energies$variable)))
  expect_setequal(result$method, c("observed", "wts"))
})

test_that("check_balance continuous correlations match bal_corr", {
  data <- continuous_data()

  result <- check_balance(
    data,
    c(z1, z2),
    exposure,
    .weights = wts,
    .metrics = "correlation"
  )

  observed <- dplyr::filter(result, method == "observed")
  weighted <- dplyr::filter(result, method == "wts")

  expect_equal(
    observed$estimate,
    c(
      bal_corr(data$z1, data$exposure),
      bal_corr(data$z2, data$exposure)
    )
  )
  expect_equal(
    weighted$estimate,
    c(
      bal_corr(data$z1, data$exposure, .weights = data$wts),
      bal_corr(data$z2, data$exposure, .weights = data$wts)
    )
  )
})

test_that("check_balance drops group levels for a continuous exposure", {
  # three distinct values would otherwise expand into two group comparisons
  data <- continuous_data(n = 60)
  data$exposure <- rep(c(0, 1, 2), length.out = nrow(data))

  result <- check_balance(
    data,
    c(z1, z2),
    exposure,
    exposure_type = "continuous"
  )

  expect_setequal(unique(result$metric), c("correlation", "energy"))
  expect_setequal(
    result$group_level[result$metric == "correlation"],
    "exposure"
  )
  expect_equal(nrow(result), 3L)
})

# =============================================================================
# ENERGY DISPATCH
# =============================================================================

test_that("check_balance energy follows the resolved exposure type", {
  data <- sparse_numeric_data()
  covariates <- data[c("z1", "z2")]

  categorical <- check_balance(
    data,
    c(z1, z2),
    exposure,
    .metrics = "energy",
    exposure_type = "categorical"
  )
  continuous <- check_balance(
    data,
    c(z1, z2),
    exposure,
    .metrics = "energy",
    exposure_type = "continuous"
  )

  expect_equal(
    categorical$estimate,
    bal_energy(covariates, factor(data$exposure))
  )
  expect_equal(
    continuous$estimate,
    bal_energy(covariates, data$exposure)
  )
  expect_false(isTRUE(all.equal(categorical$estimate, continuous$estimate)))
})

test_that("bal_energy keeps its own exposure heuristic for direct calls", {
  data <- sparse_numeric_data()
  covariates <- data[c("z1", "z2")]

  # `criterion = "dcor"` is available only for a continuous exposure, so it
  # reports which branch the heuristic took
  expect_no_error(
    bal_energy(covariates, data$exposure, criterion = "dcor")
  )
  expect_error(
    bal_energy(covariates, factor(data$exposure), criterion = "dcor"),
    class = "halfmoon_arg_error"
  )
})

# =============================================================================
# BACKWARD COMPATIBILITY
# =============================================================================

test_that("default results for discrete exposures are unchanged", {
  # The fixture is saved on one platform; estimates differ across BLAS builds
  # at ~1e-13, so compare within testthat's default tolerance
  defaults <- readRDS(test_path("fixtures", "check-balance-defaults.rds"))

  expect_equal(
    check_balance(
      nhefs_weights,
      c(age, wt71, smokeintensity, race, education),
      qsmk
    ),
    defaults$binary_observed
  )
  expect_equal(
    check_balance(
      nhefs_weights,
      c(age, wt71, smokeintensity, race, education),
      qsmk,
      .weights = c(w_ate, w_att)
    ),
    defaults$binary_weighted
  )
  expect_equal(
    check_balance(nhefs_weights, c(age, wt71, sex), alcoholfreq_cat),
    defaults$categorical_observed
  )
  expect_equal(
    check_balance(
      nhefs_weights,
      c(age, wt71, sex),
      alcoholfreq_cat,
      .weights = c(w_cat_ate, w_cat_att_2_3wk)
    ),
    defaults$categorical_weighted
  )
})

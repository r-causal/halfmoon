suppressPackageStartupMessages(library(survey))
suppressPackageStartupMessages(library(gtsummary))
suppressPackageStartupMessages(library(dplyr))

# Create survey design and gtsummary tables.
svy <- svydesign(~1, data = nhefs_weights, weights = ~w_ate)
# Suppress warnings from external packages (gtsummary/survey/cardx partial argument matches)
tbl <- suppressWarnings(tbl_svysummary(svy, include = c(age, sex, smokeyrs)))
tbl_by <- suppressWarnings(tbl_svysummary(
  svy,
  by = qsmk,
  include = c(age, sex, smokeyrs)
))

# Tests --------------------------------------------------------------------

test_that("Non-by case ESS values match ess() results", {
  res <- add_ess_header(tbl)

  # Compute expected ESS from the survey design weights.
  expected_ess <- ess(weights(svy))

  # For a non-by table, the header has one row.
  expect_equal(res$table_styling$header$modify_stat_n[[1]], expected_ess)
  expect_equal(res$table_styling$header$modify_stat_N[[1]], expected_ess)
  expect_equal(res$table_styling$header$modify_stat_p[[1]], 1)

  # Verify that the ESS result stored in cards matches.
  expect_equal(res$cards$add_ess_header$stat[[1]], expected_ess)
})

test_that("By case ESS values match ess() results", {
  res_by <- add_ess_header(tbl_by)
  header_tbl <- res_by$table_styling$header

  # In a by table, the header may include extra rows.
  # We'll restrict to rows with a non-missing group label.
  by_rows <- header_tbl |> filter(!is.na(modify_stat_level))

  # Compute expected ESS by group.
  expected_by <- nhefs_weights |>
    group_by(qsmk) |>
    summarize(expected_ess = ess(w_ate), .groups = "drop") |>
    arrange(as.character(qsmk))

  # Compare group labels.
  expect_equal(by_rows$modify_stat_level, as.character(expected_by$qsmk))
  # Compare group ESS values.
  expect_equal(by_rows$modify_stat_n, expected_by$expected_ess)

  # Total ESS is the sum of the group ESS values.
  total_expected <- sum(expected_by$expected_ess)
  expect_equal(by_rows$modify_stat_N, rep(total_expected, nrow(by_rows)))

  # Compare proportions.
  expected_prop <- expected_by$expected_ess / total_expected
  expect_equal(by_rows$modify_stat_p, expected_prop)

  # The ESS results stored in cards may be a list-column; unlist before comparing.
  expect_equal(
    unlist(res_by$cards$add_ess_header$stat),
    expected_by$expected_ess
  )
})

test_that("Error if `x` is not a tbl_svysummary", {
  expect_error(
    add_ess_header(1),
    class = "halfmoon_type_error"
  )
})

test_that("Error if `header` is not a string", {
  expect_error(
    add_ess_header(tbl, header = 123),
    class = "halfmoon_type_error"
  )
})

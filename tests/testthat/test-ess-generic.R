# Provenance -----------------------------------------------------------------
#
# The numeric results of `ess()` are unchanged by the move to the shared
# generic, so a behavior test alone cannot tell the reexport apart from
# halfmoon's own definition. These tests read where `ess` comes from.
#
# The S3 registration state is read from the namespace tables rather than with
# `getS3method()`. `getS3method()` returns `NULL` for a generic that is not
# visible on the search path, which makes an absence assertion pass whether or
# not a method exists. `UseMethod()` also dispatches on definitions found in the
# calling environment, so each absence claim is paired with a direct read of
# halfmoon's namespace.

test_that("ess is the causalgenerics generic rather than a halfmoon closure", {
  expect_identical(ess, causalgenerics::ess)
  expect_identical(environment(ess), asNamespace("causalgenerics"))
  expect_identical(names(formals(ess)), c("x", "..."))
})

test_that("halfmoon reexports ess without defining it", {
  # The name is absent from halfmoon's namespace frame itself, so it can only
  # reach callers through the imports environment, and halfmoon still exports it.
  expect_false(exists("ess", envir = asNamespace("halfmoon"), inherits = FALSE))
  expect_true("ess" %in% getNamespaceExports("halfmoon"))
})

test_that("halfmoon registers no ess method", {
  registrations <- getNamespaceInfo("halfmoon", "S3methods")
  expect_false("ess" %in% registrations[, 1])

  # Methods for a foreign generic are registered into the owning namespace's
  # table, so the only ess method visible there is the upstream default.
  methods_table <- asNamespace("causalgenerics")[[".__S3MethodsTable__."]]
  expect_identical(
    grep("^ess\\.", ls(methods_table), value = TRUE),
    "ess.default"
  )

  # A leftover definition in halfmoon's namespace would still serve internal
  # calls even with nothing registered.
  candidates <- c("ess.default", "ess.numeric", "ess.psw", "ess.causal_wts")
  found <- vapply(
    candidates,
    exists,
    logical(1),
    envir = asNamespace("halfmoon"),
    inherits = FALSE
  )
  expect_false(any(found))
})

# Numbers that must not change -----------------------------------------------

test_that("bal_ess returns unchanged values on nhefs_weights columns", {
  expect_equal(bal_ess(nhefs_weights$w_ate), 1013.3122925812)
  expect_equal(bal_ess(nhefs_weights$w_att), 1069.58749958148)
  expect_equal(bal_ess(nhefs_weights$w_ato), 1112.60208008461)

  # na.rm routes through to the default method unchanged.
  weights_na <- nhefs_weights$w_ate
  weights_na[1:5] <- NA
  expect_true(is.na(bal_ess(weights_na)))
  expect_equal(bal_ess(weights_na, na.rm = TRUE), 1010.01198048868)
})

test_that("check_ess returns unchanged values without a group", {
  result <- check_ess(nhefs_weights, .weights = c(w_ate, w_att, w_atm))

  expect_identical(result$method, c("observed", "w_ate", "w_atm", "w_att"))
  expect_equal(
    result$ess,
    c(1566, 1013.3122925812, 1088.98134930972, 1069.58749958148)
  )
  expect_equal(
    result$ess_pct,
    c(100, 64.707042948991, 69.5390389086665, 68.3006066143983)
  )
})

test_that("check_ess returns unchanged values on the has_group branch", {
  result <- check_ess(nhefs_weights, .weights = w_ate, .exposure = qsmk)

  expect_identical(result$method, c("observed", "observed", "w_ate", "w_ate"))
  expect_identical(as.character(result$group), c("0", "1", "0", "1"))
  expect_equal(
    result$ess,
    c(1163, 403, 1128.60986313319, 325.974728957833)
  )
})

test_that("add_ess_header returns unchanged values", {
  skip_if_not_installed("survey")
  skip_if_not_installed("gtsummary")
  skip_if_not_installed("cards")
  skip_if_not_installed("cardx")

  design <- survey::svydesign(~1, data = nhefs_weights, weights = ~w_ate)

  # gtsummary, survey, and cardx emit partial argument matching warnings.
  table_overall <- suppressWarnings(
    gtsummary::tbl_svysummary(design, include = c(age, sex))
  )
  header_overall <- add_ess_header(table_overall)$table_styling$header
  expect_equal(header_overall$modify_stat_n[[1]], 1013.3122925812)

  table_by <- suppressWarnings(
    gtsummary::tbl_svysummary(design, by = qsmk, include = c(age, sex))
  )
  header_by <- add_ess_header(table_by)$table_styling$header
  header_by <- header_by[!is.na(header_by$modify_stat_level), ]
  expect_identical(header_by$modify_stat_level, c("0", "1"))
  expect_equal(
    header_by$modify_stat_n,
    c(1128.60986313319, 325.974728957833)
  )
})

test_that("ess still returns NaN for degenerate weights", {
  expect_true(is.nan(ess(rep(0, 5))))
  expect_true(is.nan(ess(numeric(0))))
})

# Intended behavior changes --------------------------------------------------

test_that("ess no longer accepts the wts argument name", {
  # The generic's first argument is `x`, so the old spelling leaves it missing.
  # Matched on the message rather than the condition class because R only gained
  # the `missingArgError` class in 4.5.0 and this package supports R >= 4.1.0.
  expect_error(ess(wts = c(1, 2, 3)), 'argument "x" is missing')
})

test_that("ess rejects non-numeric input", {
  expect_error(ess(NULL), class = "causalgenerics_invalid_argument_x")
  expect_error(
    ess(factor(c("a", "b", "b"))),
    class = "causalgenerics_invalid_argument_x"
  )
  expect_error(
    ess(c(TRUE, FALSE, TRUE)),
    class = "causalgenerics_invalid_argument_x"
  )
  expect_error(
    ess(data.frame(a = 1:3)),
    class = "causalgenerics_invalid_argument_x"
  )
})

test_that("check_ess works with no weights", {
  result <- check_ess(nhefs_weights)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$method, "observed")
  expect_equal(result$ess_pct, 100)
  expect_equal(result$ess, result$n)
})

test_that("check_ess works with single weight", {
  result <- check_ess(nhefs_weights, .wts = w_ate)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$method, c("observed", "w_ate"))
  expect_true(all(result$ess_pct <= 100))
  expect_true(all(result$ess > 0))
})

test_that("check_ess works with multiple weights", {
  result <- check_ess(nhefs_weights, .wts = c(w_ate, w_att, w_atm))
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  # Methods may be returned in different order due to tidyselect
  expect_setequal(result$method, c("observed", "w_ate", "w_att", "w_atm"))
})

test_that("check_ess works without observed", {
  result <- check_ess(nhefs_weights, .wts = w_ate, include_observed = FALSE)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$method, "w_ate")
})

test_that("check_ess works with binary groups", {
  result <- check_ess(nhefs_weights, .wts = w_ate, .group = qsmk)
  
  expect_s3_class(result, "tbl_df")
  expect_true("group" %in% names(result))
  expect_equal(length(unique(result$group)), 2)
  expect_equal(nrow(result), 4) # 2 methods x 2 groups
})

test_that("check_ess works with categorical groups", {
  result <- check_ess(nhefs_weights, .wts = w_cat_ate, .group = alcoholfreq_cat)
  
  expect_s3_class(result, "tbl_df")
  expect_true("group" %in% names(result))
  expect_true(length(unique(result$group)) > 2)
})

test_that("check_ess works with continuous groups", {
  result <- check_ess(nhefs_weights, .wts = w_ate, .group = age, n_tiles = 4)
  
  expect_s3_class(result, "tbl_df")
  expect_true("group" %in% names(result))
  expect_equal(length(unique(result$group)), 4)
  expect_true(all(grepl("^Q[1-4]$", unique(result$group))))
})

test_that("check_ess works with custom tile labels", {
  labels <- c("Young", "Middle", "Older")
  result <- check_ess(nhefs_weights, .wts = w_ate, .group = age, 
                      n_tiles = 3, tile_labels = labels)
  
  expect_setequal(as.character(unique(result$group)), labels)
})

test_that("check_ess handles tidyselect syntax", {
  result1 <- check_ess(nhefs_weights, .wts = starts_with("w_a"))
  result2 <- check_ess(nhefs_weights, .wts = c(w_ate, w_att, w_atc, w_atm, w_ato))
  
  # Should have same number of methods (plus observed)
  expect_equal(nrow(result1), nrow(result2))
})

test_that("check_ess handles psw weight objects", {
  # Assuming psw weights are numeric vectors with special class
  # The extract_weight_data function should handle conversion
  result <- check_ess(nhefs_weights, .wts = w_ate)
  
  expect_true(is.numeric(result$ess))
  expect_true(all(result$ess > 0))
})

test_that("check_ess validates inputs", {
  expect_halfmoon_error(
    check_ess("not a data frame"),
    "halfmoon_type_error"
  )
  
  expect_halfmoon_error(
    check_ess(nhefs_weights, .group = "not_a_column"),
    "halfmoon_column_error"
  )
  
  expect_halfmoon_error(
    check_ess(nhefs_weights, .wts = w_ate, .group = age,
              n_tiles = 3, tile_labels = c("Too", "Few")),
    "halfmoon_length_error"
  )
})

test_that("ESS calculation is correct", {
  # Create simple test case with known ESS
  test_df <- data.frame(
    wts = c(1, 1, 1, 1),  # Equal weights -> ESS = n
    wts2 = c(4, 0, 0, 0)  # All weight on one obs -> ESS = 1
  )
  
  result <- check_ess(test_df, .wts = c(wts, wts2), include_observed = FALSE)
  
  expect_equal(result$ess[result$method == "wts"], 4)
  expect_equal(result$ess[result$method == "wts2"], 1)
  expect_equal(result$ess_pct[result$method == "wts"], 100)
  expect_equal(result$ess_pct[result$method == "wts2"], 25)
})

test_that("check_ess handles NA values", {
  # Create data with NAs
  test_df <- nhefs_weights
  test_df$w_ate[1:10] <- NA
  
  result <- check_ess(test_df, .wts = w_ate)
  
  # Should still compute ESS on non-NA values
  expect_true(all(!is.na(result$ess)))
  expect_true(all(result$ess > 0))
})
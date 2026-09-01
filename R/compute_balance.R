#' Does either group carry no weight?
#'
#' `validate_weights()` allows zero weights, so a group can end up with a total
#' weight of zero. A weighted mean, variance, or empirical CDF is undefined
#' there, and the statistic is missing rather than an error.
#' @noRd
has_zero_weight_group <- function(weights, idx_ref, idx_other) {
  if (is.null(weights)) {
    return(FALSE)
  }

  weights <- extract_weight_data(weights)
  group_totals <- c(sum(weights[idx_ref]), sum(weights[idx_other]))

  any(!is.finite(group_totals) | group_totals <= 0)
}

#' Balance Standardized Mean Difference (SMD)
#'
#' Calculates the standardized mean difference between two groups using the
#' smd package. This is a common measure of effect size for comparing group
#' differences while accounting for variability.
#'
#' @details
#' The standardized mean difference (SMD) is calculated as:
#' \deqn{SMD = \frac{\bar{x}_1 - \bar{x}_0}{\sqrt{(s_1^2 + s_0^2)/2}}}
#' where \eqn{\bar{x}_1} and \eqn{\bar{x}_0} are the means of the treatment and control
#' groups, and \eqn{s_1^2} and \eqn{s_0^2} are their variances.
#'
#' In causal inference, SMD values of 0.1 or smaller are often considered
#' indicative of good balance between treatment groups.
#'
#' @inheritParams balance_params
#' @return For a binary exposure, a numeric value: the standardized mean
#'   difference of the comparison group minus the reference group. Positive
#'   values indicate the comparison group has a higher mean than the reference
#'   group. For a categorical exposure, a named numeric vector with one element
#'   per non-reference level, named `X_vs_ref`, holding level `X` minus the
#'   reference level.
#' @family balance functions
#' @seealso [check_balance()] for computing multiple balance metrics at once
#' @examples
#' # Binary exposure
#' bal_smd(nhefs_weights$age, nhefs_weights$qsmk)
#'
#' # With weights
#' bal_smd(nhefs_weights$wt71, nhefs_weights$qsmk,
#'         .weights = nhefs_weights$w_ate)
#'
#' # Categorical exposure (returns named vector). This exposure has missing
#' # values, so `na.rm = TRUE` is needed for a non-missing result.
#' bal_smd(nhefs_weights$age, nhefs_weights$alcoholfreq_cat, na.rm = TRUE)
#'
#' # Specify reference level
#' bal_smd(nhefs_weights$age, nhefs_weights$alcoholfreq_cat,
#'         .reference_level = "daily", na.rm = TRUE)
#'
#' # With categorical weights
#' bal_smd(nhefs_weights$wt71, nhefs_weights$alcoholfreq_cat,
#'         .weights = nhefs_weights$w_cat_ate, na.rm = TRUE)
#'
#' @export
bal_smd <- function(
  .covariate,
  .exposure,
  .weights = NULL,
  .reference_level = NULL,
  na.rm = FALSE
) {
  validate_numeric(.covariate)
  validate_not_empty(.covariate)
  validate_equal_length(.covariate, .exposure)
  validate_weights(.weights, length(.covariate))

  # Check if exposure is categorical first
  if (is_categorical_exposure(.exposure)) {
    return(.bal_smd_categorical(
      covariate = .covariate,
      group = .exposure,
      weights = .weights,
      reference_group = .reference_level,
      na.rm = na.rm
    ))
  }

  # Binary exposure handling. `smd::smd()` splits the covariate by the levels of
  # the exposure, so the reference index has to be a position in those levels
  # rather than a position in the order the levels happen to appear.
  .exposure <- drop_unused_levels(.exposure)
  levels_g <- extract_group_levels(.exposure, require_binary = TRUE)
  ref_level <- determine_reference_group(.exposure, .reference_level)
  gref_index <- which(levels_g == ref_level)

  w_data <- extract_weight_data(.weights)

  # `smd::smd()` only removes missing covariate values, so filter the rows here
  # and hand it complete data
  if (na.rm) {
    keep <- !is.na(.covariate) & !is.na(.exposure)
    if (!is.null(w_data)) {
      keep <- keep & !is.na(w_data)
    }
    .covariate <- .covariate[keep]
    .exposure <- .exposure[keep]
    if (!is.null(w_data)) {
      w_data <- w_data[keep]
    }
  } else if (
    anyNA(.covariate) || anyNA(.exposure) || (!is.null(w_data) && anyNA(w_data))
  ) {
    return(NA_real_)
  }

  # Both groups need at least one observation left to compare
  idx_ref <- which(.exposure == ref_level)
  idx_other <- which(.exposure != ref_level)
  if (length(idx_ref) == 0 || length(idx_other) == 0) {
    return(NA_real_)
  }

  # `smd:::n_mean_var()` reports a mean and variance of 0 for a group with no
  # weight, which would turn an undefined statistic into a plausible number
  if (has_zero_weight_group(w_data, idx_ref, idx_other)) {
    return(NA_real_)
  }

  res <- smd::smd(
    x = .covariate,
    g = .exposure,
    w = w_data,
    gref = gref_index,
    na.rm = FALSE
  )

  # `smd::smd()` reports the reference group minus the comparison group; the
  # halfmoon convention is the comparison group minus the reference group
  -res$estimate
}


#' Balance Variance Ratio for Two Groups
#'
#' Calculates the ratio of variances between two groups: var(comparison) / var(reference).
#' For binary variables, uses the p*(1-p) variance formula. For continuous variables,
#' uses Bessel's correction for weighted sample variance.
#'
#' @details
#' The variance ratio compares the variability of a covariate between treatment groups.
#' It is calculated as:
#' \deqn{VR = \frac{s_1^2}{s_0^2}}
#' where \eqn{s_1^2} and \eqn{s_0^2} are the variances of the treatment and control groups.
#'
#' For binary variables (0/1), variance is computed as \eqn{p(1-p)} where \eqn{p} is the
#' proportion of 1s in each group. For continuous variables, the weighted sample variance
#' is used with Bessel's correction when weights are provided.
#'
#' Values close to 1.0 indicate similar variability between groups, which is desirable
#' for balance. Values substantially different from 1.0 suggest imbalanced variance.
#'
#' @inheritParams balance_params
#' @return For a binary exposure, a numeric value representing the variance
#'   ratio. Values greater than 1 indicate the comparison group has higher
#'   variance than the reference group. For a categorical exposure, a named
#'   numeric vector with one element per non-reference level, named `X_vs_ref`,
#'   holding the variance of level `X` divided by the variance of the reference
#'   level.
#' @family balance functions
#' @seealso [check_balance()] for computing multiple balance metrics at once
#' @examples
#' # Binary exposure
#' bal_vr(nhefs_weights$age, nhefs_weights$qsmk)
#'
#' # With weights
#' bal_vr(nhefs_weights$wt71, nhefs_weights$qsmk,
#'        .weights = nhefs_weights$w_ate)
#'
#' # Categorical exposure (returns named vector). This exposure has missing
#' # values, so `na.rm = TRUE` is needed for a non-missing result.
#' bal_vr(nhefs_weights$age, nhefs_weights$alcoholfreq_cat, na.rm = TRUE)
#'
#' # Specify reference level
#' bal_vr(nhefs_weights$age, nhefs_weights$alcoholfreq_cat,
#'        .reference_level = "2_3_per_week", na.rm = TRUE)
#'
#' # With categorical weights
#' bal_vr(nhefs_weights$wt71, nhefs_weights$alcoholfreq_cat,
#'        .weights = nhefs_weights$w_cat_ate, na.rm = TRUE)
#'
#' @export
bal_vr <- function(
  .covariate,
  .exposure,
  .weights = NULL,
  .reference_level = NULL,
  na.rm = FALSE
) {
  # Input validation
  validate_numeric(.covariate)
  validate_not_empty(.covariate)
  validate_equal_length(.covariate, .exposure)
  validate_weights(.weights, length(.covariate))

  # Check if exposure is categorical
  if (is_categorical_exposure(.exposure)) {
    return(.bal_vr_categorical(
      covariate = .covariate,
      group = .exposure,
      weights = .weights,
      reference_group = .reference_level,
      na.rm = na.rm
    ))
  }

  # Binary exposure handling (existing code)
  # Identify reference and comparison indices
  group_splits <- split_by_group(.covariate, .exposure, .reference_level)
  idx_ref <- group_splits$reference
  idx_other <- group_splits$comparison
  # Handle missing values
  if (na.rm) {
    idx_ref <- filter_na_indices(idx_ref, .covariate, .weights, na.rm = TRUE)
    idx_other <- filter_na_indices(
      idx_other,
      .covariate,
      .weights,
      na.rm = TRUE
    )
  } else {
    # A missing exposure is dropped by the split above, so it has to be checked
    # against the whole vector rather than the two groups
    if (
      anyNA(.exposure) ||
        check_na_return(
          .covariate[c(idx_ref, idx_other)],
          extract_weight_data(.weights)[c(idx_ref, idx_other)] %||% NULL,
          na.rm = FALSE
        )
    ) {
      return(NA_real_)
    }
  }

  # Check if we have enough data after removing NAs
  if (length(idx_ref) == 0 || length(idx_other) == 0) {
    return(NA_real_)
  }
  # A group carrying no weight contributes no variance to compare
  if (has_zero_weight_group(.weights, idx_ref, idx_other)) {
    return(NA_real_)
  }
  # Compute variances
  if (is_binary(.covariate)) {
    # For binary variables, use p*(1-p) formula
    var_ref <- if (is.null(.weights)) {
      p <- mean(.covariate[idx_ref])
      p * (1 - p)
    } else {
      wr <- extract_weight_data(.weights)[idx_ref]
      xr <- .covariate[idx_ref]
      p <- sum(wr * xr) / sum(wr)
      p * (1 - p)
    }
    var_other <- if (is.null(.weights)) {
      p <- mean(.covariate[idx_other])
      p * (1 - p)
    } else {
      wo <- extract_weight_data(.weights)[idx_other]
      xo <- .covariate[idx_other]
      p <- sum(wo * xo) / sum(wo)
      p * (1 - p)
    }
  } else {
    # For continuous variables, use Bessel's correction
    var_ref <- if (is.null(.weights)) {
      stats::var(.covariate[idx_ref])
    } else {
      wr <- extract_weight_data(.weights)[idx_ref]
      xr <- .covariate[idx_ref]
      mr <- sum(wr * xr) / sum(wr)
      # Use Bessel's correction for weighted sample variance
      denom <- sum(wr) - sum(wr^2) / sum(wr)
      if (!is.finite(denom) || denom <= 0) {
        sum(wr * (xr - mr)^2) / sum(wr)
      } else {
        sum(wr * (xr - mr)^2) / denom
      }
    }
    var_other <- if (is.null(.weights)) {
      stats::var(.covariate[idx_other])
    } else {
      wo <- extract_weight_data(.weights)[idx_other]
      xo <- .covariate[idx_other]
      mo <- sum(wo * xo) / sum(wo)
      # Use Bessel's correction for weighted sample variance
      denom <- sum(wo) - sum(wo^2) / sum(wo)
      if (!is.finite(denom) || denom <= 0) {
        sum(wo * (xo - mo)^2) / sum(wo)
      } else {
        sum(wo * (xo - mo)^2) / denom
      }
    }
  }
  # Return ratio
  if (is.na(var_ref) || is.na(var_other)) {
    return(NA_real_)
  }
  if (var_ref == 0 && var_other == 0) {
    return(1)
  }
  if (var_ref == 0) {
    return(Inf)
  }
  if (var_other == 0) {
    return(0)
  }
  var_other / var_ref
}

#' Balance Kolmogorov-Smirnov (KS) Statistic for Two Groups
#'
#' Computes the two-sample KS statistic comparing empirical cumulative distribution
#' functions (CDFs) between two groups. For binary variables, returns the absolute
#' difference in proportions. For continuous variables, computes the maximum
#' difference between empirical CDFs.
#'
#' @details
#' The Kolmogorov-Smirnov statistic measures the maximum difference between
#' empirical cumulative distribution functions of two groups:
#' \deqn{KS = \max_x |F_1(x) - F_0(x)|}
#' where \eqn{F_1(x)} and \eqn{F_0(x)} are the empirical CDFs of the treatment
#' and control groups.
#'
#' For binary variables, this reduces to the absolute difference in proportions.
#' For continuous variables, the statistic captures differences in the entire
#' distribution shape, not just means or variances.
#'
#' The KS statistic ranges from 0 (identical distributions) to 1 (completely
#' separate distributions). Smaller values indicate better distributional balance
#' between groups.
#'
#' @inheritParams balance_params
#' @return For a binary exposure, a numeric value representing the KS
#'   statistic. Values range from 0 to 1, with 0 indicating identical
#'   distributions and 1 indicating completely separate distributions. For a
#'   categorical exposure, a named numeric vector with one element per
#'   non-reference level, named `X_vs_ref`, comparing level `X` with the
#'   reference level.
#' @family balance functions
#' @seealso [check_balance()] for computing multiple balance metrics at once
#' @examples
#' # Binary exposure
#' bal_ks(nhefs_weights$age, nhefs_weights$qsmk)
#'
#' # With weights
#' bal_ks(nhefs_weights$wt71, nhefs_weights$qsmk,
#'        .weights = nhefs_weights$w_ate)
#'
#' # Categorical exposure (returns named vector). This exposure has missing
#' # values, so `na.rm = TRUE` is needed for a non-missing result.
#' bal_ks(nhefs_weights$age, nhefs_weights$alcoholfreq_cat, na.rm = TRUE)
#'
#' # Specify reference level
#' bal_ks(nhefs_weights$age, nhefs_weights$alcoholfreq_cat,
#'        .reference_level = "none", na.rm = TRUE)
#'
#' # With categorical weights
#' bal_ks(nhefs_weights$wt71, nhefs_weights$alcoholfreq_cat,
#'        .weights = nhefs_weights$w_cat_ate, na.rm = TRUE)
#' @export
bal_ks <- function(
  .covariate,
  .exposure,
  .weights = NULL,
  .reference_level = NULL,
  na.rm = FALSE
) {
  # Input validation
  validate_numeric(.covariate)
  validate_not_empty(.covariate)
  validate_equal_length(.covariate, .exposure)
  validate_weights(.weights, length(.covariate))

  # Check if exposure is categorical
  if (is_categorical_exposure(.exposure)) {
    return(.bal_ks_categorical(
      covariate = .covariate,
      group = .exposure,
      weights = .weights,
      reference_group = .reference_level,
      na.rm = na.rm
    ))
  }

  # Binary exposure handling (existing code)
  group_splits <- split_by_group(.covariate, .exposure, .reference_level)
  idx_ref <- group_splits$reference
  idx_other <- group_splits$comparison
  # Handle missing values
  if (na.rm) {
    idx_ref <- filter_na_indices(idx_ref, .covariate, .weights, na.rm = TRUE)
    idx_other <- filter_na_indices(
      idx_other,
      .covariate,
      .weights,
      na.rm = TRUE
    )
  } else {
    # A missing exposure is dropped by the split above, so it has to be checked
    # against the whole vector rather than the two groups
    if (
      anyNA(.exposure) ||
        check_na_return(
          .covariate[c(idx_ref, idx_other)],
          extract_weight_data(.weights)[c(idx_ref, idx_other)] %||% NULL,
          na.rm = FALSE
        )
    ) {
      return(NA_real_)
    }
  }

  # Check if we have enough data after removing NAs
  if (length(idx_ref) == 0 || length(idx_other) == 0) {
    return(NA_real_)
  }
  # A group carrying no weight has no distribution to compare
  if (has_zero_weight_group(.weights, idx_ref, idx_other)) {
    return(NA_real_)
  }
  # For binary variables, KS statistic is just the difference in proportions
  if (is_binary(.covariate)) {
    # Calculate weighted proportions
    p_ref <- if (is.null(.weights)) {
      mean(.covariate[idx_ref])
    } else {
      sum(extract_weight_data(.weights)[idx_ref] * .covariate[idx_ref]) /
        sum(extract_weight_data(.weights)[idx_ref])
    }
    p_other <- if (is.null(.weights)) {
      mean(.covariate[idx_other])
    } else {
      sum(extract_weight_data(.weights)[idx_other] * .covariate[idx_other]) /
        sum(extract_weight_data(.weights)[idx_other])
    }
    return(abs(p_other - p_ref))
  }

  # For continuous variables, compute full KS statistic
  # Extract and weight
  x_ref <- .covariate[idx_ref]
  x_other <- .covariate[idx_other]
  w_ref <- if (is.null(.weights)) {
    rep(1, length(x_ref))
  } else {
    extract_weight_data(.weights)[idx_ref]
  }
  w_other <- if (is.null(.weights)) {
    rep(1, length(x_other))
  } else {
    extract_weight_data(.weights)[idx_other]
  }
  w_ref <- w_ref / sum(w_ref)
  w_other <- w_other / sum(w_other)
  # Sort and CDF
  ord_ref <- order(x_ref)
  ord_other <- order(x_other)
  x_r <- x_ref[ord_ref]
  c_r <- cumsum(w_ref[ord_ref])
  x_o <- x_other[ord_other]
  c_o <- cumsum(w_other[ord_other])
  allv <- sort(unique(c(x_r, x_o)))
  F_r <- stats::approx(
    x_r,
    c_r,
    xout = allv,
    method = "constant",
    yleft = 0,
    yright = 1,
    ties = "ordered"
  )$y
  F_o <- stats::approx(
    x_o,
    c_o,
    xout = allv,
    method = "constant",
    yleft = 0,
    yright = 1,
    ties = "ordered"
  )$y
  max(abs(F_o - F_r))
}

#' Balance Weighted or Unweighted Pearson Correlation
#'
#' Calculates the Pearson correlation coefficient between two numeric vectors,
#' with optional case weights. Uses the standard correlation formula for
#' unweighted data and weighted covariance for weighted data.
#'
#' @param .x A numeric vector containing the first variable.
#' @param .y A numeric vector containing the second variable. Must have the same
#'   length as `.x`.
#' @param .weights An optional numeric vector of case weights. If provided, must
#'   have the same length as `.x` and `.y`. All weights must be non-negative.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values result in
#'   `NA` output.
#' @return A numeric value representing the correlation coefficient between -1 and 1.
#'   Returns `NA` if either variable has zero variance.
#' @family balance functions
#' @seealso [check_balance()] for computing multiple balance metrics at once
#' @examples
#' bal_corr(nhefs_weights$age, nhefs_weights$wt71)
#'
#' @export
bal_corr <- function(.x, .y, .weights = NULL, na.rm = FALSE) {
  # Input validation
  validate_numeric(.x)
  validate_numeric(.y)
  validate_not_empty(.x)
  validate_not_empty(.y)
  validate_equal_length(.x, .y)
  validate_weights(.weights, length(.x))

  if (na.rm) {
    # Handle missing values carefully - avoid logical(0) issue
    if (is.null(.weights)) {
      idx <- !(is.na(.x) | is.na(.y))
    } else {
      idx <- !(is.na(.x) | is.na(.y) | is.na(.weights))
    }
    .x <- .x[idx]
    .y <- .y[idx]
    if (!is.null(.weights)) .weights <- extract_weight_data(.weights)[idx]
  } else {
    # Extract weight data if needed
    if (!is.null(.weights)) {
      .weights <- extract_weight_data(.weights)
    }
    # Check for missing values
    if (is.null(.weights)) {
      if (any(is.na(.x) | is.na(.y))) return(NA_real_)
    } else {
      if (any(is.na(.x) | is.na(.y) | is.na(.weights))) return(NA_real_)
    }
  }

  # Check if we have enough data after removing NAs
  if (length(.x) < 2) {
    return(NA_real_)
  }

  if (is.null(.weights)) {
    return(stats::cor(.x, .y))
  }

  # Weights that sum to zero leave nothing to correlate
  total_weight <- sum(.weights)
  if (!is.finite(total_weight) || total_weight <= 0) {
    return(NA_real_)
  }

  # Compute weighted covariance
  w_norm <- .weights / total_weight
  mx <- sum(w_norm * .x)
  my <- sum(w_norm * .y)
  cov <- sum(w_norm * (.x - mx) * (.y - my))
  vx <- sum(w_norm * (.x - mx)^2)
  vy <- sum(w_norm * (.y - my)^2)

  if (!is.finite(vx) || !is.finite(vy) || vx <= 0 || vy <= 0) {
    return(NA_real_)
  }

  # Return standard correlation
  cov / sqrt(vx * vy)
}

#' Balance Energy Distance
#'
#' Computes the energy distance as a multivariate measure of covariate balance
#' between groups. Energy distance captures the similarity between distributions
#' across the entire joint distribution of .covariates, making it more comprehensive
#' than univariate balance measures.
#'
#' @param .covariates A data frame or matrix containing the .covariates to compare.
#' @param .exposure A vector (factor or numeric) indicating group membership. For
#'   binary and multi-category treatments, must have 2+ unique levels. For
#'   continuous treatments, should be numeric. When `exposure_type` is `"auto"`,
#'   a numeric exposure taking more than ten unique values is treated as
#'   continuous and returns the continuous-exposure statistic instead of an
#'   energy distance between groups; set `exposure_type` or wrap the exposure in
#'   `factor()` to force the categorical reading.
#' @param .weights An optional numeric vector of weights. If provided, must
#'   have the same length as rows in `.covariates`. All weights must be non-negative.
#' @param estimand Character string specifying the estimand. Options are:
#'   - NULL (default): Pure between-group energy distance comparing distributions
#'   - "ATE": Energy distance between each weighted group and the unweighted full
#'     sample, the target population of an average treatment effect
#'   - "ATT": Energy distance between each weighted group and the unweighted
#'     focal group, measuring how well the other groups match the treated
#'     distribution
#'   - "ATC": The same statistic with the control group as the focal group,
#'     measuring how well the treated units match the control distribution
#'   For continuous treatments, only NULL is supported.
#' @param .focal_level The treatment level whose unweighted distribution is the
#'   target for `estimand = "ATT"` or `estimand = "ATC"`. Must name a level the
#'   exposure takes. If `NULL` (default), the last observed level for `"ATT"`
#'   and the first observed level for `"ATC"`, which on a 0/1 exposure are the
#'   level `1` and the level `0`. Only `"ATT"` and `"ATC"` have a focal group,
#'   so supplying `.focal_level` with any other `estimand` is an error.
#' @param use_improved Logical. Use improved energy distance for ATE? Default is TRUE.
#'   When TRUE, adds pairwise treatment comparisons for better group separation.
#' @param standardized Logical. Only used when `criterion = "dcor"` for a
#'   continuous exposure, where `TRUE` (default) returns the standardized
#'   distance correlation and `FALSE` returns the unstandardized square-root
#'   distance covariance. Ignored for `criterion = "dependence"`.
#' @param criterion Character string selecting the continuous-exposure statistic.
#'   `"dependence"` (default) returns the weighted dependence distance \eqn{D(w)}
#'   of Huling, Greifer, and Chen (2023); `"dcor"` returns cobalt's
#'   `distance.cor` balance statistic. Binary and multi-category exposures
#'   always use the energy distance and accept only the default; supplying
#'   `"dcor"` with a non-continuous exposure is an error.
#' @param dimension_adj Logical. For `criterion = "dependence"`, weight the two
#'   marginal energy terms by a dimension adjustment (`TRUE`, default) so that
#'   the covariate term and the treatment term contribute comparably regardless
#'   of the number of covariates, or split them evenly (`FALSE`). Ignored when
#'   `criterion = "dcor"`. Binary and multi-category exposures accept only the
#'   default; `dimension_adj = FALSE` with a non-continuous exposure is an
#'   error.
#' @param exposure_type The type of exposure `.exposure` holds: one of "binary",
#'   "categorical", or "continuous", or "auto" (default) to read the type from
#'   the exposure. "binary" and "categorical" name the same energy distance
#'   between groups. Under "auto", a numeric exposure taking more than ten
#'   unique values is treated as continuous and anything else as categorical.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), a missing value in the
#'   covariates, the exposure, or the weights returns `NA`. If `TRUE`, rows with
#'   missing values are dropped before computation.
#'
#' @return A numeric value, or `NA_real_` when `na.rm = FALSE` and the
#'   covariates, the exposure, or the weights contain missing values, and also
#'   when `na.rm = TRUE` leaves no rows to compute on. For binary
#'   and multi-category exposures, the energy
#'   distance between groups, where lower values indicate better balance and 0
#'   indicates identical distributions. For a continuous exposure with
#'   `criterion = "dependence"`, the weighted dependence distance \eqn{D(w)},
#'   which is 0 if and only if the weighted joint distribution of the exposure
#'   and covariates factorizes into their unweighted marginals; smaller values
#'   indicate better balance, and the statistic is not bounded above by 1. For a
#'   continuous exposure with `criterion = "dcor"`, cobalt's `distance.cor`
#'   balance statistic.
#'
#' @details
#' Energy distance is based on the energy statistics framework (Székely & Rizzo, 2004)
#' and implemented following Huling & Mak (2024) and Huling et al. (2024).
#' The calculation uses a quadratic form: \eqn{w^T P w + q^T w + k},
#' where the components depend on the estimand.
#'
#' An estimand names a target distribution that the weighted groups are compared
#' against, and that target is always an unweighted distribution of the sample
#' at hand: the whole sample for `"ATE"`, and the focal group for `"ATT"` and
#' `"ATC"`. Weighting the target as well would compare the weighted groups
#' against themselves, which no weighting could fail. With uniform weights
#' `"ATT"` and `"ATC"` therefore reduce to the between-group energy distance.
#'
#' For binary variables in the .covariates, variance is calculated as p(1-p)
#' rather than sample variance to prevent over-weighting.
#'
#' For a continuous exposure, `criterion = "dependence"` returns the weighted
#' dependence distance \eqn{D(w)} of Huling, Greifer, and Chen (2023, eq. 7),
#' \deqn{D(w) = \mathrm{dCov}_w(A, X) + E_w(A) + E_w(X),}
#' the weighted distance covariance between the exposure \eqn{A} and the
#' covariates \eqn{X} plus dimension-adjusted energy distances \eqn{E_w(A)} and
#' \eqn{E_w(X)} between the weighted and unweighted marginals of the exposure and
#' of the covariates. All three terms are computed on unscaled Euclidean distance
#' matrices with the weights normalized to mean 1. The weighted distance
#' covariance alone has a false converse, since weights can shrink it while
#' distorting the marginals, so by their Theorem 3.2 it is the full \eqn{D(w)},
#' not the distance covariance, that is 0 exactly when the weights make the
#' exposure and covariates independent without distorting their unweighted
#' marginal distributions. The `dimension_adj` argument controls the relative
#' weighting of the two marginal energy terms.
#'
#' `criterion = "dcor"` instead returns cobalt's `distance.cor` balance
#' statistic, a variance-scaled distance correlation (or, with
#' `standardized = FALSE`, the corresponding square-root distance covariance).
#' This is a descriptive balance summary rather than a measure of weighted
#' dependence. The weights enter only the quadratic form that evaluates the
#' dependence; the variances the distances are scaled by and the denominator
#' that standardizes them describe the sample and are computed unweighted, so
#' the value matches
#' `cobalt::bal.compute(cobalt::bal.init(x, treat, stat = "distance.cor"), weights = w)`.
#' Following that reference implementation, a distance covariance that is not
#' positive, which a covariate with no variation produces, reports 0.
#'
#' @references
#' Huling, J. D., & Mak, S. (2024). Energy Balancing of Covariate Distributions.
#' Journal of Causal Inference, 12(1)
#' .
#' Huling, J. D., Greifer, N., & Chen, G. (2023). Independence weights for
#' causal inference with continuous treatments. *Journal of the American
#' Statistical Association*, 0(ja), 1–25. \doi{10.1080/01621459.2023.2213485}
#'
#' Székely, G. J., & Rizzo, M. L. (2004). Testing for equal distributions in
#' high dimension. InterStat, 5.
#'
#' @examples
#' # Binary treatment
#' bal_energy(
#'   .covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
#'   .exposure = nhefs_weights$qsmk
#' )
#'
#' # With weights
#' bal_energy(
#'   .covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
#'   .exposure = nhefs_weights$qsmk,
#'   .weights = nhefs_weights$w_ate
#' )
#'
#' # ATT estimand
#' bal_energy(
#'   .covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
#'   .exposure = nhefs_weights$qsmk,
#'   .weights = nhefs_weights$w_att,
#'   estimand = "ATT"
#' )
#'
#' @export
#' @importFrom stats dist model.matrix sd
bal_energy <- function(
  .covariates,
  .exposure,
  .weights = NULL,
  estimand = NULL,
  .focal_level = NULL,
  use_improved = TRUE,
  standardized = TRUE,
  criterion = c("dependence", "dcor"),
  dimension_adj = TRUE,
  exposure_type = c("auto", "binary", "categorical", "continuous"),
  na.rm = FALSE
) {
  bal_energy_impl(
    .covariates = .covariates,
    .exposure = .exposure,
    .weights = .weights,
    estimand = estimand,
    .focal_level = .focal_level,
    use_improved = use_improved,
    standardized = standardized,
    criterion = criterion,
    dimension_adj = dimension_adj,
    exposure_type = exposure_type,
    na.rm = na.rm
  )
}

#' The body of `bal_energy()`
#'
#' `exposure_type` names the type the exposure should be treated as, with
#' `"auto"` leaving the choice to the shape of the exposure, which is what a
#' direct call to `bal_energy()` does. `call` keeps errors pointing at the
#' function the user called.
#'
#' @noRd
bal_energy_impl <- function(
  .covariates,
  .exposure,
  .weights = NULL,
  estimand = NULL,
  .focal_level = NULL,
  use_improved = TRUE,
  standardized = TRUE,
  criterion = c("dependence", "dcor"),
  dimension_adj = TRUE,
  exposure_type = c("auto", "binary", "categorical", "continuous"),
  na.rm = FALSE,
  call = rlang::caller_env()
) {
  prepared <- bal_energy_prepare(
    .covariates = .covariates,
    .exposure = .exposure,
    .weights = .weights,
    estimand = estimand,
    .focal_level = .focal_level,
    use_improved = use_improved,
    standardized = standardized,
    criterion = criterion,
    dimension_adj = dimension_adj,
    exposure_type = exposure_type,
    na.rm = na.rm,
    call = call
  )

  if (is.null(prepared)) {
    return(NA_real_)
  }

  # The distance correlation scales its distances by the weighted variances, so
  # unlike the other criteria it has no weight-independent part to prepare
  if (prepared$is_continuous && prepared$criterion == "dcor") {
    return(bal_energy_continuous(
      .covariates = prepared$covariates,
      treatment = prepared$exposure,
      .weights = prepared$weights,
      standardized = prepared$standardized
    ))
  }

  bal_energy_evaluate(bal_energy_init(prepared), prepared$weights)
}

# Helper functions for bal_energy() - internal use only

#' Validate the arguments of `bal_energy()` and apply its missing-value policy
#'
#' Returns everything the statistic needs, or `NULL` when the missing values the
#' caller asked to keep make the statistic undefined. The result carries no
#' weighting decisions, so the caller can form the weight-independent pieces of
#' the statistic once and reuse them across several weight vectors.
#' @noRd
bal_energy_prepare <- function(
  .covariates,
  .exposure,
  .weights = NULL,
  estimand = NULL,
  .focal_level = NULL,
  use_improved = TRUE,
  standardized = TRUE,
  criterion = c("dependence", "dcor"),
  dimension_adj = TRUE,
  exposure_type = c("auto", "binary", "categorical", "continuous"),
  na.rm = FALSE,
  call = rlang::caller_env()
) {
  criterion <- match_option(
    criterion,
    c("dependence", "dcor"),
    "criterion",
    call = call
  )
  exposure_type <- match_option(
    exposure_type,
    c("auto", "binary", "categorical", "continuous"),
    "exposure_type",
    call = call
  )

  validate_flag(dimension_adj, "dimension_adj", call = call)
  validate_flag(use_improved, "use_improved", call = call)
  validate_flag(standardized, "standardized", call = call)
  validate_flag(na.rm, "na.rm", call = call)

  if (!is.null(estimand)) {
    if (
      !is.character(estimand) ||
        length(estimand) != 1 ||
        is.na(estimand) ||
        !estimand %in% c("ATE", "ATT", "ATC")
    ) {
      abort(
        "{.arg estimand} must be one of: {.val ATE}, {.val ATT}, {.val ATC}, or {.code NULL}",
        error_class = "halfmoon_arg_error",
        call = call
      )
    }
  }

  if (!is.null(.focal_level) && length(.focal_level) != 1) {
    abort(
      "{.arg .focal_level} must be a single value or {.code NULL}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  # Only a focal estimand has a focal group, so a focal level supplied with any
  # other estimand names a target that the comparison never uses
  if (!is.null(.focal_level) && !isTRUE(estimand %in% c("ATT", "ATC"))) {
    abort(
      c(
        "{.arg .focal_level} applies only to {.arg estimand} {.val ATT} or {.val ATC}.",
        i = "Set {.arg estimand} or drop {.arg .focal_level}."
      ),
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  if (!is.data.frame(.covariates) && !is.matrix(.covariates)) {
    abort(
      "Argument {.arg .covariates} must be a data frame or matrix",
      error_class = "halfmoon_type_error",
      call = call
    )
  }

  if (is.data.frame(.covariates)) {
    .covariates <- create_dummy_variables(.covariates, binary_as_single = TRUE)
    .covariates <- as.matrix(.covariates)
  }

  if (nrow(.covariates) == 0) {
    abort(
      "Argument {.arg .covariates} cannot be empty",
      error_class = "halfmoon_empty_error",
      call = call
    )
  }

  validate_equal_length(
    .exposure,
    .covariates,
    ".exposure",
    ".covariates",
    call = call
  )
  validate_weights(.weights, nrow(.covariates), call = call)
  .weights <- extract_weight_data(.weights)

  # Missing values the caller asked to keep leave the statistic undefined
  if (
    !na.rm &&
      (anyNA(.covariates) || anyNA(.exposure) || anyNA(.weights))
  ) {
    return(NULL)
  }

  # Rows dropped for a missing weight belong to that weight vector alone, so a
  # caller reusing the prepared pieces across weight vectors is told about them
  weights_reduced_rows <- FALSE
  if (na.rm) {
    complete_cases <- stats::complete.cases(.covariates, .exposure)
    if (!is.null(.weights)) {
      weights_reduced_rows <- anyNA(.weights[complete_cases])
      complete_cases <- complete_cases & !is.na(.weights)
      .weights <- .weights[complete_cases]
    }
    .covariates <- .covariates[complete_cases, , drop = FALSE]
    .exposure <- .exposure[complete_cases]

    if (nrow(.covariates) == 0) {
      return(NULL)
    }
  }

  unique_groups <- unique(.exposure)
  n_groups <- length(unique_groups)

  # Special case: constant group (only one unique value)
  if (n_groups <= 1) {
    abort(
      "Exposure variable must have at least two levels",
      error_class = "halfmoon_group_error",
      call = call
    )
  }

  is_continuous <- switch(
    exposure_type,
    auto = is.numeric(.exposure) && n_groups > 10,
    continuous = TRUE,
    FALSE
  )

  if (is_continuous && !is.numeric(.exposure)) {
    abort(
      "Exposure variable must be numeric when treated as continuous",
      error_class = "halfmoon_type_error",
      call = call
    )
  }

  if (is_continuous && !is.null(estimand)) {
    abort(
      "For continuous treatments, {.arg estimand} must be {.code NULL}",
      error_class = "halfmoon_arg_error",
      call = call
    )
  }

  # criterion and dimension_adj only apply to continuous exposures
  if (!is_continuous) {
    if (criterion == "dcor") {
      abort(
        "{.arg criterion} {.val dcor} is only available for continuous exposures.",
        error_class = "halfmoon_arg_error",
        call = call
      )
    }
    if (!dimension_adj) {
      abort(
        "{.arg dimension_adj} is only used for continuous exposures.",
        error_class = "halfmoon_arg_error",
        call = call
      )
    }

    # Groups are the levels the exposure actually takes, in their declared
    # order for a factor and sorted otherwise
    .exposure <- droplevels(as.factor(.exposure))
    unique_groups <- levels(.exposure)

    if (!is.null(estimand) && estimand %in% c("ATT", "ATC")) {
      .focal_level <- resolve_energy_focal_level(
        .focal_level,
        unique_groups,
        estimand,
        call = call
      )
    }
  }

  list(
    covariates = .covariates,
    exposure = .exposure,
    weights = .weights,
    unique_groups = unique_groups,
    is_continuous = is_continuous,
    estimand = estimand,
    focal_level = .focal_level,
    use_improved = use_improved,
    standardized = standardized,
    criterion = criterion,
    dimension_adj = dimension_adj,
    weights_reduced_rows = weights_reduced_rows
  )
}

#' Resolve the focal level of a focal estimand
#'
#' `NULL` takes the last observed level for the ATT and the first for the ATC,
#' so that the treated group is focal for the ATT on the usual 0/1 coding. A
#' supplied value must name a level the exposure takes.
#' @noRd
resolve_energy_focal_level <- function(
  focal_level,
  unique_groups,
  estimand,
  call = rlang::caller_env()
) {
  if (is.null(focal_level)) {
    if (estimand == "ATT") {
      return(unique_groups[[length(unique_groups)]])
    }
    return(unique_groups[[1]])
  }

  focal_level <- as.character(focal_level)
  if (!focal_level %in% unique_groups) {
    abort(
      "{.arg .focal_level} must name a level of {.arg .exposure}: {.val {unique_groups}}",
      error_class = "halfmoon_reference_error",
      call = call
    )
  }

  focal_level
}

#' Form the parts of the energy distance that do not depend on the weights
#'
#' The distance matrix and the target terms are fixed by the covariates and the
#' exposure alone, so a caller reporting several weightings of one sample forms
#' them once and evaluates each weight vector against them.
#' @noRd
bal_energy_init <- function(prepared) {
  if (prepared$is_continuous) {
    return(bal_energy_dependence_init(
      covariates = prepared$covariates,
      treatment = prepared$exposure,
      dimension_adj = prepared$dimension_adj
    ))
  }

  # Identify binary variables (checking each column)
  binary_vars <- purrr::map_lgl(as.data.frame(prepared$covariates), \(x) {
    unique_vals <- unique(x)
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  })

  standardized_covariates <- bal_energy_standardize(
    .covariates = prepared$covariates,
    binary_vars = binary_vars
  )

  distance_matrix <- as.matrix(dist(standardized_covariates))

  exposure <- prepared$exposure
  treatment_indicators <- model.matrix(~ exposure - 1)
  n_obs <- nrow(distance_matrix)

  components <- if (is.null(prepared$estimand)) {
    # Between-group energy distance only
    bal_energy_between_group(
      distance_matrix = distance_matrix,
      treatment_indicators = treatment_indicators,
      unique_groups = prepared$unique_groups
    )
  } else if (prepared$estimand == "ATE") {
    # Average treatment effect: the target is the unweighted full sample
    bal_energy_ate(
      distance_matrix = distance_matrix,
      treatment_indicators = treatment_indicators,
      unique_groups = prepared$unique_groups,
      target_dist = rep(1 / n_obs, n_obs),
      use_improved = prepared$use_improved
    )
  } else {
    # Average treatment effect on treated/controls: the target is the
    # unweighted focal group
    bal_energy_att_atc(
      distance_matrix = distance_matrix,
      treatment_indicators = treatment_indicators,
      unique_groups = prepared$unique_groups,
      .exposure = exposure,
      .focal_level = prepared$focal_level
    )
  }

  list(
    kind = "discrete",
    P = components$P,
    q = components$q,
    k = components$k,
    exposure = exposure,
    unique_groups = prepared$unique_groups
  )
}

#' Evaluate a prepared energy statistic at one weight vector
#' @noRd
bal_energy_evaluate <- function(init, weights) {
  if (init$kind == "dependence") {
    return(bal_energy_dependence_evaluate(init, weights))
  }

  n_obs <- nrow(init$P)
  if (is.null(weights)) {
    weights <- rep(1, n_obs)
  }

  # Normalize weights by group
  weights_normalized <- weights
  for (g in init$unique_groups) {
    group_mask <- init$exposure == g
    if (any(group_mask)) {
      group_weights <- weights[group_mask]
      weights_normalized[group_mask] <- group_weights / mean(group_weights)
    }
  }

  # Compute final energy distance using quadratic form
  as.numeric(t(weights_normalized) %*% init$P %*% weights_normalized) +
    sum(init$q * weights_normalized) +
    init$k
}

#' Calculate variance for a single covariate
#' @noRd
calculate_variance <- function(col, is_binary, weights_norm) {
  if (is_binary) {
    p <- sum(weights_norm * col)
    p * (1 - p)
  } else {
    mean_x <- sum(weights_norm * col)
    denom <- 1 - sum(weights_norm^2)
    if (denom <= 0) {
      sum(weights_norm * (col - mean_x)^2)
    } else {
      sum(weights_norm * (col - mean_x)^2) / denom
    }
  }
}

#' Calculate scaling factor for a single covariate
#' @noRd
calculate_scaling_factor <- function(col, is_binary) {
  if (is_binary) {
    p <- mean(col)
    sqrt(p * (1 - p))
  } else {
    sd(col)
  }
}

#' Standardize .covariates for energy distance calculation
#'
#' The spread of each covariate is measured over the whole sample rather than
#' under the weights, so that the distance matrix describes one fixed geometry
#' whatever weighting is being assessed. This is what cobalt does with its
#' sampling weights.
#' @noRd
bal_energy_standardize <- function(.covariates, binary_vars) {
  scaling_factors <- purrr::map2_dbl(
    as.data.frame(.covariates),
    binary_vars,
    calculate_scaling_factor
  )

  # Avoid division by zero
  scaling_factors[scaling_factors == 0] <- 1

  # Standardize .covariates
  scale(.covariates, center = TRUE, scale = scaling_factors)
}

#' Compute between-group energy distance components
#' @noRd
bal_energy_between_group <- function(
  distance_matrix,
  treatment_indicators,
  unique_groups
) {
  n_obs <- nrow(distance_matrix)
  n_groups <- length(unique_groups)

  # Compute group sizes
  group_sizes <- colSums(treatment_indicators)

  # Normalize indicators by group size
  normalized_indicators <- sweep(treatment_indicators, 2, group_sizes, "/")

  # Compute pairwise differences
  if (n_groups == 2) {
    # Binary case
    diff_vec <- normalized_indicators[, 1] - normalized_indicators[, 2]
    nn_matrix <- tcrossprod(diff_vec)
  } else {
    # Multi-category case
    nn_matrix <- purrr::map(
      utils::combn(seq_len(n_groups), 2, simplify = FALSE),
      \(pair) {
        diff_vec <- normalized_indicators[, pair[1]] -
          normalized_indicators[, pair[2]]
        tcrossprod(diff_vec)
      }
    ) |>
      purrr::reduce(`+`)
  }

  # Compute P matrix
  P <- -distance_matrix * nn_matrix

  # For between-group only, q and k are zero
  list(P = P, q = rep(0, n_obs), k = 0)
}

#' Compute ATE energy distance components
#'
#' `target_dist` is the distribution the weighted groups are compared against,
#' which for the ATE is the unweighted full sample: a uniform vector summing
#' to 1.
#' @noRd
bal_energy_ate <- function(
  distance_matrix,
  treatment_indicators,
  unique_groups,
  target_dist,
  use_improved
) {
  n_obs <- nrow(distance_matrix)
  n_groups <- length(unique_groups)

  # Compute group sizes
  group_sizes <- colSums(treatment_indicators)

  # Normalize indicators by group size
  normalized_indicators <- sweep(treatment_indicators, 2, group_sizes, "/")

  # Compute nn matrix
  nn_matrix <- tcrossprod(normalized_indicators)

  # Add pairwise differences if use_improved
  if (use_improved && n_groups > 1) {
    pairwise_matrices <- purrr::map(
      utils::combn(seq_len(n_groups), 2, simplify = FALSE),
      \(pair) {
        diff_vec <- normalized_indicators[, pair[1]] -
          normalized_indicators[, pair[2]]
        tcrossprod(diff_vec)
      }
    )
    nn_matrix <- nn_matrix + purrr::reduce(pairwise_matrices, `+`)
  }

  # Compute P matrix
  P <- -distance_matrix * nn_matrix

  # Compute q vector
  q <- 2 *
    as.vector(target_dist %*% distance_matrix) *
    rowSums(normalized_indicators)

  # Compute k constant
  k <- -n_groups *
    as.numeric(
      target_dist %*% distance_matrix %*% target_dist
    )

  list(P = P, q = q, k = k)
}

#' Compute ATT/ATC energy distance components
#'
#' The target distribution is the unweighted focal group, so `.focal_level` has
#' already been resolved to a level the exposure takes.
#' @noRd
bal_energy_att_atc <- function(
  distance_matrix,
  treatment_indicators,
  unique_groups,
  .exposure,
  .focal_level
) {
  n_groups <- length(unique_groups)

  # Compute group sizes
  group_sizes <- colSums(treatment_indicators)

  # Normalize indicators by group size
  normalized_indicators <- sweep(treatment_indicators, 2, group_sizes, "/")

  # Compute nn matrix
  nn_matrix <- tcrossprod(normalized_indicators)

  # Identify focal group observations
  focal_mask <- .exposure == .focal_level
  focal_target <- rep(1 / sum(focal_mask), sum(focal_mask))

  # Compute P matrix
  P <- -distance_matrix * nn_matrix

  # Compute q vector using focal group
  q <- 2 *
    as.vector(
      focal_target %*% distance_matrix[focal_mask, , drop = FALSE]
    ) *
    rowSums(normalized_indicators)

  # Compute k constant using focal group
  k <- -n_groups *
    as.numeric(
      focal_target %*%
        distance_matrix[focal_mask, focal_mask, drop = FALSE] %*%
        focal_target
    )

  list(P = P, q = q, k = k)
}

#' Form the weight-independent parts of the dependence distance D(w)
#'
#' Native implementation of the D(w) statistic of Huling, Greifer, and Chen
#' (2023), matching `independenceWeights::weighted_energy_stats(A, X, w,
#' dimension_adj)$D_w`. The exposure and covariate distance matrices are each
#' formed once and shared across the weighted distance covariance and the two
#' marginal energy terms.
#' @noRd
bal_energy_dependence_init <- function(
  covariates,
  treatment,
  dimension_adj
) {
  n_obs <- nrow(covariates)
  n_cov <- ncol(covariates)

  # Distance matrices on unscaled covariates and treatment, formed once
  cov_dist <- as.matrix(dist(covariates))
  treat_dist <- as.matrix(dist(treatment))

  # Marginal energy pieces comparing weighted and unweighted marginals
  q_energy_treat <- -treat_dist / n_obs^2
  q_energy_cov <- -cov_dist / n_obs^2
  row_energy_treat <- rowSums(treat_dist) / n_obs^2
  row_energy_cov <- rowSums(cov_dist) / n_obs^2
  mean_treat_dist <- mean(treat_dist)
  mean_cov_dist <- mean(cov_dist)

  # Double-centered distance matrices for the weighted distance covariance.
  # The matrices are symmetric, so row and column means coincide: recycling
  # subtracts the means down the rows and sweep() subtracts them across the
  # columns, avoiding the large temporary that outer() would allocate.
  cov_means <- colMeans(cov_dist)
  cov_centered <- sweep(cov_dist + mean(cov_means) - cov_means, 2, cov_means)
  treat_means <- colMeans(treat_dist)
  treat_centered <- sweep(
    treat_dist + mean(treat_means) - treat_means,
    2,
    treat_means
  )
  dcov_matrix <- cov_centered * treat_centered / n_obs^2

  # Dimension adjustment splits weight between the two marginal energy terms
  if (dimension_adj) {
    adj_treat <- 1 / sqrt(n_cov)
    adj_cov <- 1
    adj_sum <- adj_treat + adj_cov
    adj_treat <- adj_treat / adj_sum
    adj_cov <- adj_cov / adj_sum
  } else {
    adj_treat <- 0.5
    adj_cov <- 0.5
  }

  list(
    kind = "dependence",
    n_obs = n_obs,
    quad_matrix = dcov_matrix +
      q_energy_treat * adj_treat +
      q_energy_cov * adj_cov,
    lin_vec = 2 * (row_energy_treat * adj_treat + row_energy_cov * adj_cov),
    const_part = -mean_cov_dist * adj_cov - mean_treat_dist * adj_treat
  )
}

#' Evaluate the dependence distance D(w) at one weight vector
#' @noRd
bal_energy_dependence_evaluate <- function(init, weights) {
  # Weights normalized to mean 1 (unit weights when none supplied)
  if (is.null(weights)) {
    weights <- rep(1, init$n_obs)
  }
  weights <- weights / mean(weights)

  as.numeric(t(weights) %*% init$quad_matrix %*% weights) +
    as.numeric(weights %*% init$lin_vec) +
    init$const_part
}

#' Compute distance correlation for continuous treatments
#' @noRd
bal_energy_continuous <- function(
  .covariates,
  treatment,
  .weights,
  standardized
) {
  n_obs <- nrow(.covariates)

  # Default weights
  if (is.null(.weights)) {
    .weights <- rep(1, n_obs)
  }

  # Normalize weights
  weights_norm <- .weights / sum(.weights)

  # The balancing weights say how the sample is reweighted, not what the
  # statistic is measured in. The scale the distances are taken on and the
  # denominator that standardizes them are therefore properties of the sample,
  # computed unweighted, and the balancing weights enter only the quadratic
  # form that evaluates the dependence. This is the target-population reading
  # `cobalt::bal.compute()` uses for a set of candidate weights.
  scale_weights <- rep(1 / n_obs, n_obs)

  # Identify binary variables
  binary_vars <- purrr::map_lgl(as.data.frame(.covariates), \(x) {
    unique_vals <- unique(x)
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  })

  # Compute weighted variances for scaling
  covariate_vars <- purrr::map2_dbl(
    as.data.frame(.covariates),
    binary_vars,
    calculate_variance,
    weights_norm = scale_weights
  )

  # Treatment variance
  mean_t <- sum(scale_weights * treatment)
  denom <- 1 - sum(scale_weights^2)
  if (denom <= 0) {
    treatment_var <- sum(scale_weights * (treatment - mean_t)^2)
  } else {
    treatment_var <- sum(scale_weights * (treatment - mean_t)^2) / denom
  }

  # Avoid division by zero
  covariate_vars[covariate_vars == 0] <- 1
  if (treatment_var == 0) {
    treatment_var <- 1
  }

  # Scale .covariates and treatment
  scaled_.covariates <- scale(.covariates, scale = sqrt(covariate_vars))
  scaled_treatment <- treatment / sqrt(treatment_var)

  # Compute distance matrices
  cov_dist <- as.matrix(dist(scaled_.covariates))
  treat_dist <- as.matrix(dist(scaled_treatment))

  # Double-center the distance matrices. The matrices are symmetric, so row and
  # column means coincide: recycling subtracts the means down the rows and
  # sweep() subtracts them across the columns, avoiding the large temporary that
  # outer() would allocate.
  cov_means <- colMeans(cov_dist)
  cov_grand_mean <- mean(cov_means)
  cov_centered <- sweep(cov_dist + cov_grand_mean - cov_means, 2, cov_means)

  treat_means <- colMeans(treat_dist)
  treat_grand_mean <- mean(treat_means)
  treat_centered <- sweep(
    treat_dist + treat_grand_mean - treat_means,
    2,
    treat_means
  )

  # Compute P matrix
  P <- cov_centered * treat_centered

  # Compute distance covariance
  dcov <- as.numeric(t(weights_norm) %*% P %*% weights_norm)

  if (dcov <= 0) {
    return(0)
  }

  if (standardized) {
    # Compute denominators for standardization
    treat_denom <- sqrt(as.numeric(
      t(scale_weights) %*% (treat_centered^2) %*% scale_weights
    ))
    cov_denom <- sqrt(as.numeric(
      t(scale_weights) %*% (cov_centered^2) %*% scale_weights
    ))
    denom <- treat_denom * cov_denom

    if (denom <= 0) {
      return(0)
    }

    return(sqrt(dcov / denom))
  } else {
    return(sqrt(dcov))
  }
}

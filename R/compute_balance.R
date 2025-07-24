#' Balance Standardized Mean Difference (SMD)
#'
#' Calculates the standardized mean difference between two groups using the
#' smd package. This is a common measure of effect size for comparing group
#' differences while accounting for variability.
#'
#' @param covariate A numeric vector containing the covariate values to compare.
#' @param group A vector (factor or numeric) indicating group membership. Must
#'   have exactly two unique levels.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as `covariate`. All weights must be non-negative.
#' @param reference_group The reference group level to use as the comparison
#'   baseline. Can be either a group level or index. If `NULL` (default), uses the first level.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values result in
#'   `NA` output.
#' @return A numeric value representing the standardized mean difference.
#'   Positive values indicate the comparison group has a higher mean than
#'   the reference group.
#' @examples
#' bal_smd(nhefs_weights$age, nhefs_weights$qsmk)
#'
#' # With weights
#' bal_smd(nhefs_weights$wt71, nhefs_weights$qsmk,
#'             weights = nhefs_weights$w_ate)
#'
#' @export
bal_smd <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE
) {
  if (!is.numeric(covariate)) {
    stop("Argument 'covariate' must be numeric, got ", class(covariate)[1])
  }
  if (length(covariate) == 0) {
    stop("Argument 'covariate' cannot be empty")
  }
  if (length(covariate) != length(group)) {
    stop("Arguments 'covariate' and 'group' must have the same length")
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      abort("Argument {.arg weights} must be numeric or {.code NULL}")
    }
    if (length(weights) != length(covariate)) {
      stop("Argument 'weights' must have the same length as 'covariate'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      abort("Weights cannot be negative")
    }
  }

  # Handle missing values first
  if (!na.rm) {
    # Check for missing values
    if (is.null(weights)) {
      if (any(is.na(covariate) | is.na(group))) return(NA_real_)
    } else {
      if (any(is.na(covariate) | is.na(group) | is.na(weights)))
        return(NA_real_)
    }
  }

  # Convert reference_group to index if it's a level value
  levels_g <- unique(stats::na.omit(group))
  if (length(levels_g) != 2) {
    stop(
      "Grouping variable must have exactly two levels, got ",
      length(levels_g)
    )
  }

  # If reference_group is NULL, use the first level (like other functions)
  if (is.null(reference_group)) {
    gref_index <- 1L
  } else {
    # If reference_group is a level value, convert to index
    if (reference_group %in% levels_g) {
      gref_index <- which(levels_g == reference_group)
    } else {
      gref_index <- reference_group
    }
  }

  res <- smd::smd(
    x = covariate,
    g = group,
    w = weights,
    gref = gref_index,
    na.rm = na.rm
  )

  res$estimate
}

is_binary <- function(x) {
  unique_vals <- unique(stats::na.omit(x))
  length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
}

#' Balance Variance Ratio for Two Groups
#'
#' Calculates the ratio of variances between two groups: var(comparison) / var(reference).
#' For binary variables, uses the p*(1-p) variance formula. For continuous variables,
#' uses Bessel's correction for weighted sample variance.
#'
#' @param covariate A numeric vector containing the covariate values to compare.
#' @param group A vector (factor or numeric) indicating group membership. Must
#'   have exactly two unique levels.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as `covariate`. All weights must be non-negative.
#' @param reference_group The reference group level to use as the denominator.
#'   If `NULL` (default), uses the first level.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values result in
#'   `NA` output.
#' @return A numeric value representing the variance ratio. Values greater than 1
#'   indicate the comparison group has higher variance than the reference group.
#' @examples
#' bal_vr(nhefs_weights$age, nhefs_weights$qsmk)
#' # With weights
#' bal_vr(nhefs_weights$wt71, nhefs_weights$qsmk,
#'                       weights = nhefs_weights$w_ate)
#'
#' @export
bal_vr <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE
) {
  # Input validation
  if (!is.numeric(covariate)) {
    stop("Argument 'covariate' must be numeric, got ", class(covariate)[1])
  }
  if (length(covariate) == 0) {
    stop("Argument 'covariate' cannot be empty")
  }
  if (length(covariate) != length(group)) {
    stop("Arguments 'covariate' and 'group' must have the same length")
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      abort("Argument {.arg weights} must be numeric or {.code NULL}")
    }
    if (length(weights) != length(covariate)) {
      stop("Argument 'weights' must have the same length as 'covariate'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      abort("Weights cannot be negative")
    }
  }

  # Identify reference and comparison indices
  lvl <- unique(stats::na.omit(group))
  if (length(lvl) != 2) {
    stop("Grouping variable must have exactly two levels, got ", length(lvl))
  }
  ref <- if (!is.null(reference_group)) reference_group else lvl[1]
  if (!ref %in% lvl) {
    stop("Reference group '", ref, "' not found in grouping variable")
  }
  idx_ref <- which(group == ref)
  idx_other <- which(group != ref)
  # Handle missing values
  if (na.rm) {
    if (is.null(weights)) {
      idx_ref <- idx_ref[!is.na(covariate[idx_ref])]
      idx_other <- idx_other[!is.na(covariate[idx_other])]
    } else {
      idx_ref <- idx_ref[!is.na(covariate[idx_ref]) & !is.na(weights[idx_ref])]
      idx_other <- idx_other[
        !is.na(covariate[idx_other]) & !is.na(weights[idx_other])
      ]
    }
  } else {
    if (is.null(weights)) {
      if (any(is.na(covariate[c(idx_ref, idx_other)]))) return(NA_real_)
    } else {
      if (
        any(is.na(covariate[c(idx_ref, idx_other)])) ||
          any(is.na(weights[c(idx_ref, idx_other)]))
      )
        return(NA_real_)
    }
  }

  # Check if we have enough data after removing NAs
  if (length(idx_ref) == 0 || length(idx_other) == 0) {
    return(NA_real_)
  }
  # Compute variances
  if (is_binary(covariate)) {
    # For binary variables, use p*(1-p) formula
    var_ref <- if (is.null(weights)) {
      p <- mean(covariate[idx_ref])
      p * (1 - p)
    } else {
      wr <- weights[idx_ref]
      xr <- covariate[idx_ref]
      p <- sum(wr * xr) / sum(wr)
      p * (1 - p)
    }
    var_other <- if (is.null(weights)) {
      p <- mean(covariate[idx_other])
      p * (1 - p)
    } else {
      wo <- weights[idx_other]
      xo <- covariate[idx_other]
      p <- sum(wo * xo) / sum(wo)
      p * (1 - p)
    }
  } else {
    # For continuous variables, use Bessel's correction
    var_ref <- if (is.null(weights)) {
      stats::var(covariate[idx_ref])
    } else {
      wr <- weights[idx_ref]
      xr <- covariate[idx_ref]
      mr <- sum(wr * xr) / sum(wr)
      # Use Bessel's correction for weighted sample variance
      denom <- sum(wr) - sum(wr^2) / sum(wr)
      if (denom <= 0) sum(wr * (xr - mr)^2) / sum(wr) else
        sum(wr * (xr - mr)^2) / denom
    }
    var_other <- if (is.null(weights)) {
      stats::var(covariate[idx_other])
    } else {
      wo <- weights[idx_other]
      xo <- covariate[idx_other]
      mo <- sum(wo * xo) / sum(wo)
      # Use Bessel's correction for weighted sample variance
      denom <- sum(wo) - sum(wo^2) / sum(wo)
      if (denom <= 0) sum(wo * (xo - mo)^2) / sum(wo) else
        sum(wo * (xo - mo)^2) / denom
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
#' @param covariate A numeric vector containing the covariate values to compare.
#' @param group A vector (factor or numeric) indicating group membership. Must
#'   have exactly two unique levels.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as `covariate`. All weights must be non-negative.
#' @param reference_group The reference group level to use as the comparison
#'   baseline. If `NULL` (default), uses the first level.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values result in
#'   `NA` output.
#' @return A numeric value representing the KS statistic. Values range from 0 to 1,
#'   with 0 indicating identical distributions and 1 indicating completely separate
#'   distributions.
#'
#' @examples
#' bal_ks(nhefs_weights$age, nhefs_weights$qsmk)
#'
#' # With weights
#' bal_ks(nhefs_weights$wt71, nhefs_weights$qsmk,
#'            weights = nhefs_weights$w_ate)
#' @export
bal_ks <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na.rm = FALSE
) {
  # Input validation
  if (!is.numeric(covariate)) {
    stop("Argument 'covariate' must be numeric, got ", class(covariate)[1])
  }
  if (length(covariate) == 0) {
    stop("Argument 'covariate' cannot be empty")
  }
  if (length(covariate) != length(group)) {
    stop("Arguments 'covariate' and 'group' must have the same length")
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      abort("Argument {.arg weights} must be numeric or {.code NULL}")
    }
    if (length(weights) != length(covariate)) {
      stop("Argument 'weights' must have the same length as 'covariate'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      abort("Weights cannot be negative")
    }
  }

  lvl <- unique(stats::na.omit(group))
  if (length(lvl) != 2) {
    stop("Grouping variable must have exactly two levels, got ", length(lvl))
  }
  ref <- if (!is.null(reference_group)) reference_group else lvl[1]
  if (!ref %in% lvl) {
    stop("Reference group '", ref, "' not found in grouping variable")
  }
  idx_ref <- which(group == ref)
  idx_other <- which(group != ref)
  # Handle missing values
  if (na.rm) {
    if (is.null(weights)) {
      idx_ref <- idx_ref[!is.na(covariate[idx_ref])]
      idx_other <- idx_other[!is.na(covariate[idx_other])]
    } else {
      idx_ref <- idx_ref[!is.na(covariate[idx_ref]) & !is.na(weights[idx_ref])]
      idx_other <- idx_other[
        !is.na(covariate[idx_other]) & !is.na(weights[idx_other])
      ]
    }
  } else {
    if (is.null(weights)) {
      if (any(is.na(covariate[c(idx_ref, idx_other)]))) return(NA_real_)
    } else {
      if (
        any(is.na(covariate[c(idx_ref, idx_other)])) ||
          any(is.na(weights[c(idx_ref, idx_other)]))
      )
        return(NA_real_)
    }
  }

  # Check if we have enough data after removing NAs
  if (length(idx_ref) == 0 || length(idx_other) == 0) {
    return(NA_real_)
  }
  # For binary variables, KS statistic is just the difference in proportions
  if (is_binary(covariate)) {
    # Calculate weighted proportions
    p_ref <- if (is.null(weights)) {
      mean(covariate[idx_ref])
    } else {
      sum(weights[idx_ref] * covariate[idx_ref]) / sum(weights[idx_ref])
    }
    p_other <- if (is.null(weights)) {
      mean(covariate[idx_other])
    } else {
      sum(weights[idx_other] * covariate[idx_other]) / sum(weights[idx_other])
    }
    return(abs(p_other - p_ref))
  }

  # For continuous variables, compute full KS statistic
  # Extract and weight
  x_ref <- covariate[idx_ref]
  x_other <- covariate[idx_other]
  w_ref <- if (is.null(weights)) rep(1, length(x_ref)) else weights[idx_ref]
  w_other <- if (is.null(weights)) rep(1, length(x_other)) else
    weights[idx_other]
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
#' @param x A numeric vector containing the first variable.
#' @param y A numeric vector containing the second variable. Must have the same
#'   length as `x`.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as `x` and `y`. All weights must be non-negative.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values result in
#'   `NA` output.
#' @return A numeric value representing the correlation coefficient between -1 and 1.
#'   Returns `NA` if either variable has zero variance.
#'
#' @examples
#' bal_corr(nhefs_weights$age, nhefs_weights$wt71)
#'
#' @export
bal_corr <- function(x, y, weights = NULL, na.rm = FALSE) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Argument 'x' must be numeric, got ", class(x)[1])
  }
  if (!is.numeric(y)) {
    stop("Argument 'y' must be numeric, got ", class(y)[1])
  }
  if (length(x) == 0 || length(y) == 0) {
    stop("Arguments 'x' and 'y' cannot be empty")
  }
  if (length(x) != length(y)) {
    stop("Arguments 'x' and 'y' must have the same length")
  }
  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      abort("Argument {.arg weights} must be numeric or {.code NULL}")
    }
    if (length(weights) != length(x)) {
      stop("Argument 'weights' must have the same length as 'x' and 'y'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      abort("Weights cannot be negative")
    }
  }

  if (na.rm) {
    # Handle missing values carefully - avoid logical(0) issue
    if (is.null(weights)) {
      idx <- !(is.na(x) | is.na(y))
    } else {
      idx <- !(is.na(x) | is.na(y) | is.na(weights))
    }
    x <- x[idx]
    y <- y[idx]
    if (!is.null(weights)) weights <- weights[idx]
  } else {
    # Check for missing values
    if (is.null(weights)) {
      if (any(is.na(x) | is.na(y))) return(NA_real_)
    } else {
      if (any(is.na(x) | is.na(y) | is.na(weights))) return(NA_real_)
    }
  }

  # Check if we have enough data after removing NAs
  if (length(x) < 2) {
    return(NA_real_)
  }

  if (is.null(weights)) {
    return(stats::cor(x, y))
  }

  # Compute weighted covariance
  w_norm <- weights / sum(weights)
  mx <- sum(w_norm * x)
  my <- sum(w_norm * y)
  cov <- sum(w_norm * (x - mx) * (y - my))
  vx <- sum(w_norm * (x - mx)^2)
  vy <- sum(w_norm * (y - my)^2)

  if (vx <= 0 || vy <= 0) {
    return(NA_real_)
  }

  # Return standard correlation
  cov / sqrt(vx * vy)
}

#' Balance Energy Distance
#'
#' Computes the energy distance as a multivariate measure of covariate balance
#' between groups. Energy distance captures the similarity between distributions
#' across the entire joint distribution of covariates, making it more comprehensive
#' than univariate balance measures.
#'
#' @param covariates A data frame or matrix containing the covariates to compare.
#' @param group A vector (factor or numeric) indicating group membership. For
#'   binary and multi-category treatments, must have 2+ unique levels. For
#'   continuous treatments, should be numeric.
#' @param weights An optional numeric vector of weights. If provided, must
#'   have the same length as rows in `covariates`. All weights must be non-negative.
#' @param estimand Character string specifying the estimand. Options are:
#'   - NULL (default): Pure between-group energy distance comparing distributions
#'   - "ATE": Energy distance weighted to reflect balance for estimating average
#'     treatment effects across the entire population
#'   - "ATT": Energy distance weighted to reflect balance for the treated group,
#'     measuring how well controls match the treated distribution
#'   - "ATC": Energy distance weighted to reflect balance for the control group,
#'     measuring how well treated units match the control distribution
#'   For continuous treatments, only NULL is supported.
#' @param treatment_level The treatment level for ATT/ATC. If `NULL` (default),
#'   automatically determined based on estimand.
#' @param use_improved Logical. Use improved energy distance for ATE? Default is TRUE.
#'   When TRUE, adds pairwise treatment comparisons for better group separation.
#' @param standardized Logical. For continuous treatments, return standardized
#'   distance correlation? Default is TRUE.
#' @param na.rm A logical value indicating whether to remove missing values
#'   before computation. If `FALSE` (default), missing values result in
#'   an error (energy distance cannot be computed with missing data).
#'
#' @return A numeric value representing the energy distance between groups. Lower
#'   values indicate better balance, with 0 indicating perfect balance (identical
#'   distributions). For continuous treatments, returns the distance correlation
#'   coefficient (0 = independence, 1 = perfect dependence).
#'
#' @details
#' Energy distance is based on the energy statistics framework (Székely & Rizzo, 2004)
#' and implemented following Huling & Mak (2024) and Huling et al. (2024).
#' The calculation uses a quadratic form: \eqn{w^T P w + q^T w + k},
#' where the components depend on the estimand.
#'
#' For binary variables in the covariates, variance is calculated as p(1-p)
#' rather than sample variance to prevent over-weighting.
#'
#' For continuous treatments, the function uses distance correlation instead of
#' traditional energy distance, measuring independence between treatment and covariates.
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
#'   covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
#'   group = nhefs_weights$qsmk
#' )
#'
#' # With weights
#' bal_energy(
#'   covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
#'   group = nhefs_weights$qsmk,
#'   weights = nhefs_weights$w_ate
#' )
#'
#' # ATT estimand
#' bal_energy(
#'   covariates = dplyr::select(nhefs_weights, age, wt71, smokeyrs),
#'   group = nhefs_weights$qsmk,
#'   weights = nhefs_weights$w_att,
#'   estimand = "ATT"
#' )
#'
#' @export
#' @importFrom stats dist model.matrix sd
bal_energy <- function(
  covariates,
  group,
  weights = NULL,
  estimand = NULL,
  treatment_level = NULL,
  use_improved = TRUE,
  standardized = TRUE,
  na.rm = FALSE
) {
  # Input validation
  if (!is.data.frame(covariates) && !is.matrix(covariates)) {
    abort("Argument {.arg covariates} must be a data frame or matrix")
  }

  if (is.data.frame(covariates)) {
    covariates <- create_dummy_variables(covariates, binary_as_single = TRUE)
    covariates <- as.matrix(covariates)
  }

  if (nrow(covariates) == 0) {
    abort("Argument {.arg covariates} cannot be empty")
  }

  if (length(group) != nrow(covariates)) {
    abort(
      "Arguments {.arg group} and {.arg covariates} must have the same length"
    )
  }

  if (!is.null(weights)) {
    if (!is.numeric(weights)) {
      abort("Argument {.arg weights} must be numeric or {.code NULL}")
    }
    if (length(weights) != nrow(covariates)) {
      abort(
        "Argument {.arg weights} must have the same length as rows in {.arg covariates}"
      )
    }
    if (any(weights < 0, na.rm = TRUE)) {
      abort("Weights cannot be negative")
    }
  }

  if (!is.null(estimand) && !estimand %in% c("ATE", "ATT", "ATC")) {
    abort(
      "{.arg estimand} must be one of: {.val ATE}, {.val ATT}, {.val ATC}, or {.code NULL}"
    )
  }

  if (!na.rm && anyNA(covariates)) {
    abort(
      "Energy distance cannot be computed with missing values in {.arg covariates}. Set {.arg na.rm = TRUE} or remove missing values."
    )
  }

  if (!na.rm && anyNA(group)) {
    abort(
      "Energy distance cannot be computed with missing values in {.arg group}. Set {.arg na.rm = TRUE} or remove missing values."
    )
  }

  if (!na.rm && !is.null(weights) && anyNA(weights)) {
    abort(
      "Energy distance cannot be computed with missing values in {.arg weights}. Set {.arg na.rm = TRUE} or remove missing values."
    )
  }

  # Remove missing values if requested
  if (na.rm) {
    complete_cases <- stats::complete.cases(covariates, group)
    if (!is.null(weights)) {
      complete_cases <- complete_cases & !is.na(weights)
      weights <- weights[complete_cases]
    }
    covariates <- covariates[complete_cases, , drop = FALSE]
    group <- group[complete_cases]

    if (nrow(covariates) == 0) {
      return(NA_real_)
    }
  }

  # Determine treatment type
  unique_groups <- unique(group)
  n_groups <- length(unique_groups)

  # Special case: constant group (only one unique value)
  if (n_groups <= 1) {
    abort("Group variable must have at least two levels")
  }

  # Determine if treatment is continuous
  is_continuous <- is.numeric(group) && n_groups > 10

  if (is_continuous && !is.null(estimand)) {
    abort("For continuous treatments, {.arg estimand} must be {.code NULL}")
  }

  # For continuous treatments, use distance correlation
  if (is_continuous) {
    return(bal_energy_continuous(
      covariates = covariates,
      treatment = group,
      weights = weights,
      standardized = standardized
    ))
  }

  # For discrete treatments, proceed with energy distance
  # Convert group to factor for consistent handling
  group <- as.factor(group)
  unique_groups <- levels(group)
  n_groups <- length(unique_groups)

  # Default weights
  if (is.null(weights)) {
    weights <- rep(1, nrow(covariates))
  }

  # Normalize weights by group
  weights_normalized <- weights
  for (g in unique_groups) {
    group_mask <- group == g
    if (any(group_mask)) {
      group_weights <- weights[group_mask]
      weights_normalized[group_mask] <- group_weights / mean(group_weights)
    }
  }

  # Identify binary variables (checking each column)
  binary_vars <- purrr::map_lgl(as.data.frame(covariates), function(x) {
    unique_vals <- unique(x)
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  })

  # Standardize covariates
  standardized_covariates <- bal_energy_standardize(
    covariates = covariates,
    weights = weights_normalized,
    binary_vars = binary_vars,
    use_weights = is.null(weights) # Only use weights for standardization when no weights provided
  )

  # Compute distance matrix
  distance_matrix <- as.matrix(dist(standardized_covariates))

  # Create treatment indicators
  treatment_indicators <- model.matrix(~ group - 1)

  # Compute energy distance components based on estimand
  if (is.null(estimand)) {
    # Between-group energy distance only
    components <- bal_energy_between_group(
      distance_matrix = distance_matrix,
      treatment_indicators = treatment_indicators,
      unique_groups = unique_groups
    )
  } else if (estimand == "ATE") {
    # Average treatment effect
    components <- bal_energy_ate(
      distance_matrix = distance_matrix,
      treatment_indicators = treatment_indicators,
      unique_groups = unique_groups,
      weights_normalized = weights_normalized / sum(weights_normalized),
      use_improved = use_improved
    )
  } else if (estimand %in% c("ATT", "ATC")) {
    # Average treatment effect on treated/controls
    components <- bal_energy_att_atc(
      distance_matrix = distance_matrix,
      treatment_indicators = treatment_indicators,
      unique_groups = unique_groups,
      weights = weights_normalized,
      group = group,
      treatment_level = treatment_level,
      estimand = estimand
    )
  }

  # Compute final energy distance using quadratic form
  energy_distance <- as.numeric(
    t(weights_normalized) %*% components$P %*% weights_normalized
  ) +
    sum(components$q * weights_normalized) +
    components$k

  return(energy_distance)
}

# Helper functions for bal_energy() - internal use only

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
calculate_scaling_factor <- function(col, is_binary, weights_norm = NULL) {
  if (is.null(weights_norm)) {
    # Unweighted case
    if (is_binary) {
      p <- mean(col)
      sqrt(p * (1 - p))
    } else {
      sd(col)
    }
  } else {
    # Weighted case
    if (is_binary) {
      weighted_mean <- sum(weights_norm * col)
      sqrt(weighted_mean * (1 - weighted_mean))
    } else {
      weighted_mean <- sum(weights_norm * col)
      # Use Bessel's correction for weighted variance
      denom <- 1 - sum(weights_norm^2)
      if (denom <= 0) {
        # Fall back to biased estimator if correction fails
        weighted_var <- sum(weights_norm * (col - weighted_mean)^2)
      } else {
        weighted_var <- sum(weights_norm * (col - weighted_mean)^2) / denom
      }
      sqrt(weighted_var)
    }
  }
}

#' Standardize covariates for energy distance calculation
#' @noRd
bal_energy_standardize <- function(
  covariates,
  weights,
  binary_vars,
  use_weights = TRUE
) {
  if (use_weights) {
    # Normalize weights
    weights_norm <- weights / sum(weights)

    scaling_factors <- purrr::map2_dbl(
      as.data.frame(covariates),
      binary_vars,
      calculate_scaling_factor,
      weights_norm = weights_norm
    )
  } else {
    # Use unweighted standardization (to match cobalt when weights are provided)
    scaling_factors <- purrr::map2_dbl(
      as.data.frame(covariates),
      binary_vars,
      calculate_scaling_factor,
      weights_norm = NULL
    )
  }

  # Avoid division by zero
  scaling_factors[scaling_factors == 0] <- 1

  # Standardize covariates
  scale(covariates, center = TRUE, scale = scaling_factors)
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
  normalized_indicators <- purrr::map2_dfc(
    as.data.frame(treatment_indicators),
    group_sizes,
    ~ .x / .y
  ) |>
    as.matrix()

  # Compute pairwise differences
  if (n_groups == 2) {
    # Binary case
    diff_vec <- normalized_indicators[, 1] - normalized_indicators[, 2]
    nn_matrix <- tcrossprod(diff_vec)
  } else {
    # Multi-category case
    nn_matrix <- purrr::map(
      utils::combn(seq_len(n_groups), 2, simplify = FALSE),
      function(pair) {
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
#' @noRd
bal_energy_ate <- function(
  distance_matrix,
  treatment_indicators,
  unique_groups,
  weights_normalized,
  use_improved
) {
  n_obs <- nrow(distance_matrix)
  n_groups <- length(unique_groups)

  # Compute group sizes
  group_sizes <- colSums(treatment_indicators)

  # Normalize indicators by group size
  normalized_indicators <- purrr::map2_dfc(
    as.data.frame(treatment_indicators),
    group_sizes,
    ~ .x / .y
  ) |>
    as.matrix()

  # Compute nn matrix
  nn_matrix <- tcrossprod(normalized_indicators)

  # Add pairwise differences if use_improved
  if (use_improved && n_groups > 1) {
    pairwise_matrices <- purrr::map(
      utils::combn(seq_len(n_groups), 2, simplify = FALSE),
      function(pair) {
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
    as.vector(weights_normalized %*% distance_matrix) *
    rowSums(normalized_indicators)

  # Compute k constant
  k <- -n_groups *
    as.numeric(
      weights_normalized %*% distance_matrix %*% weights_normalized
    )

  list(P = P, q = q, k = k)
}

#' Compute ATT/ATC energy distance components
#' @noRd
bal_energy_att_atc <- function(
  distance_matrix,
  treatment_indicators,
  unique_groups,
  weights,
  group,
  treatment_level,
  estimand
) {
  n_obs <- nrow(distance_matrix)
  n_groups <- length(unique_groups)

  # Determine focal group
  if (is.null(treatment_level)) {
    if (estimand == "ATT") {
      # For binary, use the "treatment" group (typically coded as 1)
      treatment_level <- unique_groups[which.max(as.numeric(unique_groups))]
    } else {
      # ATC
      # Use the "control" group (typically coded as 0)
      treatment_level <- unique_groups[which.min(as.numeric(unique_groups))]
    }
  }

  # Compute group sizes
  group_sizes <- colSums(treatment_indicators)

  # Normalize indicators by group size
  normalized_indicators <- purrr::map2_dfc(
    as.data.frame(treatment_indicators),
    group_sizes,
    ~ .x / .y
  ) |>
    as.matrix()

  # Compute nn matrix
  nn_matrix <- tcrossprod(normalized_indicators)

  # Identify focal group observations
  focal_mask <- group == treatment_level
  focal_weights <- weights[focal_mask]
  focal_weights_norm <- focal_weights / sum(focal_weights)

  # Compute P matrix
  P <- -distance_matrix * nn_matrix

  # Compute q vector using focal group
  q <- 2 *
    as.vector(
      focal_weights_norm %*% distance_matrix[focal_mask, , drop = FALSE]
    ) *
    rowSums(normalized_indicators)

  # Compute k constant using focal group
  k <- -n_groups *
    as.numeric(
      focal_weights_norm %*%
        distance_matrix[focal_mask, focal_mask, drop = FALSE] %*%
        focal_weights_norm
    )

  list(P = P, q = q, k = k)
}

#' Compute distance correlation for continuous treatments
#' @noRd
bal_energy_continuous <- function(
  covariates,
  treatment,
  weights,
  standardized
) {
  n_obs <- nrow(covariates)

  # Default weights
  if (is.null(weights)) {
    weights <- rep(1, n_obs)
  }

  # Normalize weights
  weights_norm <- weights / sum(weights)

  # Identify binary variables
  binary_vars <- purrr::map_lgl(as.data.frame(covariates), function(x) {
    unique_vals <- unique(x)
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  })

  # Compute weighted variances for scaling
  covariate_vars <- purrr::map2_dbl(
    as.data.frame(covariates),
    binary_vars,
    calculate_variance,
    weights_norm = weights_norm
  )

  # Treatment variance
  mean_t <- sum(weights_norm * treatment)
  denom <- 1 - sum(weights_norm^2)
  if (denom <= 0) {
    treatment_var <- sum(weights_norm * (treatment - mean_t)^2)
  } else {
    treatment_var <- sum(weights_norm * (treatment - mean_t)^2) / denom
  }

  # Avoid division by zero
  covariate_vars[covariate_vars == 0] <- 1
  if (treatment_var == 0) treatment_var <- 1

  # Scale covariates and treatment
  scaled_covariates <- scale(covariates, scale = sqrt(covariate_vars))
  scaled_treatment <- treatment / sqrt(treatment_var)

  # Compute distance matrices
  cov_dist <- as.matrix(dist(scaled_covariates))
  treat_dist <- as.matrix(dist(scaled_treatment))

  # Double-center the distance matrices
  cov_means <- colMeans(cov_dist)
  cov_grand_mean <- mean(cov_means)
  cov_centered <- cov_dist + cov_grand_mean - outer(cov_means, cov_means, "+")

  treat_means <- colMeans(treat_dist)
  treat_grand_mean <- mean(treat_means)
  treat_centered <- treat_dist +
    treat_grand_mean -
    outer(treat_means, treat_means, "+")

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
      t(weights_norm) %*% (treat_centered^2) %*% weights_norm
    ))
    cov_denom <- sqrt(as.numeric(
      t(weights_norm) %*% (cov_centered^2) %*% weights_norm
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

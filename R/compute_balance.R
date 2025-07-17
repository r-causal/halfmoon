#' Compute Standardized Mean Difference (SMD)
#'
#' Calculates the standardized mean difference between two groups using the
#' smd package. This is a common measure of effect size for comparing group
#' differences while accounting for variability.
#'
#' @param covariate A numeric vector containing the covariate values to compare.
#' @param group A vector (factor or numeric) indicating group membership. Must
#'   have exactly two unique levels.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as \code{covariate}. All weights must be non-negative.
#' @param reference_group The reference group level to use as the comparison
#'   baseline. Can be either a group level or index. Defaults to the first level.
#' @param na_rm A logical value indicating whether to remove missing values
#'   before computation. If \code{FALSE} (default), missing values result in
#'   \code{NA} output.
#' @return A numeric value representing the standardized mean difference.
#'   Positive values indicate the comparison group has a higher mean than
#'   the reference group.
#' @examples
#' # Basic usage
#' compute_smd(c(1, 2, 3, 4, 5), c("A", "A", "B", "B", "B"))
#'
#' # With weights
#' compute_smd(c(1, 2, 3, 4, 5), c("A", "A", "B", "B", "B"),
#'             weights = c(1, 1, 2, 2, 2))
#' @export
compute_smd <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = 1L,
  na_rm = FALSE
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
      stop("Argument 'weights' must be numeric or NULL")
    }
    if (length(weights) != length(covariate)) {
      stop("Argument 'weights' must have the same length as 'covariate'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      stop("Weights cannot be negative")
    }
  }

  # Handle missing values first
  if (!na_rm) {
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

  # If reference_group is a level value, convert to index
  if (reference_group %in% levels_g) {
    gref_index <- which(levels_g == reference_group)
  } else {
    gref_index <- reference_group
  }

  # Delegate to smd::smd and return the estimate
  res <- smd::smd(
    x = covariate,
    g = group,
    w = weights,
    gref = gref_index,
    na.rm = na_rm
  )
  res$estimate
}

is_binary <- function(x) {
  unique_vals <- unique(stats::na.omit(x))
  length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
}

#' Compute Variance Ratio for Two Groups
#'
#' Calculates the ratio of variances between two groups: var(comparison) / var(reference).
#' For binary variables, uses the p*(1-p) variance formula. For continuous variables,
#' uses Bessel's correction for weighted sample variance.
#'
#' @param covariate A numeric vector containing the covariate values to compare.
#' @param group A vector (factor or numeric) indicating group membership. Must
#'   have exactly two unique levels.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as \code{covariate}. All weights must be non-negative.
#' @param reference_group The reference group level to use as the denominator.
#'   If \code{NULL} (default), uses the first level.
#' @param na_rm A logical value indicating whether to remove missing values
#'   before computation. If \code{FALSE} (default), missing values result in
#'   \code{NA} output.
#' @return A numeric value representing the variance ratio. Values greater than 1
#'   indicate the comparison group has higher variance than the reference group.
#' @examples
#' # Basic usage
#' compute_variance_ratio(c(1, 2, 3, 4, 5), c("A", "A", "B", "B", "B"))
#'
#' # With reference group specified
#' compute_variance_ratio(c(1, 2, 3, 4, 5), c("A", "A", "B", "B", "B"),
#'                        reference_group = "B")
#' @export
compute_variance_ratio <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na_rm = FALSE
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
      stop("Argument 'weights' must be numeric or NULL")
    }
    if (length(weights) != length(covariate)) {
      stop("Argument 'weights' must have the same length as 'covariate'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      stop("Weights cannot be negative")
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
  if (na_rm) {
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
    # For binary variables, use p*(1-p) formula like cobalt
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

#' Compute Kolmogorov-Smirnov (KS) Statistic for Two Groups
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
#'   have the same length as \code{covariate}. All weights must be non-negative.
#' @param reference_group The reference group level to use as the comparison
#'   baseline. If \code{NULL} (default), uses the first level.
#' @param na_rm A logical value indicating whether to remove missing values
#'   before computation. If \code{FALSE} (default), missing values result in
#'   \code{NA} output.
#' @return A numeric value representing the KS statistic. Values range from 0 to 1,
#'   with 0 indicating identical distributions and 1 indicating completely separate
#'   distributions.
#' @examples
#' # Basic usage
#' compute_ks(c(1, 2, 3, 4, 5), c("A", "A", "B", "B", "B"))
#'
#' # With weights
#' compute_ks(c(1, 2, 3, 4, 5), c("A", "A", "B", "B", "B"),
#'            weights = c(1, 1, 2, 2, 2))
#' @export
compute_ks <- function(
  covariate,
  group,
  weights = NULL,
  reference_group = NULL,
  na_rm = FALSE
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
      stop("Argument 'weights' must be numeric or NULL")
    }
    if (length(weights) != length(covariate)) {
      stop("Argument 'weights' must have the same length as 'covariate'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      stop("Weights cannot be negative")
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
  if (na_rm) {
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
  diff <- purrr::map_dbl(allv, function(v) {
    F_r <- ifelse(any(x_r <= v), c_r[which.max(x_r[x_r <= v])], 0)
    F_o <- ifelse(any(x_o <= v), c_o[which.max(x_o[x_o <= v])], 0)
    abs(F_o - F_r)
  })
  max(diff)
}

#' Compute Weighted or Unweighted Pearson Correlation
#'
#' Calculates the Pearson correlation coefficient between two numeric vectors,
#' with optional case weights. Uses the standard correlation formula for
#' unweighted data and weighted covariance for weighted data.
#'
#' @param x A numeric vector containing the first variable.
#' @param y A numeric vector containing the second variable. Must have the same
#'   length as \code{x}.
#' @param weights An optional numeric vector of case weights. If provided, must
#'   have the same length as \code{x} and \code{y}. All weights must be non-negative.
#' @param na_rm A logical value indicating whether to remove missing values
#'   before computation. If \code{FALSE} (default), missing values result in
#'   \code{NA} output.
#' @return A numeric value representing the correlation coefficient between -1 and 1.
#'   Returns \code{NA} if either variable has zero variance.
#' @examples
#' # Basic usage
#' compute_correlation(c(1, 2, 3, 4, 5), c(2, 4, 6, 8, 10))
#'
#' # With weights
#' compute_correlation(c(1, 2, 3, 4, 5), c(2, 4, 6, 8, 10),
#'                     weights = c(1, 1, 2, 2, 2))
#' @export
compute_correlation <- function(x, y, weights = NULL, na_rm = FALSE) {
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
      stop("Argument 'weights' must be numeric or NULL")
    }
    if (length(weights) != length(x)) {
      stop("Argument 'weights' must have the same length as 'x' and 'y'")
    }
    if (any(weights < 0, na.rm = TRUE)) {
      stop("Weights cannot be negative")
    }
  }

  if (na_rm) {
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

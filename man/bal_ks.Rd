% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_balance.R
\name{bal_ks}
\alias{bal_ks}
\title{Balance Kolmogorov-Smirnov (KS) Statistic for Two Groups}
\usage{
bal_ks(covariate, group, weights = NULL, reference_group = NULL, na.rm = FALSE)
}
\arguments{
\item{covariate}{A numeric vector containing the covariate values to compare.}

\item{group}{A vector (factor or numeric) indicating group membership. Must
have exactly two unique levels.}

\item{weights}{An optional numeric vector of case weights. If provided, must
have the same length as other input vectors. All weights must be non-negative.}

\item{reference_group}{The reference group level for comparisons. Can be either
a group level value or a numeric index. If \code{NULL} (default), uses the first level.}

\item{na.rm}{A logical value indicating whether to remove missing values
before computation. If \code{FALSE} (default), missing values in the input
will produce \code{NA} in the output.}
}
\value{
A numeric value representing the KS statistic. Values range from 0 to 1,
with 0 indicating identical distributions and 1 indicating completely separate
distributions.
}
\description{
Computes the two-sample KS statistic comparing empirical cumulative distribution
functions (CDFs) between two groups. For binary variables, returns the absolute
difference in proportions. For continuous variables, computes the maximum
difference between empirical CDFs.
}
\details{
The Kolmogorov-Smirnov statistic measures the maximum difference between
empirical cumulative distribution functions of two groups:
\deqn{KS = \max_x |F_1(x) - F_0(x)|}
where \eqn{F_1(x)} and \eqn{F_0(x)} are the empirical CDFs of the treatment
and control groups.

For binary variables, this reduces to the absolute difference in proportions.
For continuous variables, the statistic captures differences in the entire
distribution shape, not just means or variances.

The KS statistic ranges from 0 (identical distributions) to 1 (completely
separate distributions). Smaller values indicate better distributional balance
between groups.
}
\examples{
bal_ks(nhefs_weights$age, nhefs_weights$qsmk)

# With weights
bal_ks(nhefs_weights$wt71, nhefs_weights$qsmk,
           weights = nhefs_weights$w_ate)
}
\seealso{
\code{\link[=check_balance]{check_balance()}} for computing multiple balance metrics at once

Other balance functions: 
\code{\link{bal_corr}()},
\code{\link{bal_smd}()},
\code{\link{bal_vr}()},
\code{\link{check_auc}()},
\code{\link{check_balance}()},
\code{\link{plot_balance}()}
}
\concept{balance functions}

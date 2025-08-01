% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_auc.R
\name{check_auc}
\alias{check_auc}
\title{Check Balance Using Weighted ROC Curves}
\usage{
check_auc(
  .data,
  .truth,
  .estimate,
  .wts,
  include_observed = TRUE,
  na.rm = TRUE,
  treatment_level = NULL
)
}
\arguments{
\item{.data}{A data frame containing the variables.}

\item{.truth}{The treatment/outcome variable.}

\item{.estimate}{The propensity score or fitted values.}

\item{.wts}{Weighting variables (supports tidyselect).}

\item{include_observed}{Logical. If using \code{.wts}, also calculate observed
(unweighted) metrics? Defaults to TRUE.}

\item{na.rm}{A logical value indicating whether to remove missing values
before computation. If \code{FALSE} (default), missing values in the input
will produce \code{NA} in the output.}

\item{treatment_level}{The level of the outcome variable to consider as the
treatment/event. If \code{NULL} (default), uses the last level for factors or
the maximum value for numeric variables.}
}
\value{
A tibble with columns:
\item{method}{Character. The weighting method ("observed" or weight variable name).}
\item{auc}{Numeric. The ROC AUC value.}
}
\description{
Computes weighted ROC curves and AUC for evaluating propensity score balance.
In causal inference, a weighted ROC curve near the diagonal (AUC around 0.5)
indicates good balance between treatment groups.
}
\details{
The Area Under the ROC Curve (AUC) provides a single metric for assessing
propensity score balance. When propensity scores achieve perfect balance,
the weighted distribution of scores should be identical between treatment
groups, resulting in an AUC of 0.5 (chance performance).

AUC values significantly different from 0.5 indicate systematic differences
in propensity score distributions between groups, suggesting inadequate
balance. Values closer to 0.5 indicate better balance achieved by the
weighting scheme.

This approach complements traditional balance diagnostics by focusing
specifically on the propensity score overlap and balance.
}
\examples{
# Check balance for propensity scores
check_auc(nhefs_weights, qsmk, .fitted, c(w_ate, w_att))

# Without observed results
check_auc(nhefs_weights, qsmk, .fitted, w_ate, include_observed = FALSE)

}
\seealso{
\code{\link[=check_balance]{check_balance()}} for other balance metrics, \code{\link[=geom_roc]{geom_roc()}} for plotting ROC curves

Other balance functions: 
\code{\link{bal_corr}()},
\code{\link{bal_ks}()},
\code{\link{bal_smd}()},
\code{\link{bal_vr}()},
\code{\link{check_balance}()},
\code{\link{plot_balance}()}
}
\concept{balance functions}

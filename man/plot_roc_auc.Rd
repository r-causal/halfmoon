% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_roc.R
\name{plot_roc_auc}
\alias{plot_roc_auc}
\title{Plot ROC AUC Values for Balance Assessment}
\usage{
plot_roc_auc(
  .data,
  ref_line = TRUE,
  ref_color = "red",
  point_size = 3,
  point_shape = 19
)
}
\arguments{
\item{.data}{Output from \code{check_auc()}.}

\item{ref_line}{Show reference line at AUC = 0.5? Default is TRUE.}

\item{ref_color}{Color for reference line. Default is "red".}

\item{point_size}{Size of the points. Default is 3.}

\item{point_shape}{Shape of the points. Default is 19 (filled circle).}
}
\value{
A ggplot2 object.
}
\description{
Creates a visualization of AUC values from weighted ROC analysis.
Values near 0.5 indicate good balance.
}
\examples{
# Compute AUC values
auc_data <- check_auc(
  nhefs_weights,
  qsmk,
  .fitted,
  c(w_ate, w_att)
)

# Create plot
plot_roc_auc(auc_data)

}

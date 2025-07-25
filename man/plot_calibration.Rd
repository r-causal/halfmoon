% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_calibration.R
\name{plot_calibration}
\alias{plot_calibration}
\title{Create calibration plot}
\usage{
plot_calibration(
  .data,
  .fitted,
  .group,
  treatment_level = NULL,
  method = "breaks",
  bins = 10,
  smooth = TRUE,
  conf_level = 0.95,
  window_size = 0.1,
  step_size = window_size/2,
  k = 10,
  include_rug = FALSE,
  include_ribbon = TRUE,
  include_points = TRUE,
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{.data}{A data frame containing the variables.}

\item{.fitted}{Column name of predicted probabilities (propensity scores).
Can be unquoted (e.g., \code{.fitted}) or quoted (e.g., \code{".fitted"}).}

\item{.group}{Column name of treatment/group variable.
Can be unquoted (e.g., \code{qsmk}) or quoted (e.g., \code{"qsmk"}).}

\item{treatment_level}{Value indicating which level of \code{.group} represents treatment.
If NULL (default), uses the last level for factors or max value for numeric.}

\item{method}{Character; calibration method - "breaks", "logistic", or "windowed".}

\item{bins}{Integer >1; number of bins for the "breaks" method.}

\item{smooth}{Logical; for "logistic" method, use GAM smoothing if available.}

\item{conf_level}{Numeric in (0,1); confidence level for CIs (default = 0.95).}

\item{window_size}{Numeric; size of each window for "windowed" method.}

\item{step_size}{Numeric; distance between window centers for "windowed" method.}

\item{k}{Integer; the basis dimension for GAM smoothing when method = "logistic" and smooth = TRUE. Default is 10.}

\item{include_rug}{Logical; add rug plot showing distribution of predicted probabilities.}

\item{include_ribbon}{Logical; show confidence interval ribbon.}

\item{include_points}{Logical; show points (only for "breaks" and "windowed" methods).}

\item{na.rm}{Logical; if TRUE, drop NA values before computation.}

\item{...}{Additional parameters passed to \code{geom_calibration()}.}
}
\value{
A ggplot2 object.
}
\description{
Create a calibration plot to assess the agreement between predicted
probabilities and observed treatment rates. This function wraps
\code{geom_calibration()}.
}
\examples{
library(ggplot2)

# Basic calibration plot
plot_calibration(nhefs_weights, .fitted, qsmk)

# With rug plot
plot_calibration(nhefs_weights, .fitted, qsmk, include_rug = TRUE)

# Different methods
plot_calibration(nhefs_weights, .fitted, qsmk, method = "logistic")
plot_calibration(nhefs_weights, .fitted, qsmk, method = "windowed")

# Specify treatment level explicitly
plot_calibration(nhefs_weights, .fitted, qsmk, treatment_level = "1")
}

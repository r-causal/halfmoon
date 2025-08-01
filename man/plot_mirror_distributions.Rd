% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_mirror_distributions.R
\name{plot_mirror_distributions}
\alias{plot_mirror_distributions}
\title{Create mirror distribution plots}
\usage{
plot_mirror_distributions(
  .data,
  .var,
  .group,
  .wts = NULL,
  type = c("histogram", "density"),
  mirror_axis = "y",
  bins = NULL,
  binwidth = NULL,
  bw = "nrd0",
  adjust = 1,
  include_unweighted = TRUE,
  alpha = 0.6,
  na.rm = FALSE
)
}
\arguments{
\item{.data}{A data frame containing the variables.}

\item{.var}{The variable to plot. Supports tidyselect syntax. Can be unquoted.}

\item{.group}{Column name of treatment/group variable. Supports tidyselect syntax. Can be unquoted.
Must have exactly 2 levels.}

\item{.wts}{Optional weighting variable(s). Can be unquoted variable names, tidyselect syntax,
a character vector, or NULL. Multiple weights can be provided to compare
different weighting schemes. Default is NULL (unweighted).}

\item{type}{Character; type of plot - "histogram" or "density". Default is "histogram".}

\item{mirror_axis}{Character; which axis to mirror - "y" (default) or "x".}

\item{bins}{Integer; number of bins for histogram. Only used when type = "histogram".}

\item{binwidth}{Numeric; width of bins for histogram. Only used when type = "histogram".
If both bins and binwidth are specified, binwidth takes precedence.}

\item{bw}{Bandwidth for density estimation. Only used when type = "density".
Can be numeric or character (e.g., "nrd0", "sj").}

\item{adjust}{Numeric; bandwidth adjustment factor for density. Only used when
type = "density". Default is 1.}

\item{include_unweighted}{Logical. If using \code{.wts}, also show unweighted
distribution? Defaults to TRUE.}

\item{alpha}{Numeric; transparency level for fills. Default is 0.6.}

\item{na.rm}{Logical; if TRUE, drop NA values before plotting.}
}
\value{
A ggplot2 object.
}
\description{
Create mirror distribution plots (histograms or density plots) to compare
the distribution of variables between treatment groups before and after
weighting. This function helps assess covariate balance by visualizing
the distributions side-by-side with one group mirrored below the axis.
}
\details{
Mirror distribution plots display the distribution of one group above the
x-axis and the other group below (mirrored). This makes it easy to compare
distributions and assess balance. The function supports both histogram and
density plot types.

When using weights, the function can display both weighted and unweighted
distributions for comparison. Multiple weighting schemes can be compared
by providing multiple weight variables.
}
\examples{
library(ggplot2)

# Basic histogram (unweighted)
plot_mirror_distributions(nhefs_weights, age, qsmk)

# Density plot instead of histogram
plot_mirror_distributions(nhefs_weights, age, qsmk, type = "density")

# With weighting
plot_mirror_distributions(nhefs_weights, age, qsmk, .wts = w_ate)

# Compare multiple weighting schemes
plot_mirror_distributions(nhefs_weights, age, qsmk, .wts = c(w_ate, w_att))

# Customize appearance
plot_mirror_distributions(
  nhefs_weights, age, qsmk,
  .wts = w_ate,
  type = "density",
  alpha = 0.7
)

# Without unweighted comparison
plot_mirror_distributions(
  nhefs_weights, age, qsmk,
  .wts = w_ate,
  include_unweighted = FALSE
)

}
\seealso{
\itemize{
\item \code{\link[=geom_mirror_histogram]{geom_mirror_histogram()}} for the underlying histogram geom
\item \code{\link[=geom_mirror_density]{geom_mirror_density()}} for the underlying density geom
\item \code{\link[=plot_qq]{plot_qq()}} for QQ plots, another distributional comparison
\item \code{\link[=geom_ecdf]{geom_ecdf()}} for ECDF plots
}
}

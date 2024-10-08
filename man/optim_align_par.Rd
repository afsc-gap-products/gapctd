% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alignment_functions.R
\name{optim_align_par}
\alias{optim_align_par}
\title{Estimate channel alignment parameter (R workflow)}
\usage{
optim_align_par(
  x,
  cast_direction,
  variable = "temperature",
  offsets = seq(-1, 1, 0.01),
  cor_method = "pearson",
  ...
)
}
\arguments{
\item{x}{oce object}

\item{cast_direction}{Cast direction as a character vector ("downcast" or "upcast"). Passed to gapctd::slowdown()}

\item{variable}{Variable to be aligned ("temperature")}

\item{offsets}{Numerical vector of offsets to evaluate, in seconds.}

\item{cor_method}{Correlation method, passed to gapctd::channel_correlation().}

\item{...}{Additional arguments passed to gapctd::channel_correlation()}
}
\value{
A named numeric vector containing the estimated optimal offset ("best_offset") and correlation for the offset ("best_corr")
}
\description{
Estimate optimal alignment parameter for a channel (e.g. "temperature") within a range of candidate alignment offsets. The 'best' alignment is taken to be the one that maximizes the correlation between channels.
}
\author{
Sean Rohan
}

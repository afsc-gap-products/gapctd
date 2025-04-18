% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/window_filter.R
\name{window_filter}
\alias{window_filter}
\title{Window filter}
\usage{
window_filter(
  x,
  method,
  variables,
  window,
  offset = NULL,
  halfwidth = NULL,
  freq_n = NULL
)
}
\arguments{
\item{x}{ctd object}

\item{method}{Filtering method options are "median", "boxcar", "cosine", "gaussian", "triangle"}

\item{variables}{Character vector of data variable names to filter, such as "temperature", "conductivity".}

\item{window}{Numeric vector of windows (i.e. number of scans for the window}

\item{offset}{For "gaussian" filtering. Value of the offset parameter. SBEDP uses as defaults: offset = 0, halfwidth = 1}

\item{halfwidth}{For "gaussian" filter. Value of the halfwidth parameter.}

\item{freq_n}{For "gaussian" filter. Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.}
}
\value{
ctd object with channels filtered
}
\description{
Applies a window filter to variable channels using a user-specified filtering method.
}
\author{
Sean Rohan
}

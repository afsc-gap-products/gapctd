% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctm_functions.R
\name{ctm_obj}
\alias{ctm_obj}
\title{Objective function for cell thermal mass calculations (R workflow)}
\usage{
ctm_obj(dc = NULL, uc = NULL, alpha_C, beta_C, area_method = "ts")
}
\arguments{
\item{dc}{Downcast oce object}

\item{uc}{Upcast oce object}

\item{alpha_C}{Conductivity cell thermal inertia correction alpha parameter, passed to gapctd::conductivity_correction()}

\item{beta_C}{Conductivity cell thermal inertia correction beta parameter, passed to gapctd::conductivity_correction()}

\item{area_method}{Area between temperature-salinity ("ts") curves, depth-salinity curves ("zs"), or pressure-salinity curves ("ps")}
}
\value{
Area between T-S curves, Z-S curves, or path distance of salinity curve as a 1L numeric vector
}
\description{
Calculates area between temperature and salinity curves when upcast and downcast data are provided. Calculates path distance for salinity if only downcast or upcast is provided.
}
\author{
Sean Rohan
}

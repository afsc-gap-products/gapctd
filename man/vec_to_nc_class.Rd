% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_to_netcdf.R
\name{vec_to_nc_class}
\alias{vec_to_nc_class}
\title{Detect R vector class and return equivalent netCDF class}
\usage{
vec_to_nc_class(vec, pkg = "RNetCDF")
}
\arguments{
\item{vec}{A vector of any type.}

\item{pkg}{Package to format output for, either RNetCDF or ncdf4}
}
\description{
Supports numeric, integer, and character classes.
}
\author{
Sean Rohan
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_to_netcdf.R
\name{make_metadata_file}
\alias{make_metadata_file}
\title{Calculate bottom variables and make metadata file from cast .rds files (R workflow)}
\usage{
make_metadata_file(
  rds_dir_path = here::here("output", "gapctd"),
  in_pattern = "_qc.rds",
  output_path = NULL
)
}
\arguments{
\item{rds_dir_path}{Path to a directory containing rds files that include oce objects named 'bottom'}

\item{in_pattern}{Character vector search pattern for input files.}

\item{output_path}{Output file path (.rds) where file should be saved.}
}
\value{
data.frame containing metadata for all deployments.
}
\description{
Calculates means of bottom variables using calc_bottom_mean_oce and retrieves metadata from .rds files, and combines bottom and metadata for all casts in rds_dir_path.
}
\author{
Sean Rohan
}

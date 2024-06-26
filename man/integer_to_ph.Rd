% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hex_to_cnv.R
\name{integer_to_ph}
\alias{integer_to_ph}
\title{Convert SBE integer to pH}
\usage{
integer_to_ph(
  ph_integer,
  ph_offset,
  ph_slope,
  temperature,
  sig_figs = 3,
  par0 = 13107
)
}
\arguments{
\item{ph_integer}{pH voltage integer}

\item{ph_offset}{pH calibration parameter offset}

\item{ph_slope}{pH calibration parameter slope}

\item{temperature}{temperature in degrees C}

\item{sig_figs}{number of significant digits to use for conductivity (default = 3)}

\item{par0}{ph_integer conversion constant}
}
\description{
Convert SBE integer to pH
}
\author{
Sean Rohan
}

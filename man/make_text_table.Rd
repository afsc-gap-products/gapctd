% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df_to_netcdf.R
\name{make_text_table}
\alias{make_text_table}
\title{Write CTD data to a text file}
\usage{
make_text_table(
  x,
  output_file,
  column_descriptions = c(vessel = "vessel: AFSC/RACE vessel code", cruise =
    "cruise: AFSC/RACE cruise code", haul = "haul: AFSC/RACE haul number", stationid =
    "stationid: AFSC/RACE station code", serial_number =
    "serial_number: Primary instrument serial number", cast_direction =
    "cast_directon: Cast direction", datetime =
    "datetime: date and time in Alaska Daylight Time [UTC-9:00]", depth =
    "depth: depth [m], down is positive", pressure =
    "pressure: pressure, strain gauge [db]", conductivity =
    "conductivity: conductivity [S/m]", 
     temperature =
    "temperature: temperature [ITS-90, degrees C]", salinity =
    "salinity: salinity [PSS-78]", sound_speed =
    "sound_speed: Chen-Millero sound speed [m/s]", oxygen =
    "oxygen: dissolved oxygen [ml/l]", pH = "ph: pH", flag = "flag: data quality flag"),
  ctd_unit,
  auxiliary_sensors,
  dataset_name,
  cruise_name,
  creator_name,
  creator_email,
  creator_institution,
  collaborators = NULL,
  references,
  dataset_doi,
  ncei_accession_number,
  processing_info,
  publisher_url
)
}
\arguments{
\item{x}{A data.frame with column names that correspond with names in column_descriptions}

\item{output_file}{Output filepath for text file.}

\item{column_descriptions}{A named character vector with descriptions of the values in each output column.}

\item{ctd_unit}{CTD instrument model (character vector)}

\item{auxiliary_sensors}{Names of auxiliary sensors, such as dissolved oxygen and pH sensors (character vector)}

\item{dataset_name}{Name of the data set (character vector)}

\item{cruise_name}{Name of the cruise (character vector)}

\item{creator_name}{Name of the person who created the data set (character vector)}

\item{creator_email}{Email address for the person who created the data set (character vector)}

\item{creator_institution}{Name of the institution for the cruise/data set creator (character vector)}

\item{collaborators}{Names of contributors/collaborators (character vector)}

\item{references}{Any relevant references for the data set (character vector)}

\item{dataset_doi}{Digital object identifier for the data set (character vector)}

\item{ncei_accession_number}{NCEI Accession Number for the data product (character or numeric vector)}

\item{processing_info}{Additional information about data processing, e.g. gapctd version used for processing (character vector)}

\item{publisher_url}{URL for the dataset creator, e.g., https://github.com/afsc-gap-products/gapctd (character vector)}
}
\description{
Writes data to a csv file with metadata in a header.
}
\author{
Sean Rohan
}

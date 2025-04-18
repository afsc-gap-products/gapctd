# Generate data products
library(gapctd)

# Replace this with paths to cast rds files that should be included in the data product.
cast_files <- c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AKK/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST_1/final_cnv/", full.names = TRUE)
)

# Replace this with paths to metadata rds files that should be included in the data product.
metadata_files <- c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AKK/metadata", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST/metadata", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST_1/metadata", full.names = TRUE)
)


# Update this section with the relevant metadata
year <- 2021
region <- "EBS" # Only use EBS if the region is EBS and NBS.
dataset_name <- "CTD Data from AFSC 2021 EBS Shelf and NBS Bottom Trawl Surveys"
cruise_name <- "2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Surveys"
ctd_team <- "Nicole Charriere, Cecilia O'Leary, Nate Raring, Bethany Riggle" # Do not list yourself!
creator_name <- "Sean Rohan"
creator_email <- "sean.rohan@noaa.gov"
dataset_citation <- "Rohan, S.K., Charriere, N.E., Riggle, B., O'Leary, C.A., Raring, N.W. (2024). Water temperature, salinity, and others taken by trawl-mounted CTD from chartered fishing vessels during the 2021-2024 Alaska Fisheries Science Center Eastern and Northern Bering Sea Annual Summer Bottom Trawl Surveys (NCEI Accession 0286094). [indicate subset used]. NOAA National Centers for Environmental Information. Dataset. https://doi.org/10.25921/rkh7-7a49. Accessed [date]."
dataset_doi <- "10.25921/rkh7-7a49"
ctd_unit <- "Sea-Bird SBE19plus V2 SeaCAT"
references <- "Rohan, S. K., Charriere, N. E., Riggle, B., O’Leary, C. A., and Raring, N. W. 2023. A flexible approach for processing data collected using trawl-mounted CTDs during Alaska bottom-trawl surveys. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-AFSC-475, 43 p. https://doi.org/10.25923/8ape-q461"
creator_institution <- "NOAA Alaska Fisheries Science Center"
processing_info <- paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd"))
ncei_accession_number <- ""
publisher_url <- "https://github.com/afsc-gap-products/gapctd"
auxiliary_sensors <- ""
text_output_columns <- c("vessel", 
                   "cruise", 
                   "haul", 
                   "stationid", 
                   "serial_number", 
                   "cast_direction", 
                   "timeS", 
                   "depth", 
                   "pressure", 
                   "temperature", 
                   "conductivity", 
                   "salinity", 
                   "sound_speed", 
                   # "oxygen", 
                   # "pH", 
                   "flag")

column_descriptions <- c(
  "vessel" = "vessel: AFSC/RACE vessel code",
  "cruise" = "cruise: AFSC/RACE cruise code",
  "haul" = "haul: AFSC/RACE haul number",
  "stationid" = "stationid: AFSC/RACE station code",
  "serial_number" = "serial_number: Primary instrument serial number",
  "cast_direction" = "cast_directon: Cast direction",
  "datetime" = "datetime: date and time in Alaska Daylight Time [UTC-9:00]",
  "depth" = "depth: depth [m], down is positive",
  "pressure" = "pressure: pressure, strain gauge [db]",
  "conductivity" = "conductivity: conductivity [S/m]",
  "temperature" = "temperature: temperature [ITS-90, degrees C]",
  "salinity" = "salinity: salinity [PSS-78]",
  "sound_speed" = "sound_speed: Chen-Millero sound speed [m/s]",
  # "oxygen" = "oxygen: dissolved oxygen [ml/l]",
  # "pH" = "ph: pH",
  "flag" = "flag: data quality flag"
)


# Create a netCDF file with cast data and metadata in the working directory.
gapctd::make_oce_ncdf(
  cast_files = cast_files,
  metadata_files = metadata_files,
  output_file = here::here("data", paste0("GAPCTD_", year, "_", region, ".nc")),
  global_attributes = list(title = dataset_name, 
                           dataset_citation = dataset_citation,
                           id = dataset_doi,
                           references = references,
                           cdm_data_type = "Point",
                           cruise = cruise_name,
                           institution = creator_institution,
                           contributor_name = ctd_team,
                           creator_name = creator_name,
                           creator_institution = creator_institution,
                           creator_email = creator_email,
                           publisher = creator_institution,
                           publisher_type = "institution",
                           publisher_url = publisher_url,
                           geospatial_bounds_crs = "EPSG:4326",
                           license = "http://www.usa.gov/publicdomain/label/1.0/",
                           metadata_link = "",
                           instrument = "CTD",
                           Conventions = c("CF-1.8"),
                           standard_name_vocabulary = "CF Standard Name Table v79",
                           source = processing_info 
  )
)


# Create an rds file with cast data in the working directory
make_oce_table(cast_files = cast_files,
               output_file = here::here("data", paste0("GAPCTD_", year, "_", region, ".rds")),
               precision = c(temperature = 4,
                             conductivity = 6,
                             salinity = 4,
                             absolute_salinity = 4,
                             sound_speed = 3,
                             density = 3,
                             pressure = 3,
                             oxygen = 4,
                             ph = 3,
                             velocity = 3))

make_text_table(x = readRDS(here::here("data", 
                                       paste0("GAPCTD_", 
                                              year, "_", 
                                              region, 
                                              ".rds")))[text_output_columns] |>
                  dplyr::rename(datetime = timeS), 
                output_file = here::here("data", paste0("GAPCTD_", year, "_", region)),
                column_descriptions = column_descriptions, 
                ctd_unit = ctd_unit, 
                auxiliary_sensors = auxiliary_sensors,
                dataset_name = dataset_name, 
                cruise_name = cruise_name, 
                creator_name = creator_name, 
                creator_email = creator_email,
                creator_institution = creator_institution,
                collaborators = ctd_team, 
                references = references, 
                dataset_doi = dataset_doi, 
                ncei_accession_number = ncei_accession_number, 
                processing_info = processing_info, 
                publisher_url = publisher_url)

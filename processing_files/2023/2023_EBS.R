# Generate data products using make_oce_ncdf()

library(gapctd)

# Replace this with paths to cast rds files that should be included in the data product.
cast_files <- c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_AKK_8106_leg1/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_AKK_8106_leg2/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_AKK_8106_leg34/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_NWX_8101/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_NWX_8100_leg13/final_cnv/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_NWX_8100_leg4/final_cnv/", full.names = TRUE)
)

# Replace this with paths to metadata rds files that should be included in the data product.
metadata_files <- c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_AKK_8106_leg1/metadata/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_AKK_8106_leg2/metadata/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_AKK_8106_leg34/metadata/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_NWX_8101/metadata/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_NWX_8100_leg13/metadata/", full.names = TRUE),
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2023_NWX_8100_leg4/metadata/", full.names = TRUE)
)


# Update this section with the relevant metadata
year <- 2023
region <- "EBS" # Only use EBS if the region is EBS and NBS.
data_set_name <- "CTD Data from AFSC 2023 EBS Shelf and NBS Bottom Trawl Surveys"
cruise_name <- "2023 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Surveys"
ctd_team <- "Nicole Charriere, Cecilia O'Leary, Nate Raring, Bethany Riggle" # Do not list yourself!
creator_name <- "Sean Rohan" # your name
creator_email <- "sean.rohan@noaa.gov" # your email
dataset_doi <- ""
ctd_unit <- "SBE19plus V2"


# Create a netCDF file with cast data and metadata in the working directory.
gapctd::make_oce_ncdf(
  cast_files = cast_files,
  metadata_files = metadata_files,
  output_file = here::here("data", paste0("GAPCTD_", year, "_", region, ".nc")),
  global_attributes = list(title = data_set_name, 
                           references = "Rohan, S. K., Charriere, N. E., Riggle, B., O’Leary, C. A., and Raring, N. W. 2023. A flexible approach for processing data collected using trawl-mounted CTDs during Alaska bottom-trawl surveys. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-AFSC-475, 43 p. https://doi.org/10.25923/8ape-q461",
                           id = dataset_doi,
                           cdm_data_type = "Point",
                           cruise = cruise_name,
                           institution = "NOAA Alaska Fisheries Science Center",
                           contributor_name = ctd_team,
                           creator_name = creator_name,
                           creator_institution = "NOAA Alaska Fisheries Science Center",
                           creator_email = creator_email,
                           publisher = "NOAA Alaska Fisheries Science Center",
                           publisher_type = "institution",
                           publisher_url = "https://github.com/afsc-gap-products/gapctd",
                           geospatial_bounds_crs = "EPSG:4326",
                           license = "http://www.usa.gov/publicdomain/label/1.0/",
                           metadata_link = "",
                           instrument = "CTD",
                           Conventions = c("CF-1.8"),
                           standard_name_vocabulary = "CF Standard Name Table v79",
                           source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd"))
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
                             oxygen = 5,
                             ph = 4,
                             velocity = 3))

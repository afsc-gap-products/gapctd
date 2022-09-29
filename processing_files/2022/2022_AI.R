library(gapctd)

year <- 2022
region <- "AI"

gapctd::make_oce_ncdf(fpath = c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2022_OEX_8103/final_cnv", full.names = TRUE),
  
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2022_AP_8102/final_cnv", full.names = TRUE),
  
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2022_AP_8106/final_cnv", full.names = TRUE)
),
metadata_path = c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2022_OEX_8103/metadata", full.names = TRUE),
  
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2022_AP_8102/metadata", full.names = TRUE),
  
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2022_AP_8106/metadata", full.names = TRUE)
), 
output_file = paste0("GAPCTD_", year, "_", region, ".nc"),
global_attributes = list(
  title = "CTD Data from AFSC 2022 Aleutian Islands Bottom Trawl Survey",
  references = "https://github.com/afsc-gap-products/gapctd",
  id = "doi",
  cdm_data_type = "Point",
  cruise = "2022 Aleutian Islands Bottom-Trawl Survey",
  institution = "NOAA Alaska Fisheries Science Center",
  contributor_name = "Bethany Riggle, Nicole Charriere, Cecilia O'Leary, Nate Raring",
  creator_name = "Sean Rohan",
  creator_institution = "NOAA Alaska Fisheries Science Center",
  creator_email = "sean.rohan@noaa.gov",
  publisher = "NOAA Alaska Fisheries Science Center",
  publisher_type = "institution",
  publisher_url = "https://github.com/afsc-gap-products/gapctd",
  geospatial_bounds_crs = "EPSG:4326",
  license = "http://www.usa.gov/publicdomain/label/1.0/",
  metadata_link = "",
  instrument = "CTD",
  Conventions = "CF-1.8",
  standard_name_vocabulary = "CF Standard Name Table v79",
  source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd"))
)
)
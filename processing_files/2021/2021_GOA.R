library(gapctd)

year <- 2021
region <- "GOA"

gapctd::make_oce_ncdf(fpath = c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_OEX/final_cnv", full.names = TRUE),
                                
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AP/final_cnv", full.names = TRUE),
                                
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AP_2/final_cnv", full.names = TRUE)
),
metadata_path = c(
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_OEX/metadata", full.names = TRUE),
                  
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AP/metadata", full.names = TRUE),
                  
  list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AP_2/metadata", full.names = TRUE)
), 
output_file = paste0("GAPCTD_", year, "_", region, ".nc"),
global_attributes = list(
  title = "CTD Data from AFSC 2021 Gulf of Alaska Bottom Trawl Survey",
                         references = "https://github.com/afsc-gap-products/gapctd",
                         id = "doi",
                         cdm_data_type = "Point",
                         cruise = "2021 Gulf of Alaska Continental Shelf and Upper Continental Slope Bottom-Trawl Survey",
                         institution = "NOAA Alaska Fisheries Science Center",
                         contributor_name = "Nicole Charriere, Cecilia O'Leary, Nate Raring, Bethany Riggle, Wayne Palsson",
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

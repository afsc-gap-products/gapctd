library(gapctd)

year <- 2021
region <- "EBS"

gapctd::make_oce_ncdf(
  fpath = c(
    list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AKK/final_cnv/", full.names = TRUE),
    list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST/final_cnv/", full.names = TRUE),
    list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST_1/final_cnv/", full.names = TRUE)
  ),
  metadata_path = c(
    list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_AKK/metadata", full.names = TRUE),
    
    list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST/metadata", full.names = TRUE),
    
    list.files(path = "C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST_1/metadata", full.names = TRUE)
  ),
  output_file = paste0("GAPCTD_", year, "_", region, ".nc"),
  global_attributes = list(title = "CTD Data from AFSC 2021 EBS Shelf and NBS Bottom Trawl Surveys", 
                           references = "CTD TEAM...",
                           id = "doi",
                           cdm_data_type = "Point",
                           cruise = "2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Surveys",
                           institution = "NOAA Alaska Fisheries Science Center",
                           contributor_name = "Nicole Charriere, Cecilia O'Leary, Nate Raring, Bethany Riggle",
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
                           Conventions = c("CF-1.8"),
                           standard_name_vocabulary = "CF Standard Name Table v79",
                           source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd"))
  )
)
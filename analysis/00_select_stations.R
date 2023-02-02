# Get map layers
library(akgfmaps)
library(ggthemes)
library(gapctd)
library(RNetCDF)

# Set up regional map layers
project_crs <- "EPSG:3338"

# Stations
# G-07 - Closest to M2
# JI1918 - Closest to M4
# F-06 Resample station
# CC-06 Freshwater lens in Norton Sound

# GOA deep station (420-80)
# GOA early station (163-83)
# Yakutat Bay freshwater lens (424-75)

# Retrieve sample locations from netCDF file
samples_ebs <- RNetCDF::open.nc(con = here::here("data", "EBS", "GAPCTD_2021_BS.nc"))

samples_goa <- RNetCDF::open.nc(con = here::here("data", "GOA", "GAPCTD_2021_GOA.nc")) 

# Find closest to M2
sample_locations_df <- dplyr::bind_rows(
  data.frame(latitude = RNetCDF::var.get.nc(ncfile = samples_ebs, 
                                            variable = "latitude"),
             longitude = RNetCDF::var.get.nc(ncfile = samples_ebs, 
                                             variable = "longitude"),
             vessel = RNetCDF::var.get.nc(ncfile = samples_ebs, 
                                          variable = "vessel"),
             dd_lat = RNetCDF::var.get.nc(ncfile = samples_ebs, 
                                          variable = "latitude"),
             dd_lon = RNetCDF::var.get.nc(ncfile = samples_ebs, 
                                          variable = "longitude"),
             haul_depth = RNetCDF::var.get.nc(ncfile = samples_ebs, 
                                                variable = "haul_depth"),
             stationid = RNetCDF::var.get.nc(ncfile = samples_ebs,
                                             variable = "stationid"),
             region = "EBS/NBS",
             time = RNetCDF::var.get.nc(ncfile = samples_ebs,
                                        variable = "time")),
  data.frame(latitude = RNetCDF::var.get.nc(ncfile = samples_goa, 
                                            variable = "latitude"),
             longitude = RNetCDF::var.get.nc(ncfile = samples_goa, 
                                             variable = "longitude"),
             vessel = RNetCDF::var.get.nc(ncfile = samples_goa, 
                                          variable = "vessel"),
             dd_lat = RNetCDF::var.get.nc(ncfile = samples_goa, 
                                          variable = "latitude"),
             dd_lon = RNetCDF::var.get.nc(ncfile = samples_goa, 
                                          variable = "longitude"),
             haul_depth = RNetCDF::var.get.nc(ncfile = samples_goa, 
                                              variable = "haul_depth"),
             stationid = RNetCDF::var.get.nc(ncfile = samples_goa,
                                             variable = "stationid"),
             region = "GOA",
             time = RNetCDF::var.get.nc(ncfile = samples_goa,
                                        variable = "time")))

sample_locations_df <- sf::st_as_sf(x = sample_locations_df, 
                                    coords = c("longitude", "latitude"), 
                                    crs = "+proj=longlat") |>
  sf::st_transform(crs = project_crs)

# Eastern Bering Sea stations ----

m2_df <- data.frame(lon = -164.1,
                    lat = 56.9) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat") |>
  sf::st_transform(crs = project_crs)

stn_m2_closest <- sample_locations_df[which.min(sf::st_distance(m2_df, sample_locations_df)), ]$stationid
ex_m2_closest <- sample_locations_df[sample_locations_df$stationid == stn_m2_closest, ]

m4_df <- data.frame(lat = 57.9,
                    lon = -168.9) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat") |>
  sf::st_transform(crs = project_crs)

stn_m4_closest <- sample_locations_df[which.min(sf::st_distance(m4_df, sample_locations_df)), ]$stationid
ex_m4_closest <- sample_locations_df[sample_locations_df$stationid == stn_m4_closest, ]

# Retow station for the comparison
retow_2021 <- c("H-02", "H-03", "F-05", "F-06", "E-06", "E-05", "C-03", "C-02", "B-02", "B-03")

ex_retow <- sample_locations_df[sample_locations_df$stationid == "F-06", ][3:4, ]

# Norton Sound station ----
ex_norton_sound <- sample_locations_df[sample_locations_df$stationid == "CC-06", ]

# GOA deep station
stn_goa_deep <- sample_locations_df[which.max(sample_locations_df$haul_depth), ]$stationid
ex_goa_deep <- sample_locations_df[sample_locations_df$stationid == stn_goa_deep, ]

# GOA early station
ex_goa_early <- sample_locations_df[sample_locations_df$stationid == "163-83", ]

# Yakutat Bay (fjord with a freshwater lens)
ex_goa_late <- dplyr::filter(sample_locations_df, stationid == "424-75")

dplyr::filter(sample_locations_df, region == "GOA", stringr::str_detect(stationid, "368-168"))

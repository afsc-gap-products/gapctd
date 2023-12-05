# Load GAP CTD data products
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 6, 2023
# https://github.com/afsc-gap-products/gapctd

# This demonstration shows how to load GAP's CTD data products from plain text, netCDF (.nc), and 
# R data (.rds) formats. The plain text and NetCDF files will be available through NCEI. Raw .hex, 
# .cnv, and config (.xmlcon) files will also be available through NCEI.

# Load gapctd
library(gapctd)

# 1. CSV data - Only upcast/downcast ---------------------------------------------------------------

# The top 30 lines contain metadata
csv_header <- readLines(here::here("data", "GAPCTD_2021_EBS"), n = 30)

# The rest contains data
dat_csv <- read.csv(file = here::here("data", "GAPCTD_2021_EBS"), skip = 30)

# TIMES ARE IN AKDT
csv_header[21]
dat_csv$datetime <- as.POSIXct(dat_csv$datetime, tz = "America/Anchorage")



#3 NetCDF file upcast, downcast, and bottom --------------------------------------------------------
library(ncdf4)

# Open connection
con <- ncdf4::nc_open(here::here("data", "GAPCTD_2021_EBS.nc"))

# View variables, dimensions, attributes, and metadata. Note that bottom and surface
print(con)

# Access data and attribute metadata
ncdf4::ncvar_get(con, "sound_speed_in_sea_water")
ncdf4::ncatt_get(con, "sound_speed_in_sea_water")
ncdf4::ncvar_get(con, "sound_speed_in_sea_water")

# Bottom and profile data are in separate columns
ncdf4::ncvar_get(con, "sea_floor_sound_speed_in_sea_water")
ncdf4::ncatt_get(con, "sea_floor_sound_speed_in_sea_water")

# Dimensions
ncdf4::ncvar_get(con, "latitude")
ncdf4::ncatt_get(con, "latitude")
ncdf4::ncvar_get(con, "longitude")
ncdf4::ncatt_get(con, "latitude")
ncdf4::ncvar_get(con, "depth")
ncdf4::ncatt_get(con, "depth")
ncdf4::ncvar_get(con, "time")
ncdf4::ncatt_get(con, "time") # TIMES ARE IN UTC


# 3. R data file -----------------------------------------------------------------------------------
# Available upon request, but will not be on NCEI

dat_rds <- readRDS(file = here::here("data", "GAPCTD_2021_EBS.rds"))
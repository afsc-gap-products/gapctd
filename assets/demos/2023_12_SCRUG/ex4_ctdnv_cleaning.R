# Post-hoc filtering TDRI Citadel CTD-NV data using gapctd
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 6, 2023
# https://github.com/afsc-gap-products/gapctd

# Metadata -----------------------------------------------------------------------------------------
# Survey: 2017 Eastern Bering Sea Shelf Bottom Trawl Survey (cruise code 201701)
# Vessel: Vesteraalen (vessel code 94)
# Haul code: 180
# Haul start coordinates: 59.99046, -171.29661 
# Haul end coordinates: 60.0152, -171.27949
# Start time: 2017-07-23 07:42:50 AKDT
# Station ID: P-22
# Instrument: Teledyne RD Instruments Citadel CTD-NV
# Deployment file: 2283_20150603_0657_downcast.nc, 2283_20150603_0657_upcast.nc
# Comment: Processed by Ned Cokelet following Cokelet (2016). This CTD model had a much faster 
# sampling rate (15 Hz vs. 4 Hz) than the SBE19plus and faster sensor response times. However, the 
# CTD was not ducted (pumped) so changes in trawl ascent/descent rates may have caused more 
# pronounced dynamic errors. 
# Reference: Cokelet, E. D. (2016). 3-D water properties and geostrophic circulation on the eastern 
# Bering Sea shelf. Deep Sea Research Part II: Topical Studies in Oceanography, 134, 65–85. 
# https://doi.org/10.1016/j.dsr2.2016.08.009

install.packages("ncdf4")

library(gapctd)
library(ncdf4)

start_longitude <- 59.51123
start_latitude <- -172.904 
end_longitude <- 59.49369
end_latitude <- -172.8691
survey <- "2017 Eastern Bering Sea Continental Shelf Bottom Trawl Survey"
stationid <- "P-22"
ship <- "Vesteraalen"

# Load processed CTD data --------------------------------------------------------------------------
con <- ncdf4::nc_open(filename = here::here("assets", "demos", "2023-12 SCRUG","data", "2283_20170723_0733_downcast.nc"))

downcast <- oce::as.ctd(salinity = ncdf4::ncvar_get(nc = con, varid = "S_41"),
            temperature = ncdf4::ncvar_get(nc = con, varid = "T_28"),
            pressure = ncdf4::ncvar_get(nc = con, varid = "DEP"))

downcast@metadata$survey <- survey
downcast@metadata$ship <- ship
downcast@metadata$stationid <- stationid
downcast@metadata$longitude <- 59.99046
downcast@metadata$latitude <-  -171.29661

con <- ncdf4::nc_open(filename = here::here("assets", "demos", "2023-12 SCRUG","data", "2283_20170723_0733_upcast.nc"))

upcast <- oce::as.ctd(salinity = ncdf4::ncvar_get(nc = con, varid = "S_41"),
                        temperature = ncdf4::ncvar_get(nc = con, varid = "T_28"),
                        pressure = ncdf4::ncvar_get(nc = con, varid = "DEP"))

ncdf4::ncvar_get(nc = con, varid = "LON")

upcast@metadata$survey <- survey
upcast@metadata$ship <- ship
upcast@metadata$stationid <- stationid
upcast@metadata$longitude <- 60.0152
upcast@metadata$latitude <- -171.27949

ncdf4::nc_close(con)

# Temperature looks alright but salinity spikes due to dynamic errors result in erroneous density inversions.
plot(downcast, main = "Downcast")
plot(upcast, main = "Upcast")


# Exploring effect of different median filter window sizes on density structure --------------------
par(mfrow = c(2,2))
plot(downcast, 
     which = 2, 
     type = 'l',
     main = "Downcast, Unfiltered")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(downcast, variables = "salinity", 
                           window = 3), 
     which = 2, 
     type = 'l',
     main = "Downcast, Window = 3")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(downcast, variables = "salinity", 
                           window = 5), 
     which = 2, 
     type = 'l',
     main = "Downcast, Window = 5")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(downcast, variables = "salinity", 
                           window = 7), 
     which = 2, 
     type = 'l',
     main = "Downcast, Window = 7")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(downcast, variables = "salinity", 
                           window = 9), 
     which = 2, 
     type = 'l',
     main = "Downcast, Window = 9")
abline(v = -1e-4, col = "brown", lty = 2)

plot(gapctd::median_filter(downcast, variables = "salinity", 
                           window = 11), 
     which = 2, 
     type = 'l',
     main = "Downcast, Window = 11")


par(mfrow = c(2,3))
plot(upcast, 
     which = 2, 
     type = 'l',
     main = "Upcast, Unfiltered")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(upcast, variables = "salinity", 
                           window = 3), 
     which = 2, 
     type = 'l',
     main = "Upcast, Window = 3")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(upcast, variables = "salinity", 
                           window = 5), 
     which = 2, 
     type = 'l',
     main = "Upcast, Window = 5")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(upcast, variables = "salinity", 
                           window = 7), 
     which = 2, 
     type = 'l',
     main = "Upcast, Window = 7")
abline(v = -1e-4, col = "brown", lty = 2)
plot(gapctd::median_filter(upcast, variables = "salinity", 
                           window = 9), 
     which = 2, 
     type = 'l',
     main = "Upcast, Window = 9")
plot(gapctd::median_filter(upcast, variables = "salinity", 
                           window = 11), 
     which = 2, 
     type = 'l',
     main = "Upcast, Window = 11")
abline(v = -1e-4, col = "brown", lty = 2)

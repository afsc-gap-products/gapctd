# Cleaning SBE39 temperature-depth recorder data using gapctd
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 6, 2023
# https://github.com/afsc-gap-products/gapctd

# In this example, SBE39 temperature-depth recorder data from race_data are loaded and filtered
# as a 'ctd' object (oce package). An alternative approach could be to create a new TDR subclass  
# with its own methods (https://cran.r-project.org/web/packages/oce/vignettes/F_subclassing.html).


# Metadata -----------------------------------------------------------------------------------------
# Survey: 2021 Eastern Bering Sea Shelf Bottom Trawl Survey (cruise code 202101)
# Vessel: Alaska Knight (vessel code 162)
# Haul code: 150
# Haul start coordinates: 59.51123, -172.904 
# Haul end coordinates: 59.49369, -172.8691
# Downcast start/end (AKDT): 2021-07-08 06:51:53 / 2021-07-08 07:01:51 (from race_data)
# Upcast start/end (AKDT): 2021-07-08 07:29:47 / 2021-07-08 07:37:34 (from race_data)
# Station ID: ON2524
# Instrument: SBE39 temperature-depth recorder
# Instrument serial number: 855
# Comment: The SBE39 temperature-depth recorder data for this example are from the same haul as CTD
# data from ex2_sbe19plus_processing.R


library(gapctd)

# 1. Load SBE39 data that was retrieved from RACEBASE and convert to DATE_TIME to AKDT -------------

downcast_start <- as.POSIXct(x = "2021-07-08 06:51:53", tz = "America/Anchorage")
downcast_end <- as.POSIXct(x = "2021-07-08 07:01:51", tz = "America/Anchorage")
upcast_start <- as.POSIXct(x = "2021-07-08 07:29:47", tz = "America/Anchorage")
upcast_end <- as.POSIXct(x = "2021-07-08 07:37:34", tz = "America/Anchorage")
start_longitude <- 59.51123
start_latitude <- -172.904 
end_longitude <- 59.49369
end_latitude <- -172.8691

tdr_data <- read.csv(file = here::here("assets", "demos", "2023-12 SCRUG","data", "202101_162_150_BT.csv")) |>
  dplyr::mutate(DATE_TIME = as.POSIXct(DATE_TIME, tz = "UTC")) |>
  dplyr::mutate(DATE_TIME = lubridate::with_tz(DATE_TIME, tz = "America/Anchorage"))

# 2. Use oce to invert depth to calculate pressure -------------------------------------------------
# Pressure and salinity are required to create a 'ctd' object in oce.

# Estimate pressure from depth and latitude
tdr_data$PRESSURE <- oce::swPressure(depth = tdr_data$DEPTH, latitude = 59.5, eos = "unesco")
tdr_data$timeS <- as.numeric(difftime(tdr_data$DATE_TIME, downcast_start, units = "s"))

# Make ctd object. Using a typical value for the region but could be filled with a station average, 
# etc.
cast_data <- as.ctd(temperature = tdr_data$TEMPERATURE,
                   pressure = tdr_data$PRESSURE,
                   salinity = rep(32, nrow(tdr_data)))

# Back-calculating conductivity from temperature, pressure, and salinity
cast_data@data$conductivity <- oce::swCSTp(cast_data, eos = "unesco")

# Add time elapsed
cast_data@data$timeS <- tdr_data$timeS

# Add a data quality flag column (0 = good data)
cast_data@data$flag <- 0

# 3. Fill metadata ---------------------------------------------------------------------------------
cast_data@metadata$startTime <- downcast_start
cast_data@metadata$ship <- "Alaska Knight"
cast_data@metadata$cruise <- "2021 Eastern Bering Sea Shelf Bottom Trawl Survey"
cast_data@metadata$institute <- "NOAA Alaska Fisheries Science Center"
cast_data@metadata$address <- "7600 Sand Point Way NE Bldg. 4, Seattle, Washington, 98115"
cast_data@metadata$stationid <- "ON2524"

downcast <- cast_data |>
  gapctd::section_oce(start = downcast_start,
                      end = downcast_end,
                      by = "datetime",
                      cast_direction = "downcast")

upcast <- cast_data |>
  gapctd::section_oce(start = upcast_start,
                      end = upcast_end,
                      by = "datetime",
                      cast_direction = "upcast")

bottom <- cast_data |>
  gapctd::section_oce(start = downcast_end,
                      end = upcast_start,
                      by = "datetime",
                      cast_direction = "bottom") 

# The downcast profile has a bunch of surface data while the SBE39 was equilibrating; the upcast 
# profile has some issues around 20 m, similar to the CTD. Maybe due to a flow reversal when the trawl
# slowed down at ~20 m.
plot(downcast, main = "Downcast")
plot(upcast, main = "Upcast")
plot(bottom, main = "Bottom")

# 4. Remove flow reversals and near-surface equilibrium time, check for data issues ----------------
# Note that slowdown only flags points for removal and doesn't remove the points -- bin_average 
# removes points.
downcast_binned <- downcast|>
  gapctd::slowdown(min_speed = 0.1, 
                   window = 5, 
                   cast_direction = "downcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", 
                      bin_width = 1) |>
  gapctd:::check_density_inversion(threshold  = -1e-5, 
                                   threshold_method = "bv", 
                                   correct_inversion = TRUE) |>
  gapctd:::qc_check(prop_max_flag = 0.1,
                    prop_min_bin = 0.9)

upcast_binned <- upcast |>
  gapctd::slowdown(min_speed = 0.1, 
                   window = 5, 
                   cast_direction = "upcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", 
                      bin_width = 1) |>
  gapctd:::check_density_inversion(threshold  = -1e-5, 
                                   threshold_method = "bv", 
                                   correct_inversion = TRUE) |>
  gapctd:::qc_check(prop_max_flag = 0.1,
                    prop_min_bin = 0.9)

# The temperature profile looks a lot better; surface values in the downcast were excluded because the 
# descent speed was too slow.
plot(downcast_binned, main = "Downcast")
plot(upcast_binned, main = "Upcast")

# 5. Temperature and depth profiles from this cast are now analysis-ready.


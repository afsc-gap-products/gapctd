# Processing SBE19plus data using gapctd
# Sean Rohan <sean.rohan@noaa.gov>
# Survey-Centric R Users Group
# December 6, 2023
# https://github.com/afsc-gap-products/gapctd

# This demonstration shows how functions in the gapctd package are used to process data from trawl-
# mounted CTDs collected during bottom trawl surveys. 

# Metadata -----------------------------------------------------------------------------------------
# Survey: 2021 Eastern Bering Sea Shelf Bottom Trawl Survey (cruise code 202101)
# Vessel: Alaska Knight (vessel code 162)
# Haul code: 150
# Haul start coordinates: 59.51123, -172.904 
# Haul end coordinates: 59.49369, -172.8691
# Downcast start/end (AKDT): 2021-07-08 06:51:53 / 2021-07-08 07:01:51 (from race_data)
# Upcast start/end (AKDT): 2021-07-08 07:29:47 / 2021-07-08 07:37:34 (from race_data)
# Station ID: ON2524
# Instrument: SBE 19plus V2 SeaCAT CTD
# Instrument serial number: 19-8102
# Deployment file: 2021_07_08_0001.hex
# Configuration file: 19-8102_Deploy2021.xmlcon
# Comment: Metadata are provided here for illustrative purposes. They are retrieved automatically 
# for 'actual' survey data processing using gapctd::run_gapctd().

# Load gapctd
library(gapctd)

# 1. Convert CTD data to SI units and write output to a cnv file -----------------------------------
config_path <- here::here("data", "19-8102_Deploy2021.xmlcon")
raw_cnv_path <- here::here("data", "2021_07_08_0001_raw.cnv")
hex_path <- here::here("data", "2021_07_08_0001.hex")
downcast_start <- as.POSIXct(x = "2021-07-08 06:51:53", tz = "America/Anchorage")
downcast_end <- as.POSIXct(x = "2021-07-08 07:01:51", tz = "America/Anchorage")
upcast_start <- as.POSIXct(x = "2021-07-08 07:29:47", tz = "America/Anchorage")
upcast_end <- as.POSIXct(x = "2021-07-08 07:37:34", tz = "America/Anchorage")
start_longitude <- 59.51123
start_latitude <- -172.904 
end_longitude <- 59.49369
end_latitude <- -172.8691

gapctd::hex_to_cnv(hex_path = hex_path,
                   output_path = raw_cnv_path,
                   xmlcon_path = config_path)


# 2. Load the raw cnv file, assign metadata, and inspect the plot ----------------------------------
#    The oce package automatically calculates salinity from temperature and conductivity, but it is not 
cast_data <- read.ctd(file = raw_cnv_path)

head(as.data.frame(cast_data@data))
cast_data@metadata
cast_data@processingLog


# 3. Add metadata ----------------------------------------------------------------------------------

# oce assumes instrument time is UTC but the instrument is set to 'survey time' (AKDT)
cast_data@metadata$startTime <- lubridate::force_tz(cast_data@metadata$startTime, "America/Anchorage")
cast_data@metadata$ship <- "Alaska Knight"
cast_data@metadata$cruise <- "2021 Eastern Bering Sea Shelf Bottom Trawl Survey"
cast_data@metadata$institute <- "NOAA Alaska Fisheries Science Center"
cast_data@metadata$address <- "7600 Sand Point Way NE Bldg. 4, Seattle, Washington, 98115"
cast_data@metadata$stationid <- "ON2524"

# Problems with the raw data:
# - Salinity is erroneously low near the surface.
# - There are big salinity spikes around the pycnocline
# - Multiple density inversions (i.e., density increases with depth)
plot(cast_data)


# 4. Split upcast, downcast, and bottom data -------------------------------------------------------
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

# 5. Add cast location metadata  ------------------------------------------------------------------- 

downcast@metadata$longitude <- start_longitude
downcast@metadata$latitude <- start_latitude

upcast@metadata$longitude <- end_longitude
upcast@metadata$latitude <- end_latitude


plot(downcast, main = "Downcast")
plot(upcast, main = "Upcast")
plot(bottom, main = "Bottom")

par(mfrow = c(3,1))
plot(x = bottom@data$timeS, 
     y = bottom@data$temperature, 
     main = "Bottom", 
     xlab = "Time (s)", 
     ylab = expression(Temperature~(degree*C)),
     type = "l",
     col = "red")
plot(x = bottom@data$timeS, 
     y = bottom@data$salinity, 
     main = "Bottom", 
     xlab = "Time (s)", 
     ylab = "Salinity (PSS-78)",
     type = "l",
     col = "darkgreen")
plot(x = bottom@data$timeS, 
     y = bottom@data$pressure, main = "Bottom", 
     xlab = "Time (s)", 
     ylab = "Pressure (dbar)",
     type = "l",
     col = "black")

# 6. Process downcast and upcast using manufacturer's 'typical' settings ---------------------------
# Steps:
#   - Median filter temperature and conductivity with a window of 5 scans.
#   - Low pass filter temperature, conductivity and pressure with time constants of 0.5, 0.5, and 0.1 seconds, respectively.
#   - Align temperature by shifting temperature channel data by 0.5 seconds.
#   - Discrete-time filter conductivity for thermal inertia correction with initial conductivity error, alpha = 0.04 and time decay constant beta = 0.125 (tau = 8; beta = 1/tau).
#   - Exclude scans where the CTD vertical speed was < 0.1 m/s within five-scan windows to minimize errors caused by turbulent mixing and flow reversals.
#   - Derive salinity, depth, density, sound speed, and buoyancy frequency based on UNESCO 1983 EOS.
#   - Calculate averages by 1-m depth bins.
#   - Remove and interpolate density inversions in the upper 20 m of the water column based on the squared Buoyancy Frequency.
#   - Flag casts if more than 10% of data were estimated based on interpolation, if less than 90% of possible depth bins are missing data, and if there is a discrepancy between on-bottom/EQ time in race_data and cast start time from the instrument.

downcast_binned <- downcast |>
  gapctd:::median_filter(variables = c("temperature", "conductivity"),
                         window = c(5,5)) |>
  gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                          time_constant = c(0.5, 0.5, 1),
                          precision = c(4, 6, 3),
                          freq_n = 0.25) |>
  gapctd::align_var(variables = "temperature", 
                    offset = -0.5, 
                    interp_method = "linear") |>
  gapctd::conductivity_correction(alpha_C = 0.04,
                                  beta_C = 0.125) |>
  gapctd::slowdown(min_speed = 0.1, 
                   window = 5, 
                   cast_direction = "downcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", bin_width = 1) |>
  gapctd:::check_density_inversion(threshold  = -1e-5, 
                                   threshold_method = "bv", 
                                   correct_inversion = TRUE) |>
  gapctd:::qc_check(prop_max_flag = 0.1,
                    prop_min_bin = 0.9)

upcast_binned <- upcast |>
  gapctd:::median_filter(variables = c("temperature", "conductivity"),
                         window = c(5,5)) |>
  gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                          time_constant = c(0.5, 0.5, 1),
                          precision = c(4, 6, 3),
                          freq_n = 0.25) |>
  gapctd::align_var(variables = "temperature", 
                    offset = -0.5, 
                    interp_method = "linear") |>
  gapctd::conductivity_correction(alpha_C = 0.04,
                                  beta_C = 0.125) |>
  gapctd::slowdown(min_speed = 0.1, 
                   window = 5, 
                   cast_direction = "upcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", bin_width = 1) |>
  gapctd:::check_density_inversion(threshold  = -1e-5, 
                                   threshold_method = "bv", 
                                   correct_inversion = TRUE) |>
  gapctd:::qc_check(prop_max_flag = 0.1,
                    prop_min_bin = 0.9)

# 7. Filter bottom data ----------------------------------------------------------------------------
# Try changing filter settings

bottom_filtered <- bottom |>
  gapctd:::median_filter(variables = c("temperature", "conductivity"),
                         window = c(5,5)) |>
  gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                          time_constant = c(0.5, 0.5, 1),
                          precision = c(4, 6, 3),
                          freq_n = 0.25) |>
  gapctd::align_var(variables = "temperature", 
                    offset = -0.5) |>
  gapctd:::derive_eos()

par(mfrow = c(3,1))
plot(x = bottom_filtered@data$timeS, 
     y = bottom_filtered@data$temperature, 
     xlab = "Time (s)", 
     ylab = expression(Temperature~(degree*C)),
     type = "l",
     col = "red")
plot(x = bottom_filtered@data$timeS, 
     y = bottom_filtered@data$salinity, 
     main = "Bottom", 
     xlab = "Time (s)", 
     ylab = "Salinity (PSS-78)",
     type = "l",
     col = "darkgreen")
plot(x = bottom_filtered@data$timeS, 
     y = bottom_filtered@data$pressure, 
     xlab = "Time (s)", 
     ylab = "Pressure (dbar)",
     type = "l",
     col = "black")

# 8. Inspecting plots from 'typical' method --------------------------------------------------------
# Temperature is better than at the started but there are still erroneous salinity spikes at sharp temperature gradients.
# In upcasts, this may be exacerbated by variable ascent speeds, including the trawl slowing down during the ascent at around 20 m.
plot(downcast_binned, main = "Downcast")
plot(upcast_binned, main = "Upcast")

par(mfrow = c(2,1))
plot(upcast_binned@data$timeS,
     abs(upcast_binned@data$velocity),
     type = 'l',
     xlab = "Time (s)",
     ylab = "Ascent speed (m/s)")
plot(upcast_binned@data$timeS,
     -1*upcast_binned@data$depth,
     type = 'l',
     xlab = "Time (s)",
     ylab = "Depth (m)")

# 9. Estimate temperature alignment ----------------------------------------------------------------
# Estimate optimal alignment parameters by finding the value that maximizes the correlation between rates of change in temperature and conductivity over time.

downcast_align_par <- gapctd::optim_align_par(downcast, 
                                              variable = "temperature",
                                              offsets = seq(-1,1,0.01),
                                              cor_method = "pearson")
upcast_align_par <- gapctd::optim_align_par(upcast, 
                                            variable = "temperature",
                                            offsets = seq(-1,1,0.01), 
                                            cor_method = "pearson")

downcast_aligned <- downcast |>
  gapctd:::median_filter(variables = c("temperature", "conductivity"),
                         window = c(5,5)) |>
  gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                          time_constant = c(0.5, 0.5, 1),
                          precision = c(4, 6, 3),
                          freq_n = 0.25) |>
  gapctd::align_var(variables = "temperature", 
                    offset = downcast_align_par[['temperature']][['offset']],
                    interp_method = "linear")

upcast_aligned <- upcast |>
  gapctd:::median_filter(variables = c("temperature", "conductivity"),
                         window = c(5,5)) |>
  gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                          time_constant = c(0.5, 0.5, 1),
                          precision = c(4, 6, 3),
                          freq_n = 0.25) |>
  gapctd::align_var(variables = "temperature", 
                    offset = upcast_align_par[['temperature']][['offset']],
                    interp_method = "linear")

# 10. Estimate conductivity correction -------------------------------------------------------------
# Estimate optimal discrete-time filter parameters for thermal inertia correction.

# Temperature-Salinity Area (TSA) Method: Optimize by minimizing the area between temperature-salinity 
# curves. There's one set of parameter values for both casts. Requires a grid search to identify starting 
# values. Sometimes doesn't converge because of complex surfaces and can take awhile.

gapctd:::optim_ctm_pars(dc = downcast_aligned,
                        uc = upcast_aligned,
                        optim_method = "L-BFGS-B",
                        start_alpha_C = c(0.001, 0.01, 0.02, 0.04, 0.08, 0.12),
                        start_beta_C = c(1, 1/2, 1/4, 1/8, 1/12, 1/24),
                        default_parameters = c(alpha_C = 0.04, beta_C = 0.125),
                        area_method = "ts")

# Minimum Salinity Gradient (MSG) Method: Optimize by minimizing the gradient in single salinity curves 
# Separate parameters for downcasts and upcasts.

downcast_ctm_pars <- gapctd:::optim_ctm_pars(dc = downcast_aligned, 
                        optim_method = "L-BFGS-B",
                        start_alpha_C = c(0.001, 0.01, 0.02, 0.04, 0.08, 0.12),
                        start_beta_C = c(1, 1/2, 1/4, 1/8, 1/12, 1/24),
                        default_parameters = c(alpha_C = 0.04, beta_C = 0.125))

upcast_ctm_pars <- gapctd:::optim_ctm_pars(uc = upcast_aligned, 
                        optim_method = "L-BFGS-B",
                        start_alpha_C = c(0.001, 0.01, 0.02, 0.04, 0.08, 0.12),
                        start_beta_C = c(1, 1/2, 1/4, 1/8, 1/12, 1/24),
                        default_parameters = c(alpha_C = 0.04, beta_C = 0.125))

# Minimum salinity gradient parameters applied to both casts

downcast_optim_binned <- downcast_aligned |>
  gapctd::conductivity_correction(alpha_C = downcast_ctm_pars$both[['alpha_C']],
                                  beta_C = downcast_ctm_pars$both[['beta_C']]) |>
  gapctd::slowdown(min_speed = 0.1, 
                   window = 5, 
                   cast_direction = "downcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", bin_width = 1) |>
  gapctd:::check_density_inversion(threshold  = -1e-5, 
                                   threshold_method = "bv", 
                                   correct_inversion = TRUE) |>
  gapctd:::qc_check(prop_max_flag = 0.1,
                    prop_min_bin = 0.9)

upcast_optim_binned <- upcast_aligned |>
  gapctd::conductivity_correction(alpha_C = upcast_ctm_pars$both[['alpha_C']],
                                  beta_C = upcast_ctm_pars$both[['beta_C']]) |>
  gapctd::slowdown(min_speed = 0.1, 
                   window = 5, 
                   cast_direction = "upcast") |>
  gapctd::derive_eos() |>
  gapctd::bin_average(by = "depth", bin_width = 1) |>
  gapctd:::check_density_inversion(threshold  = -1e-5, 
                                   threshold_method = "bv", 
                                   correct_inversion = TRUE) |>
  gapctd:::qc_check(prop_max_flag = 0.1,
                    prop_min_bin = 0.9)


# Comparing typical method profiles to 'optimized' profiles, there are still some salinity spikes, 
# especially in the upcast. In the downcast, it looks like salinity may overshoot at ~47 dbar.
plot(downcast_binned, main = "Downcast")
plot(downcast_optim_binned, main = "Downcast")

plot(upcast_binned, main = "Upcast")
plot(upcast_optim_binned, main = "Upcast")

# Comparing SBE Data Processing window filter results to gapctd window filter results
# Differences are in the last digit appear to mainly be a result of rounding error.
# gapctd version 2.1.0
# SBE Data Processing version 7.26.7
# Sean Rohan <sean.rohan@noaa.gov>
# December 4, 2023

library(gapctd)

dat <- oce::read.oce(file = here::here("SBE19plus_01908106_2023_06_14_0008_raw.cnv"))

# Median filter comparison
dat_median <- dat |>
  gapctd::median_filter(variables = c("temperature", "conductivity", "pH"), window = c(5,7,5))

sbe_median <- oce::read.oce(file = here::here("SBE19plus_01908106_2023_06_14_0008_raw_median.cnv"))

compare_median <- as.data.frame(dat_median@data) - as.data.frame(sbe_median@data)

summary(compare_median)

dat_median@data$temperature - sbe_median@data$temperature
dat_median@data$conductivity - sbe_median@data$conductivity
dat_median@data$pH - sbe_median@data$pH


# Boxcar filter
dat_boxcar <- dat |>
  gapctd::window_filter(variables = c("temperature", "conductivity", "pH"), window = c(5,7,5), method = "boxcar")

sbe_boxcar <- oce::read.oce(file = here::here("SBE19plus_01908106_2023_06_14_0008_raw_boxcar.cnv"))

compare_boxcar <- as.data.frame(dat_boxcar@data) - as.data.frame(sbe_boxcar@data)

summary(compare_boxcar)

dat_boxcar@data$temperature - sbe_boxcar@data$temperature
dat_boxcar@data$conductivity - sbe_boxcar@data$conductivity
dat_boxcar@data$pH - sbe_boxcar@data$pH


# Cosine filter
dat_cosine <- dat |>
  gapctd::window_filter(variables = c("temperature", "conductivity", "pH"), window = c(5,7,5), method = "cosine")

sbe_cosine <- oce::read.oce(file = here::here("SBE19plus_01908106_2023_06_14_0008_raw_cosine.cnv"))

compare_cosine <- as.data.frame(dat_cosine@data) - as.data.frame(sbe_cosine@data)

summary(compare_cosine)

dat_cosine@data$temperature - sbe_cosine@data$temperature
dat_cosine@data$conductivity - sbe_cosine@data$conductivity
dat_cosine@data$pH - sbe_cosine@data$pH


# Triangle filter
dat_triangle <- dat |>
  gapctd::window_filter(variables = c("temperature", "conductivity", "pH"), window = c(5,7,5), method = "triangle")

sbe_triangle <- oce::read.oce(file = here::here("SBE19plus_01908106_2023_06_14_0008_raw_triangle.cnv"))

compare_triangle <- as.data.frame(dat_triangle@data) - as.data.frame(sbe_triangle@data)

summary(compare_triangle)

dat_triangle@data$temperature - sbe_triangle@data$temperature
dat_triangle@data$conductivity - sbe_triangle@data$conductivity
dat_triangle@data$pH - sbe_triangle@data$pH


# Gaussian filter
dat_gaussian <- dat |>
  gapctd::window_filter(variables = c("temperature", "conductivity", "pH"), 
                        window = c(5,7,5), 
                        method = "gaussian", 
                        offset = 0, 
                        halfwidth = 1)

sbe_gaussian <- oce::read.oce(file = here::here("SBE19plus_01908106_2023_06_14_0008_raw_gaussian.cnv"))

compare_gaussian <- as.data.frame(dat_gaussian@data) - as.data.frame(sbe_gaussian@data)

summary(compare_gaussian)

dat_gaussian@data$temperature - sbe_gaussian@data$temperature
dat_gaussian@data$conductivity - sbe_gaussian@data$conductivity
dat_gaussian@data$pH - sbe_gaussian@data$pH

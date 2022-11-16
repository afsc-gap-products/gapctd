# Examples of manually reviewing profiles
# Created by Sean Rohan <sean.rohan@noaa.gov>
# Updated: November 16, 2022

library(gapctd)

# Copy built-in example file and unzip to /output/ex_review_files/
ex_zip_path <- system.file(package = "gapctd", "extdata/example/ex_review_files.zip")
ex_dest_path <- gsub(pattern = system.file(package = "gapctd", "extdata/example/"),
                     replacement = here::here("output"),
                     x = ex_zip_path)
file.copy(from = ex_zip_path, to = ex_dest_path)

# Unzip files
unzip(zipfile = ex_dest_path,
      exdir = here::here("output"))

# Let's examine some of these files
rds_paths <- list.files(here::here("output", "ex_review_files"), full.names = TRUE)

# Load data and examine the contents of the downcast
profile_data <- readRDS(file = rds_paths[3])

str(profile_data$downcast@data)
str(profile_data$downcast@metadata)

# Plot the downcast and upcast data using oce
plot(profile_data$downcast)
plot(profile_data$upcast)

# Select best method -----------------------------------------------------------------------------

# Review profile plots from data processing using four methods; choose the best method.
# Files chosen as the best will be renamed to end with "_best.rds"

gapctd::select_best_method(
  rds_dir_path = here::here("output", "ex_review_files"))


# Flag and interpolate --------------------------------------------------------------------------

# Visually inspect, flag, and interpolate
# Outputs written to /output/gapctd/ in rds files ending with "_qc.rds"

gapctd::wrapper_flag_interpolate(rds_dir_path = here::here("output", "ex_review_files"),
                                 review = c("density", "salinity"))


# Review ----------------------------------------------------------------------------------------

# Review profiles plots and select the casts that pass quality control checks.

gapctd::review_profiles(rds_dir_path = here::here("output", "ex_review_files"),
                        threshold = -1e-5, 
                        in_pattern = "_qc.rds")
# Examples of processing and manually reviewing profiles from one vessel/cruise/CTD
# Created by Sean Rohan <sean.rohan@noaa.gov>
# Updated: November 16, 2022

library(gapctd)

vessel <- 94
cruise <- c(202101, 202102)
region <- "BS"
processing_method <- "gapctd"
ctd_dir <- "G:/RACE_CTD/data/2021/ebs/v94_8091"

# 1. Setup -----------------------------------------------------------------------------------------

# Setup directory and retrieve haul data. Running this more than once will remove all files from the working directory and require reprocessing from scratch.
gapctd:::setup_gapctd_directory(processing_method = processing_method, 
                                ctd_dir = ctd_dir,
                                use_sbedp_to_convert = FALSE)


# 2. Run gapctd ------------------------------------------------------------------------------------

# Wrapper function around automated data processing steps that are used to create deployment rds files containing upcast, downcast, and bottom data.
# Performs the following tasks:
# - Retrieves haul data from racebase and race_data; writes haul data to /output/. 
# - Splits deployment data into downcast, bottom, and upcast; writes file to /data/split/
# - Runs four versions of the data processing (Typical, Typical CTM, T-s Area, and S Path distance) on deployment data. 
# - Saves processed deployment data to /output/[processing_method] directory as lists. Filename suffix and metadata in the file describe the processing method (Example: files ending in '_typical.rds' were processed using the Typical method.
# - Processes deployment files to create metadata file that includes bottom variables for each deployment; saved to file in /metadata/ directory.
# - Checks files for data flags moves data with flagged files to /bad_cnv/

gapctd::wrapper_run_gapctd(cnv_dir_path = here::here("cnv"),
                           processing_method = processing_method,
                           haul_df = NULL,
                           vessel = vessel,
                           cruise = cruise,
                           channel = NULL)


#3. Select best method -----------------------------------------------------------------------------

# Review profile plots from data processing using four methods; choose the best method.
# Files chosen as the best will be renamed to end with "_best.rds"

gapctd::select_best_method(
  rds_dir_path = here::here("output", processing_method))


# 4. Flag and interpolate --------------------------------------------------------------------------

# Visually inspect, flag, and interpolate
# Outputs written to /output/gapctd/ in rds files ending with "_qc.rds"

gapctd::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                 review = c("density", "salinity"))


# 5. Review ----------------------------------------------------------------------------------------

# Review profiles plots and select the casts that pass quality control checks.

gapctd::review_profiles(rds_dir_path = here::here("output", processing_method),
                        threshold = -1e-5, 
                        in_pattern = "_qc.rds")


# 6. Finalize --------------------------------------------------------------------------------------

# Move final profile data to /final_cnv/
gapctd::finalize_data(rds_dir_path = here::here("output", processing_method))
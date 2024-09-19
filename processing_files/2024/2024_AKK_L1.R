# This CTD was deployed with a dissolved oxygen sensors on external voltage channels 0

library(gapctd)

vessel <- 162
region <- "BS"
cruise <- 202401
processing_method <- "gapctd"
ctd_dir <- "G:/RACE_CTD/data/2024/ebs/v162_8102_L1"


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


# 4. Align oxygen ----------------------------------------------------------------------------------

# Test oxygen alignment offsets ranging from 2-7 seconds
# Candidate aligned oxygen data will be saved to /output/align_oxygen/*_ox_align.rds
gapctd:::wrapper_align_oxygen()


# 5. Select best oxygen alignment ------------------------------------------------------------------

# Review profiles of oxygen with different alignments from step 4 and select the best one.
# Data are reprocessed using run_gapctd(). Profiles written to /output/gapctd/*_best_oxygen.rds

gapctd:::select_best_oxygen_method()


# 6. Flag and interpolate --------------------------------------------------------------------------

# Visually inspect, flag, and interpolate
# Outputs written to /output/gapctd/ in rds files ending with "_qc.rds"

gapctd::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                 in_pattern = "_best_oxygen.rds",
                                 review = c("density", "salinity", "oxygen"))


# 7. Review ----------------------------------------------------------------------------------------

# Review profiles plots and select the casts that pass quality control checks.

gapctd::review_profiles(rds_dir_path = here::here("output", processing_method),
                        threshold = -1e-5,
                        in_pattern = "_qc.rds",
                        out_pattern = "_final_ts.rds")

gapctd::review_oxygen_ph_profiles(rds_dir_path = here::here("output", processing_method),
                                  in_pattern = "_final_ts.rds",
                                  out_pattern = "_final.rds")


# 8. Finalize --------------------------------------------------------------------------------------

# Move final profile data to /final_cnv/
gapctd::finalize_data(rds_dir_path = here::here("output", processing_method))

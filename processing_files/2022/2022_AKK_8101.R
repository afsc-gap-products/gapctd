library(gapctd)

vessel <- 162
cruise <- c(202201, 202202)
region <- "BS"
processing_method <- "gapctd"
ctd_dir <- "G:/RACE_CTD/data/2022/ebs/v162_8101"
channel <- gapctd::get_connected(schema = "AFSC")

# Setup directory and retrieve haul data. Running this more than once will remove all files from the working directory and require reprocessing from scratch.
gapctd:::setup_gapctd_directory(processing_method = processing_method, 
                                ctd_dir = ctd_dir)

# Get haul data from RACEBASE and write to an .rds file in /output/
haul_df <- gapctd:::get_haul_data(channel = channel,
                                  vessel = vessel,
                                  cruise = cruise,
                                  tzone = "America/Anchorage")
# Load haul data
haul_df <- readRDS(file = here::here("output", 
                                     paste0("HAUL_DATA_", vessel, "_", paste(cruise, collapse = "_"), ".rds")))

# Round 1: Estimate alignment parameters, use typical CTM parameters -------------------------------
gapctd:::wrapper_run_gapctd(cnv_dir_path = here::here("cnv"),
                            processing_method = processing_method,
                            haul_df = haul_df,
                            ctm_pars = list(alpha_C = 0.04, beta_C = 0.125),
                            gapctd_round = 1)

# Make metadata and bottom averages file
gapctd:::make_metadata_file(rds_dir_path = here::here("output", "gapctd"),
                            in_pattern = "_full.rds",
                            output_path = here::here("metadata", 
                                                     paste0("CTD_HAUL_DATA_", vessel, "_", paste(cruise, collapse = "_"), ".rds")))

# Move 'bad' files to bad_cnv
gapctd:::move_bad_rds(rds_dir_path = here::here("output", processing_method),
                      in_pattern = "_full.rds")

# Move 'bad' files to bad_cnv
gapctd:::move_bad_rds(rds_dir_path = here::here("output", processing_method),
                      in_pattern = "_split.rds")

# Visually inspect, flag, and interpolate
gapctd:::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                  review = c("density", "salinity"))

# Review profiles
gapctd:::review_profiles(rds_dir_path = here::here("output", processing_method),
                         threshold = -1e-5, 
                         in_pattern = "_qc.rds")


# Round 2: Optimize alignment parameters, optimize CTM parameters using T-S area  ------------------
gapctd:::wrapper_run_gapctd(cnv_dir_path = here::here("cnv"),
                            processing_method = processing_method,
                            haul_df = haul_df,
                            ctm_pars = list(),
                            gapctd_round = 2)

# Move 'bad' files to bad_cnv
gapctd:::move_bad_rds(rds_dir_path = here::here("output", processing_method),
                      in_pattern = "_full.rds")

# Visually inspect, flag, and interpolate
gapctd:::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                  review = c("density", "salinity"))

# Review profiles
gapctd:::review_profiles(rds_dir_path = here::here("output", processing_method),
                         threshold = -1e-5, 
                         in_pattern = "_qc.rds")

# Round 3: Optimize alignment parameters, optimize CTM parameters using S path distance ------------
gapctd:::remedial_ctm(rds_path = here::here("output", processing_method),
                      haul_df = haul_df)

gapctd:::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                  review = c("density", "salinity"))

gapctd:::review_profiles(rds_dir_path = here::here("output", processing_method),
                         threshold = -1e-5,
                         in_pattern = "_qc.rds")

# Finalize data product ----------------------------------------------------------------------------
gapctd::finalize_data(rds_dir_path = here::here("output", processing_method))

library(gapctd)

vessel <- 176
region <- "AI"
cruise <- c(202201)
processing_method <- "gapctd"
ctd_dir <- "G:/RACE_CTD/data/2022/ai/v176_8102/202201_176_L3"
channel <- gapctd::get_connected(schema = "AFSC")

# Setup directory
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

# Run data processing algorithm on files. Write .rds
gapctd:::wrapper_run_gapctd(cnv_dir_path = here::here("cnv"),
                            processing_method = processing_method,
                            haul_df = haul_df)

# Make metadata and bottom averages file
gapctd:::make_metadata_file(rds_dir_path = here::here("output", "gapctd"),
                            in_pattern = "_raw.rds",
                            output_path = here::here("metadata", 
                                                     paste0("CTD_HAUL_DATA_", vessel, "_", paste(cruise, collapse = "_"), ".rds")))

# Move 'bad' files to bad_cnv
gapctd:::move_bad_rds(rds_dir_path = here::here("output", processing_method))

# Visually inspect, flag, and interpolate
gapctd:::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                  review = c("density", "salinity"))

# Review profiles
gapctd:::review_profiles(rds_dir_path = here::here("output", processing_method),
                         threshold = -1e-5, 
                         in_pattern = "_qc.rds")

# Examine rejected profiles and run cell thermal mass corrections with alternate values using split_ctm()
gapctd:::remedial_ctm(rds_path = here::here("output", processing_method),
                      haul_df = haul_df)

##---- Second round of review
# Visually inspect, flag, and interpolate
gapctd:::wrapper_flag_interpolate(rds_dir_path = here::here("output", processing_method),
                                  review = c("density", "salinity"))

# Review profiles
gapctd:::review_profiles(rds_dir_path = here::here("output", processing_method),
                         threshold = -1e-5, 
                         in_pattern = "_qc.rds")

# Finalize
gapctd::finalize_data(rds_dir_path = here::here("output", processing_method))
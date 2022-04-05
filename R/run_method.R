#' Process files using a specified batch processing method
#' 
#' Function to run a batch processing workflow and save output. Useful for comparing batch processing workflows using different sets of SBE data processing modules.
#' 
#' @param vessel RACEBASE Vessel. Passed to gapctd::run_sbe_batch() and gapctd::remove_bad_station_data()
#' @param year Year. Passed to gapctd::run_sbe_batch() and gapctd::remove_bad_station_data()
#' @param region RACEBASE region. Passed to gapctd::run_sbe_batch() and gapctd::remove_bad_station_data()
#' @param channel RODBC channel. Passed to gapctd::run_sbe_batch(), and gapctd::get_haul_events(), gapctd::remove_bad_station_data()
#' @param processing_method Character vector indicating which processing method to use. Passed to gapctd::setup_ctd_processing_dir
#' @param ctd_dir Directory containing CTD data. Passed to gapctd::setup_ctd_processing_dir
#' @param rm_files Logical. Should be removed from the directory before starting processing
#' @export

run_method <- function(vessel, year, region, channel, processing_method, last_pattern = "TEOS10.cnv", ctd_dir = "G:/RACE_CTD/data/2021/ebs/v162", alignment_df = NULL) {
  
  # Retrieve haul data
  if(file.exists(here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))) {
    haul_df <- readRDS(here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))
  } else {
    print("Running query")
    if(class(channel) == "RODBC") {
      haul_df <- RODBC::sqlQuery(channel, 
                                 paste0("select * from racebase.haul where vessel = ", vessel, 
                                        " and region = '", region, 
                                        "' and cruise between ", year*100, " and ", year*100+99))
      
      saveRDS(object = haul_df, 
              file = here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))
    }
  }
  
  # Create processing directory for a method
  if(!dir.exists(here::here("output", processing_method))) {
    dir.create(here::here("output", processing_method))
  }
  
  # Select workflow
  if(is.null(alignment_df)) {
    workflow <- 1
  } else {
    workflow <- 2
  }
  
  if(workflow == 1) {
    
    # Empty directories
    cnv_files <- list.files(path = here::here("cnv"), full.names = TRUE)
    bad_cnv_files <- list.files(path = here::here("bad_cnv"), full.names = TRUE)
    meta_files <- list.files(path = here::here("metadata"), full.names = TRUE)
    data_files <-  list.files(path = here::here("data"), full.names = TRUE, pattern = ".hex")
    psa_files <- list.files(path = here::here("psa_xmlcon"), full.names = TRUE, pattern = ".psa")
    xmlcon_files <- list.files(path = here::here("psa_xmlcon"), full.names = TRUE, pattern = ".xmlcon")
    bat_files <- list.files(path = here::here(), full.names = TRUE, pattern = ".bat")
    
    message(paste0("Deleting files from working directory:\n", 
                   length(cnv_files), " cnv\n", 
                   length(bad_cnv_files), " bad_cnv\n", 
                   length(meta_files), " metadata\n", 
                   length(data_files), " .hex data\n",
                   length(psa_files), " .psa\n",
                   length(xmlcon_files), " .xmlcon\n",
                   length(bat_files), " .bat\n"))
    
    file.remove(cnv_files)
    file.remove(bad_cnv_files)
    file.remove(meta_files)
    file.remove(data_files)
    file.remove(psa_files)
    file.remove(xmlcon_files)
    file.remove(bat_files)
    
    gapctd::setup_ctd_processing_dir(ctd_dir = ctd_dir,
                                     ctd_unit = processing_method,
                                     recursive = TRUE)
    
    gapctd::run_sbe_batch(vessel = vessel,
                          year = year,
                          region = region,
                          rodbc_channel = channel, 
                          haul_df = haul_df)
    
    gapctd::get_haul_events(channel = channel,
                            append_haul_metadata = TRUE)
    
    gapctd::calc_bottom_mean(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), 
                                                             full.names = TRUE),
                             pattern = last_pattern,
                             timezone = "America/Anchorage",
                             time_buffer = 30,
                             append_haul_metadata = TRUE)
    
    gapctd::make_cast_sections(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), 
                                                               full.names = TRUE),
                               pattern = last_pattern, 
                               xmlcon_file = NA,
                               section_bat = NA,
                               binavg_bat = NA, 
                               pressure_bat = NA) 
    
    gapctd::remove_bad_station_data(vessel = vessel,
                                    year = year,
                                    region = region,
                                    haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), 
                                                                    full.names = TRUE),
                                    max_diff_bt_ctd_depth = 10,
                                    min_ctd_depth = 5)
  }
  
  if(workflow == 2) {
    
    message("Running automatic alignment. Processed files will not be cleared from working directory.")
    
    gapctd::run_sbe_batch(vessel = vessel,
                          year = year,
                          region = region,
                          rodbc_channel = channel, 
                          haul_df = haul_df,
                          alignment_df = alignment_df)
    
    gapctd::make_cast_sections(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), 
                                                               full.names = TRUE),
                               pattern = last_pattern, 
                               xmlcon_file = NA,
                               section_bat = NA,
                               binavg_bat = NA, 
                               pressure_bat = NA)
  }
  
  file.remove(list.files(here::here("output", processing_method), full.names = TRUE))
  
  # Move files to method directory
  dc_files <- list.files(here::here("cnv"), pattern = "downcast", full.names = TRUE)
  uc_files <- list.files(here::here("cnv"), pattern = "upcast", full.names = TRUE)
  
  file.copy(dc_files,
            gsub(pattern = "/cnv/", 
                 replacement = paste0("/output/", processing_method, "/"),
                 x = dc_files))
  
  file.copy(uc_files,
            gsub(pattern = "/cnv/", 
                 replacement = paste0("/output/", processing_method, "/"),
                 x = uc_files))
  
}
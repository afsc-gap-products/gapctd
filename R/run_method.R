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

run_method <- function(vessel, year, region, channel, processing_method, last_pattern = "TEOS10.cnv", ctd_dir = "G:/RACE_CTD/data/2021/ebs/v162", rm_files = TRUE) {
  
  if(!dir.exists(here::here("output", processing_method))) {
    dir.create(here::here("output", processing_method))
  }
  
  # Empty directories
  cnv_files <- list.files(path = here::here("cnv"), full.names = TRUE)
  bad_cnv_files <- list.files(path = here::here("bad_cnv"), full.names = TRUE)
  meta_files <- list.files(path = here::here("metadata"), full.names = TRUE)
  data_files <-  list.files(path = here::here("data"), full.names = TRUE, pattern = ".hex")
  psa_files <- list.files(path = here::here("psa_xmlcon"), full.names = TRUE, pattern = ".psa")
  xmlcon_files <- list.files(path = here::here("psa_xmlcon"), full.names = TRUE, pattern = ".xmlcon")
  bat_files <- list.files(path = here::here(), full.names = TRUE, pattern = ".bat")
  

  
  if(rm_files) {
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
  }

  
  gapctd::setup_ctd_processing_dir(ctd_dir = ctd_dir,
                                   ctd_unit = processing_method,
                                   recursive = TRUE)
  
  gapctd::run_sbe_batch(vessel = vessel,
                        year = year,
                        region = region,
                        rodbc_channel = channel)
  
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
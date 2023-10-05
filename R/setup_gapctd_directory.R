#' Setup gapctd directory (R workflow)
#' 
#' Setup directory for processing using the R-based workflow
#' 
#' @param processing_method "gapctd"
#' @param ctd_dir Filepath to directory with .hex and .xmlcon files
#' @param use_sbedp_to_convert Logical. Should hex files be converted using SBE Data Processing software (TRUE) or gapctd's hexadecimal conversion functions (FALSE). Conversion with SBEDP is faster but requires that the software is installed.
#' @param bat_file Optional. Filepath to a batch (.bat) file for batch processign using SBE data processing.
#' @return Files to working directory.
#' @noRd

setup_gapctd_directory <- function(processing_method = "gapctd", ctd_dir, use_sbedp_to_convert = TRUE, bat_file = NULL) {
  
  storage_directory <- here::here("output", processing_method)
  
  # Copy hex and xmlcon files to processing directory ---- 
  hex_files <- list.files(ctd_dir,
                          pattern = "*.hex", 
                          full.names = TRUE,
                          recursive = TRUE)
  
  xmlcon_file <- list.files(ctd_dir, 
                            pattern = "*.xmlcon", 
                            full.names = TRUE,
                            recursive = TRUE)
  
  
  # Error messages for hex and xmlcon files
  stopifnot("setup_gapctd_directory: No .hex files found in ctd_dir" = length(hex_files) > 0)
  stopifnot("setup_gapctd_directory: No .xmlcon files found in ctd_dir"  = !(length(xmlcon_file) < 1))
  stopifnot("setup_gapctd_directory: More than one .xmlcon files found in ctd_dir. Can only have one .xmlcon (configuration) file."  = !(length(xmlcon_file) > 1))
  
  # Create processing directory for a method
  if(!dir.exists(storage_directory)) {
    dir.create(paste0(getwd(),"/output"))
    dir.create(storage_directory)
  }
  
  # Empty directory
  repl_cnv_files <- list.files(path = here::here("cnv"), full.names = TRUE)
  repl_rds_files <- list.files(path = here::here("output", processing_method), full.names = TRUE, pattern = ".rds")
  repl_bad_cnv_files <- list.files(path = here::here("bad_cnv"), full.names = TRUE)
  repl_meta_files <- list.files(path = here::here("metadata"), full.names = TRUE)
  repl_data_files <-  list.files(path = here::here("data"), full.names = TRUE, pattern = ".hex")
  repl_psa_files <- list.files(path = here::here("psa_xmlcon"), full.names = TRUE, pattern = ".psa")
  repl_xmlcon_files <- list.files(path = here::here("psa_xmlcon"), full.names = TRUE, pattern = ".xmlcon")
  repl_bat_files <- list.files(path = here::here(), full.names = TRUE, pattern = ".bat")
  
  message(paste0("Deleting files from working directory:\n", 
                 length(repl_cnv_files), " cnv\n", 
                 length(repl_rds_files), " rds\n", 
                 length(repl_bad_cnv_files), " bad_cnv\n", 
                 length(repl_meta_files), " metadata\n", 
                 length(repl_data_files), " .hex data\n",
                 length(repl_psa_files), " .psa\n",
                 length(repl_xmlcon_files), " .xmlcon\n",
                 length(repl_bat_files), " .bat\n"))
  
  file.remove(repl_cnv_files)
  file.remove(repl_rds_files)
  file.remove(repl_bad_cnv_files)
  file.remove(repl_meta_files)
  file.remove(repl_data_files)
  file.remove(repl_psa_files)
  file.remove(repl_xmlcon_files)
  file.remove(repl_bat_files)
  
  # Verify that ctd unit is correct ----
  processing_method <- tolower(processing_method)
  
  if(!(processing_method %in% tolower(list.files(system.file("extdata", package = "gapctd"))))) {
    stop(paste0("processing_method must be one of: ", paste0(paste0("\'", list.files(system.file("extdata", package = "gapctd")), collapse = "\', "),"\'")))
  }
  
  # Setup processing directory structure ----
  
  output_dir <- setwd(here::here())
  
  if(!dir.exists("data")) {
    dir.create("data")
  }
  
  if(!dir.exists(here::here("data", "split"))) {
    dir.create(here::here("data", "split"))
  }
  
  if(!dir.exists("psa_xmlcon")) {
    dir.create("psa_xmlcon")
  }
  
  if(!dir.exists("cnv")) {
    dir.create("cnv")
  }
  
  if(!dir.exists("bad_cnv")) {
    dir.create("bad_cnv")
  }
  
  if(!dir.exists("final_cnv")) {
    dir.create("final_cnv")
  }
  
  if(!dir.exists("output")) {
    dir.create("output")
  }
  
  if(!dir.exists("plots")) {
    dir.create("plots")
  }
  
  if(!dir.exists("metadata")) {
    dir.create("metadata")
  }
  
  # Copy hex files to data subdirectory ----
  message(paste0("Copying ", length(hex_files), " .hex files from ", ctd_dir))
  file.copy(hex_files, "./data")
  
  # Copy xmlcon files to xmlcon subdirectory ----
  file.copy(xmlcon_file, "./psa_xmlcon")
  
  # Copy psa files to pas_xmlcon subdirectory ----
  psa_files <- list.files(system.file(package = "gapctd", paste0("extdata/", processing_method)), 
                          full.names = TRUE,
                          pattern = "*.psa")
  print(paste0("Copying ", length(psa_files), " .psa files."))
  file.copy(psa_files, "./psa_xmlcon")
  
  bat_files <- list.files(system.file(package = "gapctd", paste0("extdata/", processing_method)), 
                          full.names = TRUE,
                          pattern = "*.bat")
  print(paste0("Copying ", length(bat_files), " .bat files."))
  file.copy(bat_files, "./")
  
  # Get data directories ----
  if(is.null(bat_file)) {
    message("setup_gapctd_directory: Automatically selecting getdata.bat file (no user-specified argument to bat_file)")
    bat_file <- list.files(pattern = "getdata.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("setup_gapctd_directory: No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", bat_file))) {
      stop(paste0("setup_gapctd_directory: ", bat_file, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  if(use_sbedp_to_convert) {
    xmlcon_local <- list.files(path = "./psa_xmlcon", pattern = "xmlcon")
    
    message("setup_gapctd_directory: Starting sbebatch")
    system(command = paste0("sbebatch ", getwd(), "/", bat_file, " ", getwd(), " ", xmlcon_local))
  } else {
    
    hex_files <- list.files(here::here("data"), pattern = ".hex", full.names = TRUE)
    cnv_output <- gsub(x = hex_files, pattern = ".hex", replacement = "_raw.cnv")
    cnv_output <- gsub(x = cnv_output, pattern = "/data/", replacement = "/cnv/")
    xmlcon_path <- list.files(path = here::here("psa_xmlcon"), 
                              pattern = "xmlcon", 
                              full.names = TRUE)
    
    for(II in 1:length(hex_files)) {
      message("setup_gapctd_directory: Converting ", hex_files[II])
      hex_to_cnv(hex_path = hex_files[II], 
                 output_path = cnv_output[II],
                 xmlcon_path = xmlcon_path,
                 sample_interval = 0.25,
                 output_channels = NULL,
                 output_sig_digits = NULL
                 )
    }
    
    # Save calibration parameters to RDS
    calibration_parameters <- gapctd::extract_calibration_xmlcon(xmlcon_path = xmlcon_path)
    
    calibration_file <- here::here("psa_xmlcon", "calibration_parameters.rds")
    message("setup_gapctd_directory: Saving calibration parameters to ", calibration_file)
    saveRDS(calibration_parameters, file = calibration_file)
    
  }
  
  gapctd:::.check_duplicates()
  
}
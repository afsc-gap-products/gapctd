#' Setup gapctd directory (R workflow)
#' 
#' Setup directory for processing using the R-based workflow
#' 
#' @param processing_method "gapctd"
#' @param ctd_dir Filepath to directory with .hex and .xmlcon files
#' @param bat_file Optional. Filepath to a batch (.bat) file for batch processign using SBE data processing.
#' @return Files to working directory.
#' @noRd

setup_gapctd_directory <- function(processing_method = "gapctd", ctd_dir, bat_file = NULL) {
  
  # ctd_dir = "G:/RACE_CTD/data/2021/ebs/v94_ctd1"
  # processing_method = "gapctd"
  # bat_file = NULL
  
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
  
  if(!dir.exists(here::here("output", "density_corrected"))) {
    dir.create(here::here("output",  "density_corrected"))
  }
  
  if(!dir.exists(here::here("output", "manual_flag"))) {
    dir.create(here::here("output", "manual_flag"))
  }
  
  if(!dir.exists(here::here("output", "accepted_profiles"))) {
    dir.create(here::here("output", "accepted_profiles"))
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
  
  # Get data, filter, loop edit, align ----
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
  
  xmlcon_local <- list.files(path = "./psa_xmlcon", pattern = "xmlcon")
  
  message("setup_gapctd_directory: Starting sbebatch")
  system(command = paste0("sbebatch ", getwd(), "/", bat_file, " ", getwd(), " ", xmlcon_local))
  
}
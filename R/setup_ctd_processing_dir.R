#' Function to copy CTD files from survey directory to a working directory
#' 
#' @param ctd_dir Character vector. indicating the filepath to the CTD directory.
#' @param output_dir Character vector indicating directory for saving the output. Default NA uses the current directory.
#' @param unit Character vector indicating model of the unit. Options = sbe19plus, sbe19. Won't run if the wrong unit is used.
#' @param recursive Logical. Passed to 'recursive' argument in list.files, indicating whether to recurse into directories.
#' @export

setup_ctd_processing_dir <- function(ctd_dir,
                                     output_dir = NA,
                                     ctd_unit,
                                     recursive = TRUE) {
  
  # Verify that ctd unit is correct ----
  ctd_unit <- tolower(ctd_unit)
  
  if(!(ctd_unit %in% tolower(list.files(system.file("extdata", package = "gapctd"))))) {
    stop(paste0("ctd_unit must be one of: ", paste0(paste0("\'", list.files(system.file("extdata", package = "gapctd")), collapse = "\', "),"\'")))
  }
  
  # Setup processing directory structure ----
  if(is.na(output_dir)) {
    output_dir <- setwd(here::here())
  }

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
  
  # Copy hex and xmlcon files to processing directory ---- 
  hex_files <- list.files(ctd_dir,
                          pattern = "*.hex", 
                          full.names = TRUE,
                          recursive = recursive)
  
  xmlcon_file <- list.files(ctd_dir, 
                            pattern = "*.xmlcon", 
                            full.names = TRUE,
                            recursive = recursive)
  
  # Error messages for hex and xmlcon files
  if(length(xmlcon_file) > 1) {
    stop("Multiple xmlcon files found in the CTD directory (ctd_dir)! Can only have one xmlcon file!")
  }

  if(length(xmlcon_file) < 1) {
    stop("No xmlcon file found in the CTD directory (ctd_dir)!  An xmlcon file must be included in the ctd directory.")
  }
  
  if(length(hex_files) < 1) {
    stop("No hex files found in the CTD directory (ctd_dir)!")
  }
  
  # Copy hex files to data subdirectory ----
  print(paste0("Copying ", length(hex_files), " .hex files from ", ctd_dir))
  file.copy(hex_files, "./data")
  
  # Copy xmlcon files to xmlcon subdirectory ----
  file.copy(xmlcon_file, "./psa_xmlcon")
  
  # Copy psa files to pas_xmlcon subdirectory ----
  psa_files <- list.files(system.file(package = "gapctd", paste0("extdata/", ctd_unit)), 
                          full.names = TRUE,
                          pattern = "*.psa")
  print(paste0("Copying ", length(psa_files), " .psa files."))
  file.copy(psa_files, "./psa_xmlcon")
  
  bat_files <- list.files(system.file(package = "gapctd", paste0("extdata/", ctd_unit)), 
                          full.names = TRUE,
                          pattern = "*.bat")
  print(paste0("Copying ", length(bat_files), " .bat files."))
  file.copy(bat_files, "./")
}


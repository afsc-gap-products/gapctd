#' Function to run SBE19 batch file
#' 
#' Wrapper function to run SBE DataProcessing on Windows using command line in the local directory. Run after gapctd::setup_ctd_processing_dir().
#' 
#' @param xmlcon_file Optional character vector specifying the name of the .xmlcon file. Default NA automatically retrieves the filename from psa_xmlcon subdirectory of the working directory.
#' @param bat_file Optional character vector specifying the name of the .bat file in the working directory to use. Must specify if multiple batch files are present in the working directory (i.e., you're comparing multiple processing methods).
#' @export


run_sbe_batch <- function(xmlcon_file = NA,
                          bat_file = NA) {
  
  print(paste0("Batch processing in directory: ", getwd()))
  
  if(is.na(xmlcon_file)) {
    print("Automatically retrieving .xmlcon (no user-specified argument to xmlcon_file)")
    xmlcon_file <- list.files("./psa_xmlcon/", pattern = ".xmlcon")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .xmlcon file found in ", getwd(), "/psa_xmlcon/. Must have a valid .xmlcon file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/psa_xmlcon/", xmlcon_file))) {
      stop(paste0(xmlcon_file, " file not found in ", getwd(), "/psa_xmlcon/", ". Must have a valid .xmlcon file in /psa_xmlcon/."))
    }
  }
  
  if(is.na(bat_file)) {
    print("Automatically selecting .bat file (no user-specified argument to bat_file)")
    bat_file <- list.files(pattern = ".bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", bat_file))) {
      stop(paste0(bat_file, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  print("Starting sbebatch")
  system(command = paste0("sbebatch ", getwd(), "/", bat_file, " ", getwd(), " ", xmlcon_file))
  
}
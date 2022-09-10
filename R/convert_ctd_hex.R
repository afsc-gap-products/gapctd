#' Check at-sea CTD data
#' 
#'  In development
#'  
#'  @param ctd_directory_path Path to CTD directory
#'  @param xmlcon_path Path to xmlcon file
#'  @noRd

convert_ctd_hex <- function(ctd_directory_path, 
                            xmlcon_path, 
                            bat_file = NULL, 
                            datcnv_file = NULL) {
  
  if(is.null(bat_file)) {
    bat_file <- system.file("extdata/atsea/atsea_getdata.bat", package = "gapctd")    
  }
  
  if(is.null(datcnv_file)) {
    datcnv_file <- system.file("extdata/atsea/DatCnv.psa", package = "gapctd")
  }
  
  if(!dir.exists(paste0(getwd(), "/output"))) {
    dir.create(paste0(getwd(), "/output"))
  }
  
  if(!dir.exists(paste0(getwd(), "/data"))) {
    dir.create(paste0(getwd(), "/data"))
  }
  
  # Generate list of files
  hex_file_paths <- list.files(path = ctd_directory_path, 
                               pattern = ".hex",
                               full.names = TRUE)
  
  file.copy(from = hex_file_paths,
            to = gsub(pattern = ctd_directory_path, 
                      replacement = paste0(getwd(), "/data"),
                      x = hex_file_paths))
  
  # Run batch processing to generate raw .cnv files
  system(command = paste0("sbebatch ", 
                          bat_file, " ", 
                          getwd(), " ", 
                          datcnv_file, " ",
                          xmlcon_path))
}
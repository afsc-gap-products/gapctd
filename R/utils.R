#' Create a database connection using RODBC
#'
#' A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
#'
#' @param schema Data source name (DSN) as a character vector.
#' @return An RODBC class ODBC connection.
#' @export

get_connected <- function(schema = NA){
  (echo = FALSE)
  if(is.na(schema)) {
    schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
  }
  username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                 uid = paste(username),
                                 pwd = paste(password),
                                 believeNRows = FALSE)
}



#' Convert decimal degree latitude to NMEA format
#' 
#' @param x Numeric decimal degrees
#' @noRd

ddlat_to_nmea <- function(x) {
  paste0(format(floor(abs(x)), 2), " ", 
         format(abs(x)%%1*60, nsmall = 3), " ",
         c("S", "N")[sign(x) == c(-1, 1)])
}



#' Convert decimal degree longitude to NMEA format
#' 
#' @param x Numeric decimal degrees
#' @noRd

ddlon_to_nmea <- function(x) {
  paste0(format(floor(abs(x)), 2), " ", 
         format(abs(x)%%1*60, nsmall = 3), " ",
         c("W", "E")[sign(x) == c(-1, 1)])
}



#' Use readline to accept responses and evaluate if response are valid
#' 
#' @param valid_responses Vector of valid response for readline.
#' @param prompt readline prompt to display. See ?readline
#' @noRd

accept_response <- function(valid_responses, prompt) {
  
  accept <- FALSE 
  
  while(!accept) {
    entry <- readline(prompt = prompt)
    
    if((entry %in% valid_responses) & length(entry) == 1) {
      accept <- TRUE
    } else {
      message("Invalid entry, please try again. Must be one of ", paste(valid_responses, sep = ", "))
    }
  }
  return(entry)
}



#' Convert CTD .hex file to .cnv using SBE Data Processing
#'
#' Converts SBE19plus V2 CTD hex file to cnv using Sea-Bird Data Processing software. cnv files generated with this function will be properly formatted for conversion to BTD using CTDtoBTD().
#'
#' @param hex_file_path Path to a single .hex file from the CTD.
#' @param xmlcon_path Path to CTD the config file (.xmlcon) for the CTD that was used to collect data in the .hex file.
#' @param bat_file Optional. Filepath to a batch (.bat) file to be executed in command line to run SBE Data Processing. Default NULL automatically uses the .bat file that is included in GAPsurvey.
#' @param datcnv_file Optional. Filepath to .psa file for the SBE Data Processing Datcnv module. Default NULL automatically uses the .psa file that is included in GAPsurvey.
#' @return Writes decoded CTD data to a cnv file and returns the path to the file as a 1L character vector.
#' @export
#'
#' @examples
#' # Copy system files to working directory for example
#' file.copy(from = system.file("exdata/CTDtoBTD/2021_06_13_0003.hex", package = "GAPsurvey"),
#' to = gsub(pattern = system.file("exdata/CTDtoBTD/", package = "GAPsurvey"),
#'           replacement = getwd(),
#'           x = system.file("exdata/CTDtoBTD/2021_06_13_0003.hex", package = "GAPsurvey")))
#'
#' file.copy(from = system.file("exdata/CTDtoBTD/19-8102_Deploy2021.xmlcon", package = "GAPsurvey"),
#'           to = gsub(pattern = system.file("exdata/CTDtoBTD/", package = "GAPsurvey"),
#'                     replacement = getwd(),
#'                     x = system.file("exdata/CTDtoBTD/19-8102_Deploy2021.xmlcon", package = "GAPsurvey")))
#'
#' # Run convert_ctd_hex()
#' GAPsurvey:::convert_ctd_hex(hex_file_path = "2021_06_13_0003.hex",
#'                             xmlcon_path = "19-8102_Deploy2021.xmlcon",
#'                             bat_file = NULL,
#'                             datcnv_file = NULL)

convert_ctd_hex <- function(hex_file_path,
                            xmlcon_path,
                            bat_file = NULL,
                            datcnv_file = NULL) {
  
  # Replace double backslashes in filepaths with forward slashes
  hex_file_path <- gsub(pattern = "\\\\", replacement = "/", x = hex_file_path)
  xmlcon_path<- gsub(pattern = "\\\\", replacement = "/", x = xmlcon_path)
  
  # Handle case where file is in the root directory
  if(dirname(hex_file_path) == ".") {
    hex_file_path <- paste0(getwd(), "/", hex_file_path)
  }
  
  if(dirname(xmlcon_path) == ".") {
    xmlcon_path <- paste0(getwd(), "/", xmlcon_path)
  }
  
  if(!file.exists(hex_file_path)) {
    stop("convert_ctd_hex: Specified hex_file_path (", hex_file_path,  ") does not exist. Please replace with a valid path to a .hex file.")
  }
  
  message("convert_ctd_hex: Attempting to convert ", hex_file_path, " to .cnv")
  
  out_path <- gsub(pattern = ".hex", replacement = "_raw.cnv", x = hex_file_path)
  
  # Specify paths to .bat and .psa files for SBE data processing
  if(is.null(bat_file)) {
    bat_file <- system.file("exdata/CTDtoBTD/atsea_getdata.bat", package = "GAPsurvey")
  }
  
  if(is.null(datcnv_file)) {
    datcnv_file <- system.file("exdata/CTDtoBTD/DatCnv.psa", package = "GAPsurvey")
  }
  
  # Run SBE data processing
  system(command = paste0("sbebatch ",
                          bat_file, " ",
                          hex_file_path, " ",
                          datcnv_file, " ",
                          xmlcon_path, " ",
                          sub("/[^/]+$", "", hex_file_path)
  ))
  
  if(file.exists(out_path)) {
    message("convert_ctd_hex: .cnv file created at ", out_path, ".")
  } else {
    stop("convert_ctd_hex: Failed to convert .hex to .cnv.")
  }
  
  return(out_path)
}



#' Remove rds files based on QA/QC flags (R workflow)
#' 
#' Remove deployment rds files that contain bottom, upcast, and/or bottom oce objects based on QA/QC flags.
#' 
#' @param rds_dir_path File path to directory containing rds files to be evaluated.
#' @return Bad files moved to /bad_cnv/
#' @export

move_bad_rds <- function(rds_dir_path = here::here("output", "gapctd")) {
  
  rds_path <- list.files(rds_dir_path, pattern = ".rds", full.names = TRUE)
  
  n_moved <- 0
  
  for(hh in 1:length(rds_path)) {
    
    message("move_bad_rds: Checking ", rds_path[hh])
    eval_deployment <- readRDS(rds_path[hh])
    
    dc_flag <- TRUE
    if("downcast" %in% names(eval_deployment)) {
      dc_flag <- any(as.logical(eval_deployment$downcast@metadata$flags), na.rm = TRUE)
    }
    
    uc_flag <- TRUE
    if("upcast" %in% names(eval_deployment)) {
      uc_flag <- any(as.logical(eval_deployment$upcast@metadata$flags), na.rm = TRUE)
    }
    
    # Move bad rds file
    if(any(dc_flag, uc_flag)) {
      message(paste0("move_bad_rds: Moving ", rds_path[hh]))
      
      new_loc <- gsub(pattern = paste0("/output/", processing_method, "/"), 
                      replacement = "/bad_cnv/",
                      x = rds_path[hh])
      
      file.rename(from =  rds_path[hh], to = new_loc)
      
      n_moved <- n_moved + 1
    }
    
    # Remove only the bad profile
    if(dc_flag != uc_flag) {
      
      cond <- FALSE
      
      if(dc_flag & "downcast" %in% names(eval_deployment) & !uc_flag) {
        eval_deployment <- eval_deployment[-which(names(eval_deployment) == "downcast")]
        out_path <- gsub(pattern = "_raw.rds", replacement = "_uc_raw.rds", x = rds_path[hh])
        cond <- TRUE
      }
      
      if(uc_flag & "upcast" %in% names(eval_deployment) & !dc_flag) {
        eval_deployment <- eval_deployment[-which(names(eval_deployment) == "upcast")]
        out_path <- gsub(pattern = "_raw.rds", replacement = "_dc_raw.rds", x = rds_path[hh])
        cond <- TRUE
      }
      
      if(cond) {
        saveRDS(object = eval_deployment, file = out_path)
      }
      
      
    }
  }
  
  message(paste0("move_bad_rds: Moved ", n_moved, " files to /bad_cnv/"))
  
}



#' Move good and bad profiles to final directories (R workflow)
#' 
#' @param rds_dir_path Path to directory containing rds files
#' @export

finalize_data <- function(rds_dir_path = here::here("output", processing_method)) {
  
  
  bad_files <- c(list.files(rds_dir_path, pattern = "_uc_uc_raw.rds", full.names = TRUE),
                 list.files(rds_dir_path, pattern = "_dc_dc_raw.rds", full.names = TRUE))
  
  if(length(bad_files) > 0) {
    bad_cnv_path <- gsub(pattern = rds_dir_path,
                         replacement = here::here("bad_cnv"),
                         x = bad_files)
    
    message("finalize_data: Moving ", length(bad_files), " files to ", here::here("bad_cnv"))
    
    file.rename(from = bad_files, 
                to = bad_cnv_path)    
  }
  
  
  good_files <- list.files(rds_dir_path, pattern = "_final.rds", full.names = TRUE)
  
  good_cnv_path <- gsub(pattern = rds_dir_path,
                        replacement = here::here("final_cnv"),
                        x = good_files)
  
  message("finalize_data: Moving ", length(good_files), " files to ", here::here("final_cnv"))
  
  file.rename(from = good_files, 
              to = good_cnv_path)
  
}
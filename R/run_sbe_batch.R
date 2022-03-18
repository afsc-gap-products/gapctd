#' Function to run SBE19 batch file
#' 
#' Wrapper function to run SBE DataProcessing on Windows using command line in the local directory. Run after gapctd::setup_ctd_processing_dir().
#' 
#' @param vessel Required. Vessel number as a numeric vector.
#' @param year Required. Year as a numeric vector. 
#' @param region Required. Region as a character vector. Either "BS", "AI", or "GOA".
#' @param xmlcon_file Optional. Character vector specifying the name of the .xmlcon file. Default NA automatically retrieves the filename from psa_xmlcon subdirectory of the working directory.
#' @param bat_file Optional. Character vector specifying the name of the .bat file in the working directory to use to run data conversion, filter, alignment, cell thermal mass correction, and split. If not provided, defaults to *getdata.bat
#' @param derive_file Optional. Character vectory specifying the .bat file for deriving EOS80 and TEOS-10. If not provided, defaults to *derive.bat
#' @param rodbc_channel Required if haul_csv is not provided. Must provide an open RODBC channel (this parameter) or path to haul_csv. Default = NA.
#' @param haul_csv Required if rodbc_channel is not provided. Path to a csv file that contains VESSEL, CRUISE, HAUL, START_TIME, START_LONGITUDE, START_LATITUDE, END_LONGITUDE, END_LATITUDE.
#' @param make_NMEA Required. Should NMEA files be generated? Default = TRUE.
#' @export

run_sbe_batch <- function(vessel, year, region, xmlcon_file = NA, bat_file = NA, derive_file = NA, rodbc_channel = NA, haul_csv = NA, make_NMEA = TRUE) {
  
  # Check for rodbc_channel or haul_csv ----
  if(make_NMEA) {
    if(!(class(rodbc_channel) == "RODBC") & is.na(haul_csv)) {
      stop("Must pass either rodbc_channel or haul_csv")
    } else {
      if(!is.na(rodbc_channel) & !is.na(haul_csv)) {
        stop("Must pass only one of rodbc_channel or haul_csv")
      }
      if(is.na(rodbc_channel)) {
        if(!file.exists(haul_csv)) {
          stop(paste0(haul_csv, " does not exist!"))        
        }
      } else {
        if(!(class(rodbc_channel) == "RODBC")) {
          stop(paste0("Invalid channel. Not RODBC."))
        }
      }
    }
  }
  
  # Check for valid region ----
  region <- toupper(region)
  
  if(!(region %in% c("AI", "BS", "GOA"))) {
    stop(paste0("Invalid region selection ('", region, "')! Must be AI, BS, or GOA."))
  }
  
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
  
  # Get data, filter, loop edit, align ----
  if(is.na(bat_file)) {
    print("Automatically selecting getdata.bat file (no user-specified argument to bat_file)")
    bat_file <- list.files(pattern = "getdata.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", bat_file))) {
      stop(paste0(bat_file, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  print("Starting sbebatch to get data")
  system(command = paste0("sbebatch ", getwd(), "/", bat_file, " ", getwd(), " ", xmlcon_file))
  
  if(make_NMEA) {
    print("Generating NMEA files")
    gapctd::create_NMEA_files(rodbc_channel = rodbc_channel,
                              haul_csv = haul_csv,
                              year = year,
                              vessel = vessel,
                              region = region)
  } else {
    print("Generating NMEA proxy files with coordinates 57 N, -168 W")
    cnv_files <- list.files(path = paste0(getwd(), "/cnv"), pattern = "\\_tmcorrect.cnv$")
    for(i in 1:length(cnv_files)) {
      ## paste info together for txt file output ----
      first_line <- paste0("// VESSEL ", vessel, ", CRUISE NA", ", HAUL NA"," //")
      second_line <- "Longitude: -168"
      third_line <- "Latitude: 57"
      
      ##write the nmea txt files ----
      file_name <- gsub("\\..*","",cnv_files[i])
      
      # Setup NMEA for no EOS80 ----
      fileConn <- file(paste0(getwd(), "/cnv/", file_name, ".txt"))
      writeLines(c(first_line, second_line, third_line), fileConn)
      close(fileConn)
      
      # Setup NMEA with EOS80 ----
      fileConn <- file(paste0(getwd(), "/cnv/", file_name, "_EOS80.txt"))
      writeLines(c(first_line, second_line, third_line), fileConn)
      close(fileConn)
    }
  }
  
  # Derive EOS80 and TEOS10 ----
  if(is.na(derive_file)) {
    print("Automatically selecting derive.bat file (no user-specified argument to bat_file)")
    derive_file <- list.files(pattern = "derive.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", derive_file))) {
      stop(paste0(derive_file, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  print("Starting sbebatch to derive EOS80 and TEOS10")
  system(command = paste0("sbebatch ", getwd(), "/", derive_file, " ", getwd(), " ", xmlcon_file))
  
  # Remove cnv files without data ----
  # print("Removing bad cnv files")
  # gapctd::move_binned_cnv()
  
}
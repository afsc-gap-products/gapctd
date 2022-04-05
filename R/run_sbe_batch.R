#' Function to run SBE19 batch file
#' 
#' Wrapper function to run SBE DataProcessing on Windows using command line in the local directory. Run after gapctd::setup_ctd_processing_dir().
#' 
#' @param vessel Required. Vessel number as a numeric vector.
#' @param year Required. Year as a numeric vector. 
#' @param region Required. Region as a character vector. Either "BS", "AI", or "GOA".
#' @param xmlcon_file Optional. Character vector specifying the name of the .xmlcon file. Default NA automatically retrieves the filename from psa_xmlcon subdirectory of the working directory.
#' @param bat_file Optional. Character vector specifying the name of the .bat file in the working directory to use to run data conversion, filter, alignment, cell thermal mass correction, and split. If not provided, defaults to *getdata.bat
#' @param derive_file Optional. Character vector specifying the .bat file for deriving EOS80 and TEOS-10. If not provided, defaults to *derive.bat
#' @param rodbc_channel Required if haul_csv is not provided. Must provide an open RODBC channel (this parameter) or path to haul_csv. Default = NA.
#' @param make_NMEA Required. Should NMEA files be generated? Default = TRUE.
#' @param haul_df Data frame containing haul data.
#' @param alignment_df Data frame containing alignment parameters.
#' @export

run_sbe_batch <- function(vessel, year, region, xmlcon_file = NA, bat_file = NA, derive_file = NA, rodbc_channel = NA, make_NMEA = TRUE, alignment_df = NULL, haul_df = NULL, ...) {
  
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
  
  # Run without automatic alignment
  if(is.null(alignment_df)) {
    
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
    
    print("Starting sbebatch")
    system(command = paste0("sbebatch ", getwd(), "/", bat_file, " ", getwd(), " ", xmlcon_file))
    
  } else {
    
    message("Running automatic alignment")
    if(!(nrow(alignment_df) > 0) ) {
      stop("run_sbe_batch: No data found in alignment_df!")
    }
    
    # Abbreviation for upcast and downcast profile outputs
    alignment_df$dir_abbv <- c("uuu", "ddd")[match(alignment_df$cast_direction, c("upcast", "downcast"))]
    
    for(kk in 1:nrow(alignment_df)) {
      
      # Change alignment
      gapctd::change_psa_alignctd(variable = "temperature",
                                  alignment = alignment_df$best_adj[kk])
      
      # Find files from the deployment
      deploy_files <- list.files(here::here("cnv"), 
                                 pattern = alignment_df$deploy_id[kk], 
                                 full.names = TRUE)
      
      if(!(length(deploy_files) > 0)) {
        warning(paste0("Files not found for: ", alignment_df$deploy_id[kk]))
        next
      }
      
      # Remove unused files
      keep_files <- c(grep(pattern = "raw.cnv", x = deploy_files),
                      grep(pattern = "wfil.cnv", x = deploy_files),
                      grep(pattern = "fil1.cnv", x = deploy_files),
                      grep(pattern = "_uuu_", x = deploy_files),
                      grep(pattern = "_ddd_", x = deploy_files))
      invisible(file.remove(deploy_files[-keep_files]))
      
      if(alignment_df$best_adj[kk] == 0) {
        in_celltm <- deploy_files[grepl(pattern = "fil1.cnv", x = deploy_files)]
        in_loopedit <- gsub(pattern = "\\.cnv", 
                            replacement =  paste0("_", alignment_df$dir_abbv[kk], "_align_tmcorrect.cnv"),
                            x = in_celltm)
        
        system(command = paste0("sbebatch ", getwd(), "/sbe19plus_noalign.bat", " ", 
                                getwd(), " ", #%1
                                xmlcon_file, " ", #%2
                                in_celltm, " ", #%3
                                paste0("_", alignment_df$dir_abbv[kk], "_align_tmcorrect"), " ", #%4
                                in_loopedit, #%5
                                " #m"))
      } else {
        # Setup input paths for alignment, celltm, loopedit
        in_align <- deploy_files[grepl(pattern = "fil1.cnv", x = deploy_files)]
        in_celltm <- gsub(pattern = "\\.cnv", 
                          replacement =  paste0("_", alignment_df$dir_abbv[kk], "_align.cnv"),
                          x = in_align )
        in_loopedit <- gsub(pattern = "\\.cnv", 
                            replacement = "_tmcorrect.cnv",
                            x = in_celltm)
        
        # Calculate new alignments
        system(command = paste0("sbebatch ", getwd(), "/sbe19plus_realign.bat", " ", 
                                getwd(), " ", #%1
                                xmlcon_file, " ", #%2
                                in_align, " ", #%3
                                paste0("_", alignment_df$dir_abbv[kk], "_align"), " ", #%4
                                in_celltm, " ", #%5
                                in_loopedit, #%6
                                " #m"))
      }
    }
  }
  
  if(make_NMEA) {
    print("Generating NMEA files")
    gapctd::create_NMEA_files(rodbc_channel = rodbc_channel,
                              year = year,
                              vessel = vessel,
                              region = region,
                              haul_df = haul_df, 
                              ...)
  } else {
    print("Generating NMEA proxy files with coordinates 57 N, -168 W")
    cnv_files <- list.files(path = paste0(getwd(), "/cnv"), pattern = "\\_loopedit.cnv$")
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
  
}
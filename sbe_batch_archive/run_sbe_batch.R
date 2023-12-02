#' Function to copy CTD files from survey directory to a working directory (SBEDP workflow)
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



#' Process files using a specified batch processing method (SBEDP workflow)
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

run_method <- function(vessel, year, region, channel, processing_method, storage_directory = NULL, last_pattern = "TEOS10.cnv", ctd_dir = "G:/RACE_CTD/data/2021/ebs/v162", alignment_df = NULL, ctm_df = NULL, ...) {
  
  
  if(is.null(storage_directory)) {
    storage_directory <- here::here("output", processing_method)
  }
  
  # Create processing directory for a method
  if(!dir.exists(storage_directory)) {
    dir.create(paste0(getwd(),"/output"))
    dir.create(storage_directory)
  }
  
  # Select workflow
  if(is.null(alignment_df) & is.null(ctm_df)) {
    workflow <- 1 # Runs the whole way through and produces metadata output.
  } else {
    workflow <- 2 # For estimating and applying alignment and cell thermal mass corrections.
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
    
    # Retrieve haul data
    if(file.exists(here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))) {
      haul_df <- readRDS(here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))
    } else {
      message("run_method: Running query")
      if(class(channel) == "RODBC") {
        haul_df <- RODBC::sqlQuery(channel, 
                                   paste0("select * from racebase.haul where vessel = ", vessel, 
                                          " and region = '", region, 
                                          "' and cruise between ", year*100, " and ", year*100+99))
        
        saveRDS(object = haul_df, 
                file = here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))
      }
    }
    
    gapctd::run_sbe_batch(vessel = vessel,
                          year = year,
                          region = region,
                          rodbc_channel = channel, 
                          haul_df = haul_df,
                          alignment_df = alignment_df,
                          ctm_df = ctm_df)
    
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
    
    message("run_method: Running automatic alignment. Processed files will not be cleared from working directory.")
    
    
    haul_df <- readRDS(here::here("data", paste0("haul_dat_", year, "_", region, "_", vessel, ".rds")))
    message(paste0("run_method: Loaded data from ", nrow(haul_df), " hauls." ))
    
    
    message("run_method: Starting batch processing.")
    gapctd::run_sbe_batch(vessel = vessel,
                          year = year,
                          region = region,
                          rodbc_channel = channel, 
                          haul_df = haul_df,
                          alignment_df = alignment_df, 
                          ctm_df = ctm_df)
    
    gapctd::make_cast_sections(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), 
                                                               full.names = TRUE),
                               pattern = last_pattern, 
                               xmlcon_file = NA,
                               section_bat = NA,
                               binavg_bat = NA, 
                               pressure_bat = NA)
    
  }
  
  if(length(list.files(storage_directory, full.names = TRUE)) >= 1){
    file.remove(list.files(storage_directory, full.names = TRUE))
  }
  
  # Move files to method directory
  dc_files <- list.files(here::here("cnv"), pattern = "downcast", full.names = TRUE)
  uc_files <- list.files(here::here("cnv"), pattern = "upcast", full.names = TRUE)
  
  file.copy(dc_files,
            gsub(pattern = here::here("cnv"), 
                 replacement = paste0(storage_directory, "/"),
                 x = dc_files))
  
  file.copy(uc_files,
            gsub(pattern = here::here("cnv"), 
                 replacement = paste0(storage_directory, "/"),
                 x = uc_files))
  
}



#' Function to run SBE19 batch file (SBEDP workflow)
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

run_sbe_batch <- function(vessel, year, region, xmlcon_file = NA, bat_file = NA, derive_file = NA, rodbc_channel = NA, make_NMEA = TRUE, alignment_df = NULL, ctm_df = NULL, haul_df = NULL, ...) {
  
  # Check for valid region ----
  region <- toupper(region)
  
  batch_method <- which(
    c(is.null(alignment_df) & is.null(ctm_df),
      !is.null(alignment_df) & is.null(ctm_df),
      is.null(alignment_df) & !is.null(ctm_df),
      !is.null(alignment_df) & !is.null(ctm_df)))
  
  batch_method_name <- c("Basic processing", "Estimate alignment parameters", "Estimate cell thermal mass correction", "Invalid selection")[batch_method]
  
  print(paste0("Batch method: ", batch_method, " - ", batch_method_name))
  
  if(batch_method == 4) {stop("run_sbe_batch: cannot provide both alignment_df and ctm_df. One must be NULL.")}
  
  if(!(region %in% c("AI", "BS", "GOA"))) {
    stop(paste0("run_sbe_batch: Invalid region selection ('", region, "')! Must be AI, BS, or GOA."))
  }
  
  message(paste0("run_sbe_batch: Batch processing in directory: ", getwd()))
  
  if(is.na(xmlcon_file)) {
    message("run_sbe_batch: Automatically retrieving .xmlcon (no user-specified argument to xmlcon_file)")
    xmlcon_file <- list.files("./psa_xmlcon/", pattern = ".xmlcon")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("run_sbe_batch: No .xmlcon file found in ", getwd(), "/psa_xmlcon/. Must have a valid .xmlcon file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/psa_xmlcon/", xmlcon_file))) {
      stop(paste0("run_sbe_batch: ", xmlcon_file, " file not found in ", getwd(), "/psa_xmlcon/", ". Must have a valid .xmlcon file in /psa_xmlcon/."))
    }
  }
  
  # Derive EOS80 and TEOS10 ----
  if(is.na(derive_file)) {
    message("run_sbe_batch: Automatically selecting derive.bat file (no user-specified argument to bat_file)")
    derive_file <- list.files(pattern = "derive.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", derive_file))) {
      stop(paste0(derive_file, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  print(batch_method)
  
  # Run without automatic alignment
  if(batch_method == 1) {
    
    nmea_pattern <- "_loopedit"
    
    write_metadata <- TRUE
    
    # Get data, filter, loop edit, align ----
    if(is.na(bat_file)) {
      message("run_sbe_batch: Automatically selecting getdata.bat file (no user-specified argument to bat_file)")
      bat_file <- list.files(pattern = "getdata.bat")
      
      if(length(xmlcon_file) < 1) {
        stop(paste0("run_sbe_batch: No .bat file found in ", getwd(), ". Must have a valid .bat file."))
      } 
    } else {
      if(!file.exists(paste0(getwd(), "/", bat_file))) {
        stop(paste0("run_sbe_batch: ", bat_file, " file not found in ", getwd(), ". Must have a valid .bat file."))
      }
    }
    
    message("run_sbe_batch: Starting sbebatch")
    system(command = paste0("sbebatch ", getwd(), "/", bat_file, " ", getwd(), " ", xmlcon_file))
    
  } else if(batch_method == 2)  {
    
    nmea_pattern <- "_loopedit"
    write_metadata <- FALSE
    
    message("run_sbe_batch: Estimating alignment parameters")
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
                      grep(pattern = "loopedit.cnv", x = deploy_files),
                      grep(pattern = "_uuu_", x = deploy_files),
                      grep(pattern = "_ddd_", x = deploy_files))
      invisible(file.remove(deploy_files[-keep_files]))
      
      if(alignment_df$best_adj[kk] == 0) {
        in_celltm <- deploy_files[grepl(pattern = "fil1.cnv", x = deploy_files)]
        
        if(file.exists(in_celltm)) {
          system(command = paste0("sbebatch ", getwd(), "/sbe19plus_noalign.bat", " ", 
                                  getwd(), " ", #%1
                                  xmlcon_file, " ", #%2
                                  in_celltm, " ", #%3
                                  paste0("_", alignment_df$dir_abbv[kk], "_align"), " ", #%4
                                  " #m"))
        } else {
          warning(paste0("run_sbe_batch: ", in_celltm, " not found."))
        }
        

      } else {
        # Setup input paths for alignment, celltm, loopedit
        in_align <- deploy_files[grepl(pattern = "fil1.cnv", x = deploy_files)]
        
        if(file.exists(in_align)) {
        
        in_celltm <- gsub(pattern = "\\.cnv", 
                          replacement =  paste0("_", alignment_df$dir_abbv[kk], "_align.cnv"),
                          x = in_align)
        
        # Calculate new alignments
        system(command = paste0("sbebatch ", getwd(), "/sbe19plus_realign.bat", " ", 
                                getwd(), " ", #%1
                                xmlcon_file, " ", #%2
                                in_align, " ", #%3
                                paste0("_", alignment_df$dir_abbv[kk], "_align"), " ", #%4
                                " #m")) 
        } else {
          warning(paste0("run_sbe_batch: ", in_align, " not found."))
        }
      }
    }
  } else if(batch_method == 3) {
    
    nmea_pattern <- "_tmcorrect"
    write_metadata <- FALSE
    
    message("run_sbe_batch: Estimating CTM parameters")
    if(!(nrow(ctm_df) > 0) ) {
      stop("run_sbe_batch: No data found in ctm_df!")
    }
    
    # Add upcast and downcast columns if only one set of CTM parameters
    if(!"cast_direction" %in% names(ctm_df)) {
      ctm_df <- dplyr::bind_rows(ctm_df |>
        dplyr::mutate(cast_direction = "downcast"),
        ctm_df |>
          dplyr::mutate(cast_direction = "upcast"))
      
    }
    
    # Abbreviation for upcast and downcast profile outputs
    ctm_df$dir_abbv <- c("uuu", "ddd")[match(ctm_df$cast_direction, c("upcast", "downcast"))]
    
    for(kk in 1:nrow(ctm_df)) {
      
      # Change alignment
      gapctd::change_psa_celltm(alpha = round(ctm_df$alpha[kk], 5),
                                tau = round(ctm_df$tau[kk], 5))
      
      # Find files from the deployment
      deploy_files <- list.files(here::here("cnv"), 
                                 pattern = ctm_df$deploy_id[kk], 
                                 full.names = TRUE)
      
      if(!(length(deploy_files) > 0)) {
        warning(paste0("run_sbe_batch: Files not found for: ", ctm_df$deploy_id[kk]))
        next
      }
      
      # Remove unused files
      keep_files <- c(grep(pattern = "raw.cnv", x = deploy_files),
                      grep(pattern = "tmcorrect.cnv", x = deploy_files),
                      grep(pattern = "align.cnv", x = deploy_files))
      invisible(file.remove(deploy_files[-keep_files]))
      
      # Setup input paths for alignment, celltm, loopedit
      in_celltm <- deploy_files[grepl(pattern = paste0("_", ctm_df$dir_abbv[kk], "_align.cnv"), x = deploy_files)]
      
      # Calculate new alignments
      system(command = paste0("sbebatch ", getwd(), "/sbe19plus_tmcorrect.bat", " ", 
                              getwd(), " ", #%1
                              xmlcon_file, " ", #%2
                              in_celltm, " ", #%3
                              nmea_pattern, #%4
                              " #m"))
    }
    
  }

  if(make_NMEA) {
    nmea_pattern <- ifelse(length(list.files(path = paste0(getwd(), "/cnv/"),
                                      pattern = "_loopedit.cnv")) >= 1, "_loopedit", "_fil1")
    
    message("run_sbe_batch: Generating NMEA files")
    gapctd::create_NMEA_files(rodbc_channel = rodbc_channel,
                              year = year,
                              vessel = vessel,
                              region = region,
                              haul_df = haul_df, 
                              nmea_pattern = nmea_pattern,
                              write_metadata = write_metadata)
  } else {
    message("run_sbe_batch: Generating NMEA proxy files with coordinates 57 N, 168 W")
    cnv_files <- list.files(path = paste0(getwd(), "/cnv"), pattern = paste0("\\", nmea_pattern, ".cnv$"))
    for(i in 1:length(cnv_files)) {
      ## paste info together for txt file output ----
      first_line <- paste0("// VESSEL ", vessel, ", CRUISE NA", ", HAUL NA"," //")
      second_line <- "Longitude: -168"
      third_line <- "Latitude: 57"
      
      ##write the NMEA txt files ----
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
  
  message("run_sbe_batch: Starting sbebatch to derive EOS80 and TEOS10")
  system(command = paste0("sbebatch ", getwd(), "/", derive_file, " ", getwd(), " ", xmlcon_file, " ", paste0("*", nmea_pattern, ".cnv")))
  
}



#' Create NMEA files with LAT/LON for Derive TEOS-10 (SBEDP workflow)
#'
#' Function to retrieve haul data from RACEBASE or a csv file and generate NMEA (.txt) files that match .cnv file names. Function dependencies: getPass, RODBC, oce
#'
#' @param rodbc_channel Required if haul_csv is not provided. Must provide an open RODBC channel (this parameter) or path to haul_csv. Default = NA.
#' @param vessel Required. Vessel number as a numeric vector.
#' @param year Required. Year as a numeric vector. 
#' @param region Required. Region as a character vector. Either "bs", "ai", or "goa".
#' @param haul_df If TRUE, loads an rds file named "data/haul_data.rds" instead of running a query to retrieve haul data.
#' @param nmea_pattern Character vector denoting the last appended name from the cnv for which the NMEA file should access.
#' @export

create_NMEA_files <- function(rodbc_channel = NA, 
                              vessel,
                              region,
                              year, 
                              haul_df = NULL,
                              write_metadata = TRUE,
                              nmea_pattern)
{
  
  ## Load cnv files ----
  cnv_files <- list.files(path = paste0(getwd(), "/cnv"), pattern = paste0("\\", nmea_pattern, ".cnv$"))
  message(paste0("CNV files found: ", length(cnv_files)))
  
  ###### ADD cnv_files check ----
  
  if(is.null(haul_df)) {
    message("Running query")
    haul_df <- RODBC::sqlQuery(rodbc_channel, "select * from racebase.haul where cruise > 200700")
  }
  
  ###### create empty data frame for metadata storage ----
  survey_metadata <- data.frame(matrix(ncol = 16, 
                                       nrow = 0, 
                                       dimnames=list(NULL, c("VESSEL",
                                                             "CRUISE",
                                                             "HAUL",
                                                             "TIME_DIFF_MINUTES",
                                                             "START_LATITUDE",
                                                             "START_LONGITUDE",
                                                             "END_LATITUDE",
                                                             "END_LONGITUDE",
                                                             "DATE",
                                                             "tow_start_time",
                                                             "STATIONID",
                                                             "cnv_file_name",
                                                             "GEAR_TEMPERATURE",
                                                             "SURFACE_TEMPERATURE",
                                                             "GEAR_DEPTH",
                                                             "PERFORMANCE"))))
  
  
  ###### ADD START_TIME STRING LENGTH CHECK and haul_df row check ----
  
  haul_df$YEAR <- floor(haul_df$CRUISE/100)
  haul_df <- haul_df[haul_df$YEAR == year, ]
  haul_df <- haul_df[haul_df$VESSEL == vessel, ]
  haul_df <- haul_df[haul_df$REGION == region, ]
  haul_df$tow_date <- substring(haul_df$START_TIME, 1,10)
  haul_df$tow_start_time <- substring(haul_df$START_TIME, 12,19)
  
  for(i in 1:length(cnv_files))
  {
    
    current_cnv <- oce::read.oce(paste0(getwd(), "/cnv/", cnv_files[i]))
    cnv_date <- substring(current_cnv[["metadata"]]$startTime, 1, 10)
    cnv_time_zone <- attr(current_cnv[["metadata"]]$startTime, "tzone")
    
    ## Find haul that matches cnv metadata by finding the haul of the right date that has the least diff between cnv start time & haul start time ----
    subset_haul <- haul_df[haul_df$tow_date == cnv_date, ]
    haul_time_zone <- attr(as.POSIXlt(subset_haul$START_TIME[1]),"tzone")[3]
    
    time_diff <- abs(
      as.numeric(
        difftime(strptime(subset_haul$START_TIME, "%Y-%m-%d %H:%M:%S"),
                 strptime(current_cnv[["metadata"]]$startTime, "%Y-%m-%d %H:%M:%S"),
                 units = "mins")
      )
    )
    minimum_diff <- min(time_diff)
    position <- which(time_diff == minimum_diff)
    
    ### & extract start lat/lon, vessel, cruise, haul from matching haul file to create NMEA file ----
    nmea_vessel <- subset_haul[position, ]$VESSEL
    nmea_cruise <- subset_haul[position, ]$CRUISE
    nmea_haul <- subset_haul[position, ]$HAUL
    nmea_longitude <- subset_haul[position, ]$START_LONGITUDE
    nmea_latitude <- subset_haul[position, ]$START_LATITUDE
    
    ## write haul metadata to a .csv to be able to link to original cnv files ----
    survey_metadata_tmp <- c()
    end_longitude <- subset_haul[position, ]$END_LONGITUDE
    end_latitude <- subset_haul[position, ]$END_LATITUDE
    data_date <- subset_haul[position, ]$tow_date
    tow_start_time <- subset_haul[position, ]$tow_start_time
    station_id <- subset_haul[position, ]$STATIONID
    gear_temperature <- subset_haul[position, ]$GEAR_TEMPERATURE
    surface_temperature <- subset_haul[position, ]$SURFACE_TEMPERATURE
    gear_depth <- subset_haul[position, ]$GEAR_DEPTH
    performance <- subset_haul[position, ]$PERFORMANCE
    
    # Add NMEA info to cnv header
    cnv_text <- readLines(paste0(getwd(), "/cnv/", cnv_files[i]))
    head_cnv <- cnv_text[c(1:6)]
    tail_cnv <- cnv_text[c(7:length(cnv_text))]
    lon_line <- paste0("** Longitude: ", gapctd:::ddlon_to_nmea(nmea_longitude))
    lat_line <- paste0("** Latitude: ", gapctd:::ddlat_to_nmea(nmea_latitude))
    vessel_line <- paste0("** ship: ", nmea_vessel)
    station_line <- paste0("** station: ", station_id)
    depth_line <- paste0("** depth: ", gear_depth)
    
    writeLines(text = c(head_cnv, lon_line, lat_line, vessel_line, station_line, depth_line, tail_cnv), con = paste0(getwd(), "/cnv/", cnv_files[i]))
    
    # paste info together for txt file output ----
    first_line <- paste0("// VESSEL ", nmea_vessel,", CRUISE ", nmea_cruise,", HAUL ", nmea_haul," //")
    second_line <- paste0("Longitude: ", nmea_longitude)
    third_line <- paste0("Latitude: ", nmea_latitude)
    
    
    survey_metadata_tmp <- data.frame(cbind("VESSEL" = nmea_vessel, 
                                            "CRUISE" = nmea_cruise, 
                                            "HAUL" = nmea_haul, 
                                            "TIME_DIFF_MINUTES" = minimum_diff, 
                                            "START_LATITUDE" = nmea_latitude, 
                                            "START_LONGITUDE" = nmea_longitude,
                                            "END_LATITUDE" = end_latitude,
                                            "END_LONGITUDE" = end_longitude,
                                            "DATE" = data_date,
                                            "tow_start_time" = tow_start_time, 
                                            "STATIONID" = station_id,
                                            "cnv_file_name" = cnv_files[i],
                                            "deploy_id" = gsub(pattern = "_raw.*", "", x = cnv_files[i]),
                                            "BT_GEAR_TEMPERATURE" = gear_temperature,
                                            "BT_SURFACE_TEMPERATURE" = surface_temperature,
                                            "BT_GEAR_DEPTH" = gear_depth,
                                            "PERFORMANCE" = performance))
    
    survey_metadata <- rbind(survey_metadata, survey_metadata_tmp)
    
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
  
  if(write_metadata) {
    write.csv(survey_metadata, 
              file = paste0(getwd(), "/metadata/survey_metadata_", year, ".csv"),
              row.names = FALSE)
  }
  
}



#' Calculate station bottom temperature and salinity (SBEDP workflow)
#' 
#' Use the oce package to retrieve CTD bottom data from cnv files and calculate mean temperature and salinity. Run after haul events have been populated to metadata and before calc_surface_mean().
#' 
#' @param haul_metadata_path Character vector. Path to haul data .csv generated by get_bottom_data().
#' @param pattern Character vector. Suffix of cnv file to search for. Default = "TEOS10.cnv"
#' @param timezone Timezone as a character vector to force instrument time. Default = "America/Anchorage" to correspond with survey time.
#' @param time_buffer 1L numeric vector. Buffer between on-bottom/off-bottom events and first/last data point to use to calculate bottom values, in seconds.
#' @param append_haul_metadata Logical. Should the file named in haul_metadata_path be appended (overwritten) to include mean bottom values and metadata?
#' @export

calc_bottom_mean <- function(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), full.names = TRUE),
                             pattern = "TEOS10.cnv",
                             timezone = "America/Anchorage",
                             time_buffer = 30,
                             append_haul_metadata = TRUE) {
  
  if(length(haul_metadata_path) != 1) {
    stop(paste("get_haul_events(): Argument haul_meta_data path must only be file but has length ", length(haul_metadata_path)))
  }
  
  # Load haul metadata file
  haul_metadata <- read.csv(file = haul_metadata_path, 
                            stringsAsFactors = FALSE)
  
  out_df <- haul_metadata
  
  # Extract bottom data from each haul
  for(i in 1:nrow(haul_metadata)) {
    
    if(i%%50 == 0) {
      print(paste0("Calculating bottom values from file ", i, " out of ", nrow(haul_metadata)))
    }
    
    fpath <- list.files(here::here("cnv"), pattern = sub("\\_raw.*", "", haul_metadata$cnv_file_name[i]), 
                        full.names = TRUE)
    fpath <- fpath[grepl(pattern = pattern, fpath, fixed = TRUE)]
    
    # Get cast_start time
    cast_dat <- suppressWarnings(try(oce::read.ctd.sbe(file = fpath), silent = TRUE))
    
    if(class(cast_dat) == "try-error") {
      
      warning("try-error in ", fpath)
      
      out_df$CTD_MEAN_HAUL_DEPTH[i] <- NA
      out_df$CTD_MEAN_BOTTOM_TEMPERATURE_C[i] <- NA
      out_df$CTD_MEAN_BOTTOM_SALINITY_SA[i] <- NA
      out_df$CTD_MEAN_BOTTOM_SALINITY_SP[i] <- NA
      out_df$CTD_MEAN_BOTTOM_SOUNDSPEED[i] <- NA
      out_df$CTD_N_BOTTOM_SAMPLE[i] <- NA
      out_df$CTD_CALIBRATION_DATE[i] <- NA
      out_df$CTD_SERIAL_NUMBER[i] <- NA
      out_df$CTD_CAST_START[i] <- NA
      out_df$DOWNCAST_END_SECONDS[i] <- NA
      out_df$DOWNCAST_END_INDEX[i] <- NA
      out_df$UPCAST_START_SECONDS[i] <- NA
      out_df$UPCAST_START_INDEX[i] <- NA
      
      next
    }
    
    cast_start <- as.POSIXct(cast_dat@metadata$startTime)
    cast_start <- lubridate::force_tz(cast_start, tzone = timezone)
    
    sample_interval <- cast_dat@metadata$sampleInterval
    
    start_index <- (difftime(as.POSIXct(haul_metadata$ON_BOTTOM[i]) |> 
                               lubridate::force_tz(tzone = timezone), 
                             cast_start + time_buffer,
                             units = "secs") / sample_interval) |>
      as.numeric()
    
    end_index <- (difftime(as.POSIXct(haul_metadata$HAULBACK[i]) |> 
                             lubridate::force_tz(tzone = timezone), 
                           cast_start - time_buffer,
                           units = "secs") / sample_interval) |>
      as.numeric()
    
    if(!is.na(start_index) & !is.na(end_index)) {
      
      # Calibration date
      calibration_date <- cast_dat@metadata$header[which(grepl(pattern = "CalibrationDate", cast_dat@metadata$header))]
      calibration_date <- gsub(pattern = ".*<CalibrationDate>", "", calibration_date)
      calibration_date <- gsub(pattern = "</CalibrationDate>.*", "", calibration_date)
      
      
      serial_number <- cast_dat@metadata$header[which(grepl(pattern = "<SerialNumber>", cast_dat@metadata$header))]
      serial_number <- gsub(pattern = ".*<SerialNumber>", "", serial_number)
      serial_number <- gsub(pattern = "</SerialNumber>.*", "", serial_number)
      
      
      out_df$CTD_MEAN_HAUL_DEPTH[i] <- mean(cast_dat@data$depth[start_index:end_index], na.rm = TRUE)
      out_df$CTD_MEAN_BOTTOM_TEMPERATURE_C[i] <- mean(cast_dat@data$temperature[start_index:end_index], na.rm = TRUE)
      out_df$CTD_MEAN_BOTTOM_SALINITY_SA[i] <- mean(cast_dat@data$gsw_saA0[start_index:end_index], na.rm = TRUE)
      out_df$CTD_MEAN_BOTTOM_SALINITY_SP[i] <- mean(cast_dat@data$salinity[start_index:end_index], na.rm = TRUE)
      out_df$CTD_MEAN_BOTTOM_SOUNDSPEED[i] <- mean(cast_dat@data$soundSpeed[start_index:end_index], na.rm = TRUE)
      out_df$CTD_N_BOTTOM_SAMPLE[i] <- end_index - start_index + 1
      out_df$CTD_CALIBRATION_DATE[i] <- calibration_date[1]
      out_df$CTD_SERIAL_NUMBER[i] <- serial_number[1]
      out_df$CTD_CAST_START[i] <- as.character(cast_start)
      out_df$DOWNCAST_END_SECONDS[i] <- cast_dat@data$timeS[start_index] # Downcast end time in seconds elapsed
      out_df$DOWNCAST_END_INDEX[i] <- start_index
      out_df$UPCAST_START_SECONDS[i] <- cast_dat@data$timeS[end_index] # Upcast start time in seconds elapsed
      out_df$UPCAST_START_INDEX[i] <- end_index
    }
    
  }
  
  # Handle cases that don't correspond with hauls
  out_df$DOWNCAST_END_SECONDS[out_df$DOWNCAST_END_SECONDS <= 0] <- NA
  out_df$UPCAST_START_SECONDS[out_df$UPCAST_START_SECONDS <= 0] <- NA
  out_df$DOWNCAST_END_INDEX[is.na(out_df$DOWNCAST_END_SECONDS)] <- NA
  out_df$UPCAST_START_INDEX[is.na(out_df$UPCAST_START_SECONDS)] <- NA
  
  
  if(append_haul_metadata) {
    write.csv(x = out_df, 
              file = haul_metadata_path, 
              row.names = FALSE)
  }
  
  return(out_df)
  
}



#' Calculate fixed depth temperature and salinity (SBEDP workflow)
#' 
#' Use the oce package to retrieve CTD near-surface data from upcast or downcast cnv files and calculate mean temperature and salinity for a fixed depth. Run after calc_bottom_mean().
#' 
#' @param haul_metadata_path Character vector. Path to haul data .csv generated by get_bottom_data().
#' @param depth Numeric vector (1L or 2L) indicating a fixed depth or range of depths from which to calculate a mean.
#' @param append_haul_metadata Logical. Should the file named in haul_metadata_path be appended (overwritten) to include mean surface values and metadata?
#' @param cast_direction Character vector, either "upcast" or "downcast."
#' @export

calc_fixed_depth_ts <- function(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), full.names = TRUE),
                                depth_interval = 1,
                                cast_direction = "upcast",
                                append_haul_metadata = TRUE) {
  
  haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), 
                                  full.names = TRUE)
  
  cd_short <- c("d", "d", "u", "u")[match(tolower(cast_direction), c("downcast", "d", "upcast", "u"))]
  
  if(length(haul_metadata_path) != 1) {
    stop(paste("get_haul_events(): Argument haul_meta_data path must only be file but has length ", length(haul_metadata_path)))
  }
  
  # Load haul metadata file
  haul_metadata <- read.csv(file = haul_metadata_path, 
                            stringsAsFactors = FALSE)
  
  out_df <- haul_metadata
  
  # Extract surface data from downcasts
  for(i in 1:nrow(haul_metadata)) {
    
    if(i%%50 == 0) {
      print(paste0("Calculating surface values from file ", i, " out of ", nrow(haul_metadata)))
    }
    
    fpath <- list.files(here::here("final_cnv"), pattern = sub("\\_raw.*", "", haul_metadata$cnv_file_name[i]), 
                        full.names = TRUE)
    fpath <- fpath[grepl(pattern = paste0(cd_short, sub("\\_raw.*", "", haul_metadata$cnv_file_name[i])), fpath, fixed = TRUE)]
    
    if(length(fpath) == 1) {
      
      cast_dat <- suppressWarnings(oce::read.ctd.sbe(file = fpath))
      
      temp_vec <- cast_dat@data$temperature[cast_dat@data$depth >= min(depth_interval) & cast_dat@data$depth <= max(depth_interval)]
      sa_vec <- cast_dat@data$gsw_saA0[cast_dat@data$depth >= min(depth_interval) & cast_dat@data$depth <= max(depth_interval)]
      sp_vev <- cast_dat@data$salinity[cast_dat@data$depth >= min(depth_interval) & cast_dat@data$depth <= max(depth_interval)]
      
      out_df$CTD_MEAN_TEMPERATURE[i] <- mean(temp_vec, na.rm = TRUE)
      out_df$CTD_MEAN_SALINITY_SA[i] <- mean(sa_vec, na.rm = TRUE)
      out_df$CTD_MEAN_SALINITY_SP[i] <- mean(sp_vev, na.rm = TRUE)
      
    }
    
  }
  
  names(out_df)[names(out_df) == "CTD_MEAN_TEMPERATURE"] <- paste0("CTD_MEAN_TEMPERATURE_", toupper(cast_direction), "_", paste(depth_interval, collapse = "_"))
  names(out_df)[names(out_df) == "CTD_MEAN_SALINITY_SA"] <- paste0("CTD_MEAN_SALINITY_SA_", toupper(cast_direction), "_", paste(depth_interval, collapse = "_"))
  names(out_df)[names(out_df) == "CTD_MEAN_SALINITY_SP"] <- paste0("CTD_MEAN_SALINITY_SP_", toupper(cast_direction), "_", paste(depth_interval, collapse = "_"))
  
  if(append_haul_metadata) {
    write.csv(x = out_df, 
              file = haul_metadata_path, 
              row.names = FALSE)
  }
  
  return(out_df)
  
}



#' Estimate time adjustment for Align CTD (SBEDP workflow)
#' 
#' Find the time adjustment that maximizes correlations of first time difference (dx/dt) between variables, similar to Ullman and Hebert (2014) and Dever et al. (2020). 
#' 
#' @param var1 Numeric vector of measurements from the first sensors (e.g., conductivity)
#' @param var2 Numeric vector of measurements from the sensor for which measurements will be time-adjusted (e.g., temperature)
#' @param time1 Scan time in seconds as a numeric vector for the first sensor.
#' @param time2 Scan time in seconds as a numeric vector for the second sensor.
#' @param time2_adjustments Adjustments to try, in seconds, as a numeric vector.
#' @param cor_method Correlation method to use to evaluate time2_adjustment as a character vector. See documentation for cor().
#' @param interp_fn Character vector indicating which interpolation function to use ("approx" or "oce::oce.approx") to estimate time-shifted values at shifted scan times.
#' @param interp_method Interpolation method to apply to estimate time-shifted measurements. See documentation for approx() and oce::oce.approx()
#' @param return_adj Logical indicating whether to return only the time2_adjustments value with the best correlation (TRUE) or a list containing additional results (see Details below)
#' @details This function time-shifts measurements from one sensor (e.g. temperature) relative to measurements from another sensor (e.g. conductivity) for all user-provided time2_adjustment values. It then estimates a correlations between time-shifted sensor measurements and returns either the best time2_adustment estimate or a list containing the best time2_adjustment, highest correlation, and data.frame containing results of all time2_adustment comparisons. Time-shifted values between measurements can be interpolated using "rr" or "unesco" from oce::oce.approx or the "linear" method in approx. Correlation methods are any valid methods in cor().
#' @references Ullman, D.S., Hebert, D., 2014. Processing of underway CTD data. J. Atmos. Ocean. Technol. 31, 984–998. https://doi.org/10.1175/JTECH-D-13-00200.1
#' @references Dever, M., Freilich, M., Farrar, J.T., Hodges, B., Lanagan, T., Baron, A.J., Mahadevan, A., 2020. EcoCTD for profiling oceanic physical–biological properties from an underway ship. J. Atmos. Ocean. Technol. 37, 825–840. https://doi.org/10.1175/JTECH-D-19-0145.1
#' @export

estimate_alignment <- function(var1, var2, time1, time2, time2_adjustments = seq(-1.5,1.5,0.01), cor_method = "spearman", interp_method = "linear", interp_fn = "approx", only_return_adj = TRUE) {
  
  series_1 <- data.frame(t = time1,
                         x1 = var1)
  
  s2_series <- data.frame(t = time2,
                          x1 = var2)
  
  corr_vec <- numeric(length = length(time2_adjustments))
  
  for(jj in 1:length(time2_adjustments)) {
    
    series_2 <- s2_series
    series_2$t <- series_2$t + time2_adjustments[jj]
    
    loc_pred <- data.frame(locs = unique(c(series_1$t, series_2$t)))
    
    loc_pred$s1 <- switch(interp_fn,
                          `oce.approx` = {oce::oce.approx(x = series_1$t, 
                                                          y = series_1$x1, 
                                                          xout = loc_pred$locs, 
                                                          method = interp_method)},
                          `approx` = { approx(x = series_1$t, 
                                              y = series_1$x1, 
                                              xout = loc_pred$locs, 
                                              method = interp_method)$y})
    
    loc_pred$s2 <- switch(interp_fn,
                          `oce.approx` = {oce::oce.approx(x = series_2$t, 
                                                          y = series_2$x1, 
                                                          xout = loc_pred$locs, 
                                                          method = interp_method)},
                          `approx` = {approx(x = series_2$t, 
                                             y = series_2$x1, 
                                             xout = loc_pred$locs, 
                                             method = interp_method)$y})
    
    obj <- -1*cor(diff(loc_pred$s1), 
                  diff(loc_pred$s2), 
                  use = "complete.obs",
                  method = cor_method)^2
    corr_vec[jj] <- obj
    
  }
  
  if(only_return_adj) {
    return(-1*time2_adjustments[which.min(corr_vec)])
  } else{
    return(list(time2_adj = -1*time2_adjustments[which.min(corr_vec)],
                adj_cor = -1*min(corr_vec),
                adj_est = data.frame(time2_adjustment = time2_adjustments,
                                     correlation = corr_vec,
                                     cor_method = cor_method)))
  }
}



#' Wrapper function around estimate_alignment (SBEDP workflow)
#' 
#' Estimate alignment for a channel
#' 
#' @param profile_files Paths to profile files for which alignments should be estimated, as a character vector. For trawl data, should have separate upcast and downcast files.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, especially in downcasts.
#' @param make_diagnostic_plots Logical. Should diagnostic plots be saved to plots/alignment.pdf?
#' @param ... Optional arguments passed to gapctd::estimate_alignment().
#' @export

run_alignment_calcs <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                  full.names = TRUE, pattern = "downcast.cnv"),
                                                       list.files(here::here("output", "sbe19plus_v0"), 
                                                                  full.names = TRUE, pattern = "upcast.cnv"))),
                                make_diagnostic_plots = TRUE,
                                min_pressure = 4,
                                ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  cast_direction <- character(length = length(deploy_id))
  cast_direction[grepl(pattern = "downcast", x = profile_files)] <- "downcast"
  cast_direction[grepl(pattern = "upcast", x = profile_files)] <- "upcast"
  
  best_eval <- numeric(length = length(profile_files))
  best_corr <- numeric(length = length(profile_files))
  prof_rate <- numeric(length = length(profile_files))
  
  if(make_diagnostic_plots) {
    pdf(file = here::here("plots", "alignment.pdf"), onefile = TRUE)
  }
  
  for(kk in 1:length(profile_files)) {
    
    ctd_profile <- try(oce::read.ctd(file = profile_files[kk]), silent = TRUE)
    
    if(class(ctd_profile) == "try-error") next
    
    prof_rate[kk] <- mean(diff(ctd_profile@data$pressure[ctd_profile@data$pressure > min_pressure])/ctd_profile@metadata$sampleInterval)
    
    # If oxygen is added, will need to identify the adjustment column in output, which could be be passed to estimate_alignment
    cond <- ctd_profile@data$conductivity[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    cond_time <- ctd_profile@data$timeS[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    temp <- ctd_profile@data$temperature[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    temp_time <- ctd_profile@data$timeS[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    
    if(any(length(cond) < 10, 
           length(cond_time) < 10, 
           length(temp) < 10, 
           length(temp_time) < 10)) {
      next
    }
    
    est <- gapctd::estimate_alignment(var1 = cond,
                                      time1 = cond_time, 
                                      var2 = temp,
                                      time2 = temp_time,
                                      only_return_adj = FALSE, ...)
    
    best_eval[kk] <- round(est$time2_adj, 3)
    best_corr[kk] <- est$adj_cor
    
    if(make_diagnostic_plots) {
      plot(
        est$adj_est$time2_adjustment, 
        est$adj_est$correlation, 
        main = paste0(deploy_id[kk], " ", cast_direction[kk], "\n", est$time2_adj),
        xlab = "Adjustment",
        ylab = "Correlation",
      )
    }
  }
  
  if(make_diagnostic_plots) {dev.off()}
  
  return(data.frame(deploy_id = deploy_id,
                    cast_direction = cast_direction,
                    best_adj = best_eval,
                    best_corr = best_corr,
                    profile_rate = prof_rate))
}



#' Perform conductivity cell thermal inertia correction and compare upcasts to downcasts (SBEDP workflow)
#' 
#' Calculates conductivity cell thermal intertia correction using ctm_par_a(), ctm_par_b(), and ctm_correct_c_t(), and recalculates salinity for paired upcasts and downcasts. If argument obj_fn is set to area, the function calculates and return the value of an objective function which is the area between upcast and downcast temperature-salinity curves in 1 dbar pressure bins. If obj_fn = "none," returns a data.frame with upcast and downcast temperature, conductivity, corrected conductivity, and salinity in 1 dbar pressure bins.
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in the cell thermal mass correction filter (default = 0.04 for SBE19plus)
#' @param tau Numeric vector (1L). Tau (inverse beta) parameter in the cell thermal mass correction filter (default = 8 for SBE19plus).
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @param down_temperature Numeric vector of downcast temperatures in degC (ITS-90 scale),
#' @param down_pressure Numeric vector of downcast pressure in dbar.
#' @param down_conductivity Numeric vector of downcast conductivity in S/m.
#' @param up_temperature Numeric vector of upcast temperatures in degC (ITS-90 scale),
#' @param up_pressure Numeric vector of upcast pressure in dbar.
#' @param up_conductivity Numeric vector of upcast conductivity in S/m.
#' @param obj_fn Character vector ("none" or "area")
#' @param scale_vars Logical. Should temperature and salinity be scaled to calculate T-S area?
#' @export

ctm_adjust_tsarea <- function(alpha = 0.04,
                              tau = 8,
                              f_n = 0.25,
                              down_temperature,
                              down_pressure,
                              down_conductivity,
                              down_flag,
                              up_temperature,
                              up_pressure,
                              up_conductivity,
                              up_flag,
                              obj_fn = "none",
                              scale_vars = FALSE) {
  
  
  aa <- ctm_par_a(alpha = alpha, f_n = f_n, beta = 1/tau)
  bb <- ctm_par_b(alpha = alpha, a = aa)
  
  if(all(!is.null(down_temperature), 
         !is.null(down_pressure), 
         !is.null(down_conductivity), 
         !is.null(down_flag))) {
    
    down_exists <- TRUE
    
    # Recalculate downcast salinity
    downcast_c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = down_temperature)
    downcast_c_corr <- down_conductivity + downcast_c_t
    downcast_salinity_psu <- oce::swSCTp(conductivity = downcast_c_corr,
                                         temperature = down_temperature,
                                         pressure = down_pressure,
                                         conductivityUnit = "S/m")
    
  } else {
    down_exists <- FALSE
  }
  
  if(all(!is.null(up_temperature), 
         !is.null(up_pressure), 
         !is.null(up_conductivity), 
         !is.null(up_flag))) {
    up_exists <- TRUE
    
    upcast_c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = up_temperature)
    upcast_c_corr <- up_conductivity + upcast_c_t
    
    # Recalculate salinity
    upcast_salinity_psu <- oce::swSCTp(conductivity = upcast_c_corr,
                                       temperature = up_temperature,
                                       pressure = up_pressure,
                                       conductivityUnit = "S/m")
    
  } else {
    up_exists <- FALSE
  }
  
  if(up_exists & down_exists) {
    updown_df <- aggregate(x = data.frame(conductivity_down = down_conductivity[!is.na(down_flag)],
                                          conductivity_corr_down = downcast_c_corr[!is.na(down_flag)],
                                          temperature_down = down_temperature[!is.na(down_flag)],
                                          salinity_down = downcast_salinity_psu[!is.na(down_flag)]), 
                           by = list(pressure_bin = ceiling(down_pressure[!is.na(down_flag)])), 
                           FUN = mean) |>
      dplyr::full_join(
        aggregate(x = data.frame(conductivity_up = up_conductivity[!is.na(up_flag)],
                                 conductivity_corr_up = upcast_c_corr[!is.na(up_flag)],
                                 temperature_up = up_temperature[!is.na(up_flag)],
                                 salinity_up = upcast_salinity_psu[!is.na(up_flag)]), 
                  by = list(pressure_bin = ceiling(up_pressure[!is.na(up_flag)])), 
                  FUN = mean),
        by = "pressure_bin")
  } else {
    val <- c("d", "u")[c(down_exists, up_exists)]
    
    updown_df <- switch(val,
                        `d` = {
                          aggregate(x = data.frame(conductivity_down = down_conductivity[!is.na(down_flag)],
                                                   conductivity_corr_down = downcast_c_corr[!is.na(down_flag)],
                                                   temperature_down = down_temperature[!is.na(down_flag)],
                                                   salinity_down = downcast_salinity_psu[!is.na(down_flag)]), 
                                    by = list(pressure_bin = ceiling(down_pressure[!is.na(down_flag)])), 
                                    FUN = mean)
                        },
                        `u` = {
                          aggregate(x = data.frame(conductivity_up = up_conductivity[!is.na(up_flag)],
                                                   conductivity_corr_up = upcast_c_corr[!is.na(up_flag)],
                                                   temperature_up = up_temperature[!is.na(up_flag)],
                                                   salinity_up = upcast_salinity_psu[!is.na(up_flag)]), 
                                    by = list(pressure_bin = ceiling(up_pressure[!is.na(up_flag)])), 
                                    FUN = mean)
                        })
  }
  
  
  if(obj_fn == "none") {
    # Return prediction
    return(updown_df)
  } else {
    if(down_exists & up_exists) {
      if(obj_fn == "area") {
        
        comb_df <- updown_df |>
          dplyr::filter(!is.na(salinity_up),
                        !is.na(temperature_up),
                        !is.na(salinity_down),
                        !is.na(temperature_down))
        
        if(nrow(comb_df) == 0) {
          obj <- 1e7
        } else {
          
          if(scale_vars) {
            comb_df$salinity_up <- scale(c(comb_df$salinity_up, salinity_down))[,1][nrow(comb_df)]
            comb_df$salinity_down <- scale(c(salinity_down, comb_df$salinity_up))[,1][nrow(comb_df)]
            comb_df$temperature_up <- scale(c(comb_df$temperature_up, temperature_down))[,1][nrow(comb_df)]
            comb_df$temperature_down <- scale(c(comb_df$temperature_down, temperature_up))[,1][nrow(comb_df)]
          }
          
          wkt_poly <- data.frame(geometry = paste0("LINESTRING (", apply(X = 
                                                                           cbind(
                                                                             apply(
                                                                               X = cbind(
                                                                                 comb_df$salinity_down[1:(nrow(comb_df))],
                                                                                 comb_df$temperature_down[1:(nrow(comb_df))]),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               cbind(c(comb_df$salinity_up[1:(nrow(comb_df)-1)], comb_df$salinity_up[(nrow(comb_df)-1)]),
                                                                                     c(comb_df$temperature_up[1:(nrow(comb_df)-1)], comb_df$temperature_up[(nrow(comb_df)-1)])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(c(comb_df$salinity_down[2:(nrow(comb_df))],comb_df$salinity_up[(nrow(comb_df))]),
                                                                                         c(comb_df$temperature_down[2:(nrow(comb_df))],comb_df$temperature_up[(nrow(comb_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(
                                                                                 c(comb_df$salinity_down[1:(nrow(comb_df)-1)],comb_df$salinity_down[(nrow(comb_df))]),
                                                                                 c(comb_df$temperature_down[1:(nrow(comb_df)-1)],comb_df$temperature_down[(nrow(comb_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " ")),
                                                                         MARGIN = 1,
                                                                         FUN = paste,
                                                                         collapse = ", "), ")")) |>
            dplyr::mutate(ID = row_number()) |>
            st_as_sf(wkt = "geometry") |> 
            dplyr::group_by(ID) |>
            summarise(do_union = FALSE) |>
            sf::st_cast(to = "POLYGON")
          
          obj <- sum(sf::st_area(wkt_poly), na.rm = TRUE)
        }
        
        if(class(obj) == "try-error") {
          return(comb_df)
        }
      } 
    } else {
      # Path length
      obj <- switch(val,
                    `d` = {sum(abs(diff(updown_df$salinity_down)))},
                    `u` = {sum(abs(diff(updown_df$salinity_up)))})  
      
    }
    return(obj)
  }
}



#' Wrapper function around ctm_adjust_tsarea (SBEDP workflow)
#' 
#' Estimate conductivity cell thermal inertia correction using ctm_adjust_tsarea and bbmle::mle2().
#' 
#' @param profile_files Paths to profile files for which CTM parameters should be estimated.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, especially in downcasts.
#' @param optim_method Optimization method for optim(). Default is the Broyden-Fletcher-Goldfarb-Shanno algorithm ("BFGS")
#' @param optim_maxit Number of optim iterations or maximum number of iterations, depending on the optim method. Default = 500.
#' @param start_alpha Default alpha value.
#' @param start_tau Default tau value.
#' @return Returns a data frame containing \itemize{
#' \item deploy_id: deployment file ID
#' \item cast_direction: Direction of the cast ("downcast" or "upcast")
#' \item default_obj: estimated CTM objective function for the default parameters (alpha = 0.04, tau = 8)
#' \item adj_obj: estimated CTM objective function for the estimated parameters
#' \item alpha: estimated alpha parameter
#' \item tau: estimated tau[CTM] parameter
#' \item mean_t_rate: average rate of change in temperature for the cast in degrees Celsius per db (pressure).
#' \item mean_s_rate: average rate of change in salinity for the cast in PSS-78 salinity per db (pressure).
#' \item max_t_rate:  maximum rate of change in temperature for the cast in degrees Celsius per db (pressure).
#' \item max_s_rate:  maximum rate of change in salinity for the cast in PSS-78 salinity per db (pressure).
#' \item range_t: absolute range of temperature in a cast in degrees Celsius.
#' \item range_s: absolute range of salinity in a cast in PSS-78 salinity.}
#' @export

run_ctm_adjust_tsarea <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                    full.names = TRUE, pattern = "downcast.cnv"),
                                                         list.files(here::here("output", "sbe19plus_v0"), 
                                                                    full.names = TRUE, pattern = "upcast.cnv"))),
                                  min_pressure = 4,
                                  optim_method = "L-BFGS-B",
                                  optim_maxit = 500,
                                  start_alpha = 0.04,
                                  start_tau = 8,
                                  obj_method = "area",
                                  ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  cast_direction <- character(length = length(deploy_id))
  cast_direction[grepl(pattern = "downcast", x = profile_files)] <- "downcast"
  cast_direction[grepl(pattern = "upcast", x = profile_files)] <- "upcast"
  
  unique_id <- unique(deploy_id)
  
  default_obj <- numeric(length = length(unique_id))
  adj_obj <- numeric(length = length(unique_id))
  par_alpha <- numeric(length = length(unique_id))
  par_tau <- numeric(length = length(unique_id))
  mean_t_rate <- numeric(length = length(unique_id))
  mean_s_rate <- numeric(length = length(unique_id))
  max_t_rate <- numeric(length = length(unique_id))
  max_s_rate <- numeric(length = length(unique_id))
  range_t <- numeric(length = length(unique_id))
  range_s <- numeric(length = length(unique_id))
  
  for(ii in 1:length(unique_id)) {
    
    message(unique_id[ii])
    cast_files <- profile_files[grepl(pattern = unique_id[ii], profile_files)]
    
    dc_dat <- try(oce::read.ctd(file = cast_files[grepl(pattern = "downcast", 
                                                        x = cast_files)]), silent = TRUE)
    uc_dat <- try(oce::read.ctd(file = cast_files[grepl(pattern = "upcast", 
                                                        x = cast_files)]), silent = TRUE)
    
    if((class(dc_dat) == "try-error") & (class(uc_dat) == "try-error")) {
      next
    } else if(class(dc_dat) == "try-error") {
      temperature <- NULL
      flag <- NULL
      pressure <- NULL
      conductivity <- NULL
      salinity <- NULL
      
      temperature_2 <- uc_dat@data$temperature[uc_dat@data$pressure > min_pressure]
      flag_2 <- uc_dat@data$flag[uc_dat@data$pressure > min_pressure]
      pressure_2 <- uc_dat@data$pressure[uc_dat@data$pressure > min_pressure]
      conductivity_2 <- uc_dat@data$conductivity[uc_dat@data$pressure > min_pressure]
      salinity_2 <- uc_dat@data$salinity[uc_dat@data$pressure > min_pressure]
      
      # Min. sample size
      if(any(length(temperature_2) < 20, sum(!is.na(flag_2)) < 20)) {
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
        next
      }
      
      
    } else if(class(uc_dat) == "try-error") {
      temperature <- dc_dat@data$temperature[dc_dat@data$pressure > min_pressure]
      flag <- dc_dat@data$flag[dc_dat@data$pressure > min_pressure]
      pressure <- dc_dat@data$pressure[dc_dat@data$pressure > min_pressure]
      conductivity <- dc_dat@data$conductivity[dc_dat@data$pressure > min_pressure]
      salinity <- dc_dat@data$salinity[dc_dat@data$pressure > min_pressure]
      
      if(any(length(temperature) < 20, sum(!is.na(flag)) < 20)) {
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
        next
      }
      
      # Min. sample size
      temperature_2 <- NULL
      flag_2 <- NULL
      pressure_2 <- NULL
      conductivity_2 <- NULL
      salinity_2 <- NULL
      
    } else {
      temperature <- dc_dat@data$temperature[dc_dat@data$pressure > min_pressure]
      flag <- dc_dat@data$flag[dc_dat@data$pressure > min_pressure]
      pressure <- dc_dat@data$pressure[dc_dat@data$pressure > min_pressure]
      conductivity <- dc_dat@data$conductivity[dc_dat@data$pressure > min_pressure]
      salinity <- dc_dat@data$salinity[dc_dat@data$pressure > min_pressure]
      
      temperature_2 <- uc_dat@data$temperature[uc_dat@data$pressure > min_pressure]
      flag_2 <- uc_dat@data$flag[uc_dat@data$pressure > min_pressure]
      pressure_2 <- uc_dat@data$pressure[uc_dat@data$pressure > min_pressure]
      conductivity_2 <- uc_dat@data$conductivity[uc_dat@data$pressure > min_pressure]
      salinity_2 <- uc_dat@data$salinity[uc_dat@data$pressure > min_pressure]
      
      # Min. sample size
      if(any(length(temperature) < 20, sum(!is.na(flag)) < 20) & any(length(temperature_2) < 20, sum(!is.na(flag_2)) < 20)) {
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
        next
      } else if(any(length(temperature_2) < 20, sum(!is.na(flag_2)) < 20)) {
        temperature_2 <- NULL
        flag_2 <- NULL
        pressure_2 <- NULL
        conductivity_2 <- NULL
        salinity_2 <- NULL
      } else if(any(length(temperature) < 20, sum(!is.na(flag)) < 20)){
        temperature <- NULL
        flag <- NULL
        pressure <- NULL
        conductivity <- NULL
        salinity <- NULL
      }
    }
    
    default_dat <- gapctd:::ctm_adjust_tsarea(alpha = start_alpha,
                                              tau = start_tau,
                                              f_n = 0.25,
                                              down_temperature = temperature,
                                              down_pressure = pressure,
                                              down_conductivity = conductivity,
                                              down_flag = flag,
                                              up_temperature = temperature_2,
                                              up_pressure = pressure_2,
                                              up_conductivity = conductivity_2,
                                              up_flag = flag_2,
                                              obj_fn = "none")
    
    default_obj[ii] <- gapctd:::ctm_adjust_tsarea(alpha = start_alpha,
                                                  tau = start_tau,
                                                  f_n = 0.25,
                                                  down_temperature = temperature,
                                                  down_pressure = pressure,
                                                  down_conductivity = conductivity,
                                                  down_flag = flag,
                                                  up_temperature = temperature_2,
                                                  up_pressure = pressure_2,
                                                  up_conductivity = conductivity_2,
                                                  up_flag = flag_2,
                                                  obj_fn = obj_method)
    
    est_pars <- try(bbmle::mle2(minuslogl = gapctd:::ctm_adjust_tsarea,
                                start = list(alpha = start_alpha,
                                             tau = start_tau),
                                data = list(down_temperature = temperature,
                                            down_pressure = pressure,
                                            down_conductivity = conductivity,
                                            down_flag = flag,
                                            up_temperature = temperature_2,
                                            up_pressure = pressure_2,
                                            up_conductivity = conductivity_2,
                                            up_flag = flag_2,
                                            obj_fn = obj_method,
                                            f_n = 0.25),
                                method = optim_method,
                                lower = c(alpha = 0, tau = 0),
                                upper = c(alpha = 1, tau = 45),
                                control = list(maxit = optim_maxit, 
                                               reltol = 1e-4, 
                                               trace = 1, 
                                               parscale = c(alpha = 0.1, tau = 1))), 
                    silent = TRUE)
    conv <- try(est_pars@details$convergence, silent = TRUE)
    
    if(any(class(est_pars) == "try-error", conv != 0)) {
      est_pars <- try(bbmle::mle2(minuslogl = gapctd::ctm_adjust_tsarea,
                                  start = list(alpha = 0.1,
                                               tau = 12),
                                  data = list(down_temperature = temperature,
                                              down_pressure = pressure,
                                              down_conductivity = conductivity,
                                              down_flag = flag,
                                              up_temperature = temperature_2,
                                              up_pressure = pressure_2,
                                              up_conductivity = conductivity_2,
                                              up_flag = flag_2,
                                              obj_fn = obj_method,
                                              f_n = 0.25),
                                  method = optim_method,
                                  lower = c(alpha = 0, tau = 0),
                                  upper = c(alpha = 1, tau = 45),
                                  control = list(maxit = optim_maxit, 
                                                 reltol = 1e-4, 
                                                 trace = 1, 
                                                 parscale = c(alpha = 0.1, tau = 1))), 
                      silent = TRUE)
      conv <- try(est_pars@details$convergence, silent = TRUE)
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      adj_dat <- gapctd::ctm_adjust_tsarea(alpha = est_pars@coef['alpha'],
                                           tau = est_pars@coef['tau'],
                                           f_n = 0.25,
                                           down_temperature = temperature,
                                           down_pressure = pressure,
                                           down_conductivity = conductivity,
                                           down_flag = flag,
                                           up_temperature = temperature_2,
                                           up_pressure = pressure_2,
                                           up_conductivity = conductivity_2,
                                           up_flag = flag_2,
                                           obj_fn = "none")
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      
      if(est_pars@min < default_obj[ii]) {
        if(as.numeric(est_pars@coef['tau']) > 0 & 
           as.numeric(est_pars@coef['tau']) < 45) { #  0 < tau < 45 seconds constrains thermal inertia effects to last from 0-45 seconds.
          par_alpha[ii] <- as.numeric(est_pars@coef['alpha'])
          par_tau[ii] <- as.numeric(est_pars@coef['tau'])
          adj_obj[ii] <- est_pars@min
        } else {
          par_alpha[ii] <- start_alpha
          par_tau[ii] <- start_tau
        }
        
      } else {
        adj_obj[ii] <- default_obj[ii]
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
      }
      
      max_t_rate[ii] <- max(c(abs(diff(default_dat$temperature_down)/diff(default_dat$pressure_bin)),
                              abs(diff(default_dat$temperature_up)/diff(default_dat$pressure_bin))),
                            na.rm = TRUE)
      
      mean_t_rate[ii] <- mean(c(abs(diff(default_dat$temperature_down)/diff(default_dat$pressure_bin)),
                                abs(diff(default_dat$temperature_down)/diff(default_dat$pressure_bin))), na.rm = TRUE)
      
      range_t[ii] <- max(c(diff(range(default_dat$temperature_down, na.rm = TRUE)),
                           diff(range(default_dat$temperature_up, na.rm = TRUE))))
      
      max_s_rate[ii] <- max(c(abs(diff(default_dat$salinity_down)/diff(default_dat$pressure_bin)),
                              abs(diff(default_dat$salinity_up)/diff(default_dat$pressure_bin))),
                            na.rm = TRUE)
      
      mean_s_rate[ii] <- mean(c(abs(diff(default_dat$salinity_down)/diff(default_dat$pressure_bin)),
                                abs(diff(default_dat$salinity_down)/diff(default_dat$pressure_bin))), na.rm = TRUE)
      
      range_s[ii] <- max(c(diff(range(default_dat$salinity_down, na.rm = TRUE)),
                           diff(range(default_dat$salinity_up, na.rm = TRUE))))
      
    } else {
      par_alpha[ii] <- NA
      par_tau[ii] <- NA
    }
  }
  
  par_alpha[is.na(par_alpha)] <- start_alpha
  par_tau[is.na(par_tau)] <- start_tau
  out_df <- data.frame(deploy_id = unique_id,
                       default_obj = default_obj,
                       adj_obj = adj_obj,
                       alpha = round(par_alpha, 5),
                       tau = round(par_tau, 5),
                       mean_t_rate = mean_t_rate,
                       mean_s_rate = mean_s_rate,
                       max_t_rate = max_t_rate,
                       max_s_rate = max_s_rate,
                       range_t = range_t,
                       range_s = range_s)
  
  return(out_df)
  
}



#' Calculate conductivity cell thermal intertia correction (SBEDP workflow)
#' 
#' Calculates conductivity cell thermal intertia correction based on alpha, inverse beta (tau[CTM]), scan interval, temperature, and conductivity, then recalculates salinity. 
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in the cell thermal mass correction filter (default = 0.04 for SBE19plus)
#' @param tau Numeric vector (1L). Tau (inverse beta) parameter in the cell thermal mass correction filter (default = 8 for SBE19plus).
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @param temperature Numeric vector of temperature in degC (ITS-90 scale),
#' @param pressure Numeric vector of pressure in dbar.
#' @param conductivity Numeric vector of conductivity in S/m.
#' @param flag Character or numeric vector of data quality flags where NAs are scans to be excluded from final profiles. NA scans are used to estimate thermal cell inertia conductivity corrections and salinity but re removed from binned outputs. 
#' @param obj_fn Logical. If "path", returns an objective function, the total path distance of the salinity profile. Otherwise, returns temperature, salinity, and conductivity in 1 db depth bins.
#' @param binned Logical. If TRUE, objective function is calculated for pressure-binned data. If FALSE, the objective function is calculated using all un-flagged data. 
#' @export

ctm_adjust_path <- function(alpha = 0.04,
                            tau = 8,
                            f_n = 0.25,
                            temperature,
                            pressure,
                            conductivity,
                            flag,
                            obj_fn = NULL,
                            binned = TRUE) {
  
  aa <- ctm_par_a(alpha = alpha, f_n = f_n, beta = 1/tau)
  bb <- ctm_par_b(alpha = alpha, a = aa)
  
  c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = temperature)
  c_corr <- conductivity + c_t
  salinity_psu <- oce::swSCTp(conductivity = c_corr,
                              temperature = temperature,
                              pressure = pressure,
                              conductivityUnit = "S/m")
  
  out_df <- aggregate(x = data.frame(conductivity = conductivity[!is.na(flag)],
                                     corrected_conductivity = c_corr[!is.na(flag)],
                                     temperature = temperature[!is.na(flag)],
                                     salinity = salinity_psu[!is.na(flag)]), 
                      by = list(pressure_bin = round(pressure[!is.na(flag)])), 
                      FUN = mean)
  
  if(obj_fn == "path") {
    if(binned) {
      obj <- sum(abs(diff(out_df$salinity)))
    } else {
      obj <- sum(abs(diff(salinity_psu[!is.na(flag)])))
    }
    
  } else {
    obj <- out_df
  }
  
  return(obj)
}


#' Wrapper function around ctm_adjust_path (SBEDP workflow)
#' 
#' Estimate conductivity cell thermal inertia correction using ctm_adjust_path and bbmle::mle2().
#' 
#' @param profile_files Paths to profile files for which CTM parameters should be estimated.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, especially in downcasts.
#' @param optim_method Optimization method for optim(). Default is simulated annealing ("SANN")
#' @param optim_maxit Number of optim iterations or maximum number of iterations, depending on the optim method. Default = 500.
#' @param start_alpha Default alpha value.
#' @param start_tau Default tau value.
#' @return Returns a data frame containing \itemize{
#' \item deploy_id: deployment file ID
#' \item cast_direction: Direction of the cast ("downcast" or "upcast")
#' \item default_obj: estimated CTM objective function for the default parameters (alpha = 0.04, tau = 8)
#' \item adj_obj: estimated CTM objective function for the estimated parameters
#' \item alpha: estimated alpha parameter
#' \item tau: estimated tau[CTM] parameter
#' \item mean_t_rate: average rate of change in temperature for the cast in degrees Celsius per db (pressure).
#' \item mean_s_rate: average rate of change in salinity for the cast in PSS-78 salinity per db (pressure).
#' \item max_t_rate:  maximum rate of change in temperature for the cast in degrees Celsius per db (pressure).
#' \item max_s_rate:  maximum rate of change in salinity for the cast in PSS-78 salinity per db (pressure).
#' \item range_t: absolute range of temperature in a cast in degrees Celsius.
#' \item range_s: absolute range of salinity in a cast in PSS-78 salinity.}
#' @export


run_ctm_adjust_path <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                  full.names = TRUE, pattern = "downcast.cnv"),
                                                       list.files(here::here("output", "sbe19plus_v0"), 
                                                                  full.names = TRUE, pattern = "upcast.cnv"))),
                                min_pressure = 4,
                                optim_method = "SANN",
                                optim_maxit = 500,
                                start_alpha = 0.04,
                                start_tau = 8,
                                ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  cast_direction <- character(length = length(deploy_id))
  cast_direction[grepl(pattern = "downcast", x = profile_files)] <- "downcast"
  cast_direction[grepl(pattern = "upcast", x = profile_files)] <- "upcast"
  
  default_obj <- numeric(length = length(profile_files))
  adj_obj <- numeric(length = length(profile_files))
  par_alpha <- numeric(length = length(profile_files))
  par_tau <- numeric(length = length(profile_files))
  mean_t_rate <- numeric(length = length(profile_files))
  mean_s_rate <- numeric(length = length(profile_files))
  max_t_rate <- numeric(length = length(profile_files))
  max_s_rate <- numeric(length = length(profile_files))
  range_t <- numeric(length = length(profile_files))
  range_s <- numeric(length = length(profile_files))
  
  
  for(ii in 1:length(profile_files)) { 
    
    message(profile_files[ii])
    
    profile_dat <- try(oce::read.ctd(file = profile_files[ii]), silent = TRUE)
    if(class(profile_dat) == "try-error") next
    
    temperature <- profile_dat@data$temperature[profile_dat@data$pressure > min_pressure]
    flag <- profile_dat@data$flag[profile_dat@data$pressure > min_pressure]
    
    if(any(length(temperature) < 20, sum(!is.na(flag)) < 20)) {
      par_alpha[ii] <- start_alpha
      par_tau[ii] <- start_tau
      next
    }
    
    pressure <- profile_dat@data$pressure[profile_dat@data$pressure > min_pressure]
    conductivity <- profile_dat@data$conductivity[profile_dat@data$pressure > min_pressure]
    salinity <- profile_dat@data$salinity[profile_dat@data$pressure > min_pressure]
    
    default_dat <- gapctd::ctm_adjust_path(alpha = start_alpha,
                                           tau = start_tau,
                                           f_n = 0.25,
                                           temperature = temperature,
                                           pressure = pressure,
                                           conductivity = conductivity,
                                           flag = flag,
                                           obj_fn = "none")
    
    default_obj[ii] <- gapctd::ctm_adjust_path(alpha = start_alpha,
                                               tau = start_tau,
                                               f_n = 0.25,
                                               temperature = temperature,
                                               pressure = pressure,
                                               conductivity = conductivity,
                                               flag = flag,
                                               obj_fn = "path")
    
    est_pars <- try(bbmle::mle2(minuslogl = gapctd::ctm_adjust_path,
                                start = list(alpha = start_alpha,
                                             tau = start_tau),
                                data = list(temperature = temperature,
                                            pressure = pressure,
                                            conductivity = conductivity,
                                            flag = flag,
                                            obj_fn = "path",
                                            f_n = 0.25),
                                method = optim_method,
                                control = list(maxit = optim_maxit, reltol = 1e-5,
                                               parscale = c(alpha = 0.01, tau = 1))), silent = TRUE)
    conv <- try(est_pars@details$convergence, silent = TRUE)
    
    if(any(class(est_pars) == "try-error", conv != 0)) {
      est_pars <- try(bbmle::mle2(minuslogl = gapctd::ctm_adjust_path,
                                  start = list(alpha = 0.1,
                                               tau = 12),
                                  data = list(temperature = temperature,
                                              pressure = pressure,
                                              conductivity = conductivity,
                                              flag = flag,
                                              obj_fn = "path",
                                              f_n = 0.25),
                                  method = optim_method,
                                  control = list(maxit = optim_maxit, reltol = 1e-5,
                                                 parscale = c(alpha = 0.1, tau = 1))), silent = TRUE)
      conv <- try(est_pars@details$convergence, silent = TRUE)
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      adj_dat <- gapctd::ctm_adjust_path(alpha = est_pars@coef['alpha'],
                                         tau = est_pars@coef['tau'],
                                         f_n = 0.25,
                                         temperature = temperature,
                                         pressure = pressure,
                                         conductivity = conductivity,
                                         flag = flag,
                                         obj_fn = "none")
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      
      if(est_pars@min < default_obj[ii]) {
        par_alpha[ii] <- as.numeric(est_pars@coef['alpha'])
        par_tau[ii] <- as.numeric(est_pars@coef['tau'])
        adj_obj[ii] <- est_pars@min
      } else {
        adj_obj[ii] <- default_obj[ii]
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
      }
      
      max_t_rate[ii] <- max(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      mean_t_rate[ii] <- mean(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      range_t[ii] <- diff(range(default_dat$temperature, na.rm = TRUE))
      
      max_s_rate[ii] <- max(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      mean_s_rate[ii] <- mean(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      range_s[ii] <- diff(range(default_dat$temperature, na.rm = TRUE))
      
    } else {
      par_alpha[ii] <- NA
      par_tau[ii] <- NA
    }
    
  }
  
  out_df <- data.frame(deploy_id = deploy_id,
                       cast_direction = cast_direction,
                       default_obj = default_obj,
                       adj_obj = adj_obj,
                       alpha = round(par_alpha, 5),
                       tau = round(par_tau, 5),
                       mean_t_rate = mean_t_rate,
                       mean_s_rate = mean_s_rate,
                       max_t_rate = max_t_rate,
                       max_s_rate = max_s_rate,
                       range_t = range_t,
                       range_s = range_s)
  
  return(out_df)
  
}



#' Retrieve haul event times from RACEBASE (SBEDP workflow)
#' 
#' Retrieves on bottom, haul back, off bottom, and marked event times from RACE_DATA.
#' 
#' @param channel An RODBC connection to Oracle.
#' @param haul_metadata_path Character vector. Path to haul data .csv generated by create_NMEA().
#' @param append_haul_metadata Logical. Should the file named in haul_metadata_path be appended (overwritten) to include haul events?
#' @export

get_haul_events <- function(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), full.names = TRUE),
                            append_haul_metadata = TRUE,
                            channel) {
  
  if(length(haul_metadata_path) != 1) {
    stop(paste("get_haul_events(): Argument haul_meta_data path must only be file but has length ", length(haul_metadata_path)))
  }
  
  haul_metadata <- read.csv(file = haul_metadata_path, stringsAsFactors = FALSE)
  
  unique_vessel_cruise <- haul_metadata |>
    dplyr::select(VESSEL, CRUISE) |>
    unique()
  
  events_df <- data.frame()
  for(i in 1:nrow(unique_vessel_cruise)) {
    
    events_df <- 
      RODBC::sqlQuery(channel = channel, 
                      query = paste0("select a.vessel_id vessel, 
                        a.cruise cruise, 
                         b.haul haul,
                         c.edit_date_time date_time, 
                         c.event_type_id time_flag
                         from 
                         race_data.cruises a,
                         race_data.hauls b,
                         race_data.events c,
                         race_data.datum_codes d
                         where 
                         a.cruise = ", unique_vessel_cruise$CRUISE[i],
                                     " and a.vessel_id = ", unique_vessel_cruise$VESSEL[i],
                                     " and b.cruise_id = a.cruise_id
                          and c.haul_id = b.haul_id
                          and c.event_type_id in (3,6,7)
                          and c.datum_code = d.datum_code
                          and d.use_in_analysis = 'Y'")) |>
      dplyr::inner_join(data.frame(TIME_FLAG = c(3,6,7),
                                   EVENT_NAME = c("ON_BOTTOM", "HAULBACK", "OFF_BOTTOM"))) |>
      tidyr::pivot_wider(id_cols = c("VESSEL", "CRUISE", "HAUL"),
                         names_from = c("EVENT_NAME"),
                         values_from = "DATE_TIME") |>
      dplyr::bind_rows(events_df)
  }
  
  events_df <- haul_metadata |> 
    dplyr::inner_join(events_df)
  
  if(nrow(haul_metadata) != nrow(events_df)) {
    warning(print("get_haul_events(): Length of haul metadata input (", nrow(haul_metadata), ") differs from length of output with haul events (", nrow(events_df) , ")"))
  }
  
  if(append_haul_metadata) {
    write.csv(x = events_df, 
              file = haul_metadata_path, 
              row.names = FALSE)
  }
  
  return(events_df)
}



#' Flag density inversion and correct (SBEDP workflow)
#' 
#' Screen profiles for density inversions and flag based on a threshold criteria based on density (kg m^-3) or Brunt-Vaisala buoyancy frequency (N^2), s^-2. Either  Optional: remove missing value and interpolate.
#' 
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses. GTSPP uses a density threshold of 0.05.
#' @param threshold_method Character. "density" to flag inversions based on density anomalies, "bv" for Brunt-Vaisala buoyancy frequency
#' @param correct_inversion Logical. Should density inversions be removed and interpolated?
#' @param cnv_dir File path to cnv directory.
#' @return Writes flagged and corrected data to csvs in output/density_corrected.
#' @export

correct_density_inversion <- function(threshold  = NULL, threshold_method = "bv", correct_inversion = TRUE, cnv_dir = NULL, ...) {
  
  threshold_method <- tolower(threshold_method)
  
  # Check that inputs are sensible
  if(sign(threshold) == 1) {
    stop("correct_density_inversion: Max inversion must be positive but is set to ", threshold, "!")
  }
  
  if(threshold_method == "bv" & threshold < -1e-4) {
    warning(paste0("correct_density_inversion: Brunt-Vaisala (N^2) threshold, (", threshold, ") is unusually low. Consider using a higher value (e.g. -1e-5)."))
  }
  
  if(threshold_method == "density" & threshold > -0.01) {
    warning(paste0("correct_density_inversion: Density threshold, (", threshold, ") is unusually high. Consider using a lower value (e.g., -0.03)."))
  }
  
  # List final cnv_files
  if(is.null(cnv_dir)) {
    cnv_dir <- here::here("final_cnv")
  } else {
    
  }
  
  cnv_path <- list.files(cnv_dir, full.names = TRUE, ...)
  
  if(!(length(cnv_path) >= 1)) {
    stop(paste0("correct_density_inversion: No files found in ", cnv_dir))
  }
  
  deploy <- list.files(cnv_dir, ...)
  
  deploy_id <- sub("\\_raw.*", "", deploy)
  deploy_id[grepl(pattern = "downcast", x = deploy)] <- paste0(deploy_id, "_downcast")[grepl(pattern = "downcast", x = deploy)] 
  deploy_id[grepl(pattern = "upcast", x = deploy)] <- paste0(deploy_id, "_upcast")[grepl(pattern = "upcast", x = deploy)] 
  
  for(jj in 1:length(cnv_path)) {
    
    profile_oce <- try(oce::read.ctd(file = cnv_path[jj]), silent = TRUE)
    
    if(class(profile_oce) == "try-error") next
    
    # Calculate N2
    profile_oce@data$N2 <- oce::swN2(profile_oce)
    
    
    # Flag based on threshold method
    flag_density <- switch(threshold_method,
                           `bv` = {which(profile_oce@data$N2 < threshold)},
                           `density` = {which(diff(profile_oce@data$gsw_densityA0[rank(profile_oce@data$pressure)]) < threshold)})
    
    # Counter to break the loop if automatic corrections get stuck
    flag_loop <- 1
    
    while(length(flag_density) > 0 & flag_loop < 3) {
      
      if(threshold_method == "density") {
        profile_oce@data$flag[rank(profile_oce@data$pressure)][flag_density+1] <- -1*(correct_inversion *-1)
      } else if(threshold_method == "bv") {
        # Flag but don't interpolate N2 anomalies at pressures < 20 db
        profile_oce@data$flag[flag_density] <- 6*(correct_inversion)
        profile_oce@data$flag[profile_oce@data$flag == 6*(correct_inversion) & profile_oce@data$pressure < 20] <- -6*(correct_inversion)
      }
      
      flags <- which(profile_oce@data$flag == 6*(correct_inversion))
      
      
      if(correct_inversion & length(flags) > 0) {
        
        profile_oce@data$temperature[flags] <- oce::oce.approx(x = profile_oce@data$depth[-flags], 
                                                               y = profile_oce@data$temperature[-flags], 
                                                               xout = profile_oce@data$depth[flags], 
                                                               method = "unesco")
        profile_oce@data$conductivity[flags] <- oce::oce.approx(x = profile_oce@data$depth[-flags], 
                                                                y = profile_oce@data$conductivity[-flags], 
                                                                xout = profile_oce@data$depth[flags], 
                                                                method = "unesco")
        profile_oce@data$salinity[flags] <- oce::oce.approx(x = profile_oce@data$depth[-flags], 
                                                            y = profile_oce@data$salinity[-flags], 
                                                            xout = profile_oce@data$depth[flags], 
                                                            method = "unesco")
        profile_oce@data$soundSpeed[flags] <- oce::oce.approx(x = profile_oce@data$depth[-flags], 
                                                              y = profile_oce@data$soundSpeed[-flags], 
                                                              xout = profile_oce@data$depth[flags], 
                                                              method = "unesco")
        profile_oce@data$gsw_saA0[flags] <- oce::oce.approx(x = profile_oce@data$depth[-flags], 
                                                            y = profile_oce@data$gsw_saA0[-flags], 
                                                            xout = profile_oce@data$depth[flags], 
                                                            method = "unesco")
        profile_oce@data$gsw_densityA0[flags] <- oce::oce.approx(x = profile_oce@data$depth[-flags], 
                                                                 y = profile_oce@data$gsw_densityA0[-flags], 
                                                                 xout = profile_oce@data$depth[flags], 
                                                                 method = "unesco")
        
        # Re-calculate N2
        profile_oce@data$N2 <- oce::swN2(profile_oce)
        
        # Re-flag
        flag_density <- switch(threshold_method,
                               `bv` = {which(profile_oce@data$N2 < threshold & profile_oce@data$pressure > 20)},
                               `density` = {which(diff(profile_oce@data$gsw_densityA0[rank(profile_oce@data$pressure)]) < threshold)})
      } else {
        flag_density <- 0
      }
      
      flag_loop <- flag_loop + 1
    }
    
    # Write corrected profile to output/density_corrected
    oce::write.ctd(object = profile_oce, file = here::here("output", "density_corrected", paste0(deploy_id[jj], ".csv")), metadata = FALSE, format = "csv")
    
    # Remove NAs
    new_csv <- read.csv(file = here::here("output", "density_corrected", paste0(deploy_id[jj], ".csv")))
    new_csv$deploy_id <- deploy_id[jj]
    write.csv(x = new_csv[complete.cases(new_csv),], 
              file = here::here("output", "density_corrected", paste0(deploy_id[jj], ".csv")), 
              row.names = FALSE)
    
  }
}



#' Manually flag and interpolate bad data (SBEDP workflow)
#' 
#' Visually inspect plots of depth versus salinity, and depth versus pressure. Flag data for removal in a GUI and interpolate missing data. Write interpolated data to output/manual_flag
#' 
#' @param csv_paths Path to csv files to be flagged and reviewed.
#' @export

manual_flag_interpolate <- function(csv_paths = NULL, review = c("density")) {
  
  if(is.null(csv_paths)) {
    csv_paths <- list.files(here::here("output", "density_corrected"), full.names = TRUE)
  }
  
  interp_flags <- function(x, flags) {
    if(length(flags) > 0) {
      x$temperature[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                              y = x$temperature[-flags], 
                                              xout = x$depth[flags], 
                                              method = "unesco")
      x$conductivity[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                               y = x$conductivity[-flags], 
                                               xout = x$depth[flags], 
                                               method = "unesco")
      x$salinity[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                           y = x$salinity[-flags], 
                                           xout = x$depth[flags], 
                                           method = "unesco")
      x$soundSpeed[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                             y = x$soundSpeed[-flags], 
                                             xout = x$depth[flags], 
                                             method = "unesco")
      x$gsw_saA0[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                           y = x$gsw_saA0[-flags], 
                                           xout = x$depth[flags], 
                                           method = "unesco")
      x$gsw_densityA0[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                                y = x$gsw_densityA0[-flags], 
                                                xout = x$depth[flags], 
                                                method = "unesco")
      ctd_obj <- as.ctd(salinity = x$salinity,
                        temperature = x$temperature,
                        pressure = x$pressure,
                        conductivity = x$conductivity)
      
      x$N2 <- oce::swN2(ctd_obj)
    }
    return(x)
  }
  
  for(kk in 1:length(csv_paths)) {
    
    dat <- read.csv(file = csv_paths[kk])
    
    dat <- dat |>
      dplyr::arrange(pressure)
    
    if(!file.exists(here::here("output", "manual_flag", paste0(dat$deploy_id[1], ".csv")))) {
      
      message(dat$deploy_id[1])
      
      if("density" %in% review) {
        
        loop_ind <- 1
        while(loop_ind == 1) {
          
          par(mfrow = c(1,3))
          plot(dat$temperature, -1*dat$pressure, 
               xlab = "Temperature", 
               ylab = "Pressure",
               main = " ",
               col = "red")
          abline(h = -1, lty = 2)
          lines(x = dat$temperature, 
                y = -1*dat$pressure,
                col = "red")
          plot(dat$salinity, -1*dat$pressure, 
               xlab = "Salinity", 
               ylab = "Pressure",
               main = " ",
               col = "darkgreen")
          abline(h = -1, lty = 2)
          lines(x = dat$salinity, 
                y = -1*dat$pressure,
                col = "darkgreen")
          plot(x = dat$gsw_densityA0, 
               y = -1*dat$pressure, 
               xlab = "Density", 
               ylab = "Pressure",
               main = "Left-click on points to be removed then press 'Esc'",
               col = "blue")
          abline(h = -1, lty = 2)
          lines(x = dat$gsw_densityA0, 
                y = -1*dat$pressure,
                col = "blue")
          
          new_flags <- identify(dat$gsw_densityA0, -1*dat$pressure)
          
          
          if(!(length(new_flags) > 0)) {
            loop_ind <- 0
            next
          }
          
          dat$flag[new_flags] <- 7
          dat <- interp_flags(x = dat, flags = new_flags)
        }
      }
      
      if("temperature" %in% review) {
        
        loop_ind <- 1
        while(loop_ind == 1) {
          
          par(mfrow = c(1,2))
          plot(diff(dat$temperature)/diff(dat$pressure),
               y = (nrow(dat)-1):1,
               type = 'p',
               xlab = expression(d*"T"/d*rho),
               ylab = "Index")
          abline(h = -1, lty = 2)
          lines(diff(dat$temperature)/diff(dat$pressure),
                y = (nrow(dat)-1):1)
          plot(dat$temperature, -1*dat$pressure,
               xlab = "Temperature",
               ylab = "Pressure",
               main = "Left-click on points to be removed then press 'Esc'")
          abline(h = -1, lty = 2)
          lines(x = dat$temperature, y = -1*dat$pressure)
          
          new_flags <- identify(dat$temperature, -1*dat$pressure)
          
          
          if(!(length(new_flags) > 0)) {
            loop_ind <- 0
            next
          }
          
          dat$flag[new_flags] <- 7
          dat <- interp_flags(x = dat, flags = new_flags)
        }
      }
      
      if("salinity" %in% review) {
        loop_ind <- 1
        while(loop_ind == 1) {
          
          par(mfrow = c(1,2))
          plot(diff(dat$salinity)/diff(dat$pressure),
               y = (nrow(dat)-1):1,
               type = 'p',
               xlab = expression(d*S/d*rho),
               ylab = "Index")
          abline(h = -1, lty = 2)
          lines(diff(dat$salinity)/diff(dat$pressure),
                y = (nrow(dat)-1):1,)
          plot(dat$salinity, -1*dat$pressure,
               xlab = "Salinity",
               ylab = "Pressure",
               main = "Left-click on points to be removed then press 'Esc'")
          abline(h = -1, lty = 2)
          lines(x = dat$salinity, y = -1*dat$pressure)
          
          new_flags <- identify(dat$salinity, -1*dat$pressure)
          
          if(!(length(new_flags) > 0)) {
            loop_ind <- 0
            next
          }
          
          dat$flag[new_flags] <- 7
          dat <- interp_flags(x = dat, flags = new_flags)
          
        }
      }
      
      write.csv(x = dat, 
                file = here::here("output", "manual_flag", paste0(dat$deploy_id[1], ".csv")), 
                row.names = FALSE)
      
      review_next <- tolower(readline(prompt = "Review next? (y or n): ")) == "y"
      print(review_next)
      
      if(!review_next) {
        stop("Review stopped")
      }
    } else {
      message(paste0("Skipping ", dat$deploy_id[1]))
    }
  }
}



#' Visually inspect profiles (SBEDP workflow)
#' 
#' Visually inspect profile data to evaluate whether they are acceptable. Profiles that are acceptable will be copied to a review directory. Unacceptable profiles will be removed from the directory and may need to be re-evaluated using manual_flag_review() or other remedial measures. This function skips profiles that have already been reviewed and accepted.
#' 
#' @param csv_paths Path to csv files. If NULL, the function uses all csv files in output/manual_flag/
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses. GTSPP uses a density threshold of 0.05.
#' @export

manual_review <- function(csv_paths = NULL, threshold = -1e-5) {
  
  if(is.null(csv_paths)) {
    csv_paths <- list.files(here::here("output", "manual_flag"), full.names = TRUE)
  }
  
  for(ii in 1:length(csv_paths)) {
    
    sel_profile <- read.csv(file = csv_paths[ii])
    
    if(min(sel_profile$depth) > 1) {
      message(paste0("Skipping ", sel_profile$deploy_id[ii]))
      next
    }
    
    if(!file.exists(here::here("output", "accepted_profiles", paste0(sel_profile$deploy_id[1], ".csv")))) {
      
      profile_oce <- oce::as.ctd(salinity = sel_profile$salinity,
                                 temperature = sel_profile$temperature,
                                 pressure = sel_profile$pressure,
                                 conductivity = sel_profile$conductivity)
      message(sel_profile$deploy_id[1])
      par(mfrow = c(1,2))
      oce::plot(profile_oce, which = 1, type = 'l')
      oce::plot(profile_oce, which = 2, type = 'l')
      abline(v = threshold, lwd = 3, col = "brown")
      abline(h = 1, lwd = 3, col = "black", lty = 2)
      
      accept <- tolower(readline(prompt = "Accept profile? (y or n): ")) == "y"
      
      if(accept) {
        file.copy(from = csv_paths[ii], 
                  to = here::here("output", "accepted_profiles", paste0(sel_profile$deploy_id[1], ".csv")))
      } else {
        message(paste0("Removing ", csv_paths[ii], ". Rerun manual_flag_review() if manual adjustment should be made to this profile. Otherwise the profile will be exluded from the output."))
        file.remove(csv_paths[ii])
      }
      
    } else {
      message(paste0("Skipping ", sel_profile$deploy_id[ii]))
    }
    
  }
  
}



#' Calculate variable averages from accepted profiles, by depth/pressure bin (SBEDP workflow)
#' 
#' Calculate temperature from the 1 db pressure bin.
#' 
#' @param accepted_dir File path to folder with data from accepted profiles.
#' @param z_var Name of the z variable ("pressure" or "depth").
#' @param depth_range Range of depth/pressure bins to use, as a 1L or 2L numeric vector.
#' @param agg_fun Function to use.
#' @export

calc_bin_summary <- function(accepted_dir = here::here("output", "accepted_profiles"),
                             z_var = "pressure",
                             z_range = 1,
                             agg_fun = mean) {
  
  f_list <- list.files(accepted_dir, full.names = TRUE, pattern = "accepted.csv")
  
  if(!(length(f_list) > 1)) {
    stop(paste("calc_surface_mean(): No files found in accepted_dir!"))
  }
  
  agg_df <- data.frame()
  
  for(jj in 1:length(accepted_dir)) {
    # Load file
    sel_dat <- read.csv(file = f_list[jj], stringsAsFactors = FALSE)
    names(sel_dat)[names(sel_dat) == z_var] <- "z_col"
    
    sel_dat <- dplyr::filter(sel_dat, z_col <= max(z_range), z_col >= min(z_range))
    
    agg_df <- dplyr::bind_rows(agg_df, sel_dat)
    
  }
  
  var_names <- names(agg_df)[!(names(agg_df) %in% c("z_col", "file"))]
  
  out_df <- agg_df %>%
    dplyr::group_by(file) %>%
    dplyr::summarise(dplyr::across(var_names, agg_fun))
  
  out_df$z_range <- paste(1, collapse = ",")
  
  return(out_df)
  
}



#' Package final files and metadata in a zip file (SBEDP workflow)
#' 
#' @param region Survey region
#' @param vessel Survey vessel
#' @param year Survey year
#' @export

make_ctd_zip <- function(region, vessel, year) {
  
  xmlcon_text <- readLines(paste0("./psa_xmlcon/", list.files("./psa_xmlcon/", pattern = ".xmlcon")))
  
  serial_number <- xmlcon_text[which(grepl(pattern = "<SerialNumber>", xmlcon_text))][1]
  serial_number <- gsub(pattern = ".*<SerialNumber>", "", serial_number)
  serial_number <- gsub(pattern = "</SerialNumber>.*", "", serial_number)
  
  zip_name <- paste0("CTD_", year, "_", region, "_", vessel, "_", serial_number, ".zip")
  
  message(paste0("Writing CTD data to ", zip_name))
  utils::zip(zipfile = zip_name, files = "./metadata/")
  utils::zip(zipfile = zip_name, files = "./final_cnv/")
  utils::zip(zipfile = zip_name, files = "./data/")
  utils::zip(zipfile = zip_name, files = paste0("./output/", list.files("./output/", pattern = ".rds")))
  utils::zip(zipfile = zip_name, files = "./output/accepted_profiles/")
  utils::zip(zipfile = zip_name, files = paste0("./psa_xmlcon/", list.files("./psa_xmlcon/", pattern = ".xmlcon")))
  
}



#' Change AlignCTD parameters (SBEDP workflow)
#' 
#' Change SBE Data Processing alignment module parameters in a .psa file.
#' 
#' @param var Character vector indicating which variable channel should be aligned ("temperature", "conductivity", or "oxygen")
#' @param alignment Adjustment to channel alignment relative to pressure, in seconds. The 'typical' alignment for a pumped SBE19plus is 0.5 for temperature.
#' @param psa_path File path to the alignctd psa file. If not provided, attempts to use /psa_xmlcon/alignctd.psa.
#' @details This funtion modifies text in psa files that are used to run the AlignCTD module in SBE Data Processing. 
#' @export

change_psa_alignctd <- function(variable, alignment, psa_path = NULL) {
  
  variable <- tolower(variable)
  
  if(is.null(psa_path)) {
    psa_path <- here::here("psa_xmlcon", "alignctd.psa")
  }
  
  if(variable == "temperature") {
    align_psa_file <- readLines(psa_path)
    align_index <- which(grepl(pattern = "variable_name=\"Temperature\" />", align_psa_file))
  }
  
  if(variable == "conductivity") {
    align_psa_file <- readLines(psa_path)
    align_index <- which(grepl(pattern = "variable_name=\"Conductivity\" />", align_psa_file))
  }
  
  if(variable == "oxygen") {
    align_psa_file <- readLines(psa_path)
    align_index <- which(grepl(pattern = "variable_name=\"Oxygen\" />", align_psa_file))
  }
  
  string1 <- paste0(gsub(pattern = "\\value.*", "", align_psa_file[align_index]), "value")
  string2 <- paste0("=\"", format(alignment, nsmall = 6),  "\" ")
  string3 <- paste0("variable_name", gsub(pattern = ".*variable_name", "", align_psa_file[align_index]))
  new_string <- paste0(string1, string2, string3)
  align_psa_file[align_index] <- new_string
  psa_con <- file(psa_path)
  writeLines(text = align_psa_file, con = psa_con)
  close(psa_con)
  
}

#' Change CellTM parameters (SBEDP workflow)
#' 
#' Change SBE Data Processing conductivity cell thermal inertia correction module parameters in a .psa file.
#' 
#' @param alpha Alpha parameter. 1L numeric vector.
#' @param tau_ctm Tau parameter (i.e., inv_beta) 1L numeric vector.
#' @param psa_path File path to cell thermal mass .psa file.
#' @details This funtion modifies text in psa files that are used to run the CellTM module in SBE Data Processing. 
#' @export

change_psa_celltm <- function(alpha, tau_ctm, psa_path = here::here("psa_xmlcon", "celltm.psa")) {
  
  celltm_psa_file <- readLines(psa_path)
  
  celltm_index <- which(grepl(pattern = "    <TA_Amplitude", celltm_psa_file))
  celltm_psa_file[celltm_index] <- paste0("    <TA_Amplitude value=\"", format(alpha, nsmall = 6), "\" />")
  
  celltm_index <- which(grepl(pattern = "    <TA_TimeConstant", celltm_psa_file))
  celltm_psa_file[celltm_index] <- paste0("    <TA_TimeConstant value=\"", format(tau_ctm, nsmall = 6), "\" />")
  
  psa_con <- file(psa_path)
  writeLines(text = celltm_psa_file, con = psa_con)
  close(psa_con)
  
}



#' Make sections from upcasts and downcasts (SBEDP workflow)
#' 
#' Split cnv file data into upcast and downcast files based on haul event times using the SBEDataProcessing 'section' module. This is used because SBEDataProcessing split methods result in profiles that include samples while the trawl was being towed along the bottom, which results in sampling artifacts in the bottom depth bin.
#' 
#' @param haul_metadata_path Character vector. Path to haul data .csv generated by get_bottom_data().
#' @param pattern Character vector. Suffix of cnv file to search for. Default = "TEOS10.cnv"
#' @param xmlcon_file xmlcon file
#' @param section_bat section batch file
#' @param pressure_bat pressure batch file
#' @param binavg_bat binavg batch file
#' @export

make_cast_sections <- function(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), full.names = TRUE),
                               pattern = "TEOS10.cnv", 
                               xmlcon_file = NA,
                               section_bat = NA,
                               pressure_bat = NA,
                               binavg_bat = NA) {
  
  haul_metadata <- read.csv(file = haul_metadata_path, 
                            stringsAsFactors = FALSE)
  append_char <- c("_downcast", "_upcast")
  
  # Setup xmlcon file
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
  
  # Setup batch files
  if(is.na(section_bat)) {
    print("Automatically selecting section.bat file (no user-specified argument to section_bat)")
    section_bat <- list.files(pattern = "section.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", section_bat))) {
      stop(paste0(section_bat, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  if(is.na(pressure_bat)) {
    print("Automatically selecting pressure.bat file (no user-specified argument to pressure_bat)")
    pressure_bat <- list.files(pattern = "pressure.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", pressure_bat))) {
      stop(paste0(pressure_bat, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  if(is.na(binavg_bat)) {
    print("Automatically selecting binavg.bat file (no user-specified argument to binavg_bat)")
    binavg_bat <- list.files(pattern = "bin.bat")
    
    if(length(xmlcon_file) < 1) {
      stop(paste0("No .bat file found in ", getwd(), ". Must have a valid .bat file."))
    } 
  } else {
    if(!file.exists(paste0(getwd(), "/", bin_bat))) {
      stop(paste0(binavg_bat, " file not found in ", getwd(), ". Must have a valid .bat file."))
    }
  }
  
  cnv_files <- list.files(here::here("cnv"), pattern = pattern, 
                          full.names = TRUE)
  
  deploy_id <- sub("\\_raw.*", "", list.files(here::here("cnv"), pattern = pattern))
  
  for(ii in 1:length(cnv_files)) {
    
    if(ii%%50 == 0) {
      message(paste0("Splitting upcast and downcast from file ", ii, " out of ", length(cnv_files)))
    }
    
    for(jj in 1:2) {
      
      oce_dat <- try(oce::read.ctd(cnv_files[ii]))
      
      ind_vec <- switch(jj,
                        `1` = { c(0, max(which(oce_dat@data$timeS < (haul_metadata$DOWNCAST_END_SECONDS[which(deploy_id[ii] == sub("\\_raw.*", "", haul_metadata$cnv_file_name))] + 30))))}, 
                        `2` = {c(min(which(oce_dat@data$timeS > (haul_metadata$UPCAST_START_SECONDS[which(deploy_id[ii] == sub("\\_raw.*", "", haul_metadata$cnv_file_name))] - 30))), 100000)})
      
      section_path <- here::here("psa_xmlcon", "Section.psa")
      section_psa_file <- readLines(section_path)
      
      append_index <- which(grepl(pattern = "  <NameAppend", section_psa_file))
      min_val_index <- which(grepl(pattern = "<MinValue", section_psa_file))
      max_val_index <- which(grepl(pattern = "<MaxValue", section_psa_file))
      
      
      
      if(!any(is.na(ind_vec))) {
        section_psa_file[append_index] <- paste0("  <NameAppend value=\"_", append_char[jj],  "\" />")
        section_psa_file[min_val_index] <- paste0("  <MinValue value=\"",ind_vec[1], ".000000","\" />")
        section_psa_file[max_val_index] <- paste0("  <MaxValue value=\"",ind_vec[2], ".000000","\" />")
        
        psa_con <- file(section_path)
        writeLines(text = section_psa_file, con = psa_con)
        
        close(psa_con)
        
        # Run section function
        cmd <- paste0("sbebatch ", getwd(), "/", section_bat, " ",  
                      getwd(), " ", #%1
                      xmlcon_file, " ", #%2
                      cnv_files[ii], " ", #%3
                      section_path, " ", #%4
                      append_char[jj], " #m") #%5
        system(cmd)
      }
    }
  }
  
  # Remove surface values (pressure < 0.5) from cnv downcast files
  pressure_path <- here::here("psa_xmlcon", "Section_Pressure.psa")
  pressure_psa_file <- readLines(pressure_path)
  
  pressure_index <- which(grepl(pattern = "  <PressureCast ", pressure_psa_file))
  pressure_psa_file[pressure_index] <- "  <PressureCast value=\"0\" high=\"1\" low=\"0\" initialValue=\"0\" />" 
  psa_con <- file(pressure_path)
  writeLines(text = pressure_psa_file, con = psa_con)
  close(psa_con)
  
  system(paste0("sbebatch ", getwd(), "/", pressure_bat, " ", 
                getwd(), " ", #%1
                xmlcon_file, " ", #%2
                pressure_path, " ",
                paste0(append_char[1],".cnv"))) #%3
  
  # Remove surface values (pressure < 0.5) from cnv upcast files 
  pressure_psa_file <- readLines(pressure_path)
  
  pressure_index <- which(grepl(pattern = "  <PressureCast ", pressure_psa_file))
  pressure_psa_file[pressure_index] <- "  <PressureCast value=\"1\" high=\"1\" low=\"0\" initialValue=\"0\" />" 
  
  psa_con <- file(pressure_path)
  writeLines(text = pressure_psa_file, con = psa_con)
  close(psa_con)
  
  system(paste0("sbebatch ", getwd(), "/", pressure_bat, " ", 
                getwd(), " ", #%1
                xmlcon_file, " ", #%2
                pressure_path, " ",  #%3
                paste0(append_char[2],".cnv") #%4
  ))
  
  # Remove downcast files from upcast alignments and upcast files from downcast alignments
  print("Binning upcast and downcast data")
  system(command = paste0("sbebatch ", getwd(), "/", binavg_bat, " ", getwd(), " ", xmlcon_file))
  
  if(length(list.files(path = here::here("cnv"), pattern = "_uuu_", full.names = TRUE)) >= 1){
    upcast_files <- list.files(path = here::here("cnv"), pattern = "_uuu_", full.names = TRUE)
    file.remove(upcast_files[grep(pattern = "downcast", x = upcast_files)])
  }
  
  if(length(list.files(path = here::here("cnv"), pattern = "_ddd_", full.names = TRUE)) >= 1){
    downcast_files <- list.files(path = here::here("cnv"), pattern = "_ddd_", full.names = TRUE)
    file.remove(downcast_files[grep(pattern = "upcast", x = downcast_files)])
  }
  
}



#' Function to move final .cnv files to bad_cnv and final_cnv directories (SBEDP workflow)
#' 
#' Moves binned .cnv files without data from the cnv folder to bad_cnv, and files with data to final_cnv.
#' 
#' @export

move_binned_cnv <- function() {
  bin_files <- list.files(path = paste0(getwd(), "/cnv/"), 
                          pattern = "_binavg", 
                          full.names = FALSE, 
                          include.dirs = TRUE)
  
  # Output data frame ----
  out_df <- data.frame(hex_file = character(length = length(bin_files)),
                       hex_id = character(length = length(bin_files)),
                       nvalues = numeric(length = length(bin_files)),
                       cnv_file = character(length = length(bin_files)))
  
  for(ii in 1:length(bin_files)) {
    
    # Find number of binned values in each cast ----
    sel_cnv <- read.delim(paste0(getwd(), "/cnv/", bin_files[ii]))
    nval_row <- sel_cnv[grepl("nvalues", sel_cnv[,1]),]
    nval_row <- gsub("[[:space:]]", "", nval_row) 
    nvals <- as.numeric(strsplit(nval_row, "=")[[1]][2])
    
    # Name of original .hex file ----
    hex_file <- sel_cnv[1,]
    hex_file <- gsub("[[:space:]]", "", hex_file) 
    hex_file <- strsplit(hex_file, "\\\\")
    hex_id <- hex_file[[1]][length(hex_file[[1]])] 
    hex_id <- strsplit(hex_id, ".hex")[[1]]
    
    hex_file <- paste0(hex_file[[1]][length(hex_file[[1]])-1], "\\", hex_file[[1]][length(hex_file[[1]])])
    
    out_df$hex_file[ii] <- hex_file
    out_df$hex_id[ii] <- hex_id
    out_df$nvalues[ii] <- nvals
    out_df$cnv_file[ii] <- bin_files[ii]
    
  }
  
  # Save csv with number of cast values ----
  write.csv(out_df, file = paste0(getwd(), "/output/cast_nvalues.csv"), row.names = FALSE)
  
  # Find and move cnv files for all casts with no values ----
  print("Moving bad cnv files")
  no_data_df <- subset(out_df, nvalues < 1)
  
  if(nrow(no_data_df) > 0) {
    for(ii in 1:nrow(no_data_df)) {
      file.rename(from = paste0(getwd(), "/cnv/", no_data_df$cnv_file[ii]), 
                  to = paste0(getwd(), "/bad_cnv/", no_data_df$cnv_file[ii]))
    }
  }
  
  # Find and move cnv files for all casts with no values ----
  print("Moving good cnv files")
  good_cnv_df <- subset(out_df, nvalues > 0)
  if(nrow(good_cnv_df) > 0) {
    for(ii in 1:nrow(good_cnv_df )) {
      file.rename(from = paste0(getwd(), "/cnv/", good_cnv_df $cnv_file[ii]), 
                  to = paste0(getwd(), "/final_cnv/", good_cnv_df $cnv_file[ii]))
    }
  }
}



#' Move best method files to final_cnv (SBEDP workflow)
#' 
#' Move best method files based on spikiness of profiles.
#' 
#' @param best_df Output of gapctd::compare_methods()
#' @export

move_to_final_cnv <- function(best_df) {
  best_df$new_loc <- NA
  
  for(ii in 1:nrow(best_df)) {
    best_df$new_loc[ii] <- here::here("final_cnv",  paste0(best_df$deploy[ii], gsub(paste0(".*", best_df$deploy[ii]), "", best_df$move[ii])))
  }
  
  file.copy(from = best_df$move, to = best_df$new_loc, overwrite = TRUE)
}



#' Filter out bad bottom and surface data and cnvs (SBEDP workflow)
#' 
#' Filter out data bottom and surface averages that show evidence of operational error and equipment failure (e.g. CTD on while on deck, not reaching haul depth). Run after calc_surface_mean().
#' 
#' @param haul_metadata_path Character vector. Path to haul data .csv generated by get_bottom_data().
#' @param max_diff_bt_ctd_depth Numeric vector (1L). Maximum permissable difference between BT and CTD depth. Default = 10 m.
#' @param min_ctd_depth Numeric vector (1L). Minimum haul depth in meters. Default = 5 m.
#' @param vessel Required. Vessel number as a numeric vector.
#' @param year Required. Year as a numeric vector. 
#' @param region Required. Region as a character vector. Either "BS", "AI", or "GOA".
#' @export


remove_bad_station_data <- function(haul_metadata_path = list.files(paste0(getwd(), "/metadata/"), full.names = TRUE),
                                    max_diff_bt_ctd_depth = 10,
                                    min_ctd_depth = 5,
                                    region,
                                    vessel,
                                    year) {
  
  region <- toupper(region)
  
  if(length(haul_metadata_path) != 1) {
    stop(paste("get_haul_events(): Argument haul_meta_data path must only be file but has length ", length(haul_metadata_path)))
  }
  
  # Load haul metadata file
  haul_dat <- read.csv(file = haul_metadata_path, 
                       stringsAsFactors = FALSE)
  haul_dat$flag <- 0
  
  index_error <- unique(c(which(haul_dat$CTD_MEAN_HAUL_DEPTH < 5), 
                          which(abs(haul_dat$BT_GEAR_DEPTH - haul_dat$CTD_MEAN_HAUL_DEPTH) > 7), 
                          which(is.na(haul_dat$CTD_MEAN_HAUL_DEPTH))))
  haul_dat$flag[index_error] <- -1
  
  bad_files <- sub("\\_raw.*", "", haul_dat$cnv_file_name[index_error])
  
  if(length(bad_files) >= 1){
    # Move bad cnv files
    for(i in 1:length(bad_files)) {
      f_list <- list.files(pattern = bad_files[i], recursive = TRUE, full.names = TRUE)
      f_list <- f_list[grepl(pattern = ".cnv", x = f_list, fixed = TRUE)]
      f_newloc <- gsub(pattern = "/cnv/", replacement = "/bad_cnv/", f_list)
      file.rename(from = f_list, 
                  to = f_newloc)
    }
  }
  
  # Remove erroneous data from data frame
  haul_dat <- haul_dat[-index_error,]
  
  write.csv(haul_dat, 
            file = here::here("output", paste0("CTD_", region, "_", year, "_", vessel, ".csv")),
            row.names = FALSE)
  
  return(haul_dat)
  
}



#' Compare methods based on spikes and make multipanel plots (SBEDP workflow)
#' 
#' Compare processing methods based on spikiness of profiles and make multipanel plots to compare methods/workflows for processing CTD data.
#' 
#' @param prefix Character vector to include before output plots.
#' @param processing_method Character vector denoting the name of the processing method, corresponding with names of folders in /output/ that contain processed cnv files.
#' @param method_labels Character vector denoting titles to use on multipanel plots. Must be the same length as processing_method. Default (NULL) uses processing_method character vector for plot titles.
#' @param return_output Logical. If true, returns output in a data.frame.
#' @param pattern_upcast Character vector pattern for upcast file.
#' @param pattern_downcast Character vector pattern for downcast file.
#' @param min_pressure_bin Numeric vector (1L) indicating the minimum depth bin to use for comparing profiles.
#' @param area_method Should area calculations use temperature-salinity ("ts") or pressure-salinity ("ps") profiles?
#' @param scale_vars Logical. Should temperature and salinity be scaled to calculate T-S area?
#' @export

compare_methods <- function(prefix,
                            processing_method,
                            method_labels = NULL,
                            return_output = FALSE,
                            pattern_downcast = "downcast.cnv",
                            pattern_upcast = "upcast.cnv",
                            min_pressure_bin = 4,
                            area_method = "ts",
                            scale_vars = FALSE) {
  
  if(!dir.exists(here::here("plots", "binavg"))) {
    dir.create(here::here("plots", "binavg"))
  }
  
  if(!dir.exists(here::here("plots", "raw"))) {
    dir.create(here::here("plots", "raw"))
  }
  
  
  make_ud_plot <- function(dat_d, dat_u) {
    
    down_df <- data.frame(temperature = dat_d@data$temperature[!is.na(dat_d@data$flag)],
                          salinity = dat_d@data$salinity[!is.na(dat_d@data$flag)],
                          pressure = dat_d@data$pressure[!is.na(dat_d@data$flag)],
                          direction = "downcast") 
    up_df <- data.frame(temperature = dat_u@data$temperature[!is.na(dat_u@data$flag)],
                        salinity = dat_u@data$salinity[!is.na(dat_u@data$flag)],
                        pressure = dat_u@data$pressure[!is.na(dat_u@data$flag)],
                        direction = "upcast")
    
    comb_df <- dplyr::bind_rows(down_df, up_df, by = "method")
    
    temp_plot <- ggplot() +
      geom_path(data = comb_df, 
                aes(x = temperature, y = pressure, color = dir)) +
      scale_y_reverse()
    
    salin_plot <- ggplot() +
      geom_path(data = comb_df, aes(x = salinity, y = pressure, color = dir)) +
      scale_y_reverse()
    
    return(list(temp_plot = temp_plot,
                salin_plot = salin_plot))
  }
  
  
  if(!is.null(method_labels)) {
    plot_labels_df <- data.frame(method = processing_method,
                                 label = method_labels)
  } else {
    plot_labels_df <- data.frame(method = processing_method,
                                 label = processing_method)
  }
  
  if(!all(dir.exists(paste0(here::here("output"), "/", processing_method)))) {
    stop(paste0("\nDirectory not found for processing_method: ",  paste0(here::here("output"), "/", processing_method[!dir.exists(paste0(here::here("output"), "/", processing_method))], "\n")))
  }
  
  processing_dirs <- paste0(here::here("output"), "/", processing_method)
  id_vec <- character(length = 0L)
  
  for(ii in 1:length(processing_method)) {
    assign(paste0("u_listcnv_",processing_method[ii]), list.files(path = here::here("output", processing_method[ii]), pattern =  pattern_upcast, full.names = TRUE))
    assign(paste0("d_listcnv_",processing_method[ii]), list.files(path = here::here("output", processing_method[ii]), pattern = pattern_downcast, full.names = TRUE))
    id_vec <- c(id_vec, gsub("[^\\d]+", "", gsub(paste0(".*output/", processing_method[ii]), "", eval(parse(text = paste0("d_listcnv_", processing_method[ii])))), perl=TRUE))
    id_vec <- c(id_vec, gsub("[^\\d]+", "", gsub(paste0(".*output/", processing_method[ii]), "", eval(parse(text = paste0("u_listcnv_", processing_method[ii])))), perl=TRUE))
    
  }
  
  n_files <- as.data.frame(table(id_vec))
  n_files <- dplyr::filter(n_files, Freq == max(n_files$Freq))
  
  # Only files where data are available for all methods
  for(jj in 1:length(processing_method)) {
    assign(paste0("u_listcnv_", processing_method[jj]),
           eval(parse(
             text = paste0("u_listcnv_", processing_method[jj])
           ))[which(gsub("[^\\d]+", "",
                         gsub(
                           paste0(".*output/", processing_method[jj]), "",
                           eval(parse(
                             text = paste0("u_listcnv_", processing_method[jj])
                           ))
                         ), perl = TRUE) %in% n_files$id_vec)])
    
    assign(paste0("d_listcnv_", processing_method[jj]), eval(parse(
      text = paste0("d_listcnv_", processing_method[jj])
    ))[which(gsub("[^\\d]+", "", gsub(
      paste0(".*output/", processing_method[jj]), "", eval(parse(
        text = paste0("d_listcnv_", processing_method[jj])
      ))
    ), perl = TRUE) %in% n_files$id_vec)])
  }
  
  out_df <- data.frame()
  best_df <- data.frame()
  
  for(kk in 1:nrow(n_files)) {
    
    comb_df <- data.frame()
    
    for(mm in 1:length(processing_method)) {
      dat_d <- try(oce::read.oce(eval(parse(text = paste0("d_listcnv_", processing_method[mm])))[kk]), silent = TRUE)
      dat_u <- try(oce::read.oce(eval(parse(text = paste0("u_listcnv_", processing_method[mm])))[kk]), silent = TRUE)
      
      if(class(dat_d) == "try-error" & class(dat_u) == "try-error") { next }
      
      p_max <- 0
      if(class(dat_d) == "try-error") {
        down_df <- data.frame(temperature = numeric(),
                              salinity = numeric(),
                              pressure = numeric(),
                              delta_s = numeric(),
                              direction = character())
        p_max <- 1
      } else {
        down_df <- data.frame(temperature = dat_d@data$temperature[!is.na(dat_d@data$flag)],
                              salinity = dat_d@data$salinity[!is.na(dat_d@data$flag)],
                              pressure = dat_d@data$pressure[!is.na(dat_d@data$flag)],
                              delta_s = mean(abs(diff(dat_d@data$salinity[dat_d@data$pressure > min_pressure_bin])), na.rm = TRUE),
                              direction = "down") 
        if(nrow(down_df) < 8) {
          # Handle insufficient data
          p_max <- 1
        }
      }
      
      
      
      if(class(dat_u) == "try-error") {
        up_df <- data.frame(temperature = numeric(),
                            salinity = numeric(),
                            pressure = numeric(),
                            delta_s = numeric(),
                            direction = character())
        p_max <- 2
      } else {
        up_df <- data.frame(temperature = dat_u@data$temperature[!is.na(dat_u@data$flag)],
                            salinity = dat_u@data$salinity[!is.na(dat_u@data$flag)],
                            pressure = dat_u@data$pressure[!is.na(dat_u@data$flag)],
                            delta_s = mean(abs(diff(dat_u@data$salinity[dat_d@data$pressure > min_pressure_bin])), na.rm = TRUE),
                            direction = "up")
        if(nrow(up_df) < 8) {
          # Handle insufficient data
          p_max <- 2
        }
      }
      
      if(p_max == 0) {
        max_pressure <- min(c(max(down_df$pressure, na.rm = TRUE),  max(up_df$pressure, na.rm = TRUE)))
        
        
        poly_df <- dplyr::inner_join(dplyr::select(up_df, temperature, salinity, pressure) |>
                                       dplyr::rename(temperature_up = temperature,
                                                     salinity_up = salinity),
                                     dplyr::select(down_df, temperature, salinity, pressure) |>
                                       dplyr::rename(temperature_down = temperature,
                                                     salinity_down = salinity),
                                     by = "pressure")
        
        if(scale_vars) {
          poly_df$salinity_up <- scale(c(poly_df$salinity_up, poly_df$salinity_down))[,1][nrow(poly_df)]
          poly_df$salinity_down <- scale(c(poly_df$salinity_down, poly_df$poly_df$salinity_up))[,1][nrow(poly_df)]
          poly_df$temperature_up <- scale(c(poly_df$temperature_up, poly_df$temperature_down))[,1][nrow(poly_df)]
          poly_df$temperature_down <- scale(c(poly_df$temperature_down, poly_df$temperature_up))[,1][nrow(poly_df)]
        }
        
        if(area_method == "ts") {
          wkt_poly <- data.frame(geometry = paste0("LINESTRING (", apply(X = 
                                                                           cbind(
                                                                             apply(
                                                                               X = cbind(
                                                                                 poly_df$salinity_down[1:(nrow(poly_df))],
                                                                                 poly_df$temperature_down[1:(nrow(poly_df))]),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               cbind(c(poly_df$salinity_up[1:(nrow(poly_df)-1)], poly_df$salinity_up[(nrow(poly_df)-1)]),
                                                                                     c(poly_df$temperature_up[1:(nrow(poly_df)-1)], poly_df$temperature_up[(nrow(poly_df)-1)])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(c(poly_df$salinity_down[2:(nrow(poly_df))],poly_df$salinity_up[(nrow(poly_df))]),
                                                                                         c(poly_df$temperature_down[2:(nrow(poly_df))],poly_df$temperature_up[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(
                                                                                 c(poly_df$salinity_down[1:(nrow(poly_df)-1)],poly_df$salinity_down[(nrow(poly_df))]),
                                                                                 c(poly_df$temperature_down[1:(nrow(poly_df)-1)],poly_df$temperature_down[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " ")),
                                                                         MARGIN = 1,
                                                                         FUN = paste,
                                                                         collapse = ", "), ")")) 
        } else if(area_method == "ps") {
          wkt_poly <- data.frame(geometry = paste0("LINESTRING (", apply(X = 
                                                                           cbind(
                                                                             apply(
                                                                               X = cbind(
                                                                                 poly_df$salinity_down[1:(nrow(poly_df))],
                                                                                 poly_df$pressure[1:(nrow(poly_df))]),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               cbind(c(poly_df$salinity_up[1:(nrow(poly_df)-1)], poly_df$salinity_up[(nrow(poly_df)-1)]),
                                                                                     c(poly_df$pressure[1:(nrow(poly_df)-1)], poly_df$pressure[(nrow(poly_df)-1)])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(c(poly_df$salinity_down[2:(nrow(poly_df))],poly_df$salinity_up[(nrow(poly_df))]),
                                                                                         c(poly_df$pressure[2:(nrow(poly_df))],poly_df$pressure[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(
                                                                                 c(poly_df$salinity_down[1:(nrow(poly_df)-1)],poly_df$salinity_down[(nrow(poly_df))]),
                                                                                 c(poly_df$pressure[1:(nrow(poly_df)-1)],poly_df$pressure[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " ")),
                                                                         MARGIN = 1,
                                                                         FUN = paste,
                                                                         collapse = ", "), ")")) 
        }
        
        # Calculate area
        wkt_poly <- wkt_poly |>
          dplyr::mutate(ID = row_number()) |>
          st_as_sf(wkt = "geometry") |> 
          dplyr::group_by(ID) |>
          summarise(do_union = FALSE) |>
          sf::st_cast(to = "POLYGON")
        
        ts_area <- sum(sf::st_area(wkt_poly), na.rm = TRUE)
        
        down_df$area_ts <- ts_area
        up_df$area_ts <- ts_area
        
      } else if(p_max == 1) {
        max_pressure <- min(max(up_df$pressure, na.rm = TRUE))
      } else {
        max_pressure <- min(max(down_df$pressure, na.rm = TRUE))
      }
      
      comb_df <- dplyr::bind_rows(down_df, up_df) |>
        dplyr::mutate(method = processing_method[mm],
                      method_index = mm) |>
        dplyr::bind_rows(comb_df) |>
        dplyr::filter(pressure > 1)
      
    }
    
    comb_df <- comb_df |>
      dplyr::inner_join(plot_labels_df, by = "method")
    
    if(return_output) {
      comb_df$index <- kk
      comb_df$deploy <-  sub(paste0(".*/", processing_method[mm], "/"), "", sub("_raw.*", "", eval(parse(text = paste0("d_listcnv_", processing_method[mm])))[kk]))
      out_df <- dplyr::bind_rows(out_df, comb_df)
      
      # List best files to move
      if("area_ts" %in% names(comb_df)) {
        summary_df <- comb_df |>
          dplyr::select(direction, method, label, deploy, method_index, index, delta_s, area_ts) |>
          unique()
        
        summary_df <- summary_df |>
          dplyr::group_by(direction) |>
          dplyr::summarise(area_ts = min(area_ts, na.rm = TRUE)) |>
          dplyr::inner_join(summary_df, by = c("direction", "area_ts")) |>
          dplyr::mutate(move = NA) |>
          dplyr::select(-area_ts, -delta_s)
        
      } else {
        summary_df <- comb_df |>
          dplyr::select(direction, method, label, deploy, method_index, index, delta_s) |>
          unique()
        
        summary_df <- summary_df |>
          dplyr::group_by(direction) |>
          dplyr::summarise(delta_s = min(delta_s, na.rm = TRUE)) |>
          dplyr::inner_join(summary_df) |>
          dplyr::mutate(move = NA) |>
          dplyr::select(-delta_s)
      }
      
      summary_df$move[summary_df$direction == "down"] <- try(eval(parse(text = paste0("d_listcnv_", processing_method[summary_df$method_index[summary_df$direction == "down"]])))[summary_df$index[summary_df$direction == "down"]], silent = TRUE)
      summary_df$move[summary_df$direction == "up"] <- try(eval(parse(text = paste0("u_listcnv_", processing_method[summary_df$method_index[summary_df$direction == "up"]])))[summary_df$index[summary_df$direction == "up"]], silent = TRUE)
      
      best_df <- dplyr::bind_rows(summary_df, best_df)
      
    }
    
    best_profiles_df <- comb_df |> 
      dplyr::inner_join(summary_df, by = c("direction", "method", "method_index", "label", "index", "deploy"))
    
    png(file = here::here("plots", paste0(prefix, "_salinity_", kk, ".png")), width = 8, height = 8, units = "in", res = 300)
    print(ggplot() +
            geom_path(data = comb_df, 
                      aes(x = salinity, 
                          y = pressure, 
                          color = direction)) +
            geom_path(data = best_profiles_df, 
                      aes(x = salinity, 
                          y = pressure, 
                          color = direction),
                      size = rel(1.4)) +
            ggtitle(comb_df$deploy[1]) +
            facet_wrap(~label) +
            scale_y_reverse() +
            scale_color_manual(values = c("red", "black")) +
            theme_bw())
    dev.off()
    
    png(file = here::here("plots", paste0(prefix, "_temperature_", kk, ".png")), width = 8, height = 8, units = "in", res = 300)
    print(ggplot() +
            geom_path(data = comb_df, 
                      aes(x = temperature, 
                          y = pressure, 
                          color = direction)) +
            geom_path(data = best_profiles_df, 
                      aes(x = temperature, 
                          y = pressure, 
                          color = direction),
                      size = rel(1.4)) +
            ggtitle(comb_df$deploy[1]) +
            facet_wrap(~label) +
            scale_y_reverse() +
            scale_color_manual(values = c("red", "black")) +
            theme_bw())
    dev.off()
    
    png(file = here::here("plots", paste0(prefix, "_tsarea_", kk, ".png")), width = 8, height = 8, units = "in", res = 300)
    print(ggplot() +
            geom_path(data = comb_df, 
                      aes(x = temperature, 
                          y = salinity, 
                          color = direction)) +
            geom_path(data = best_profiles_df, 
                      aes(x = temperature, 
                          y = salinity, 
                          color = direction),
                      size = rel(1.4)) +
            ggtitle(comb_df$deploy[1]) +
            facet_wrap(~label) +
            scale_y_reverse() +
            scale_color_manual(values = c("red", "black")) +
            theme_bw())
    dev.off()
    
  }
  
  if(return_output) {
    return(list(compare_df = out_df,
                best_df = best_df))
  }
}



#' Make netCDF from profiles and metadata (SBEDP workflow)
#' 
#' Read-in files from /accepted_profiles/ and /metadata/ subdirectories and create a netCDF file. Combines data from multiple vessels from the same cruise. Must provide the following global_attributes to the function in a list: references, id, cdm_data_type, cruise, institution, contributor_name, creator_name, creator_institution, creator_email,publisher, publisher_type, publisher_url, geospatial_bounds_crs, license, metadata_link, instrument, standard_name_vocabulary, Conventions, source.
#' 
#' @param fpath Path to accepted profiles.
#' @param metadata_path Path to metadata files.
#' @param global_attributes List of global attributes that is passed to gapctd::df_to_netcdf(global_attributes).
#' @noRd

make_ctd_ncdf <- function(fpath = c(list.files(path = here::here("output", "accepted_profiles"), full.names = TRUE)),
                          metadata_path = c(list.files(path = here::here("metadata"), full.names = TRUE, full.names = TRUE)), 
                          output_file = "output.nc",
                          global_attributes = list(title = "CTD Data from AFSC 2021 EBS/NBS Bottom Trawl Survey",
                                                   references = "CTD TEAM...",
                                                   id = "doi",
                                                   cdm_data_type = "Point",
                                                   cruise = "2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Survey",
                                                   institution = "NOAA Alaska Fisheries Science Center",
                                                   contributor_name = "",
                                                   creator_name = "",
                                                   creator_institution = "NOAA Alaska Fisheries Science Center",
                                                   creator_email = "",
                                                   publisher = "NOAA Alaska Fisheries Science Center",
                                                   publisher_type = "institution",
                                                   publisher_url = "https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center",
                                                   geospatial_bounds_crs = "EPSG:4326",
                                                   license = "These data may be redistributed and used without restriction.",
                                                   metadata_link = "[DOI]",
                                                   instrument = "CTD",
                                                   Conventions = "CF-1.8",
                                                   standard_name_vocabulary = "CF Standard Name Table v79",
                                                   source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd")))) {
  
  req_attributes <- c("title", "references", "id", "cdm_data_type", "cruise", "institution", "contributor_name",
                      "creator_name", "creator_institution", "creator_email","publisher", "publisher_type", 
                      "publisher_url", "geospatial_bounds_crs", "license", "metadata_link", "instrument", "standard_name_vocabulary", "Conventions", "source")
  
  if(!all(req_attributes %in% names(global_attributes))) {
    stop(paste0("make_ctd_ncdf: The follow required global attribute(s) were not found in global_attributes : ", req_attributes[which(!(req_attributes %in% names(global_attributes)))]))
  }
  
  metadata_df <- data.frame()
  all_profiles <- data.frame()
  
  for(ii in metadata_path) {
    metadata_df <- dplyr::bind_rows(metadata_df,
                                    read.csv(file = ii))
  }
  
  metadata_df$deploy_id <- gsub(pattern = "_raw.*", "", x = metadata_df$cnv_file_name)
  
  
  metadata_df <- dplyr::bind_rows(
    metadata_df |>
      dplyr::mutate(deploy_id = paste0(deploy_id, "_downcast"),
                    CASTTIME = ON_BOTTOM) |>
      dplyr::select(-END_LONGITUDE, -END_LATITUDE) |>
      dplyr::rename(LONGITUDE = START_LONGITUDE,
                    LATITUDE = START_LATITUDE),
    metadata_df |>
      dplyr::mutate(deploy_id = paste0(deploy_id, "_upcast"),
                    CASTTIME = HAULBACK) |>
      dplyr::select(-START_LONGITUDE, -START_LATITUDE) |>
      dplyr::rename(LONGITUDE = END_LONGITUDE,
                    LATITUDE = END_LATITUDE))
  
  for(jj in 1:length(fpath)) {
    
    new_profile <- read.csv(file = fpath[jj])
    
    if(any(table(new_profile$depth) > 1)) {
      # Corner case. Not sure why this happens but it likely has to do with some issue that can occur with the identify function.
      stop(paste0("make_ctd_ncdf: Duplicate depth values found in ", fpath[jj], ". Please remove duplicates from the file then rerun make_ctd_ncdf()."))
    }
    
    if(jj > 1) {
      if(new_profile$deploy_id[1] %in% all_profiles$deploy_id) {
        stop(paste0("make_ctd_ncdf: ", fpath[jj], " cannot be added to profile_df because ", new_profile$deploy_id[1], " has already been added. Please check that deploy_id values are unique for each profile (i.e. no two hex/cnv files can have the same file name)."))
      }
    }
    
    profile_df <- dplyr::inner_join(new_profile, metadata_df, by = "deploy_id")
    
    all_profiles <- dplyr::bind_rows(all_profiles, profile_df)
  }
  
  
  # Convert times to UTC
  all_profiles$CASTTIME <- as.POSIXct(all_profiles$CASTTIME, tz = "America/Anchorage")
  all_profiles$CASTTIME <- lubridate::with_tz(all_profiles$CASTTIME, tz = "UTC")
  
  
  # Define temporal coverage
  time_coverage <- paste0(as.character(range(all_profiles$CASTTIME)), " UTC")
  
  
  # Convert time to character for netCDF NC_STRING format
  all_profiles$CASTTIME <- as.character(all_profiles$CASTTIME)
  
  # Rename columns to match CF naming conventions
  names(all_profiles) <- tolower(names(all_profiles))
  
  all_profiles <- all_profiles |>
    dplyr::rename(time = casttime,
                  profile = deploy_id,
                  stationid = stationid,
                  vessel = vessel,
                  cruise = cruise,
                  haul = haul,
                  haul_depth = ctd_mean_haul_depth,
                  sea_floor_temperature = ctd_mean_bottom_temperature_c,
                  sea_floor_salinity = ctd_mean_bottom_salinity_sa,
                  sea_floor_practical_salinity = ctd_mean_bottom_salinity_sp,
                  sea_floor_sound_speed_in_sea_water = ctd_mean_bottom_soundspeed,
                  time_elapsed = times,
                  sea_water_pressure = pressure,
                  sea_water_temperature = temperature,
                  sea_water_practical_salinity = salinity,
                  sea_water_salinity = gsw_saa0,
                  sea_water_density = gsw_densitya0,
                  sea_water_electrical_conductivity = conductivity,
                  sound_speed_in_sea_water = soundspeed,
                  buoyancy_frequency = n2,
                  quality_flag = flag)
  
  # Define spatial extent of data set using WKT polygon
  geospatial_bounds <- cbind(
    c(
      min(all_profiles$latitude),
      min(all_profiles$latitude),
      max(all_profiles$latitude),
      max(all_profiles$latitude),
      min(all_profiles$latitude)),
    c(
      min(all_profiles$longitude), 
      max(all_profiles$longitude),
      max(all_profiles$longitude),
      min(all_profiles$longitude),
      min(all_profiles$longitude))
  )
  
  geospatial_bounds <- paste0("POLYGON ((",
                              paste(apply(X = geospatial_bounds, MARGIN = 1, FUN = paste, collapse = " "), collapse = ", "),
                              "))")
  
  instrument_df <- all_profiles |>
    dplyr::select(vessel, ctd_serial_number, ctd_calibration_date) |>
    unique()
  
  # Set up global attributes list
  g_attributes <- list(references = global_attributes$references,
                       id = global_attributes$id,
                       cruise =  global_attributes$cruise,
                       institution = global_attributes$institution,
                       contributor_name = global_attributes$contributor_name,
                       creator_name = global_attributes$creator_name,
                       creator_institution = global_attributes$creator_institution,
                       creator_email = global_attributes$creator_email,
                       publisher = global_attributes$publisher,
                       publisher_type = global_attributes$publisher_type,
                       publisher_url = global_attributes$publisher_url,
                       geospatial_bounds = geospatial_bounds,
                       geospatial_bounds_crs = global_attributes$geospatial_bounds_crs,
                       license = global_attributes$license,
                       metadata_link = global_attributes$metadata_link,
                       date_created = as.character(Sys.Date()),
                       instrument = global_attributes$instrument,
                       Conventions = global_attributes$Conventions,
                       standard_name_vocabulary = global_attributes$standard_name_vocabulary,
                       cdm_data_type = global_attributes$cdm_data_type,
                       time_coverage_start = time_coverage[1],
                       time_coverage_end = time_coverage[2],
                       source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd")))
  
  
  # Create netCDF file
  gapctd::df_to_ncdf(x = all_profiles,
                     output_filename = output_file,
                     dim_names_2d = c("latitude", "longitude", "time"),
                     dim_units_2d = c("degree_north", "degree_east", "time"),
                     dim_long_names_2d = c("Latitude (decimal degrees)", "Longitude (decimal degrees)", "Time in Coordinated Universal Time (UTC)"),
                     var_names_2d = c("stationid", "profile", "vessel", "cruise", "haul", "haul_depth", "sea_floor_temperature", "sea_floor_practical_salinity", "sea_floor_salinity", "sea_floor_sound_speed_in_sea_water"),
                     var_long_names_2d = c("AFSC/RACE/GAP Survey Station Name", "Profile Number and Direction", "AFSC/RACE/GAP Vessel Code", "AFSC/RACE/GAP Cruise Code", "Haul Number", "Mean towed depth of CTD during haul", "Mean bottom temperature (ITS-90) at towed depth", "Mean Practical Salinity (PSS-78) at towed depth", "Mean Absolute Salinity (TEOS-10 GSW) at towed depth", "Mean speed of sound during haul (Chen-Millero)"),
                     var_units_2d = c("1", "1", "1", "1", "1", "m", "degree_C", "1", "g kg-1", "m s-1"),
                     dim_names_3d = c("depth"),
                     dim_long_names_3d = c("Depth in meters"),
                     dim_positive_3d = list("depth" = "down"),
                     dim_units_3d = c("m"),
                     dim_sort_3d = c(TRUE),
                     var_names_3d = c("time_elapsed", "sea_water_pressure", "sea_water_temperature", "sea_water_practical_salinity", "sea_water_salinity", "sea_water_density", "buoyancy_frequency", "sea_water_electrical_conductivity", "sound_speed_in_sea_water", "quality_flag"),
                     var_long_names_3d = c("Time Elapsed Since Start of Deployment", "Strain Gauge Pressure", "ITS-90 Temperature", "PSS-78 Practical Salinity", "TEOS-10 GSW Absolute Salinity", "TEOS-10 GSW Density", "Squared Brunt-Vaisala Buoyancy Frequency", "Electrical Conductivity", "Speed of Sound (Chen-Millero)", "Data Quality Assurance Flag"),
                     var_flag_values_3d = list("quality_flag" = c(-6,0,6,7)),
                     var_flag_meanings_3d = list("quality_flag" = c("Interpolated value. Density inversion error detected based on buoyancy frequency and remains after automatic removal and interpolation of point.",
                                                                    "Good quality, no issues detected.",
                                                                    "Interpolated value. Density inversion error detected based on buoyancy frequency and corrected using automatic removal and interpolation of point.",
                                                                    "Interpolated value. Point mannually flagged and removed during visual inspection then estimated through interpolation.")),
                     var_units_3d = c("s", "dbar", "degree_C", "1", "g kg-1", "kg m-3", "s-2", "S m-1)", "m s-1", "1"),
                     instrument_attributes = c("make_model", "serial_number", "calibration_date", "vessel"),
                     instrument_values = list(make_model = "Sea-Bird SBE19plus V2",
                                              serial_number = instrument_df$ctd_serial_number,
                                              calibration_date = instrument_df$ctd_calibration_date,
                                              vessel = instrument_df$vessel),
                     global_attributes = g_attributes)
  
}
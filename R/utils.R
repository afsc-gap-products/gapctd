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
      message("Invalid entry, please try again. Must be one of ", paste(valid_responses, collapse = ", "))
    }
  }
  return(entry)
}



#' Remove rds files based on QA/QC flags (R workflow)
#' 
#' Remove deployment rds files that contain bottom, upcast, and/or bottom ctd objects based on QA/QC flags.
#' 
#' @param rds_dir_path File path to directory containing rds files to be evaluated.
#' @param in_pattern Character vector search pattern for input files.
#' @return Bad files moved to /bad_cnv/
#' @export

move_bad_rds <- function(rds_dir_path = here::here("output", "gapctd"),
                         in_pattern = "_typical.rds") {
  
  for(gg in 1:length(in_pattern)) {
    
    rds_path <- list.files(rds_dir_path, pattern = in_pattern[gg], full.names = TRUE)
    
    n_moved <- 0
    
    for(hh in 1:length(rds_path)) {
      
      message("move_bad_rds: Checking ", rds_path[hh])
      eval_deployment <- readRDS(rds_path[hh])
      
      flag <- FALSE
      
      # Check metadata for flags created by qc_check()
      if("downcast" %in% names(eval_deployment)) {
        if(any(as.logical(eval_deployment$downcast@metadata$flags), na.rm = TRUE)) {
          flag <- TRUE
          eval_deployment <- eval_deployment[-which(names(eval_deployment) == "downcast")]
        }
        
      }
      
      if("upcast" %in% names(eval_deployment)) {
        if(any(as.logical(eval_deployment$upcast@metadata$flags), na.rm = TRUE)) {
          flag <- TRUE
          eval_deployment <- eval_deployment[-which(names(eval_deployment) == "upcast")]
        }
      }
      
      # Move file containing flagged profile to a the /bad_cnv/ directory
      if(flag) {
        message(paste0("move_bad_rds: Moving ", rds_path[hh]))
        
        new_loc <- gsub(pattern = paste0("/output/", processing_method, "/"), 
                        replacement = "/bad_cnv/",
                        x = rds_path[hh])
        
        file.rename(from =  rds_path[hh], to = new_loc)
        
        # Save deployment list with only one downcast or upcast to output
        if(any(c("downcast", "upcast") %in% names(eval_deployment))) {
          saveRDS(object = eval_deployment, file = rds_path[hh])
        }
        
        n_moved <- n_moved + 1
      }
      
    }
    
    message(paste0("move_bad_rds: Moved ", n_moved, " files to /bad_cnv/"))  
  }
  
}



#' Move good and bad profiles to final directories (R workflow)
#' 
#' @param rds_dir_path Path to directory containing rds files
#' @export

finalize_data <- function(rds_dir_path = here::here("output", processing_method)) {
  
  
  bad_files <- c(list.files(rds_dir_path, pattern = "_uc_uc_full.rds", full.names = TRUE),
                 list.files(rds_dir_path, pattern = "_dc_dc_full.rds", full.names = TRUE))
  
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



#' Internal function for analysis
#' 
#' @noRd

derive_and_bin <- function(x, bin_width = NULL) {
  x$downcast <- gapctd::derive_eos(x$downcast)
  
  x$upcast <- gapctd::derive_eos(x$upcast) 
  
  
  if(!is.null(bin_width)) {
    x$downcast <- gapctd:::bin_average(x$downcast,
                                       by = "depth", 
                                       bin_width = bin_width)
    x$upcast <- gapctd:::bin_average(x$upcast,
                                     by = "depth", 
                                     bin_width = bin_width)
    
  }
  
  return(x)
  
}


#' Internal function for analysis
#' 
#' @noRd

make_stage_df <- function(x, stage, method) {
  
  out <- data.frame()
  
  if("downcast" %in% names(x)) {
    
    out <- dplyr::bind_rows(out, 
                            as.data.frame(x$downcast@data)|>
                              dplyr::mutate(cast_direction = x$downcast@metadata$cast_direction,
                                            processing_method = method) |>
                              dplyr::bind_cols(x$downcast@metadata$race_metadata |>
                                                 dplyr::select(VESSEL, CRUISE, HAUL, REGION))
    )
    
  }
  
  
  if("upcast" %in% names(x)) {
    
    out <- dplyr::bind_rows(out,
                            as.data.frame(x$upcast@data) |>
                              dplyr::mutate(cast_direction = x$upcast@metadata$cast_direction,
                                            processing_method = method) |>
                              dplyr::bind_cols(x$upcast@metadata$race_metadata |>
                                                 dplyr::select(VESSEL, CRUISE, HAUL, REGION)))
    
  }
  
  out$stage <- stage
  
  return(out)
}


#' Check for duplicates (Internal)
#' 
#' Check /data/ directory for .hex files for multiple files from the same deployment and deployments from multiple CTDs. Either of these cases results in an error.
#' 
#' @noRd

.check_duplicates <- function() {
  
  file_paths <- list.files(here::here("data"), full.names = TRUE, pattern = ".hex")
  serial_number <- numeric(length = length(file_paths))
  deployment_datetime <- character(length = length(file_paths))
  
  message(paste0("Checking ", length(file_paths), " .hex files in /data/ for duplicates and multiple CTD serial numbers."))
  
  for(ii in 1:length(file_paths)) {
    lines <- readLines(con = file_paths[ii])
    
    # Serial number
    serial_number[ii] <- as.numeric(
      gsub("[^0-9]", 
           "", 
           lines[grepl(pattern = "* Temperature SN", x = lines)])
    )
    
    # Date
    date_line <- lines[grepl(pattern = "* cast", x = lines)]
    match_positions <- gregexpr(pattern = "\\d{2} [A-Za-z]{3} \\d{4} \\d{2}:\\d{2}:\\d{2}", 
                                text = date_line)
    deployment_datetime[ii] <- regmatches(x = date_line, m = match_positions)[[1]]
  }
  
  if(any(table(deployment_datetime) > 1)) {
    
    deployment_freq <- table(deployment_datetime)
    
    duplicates <- names(deployment_freq)[which(deployment_freq > 1)]
    
    duplicate_filenames <- file_paths[deployment_datetime %in% duplicates]
    
    stop(paste0("Error: Multiple files found from the same deployment:\n", paste(duplicate_filenames, collapse = "\n")))
    
  }
  
  if(length(unique(serial_number)) > 1) {
    stop(paste0("Error: Multiple serial numbers detected among .hex files. Serial numbers:\n", 
                paste(paste0(serial_number, " - ", file_paths), collapse = "\n")))
  }
  
  message("No duplicates or multiple serial numbers detected among .hex files.")
  
}



#' Extract calibration parameters from xmlcon to a list
#' 
#' Retrives calibration parameters for temperature, conductivity, pressure, oxygen, and pH sensors from an instrument configuration (.xmlcon) file.
#' 
#' @param xmlcon_path Path to an xmlcon file.
#' @export

extract_calibration_xmlcon <- function(xmlcon_path) {
  
  lines <- readLines(con = xmlcon_path)
  
  data_channels <- c("temperature", "conductivity", "pressure", "oxygen", "ph")
  channel_tag_start <- c("<TemperatureSensor", "<ConductivitySensor", "<PressureSensor ", "<!-- Coefficients for Sea-Bird equation", "<pH_Sensor")
  channel_tag_end <- c("</TemperatureSensor", "</ConductivitySensor", "</PressureSensor>", "</OxygenSensor", "</pH_Sensor")
  
  calibration_params <- list(temperature = c("A0" = -9e9, "A1" = -9e9, "A2" = -9e9, "A3" = -9e9, "Slope" = -9e9, "Offset" = -9e9),
                             conductivity = c("A" = -9e9, "B" = -9e9, "C" = -9e9, "D" = -9e9, "M" = -9e9, "G" = -9e9, "H" = -9e9, "I" = -9e9, "J" = -9e9, "CPcor" = -9e9, "CTcor" = -9e9),
                             pressure = c("PA0" = -9e9, "PA1" = -9e9, "PA2" = -9e9, "PTEMPA0" = -9e9, "PTEMPA1" = -9e9, "PTEMPA2" = -9e9, "PTCA0" = -9e9, "PTCA1" = -9e9, "PTCA2" = -9e9, "PTCB0" = -9e9, "PTCB1" = -9e9, "PTCB2" = -9e9, "Offset" = -9e9),
                             oxygen = c("Soc" = -9e9, "offset" = -9e9, "A" = -9e9, "B" = -9e9, "C" = -9e9, "D0" = -9e9, "D1" = -9e9, "D2" = -9e9, "E" = -9e9, "Tau20" = -9e9, "H1" = -9e9, "H2" = -9e9, "H3" = -9e9),
                             ph = c("Slope" = -9e9, "Offset" = -9e9))
  
  omit <- numeric()
  
  calibration_df <- data.frame()
  
  for(ii in 1:length(data_channels)) {
    
    cal_pars <- numeric(length = length(calibration_params[[data_channels[ii]]]))
    
    if(!any(grepl(pattern = channel_tag_start[ii], x = lines))) {
      message(paste0("xmlcon_to_df: Skpping ", data_channels[ii], ". Sensor calibration parameters not detected in xmlcon file."))
      omit <- c(omit, ii)
      next
    }
    
    serial_number <- gapctd::get_calibration_parameter(header = lines, 
                                                       cal_par = "SerialNumber", 
                                                       start_block = ifelse(data_channels[ii] == "oxygen", "<OxygenSensor", channel_tag_start[ii]), 
                                                       end_block = channel_tag_end[ii])
    
    calibration_date <- gapctd::get_calibration_parameter(header = lines, 
                                                          cal_par = "CalibrationDate", 
                                                          start_block = ifelse(data_channels[ii] == "oxygen", "<OxygenSensor", channel_tag_start[ii]), 
                                                          end_block = channel_tag_end[ii],
                                                          make_numeric = FALSE)
    
    calibration_df <- dplyr::bind_rows(calibration_df,
                                       data.frame(
                                         channel = data_channels[ii],
                                         serial_number = serial_number,
                                         calibration_date = as.Date(calibration_date, format = "%d-%b-%y")
                                       )
    )
    
    for(jj in 1:length(calibration_params[[data_channels[ii]]])) {
      calibration_params[[data_channels[ii]]][jj] <- gapctd::get_calibration_parameter(header = lines, 
                                                                                       cal_par = names(calibration_params[[data_channels[ii]]][jj]), 
                                                                                       start_block = channel_tag_start[ii], 
                                                                                       end_block = channel_tag_end[ii])
    }
    
  }
  
  calibration_params$calibration <- calibration_df
  
  if(length(omit) > 0) {
    calibration_params[omit] <- NULL
  }
  
  return(calibration_params)
  
}



#' Internal function converts offset list to numeric vector
#' 
#' @param offset_list List of named vectors one element is named 'offset'
#' @param variable Character vector of data variable names to offset.
#' @noRd

offset_list_to_vector <- function(offset_list, variables) {
  
  out <- numeric(length = length(variables))
  
  for(jj in 1:length(variables)) {
    
    out[jj] <- as.numeric(offset_list[[variables[jj]]]['offset'])
    
  }
  
  return(out)
  
}
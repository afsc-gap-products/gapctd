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

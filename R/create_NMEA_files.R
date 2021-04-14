#' Create NMEA files with LAT/LON for Derive TEOS-10
#'
#' Function to retrieve haul data from RACEBASE or a csv file and generate NMEA (.txt) files that match .cnv file names. Function dependencies: getPass, RODBC, oce
#'
#' @param rodbc_channel Required if haul_csv is not provided. Must provide an open RODBC channel (this parameter) or path to haul_csv. Default = NA.
#' @param haul_csv Required if rodbc_channel is not provided. Path to a csv file that contains VESSEL, CRUISE, HAUL, START_TIME, START_LONGITUDE, START_LATITUDE, END_LONGITUDE, END_LATITUDE.
#' @param vessel Required. Vessel number as a numeric vector.
#' @param year Required. Year as a numeric vector. 
#' @param region Required. Region as a character vector. Either "bs", "ai", or "goa".
#' @export

create_NMEA_files <- function(rodbc_channel = NA, 
                              haul_csv = NA,
                              vessel,
                              region,
                              year)
{
  
  ## Load cnv files ----
  cnv_files <- list.files(path = paste0(getwd(), "/cnv"), pattern = "\\_tmcorrect.cnv$")
  print(paste0("CNV files found: ", length(cnv_files)))
  
  ###### ADD cnv_files check #####
  
  print("Running query")
  if(!is.na(rodbc_channel)) {
    haul_df <- RODBC::sqlQuery(channel, "select * 
                                  from racebase.haul where
                                  cruise > 198700")
  } else if(!is.na(haul_csv)) {
    haul_df <- read.csv(file = haul_csv, 
                        stringsAsFactors = FALSE)
  }
  
  ###### ADD START_TIME STRING LENGTH CHECK and haul_df row check #######
  
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
    
    #if(cnv_time_zone == haul_time_zone)
    #{
    time_diff <- abs(
      as.numeric(
        difftime(strptime(subset_haul$START_TIME, "%Y-%m-%d %H:%M:%S"),
                 strptime(current_cnv[["metadata"]]$startTime, "%Y-%m-%d %H:%M:%S"))))
    minimum_diff <- min(time_diff)
    position <- which(time_diff == minimum_diff)
    
    ### & extract start lat/lon, vessel, cruise, haul from matching haul file to create NMEA file
    nmea_vessel <- subset_haul[position, ]$VESSEL
    nmea_cruise <- subset_haul[position, ]$CRUISE
    nmea_haul <- subset_haul[position, ]$HAUL
    nmea_longitude <- subset_haul[position, ]$START_LONGITUDE
    nmea_latitude <- subset_haul[position, ]$START_LATITUDE
    
    ##paste info together for txt file output
    first_line <- paste0("// VESSEL ", nmea_vessel,", CRUISE ", nmea_cruise,", HAUL ", nmea_haul," //")
    second_line <- paste0("Longitude: ", nmea_longitude)
    third_line <- paste0("Latitude: ", nmea_latitude)
    
    ##write the nmea txt files
    file_name <- gsub("\\..*","",cnv_files[i])
    
    # Setup NMEA for no EOS80 ----
    fileConn <- file(paste0(getwd(), "/cnv/", file_name, ".txt"))
    writeLines(c(first_line, second_line, third_line), fileConn)
    close(fileConn)
    
    # Setup NMEA with EOS80 ----
    fileConn <- file(paste0(getwd(), "/cnv/", file_name, "_EOS80.txt"))
    writeLines(c(first_line, second_line, third_line), fileConn)
    close(fileConn)
    #  }else{
    #  print("Time zones between haul data and CTD data do not match, fix time zones")
    #    }
  }
}

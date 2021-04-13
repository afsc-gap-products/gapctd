#' Function to pull in lat/lon from bottom trawl racebase haul data that matches each .cnv to create NMEA files
#'
#' implement between derive.bat and getdata.bat. Requires a VPN connection to run. 
#' Function will prompt for oracle username, oracle password, survey region (GOA, AI, BS), and survey year.
#' Function dependencies: getPass, RODBC, oce
#'
#' @param oracle_username Character. Enter your personal oracle username to access RACEBASE.HAUL oracle data
#' @param oracle_password Character. Enter your personal oracle password to access RACEBASE.HAUL oracle data
#' @param survey_region Character. Specify the survey region of your CTD data (GOA, AI, BS). Prompted, no default.
#' @param survey_year Character. Specify the survey year of your CTD data. Prompted, no default.
#' @export

##remotes::install_github("dankelley/oce", ref="develop")

create_NMEA_files <- function()
{

##load cnv files
#cnv_files <- read.oce(paste0(getwd(),"/cnv/08_14_19_001_raw_fil1_fil2_align_tmcorrect.cnv"))
cnv_files <- list.files(path = paste0(getwd(),"/cnv"),pattern = "\\_tmcorrect.cnv$")
username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
region <- getPass::getPass(msg = "Enter survey region (options are BS, GOA, AI): ")
year <- getPass::getPass(msg = "Enter survey year: ")

for(i in 1:length(cnv_files))
{
current_cnv <- oce::read.oce(paste0(getwd(),"/cnv/",cnv_files[i]))
cnv_date <- substring(current_cnv[["metadata"]]$startTime, 1, 10)
cnv_time_zone <- attr(current_cnv[["metadata"]]$startTime,"tzone")

#load haul data
PKG <- c("RODBC")
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {  
    install.packages(p)
    require(p,character.only = TRUE)}
}


channel <- RODBC::odbcConnect(dsn = "AFSC",
                     uid = username, # change
                     pwd = password, #change
                     believeNRows = FALSE)

odbcGetInfo(channel)
haul <- RODBC::sqlQuery(channel, "select * 
                                  from racebase.haul where
                                  cruise > 198700")
haul <- haul[which(haul$REGION == region),]
haul$YEAR = as.numeric(substring(haul$START_TIME, 1,4))
year <- as.numeric(substring(haul$START_TIME, 1,4))
haul <- haul[which(haul$YEAR == year),]
haul$tow_date <- substring(haul$START_TIME, 1,10)
haul$tow_start_time <- substring(haul$START_TIME, 12,19)

##find haul that matches cnv metadata by finding the haul of the right date that has the least diff between cnv start time & haul start time 
subset_haul <- haul[which(haul$tow_date == cnv_date),]
haul_time_zone <- attr(as.POSIXlt(subset_haul$START_TIME[1]),"tzone")[3]

#if(cnv_time_zone == haul_time_zone)
#{
time_diff <- abs(as.numeric(difftime(strptime(subset_haul$START_TIME,"%Y-%m-%d %H:%M:%S"), strptime(current_cnv[["metadata"]]$startTime,"%Y-%m-%d %H:%M:%S"))))
minimum_diff <- min(time_diff)
position <- which(time_diff == minimum_diff)

### & extract start lat/lon, vessel, cruise, haul from matching haul file to create NMEA file
nmea_vessel <- subset_haul[position,]$VESSEL
nmea_cruise <- subset_haul[position,]$CRUISE
nmea_haul <- subset_haul[position,]$HAUL
nmea_longitude <- subset_haul[position,]$START_LONGITUDE
nmea_latitude <- subset_haul[position,]$START_LATITUDE

##paste info together for txt file output
first_line <- paste0("// VESSEL ",nmea_vessel,", CRUISE ",nmea_cruise,", HAUL ",nmea_haul," //")
second_line <- paste0("Longitude: ",nmea_longitude)
third_line <- paste0("Latitude: ",nmea_latitude)

##write the nmea txt file
file_name <- gsub("\\..*","",cnv_files[i])

fileConn <- file(paste0(getwd(),"/cnv/",file_name,".txt"))
writeLines(c(first_line,second_line,third_line), fileConn)
close(fileConn)
#  }else{
#  print("Time zones between haul data and CTD data do not match, fix time zones")
#    }
  }
}

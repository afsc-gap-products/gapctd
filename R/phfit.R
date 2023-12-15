#' This function fits pH calibration parameters
#'  
#' This function estimates pH calibration parameters using Sea-Bird's calibration equation.
#'  
#' @param ph ph of the reference standard
#' @param temperature Reference standard temperature in degrees celcisus
#' @param ph_voltage Measured pH channel voltage at the reference standard
#' @export
#' @references Sea-Bird Electronics Application Note No. 18-1. SBE 18, 27, and 30, and AMT pH Sensor Calibration (PHFIT Version 2.1) https://www.seabird.com/asset-get.download.jsa?code=250995
#' @examples
#' # Example calibration using three pH reference standards (4, 7, 10)
#' # Temperature = 23 degrees C
#' # Measured voltage (e.g. from Real-Time Data Acquisition in Seasave V7)
#' # See also: SBE pH Sensor Calibration. https://www.youtube.com/watch?v=4uMc5H66dVQ
#' 
#' phfit(
#'   ph_voltage = c(1.977, 2.797, 3.534),
#'   ph = c(4, 7, 10),
#'  temperature = 23)

phfit <- function(ph, ph_voltage, temperature) {
  
  stopifnot("phfit: Must provide at least three pH voltage and pH values." = length(ph_voltage) > 2 & length(ph) > 2)
  
  temp_k <- temperature + 273.15
  
  xx <- (1.98416e-4 * temp_k * (ph - 7))
  
  mat <- cbind(rep(1, length(ph)), xx)
  
  fit <- as.numeric(solve((t(mat) %*% mat)) %*% t(mat) %*% ph_voltage)
  
  fit <- c("pHslope" = fit[2], "pHoffset" = fit[1])
  
  fit <- signif(fit, 5)
  
  ph_fitted <- 7 + (ph_voltage - fit['pHoffset']) / (1.98416e-4 * temp_k * fit['pHslope']) 
  
  ph_residual <- round(ph - ph_fitted, 3)
  
  return(list(fit = fit, ph_residual =  ph_residual))
  
}


#' Fit pH calibration parameters and generate new configuration file
#'  
#' This function estimates pH calibration parameters using Sea-Bird's calibration equation.
#' 
#' @param ph ph of the reference standard
#' @param temperature Reference standard temperature in degrees celcisus
#' @param ph_voltage Measured pH channel voltage at the reference standard
#' @param xmlcon_in Filepath to the current CTD configuration file.
#' @param xmlcon_out Filepath to the output CTD configuration file. 
#' @param ph_serial_number Serial number for the pH sensor (typically four digits.)
#' @export
#' @author Sean Rohan
#' @examples
#' 
#' # Example of pH calibration with a demo file
#' 
#' ph_calibration(xmlcon_in = system.file("./extdata/example/SBE19plusV2_8106_ph_DO_leg2.xmlcon", package = "gapctd"),
#'                ph_serial_number = 9090,
#'                ph_voltage = c(1.977, 2.797, 3.534),
#'                ph = c(4, 7, 10),
#'                temperature = 23)


ph_calibration <- function(ph, temperature, ph_voltage, xmlcon_in, xmlcon_out = NULL, ph_serial_number) {
  
  stopifnot("ph_calibration: The four digit serial number for the pH sensor being calibrated must be provided." = is.numeric(ph_serial_number) & nchar(ph_serial_number) == 4)
  
  if(is.null(xmlcon_out)) {
    xmlcon_out <- basename(xmlcon_in)
    
    xmlcon_out <- gsub(x = xmlcon_out, 
                       pattern = ".xmlcon", 
                       replacement = format(as.POSIXct(Sys.time()), 
                                            format = "_%Y%m%d_%H%M%S.xmlcon"))
    
  } else {
    stopifnot("ph_calibration: Output path for xmlcon_out does not exist." =  dir.exists(dirname(xmlcon_out)))
  }
  
  header <- readLines(xmlcon_in)
  
  ph_tag_block <- grep(pattern = "pH_Sensor", x = header)
  slope_index <- grep(pattern = "Slope", x = header[ph_tag_block[1]:ph_tag_block[2]]) + ph_tag_block[1] - 1
  offset_index <- grep(pattern = "Offset", x = header[ph_tag_block[1]:ph_tag_block[2]]) + ph_tag_block[1] - 1
  serial_number_index <- grep(pattern = "SerialNumber", x = header[ph_tag_block[1]:ph_tag_block[2]]) + ph_tag_block[1] - 1
  calibration_date_index <- grep(pattern = "CalibrationDate", x = header[ph_tag_block[1]:ph_tag_block[2]]) + ph_tag_block[1] - 1
  
  stopifnot("ph_calibration: No pH sensor tag <pH_Sensor> n in the .xmlcon file. The first time the sensor is used, it must be added using SBE Data Processing Software to esnure voltage channels are configured correctly." = length(ph_tag_block) > 1)
  
  old_slope <- get_calibration_parameter(header[slope_index], 
                                                  cal_par = "Slope")
  
  old_offset <- get_calibration_parameter(header[offset_index], 
                                                   cal_par = "Offset")
  
  old_serial_number <- get_calibration_parameter(header[serial_number_index], 
                                                          cal_par = "SerialNumber") 
  
  old_calibration_date <- get_calibration_parameter(header[calibration_date_index], 
                                                             cal_par = "CalibrationDate",
                                                             make_numeric = FALSE) 
  
  # Check that ph sensor calibration fields exist
  
  stopifnot("ph_calibration: pH <Slope> or <Offset> not found in .xmlcon file." = all(is.numeric(c(old_slope, old_offset))))
  stopifnot("ph_calibration: pH <CalibrationDate> not found in .xmlcon file." = all(is.character(old_calibration_date)))
  
  same_sensor <- ph_serial_number == old_serial_number
  
  if(!same_sensor) {
    v <- readline(prompt = paste0("WARNING! pH sensor serial number (ph_serial_number) does not match the serial number in the .xmlcon file (", old_serial_number,"). Please verify that the pH serial number is correct. \nPress ENTER to continue or ESC to correct the serial number."))
  }
  
  new_fit <- phfit(ph_voltage = ph_voltage,
                   ph = ph,
                   temperature = temperature)
  
  message(
    paste0("ph_calibration: Old parameters [slope = ", 
           old_slope, ", offset = ", 
           old_slope, " from ", old_calibration_date, 
           ". New parameters [slope = ", new_fit$fit[1], 
           ", offset = ", new_fit$fit[2], "]. ")
  )
  
  # ---- This is where diagnostics will go -----
  
  header[slope_index] <- gsub(pattern = old_slope, 
                              replacement = new_fit$fit['pHslope'], 
                              x = header[slope_index])
  
  header[offset_index] <- gsub(pattern = old_slope, 
                               replacement = new_fit$fit['pHoffset'], 
                               x = header[offset_index])
  
  header[serial_number_index] <- gsub(pattern = old_serial_number, 
                                      replacement = ph_serial_number, 
                                      x = header[serial_number_index])
  
  header[calibration_date_index] <- gsub(pattern = old_calibration_date, 
                                         replacement = format(Sys.Date(), "%d-%b-%y"), 
                                         x = header[calibration_date_index])
  
  message("ph_calibration: Writing new xmlcon file to ", xmlcon_out)
  
  writeLines(text = header, con = xmlcon_out)
  
  return(new_fit)
  
}

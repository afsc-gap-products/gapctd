#' Convert SBE19plus V2 hex files to cnv
#'
#' This function decodes hexadecimal-formatted Sea-Bird CTD files to cnv files.
#'
#' @param hex_path Path to a .hex file
#' @param output_path Path to the output file location for a .cnv file
#' @param xmlcon_path Optional. Path to config file. Must be provided if .hex file does not contain configuration file parameters.
#' @param sample_interval Sampling interval for scans; 0.25 for a typical SBE19plus V2 deployment.
#' @param output_channels Optional. Named vector of output channels and their names. Do not use unless outputs are are not the defaults.
#' @param output_sig_digits Optional. Significant digits after the decimal place for output channels. Only change if a subset of channels. Do not use unless outputs are are not the defaults.
#' @export
#' @author Sean Rohan

hex_to_cnv <- function(hex_path,
                       output_path,
                       xmlcon_path = NULL,
                       sample_interval = 0.25,
                       output_channels = NULL,
                       output_sig_digits = NULL) {
  
  if(is.null(output_channels)) {
    
    output_channels <- c("time_elapsed" = "timeS: Time, Elapsed [seconds]",
                         "temperature" = "tv290C: Temperature [ITS-90, deg C]",
                         "pressure" = "prdM: Pressure, Strain Gauge [db]",
                         "conductivity" = "c0S/m: Conductivity [S/m]",
                         "oxygen_voltage" = "sbeox0V: Oxygen raw, SBE 43 [V]",
                         "ph" = "ph: pH",
                         "flag" = "flag:  0.000e+00")
    
    output_sig_digits <- c("time_elapsed" = 3,
                           "temperature" = 4,
                           "pressure" = 3,
                           "conductivity" = 6,
                           "oxygen_voltage" = 4,
                           "ph" = 3,
                           "flag" = 1)
  }
  
  # Convert bytes to unsigned integers
  hex_extract_raw_uint <- function(x, start, size, big_endian, scale = 1, offset = 0) {
    x <- x[start:(start + size - 1)]
    
    
    if (size == 3 && big_endian) {
      x <- c(as.raw(0x00), x)
      size <- 4
    } else if (size == 3 && !big_endian) {
      x <- c(x, as.raw(0x00))
      size <- 4
    }
    
    con <- rawConnection(x)
    on.exit(close(con))
    
    value <- readBin(con, 
                     what = "integer", 
                     n = 1,
                     size = size, 
                     endian = if(big_endian) "big" else "little", 
                     signed = size >= 4
    )
    
    return(value / scale + offset)
    
  }
  
  split_string <- function(input_string) {
    out <- sapply(seq(1, nchar(input_string), 2), function(x) {
      substr(input_string, x, x + 1)
    })
    
    as.raw(paste0("0x", out))
  }
  
  stopifnot("hex_to_cnv: Error! output_channels and output_sig_digits must both be either a named character vector and named numeric vector, respectively -OR- both be NULL."  = (is.null(output_channels) & is.null(output_sig_digits)) | (is.character(output_channels) & is.numeric(output_sig_digits)))
  
  
  # Split-up hex file into header and hex data
  lines_hex <- readLines(hex_path)
  end_header <- grep(pattern = "*END*", x = lines_hex)
  last_line <- length(lines_hex)
  lines_header <- lines_hex[1:(end_header-1)]
  
  # xmlcon variable names do not match hex file names and must be substituted
  if(!is.null(xmlcon_path)) {
    
    xmlcon_header <- readLines(xmlcon_path)
    
    # Get list of calibration parameters
    cal_par_list <- extract_calibration_xmlcon(xmlcon_path)
    
    # Change tags for calibration coefficients to match SBE Data Processing output
    init_var <- c("<A0>", "<A1>", "<A2>", "<A3>", "</A0>", "</A1>", "</A2>", "</A3>", "CPcor", "CTcor")
    repl_var <- c("<TA0>", "<TA1>", "<TA2>", "<TA3>", "</TA0>", "</TA1>", "</TA2>", "</TA3>", "CPCOR", "CTCOR")
    
    for(hh in 1:length(init_var)) {
      xmlcon_header <- gsub(x = xmlcon_header, pattern = init_var[hh], replacement = repl_var[hh])
    }
    
    lines_header <- c(lines_header, paste("#", xmlcon_header))
    
  }
  
  lines_data <- lines_hex[(end_header+1):(last_line-1)]
  
  # Index of hex values for different channels
  deployed_do <- any(grepl(pattern = "<OxygenSensor", x = lines_header))
  deployed_ph <- any(grepl(pattern = "<pH_Sensor", x = lines_header))

  
  byte_list <- lapply(lines_data, split_string)  
  
  hex_channels <- list(start = c(1, 4, 7, 10),
                       size = diff(c(1, 4, 7, 10, 12)),
                       big_endian = c(rep(TRUE, 4)),
                       name = c("temperature_int", "conductivity_int", "pressure_int", "tvoltage_int"))
  
  index_channels <- c(1:4,7)
  
  if(deployed_do) {
    hex_channels <- list(start = c(1, 4, 7, 10, 12),
                         size = diff(c(1, 4, 7, 10, 12, 14)),
                         big_endian = c(rep(TRUE, 5)),
                         name = c("temperature_int", "conductivity_int", "pressure_int", "tvoltage_int",
                                  "doxygen_int"))
    
    index_channels <- c(1:5,7)
  }
  
  if(deployed_do & deployed_ph) {
    hex_channels <- list(start = c(1, 4, 7, 10, 12, 14),
                         size = diff(c(1, 4, 7, 10, 12, 14, 16)),
                         big_endian = c(rep(TRUE, 6)),
                         name = c("temperature_int", "conductivity_int", "pressure_int", "tvoltage_int",
                                  "doxygen_int",
                                  "ph_int"))
    
    # index_calibration_pars <- 1:40
    index_channels <- 1:7
  }
    
    output_channels <- output_channels[index_channels]
    output_sig_digits <- output_sig_digits[index_channels]
    
    message(
      paste0("hex_to_cnv: ", 
             length(index_channels), 
             " data channels detected (", 
             paste(names(output_channels), collapse = ", "), ")"
      )
    )
  
  values_int <- list()
  
  for(ii in 1:length(hex_channels$start)) {
    
    values_int[[hex_channels$name[ii]]] <- sapply(byte_list, 
                                                  hex_extract_raw_uint, 
                                                  start = hex_channels$start[ii], 
                                                  size = hex_channels$size[ii], 
                                                  big_endian = hex_channels$big_endian[ii], 
                                                  scale = 1, 
                                                  offset = 0)
    
  }
  
  values_int <- as.data.frame(values_int)
  
  temperature  <- integer_to_temperature(
    temperature_integer = values_int$temperature_int,
    sig_figs = 4,
    a0 = cal_par_list[['temperature']]['A0'],
    a1 = cal_par_list[['temperature']]['A1'],
    a2 = cal_par_list[['temperature']]['A2'],
    a3 = cal_par_list[['temperature']]['A3']
  )
  
  pressure <- integer_to_pressure(
    pressure_integer = values_int$pressure_int,
    tvoltage_integer = values_int$tvoltage_int,
    ptempa0 = cal_par_list[['pressure']]['PTEMPA0'],
    ptempa1 = cal_par_list[['pressure']]['PTEMPA1'],
    ptempa2 = cal_par_list[['pressure']]['PTEMPA2'],
    ptca0 = cal_par_list[['pressure']]['PTCA0'],
    ptca1 = cal_par_list[['pressure']]['PTCA1'],
    ptca2 = cal_par_list[['pressure']]['PTCA2'],
    ptcb0 = cal_par_list[['pressure']]['PTCB0'],
    ptcb1 = cal_par_list[['pressure']]['PTCB1'],
    ptcb2 = cal_par_list[['pressure']]['PTCB2'],
    pa0 = cal_par_list[['pressure']]['PA0'],
    pa1 = cal_par_list[['pressure']]['PA1'],
    pa2 = cal_par_list[['pressure']]['PA2'],
    sig_figs = output_sig_digits['pressure'],
    convert_to_dbar = TRUE
  )
  
  conductivity <- integer_to_conductivity(
    conductivity_integer = values_int$conductivity_int,
    temperature = temperature,
    pressure = pressure,
    condg = cal_par_list[['conductivity']]['G'],
    condh = cal_par_list[['conductivity']]['H'],
    condi = cal_par_list[['conductivity']]['I'],
    condj = cal_par_list[['conductivity']]['J'],
    cpcor = cal_par_list[['conductivity']]['CPcor'],
    ctcor = cal_par_list[['conductivity']]['CTcor'],
    par0 = 256,
    par1 = 1000.0,
    sig_figs = output_sig_digits['conductivity']
  )
  
  # Process oxygen and pH if used
  oxygen_voltage <- NULL
  ph <- NULL
  
  if(deployed_do) {
    oxygen_voltage <- integer_to_ox_voltage(values_int$doxygen_int)
  }
  
  if(deployed_ph) {
    ph <- integer_to_ph(ph_integer = values_int$ph_int,
                                 ph_offset = cal_par_list[['ph']]['Offset'],
                                 ph_slope = cal_par_list[['ph']]['Slope'],
                                 temperature = temperature,
                                 sig_figs = output_sig_digits['ph'])
  }
  
  time_elapsed <- seq(0, (length(lines_data)-1)*sample_interval, sample_interval)
  flag <- rep(0, length(lines_data))
  
  cnv_dat <- list(
    data = as.data.frame(
      cbind(
        conductivity,
        temperature,
        pressure,
        oxygen_voltage,
        ph,
        time_elapsed,
        flag
      )
    ),
    hex_path = hex_path,
    sample_interval = sample_interval,
    cal_par_list = cal_par_list,
    header = lines_header,
    output_channels = output_channels,
    output_sig_digits = output_sig_digits
  )
  
  message("hex_to_cnv: Writing data to cnv.\n")
  write_to_cnv(data_list = cnv_dat, output_path = output_path)
  
}



#' Convert SBE integer to ITS-90 temperature
#' 
#' @param temperature_integer Temperature voltage integer
#' @param sig_figs number of significant digits to use for temperature
#' @param a0 Temperature calibration parameter a0
#' @param a1 Temperature Calibration parameter a1
#' @param a2 Temperature calibration parameter a2
#' @param a3 Temperature calibration parameter a3
#' @param offset Temperature calibration parameter offset
#' @param par0 Temperature calibration parameter par0
#' @param par1 Temperature calibration parameter par1
#' @param par2 Temperature calibration parameter par2
#' @param par3 Temperature calibration parameter par3
#' @param par4 Temperature calibration parameter par4
#' @param par5 Temperature calibration parameter par5
#' @export
#' @author Sean Rohan

integer_to_temperature <- function(temperature_integer, 
                                   sig_figs = NULL, 
                                   a0, 
                                   a1, 
                                   a2, 
                                   a3, 
                                   offset = 0,
                                   par0 = 524288, 
                                   par1 = 1.6e7,
                                   par2 = 2.9e9,
                                   par3 = 1.024e8,
                                   par4 = 2.048e4,
                                   par5 = 2e5) {
  
  kelvin_zero_celcius <- 273.15
  
  # Convert temperature integer based on SBE
  t_v <- (temperature_integer - par0)/par1
  t_r <- (t_v * par2 + par3) / (par4 - t_v * par5)
  
  # Calibration equations
  t_x <- 1/(a0 + a1*log(t_r) + a2*log(t_r)^2+a3*log(t_r)^3) - kelvin_zero_celcius
  t_x <- t_x + offset
  
  if(is.numeric(sig_figs)) {
    t_x <- round(t_x, sig_figs)
  }
  
  return(t_x)
}



#' Convert SBE integer to pressure
#' 
#' @param pressure_integer Pressure voltage integer
#' @param tvoltage_integer Temperature voltage integer
#' @param ptempa0 Pressure calibration parameter ptempa0
#' @param ptempa1 Pressure calibration parameter ptempa1
#' @param ptempa2 Pressure calibration parameter ptempa2
#' @param ptca0 Pressure calibration parameter ptca0
#' @param ptca1 Pressure calibration parameter ptca1
#' @param ptca2 Pressure calibration parameter ptca2
#' @param ptcb0 Pressure calibration parameter ptcb0
#' @param ptcb1 Pressure calibration parameter ptcb1
#' @param ptcb2 Pressure calibration parameter ptcb2
#' @param pa0 Pressure calibration parameter pa0
#' @param pa1 Pressure calibration parameter pa1
#' @param pa2 Pressure calibration parameter pa2
#' @param par0 tvoltage_integer conversion constant
#' @param sig_figs number of significant digits to use for temperature (default = 3)
#' @param convert_to_dbar Should pressure be returned in or decibars (TRUE) or pounds per square inch without offset (FALSE)
#' @export
#' @author Sean Rohan

integer_to_pressure <- function(pressure_integer,
                                tvoltage_integer,
                                ptempa0,
                                ptempa1,
                                ptempa2,
                                ptca0,
                                ptca1,
                                ptca2,
                                ptcb0,
                                ptcb1,
                                ptcb2,
                                pa0,
                                pa1,
                                pa2,
                                par0 = 13107,
                                sig_figs = 3,
                                convert_to_dbar = TRUE) {
  
  # Convert integer voltage to actual units
  t_v <- tvoltage_integer/par0
  
  # Calibration equations
  t_x <- ptempa0 + ptempa1 * t_v + ptempa2 * t_v^2
  
  p_x <- pressure_integer - ptca0 - ptca1 * t_x - ptca2 * t_x^2
  
  p_n <- p_x * ptcb0 / (ptcb0 + ptcb1 * t_x + ptcb2 * t_x^2)
  
  # Pressure in pounds per square inch
  pressure <- pa0 + pa1 * p_n + pa2 * p_n ^2
  
  if(convert_to_dbar) {
    pressure <- (pressure - 14.7) * 0.689476
  }
  
  if(is.numeric(sig_figs)) {
    pressure <- round(pressure, sig_figs)
  }
  
  return(pressure)
  
}



#' Convert SBE integer to conductivity
#' 
#' @param conductivity_integer Conductivity voltage integer
#' @param temperature Temperature in degrees C
#' @param pressure Presssure in degrees C
#' @param condg Conductivity calibration parameter condg
#' @param condh Conductivity calibration parameter condh
#' @param condi Conductivity calibration parameter condi
#' @param condj Conductivity calibration parameter condj
#' @param cpcor Conductivity calibration parameter cpcor
#' @param ctcor Conductivity calibration parameter ctcor
#' @param par0 Constant to convert integer to voltage
#' @param par1 Constant to convert integer to voltage
#' @param sig_figs number of significant digits to use for conductivity (default = 6)
#' @export
#' @author Sean Rohan

integer_to_conductivity <- function(conductivity_integer, temperature, pressure, condg, condh, condi, condj, cpcor, ctcor, par0 = 256, par1 = 1000, sig_figs = 6) {
  
  condf <- conductivity_integer / par0 / par1
  cond <- (condg + condh * condf^2 + condi * condf^3 + condj * condf^4) / (1 + ctcor * temperature + cpcor * pressure)
  
  if(is.numeric(sig_figs)) {
    cond <- round(cond, sig_figs)
  }
  
  return(cond)
}



#' Convert SBE integer to pH
#' 
#' @param ph_integer pH voltage integer
#' @param ph_offset pH calibration parameter offset
#' @param ph_slope pH calibration parameter slope
#' @param temperature temperature in degrees C
#' @param sig_figs number of significant digits to use for conductivity (default = 3)
#' @param par0 ph_integer conversion constant
#' @export
#' @author Sean Rohan

integer_to_ph <- function(ph_integer, ph_offset, ph_slope, temperature, sig_figs = 3, par0 = 13107) {
  
  ph_voltage <- ph_integer/par0
  
  ph <- 7.0 + (ph_voltage - ph_offset)/(ph_slope * (temperature + 273.15) * 1.98416e-4)
  
  if(is.numeric(sig_figs)) {
    
    ph <- round(ph, sig_figs)
    
  }
  return(ph)
  
}



#' Extract variable from xmlcon file text
#' 
#' @param header Character vector of lines from an xmlcon file
#' @param cal_par Character vector of the xmlcon tag
#' @param start_block starting index for lines of the header to search
#' @param end_block ending index for lines of the leader to search
#' @param make_numeric Logical. Should the tag value be forced to a numeric?
#' @export
#' @author Sean Rohan

get_calibration_parameter <- function(header, cal_par, start_block = NULL, end_block = NULL, make_numeric = TRUE) {
  
  # Subset header lines between start and end text
  if(!is.null(start_block) & !is.null(end_block)) {
    header <- header[grep(pattern = start_block, x = header):grep(pattern = end_block, x = header)]
  }
  
  lines_par <- header[grep(pattern = paste0("<", cal_par, ">"), header)][1]
  lines_par <- gsub(pattern = paste0(".*<", cal_par,">"), "", lines_par)
  lines_par <- gsub(pattern = paste0("</", cal_par, ">.*"), "", lines_par)
  
  if(make_numeric) {
    lines_par <- as.numeric(lines_par)
  }
  
  return(lines_par)
  
}



#' Writes outputs of hex_to_cnv to a .cnv file
#' 
#' Appends header data.
#' 
#' @param data_list List of data and metadata to write to the cnv file
#' @param output_path Output path for the cnv file 
#' @noRd
#' @author Sean Rohan

write_to_cnv <- function(data_list, output_path) {
  
  dl <- data_list
  
  dl$data <- dl$data[names(dl$output_channels)]
  
  out <- data_list$header
  
  out <- c(out, paste0("# nquan = ", length(dl[['output_channels']])))
  
  out <- c(out, paste0("# nvalues = ", nrow(dl$data)))
  
  if(!is.null(names(dl$output_channels))) {
    out <- c(out, paste0("# units = specified"))
  }
  
  out <- c(out, paste0("# name ", 1:length(dl$output_channels), " = ", dl$output_channels))
  
  out <- c(out, 
           paste0("# span ", 1:length(dl$output_channels), " = ", 
                  paste(
                    sprintf("%010s", apply(dl$data, 2, min)),
                    sprintf("%010s", apply(dl$data, 2, max)),
                    sep = ", ")))
  
  out <- c(out, 
           paste0("# interval = seconds: ", dl$sample_interval))
  
  
  time_index <- grep(pattern = "* cast", x = dl$header)
  
  out <- c(out, paste0("# start_time = ",   
                       substr(x = dl$header[time_index], start = 15, stop = 18),
                       substr(x = dl$header[time_index], start = 12, stop = 14),
                       substr(x = dl$header[time_index], start = 19, stop = 31),
                       " [Instrument's time stamp, header]"))
  out <- c(out, paste0("# bad_flag = -9.990e-29"))
  out <- c(out, paste0("# gapctd_date = ", format(Sys.time(), "%b %d %Y %T"), 
                       ", gapctd ", gsub(pattern = "'", replacement = "", x = packageVersion("gapctd"))))
  out <- c(out, paste0("# gapctd_in = ", dl$hex_path))
  out <- c(out, paste0("# file_type = ascii"))
  out <- c(out, "*END*")
  
  for(ii in 1:ncol(dl$data)) {
    col_index <- which(names(dl$output_sig_digits) %in% names(dl$data)[ii])
    
    dl$data[, col_index ] <- format(dl$data[, col_index ],
                                    nsmall = dl$output_sig_digits[col_index],
                                    width = 11,
                                    trim = FALSE,
                                    justify = "right")
  }
  
  dat_vec <- apply(X = dl$data[, 1:ncol(dl$data)] , MARGIN = 1 , FUN = paste , collapse = "")
  
  out <- c(out, dat_vec)
  
  file_conn <- file(output_path)
  writeLines(out, file_conn)
  close(file_conn)
  
}



#' Calculate oxygen saturation (ml/l)
#' 
#' Calculate oxygen saturation as a function of temperature and salinity based on oxygen solubility from Garcia and Gordon (1992).
#' 
#' @param temperature Temperature (degrees Celsius).
#' @param salinity Salinity (PSU, PSS-78).
#' @export
#' @references Garcia, H.E., Gordon, L.I., 1992. Oxygen solubility in seawater: Better fitting equations. Limnol. Oceanogr. 37, 1307–1312. https://doi.org/10.4319/lo.1992.37.6.1307
#' @author Sean Rohan

oxygen_saturation <- function(temperature, salinity) {
  
  # Constants
  A0 <- 2.00856
  A1 <- 3.22400
  A2 <- 3.99063
  A3 <- 4.80299
  A4 <- 9.78188e-01
  A5 <- 1.71069
  B0 <- -6.24097e-03
  B1 <- -6.93498e-03
  B2 <- -6.90358e-03
  B3 <- -4.29155e-03
  C0 <- -3.11680e-07
  
  Ts <- log((298.15 - temperature) / (273.15 + temperature))
  
  A <- ((((A5 * Ts + A4) * Ts + A3) * Ts + A2) * Ts + A1) * Ts + A0
  
  B <- ((B3 * Ts + B2) * Ts + B1) * Ts + B0
  
  O2 <- exp(A + salinity*(B + salinity*C0))
  
  return(O2)
  
}



#' Calculate oxygen saturation (percent) from dissolved oxygen (ml/l)
#' 
#' Dissolved oxygen divided by oxygen saturation calculated following Garcia and Gordon (1992)
#' 
#' @param oxygen Dissolved oxygen in ml/l
#' @param temperature Temperature (IPTS-68, degrees Celsius).
#' @param salinity Salinity (PSU, PSS-78).
#' @export
#' @references Garcia, H.E., Gordon, L.I., 1992. Oxygen solubility in seawater: Better fitting equations. Limnol. Oceanogr. 37, 1307–1312. https://doi.org/10.4319/lo.1992.37.6.1307
#' @author Sean Rohan

convert_do_to_o2sat <- function(oxygen, temperature, salinity) {
  
  oxsol <- oxygen_saturation(temperature = temperature,
                                      salinity = salinity)
  
  return(oxygen/oxsol)
}



#' Convert SBE integer to dissolved oxygen (ml/l)
#' 
#' @param do_integer Integer value of dissolved oxygen decoded from hex
#' @param tau_correction Should the tau correction (Edwards et al., 2010) be used to account for time-dynamic errors in oxygen?
#' @param temperature Temperature (IPTS-68, degrees Celsius).
#' @param salinity Salinity (PSU, PSS-78).
#' @param pressure Pressure (dbar).
#' @param Voffset Voltage channel offset.
#' @param a Calibration parameter A.
#' @param b Calibration parameter b.
#' @param c Calibration equation parameter C.
#' @param e Calibration equation parameter E.
#' @param soc Calibration equation parameter Soc.
#' @param d0 Optional. Tau correction calibration parameter D0.
#' @param d1 Optional. Tau correction calibration parameter D1.
#' @param d2 Optional. Tau correction calibration parameter D2.
#' @param d0 Optional. Tau correction calibration parameter Tau20.
#' @param sample_interval Sample interval in seconds (default = 0.25).
#' @param sig_figs Optional. Significant digits for output.
#' @noRd
#' @references Edwards, B., Murphy, D., Janzen, C., Larson, A.N., 2010. Calibration, response, and hysteresis in deep-sea dissolved oxygen measurements. J. Atmos. Ocean. Technol. 27, 920–931. https://doi.org/10.1175/2009JTECHO693.1
#' @author Sean Rohan
 
integer_to_dissolved_oxygen <- function(do_integer,
                                        temperature,
                                        pressure,
                                        salinity,
                                        a,
                                        b,
                                        c,
                                        e,
                                        soc,
                                        Voffset,
                                        tau,
                                        tau20 = NULL,
                                        d0 = NULL,
                                        d1 = NULL,
                                        d2 = NULL,
                                        sample_interval = 0.25,
                                        tau_correction = FALSE,
                                        sig_figs = 4
) {
  
  do_voltage <- do_integer/13107
  
  oxsol <- oxygen_saturation(temperature = temperature,
                                      salinity = salinity)
  
  tau <- 0
  dVdt <- c(0, diff(do_voltage)/sample_interval)
  
  if(tau_correction) {
    tau <- DO_tau_correction(temperature, pressure, tau20, d0, d1, d2)
  }
  
  temperature_K <- temperature + 273.15
  
  oxygen_mll <- soc * (do_voltage + Voffset + tau * dVdt) * (1 + a * temperature + b * temperature^2 + c * temperature^3) * oxsol * exp(e*pressure/temperature_K)
  
  if(is.numeric(sig_figs)) {
    oxygen_mll <- round(oxygen_mll, digits = sig_figs)
  }
  
  return(oxygen_mll)
}



#' Tau correction for dissolved oxygen voltage
#' 
#' Tau correction following Edwards et al. (2010).
#' 
#' @param temperature Temperature in degrees C
#' @param pressure Pressure in dbar
#' @param d0 Tau correction calibration parameter D0.
#' @param d1 Tau correction calibration parameter D1.
#' @param d2 Tau correction calibration parameter D2.
#' @param tau20 Tau correction calibration parameter Tau20.
#' @export
#' @references Edwards, B., Murphy, D., Janzen, C., Larson, A.N., 2010. Calibration, response, and hysteresis in deep-sea dissolved oxygen measurements. J. Atmos. Ocean. Technol. 27, 920–931. https://doi.org/10.1175/2009JTECHO693.1
#' @author Sean Rohan

tau_par <- function(temperature, pressure, tau20, d0, d1, d2) {
  tau <- tau20 * d0 * exp(d1 * pressure + d2 * (temperature-20))
}



#' Oxygen integer to raw voltage
#' 
#' @param ox_integer  Integer value of dissolved oxygen decoded from hex
#' @noRd
#' @author Sean Rohan

integer_to_ox_voltage <- function(ox_integer) {
  return(ox_integer/13107)
}
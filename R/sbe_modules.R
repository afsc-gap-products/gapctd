#' Flag slowdowns and reversals (R workflow)
#' 
#' Flags slowdowns and reversals in profiles based on minimum speed.
#' 
#' @param x oce object
#' @param min_speed Numeric vector (1L). Threshold below which observations should be flagged.
#' @param window Numeric vector (1L). Number of samples to use to calculate speeds for each observation.
#' @param cast_direction Character vector ("downcast", "upcast") denoting the cast direction.
#' @return oce object with flags updated to denote scans for which speeds were below the threshold.
#' @export

loop_edit <- function(x, min_speed = 0.1, window = 5, cast_direction = NULL) {
  half_window <- (window-1)/2
  pressure <- x@data$pressure
  timeS <- x@data$timeS
  n_pressure <- length(pressure)
  velocity <- numeric(length = length(pressure))
  
  for(ii in (half_window+1):(n_pressure-half_window)) {
    
    velocity[ii] <- mean(diff(pressure[(ii-half_window):(ii+half_window)])/diff(timeS[(ii-half_window):(ii+half_window)]))
    
  }
  
  if(is.null(cast_direction)) {
    speed <- abs(velocity)
    flag <- ifelse(speed > min_speed, 0, -9)
  }
  
  if(cast_direction == "downcast") {
    flag <- ifelse(velocity > min_speed, 0, -9)
  }
  
  if(cast_direction == "upcast") {
    flag <- ifelse(-1 * velocity > min_speed, 0, -9)
  }

  
  x@data$flag <- flag
  x@data$velocity <- velocity
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
}



#' Median window filter (R workflow)
#' 
#' Applies a median window filter to variables.
#' 
#' @param x oce object
#' @param variables Character vector of data variable names to filter.
#' @param window Numeric vector of windows (i.e. number of scans to use to calculate medians)
#' @return oce object with channels filtered
#' @export

median_filter <- function(x, variables = c("temperature", "conductivity"), window = c(5,5)) {
  
  for(ii in 1:length(variables)) {
    
    half_window <- (window[ii]-1)/2
    var_vec <- x@data[[match(variables[ii], names(x@data))]]
    n_iter <- length(var_vec)
    var_out <- numeric(length = n_iter)
    
    var_out[1:half_window] <- var_vec[1:half_window]
    var_out[(n_iter-half_window+1):n_iter] <- var_vec[(n_iter-half_window+1):n_iter]
    
    for(jj in (half_window+1):(n_iter-half_window)) {
      var_out[jj] <- median(var_vec[max(c(1,jj-half_window)):min(c(n_iter, jj+half_window))])
    }
    
    x@data[[match(variables[ii], names(x@data))]] <- var_out
    
  }
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
  
}



#' Temperature sensor thermal inertia correction (R workflow)
#' 
#' Discrete time filter to correct thermal inertia errors in temperature sensor measurements.
#'
#' @param x oce object that includes timeS, temperature, and conductivity channels.
#' @param alpha_T Alpha parameter for temperature correction (1L numeric)
#' @param beta_C Beta parameter for conductivity correction (1L numeric)
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @return oce object with correction applied to temperature.
#' @export

temperature_correction <- function(x, alpha_T, beta_T, freq_n) {
  
  in_T <- x@data$temperature
  freq_n <- ifelse(is.numeric(freq_n), freq_n, mode(diff(x@data$timeS)))
  
  # Discrete time filter functions for temperature
  temp_par_a <- function(tau, alpha, beta, f_n) {
    return(2 * f_n * alpha * tau * (1 + 2 * f_n * tau)^-1)
  }
  
  temp_par_b <- function(a, alpha) {
    return(1 - 2 * a * alpha^-1)
  }
  
  temp_correct_t_t <- function(a, b, temperature, precision = 4) {
    t_t <- numeric(length = length(temperature))
    t_t[1] <- 0
    for(ii in 2:length(temperature)) {
      t_t[ii] <- -1 * b * t_t[ii-1] + a * (t_t[ii] - t_t[ii-1])
    }
    return(t_t)
  }
  
  T_a <- gapctd::ctm_par_a(alpha = alpha_T, beta = beta_T, f_n = freq_n)
  T_b <- gapctd::ctm_par_b(alpha = alpha_T, a = T_a)

  x@data$T_corr <- gapctd::ctm_correct_t_t(a = T_a, b = T_b, temperature = x@data$temperature)
  x@data$temperature <- in_T + x@data$T_corr
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)

}



#' Conductivity cell thermal inertia correction (R workflow)
#' 
#' Discrete time filter to correct thermal inertia errors in conductivity cell measurements.
#' 
#' @param x oce object that includes timeS, temperature, and conductivity channels.
#' @param alpha_C Alpha parameter for conductivity correction (1L numeric)
#' @param beta_C Beta parameter for conductivity correction (1L numeric)
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @param method Method to use for conductivity cell thermal mass correction. Currently only "seabird" (i.e. equation in SBE Data Processing)
#' @return oce object with correction applied to conductivity.
#' @export

conductivity_correction <- function(x, alpha_C, beta_C, freq_n = 0.25, method = "seabird") {
  
  freq_n <- ifelse(is.numeric(freq_n), freq_n, mode(diff(x@data$timeS)))
  in_C <- x@data$conductivity
  
  
  if(method == "seabird") {
    C_a <- gapctd::ctm_par_a(alpha = alpha_C, beta = beta_C, f_n = freq_n)
    C_b <- gapctd::ctm_par_b(alpha = alpha_C, a = C_a)
    
    x@data$C_corr <- gapctd::ctm_correct_c_t(a = C_a, b = C_b, temperature = x@data$temperature)
    x@data$conductivity <- in_C + x@data$C_corr
    
  }
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)

}



#' Align variables in time (R workflow)
#' 
#' Offset variables in time to account for lags in sensor responses. Variables are interpolated so offsets do not need to be multiples of the scan interval.
#' 
#' @param x oce object
#' @param variables Character vector of data variable names to offset.
#' @param offset Numeric vector of offsets (in seconds) to add.
#' @param method Interpolation method for the approx function to use.
#' @param na_rm Remove scans with NAs in variable channel(s) after alignment.
#' @return Returns an oce object with offsets applied.
#' @export

align_var <- function(x, variables = "temperature", offset = -0.5, interp_method = "linear", na_rm = FALSE) {
  
  in_timeS <- x@data$timeS
  
  for(ii in 1:length(variables)) {
    
    var_vec <- x@data[[match(variables[ii], names(x@data))]]
    
    var_timeS <- in_timeS + offset[ii]
    
    x@data[[match(variables[ii], names(x@data))]] <- approx(x = var_timeS, 
                                                            y = var_vec, 
                                                            xout = in_timeS, 
                                                            method = interp_method)$y
    x@data$flag[is.na(x@data[[match(variables[ii], names(x@data))]])] <- -1
  }
  
  if(na_rm) {
    x@data <- x@data |>
      as.data.frame() |>
      dplyr::filter(flag != -1) |>
      as.list()
  }
  
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
}



#' Low-pass filter (R workflow)
#' 
#' Low-pass filter variables based on time using the filter from SBE data processing.
#' 
#' @param x oce object
#' @param variables Character vector of data variable names to filter.
#' @param time_constant Numeric vector of time constants for filters (in seconds).
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @export

lowpass_filter <- function(x,
                           variables = c("temperature", "conductivity", "pressure"),
                           time_constant = c(0.5, 0.5, 1),
                           precision = c(4, 6, 3),
                           freq_n = 0.25) {
  
  lp_filter <- function(var, tc, freq_n, prec) {
    
    n_var <- length(var)
    aa <- 1 / (1 + 2 * tc * (1/freq_n))
    bb <- (1 - 2 * tc * (1/freq_n)) * aa
    new_var <- numeric(length = n_var)
    new_var[1] <- var[1]
    
    for(jj in 2:n_var) {
      new_var[jj] <- aa*(var[jj]+var[jj-1]) - bb * new_var[jj-1]
    }
    
    new_var <- round(new_var, digits = prec)
    return(new_var)
  }
  
  freq_n <- ifelse(is.numeric(freq_n), freq_n, mode(diff(x@data$timeS)))
  
  for(ii in 1:length(variables)) {
    
    in_var <- x@data[[match(variables[ii], names(x@data))]]
    
    pass_1 <- lp_filter(var = in_var,
                        tc = time_constant[ii],
                        freq_n = freq_n,
                        prec = precision[ii])
    pass_2 <- lp_filter(var = rev(pass_1),
                        time_constant[ii],
                        freq_n = freq_n,
                        prec = precision[ii])
    out_var <- rev(pass_2)
    
    x@data[[match(variables[ii], names(x@data))]] <- out_var
    
  }
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
  
}



#' Derive EOS and GSW (R workflow)
#' 
#' Derive salinity, absolute salinity, sound speed, density, and buoyancy frequency.
#' 
#' @param x oce object
#' @param precision Optional named numeric vector with precision (decimal places) for variables.
#' @export

derive_eos <- function(x, precision = NULL) {
  
  # Set precision
  prec <- c(depth = 3, 
            salinity = 4, 
            absolute_salinity = 4, 
            sound_speed = 2,
            density = 4,
            N2 = 7)
  
  prec[match(names(precision), 
             names(prec))] <- precision
  
  # Derive variables
  x@data$depth <- round(oce::swDepth(pressure = x, 
                                     eos = "unesco"), 
                        digits = prec['depth'])
  x@data$salinity <- round(oce::swSCTp(conductivity = x, 
                                       eos = "unesco"), 
                           digits = prec['salinity'])
  x@data$absolute_salinity <- round(oce::swSCTp(conductivity = x, 
                                                eos = "gsw"), 
                                    digits = prec['absolute_salinity'])
  x@data$sound_speed <- round(oce::swSoundSpeed(salinity = x,
                                                eos = "unesco"), 
                              digits = prec['sound_speed'])
  x@data$density <- round(oce::swRho(salinity = x, 
                                     eos = "unesco"),
                          digits = prec['density'])
  x@data$N2 <- round(oce::swN2(pressure = x),
                     digits = prec['N2'])
  
  return(x)
  
}



#' Bin average: Average variables by depth/pressure bin (R workflow)
#' 
#' Calculate averages for variables by depth or pressure bin.
#'
#' @param x oce object
#' @param by Grouping variable as a character vector ("depth" or "pressure")
#' @param bin_width Width of bins to use for binning.
#' @param exclude_surface Depth or pressure (down is positive) as numeric vector (1L). Scans will be excluded from binning if they are shallower this threshold.
#' @param exclude_bad_flag Logical indicating whether to remove scans with a bad (negative) flag.
#' @param interpolate_missing Interpolate variables when there are no good data for a depth/pressure bin.
#' @param missing_latitude Latitude in decimal degrees to use for depth estimation if oce object does not contain latitude.
#' @return oce object with variables binned and 'flag' column removed.
#' @export

bin_average <- function(x, by = "depth", bin_width = 1, exclude_surface = 0.5, exclude_bad_flag = TRUE, interpolate_missing = TRUE, missing_latitude = 55) {
  
  by <- tolower(by)
  stopifnot("bin_average: Argument 'by' must be \"pressure\" or \"depth\"" = by %in% c("pressure", "depth"))
  
  var_names <- names(x@data)
  
  in_dat <- as.data.frame(x@data)
  bin_index <- which(names(in_dat) == by)
  flag_index <- which(names(in_dat) == "flag")
  
  in_dat <- in_dat[in_dat[, bin_index] > exclude_surface,]
  
  if(exclude_bad_flag) {
    in_dat <- in_dat[in_dat[, flag_index] == 0,]
  }
  
  # Round to the nearest bin
  in_dat$bin <- plyr::round_any(in_dat[, bin_index], bin_width)
  
  out_dat <- in_dat |>
    dplyr::group_by(bin) |>
    dplyr::summarise(dplyr::across(.cols = dplyr::everything(), 
                                   .fns = list(mean = mean),
                                   .names = "{.col}",
                                   na.rm = TRUE))
  
  out_dat <- out_dat[, -1*which(names(out_dat) %in% c("pressure", "depth", "flag"))]
  
  # Interpolate missing
  bin_range <- range(out_dat$bin, na.rm = TRUE)
  
  interp_dat <- data.frame(bin = bin_range[1]:bin_range[2])
  interp_dat$flag <- ifelse(interp_dat$bin %in% out_dat$bin, 0, 1)
  out_dat <- dplyr::left_join(interp_dat, out_dat, by = "bin")
  
  interp_col_index <- which(!(names(out_dat) %in% c("flag", "bin")))
  
  for(jj in interp_col_index) {
    
    out_dat[jj] <- oce::oce.approx(x = out_dat$bin, 
                                            y = out_dat[, jj],
                                            xout = out_dat$bin,
                                            method = "unesco")
  }
  
  out_dat <- out_dat
  names(out_dat)[names(out_dat) == "bin"] <- by
  
  # Recalculate pressure or depth.
  if(by == "pressure") {
    
    if(is.na(x@metadata$latitude)) {
      warning(paste0("bin_average: No latitude in metadata for oce object. Using missing_latitude (", 
                     missing_latitude,  
                     ") instead"))
    }
    
    out_dat$depth <- round(oce::swDepth(pressure = out_dat$pressure, 
                                  eos = "unesco", 
                                  latitude = ifelse(is.na(x@metadata$latitude), 
                                                    missing_latitude, 
                                                    x@metadata$latitude)), 3)
  }
  
  if(by == "depth") {
    out_dat$pressure <- round(oce::swPressure(depth = out_dat$depth, 
                                        eos = "unesco", 
                                        latitude = ifelse(is.na(x@metadata$latitude), 
                                                          missing_latitude, 
                                                          x@metadata$latitude)), 3)
  }
  
  # Replace data with binned variables
  x@data <- as.list(out_dat)
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
  
}



#' Section (R workflow)
#' 
#' Extract scans within a time or scan interval from an oce object. Time- is time elapsed since measurements started.
#' 
#' @param x oce object
#' @param by Sectioning variable as a character vector ("timeS", "scan", "datetime")
#' @param start Numeric or POSIXct. Start time or scan to include.
#' @param end Numeric or POSIXct. End time or scan to include.
#' @param cast_direction Cast direction ("downcast", "upcast", "bottom)
#' @return An oce object that only includes variables that were collected within the time/scan interval.
#' @export

section_oce <- function(x, by = "timeS", start = NULL, end = NULL, cast_direction = NULL) {
  
  by <- tolower(by)
  stopifnot("section_oce: Argument 'by' must be \"timeS\", \"scan\", or \"datetime\"" = by %in% c("times", "scan", "datetime"))
  
  if(by == "datetime" & is.null(start) & is.null(end)) {
    
    if(cast_direction == "downcast") {
      start <- x@metadata$race_metadata$dc_start
      end <- x@metadata$race_metadata$dc_end
    }

    if(cast_direction == "upcast") {
      start <- x@metadata$race_metadata$uc_start
      end <- x@metadata$race_metadata$uc_end
    }
    
    if(cast_direction == "bottom") {
      start <- x@metadata$race_metadata$dc_end
      end <- x@metadata$race_metadata$uc_start
    }
    
    stopifnot("section_oce: x must contain a data.frame named race_metadata in the metadata field." = "race_metadata" %in% names(x@metadata))
    stopifnot("section_oce: start and end must have class POSIXct when 'by = \"datetime\"' " = 
                (class(start)[1] == "POSIXct" & class(end)[1] == "POSIXct"))
  }
  
  in_dat <- as.data.frame(x@data)
  
  if(by == "times") {
    out_dat <- dplyr::filter(in_dat, timeS >= start & timeS <= end)
  }
  
  if(by == "scan") {
    out_dat <- in_dat[start:end,]
  }
  
  if(by == "datetime") {
    scan_time <- as.POSIXct(x@metadata$startTime) + in_dat$timeS
    out_dat <- in_dat[(scan_time > start & scan_time < end), ]
  }
  
  x@data <- as.list(out_dat)
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  # Remove bad cast
  bad_profile <- FALSE
  if(!(length(x@data[[1]]) > 1)) {
    # No scans
    warning(paste0("No scans in ", cast_direction))
    bad_profile <- TRUE
  } else {
    if(cast_direction != "bottom") {
      if(diff(range(x@data$pressure)) < 5) {
        # Total pressure range less than 5 dbar
        warning(paste0("Insufficient pressure range in ", cast_direction))
        bad_profile <- TRUE
      }
      
      if(min(x@data$pressure) > 5) {
        # Minimum pressure greater than 5 dbar
        warning(paste0("Insufficient pressure range in ", cast_direction))
        bad_profile <- TRUE
      }
    }
  }
  
  # Return NULL if the profile is bad
  if(bad_profile) {
    x <- NULL
  }
  
  return(x)
  
}
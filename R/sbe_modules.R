#' Flag slowdowns and reversals
#' 
#' Flags slowdowns and reversals in profiles based on minimum speed.
#' 
#' @param x oce object
#' @param min_speed Numeric vector (1L). Threshold below which observations should be flagged.
#' @param window Numeric vector (1L). Number of samples to use to calculate speeds for each observation.
#' @param cast_direction Character vector ("downcast", "upcast") denoting the cast direction.
#' @return oce object with flags updated to denote scans for which speeds were below the threshold.
#' @noRd

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

#' Median window filter
#' 
#' Applies a median window filter to variables.
#' 
#' @param x oce object
#' @param variables Character vector of data variable names to filter.
#' @param window Numeric vector of windows (i.e. number of scans to use to calculate medians)
#' @return oce object with channels filtered
#' @noRd

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

#' Temperature sensor thermal inertia correction
#' 
#' Discrete time filter to correct thermal inertia errors in temperature sensor measurements.
#'
#' @param x oce object that includes timeS, temperature, and conductivity channels.
#' @param alpha_T Alpha parameter for temperature correction (1L numeric)
#' @param beta_C Beta parameter for conductivity correction (1L numeric)
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @return oce object with correction applied to temperature.
#' @noRd

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

#' Conductivity cell thermal inertia correction
#' 
#' Discrete time filter to correct thermal inertia errors in conductivity cell measurements.
#' 
#' @param x oce object that includes timeS, temperature, and conductivity channels.
#' @param alpha_C Alpha parameter for conductivity correction (1L numeric)
#' @param beta_C Beta parameter for conductivity correction (1L numeric)
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @param method Method to use for conductivity cell thermal mass correction. Currently only "seabird" (i.e. equation in SBE Data Processing)
#' @return oce object with correction applied to conductivity.
#' @noRd

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


#' Align variables in time
#' 
#' Offset variables in time to account for lags in sensor responses. Variables are interpolated so offsets do not need to be multiples of the scan interval.
#' 
#' @param x oce object
#' @param variables Character vector of data variable names to offset.
#' @param offset Numeric vector of offsets (in seconds) to add.
#' @param method Interpolation method for the approx function to use.
#' @return Returns an oce object with offsets applied.
#' @noRd

align_var <- function(x, variables = "temperature", offset = -0.5, interp_method = "linear") {
  
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
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
}

#' Low-pass filter
#' 
#' Low-pass filter variables based on time using the filter from SBE data processing.
#' 
#' @param x oce object
#' @param variables Character vector of data variable names to filter.
#' @param time_constant Numeric vector of time constants for filters (in seconds).
#' @param freq_n Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @noRd

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

#' Bin average: Average variables by depth/pressure bin
#' 
#' Calculate averages for variables by depth or pressure bin.
#'
#' @param x oce object
#' @param by Grouping variable as a character vector ("depth" or "pressure")
#' @param bin_width Width of bins to use for binning.
#' @param exclude_surface Depth or pressure (down is positive) as numeric vector (1L). Scans will be excluded from binning if they are shallower this threshold.
#' @param exclude_bad_flag Logical indicating whether to remove scans with a bad (negative) flag.
#' @return oce object with variables binned and 'flag' column removed.
#' @noRd

bin_average <- function(x, by = "depth", bin_width = 1, exclude_surface = 0.5, exclude_bad_flag = TRUE) {

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
                                   .names = "{.col}"))

  out_dat <- out_dat[, -1*which(names(out_dat) %in% c("pressure", "depth", "flag"))]
  names(out_dat)[names(out_dat) == "bin"] <- by
  
  # Recalculate pressure or depth.
  if(by == "pressure") {
    
  }
  
  if(by == "depth") {
    
  }
  
  # Replace data with binned variables
  x@data <- as.list(out_dat)
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)

}

#' Section
#' 
#' Extract scans within a time or scan interval from an oce object. Time- is time elapsed since measurements started.
#' 
#' @param x oce object
#' @param by Sectioning variable as a character vector ("timeS", "scan", "datetime")
#' @param start Numeric or POSIXct. Start time or scan to include.
#' @param end Numeric or POSIXct. End time or scan to include.
#' @return An oce object that only includes variables that were collected within the time/scan interval.
#' @noRd

section_oce <- function(x, by = "timeS", start, end) {
  
  by <- tolower(by)
  stopifnot("section_oce: Argument 'by' must be \"timeS\", \"scan\", or \"datetime\"" = by %in% c("times", "scan", "datetime"))
  stopifnot("section_oce: start and end must have class POSIXct when 'by = \"datetime\"' " = 
              (by == "datetime" & (class(start)[1] == "POSIXct" & class(end)[1] == "POSIXct")))
  
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
  
  return(x)
  
}

#' Get haul data from RACEBASE
#' 
#' Retrieves vessel, cruise, haul, event times, locations, bottom depth, gear temperature, surface temperature, performance, haul type, and haul start time from racebase and race_data.
#' 
#' @param channel RODBC connection.
#' @param vessel Vessel code as a 1L numeric vector.
#' @param cruise Cruise code as a numeric vector (can include more than one).
#' @param out_path Optional. Filepath for the output R data file (.rds).
#' @return Returns a data.frame containing haul data. Also saves haul data to an rds file.

get_haul_data <- function(channel, vessel, cruise, out_path = NULL) {
  
  haul_dat <- RODBC::sqlQuery(channel = channel,
                              query =   paste0(
                                "select a.vessel, a.cruise, a.haul, a.bottom_depth, a.gear_temperature, a.surface_temperature, a.performance, a.haul_type, a.start_time, a.start_latitude, a.start_longitude, a.end_latitude, a.end_longitude, c.date_time, c.event_type_id, e.name
from racebase.haul a, race_data.cruises b, race_data.events c, race_data.hauls d, race_data.event_types e
where a.vessel = ", vessel, "and a.cruise in (", paste(cruise, collapse = ","), ") and a.vessel = b.vessel_id and a.cruise = b.cruise and c.haul_id = d.haul_id and d.haul = a.haul and d.cruise_id = b.cruise_id and c.event_type_id = e.event_type_id and c.event_type_id in (3,6,7)"))
  
  haul_dat <- haul_dat |>
    dplyr::inner_join(data.frame(EVENT_TYPE_ID = c(3,6,7),
                                 EVENT_NAME = c("ON_BOTTOM", "HAULBACK", "OFF_BOTTOM")),
                      by = "EVENT_TYPE_ID") |>
    tidyr::pivot_wider(id_cols = c("VESSEL", "CRUISE", "HAUL"),
                       names_from = c("EVENT_NAME"),
                       values_from = "DATE_TIME") |>
    dplyr::inner_join(haul_dat, by = c("VESSEL", "CRUISE", "HAUL")) |>
    dplyr::select(-NAME, -EVENT_TYPE_ID, -DATE_TIME) |>
    unique() |>
    as.data.frame()
  
  
  if(is.null(out_path)) {
    out_path <- here::here("output", paste0("HAUL_DATA_", vessel, "_", paste(cruise, collapse = "_"), ".rds"))
  }

  message(paste0("get_haul_data: Writing haul data to ", out_path, ". ",   nrow(haul_dat), " haul records." ))
  
  saveRDS(haul_dat, file = out_path)
  
  return(haul_dat)
}


# gapctd::get_haul_data(vessel = 162,
#                       cruise = c(202201, 202202),
#                       channel = gapctd::get_connected(schema = "AFSC")) 
# 
# ctd_dat <- read.oce(file = here::here("data", "sbe19plus_01908103_2021_06_01_94_0005_raw.cnv")) |>
#   gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
#                           time_constant = c(0.5, 0.5, 1),
#                           precision = c(4, 6, 3),
#                           freq_n = 0.25) |>
#   gapctd:::median_filter(variables = c("temperature", "conductivity"),
#                          window = c(5,5)) |>
#   gapctd:::align_var(variables = "temperature", offset = -0.5, interp_method = "linear") |>
#   gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
#   gapctd:::section_oce(by = "datetime",
#                       start = as.POSIXct("2021-06-01 18:02:15", tz = "UTC"),
#                       end = as.POSIXct("2021-06-01 18:10:00", tz = "UTC")) |>
#   gapctd:::bin_average(by = "pressure", bin_width = 1, exclude_surface = 0.5, exclude_bad_flag = TRUE)



#' Split
#' 
#' 
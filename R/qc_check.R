#' Check profile for completeness (R workflow)
#' 
#' Assign quality control/quality assurance flag to profile based on whether the data are complete.
#' 
#' @param x oce object
#' @param prop_max_flag Proportion of bins with flagged/interpolated data. Default = 0.1
#' @param prop_min_bin Minimum proportion of depth bins with data relative to depth. Default = 0.875
#' @param time_diff_max Maximum allowable time difference between on-bottom event and cast time.
#' @return Returns oce object with metadata$flags field updated with flags
#' @export

qc_check <- function(x, prop_max_flag = 0.1, prop_min_bin = 0.875, time_diff_max = 45) {
  
  if(is.null(x)) {
    return(NULL)
  }
  
  prop_bins <- length(x@data$depth)/(x@metadata$gearDepth/as.numeric(x@metadata$bin_average['bin_width']))
  
  x@metadata$flags[['bin_flag']] <- ifelse(prop_bins < prop_min_bin, TRUE, FALSE)
  
  x@metadata$flags[['missing_flag']] <-   ifelse(sum(x@data$flag != 0)/length(x@data$flag) > prop_max_flag, TRUE, FALSE)
  
  x@metadata$flags[['time_flag']] <- ifelse(abs(difftime(x@metadata$startTime, 
                                                             x@metadata$race_metadata$dc_end, units = "mins")) > time_diff_max, TRUE, FALSE)
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
  
}



#' Manually flag and interpolate bad data (R workflow)
#' 
#' Visually inspect plots of depth versus salinity, and depth versus pressure. Flag data for removal in a GUI and interpolate missing data. Write interpolated data to output/manual_flag
#' 
#' @param x oce object
#' @param review variable to review ("density", "temperature" or "salinity"). Default = "density"
#' @return An oce object with flagged scans/bins interpolated and derived quantities recalculated. Replaced scans/bins have flag = 7.
#' @export

qc_flag_interpolate <- function(x, review = c("density"), bin_var = "depth") {
  
  interp_flags <- function(x_oce, bin_var) {
    
    if(sum(x_oce@data$flag < 0) > 0) {
      
      if(bin_var == "depth") {
        x_input <- x_oce@data$depth
      } else if(bin_var == "pressure") {
        x_input <- x_oce@data$pressure
      }
      
      flag_index <- which(x_oce@data$flag < 0)
      x_oce@data$temperature[flag_index] <- oce::oce.approx(x = x_oce@data$depth[-flag_index], 
                                                            y = x_oce@data$temperature[-flag_index], 
                                                            xout = x_oce@data$depth[flag_index], 
                                                            method = "unesco")
      x_oce@data$conductivity[flag_index] <- oce::oce.approx(x = x_oce@data$depth[-flag_index], 
                                                             y = x_oce@data$conductivity[-flag_index], 
                                                             xout = x_oce@data$depth[flag_index], 
                                                             method = "unesco")
      x_oce <- x_oce |> gapctd:::derive_eos()
      
      if(bin_var == "depth") {
        x_oce@data$depth <- x_input
      } else if(bin_var == "pressure") {
        x_oce@data$pressure <- x_input
      }
      
      x_oce@data$flag[flag_index] <- 7
    }
    

    
    return(x_oce)
  }
  
  if("density" %in% review) {
    
    loop_ind <- 1
    while(loop_ind == 1) {
      
      par(mfrow = c(1,3))
      plot(x@data$temperature, -1*x@data$pressure, 
           xlab = "Temperature", 
           ylab = "Pressure",
           main = " ",
           col = "red")
      abline(h = -1, lty = 2)
      lines(x = x@data$temperature, 
            y = -1*x@data$pressure,
            col = "red")
      plot(x@data$salinity, -1*x@data$pressure, 
           xlab = "Salinity", 
           ylab = "Pressure",
           main = " ",
           col = "darkgreen")
      abline(h = -1, lty = 2)
      lines(x = x@data$salinity, 
            y = -1*x@data$pressure,
            col = "darkgreen")
      plot(x = x@data$density, 
           y = -1*x@data$pressure, 
           xlab = "Density", 
           ylab = "Pressure",
           main = "Left-click on points to be removed then press 'Esc'",
           col = "blue")
      abline(h = -1, lty = 2)
      lines(x = x@data$density, 
            y = -1*x@data$pressure,
            col = "blue")
      
      new_flags <- identify(x@data$density, -1*x@data$pressure)
      
      
      if(!(length(new_flags) > 0)) {
        loop_ind <- 0
        next
      }
      
      x@data$flag[new_flags] <- -7
      
      x <- interp_flags(x_oce = x, bin_var = bin_var)
    }
  }
  
  if("temperature" %in% review) {
    
    loop_ind <- 1
    while(loop_ind == 1) {
      
      par(mfrow = c(1,2))
      plot(diff(x@data$temperature)/diff(x@data$pressure),
           y = (length(x@data$temperature)-1):1,
           type = 'p',
           xlab = expression(d*"T"/d*rho),
           ylab = "Index")
      abline(h = -1, lty = 2)
      lines(diff(x@data$temperature)/diff(x@data$pressure),
            y = (length(x@data$temperature)-1):1)
      plot(x@data$temperature, -1*x@data$pressure,
           xlab = "Temperature",
           ylab = "Pressure",
           col = "red",
           main = "Left-click on points to be removed then press 'Esc'")
      abline(h = -1, lty = 2)
      lines(x = x@data$temperature, y = -1*x@data$pressure)
      
      new_flags <- identify(x@data$temperature, -1*x@data$pressure)
      
      
      if(!(length(new_flags) > 0)) {
        loop_ind <- 0
        next
      }
      
      x@data$flag[new_flags] <- -7
      x <- interp_flags(x_oce = x, bin_var = bin_var)
    }
  }
  
  if("salinity" %in% review) {
    loop_ind <- 1
    while(loop_ind == 1) {
      
      par(mfrow = c(1,2))
      plot(diff(x@data$salinity)/diff(x@data$pressure),
           y = (length(x@data$salinity)-1):1,
           type = 'p',
           xlab = expression(d*S/d*rho),
           ylab = "Index")
      abline(h = -1, lty = 2)
      lines(diff(x@data$salinity)/diff(x@data$pressure),
            y = (length(x@data$salinity)-1):1,)
      plot(x@data$salinity, -1*x@data$pressure,
           xlab = "Salinity",
           ylab = "Pressure",
           col = "darkgreen",
           main = "Left-click on points to be removed then press 'Esc'")
      abline(h = -1, lty = 2)
      lines(x = x@data$salinity, y = -1*x@data$pressure)
      
      new_flags <- identify(x@data$salinity, -1*x@data$pressure)
      
      if(!(length(new_flags) > 0)) {
        loop_ind <- 0
        next
      }
      
      x@data$flag[new_flags] <- -7
      
      x <- interp_flags(x_oce = x, bin_var = bin_var)
    }
  }
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call(sys.parent(n=1))))
  
  return(x)
  
}



#' Wrapper for qc_flag_interpolate (R workflow)
#' 
#' Visually inspect and interpolate errors. See documentation for gapctd:::qc_flag_interpolate()
#' 
#' @param rds_dir_path Filepath to directory containing rds files to be reviewed.
#' @param in_pattern Character vector search pattern for input files.
#' @param append_pattern Characters to append to output file name.
#' @param review Passed to qc_flag_interpolate(). Variable to review ("density", "temperature" or "salinity"). Default = "density"
#' @param bin_var Name of variable that was used to bin data ("depth" or "pressure").
#' @return rds files with flagged conductivity and temperature removed and interpolated; derived quantities recalculated.
#' @export

wrapper_flag_interpolate <- function(rds_dir_path,
                                     in_pattern = NULL,
                                     append_pattern = NULL,
                                     review = c("density", "salinity"),
                                     bin_var = "depth") {
  
  if(is.null(in_pattern)) {
    in_pattern <- "_best.rds"
  }
  
  if(is.null(append_pattern)) {
    append_pattern <- "_qc.rds"
  }
  
  rds_files <- list.files(rds_dir_path, pattern = in_pattern, full.names = TRUE)
  rds_short <- list.files(rds_dir_path, pattern = in_pattern, full.names = FALSE)
  output_files <- here::here(rds_dir_path,  gsub(pattern = in_pattern, 
                                                 replacement = append_pattern, 
                                                 x = rds_short))
  
  for(JJ in 1:length(rds_files)) {
    if(!file.exists(output_files[JJ])) {
      
      # Load CTD data
      ctd_dat <- readRDS(file = rds_files[JJ])
      
      if(!any(c("upcast", "downcast") %in% names(ctd_dat))) {
        message(paste0("skipping ", rds_short[JJ]))
        next
      }
      
      message(paste0("Processing ", rds_short[JJ]))
      
      if("downcast" %in% names(ctd_dat)) {
        ctd_dat$downcast <- gapctd:::qc_flag_interpolate(ctd_dat$downcast, review = review, bin_var = bin_var)
      }
      
      if("upcast" %in% names(ctd_dat)) {
        ctd_dat$upcast <- gapctd:::qc_flag_interpolate(ctd_dat$upcast, review = review, bin_var = bin_var)
      }
      
      saveRDS(ctd_dat, file = output_files[JJ])
    } else {
      message(paste0("skipping ", rds_short[JJ]))
    }
  }
}

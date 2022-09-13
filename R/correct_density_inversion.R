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



#' Check density inversion (R workflow)
#' 
#' @param x oce object
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses. GTSPP uses a density threshold of 0.05.
#' @param threshold_method Character. "density" to flag inversions based on density anomalies, "bv" for Brunt-Vaisala buoyancy frequency
#' @param correct_inversion Logical. Should density inversions be removed and interpolated?
#' @return oce object with density inversions flaggd and/or interpolated
#' @noRd

check_density_inversion <- function(x, threshold  = -1e-4, threshold_method = "bv", correct_inversion = TRUE) {
  
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
  
  # Flag based on threshold method
  flag_density <- switch(threshold_method,
                         `bv` = {which(x@data$N2 < threshold)},
                         `density` = {which(diff(x@data$density[rank(x@data$pressure)]) < threshold)})
  
  # Counter to break the loop if automatic corrections get stuck
  flag_loop <- 1
  
  while(length(flag_density) > 0 & flag_loop < 3) {
    
    if(threshold_method == "density") {
      x@data$flag[rank(x@data$pressure)][flag_density+1] <- -1*(correct_inversion *-1)
    } else if(threshold_method == "bv") {
      # Flag but don't interpolate N2 anomalies at pressures < 20 db
      x@data$flag[flag_density] <- -6*(correct_inversion)
      x@data$flag[x@data$flag == -6*(correct_inversion) & x@data$pressure < 20] <- 6*(correct_inversion)
    }
    
    flags <- which(x@data$flag == -6*(correct_inversion))
    
    names(x@data)
    
    if(correct_inversion & length(flags) > 0) {
      
      x@data$temperature[flags] <- oce::oce.approx(x = x@data$depth[-flags], 
                                                   y = x@data$temperature[-flags], 
                                                   xout = x@data$depth[flags], 
                                                   method = "unesco")
      x@data$conductivity[flags] <- oce::oce.approx(x = x@data$depth[-flags], 
                                                    y = x@data$conductivity[-flags], 
                                                    xout = x@data$depth[flags], 
                                                    method = "unesco")
      
      # Re-calculate N2
      x <- gapctd:::derive_eos(x = x)
      
      # Re-flag
      flag_density <- switch(threshold_method,
                             `bv` = {which(x@data$N2 < threshold & x@data$pressure > 20)},
                             `density` = {which(diff(x@data$density[rank(x@data$pressure)]) < threshold)})
    } else {
      flag_density <- 0
    }
    
    flag_loop <- flag_loop + 1
  }
  
  return(x)
}

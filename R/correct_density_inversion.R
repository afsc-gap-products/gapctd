#' Check density inversion (R workflow)
#' 
#' @param x oce object
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses. GTSPP uses a density threshold of 0.05.
#' @param threshold_method Character. "density" to flag inversions based on density anomalies, "bv" for Brunt-Vaisala buoyancy frequency
#' @param correct_inversion Logical. Should density inversions be removed and interpolated?
#' @return oce object with density inversions flaggd and/or interpolated
#' @export
#' @author Sean Rohan

check_density_inversion <- function(x, threshold  = -1e-5, threshold_method = "bv", correct_inversion = TRUE) {
  
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
      
      x_oce@data$flag[flag_index] <- 6
    }
    
    return(x_oce)
  }
  
  if(is.null(x)) {
    return(NULL)
  }
  
  threshold_method <- tolower(threshold_method)
  
  # Check that inputs are sensible
  if(sign(threshold) == 1) {
    stop("correct_density_inversion: Max inversion must be positive but is set to ", threshold, "!")
  }
  
  if(threshold_method == "bv" & threshold < -1e-5) {
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
      
      interp_flags(x_oce = x, bin_var = "depth")
      
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

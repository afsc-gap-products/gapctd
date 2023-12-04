#' Flag slowdowns and reversals
#' 
#' Flags slowdowns and reversals in profiles based on minimum speed.
#' 
#' @param x ctd object
#' @param min_speed Numeric vector (1L). Threshold below which observations should be flagged.
#' @param window Numeric vector (1L). Number of samples to use to calculate speeds for each observation.
#' @param cast_direction Character vector ("downcast", "upcast") denoting the cast direction.
#' @param exclude_bottom Pressure width to exclude near-bottom. This is used because it can takes awhile for the CTD to start ascending when the net approaches or leaves the seafloor, but the contact between the net and the seafloor generally does not cause influential flow reversals.
#' @return ctd object with flags updated to denote scans for which speeds were below the threshold.
#' @export
#' @author Sean Rohan

slowdown <- function(x, min_speed = 0.1, window = 5, cast_direction = NULL, exclude_bottom = 2) {
  
  if(is.null(x)) {
    return(x)
  }
  
  half_window <- (window-1)/2
  pressure <- x@data$pressure
  max_pressure <- max(pressure, na.rm = TRUE)
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
  
  flag[(max_pressure - pressure) < exclude_bottom] <- 0
  
  x@data$flag <- flag
  x@data$velocity <- velocity
  
  x@processingLog$time <- c(x@processingLog$time, Sys.time())
  x@processingLog$value <- c(x@processingLog$value, deparse(sys.call()))
  
  return(x)
}
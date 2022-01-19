#' Calculate bottom layer depth from temperature using a threshold
#' 
#' Based on the temperature recorded at a specified difference from the maximum depth for a profile.
#' 
#' @param temperature Numeric vector of temperatures
#' @param z Numeric vector of depths for each of the temperatures
#' @param ref_distance_from_max Numeric vector of distances from the maximum depth to use for the reference temperature.
#' @param min_temperature_diff Minimum temperature difference. If the difference in temperatures between the reference depths and shallow depths. If the difference is below this threshold, the function returns the minimum depth (i.e. column is considered fully mixed).
#' @param temp_threshold Threshold temperature difference.
#' @export

profile_bld_from_t <- function(temperature,
                                 z,
                                 ref_dist_from_max = 4,
                                 min_temperature_diff = 0.5,
                                 temp_threshold = 0.25) {
  
  bld <- NA
  z_above_bld <- NA
  
  if(diff(range(temperature[z < max(z)-ref_dist_from_max])) < min_temperature_diff) {
    bld <- min(z)
  } else {
    ref_temperature <- temperature[z %in% (max(z)-ref_dist_from_max)]
    z_above_bld <- max(z[temperature - ref_temperature > temp_threshold & z < max(z)-ref_dist_from_max])
    if(z_above_bld == min(z)) {
      z_below_bld <- min(z)
    } else{
      z_below_bld <- max(z[z < z_above_bld])
    }
    bld <- (z_below_bld + z_above_bld)/2    
  }
  
  # Corner case when temperature near bottom is higher than surface layer, but not stratified.
  if(is.infinite(z_above_bld)) {
    bld <- min(z)
  }
  
  # Case where bottom layer is right at the bottom
  if(is.infinite(bld)) {
    if(((rank(z)[z == min((max(z)-ref_dist_from_max))] - rank(z)[z == z_above_bld]) == 1)) {
      alt_index <- rank(z)[z == min((max(z)-ref_dist_from_max))]
      bld <- (z[rank(z) == alt_index] + z[rank(z) == (alt_index+1)])/2
    }
  }
  
  return(bld)
  
}

#' Calculate bottom layer depth using a threshold method
#'
#' @param rho Numeric vector of densities
#' @param z Numeric vector of depths. Depths are positive.
#' @param ref.depth Thickness of bottom layer to use for calculating bottom layer density
#' @param totdepth Maximum depth sampled by the cast
#' @param threshold Density threshold
#' @export 

profile_bld <- function(rho, z, totdepth, threshold = 0.1, ref.depth = 5) {
  rho <- rho[order(z)]
  z <- z[order(z)]
  z.max <- max(z)
  bottom.rho <- mean(rho[z >= (z.max - ref.depth)])
  bld.bin <- which(rho < (bottom.rho - threshold) & z < (z.max - ref.depth))
  
  if(length(bld.bin) > 0) {
    bld <- z[max(bld.bin)]
  } else {
    if(length(totdepth) > 0) {
      bld <- 0
    } else {
      bld <- NA
    }
  }
  return(bld)
}

#' Calculate density difference between reference depth and 30 m below mixed-layer depth (or bottom)
#' 
#' @param rho Density vector
#' @param z Depth vector (positive depths)
#' @param mld Vector of mixed layer depth
#' @param ref.depth Reference depth for calculating mixed layer density.
#' @param mld.buffer Depth difference between upper and lower
#' @export 

# Calculate density difference between the reference depth (5 m) and MLD + buffer (30 m below mld), following Cokelet (2016)
profile_cokelet_density_diff <- function(rho, z, mld, ref.depth = 5, mld.buffer = 30) {
  rho.upper <- NA
  rho.lower <- NA
  if(min(abs(z-ref.depth)) < 3){
    rho.upper <- mean(rho[z <= ref.depth])
  }
  lower.ref <- mld + 30
  if(lower.ref < max(z)) {
    rho.lower <- mean(rho[z > lower.ref])
  } else {
    rho.lower <- rho[z == max(z)]
  }
  diff.rho <- rho.lower - rho.upper
  return(diff.rho)
}

#' Calculate mixed layer depth from temperature using a threshold
#' 
#' Based on the temperature recorded at a specified reference depth, calculates the mixed layer depth using only temperature.
#' 
#' @param temperature Numeric vector of temperatures
#' @param z Numeric vector of depths for each of the temperatures
#' @param reference_depth Numeric vector of reference depth, can either be a single depth or multiple depths.
#' @param min_temperature_diff Minimum temperature difference. If the difference in temperatures between the reference depths and other depths is below this threshold. If the difference is below this threshold, the function returns the maximum depth (i.e. column is considered fully mixed).
#' @param temp_threshold Threshold temperature difference 
#' @param assign_inversion Substitutes maximum depth when temperature deep in the water column is higher than reference. Rare occurrence that may be an artifact of the survey sampling scheme.
#' @export 

profile_mld_from_t <- function(temperature,
                                 z,
                                 reference_depth = 5,
                                 min_temperature_diff = 0.5,
                                 temp_threshold = 0.25,
                                 assign_inversion) {
  
  if(diff(range(temperature[z >= max(reference_depth)])) < min_temperature_diff) {
    mld <- max(z)
  } else {
    ref_index <- which(z %in% reference_depth)
    ref_temperature <- mean(temperature[ref_index])
    z_below_mld <- min(z[ref_temperature - temperature > temp_threshold & z > max(reference_depth)])
    z_above_mld <- max(z[z < min(z_below_mld) & z >= max(reference_depth)])
    mld <- (z_below_mld + z_above_mld)/2
    
    # Corner case when temperature near bottom is higher than surface layer, but not stratified.
    if(is.infinite(z_below_mld)) {
      mld <- max(z)
    }
    
  }
  
  return(mld)
  
}

#' Calculate pycnocline depth as the maximum rate of increase in density at depths below the pycnocline and below the reference depth
#' 
#' Find the depth of the pycnocline as the depth where density is changing the fastest
#'
#' @param rho Numeric vector of density
#' @param z Numeric vector of depths
#' @mld Mixed layer depth
#' @ref.depth Reference depth for upper layer density
#' @export 

profile_pycnocline <- function(rho, z, mld = NULL, ref.depth = 5) {
  
  # Don't calculate pycnocline if MLD goes to the bottom
  if(mld < max(z)) {
    rho <- rho[order(z)]
    z <- z[order(z)]
    
    # Filter by reference depth
    rho <- rho[z > ref.depth]
    z <- z[z > ref.depth]
    
    if(!is.null(mld)) {
      rho <- rho[z > mld]
      z <- z[z > mld]
    }
    
    delta.rho <- diff(rho)/diff(z)
    if(length(delta.rho) == 0) { 
      # Case where MLD is at the bottom
      return(NA)
    } else if(which.max(delta.rho) == 1) {
      # Case where pycnocline is at the shallowest depth
      return(z[which.max(delta.rho)]-0.5)
    } else {
      # All other cases
      return((z[which.max(delta.rho)-1] + z[which.max(delta.rho)])/2)
    }
  } else {
    # Fully-mixed water column
    return(NA)
  }
}

#' Calculate temperature, salinity, and density for each layer
#' 
#' @param t temperature vector
#' @param s Salinity vector
#' @param rho Density vector
#' @param z Depth vector
#' @param mld Mixed layer depth
#' @export 

profile_tsrho_by_layer <- function(t, s, rho, z, mld, mld.buffer) {
  if(mld >= max(z)){
    t_above <- mean(t, na.rm = TRUE)
    t_below <- NA
    s_above <- mean(s, na.rm = TRUE)
    s_below <- NA
    rho_above <- mean(rho, na.rm = TRUE)
    rho_below <- NA
  } else if(mld+30 >= max(z)){
    t_above <- mean(t[z < mld], na.rm = TRUE)
    t_below <- mean(t[z > max(z)-4], na.rm = TRUE)
    s_above <- mean(s[z < mld], na.rm = TRUE)
    s_below <- mean(s[z > max(z)-4], na.rm = TRUE)
    rho_above <- mean(rho[z < mld], na.rm = TRUE)
    rho_below  <- mean(rho[z > max(z)-4], na.rm = TRUE)
  } else {
    t_above <- mean(t[z < mld], na.rm = TRUE)
    t_below <- mean(t[z > mld+30], na.rm = TRUE)
    s_above <- mean(s[z < mld], na.rm = TRUE)
    s_below <- mean(s[z > mld+30], na.rm = TRUE)
    rho_above <- mean(rho[z < mld], na.rm = TRUE)
    rho_below  <- mean(rho[z > mld+30], na.rm = TRUE)
  }
  
  t_average <- mean(t, na.rm = TRUE)
  s_average <- mean(s, na.rm = TRUE)
  
  return(list(t_above = t_above, 
              t_below = t_below, 
              s_above = s_above, 
              s_below = s_below,
              rho_above = rho_above, 
              rho_below = rho_below,
              t_average = t_average,
              s_average = s_average))
}

#' Estimate value of a profile variable at a fixed depth
#' 
#' Algorithm used since 2013 to estimate temperature at 1-m (RACE_DATA.TEMPERATURE_METHOD.TEMPERATURE_METHOD_ID = 14) from temperature-depth recorders deployed on RACE bottom trawl surveys. Returns the variable value corresponding with the first depth value at or shallower than the reference depth. If no depth values are equal to the reference depth, the variable value is estimated by interpolating from the variable values above and below the reference depth.
#' 
#' @param depth Numeric vector of depth values
#' @param var vector of variables
#' @param ref_depth Reference depth for estimating 1L numeric vector
#' @export
#' @author Sean Rohan

calc_fixed_depth_var <- function(depth, var, ref_depth) {
  
  if(!(length(depth) > 1 & min(depth) < (ref_depth + 5))) {
    return(NA)
  }
  
  index <- min(which(depth[1:(length(depth)-1)] <= ref_depth)) 
  sel_depth <- depth[index]
  sel_var <- var[index]
  
  if(is.na(sel_depth[1]) | sel_depth == 0) {
    return(NA)
  }
  
  if(sel_depth == ref_depth) {
    return(sel_var)
  } else {
    pos <- which(depth[1:(length(depth)-1)] <= ref_depth & depth[2:(length(depth))] <= ref_depth) |>
      min()
    sel_var <- var[(pos-1):pos]
    sel_depth <- depth[(pos-1):pos]
    sel_var <- sel_var[1] + ( ( ( sel_var[2] - sel_var[1]) / ( sel_depth[2] - sel_depth[1]) ) * ( ref_depth - sel_depth[1]) )
    
    return(sel_var)
  }
  
}
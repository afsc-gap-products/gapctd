#' Conductivity thermal inertia correction parameter a
#' 
#' Calculate Sea-Bird conductivity cell thermal mass correction parameter a, which is used to apply thermal inertia correction to conductivity measurements.
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in thermal mass correction formula. Default = 0.04 for SBE19plus.
#' @param inv_beta Numeric vector (1L). 1/beta parameter in thermal mass correction formula. Default = 1/8 for SBE19plus.
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @export

ctm_par_a <- function(alpha = 0.04, inv_beta, f_n = 0.25) {
  return(2 * alpha / (f_n * inv_beta^-1 + 2))
}

#' Conductivity thermal inertia correction parameter b
#' 
#' Calculate Sea-Bird conductivity cell thermal mass correction parameter b, which is used to apply thermal inertia correction to conductivity measurements.
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in thermal mass correction function (default = 0.04 for SBE19plus).
#' @param a Numeric vector (1L). Conductivity thermal mass correction value a.
#' @export

ctm_par_b <- function(alpha, a) {
  return(1-(2*a/alpha))
}

#' Conductivity thermal intertia correction factor (C[T])
#' 
#' Calculate thermal inertia adjustment factors for SBE19 conductivity cell using temperature and parameters a and b, using the same method as the Thermal Mass Correction module in SBE Data Processing software.
#' 
#' @param a Numeric vector (1L). a parameter in thermal mass correction formula, as calculated by gapctd::ctm_par_a()
#' @param b Numeric vector (1L). b parameter in thermal mass correction formula, as calculated by gapctd::ctm_par_b().
#' @param temperature Numeric vector of temperatures in degC (ITS-90 scale).
#' @export

ctm_correct_c_t <- function(a, b, temperature) {
  c_t <- numeric(length = length(temperature))
  c_t[1] <- 0
  for(i in 2:length(temperature)) {
    c_t[i] <- -1 * b * c_t[i-1] + a * 0.1 * (1 + 0.006 * (temperature[i] - 20)) * (temperature[i] - temperature[i-1])
  }
  return(c_t)
}

#' Perform conductivity cell thermal intertia correction and compare upcasts to downcasts
#' 
#' Calculates conductivity cell thermal intertia correction using ctm_par_a(), ctm_par_b(), and ctm_correct_c_t(), and recalculates salinity for paired upcasts and downcasts. If argument min_obj = TRUE, the function is used to calculate and return the value of an objective function which is mean absolute difference in salinity (PSS-78 scale) between upcast and downcast from the means of 1 dbar bins. If min_obj = FALSE, the function returns a data frame containing corrected values averaged by 1 dbar bins.
#' 
#' @param alpha_beta Numeric vector (2L). Log-transformed alpha and beta parameters in thermal mass correction algorithm (alpha default = 0.04 and beta default = 0.125 for SBE19plus), passed to ctm_par_a() and ctm_par_b()
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @param down_temperature Numeric vector of downcast temperatures in degC (ITS-90 scale),
#' @param down_pressure Numeric vector of downcast pressure in dbar.
#' @param down_conductivity Numeric vector of downcast conductivity in S/m.
#' @param up_temperature Numeric vector of upcast temperatures in degC (ITS-90 scale),
#' @param up_pressure Numeric vector of upcast pressure in dbar.
#' @param up_conductivity Numeric vector of upcast conductivity in S/m.
#' @param min_pressure_bin Numeric vector (1L) indicating the minimum depth bin to use for calculating the objective function. Default = 2 dbar to minimize surface artifacts.
#' @param buffer_bottom_pressure_bin Numeric vector (1L). Buffer for excluding near-bottom pressure bins to minimize effects of depth changes on objective function.
#' @param min_obj Logical. Return the objective function instead of corrected conductivity, temperature, etc.
#' @export

ctm_adjust <- function(alpha = 0.04,
                       beta = 0.125,
                       f_n = 0.25,
                       down_temperature,
                       down_pressure,
                       down_conductivity,
                       up_temperature,
                       up_pressure,
                       up_conductivity,
                       min_pressure_bin = 2,
                       buffer_bottom_pressure_bin = 3,
                       min_obj = TRUE) {
  
  inv_beta <- 1/beta
  
  aa <- ctm_par_a(alpha = alpha, f_n = f_n, inv_beta = inv_beta)
  bb <- ctm_par_b(alpha = alpha, a = aa)
  
  downcast_c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = down_temperature)
  upcast_c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = up_temperature)
  downcast_c_corr <- down_conductivity + downcast_c_t
  upcast_c_corr <- up_conductivity + upcast_c_t
  downcast_salinity_psu <- oce::swSCTp(conductivity = downcast_c_corr,
                                       temperature = down_temperature,
                                       pressure = down_pressure,
                                       conductivityUnit = "S/m")
  
  upcast_salinity_psu <- oce::swSCTp(conductivity = upcast_c_corr,
                                     temperature = up_temperature,
                                     pressure = up_pressure,
                                     conductivityUnit = "S/m")
  
  updown_df <- aggregate(x = data.frame(conductivity_down = down_conductivity,
                                        conductivity_corr_down = downcast_c_corr,
                                        temperature = down_temperature,
                                        salinity_down = downcast_salinity_psu), 
                         by = list(pressure_bin = ceiling(down_pressure)), 
                         FUN = mean) |>
    dplyr::inner_join(
      aggregate(x = data.frame(conductivity_up = up_conductivity,
                               conductivity_corr_up = upcast_c_corr,
                               temperature_up = up_temperature,
                               salinity_up = upcast_salinity_psu), 
                by = list(pressure_bin = ceiling(up_pressure)), 
                FUN = mean),
      by = "pressure_bin") |>
    dplyr::filter(pressure_bin >= min_pressure_bin)
  
  obj <- mean(abs(updown_df$salinity_down[updown_df$pressure_bin < (max(updown_df$pressure_bin)-buffer_bottom_pressure_bin)] - 
                    updown_df$salinity_up[updown_df$pressure_bin < (max(updown_df$pressure_bin)-buffer_bottom_pressure_bin)]),
              na.rm = TRUE)
  
  if(min_obj) {
    return(obj)
  } else {
    return(updown_df)
  }
}
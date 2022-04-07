#' Conductivity thermal inertia correction parameter a
#' 
#' Calculate Sea-Bird conductivity cell thermal mass correction parameter a, which is used to apply thermal inertia correction to conductivity measurements.
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in thermal mass correction formula. Default = 0.04 for SBE19plus.
#' @param inv_beta Numeric vector (1L). 1/beta parameter in thermal mass correction formula. Default = 1/8 for SBE19plus.
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @export

ctm_par_a <- function(alpha = 0.04, beta, f_n = 0.25) {
  return(2 * alpha / (f_n * beta + 2))
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

#' Calculate conductivity cell thermal intertia correction
#' 
#' Calculates conductivity cell thermal intertia correction based on alpha, inverse beta (tau[CTM]), scan interval, temperature, and conductivity, then recalculates salinity. 
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in the cell thermal mass correction filter (default = 0.04 for SBE19plus)
#' @param tau Numeric vector (1L). Tau (inverse beta) parameter in the cell thermal mass correction filter (default = 8 for SBE19plus).
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @param temperature Numeric vector of temperature in degC (ITS-90 scale),
#' @param pressure Numeric vector of pressure in dbar.
#' @param conductivity Numeric vector of conductivity in S/m.
#' @param flag Character or numeric vector of data quality flags where NAs are scans to be excluded from final profiles. NA scans are used to estimate thermal cell inertia conductivity corrections and salinity but re removed from binned outputs. 
#' @param obj_fn Logical. If "path", returns an objective function, the total path distance of the salinity profile. Otherwise, returns temperature, salinity, and conductivity in 1 db depth bins.
#' @param binned Logical. If TRUE, objective function is calculated for pressure-binned data. If FALSE, the objective function is calculated using all un-flagged data. 
#' @export

estimate_ctm <- function(alpha = 0.04,
                         tau = 8,
                         f_n = 0.25,
                         temperature,
                         pressure,
                         conductivity,
                         flag,
                         obj_fn = NULL,
                         binned = TRUE) {
  
  aa <- ctm_par_a(alpha = alpha, f_n = f_n, beta = 1/tau)
  bb <- ctm_par_b(alpha = alpha, a = aa)
  
  c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = temperature)
  c_corr <- conductivity + c_t
  salinity_psu <- oce::swSCTp(conductivity = c_corr,
                                       temperature = temperature,
                                       pressure = pressure,
                                       conductivityUnit = "S/m")
  
  out_df <- aggregate(x = data.frame(conductivity = conductivity[!is.na(flag)],
                                     corrected_conductivity = c_corr[!is.na(flag)],
                                     temperature = temperature[!is.na(flag)],
                                     salinity = salinity_psu[!is.na(flag)]), 
                         by = list(pressure_bin = round(pressure[!is.na(flag)])), 
                         FUN = mean)
  
  if(obj_fn == "path") {
    if(binned) {
      obj <- sum(abs(diff(out_df$salinity)))
    } else {
      obj <- sum(abs(diff(salinity_psu[!is.na(flag)])))
    }
    
  } else {
    obj <- out_df
  }
  
    return(obj)
}


#' Wrapper function around estimate_ctm
#' 
#' Estimate conductivity cell thermal inertia correction using estimate_ctm and bbmle::mle2().
#' 
#' @param profile_files Paths to profile files for which CTM parameters should be estimated.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, especially in downcasts.
#' @param optim_method Optimization method for optim(). Default is simulated annealing ("SANN")
#' @param optim_maxit Number of optim iterations or maximum number of iterations, depending on the optim method. Default = 500.
#' @param start_alpha Default alpha value.
#' @param start_tau Default tau value.
#' @return Returns a data frame containing \itemize{
#' \item deploy_id: deployment file ID
#' \item cast_direction: Direction of the cast ("downcast" or "upcast")
#' \item default_obj: estimated CTM objective function for the default parameters (alpha = 0.04, tau = 8)
#' \item adj_obj: estimated CTM objective function for the estimated parameters
#' \item alpha: estimated alpha parameter
#' \item tau: estimated tau[CTM] parameter
#' \item mean_t_rate: average rate of change in temperature for the cast in degrees Celsius per db (pressure).
#' \item mean_s_rate: average rate of change in salinity for the cast in PSS-78 salinity per db (pressure).
#' \item max_t_rate:  maximum rate of change in temperature for the cast in degrees Celsius per db (pressure).
#' \item max_s_rate:  maximum rate of change in salinity for the cast in PSS-78 salinity per db (pressure).
#' \item range_t: absolute range of temperature in a cast in degrees Celsius.
#' \item range_s: absolute range of salinity in a cast in PSS-78 salinity.}
#' @export


run_estimate_ctm <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                               full.names = TRUE, pattern = "downcast.cnv"),
                                                    list.files(here::here("output", "sbe19plus_v0"), 
                                                               full.names = TRUE, pattern = "upcast.cnv"))),
                             min_pressure = 4,
                             optim_method = "SANN",
                             optim_maxit = 500,
                             start_alpha = 0.04,
                             start_tau = 8,
                             ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  cast_direction <- character(length = length(deploy_id))
  cast_direction[grepl(pattern = "downcast", x = profile_files)] <- "downcast"
  cast_direction[grepl(pattern = "upcast", x = profile_files)] <- "upcast"
  
  default_obj <- numeric(length = length(profile_files))
  adj_obj <- numeric(length = length(profile_files))
  par_alpha <- numeric(length = length(profile_files))
  par_tau <- numeric(length = length(profile_files))
  mean_t_rate <- numeric(length = length(profile_files))
  mean_s_rate <- numeric(length = length(profile_files))
  max_t_rate <- numeric(length = length(profile_files))
  max_s_rate <- numeric(length = length(profile_files))
  range_t <- numeric(length = length(profile_files))
  range_s <- numeric(length = length(profile_files))
  
  
  for(ii in 1:length(profile_files)) { 
    
    message(profile_files[ii])
    
    profile_dat <- try(oce::read.ctd(file = profile_files[ii]), silent = TRUE)
    if(class(profile_dat) == "try-error") next
    
    temperature <- profile_dat@data$temperature[profile_dat@data$pressure > min_pressure]
    flag <- profile_dat@data$flag[profile_dat@data$pressure > min_pressure]
    
    if(any(length(temperature) < 20, sum(!is.na(flag)) < 20)) {
      par_alpha[ii] <- start_alpha
      par_tau[ii] <- start_tau
      next
    }
    
    pressure <- profile_dat@data$pressure[profile_dat@data$pressure > min_pressure]
    conductivity <- profile_dat@data$conductivity[profile_dat@data$pressure > min_pressure]
    salinity <- profile_dat@data$salinity[profile_dat@data$pressure > min_pressure]
    
    default_dat <- gapctd::estimate_ctm(alpha = start_alpha,
                                 tau = start_tau,
                                 f_n = 0.25,
                                 temperature = temperature,
                                 pressure = pressure,
                                 conductivity = conductivity,
                                 flag = flag,
                                 obj_fn = "none")
    
    default_obj[ii] <- gapctd::estimate_ctm(alpha = start_alpha,
                                     tau = start_tau,
                                     f_n = 0.25,
                                     temperature = temperature,
                                     pressure = pressure,
                                     conductivity = conductivity,
                                     flag = flag,
                                     obj_fn = "path")
    
    est_pars <- try(bbmle::mle2(minuslogl = gapctd::estimate_ctm,
                                start = list(alpha = start_alpha,
                                             tau = start_tau),
                                data = list(temperature = temperature,
                                            pressure = pressure,
                                            conductivity = conductivity,
                                            flag = flag,
                                            obj_fn = "path",
                                            f_n = 0.25),
                                method = optim_method,
                                control = list(maxit = optim_maxit, reltol = 1e-5,
                                               parscale = c(alpha = 0.01, tau = 1))), silent = TRUE)
    conv <- try(est_pars@details$convergence, silent = TRUE)
    
    if(any(class(est_pars) == "try-error", conv != 0)) {
      est_pars <- try(bbmle::mle2(minuslogl = gapctd::estimate_ctm,
                                  start = list(alpha = 0.1,
                                               tau = 12),
                                  data = list(temperature = temperature,
                                              pressure = pressure,
                                              conductivity = conductivity,
                                              flag = flag,
                                              obj_fn = "area",
                                              f_n = 0.25),
                                  method = optim_method,
                                  control = list(maxit = optim_maxit, reltol = 1e-5,
                                                 parscale = c(alpha = 0.1, tau = 1))), silent = TRUE)
      conv <- try(est_pars@details$convergence, silent = TRUE)
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      adj_dat <- gapctd::estimate_ctm(alpha = est_pars@coef['alpha'],
                               tau = est_pars@coef['tau'],
                               f_n = 0.25,
                               temperature = temperature,
                               pressure = pressure,
                               conductivity = conductivity,
                               flag = flag,
                               obj_fn = "none")
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      
      if(est_pars@min < default_obj[ii]) {
        par_alpha[ii] <- as.numeric(est_pars@coef['alpha'])
        par_tau[ii] <- as.numeric(est_pars@coef['tau'])
        adj_obj[ii] <- est_pars@min
      } else {
        adj_obj[ii] <- default_obj[ii]
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
      }
      
      max_t_rate[ii] <- max(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      mean_t_rate[ii] <- mean(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      range_t[ii] <- diff(range(default_dat$temperature, na.rm = TRUE))
      
      max_s_rate[ii] <- max(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      mean_s_rate[ii] <- mean(abs(diff(default_dat$temperature[!is.na(default_dat$temperature)])/diff(default_dat$pressure[!is.na(default_dat$temperature)])))
      
      range_s[ii] <- diff(range(default_dat$temperature, na.rm = TRUE))
      
    } else {
      par_alpha[ii] <- NA
      par_tau[ii] <- NA
    }
    
  }
  
  out_df <- data.frame(deploy_id = deploy_id,
                       cast_direction = cast_direction,
                       default_obj = default_obj,
                       adj_obj = adj_obj,
                       alpha = round(par_alpha, 5),
                       tau = round(par_tau, 5),
                       mean_t_rate = mean_t_rate,
                       mean_s_rate = mean_s_rate,
                       max_t_rate = max_t_rate,
                       max_s_rate = max_s_rate,
                       range_t = range_t,
                       range_s = range_s)
  
  return(out_df)
  
}


#' Perform conductivity cell thermal intertia correction and compare upcasts to downcasts
#' 
#' Calculates conductivity cell thermal intertia correction using ctm_par_a(), ctm_par_b(), and ctm_correct_c_t(), and recalculates salinity for paired upcasts and downcasts. If argument obj_fn is set to difference or area, the function is used to calculate and return the value of an objective function which is mean absolute difference in salinity (PSS-78 scale) between upcast and downcast from the means of 1 dbar bins. If min_obj = FALSE, the function returns a data frame containing corrected values averaged by 1 dbar bins.
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in the cell thermal mass correction filter (default = 0.04 for SBE19plus)
#' @param tau Numeric vector (1L). Tau (inverse beta) parameter in the cell thermal mass correction filter (default = 8 for SBE19plus).
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @param down_temperature Numeric vector of downcast temperatures in degC (ITS-90 scale),
#' @param down_pressure Numeric vector of downcast pressure in dbar.
#' @param down_conductivity Numeric vector of downcast conductivity in S/m.
#' @param up_temperature Numeric vector of upcast temperatures in degC (ITS-90 scale),
#' @param up_pressure Numeric vector of upcast pressure in dbar.
#' @param up_conductivity Numeric vector of upcast conductivity in S/m.
#' @param min_pressure_bin Numeric vector (1L) indicating the minimum depth bin to use for calculating the objective function. Default = 2 dbar to minimize surface artifacts.
#' @param buffer_bottom_pressure_bin Numeric vector (1L). Buffer for excluding near-bottom pressure bins to minimize effects of depth changes on objective function.
#' @param obj_fn Logical. Return the objective function instead of corrected conductivity, temperature, etc.
#' @noRd

ctm_adjust <- function(alpha = 0.04,
                       tau = 0.125,
                       f_n = 0.25,
                       down_temperature,
                       down_pressure,
                       down_conductivity,
                       up_temperature,
                       up_pressure,
                       up_conductivity,
                       min_pressure_bin = 0,
                       buffer_bottom_pressure_bin = 0,
                       obj_fn = NULL) {
  
  beta <- 1/tau
  
  aa <- ctm_par_a(alpha = alpha, f_n = f_n, beta = beta)
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
                                        temperature_down = down_temperature,
                                        salinity_down = downcast_salinity_psu), 
                         by = list(pressure_bin = ceiling(down_pressure)), 
                         FUN = mean) |>
    dplyr::full_join(
      aggregate(x = data.frame(conductivity_up = up_conductivity,
                               conductivity_corr_up = upcast_c_corr,
                               temperature_up = up_temperature,
                               salinity_up = upcast_salinity_psu), 
                by = list(pressure_bin = ceiling(up_pressure)), 
                FUN = mean),
      by = "pressure_bin")
  
  if(obj_fn == "difference") {
    
    obj <- mean(abs(updown_df$salinity_down[updown_df$pressure_bin < (max(updown_df$pressure_bin)-buffer_bottom_pressure_bin)] - 
                      updown_df$salinity_up[updown_df$pressure_bin < (max(updown_df$pressure_bin)-buffer_bottom_pressure_bin)]),
                na.rm = TRUE)
    
  } else if(obj_fn == "area") {
    
    comb_df <- data.frame(salinity = c(updown_df$salinity_down, rev(updown_df$salinity_up), updown_df$salinity_down[1]),
                          temperature = c(updown_df$temperature_down, rev(updown_df$temperature_down), updown_df$temperature_down[1])) |>
      dplyr::filter(!is.na(salinity),
                    !is.na(temperature)) 
    
    obj <- try(comb_df |>
                 sf::st_as_sf(coords = c("salinity", "temperature")) |>    
                 dplyr::group_by(ID = 1) |>
                 summarise(do_union = FALSE) |>
                 sf::st_cast(to = "POLYGON") |> 
                 sf::st_area(), silent = TRUE)
    
    if(class(obj) == "try-error") {
      return(comb_df)
    }
  }
  
  if(obj_fn == "none") {
    return(updown_df)
  } else {
    return(obj)
  }
}
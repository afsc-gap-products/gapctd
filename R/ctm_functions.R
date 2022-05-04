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

#' Perform conductivity cell thermal inertia correction and compare upcasts to downcasts
#' 
#' Calculates conductivity cell thermal intertia correction using ctm_par_a(), ctm_par_b(), and ctm_correct_c_t(), and recalculates salinity for paired upcasts and downcasts. If argument obj_fn is set to area, the function calculates and return the value of an objective function which is the area between upcast and downcast temperature-salinity curves in 1 dbar pressure bins. If obj_fn = "none," returns a data.frame with upcast and downcast temperature, conductivity, corrected conductivity, and salinity in 1 dbar pressure bins.
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
#' @param obj_fn Character vector ("none" or "area")
#' @param scale_vars Logical. Should temperature and salinity be scaled to calculate T-S area?
#' @export

ctm_adjust_tsarea <- function(alpha = 0.04,
                              tau = 8,
                              f_n = 0.25,
                              down_temperature,
                              down_pressure,
                              down_conductivity,
                              down_flag,
                              up_temperature,
                              up_pressure,
                              up_conductivity,
                              up_flag,
                              obj_fn = "none",
                              scale_vars = FALSE) {
  
  
  aa <- ctm_par_a(alpha = alpha, f_n = f_n, beta = 1/tau)
  bb <- ctm_par_b(alpha = alpha, a = aa)
  
  if(all(!is.null(down_temperature), 
         !is.null(down_pressure), 
         !is.null(down_conductivity), 
         !is.null(down_flag))) {
    
    down_exists <- TRUE
    
    # Recalculate downcast salinity
    downcast_c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = down_temperature)
    downcast_c_corr <- down_conductivity + downcast_c_t
    downcast_salinity_psu <- oce::swSCTp(conductivity = downcast_c_corr,
                                         temperature = down_temperature,
                                         pressure = down_pressure,
                                         conductivityUnit = "S/m")
    
  } else {
    down_exists <- FALSE
  }
  
  if(all(!is.null(up_temperature), 
         !is.null(up_pressure), 
         !is.null(up_conductivity), 
         !is.null(up_flag))) {
    up_exists <- TRUE
    
    upcast_c_t <- ctm_correct_c_t(a = aa, b = bb, temperature = up_temperature)
    upcast_c_corr <- up_conductivity + upcast_c_t
    
    # Recalculate salinity
    upcast_salinity_psu <- oce::swSCTp(conductivity = upcast_c_corr,
                                       temperature = up_temperature,
                                       pressure = up_pressure,
                                       conductivityUnit = "S/m")
    
  } else {
    up_exists <- FALSE
  }
  
  if(up_exists & down_exists) {
    updown_df <- aggregate(x = data.frame(conductivity_down = down_conductivity[!is.na(down_flag)],
                                          conductivity_corr_down = downcast_c_corr[!is.na(down_flag)],
                                          temperature_down = down_temperature[!is.na(down_flag)],
                                          salinity_down = downcast_salinity_psu[!is.na(down_flag)]), 
                           by = list(pressure_bin = ceiling(down_pressure[!is.na(down_flag)])), 
                           FUN = mean) |>
      dplyr::full_join(
        aggregate(x = data.frame(conductivity_up = up_conductivity[!is.na(up_flag)],
                                 conductivity_corr_up = upcast_c_corr[!is.na(up_flag)],
                                 temperature_up = up_temperature[!is.na(up_flag)],
                                 salinity_up = upcast_salinity_psu[!is.na(up_flag)]), 
                  by = list(pressure_bin = ceiling(up_pressure[!is.na(up_flag)])), 
                  FUN = mean),
        by = "pressure_bin")
  } else {
    val <- c("d", "u")[c(down_exists, up_exists)]
    
    updown_df <- switch(val,
                        `d` = {
                          aggregate(x = data.frame(conductivity_down = down_conductivity[!is.na(down_flag)],
                                                   conductivity_corr_down = downcast_c_corr[!is.na(down_flag)],
                                                   temperature_down = down_temperature[!is.na(down_flag)],
                                                   salinity_down = downcast_salinity_psu[!is.na(down_flag)]), 
                                    by = list(pressure_bin = ceiling(down_pressure[!is.na(down_flag)])), 
                                    FUN = mean)
                        },
                        `u` = {
                          aggregate(x = data.frame(conductivity_up = up_conductivity[!is.na(up_flag)],
                                                   conductivity_corr_up = upcast_c_corr[!is.na(up_flag)],
                                                   temperature_up = up_temperature[!is.na(up_flag)],
                                                   salinity_up = upcast_salinity_psu[!is.na(up_flag)]), 
                                    by = list(pressure_bin = ceiling(up_pressure[!is.na(up_flag)])), 
                                    FUN = mean)
                        })
  }
  
  
  if(obj_fn == "none") {
    # Return prediction
    return(updown_df)
  } else {
    if(down_exists & up_exists) {
      if(obj_fn == "area") {
        
        comb_df <- updown_df |>
          dplyr::filter(!is.na(salinity_up),
                        !is.na(temperature_up),
                        !is.na(salinity_down),
                        !is.na(temperature_down))
        
        if(nrow(comb_df) == 0) {
          obj <- 1e7
        } else {
          
          if(scale_vars) {
            comb_df$salinity_up <- scale(c(comb_df$salinity_up, salinity_down))[,1][nrow(comb_df)]
            comb_df$salinity_down <- scale(c(salinity_down, comb_df$salinity_up))[,1][nrow(comb_df)]
            comb_df$temperature_up <- scale(c(comb_df$temperature_up, temperature_down))[,1][nrow(comb_df)]
            comb_df$temperature_down <- scale(c(comb_df$temperature_down, temperature_up))[,1][nrow(comb_df)]
          }
          
          wkt_poly <- data.frame(geometry = paste0("LINESTRING (", apply(X = 
                                                                           cbind(
                                                                             apply(
                                                                               X = cbind(
                                                                                 comb_df$salinity_down[1:(nrow(comb_df))],
                                                                                 comb_df$temperature_down[1:(nrow(comb_df))]),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               cbind(c(comb_df$salinity_up[1:(nrow(comb_df)-1)], comb_df$salinity_up[(nrow(comb_df)-1)]),
                                                                                     c(comb_df$temperature_up[1:(nrow(comb_df)-1)], comb_df$temperature_up[(nrow(comb_df)-1)])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(c(comb_df$salinity_down[2:(nrow(comb_df))],comb_df$salinity_up[(nrow(comb_df))]),
                                                                                         c(comb_df$temperature_down[2:(nrow(comb_df))],comb_df$temperature_up[(nrow(comb_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(
                                                                                 c(comb_df$salinity_down[1:(nrow(comb_df)-1)],comb_df$salinity_down[(nrow(comb_df))]),
                                                                                 c(comb_df$temperature_down[1:(nrow(comb_df)-1)],comb_df$temperature_down[(nrow(comb_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " ")),
                                                                         MARGIN = 1,
                                                                         FUN = paste,
                                                                         collapse = ", "), ")")) |>
            dplyr::mutate(ID = row_number()) |>
            st_as_sf(wkt = "geometry") |> 
            dplyr::group_by(ID) |>
            summarise(do_union = FALSE) |>
            sf::st_cast(to = "POLYGON")
          
          obj <- sum(sf::st_area(wkt_poly), na.rm = TRUE)
        }
        
        if(class(obj) == "try-error") {
          return(comb_df)
        }
      } 
      # else if(obj_fn == "salinity_diff") {
      #   
      #   comb_df <- updown_df |>
      #     dplyr::filter(!is.na(salinity_up),
      #                   !is.na(temperature_up),
      #                   !is.na(salinity_down),
      #                   !is.na(temperature_down))
      #   
      #   if(nrow(comb_df) == 0) {
      #     obj <- 1e7
      #   } else {
      #     obj <- 1-cor(
      #       diff(comb_df$salinity_down),
      #       diff(comb_df$salinity_up),
      #       method = "pearson")
      #   }
      #   
      # }
    } else {
      # Path length
      obj <- switch(val,
                    `d` = {sum(abs(diff(updown_df$salinity_down)))},
                    `u` = {sum(abs(diff(updown_df$salinity_up)))})  
      
    }
    return(obj)
  }
}

#' Wrapper function around ctm_adjust_tsarea
#' 
#' Estimate conductivity cell thermal inertia correction using ctm_adjust_tsarea and bbmle::mle2().
#' 
#' @param profile_files Paths to profile files for which CTM parameters should be estimated.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, especially in downcasts.
#' @param optim_method Optimization method for optim(). Default is the Broyden-Fletcher-Goldfarb-Shanno algorithm ("BFGS")
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

run_ctm_adjust_tsarea <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                    full.names = TRUE, pattern = "downcast.cnv"),
                                                         list.files(here::here("output", "sbe19plus_v0"), 
                                                                    full.names = TRUE, pattern = "upcast.cnv"))),
                                  min_pressure = 4,
                                  optim_method = "L-BFGS-B",
                                  optim_maxit = 500,
                                  start_alpha = 0.04,
                                  start_tau = 8,
                                  obj_method = "area",
                                  ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  cast_direction <- character(length = length(deploy_id))
  cast_direction[grepl(pattern = "downcast", x = profile_files)] <- "downcast"
  cast_direction[grepl(pattern = "upcast", x = profile_files)] <- "upcast"
  
  unique_id <- unique(deploy_id)
  
  default_obj <- numeric(length = length(unique_id))
  adj_obj <- numeric(length = length(unique_id))
  par_alpha <- numeric(length = length(unique_id))
  par_tau <- numeric(length = length(unique_id))
  mean_t_rate <- numeric(length = length(unique_id))
  mean_s_rate <- numeric(length = length(unique_id))
  max_t_rate <- numeric(length = length(unique_id))
  max_s_rate <- numeric(length = length(unique_id))
  range_t <- numeric(length = length(unique_id))
  range_s <- numeric(length = length(unique_id))
  
  for(ii in 1:length(unique_id)) {
    
    # ii <- 9
    message(unique_id[ii])
    cast_files <- profile_files[grepl(pattern = unique_id[ii], profile_files)]
    
    dc_dat <- try(oce::read.ctd(file = cast_files[grepl(pattern = "downcast", 
                                                        x = cast_files)]), silent = TRUE)
    uc_dat <- try(oce::read.ctd(file = cast_files[grepl(pattern = "upcast", 
                                                        x = cast_files)]), silent = TRUE)
    
    if((class(dc_dat) == "try-error") & (class(uc_dat) == "try-error")) {
      next
    } else if(class(dc_dat) == "try-error") {
      temperature <- NULL
      flag <- NULL
      pressure <- NULL
      conductivity <- NULL
      salinity <- NULL
      
      temperature_2 <- uc_dat@data$temperature[uc_dat@data$pressure > min_pressure]
      flag_2 <- uc_dat@data$flag[uc_dat@data$pressure > min_pressure]
      pressure_2 <- uc_dat@data$pressure[uc_dat@data$pressure > min_pressure]
      conductivity_2 <- uc_dat@data$conductivity[uc_dat@data$pressure > min_pressure]
      salinity_2 <- uc_dat@data$salinity[uc_dat@data$pressure > min_pressure]
      
      # Min. sample size
      if(any(length(temperature_2) < 20, sum(!is.na(flag_2)) < 20)) {
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
        next
      }
      
      
    } else if(class(uc_dat) == "try-error") {
      temperature <- dc_dat@data$temperature[dc_dat@data$pressure > min_pressure]
      flag <- dc_dat@data$flag[dc_dat@data$pressure > min_pressure]
      pressure <- dc_dat@data$pressure[dc_dat@data$pressure > min_pressure]
      conductivity <- dc_dat@data$conductivity[dc_dat@data$pressure > min_pressure]
      salinity <- dc_dat@data$salinity[dc_dat@data$pressure > min_pressure]
      
      if(any(length(temperature) < 20, sum(!is.na(flag)) < 20)) {
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
        next
      }
      
      # Min. sample size
      temperature_2 <- NULL
      flag_2 <- NULL
      pressure_2 <- NULL
      conductivity_2 <- NULL
      salinity_2 <- NULL
      
    } else {
      temperature <- dc_dat@data$temperature[dc_dat@data$pressure > min_pressure]
      flag <- dc_dat@data$flag[dc_dat@data$pressure > min_pressure]
      pressure <- dc_dat@data$pressure[dc_dat@data$pressure > min_pressure]
      conductivity <- dc_dat@data$conductivity[dc_dat@data$pressure > min_pressure]
      salinity <- dc_dat@data$salinity[dc_dat@data$pressure > min_pressure]
      
      temperature_2 <- uc_dat@data$temperature[uc_dat@data$pressure > min_pressure]
      flag_2 <- uc_dat@data$flag[uc_dat@data$pressure > min_pressure]
      pressure_2 <- uc_dat@data$pressure[uc_dat@data$pressure > min_pressure]
      conductivity_2 <- uc_dat@data$conductivity[uc_dat@data$pressure > min_pressure]
      salinity_2 <- uc_dat@data$salinity[uc_dat@data$pressure > min_pressure]
      
      # Min. sample size
      if(any(length(temperature) < 20, sum(!is.na(flag)) < 20) & any(length(temperature_2) < 20, sum(!is.na(flag_2)) < 20)) {
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
        next
      } else if(any(length(temperature_2) < 20, sum(!is.na(flag_2)) < 20)) {
        temperature_2 <- NULL
        flag_2 <- NULL
        pressure_2 <- NULL
        conductivity_2 <- NULL
        salinity_2 <- NULL
      } else if(any(length(temperature) < 20, sum(!is.na(flag)) < 20)){
        temperature <- NULL
        flag <- NULL
        pressure <- NULL
        conductivity <- NULL
        salinity <- NULL
      }
    }
    
    
    
    default_dat <- gapctd:::ctm_adjust_tsarea(alpha = start_alpha,
                                              tau = start_tau,
                                              f_n = 0.25,
                                              down_temperature = temperature,
                                              down_pressure = pressure,
                                              down_conductivity = conductivity,
                                              down_flag = flag,
                                              up_temperature = temperature_2,
                                              up_pressure = pressure_2,
                                              up_conductivity = conductivity_2,
                                              up_flag = flag_2,
                                              obj_fn = "none")
    
    default_obj[ii] <- gapctd:::ctm_adjust_tsarea(alpha = start_alpha,
                                                  tau = start_tau,
                                                  f_n = 0.25,
                                                  down_temperature = temperature,
                                                  down_pressure = pressure,
                                                  down_conductivity = conductivity,
                                                  down_flag = flag,
                                                  up_temperature = temperature_2,
                                                  up_pressure = pressure_2,
                                                  up_conductivity = conductivity_2,
                                                  up_flag = flag_2,
                                                  obj_fn = obj_method)
    
    est_pars <- try(bbmle::mle2(minuslogl = gapctd:::ctm_adjust_tsarea,
                                start = list(alpha = start_alpha,
                                             tau = start_tau),
                                data = list(down_temperature = temperature,
                                            down_pressure = pressure,
                                            down_conductivity = conductivity,
                                            down_flag = flag,
                                            up_temperature = temperature_2,
                                            up_pressure = pressure_2,
                                            up_conductivity = conductivity_2,
                                            up_flag = flag_2,
                                            obj_fn = obj_method,
                                            f_n = 0.25),
                                method = optim_method,
                                lower = c(alpha = -1, tau = 0),
                                upper = c(alpha = 1, tau = 45),
                                control = list(maxit = optim_maxit, 
                                               reltol = 1e-4, 
                                               trace = 1, 
                                               parscale = c(alpha = 0.1, tau = 1))), 
                    silent = TRUE)
    conv <- try(est_pars@details$convergence, silent = TRUE)
    
    if(any(class(est_pars) == "try-error", conv != 0)) {
      est_pars <- try(bbmle::mle2(minuslogl = gapctd::ctm_adjust_tsarea,
                                  start = list(alpha = 0.1,
                                               tau = 12),
                                  data = list(down_temperature = temperature,
                                              down_pressure = pressure,
                                              down_conductivity = conductivity,
                                              down_flag = flag,
                                              up_temperature = temperature_2,
                                              up_pressure = pressure_2,
                                              up_conductivity = conductivity_2,
                                              up_flag = flag_2,
                                              obj_fn = obj_method,
                                              f_n = 0.25),
                                  method = optim_method,
                                  lower = c(alpha = -1, tau = 0),
                                  upper = c(alpha = 1, tau = 45),
                                  control = list(maxit = optim_maxit, 
                                                 reltol = 1e-4, 
                                                 trace = 1, 
                                                 parscale = c(alpha = 0.1, tau = 1))), 
                      silent = TRUE)
      conv <- try(est_pars@details$convergence, silent = TRUE)
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      adj_dat <- gapctd::ctm_adjust_tsarea(alpha = est_pars@coef['alpha'],
                                           tau = est_pars@coef['tau'],
                                           f_n = 0.25,
                                           down_temperature = temperature,
                                           down_pressure = pressure,
                                           down_conductivity = conductivity,
                                           down_flag = flag,
                                           up_temperature = temperature_2,
                                           up_pressure = pressure_2,
                                           up_conductivity = conductivity_2,
                                           up_flag = flag_2,
                                           obj_fn = "none")
    }
    
    if(!any(class(est_pars) == "try-error", conv != 0)) {
      
      if(est_pars@min < default_obj[ii]) {
        if(as.numeric(est_pars@coef['tau']) > 0 & 
           as.numeric(est_pars@coef['tau']) < 45) { #  0 < tau < 45 seconds constrains thermal inertia effects to last from 0-45 seconds.
          par_alpha[ii] <- as.numeric(est_pars@coef['alpha'])
          par_tau[ii] <- as.numeric(est_pars@coef['tau'])
          adj_obj[ii] <- est_pars@min
        } else {
          par_alpha[ii] <- start_alpha
          par_tau[ii] <- start_tau
        }
        
      } else {
        adj_obj[ii] <- default_obj[ii]
        par_alpha[ii] <- start_alpha
        par_tau[ii] <- start_tau
      }
      
      max_t_rate[ii] <- max(c(abs(diff(default_dat$temperature_down)/diff(default_dat$pressure_bin)),
                              abs(diff(default_dat$temperature_up)/diff(default_dat$pressure_bin))),
                            na.rm = TRUE)
      
      mean_t_rate[ii] <- mean(c(abs(diff(default_dat$temperature_down)/diff(default_dat$pressure_bin)),
                                abs(diff(default_dat$temperature_down)/diff(default_dat$pressure_bin))), na.rm = TRUE)
      
      range_t[ii] <- max(c(diff(range(default_dat$temperature_down, na.rm = TRUE)),
                           diff(range(default_dat$temperature_up, na.rm = TRUE))))
      
      max_s_rate[ii] <- max(c(abs(diff(default_dat$salinity_down)/diff(default_dat$pressure_bin)),
                              abs(diff(default_dat$salinity_up)/diff(default_dat$pressure_bin))),
                            na.rm = TRUE)
      
      mean_s_rate[ii] <- mean(c(abs(diff(default_dat$salinity_down)/diff(default_dat$pressure_bin)),
                                abs(diff(default_dat$salinity_down)/diff(default_dat$pressure_bin))), na.rm = TRUE)
      
      range_s[ii] <- max(c(diff(range(default_dat$salinity_down, na.rm = TRUE)),
                           diff(range(default_dat$salinity_up, na.rm = TRUE))))
      
    } else {
      par_alpha[ii] <- NA
      par_tau[ii] <- NA
    }
  }
  
  par_alpha[is.na(par_alpha)] <- start_alpha
  par_tau[is.na(par_tau)] <- start_tau
  out_df <- data.frame(deploy_id = unique_id,
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

ctm_adjust_path <- function(alpha = 0.04,
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


#' Wrapper function around ctm_adjust_path
#' 
#' Estimate conductivity cell thermal inertia correction using ctm_adjust_path and bbmle::mle2().
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


run_ctm_adjust_path <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
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
    
    default_dat <- gapctd::ctm_adjust_path(alpha = start_alpha,
                                 tau = start_tau,
                                 f_n = 0.25,
                                 temperature = temperature,
                                 pressure = pressure,
                                 conductivity = conductivity,
                                 flag = flag,
                                 obj_fn = "none")
    
    default_obj[ii] <- gapctd::ctm_adjust_path(alpha = start_alpha,
                                     tau = start_tau,
                                     f_n = 0.25,
                                     temperature = temperature,
                                     pressure = pressure,
                                     conductivity = conductivity,
                                     flag = flag,
                                     obj_fn = "path")
    
    est_pars <- try(bbmle::mle2(minuslogl = gapctd::ctm_adjust_path,
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
      est_pars <- try(bbmle::mle2(minuslogl = gapctd::ctm_adjust_path,
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
      adj_dat <- gapctd::ctm_adjust_path(alpha = est_pars@coef['alpha'],
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



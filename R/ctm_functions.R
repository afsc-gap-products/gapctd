#' Optimize cell thermal mass correction parameters (R workflow)
#' 
#' Estimate optimal cell thermal mass correction parameters by minimizing the area between downcast and upcast temperature-salinity curves. Or, if only upcast or downcast is provided, optimization based on minimizing the path distance of the salinity profile
#' 
#' @param dc downcast oce object
#' @param uc upcast oce object
#' @param optim_method Optimization method for optim(). Default is the Broyden-Fletcher-Goldfarb-Shanno with constraints algorithm ("L-BFGS-B")
#' @param optim_maxit Number of optim iterations or maximum number of iterations, depending on the optim method. Default = 500.
#' @param start_alpha_C Starting value for alpha in cell thermal mass optimization (default = 0.04, typical value for SBE19plus).
#' @param start_beta_C Starting value for beta in cell thermal mass optimization (default = 1/8, typical value for SBE19plus).
#' @param area_method Area between temperature-salinity ("ts") curves, depth-salinity curves ("zs"), or pressure-salinity curves ("ps")
#' @param default_parameters Named numeric vector of default parameters. Defaults is c(alpha_C = 0.04, beta_C = 0.125).
#' @return A named numerical vector of the optimal alpha_C or beta_C. The input values are returned if the optimization does not converge.
#' @export

optim_ctm_pars <- function(dc = NULL, 
                           uc = NULL,
                           optim_method = "L-BFGS-B",
                           start_alpha_C = c(0.001, 0.01, 0.02, 0.04, 0.08, 0.12),
                           start_beta_C = c(1, 1/2, 1/4, 1/8, 1/12, 1/24),
                           default_parameters = c(alpha_C = 0.04, beta_C = 0.125),
                           area_method = "ts",
                           ...) {
  
  both_casts <- !any(is.null(dc), is.null(uc))
  
  start_pars <- tidyr::expand_grid(start_alpha_C = start_alpha_C,
                                   start_beta_C = start_beta_C,
                                   obj = 1e7,
                                   obj_down = 1e7,
                                   obj_up = 1e7)
  
  # Find good starting values
  for(kk in 1:nrow(start_pars)) {
    if(both_casts) {
      start_pars$obj_down[kk] <- ctm_obj(dc = dc,
                                         alpha_C = start_pars$start_alpha_C[kk],
                                         beta_C = start_pars$start_beta_C[kk],
                                         area_method = area_method)
      
      start_pars$obj_up[kk] <- ctm_obj(uc = uc,
                                       alpha_C = start_pars$start_alpha_C[kk],
                                       beta_C = start_pars$start_beta_C[kk],
                                       area_method = area_method)
    }
    
    start_pars$obj[kk] <- ctm_obj(dc = dc,
                                  uc = uc,
                                  alpha_C = start_pars$start_alpha_C[kk],
                                  beta_C = start_pars$start_beta_C[kk],
                                  area_method = area_method)
  }
  
  start_index_both <- which.min(start_pars$obj)
  
  if(both_casts) {
    start_index_down <- which.min(start_pars$obj_down)
    start_index_up <- which.min(start_pars$obj_up)
  }
  
  est_pars <- try(bbmle::mle2(minuslogl = gapctd:::ctm_obj,
                              start = list(alpha_C = start_pars$start_alpha_C[start_index_both],
                                           beta_C = start_pars$start_beta_C[start_index_both]),
                              data = list(dc = dc,
                                          uc = uc,
                                          area_method = area_method),
                              method = "L-BFGS-B",
                              lower = c(alpha_C = 0, beta_C = 1/45),
                              upper = c(alpha_C = 1, beta_C = 10),
                              control = list(reltol = 1e-4, 
                                             trace = 1,
                                             parscale = c(alpha_C = 10^log10(start_pars$start_alpha_C[start_index_both]), # Estimate parameter scaling for optimization
                                                          beta_C = 0.1))),
                  silent = TRUE)
  
  est_pars_down <- try(bbmle::mle2(minuslogl = gapctd:::ctm_obj,
                                   start = list(alpha_C = start_pars$start_alpha_C[start_index_down],
                                                beta_C = start_pars$start_beta_C[start_index_down]),
                                   data = list(dc = dc,
                                               uc = NULL,
                                               area_method = area_method),
                                   method = "L-BFGS-B",
                                   lower = c(alpha_C = 0, beta_C = 1/45),
                                   upper = c(alpha_C = 1, beta_C = 10),
                                   control = list(reltol = 1e-4, 
                                                  trace = 1,
                                                  parscale = c(alpha_C = 10^log10(start_pars$start_alpha_C[start_index_down]), # Estimate parameter scaling for optimization
                                                               beta_C = 0.1))),
                       silent = TRUE)
  
  est_pars_up <- try(bbmle::mle2(minuslogl = gapctd:::ctm_obj,
                                 start = list(alpha_C = start_pars$start_alpha_C[start_index_up],
                                              beta_C = start_pars$start_beta_C[start_index_up]),
                                 data = list(dc = NULL,
                                             uc = uc,
                                             area_method = area_method),
                                 method = "L-BFGS-B",
                                 lower = c(alpha_C = 0, beta_C = 1/45),
                                 upper = c(alpha_C = 1, beta_C = 10),
                                 control = list(reltol = 1e-4, 
                                                trace = 1,
                                                parscale = c(alpha_C = 10^log10(start_pars$start_alpha_C[start_index_up]),  # Estimate parameter scaling for optimization
                                                             beta_C = 0.1))),
                     silent = TRUE)
  
  conv_df <- data.frame(cast_direction = c("both", "downcast", "upcast"),
                        error = c(class(est_pars)[1] == "try-error",
                                  class(est_pars_down)[1] == "try-error",
                                  class(est_pars_up)[1] == "try-error"))
  conv_df$convergence <- c(ifelse(!conv_df$error[1], est_pars@details$convergence == 0, FALSE),
                           ifelse(!conv_df$error[2], est_pars_down@details$convergence == 0, FALSE),
                           ifelse(!conv_df$error[3], est_pars_up@details$convergence == 0, FALSE))
  
  out <- list()
  # Return estimate from both profiles
  if(conv_df$convergence[1]) {
    out[['both']] <- est_pars@coef
  } 
  
  # Return downcast estimates
  if(!conv_df$convergence[1] & conv_df$convergence[2]) {
    out[['both']] <- est_pars_down@coef
  }
  
  # Return upcast estimates
  if(!conv_df$convergence[1]  & !conv_df$convergence[2] & conv_df$convergence[3]) {
    out[['both']] <- est_pars_up@coef
  }
  
  # Return default
  if(!conv_df$convergence[1] & !conv_df$convergence[2] & !conv_df$convergence[3]) {
    out[['both']] <- default_parameters
  }
  
  if(conv_df$convergence[2]) {
    out[['downcast']] <- est_pars_down@coef
  }
  
  if(conv_df$convergence[3]) {
    out[['upcast']] <- est_pars_up@coef
  }
  
  return(out)
  
}



#' Objective function for cell thermal mass calculations (R workflow)
#' 
#' Calculates area between temperature and salinity curves when upcast and downcast data are provided. Calculates path distance for salinity if only downcast or upcast is provided.
#' 
#' @param dc Downcast oce object
#' @param uc Upcast oce object
#' @param alpha_C Conductivity cell thermal inertia correction alpha parameter, passed to gapctd::conductivity_correction()
#' @param beta_C Conductivity cell thermal inertia correction beta parameter, passed to gapctd::conductivity_correction()
#' @param area_method Area between temperature-salinity ("ts") curves, depth-salinity curves ("zs"), or pressure-salinity curves ("ps")
#' @return Area between T-S curves, Z-S curves, or path distance of salinity curve as a 1L numeric vector 
#' @export

ctm_obj <- function(dc = NULL, uc = NULL, alpha_C, beta_C, area_method = "ts") {
  
  # Area when both dc and uc are provided
  if(!is.null(dc) & !is.null(uc)) {
    dc_eval <- dc |>
      gapctd:::conductivity_correction(alpha_C = alpha_C, beta_C = beta_C) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1, exclude_surface = 4)
    
    uc_eval <- uc |>
      gapctd:::conductivity_correction(alpha_C = alpha_C, beta_C = beta_C) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "upcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1, exclude_surface = 4)
    
    obj <- gapctd:::ts_area(dc = dc_eval, 
                            uc = uc_eval, 
                            by = "depth", 
                            return_sf = FALSE,
                            area_method = area_method)
  }
  
  # Path distance
  if(!is.null(dc) & is.null(uc)) {
    dc_eval <- dc |>
      gapctd:::conductivity_correction(alpha_C = alpha_C, beta_C = beta_C) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1, exclude_surface = 4)
    
    obj <- sum(abs(diff(dc_eval@data$salinity[dc_eval@data$flag == 0])))
  }
  
  if(is.null(dc) & !is.null(uc)) {
    uc_eval <- uc |>
      gapctd:::conductivity_correction(alpha_C = alpha_C, beta_C = beta_C) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "upcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1, exclude_surface = 4)
    
    obj <- sum(abs(diff(uc_eval@data$salinity[uc_eval@data$flag == 0])))
  }
  
  return(obj)
}



#' Conductivity thermal inertia correction parameter a
#' 
#' Calculate Sea-Bird conductivity cell thermal mass correction parameter a, which is used to apply thermal inertia correction to conductivity measurements.
#' 
#' @param alpha Numeric vector (1L). Alpha parameter in thermal mass correction formula. Default = 0.04 for SBE19plus.
#' @param beta Numeric vector (1L). 1/beta parameter in thermal mass correction formula. Default = 1/8 for SBE19plus.
#' @param f_n Numeric vector (1L). Scan interval in seconds. Default = 0.25 for SBE19plus default 4 Hz scan interval.
#' @export

ctm_par_a <- function(alpha = 0.04, beta = 1/8, f_n = 0.25) {
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
#' @param precision Precision (significant digits) for conductivity (default = 6).
#' @export

ctm_correct_c_t <- function(a, b, temperature, precision = 6) {
  c_t <- numeric(length = length(temperature))
  c_t[1] <- 0
  
  start_index <- min(which(!is.na(temperature))) + 1
  end_index <- max(which(!is.na(temperature)))
  
  for(i in start_index:end_index) {
    c_t[i] <- -1 * b * c_t[i-1] + a * 0.1 * (1 + 0.006 * (temperature[i] - 20)) * (temperature[i] - temperature[i-1])
  }
  c_t <- round(c_t, 6)
  return(c_t)
}



#' IN DEVELOPMENT Calculate area between temperature-salinity curves (R workflow)
#' 
#' @param dc downcast oce object
#' @param uc upcast oce object
#' @param area_method Area between temperature-salinity ("ts") curves, depth-salinity curves ("zs"), or pressure-salinity curves ("ps")
#' @param return_mesh Logical. If TRUE, returns 'triSht' object generated by interp::tri.mesh(). Otherwise returns the sum of polygon areas as a 1L numeric vector.
#' @return When return_mesh = TRUE, returns a 'triSht' mesh. If return_mesh = FALSE, returns a 1L numeric vector with the sum of polygon areas.
#' @noRd

ts_area_delaunay <- function(dc, uc, by = "pressure", return_mesh = FALSE, area_method = "ts") {
  
  pd_switch <- ifelse(by == "pressure", "depth", "pressure")
  
  dc_df <- as.data.frame(dc@data) |>
    dplyr::select(pressure, temperature, salinity, depth)
  dc_cols <- which(names(dc_df) %in% c("temperature", "salinity", pd_switch))
  names(dc_df)[dc_cols] <- paste0("dc_", names(dc_df)[dc_cols])
  
  uc_df <- as.data.frame(uc@data) |>
    dplyr::select(pressure, temperature, salinity, depth)
  uc_cols <- which(names(uc_df) %in% c("temperature", "salinity", pd_switch))
  names(uc_df)[uc_cols] <- paste0("uc_", names(uc_df)[uc_cols])
  
  comb_df <- dplyr::full_join(dc_df, uc_df, by = by)
  x_ind <- which(names(comb_df) == by)
  
  missing_vars <- unique(which(is.na(comb_df), arr.ind = TRUE)[,2])
  
  # Interpolate/extrapolate missing for area calculations
  for(jj in missing_vars) {
    comb_df[, jj][which(is.na(comb_df[, jj]))] <- approx(x = comb_df[, x_ind],
                                                         y = comb_df[, jj],
                                                         xout = comb_df[, x_ind][which(is.na(comb_df[, jj]))],
                                                         method = "linear",
                                                         yleft = comb_df[, jj][min(which(!is.na(comb_df[, jj])))],
                                                         yright = comb_df[, jj][max(which(!is.na(comb_df[, jj])))])$y
  }
  
  comb_df <- comb_df[order(comb_df[[by]]) ,]
  
  if(area_method == "ts") {
    
    fit_df <- data.frame(x = c(comb_df$dc_temperature, rev(comb_df$uc_temperature)),
                         y = c(comb_df$dc_salinity, rev(comb_df$uc_salinity))) |>
      unique()
    
    tri_mesh <- tri.mesh(fit_df)
    
    # ts_area_sf <- comb_df |>
    #   data.frame(geometry = paste0("LINESTRING (", apply(X = 
    #                                                        cbind(
    #                                                          apply(
    #                                                            X = cbind(
    #                                                              comb_df$dc_salinity[1:(nrow(comb_df))],
    #                                                              comb_df$dc_temperature[1:(nrow(comb_df))]),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            cbind(c(comb_df$uc_salinity[1:(nrow(comb_df)-1)], comb_df$uc_salinity[(nrow(comb_df)-1)]),
    #                                                                  c(comb_df$uc_temperature[1:(nrow(comb_df)-1)], comb_df$uc_temperature[(nrow(comb_df)-1)])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            X = cbind(c(comb_df$dc_salinity[2:(nrow(comb_df))],comb_df$uc_salinity[(nrow(comb_df))]),
    #                                                                      c(comb_df$dc_temperature[2:(nrow(comb_df))],comb_df$uc_temperature[(nrow(comb_df))])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            X = cbind(
    #                                                              c(comb_df$dc_salinity[1:(nrow(comb_df)-1)],comb_df$dc_salinity[(nrow(comb_df))]),
    #                                                              c(comb_df$dc_temperature[1:(nrow(comb_df)-1)],comb_df$dc_temperature[(nrow(comb_df))])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " ")),
    #                                                      MARGIN = 1,
    #                                                      FUN = paste,
    #                                                      collapse = ", "), ")"))
  } else if(area_method == "zs") {
    
    fit_df <- data.frame(x = c(comb_df$dc_depth, rev(comb_df$uc_depth)),
                         y = c(comb_df$dc_salinity, rev(comb_df$uc_salinity))) |>
      unique()
    
    tri_mesh <- tri.mesh(fit_df)
    
    # ts_area_sf <- comb_df |>
    #   data.frame(geometry = paste0("LINESTRING (", apply(X = 
    #                                                        cbind(
    #                                                          apply(
    #                                                            X = cbind(
    #                                                              comb_df$dc_salinity[1:(nrow(comb_df))],
    #                                                              comb_df$dc_depth[1:(nrow(comb_df))]),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            cbind(c(comb_df$uc_salinity[1:(nrow(comb_df)-1)], comb_df$uc_salinity[(nrow(comb_df)-1)]),
    #                                                                  c(comb_df$uc_depth[1:(nrow(comb_df)-1)], comb_df$uc_depth[(nrow(comb_df)-1)])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            X = cbind(c(comb_df$dc_salinity[2:(nrow(comb_df))],comb_df$uc_salinity[(nrow(comb_df))]),
    #                                                                      c(comb_df$dc_depth[2:(nrow(comb_df))],comb_df$uc_depth[(nrow(comb_df))])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            X = cbind(
    #                                                              c(comb_df$dc_salinity[1:(nrow(comb_df)-1)],comb_df$dc_salinity[(nrow(comb_df))]),
    #                                                              c(comb_df$dc_depth[1:(nrow(comb_df)-1)],comb_df$dc_depth[(nrow(comb_df))])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " ")),
    #                                                      MARGIN = 1,
    #                                                      FUN = paste,
    #                                                      collapse = ", "), ")"))
  } else if(area_method == "ps") {
    
    fit_df <- data.frame(x = c(comb_df$pressure, rev(comb_df$pressure)),
                         y = c(comb_df$dc_salinity, rev(comb_df$uc_salinity))) |>
      unique()
    
    # ts_area_sf <- comb_df |>
    #   data.frame(geometry = paste0("LINESTRING (", apply(X = 
    #                                                        cbind(
    #                                                          apply(
    #                                                            X = cbind(
    #                                                              comb_df$dc_salinity[1:(nrow(comb_df))],
    #                                                              comb_df$dc_pressure[1:(nrow(comb_df))]),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            cbind(c(comb_df$uc_salinity[1:(nrow(comb_df)-1)], comb_df$uc_salinity[(nrow(comb_df)-1)]),
    #                                                                  c(comb_df$uc_pressure[1:(nrow(comb_df)-1)], comb_df$uc_pressure[(nrow(comb_df)-1)])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            X = cbind(c(comb_df$dc_salinity[2:(nrow(comb_df))],comb_df$uc_salinity[(nrow(comb_df))]),
    #                                                                      c(comb_df$dc_pressure[2:(nrow(comb_df))],comb_df$uc_pressure[(nrow(comb_df))])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " "),
    #                                                          apply(
    #                                                            X = cbind(
    #                                                              c(comb_df$dc_salinity[1:(nrow(comb_df)-1)],comb_df$dc_salinity[(nrow(comb_df))]),
    #                                                              c(comb_df$dc_pressure[1:(nrow(comb_df)-1)],comb_df$dc_pressure[(nrow(comb_df))])),
    #                                                            MARGIN = 1,
    #                                                            FUN = paste, 
    #                                                            collapse = " ")),
    #                                                      MARGIN = 1,
    #                                                      FUN = paste,
    #                                                      collapse = ", "), ")"))
  }
  
  # ts_area_sf <- ts_area_sf |>
  #   dplyr::mutate(ID = row_number()) |>
  #   st_as_sf(wkt = "geometry") |> 
  #   dplyr::group_by(ID) |>
  #   summarise(do_union = FALSE) |>
  #   sf::st_cast(to = "POLYGON")
  
  # sum_area <- sf::st_area(ts_area_sf)
  
  if(return_mesh) {
    return(tri_mesh)
    # return(ts_area_sf)
  } else {
    return(sum_area)
  }
}


#' Calculate area between temperature-salinity curves (R workflow)
#' 
#' @param dc downcast oce object
#' @param uc upcast oce object
#' @param area_method Area between temperature-salinity ("ts") curves, depth-salinity curves ("zs"), or pressure-salinity curves ("ps")
#' @param return_sf Logical. If TRUE, returns sf object with polygons. Otherwise returns the sum of polygon areas as a 1L numeric vector.
#' @return When return_sf = TRUE, an sf object (POLYGON) of the area between T-S curves. When return_sf = FALSE, a 1L numeric vector with the sum of polygon areas.
#' @export

ts_area <- function(dc, uc, by = "pressure", return_sf = FALSE, area_method = "ts") {
  
  pd_switch <- ifelse(by == "pressure", "depth", "pressure")
  
  dc_df <- as.data.frame(dc@data) |>
    dplyr::select(pressure, temperature, salinity, depth)
  dc_cols <- which(names(dc_df) %in% c("temperature", "salinity", pd_switch))
  names(dc_df)[dc_cols] <- paste0("dc_", names(dc_df)[dc_cols])
  
  uc_df <- as.data.frame(uc@data) |>
    dplyr::select(pressure, temperature, salinity, depth)
  uc_cols <- which(names(uc_df) %in% c("temperature", "salinity", pd_switch))
  names(uc_df)[uc_cols] <- paste0("uc_", names(uc_df)[uc_cols])
  
  comb_df <- dplyr::full_join(dc_df, uc_df, by = by)
  x_ind <- which(names(comb_df) == by)
  
  missing_vars <- unique(which(is.na(comb_df), arr.ind = TRUE)[,2])
  
  comb_df <- comb_df[order(comb_df[[by]]) ,]
  
  # Interpolate/extrapolate missing for area calculations
  for(jj in missing_vars) {
    comb_df[, jj][which(is.na(comb_df[, jj]))] <- approx(x = comb_df[, x_ind],
                                                         y = comb_df[, jj],
                                                         xout = comb_df[, x_ind][which(is.na(comb_df[, jj]))],
                                                         method = "linear",
                                                         yleft = comb_df[, jj][min(which(!is.na(comb_df[, jj])))],
                                                         yright = comb_df[, jj][max(which(!is.na(comb_df[, jj])))])$y
  }
  
  ts_area_boundary_sf <- data.frame(geometry = paste0("LINESTRING (", 
                                                      paste(c(
                                                        apply(
                                                          cbind(
                                                            c(comb_df$dc_salinity, comb_df$uc_salinity[(nrow(comb_df))]),
                                                            
                                                            c(comb_df$dc_temperature, comb_df$uc_temperature[(nrow(comb_df))])),
                                                          MARGIN = 1,
                                                          FUN = paste, 
                                                          collapse = " "),
                                                        apply(
                                                          cbind(c(rev(comb_df$uc_salinity), comb_df$dc_salinity[1]),
                                                                c(rev(comb_df$uc_temperature), comb_df$dc_temperature[1])),
                                                          MARGIN = 1,
                                                          FUN = paste, 
                                                          collapse = " ")), collapse = ", "), ")")) |> 
    st_as_sf(wkt = "geometry") |>
    sf::st_cast(to = "POLYGON") |>
    sf::st_make_valid() |>
    sf::st_cast(to = "POLYGON", group_or_split = TRUE)
  
  total_area <- sum(sf::st_area(ts_area_boundary_sf), na.rm = TRUE)
    
  if(return_sf) {
    
    cast_df <- dplyr::bind_rows(
      data.frame(temperature = comb_df$dc_temperature,
                 salinity =  comb_df$dc_salinity,
                 cast = "downcast"),
      data.frame(temperature = comb_df$uc_temperature,
                 salinity =  comb_df$uc_salinity,
                 cast = "upcast")
    )
    
    return(list(sf = ts_area_boundary_sf, cast_df = cast_df, area = total_area))
  } else {
    return(total_area)
  }
}

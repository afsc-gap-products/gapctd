#' Estimate time adjustment for Align CTD (SBEDP workflow)
#' 
#' Find the time adjustment that maximizes correlations of first time difference (dx/dt) between variables, similar to Ullman and Hebert (2014) and Dever et al. (2020). 
#' 
#' @param var1 Numeric vector of measurements from the first sensors (e.g., conductivity)
#' @param var2 Numeric vector of measurements from the sensor for which measurements will be time-adjusted (e.g., temperature)
#' @param time1 Scan time in seconds as a numeric vector for the first sensor.
#' @param time2 Scan time in seconds as a numeric vector for the second sensor.
#' @param time2_adjustments Adjustments to try, in seconds, as a numeric vector.
#' @param cor_method Correlation method to use to evaluate time2_adjustment as a character vector. See documentation for cor().
#' @param interp_fn Character vector indicating which interpolation function to use ("approx" or "oce::oce.approx") to estimate time-shifted values at shifted scan times.
#' @param interp_method Interpolation method to apply to estimate time-shifted measurements. See documentation for approx() and oce::oce.approx()
#' @param return_adj Logical indicating whether to return only the time2_adjustments value with the best correlation (TRUE) or a list containing additional results (see Details below)
#' @details This function time-shifts measurements from one sensor (e.g. temperature) relative to measurements from another sensor (e.g. conductivity) for all user-provided time2_adjustment values. It then estimates a correlations between time-shifted sensor measurements and returns either the best time2_adustment estimate or a list containing the best time2_adjustment, highest correlation, and data.frame containing results of all time2_adustment comparisons. Time-shifted values between measurements can be interpolated using "rr" or "unesco" from oce::oce.approx or the "linear" method in approx. Correlation methods are any valid methods in cor().
#' @references Ullman, D.S., Hebert, D., 2014. Processing of underway CTD data. J. Atmos. Ocean. Technol. 31, 984–998. https://doi.org/10.1175/JTECH-D-13-00200.1
#' @references Dever, M., Freilich, M., Farrar, J.T., Hodges, B., Lanagan, T., Baron, A.J., Mahadevan, A., 2020. EcoCTD for profiling oceanic physical–biological properties from an underway ship. J. Atmos. Ocean. Technol. 37, 825–840. https://doi.org/10.1175/JTECH-D-19-0145.1
#' @export

estimate_alignment <- function(var1, var2, time1, time2, time2_adjustments = seq(-1.5,1.5,0.01), cor_method = "spearman", interp_method = "linear", interp_fn = "approx", only_return_adj = TRUE) {
  
  series_1 <- data.frame(t = time1,
                         x1 = var1)
  
  s2_series <- data.frame(t = time2,
                          x1 = var2)
  
  corr_vec <- numeric(length = length(time2_adjustments))
  
  for(jj in 1:length(time2_adjustments)) {
    
    series_2 <- s2_series
    series_2$t <- series_2$t + time2_adjustments[jj]
    
    loc_pred <- data.frame(locs = unique(c(series_1$t, series_2$t)))
    
    loc_pred$s1 <- switch(interp_fn,
                          `oce.approx` = {oce::oce.approx(x = series_1$t, 
                                                          y = series_1$x1, 
                                                          xout = loc_pred$locs, 
                                                          method = interp_method)},
                          `approx` = { approx(x = series_1$t, 
                                              y = series_1$x1, 
                                              xout = loc_pred$locs, 
                                              method = interp_method)$y})
    
    loc_pred$s2 <- switch(interp_fn,
                          `oce.approx` = {oce::oce.approx(x = series_2$t, 
                                                          y = series_2$x1, 
                                                          xout = loc_pred$locs, 
                                                          method = interp_method)},
                          `approx` = {approx(x = series_2$t, 
                                             y = series_2$x1, 
                                             xout = loc_pred$locs, 
                                             method = interp_method)$y})
    
    obj <- -1*cor(diff(loc_pred$s1), 
                  diff(loc_pred$s2), 
                  use = "complete.obs",
                  method = cor_method)^2
    corr_vec[jj] <- obj
    
  }
  
  if(only_return_adj) {
    return(-1*time2_adjustments[which.min(corr_vec)])
  } else{
    return(list(time2_adj = -1*time2_adjustments[which.min(corr_vec)],
                adj_cor = -1*min(corr_vec),
                adj_est = data.frame(time2_adjustment = time2_adjustments,
                                     correlation = corr_vec,
                                     cor_method = cor_method)))
  }
}


#' Wrapper function around estimate_alignment (SBEDP workflow)
#' 
#' Estimate alignment for a channel
#' 
#' @param profile_files Paths to profile files for which alignments should be estimated, as a character vector. For trawl data, should have separate upcast and downcast files.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, especially in downcasts.
#' @param make_diagnostic_plots Logical. Should diagnostic plots be saved to plots/alignment.pdf?
#' @param ... Optional arguments passed to gapctd::estimate_alignment().
#' @export

run_alignment_calcs <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                  full.names = TRUE, pattern = "downcast.cnv"),
                                                       list.files(here::here("output", "sbe19plus_v0"), 
                                                                  full.names = TRUE, pattern = "upcast.cnv"))),
                                make_diagnostic_plots = TRUE,
                                min_pressure = 4,
                                ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  cast_direction <- character(length = length(deploy_id))
  cast_direction[grepl(pattern = "downcast", x = profile_files)] <- "downcast"
  cast_direction[grepl(pattern = "upcast", x = profile_files)] <- "upcast"
  
  best_eval <- numeric(length = length(profile_files))
  best_corr <- numeric(length = length(profile_files))
  prof_rate <- numeric(length = length(profile_files))
  
  if(make_diagnostic_plots) {
    pdf(file = here::here("plots", "alignment.pdf"), onefile = TRUE)
  }
  
  for(kk in 1:length(profile_files)) {
    
    ctd_profile <- try(oce::read.ctd(file = profile_files[kk]), silent = TRUE)
    
    if(class(ctd_profile) == "try-error") next
    
    prof_rate[kk] <- mean(diff(ctd_profile@data$pressure[ctd_profile@data$pressure > min_pressure])/ctd_profile@metadata$sampleInterval)
    
    # If oxygen is added, will need to identify the adjustment column in output, which could be be passed to estimate_alignment
    cond <- ctd_profile@data$conductivity[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    cond_time <- ctd_profile@data$timeS[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    temp <- ctd_profile@data$temperature[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    temp_time <- ctd_profile@data$timeS[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)]
    
    if(any(length(cond) < 10, 
           length(cond_time) < 10, 
           length(temp) < 10, 
           length(temp_time) < 10)) {
      next
    }
    
    est <- gapctd::estimate_alignment(var1 = cond,
                                      time1 = cond_time, 
                                      var2 = temp,
                                      time2 = temp_time,
                                      only_return_adj = FALSE, ...)
    
    best_eval[kk] <- round(est$time2_adj, 3)
    best_corr[kk] <- est$adj_cor
    
    if(make_diagnostic_plots) {
      plot(
        est$adj_est$time2_adjustment, 
        est$adj_est$correlation, 
        main = paste0(deploy_id[kk], " ", cast_direction[kk], "\n", est$time2_adj),
        xlab = "Adjustment",
        ylab = "Correlation",
      )
    }
  }
  
  if(make_diagnostic_plots) {dev.off()}
  
  return(data.frame(deploy_id = deploy_id,
                    cast_direction = cast_direction,
                    best_adj = best_eval,
                    best_corr = best_corr,
                    profile_rate = prof_rate))
}

#' Correlation between two channels (R workflow)
#' 
#' Calculate correlation coefficient between two channels (e.g. temperature and conductivity)
#' 
#' @param x oce object
#' @param exclude_flag Logical denoting whether bad scans (flag < 0) should be excluded.
#' @param c1 Name of first channel as a character vector. Default = "temperature"
#' @param c2 Name of second channel as a character vector. Default = "conductivity"
#' @param min_pressure Minimum pressure to use for correlations
#' @return Correlation coefficient (1L numeric)
#' @noRd

channel_correlation <- function(x, exclude_flag = TRUE, c1 = "temperature", c2 = "conductivity", min_pressure = 4, cor_method = "pearson") {
  
  if(exclude_flag) {
    T_vec <- x@data[[c1]][x@data$flag >= 0 & x@data$pressure >= min_pressure]
    C_vec <- x@data[[c2]][x@data$flag >= 0 & x@data$pressure >= min_pressure]
  }
  
  if(!exclude_flag) {
    T_vec <- x@data[[c1]][x@data$pressure >= min_pressure]
    C_vec <- x@data[[c2]][x@data$pressure >= min_pressure]
  }
  
  cor_coef <- try(cor(T_vec,
                      C_vec,
                      use = "complete.obs",
                      method = cor_method), silent = TRUE)
  
  if(class(cor_coef) == "try-error") {
    cor_coef <- 0
  }
  
  return(cor_coef)
}


#' Estimate channel alignment parameter (R workflow)
#'
#' Estimate optimal alignment parameter for a channel (e.g. "temperature") within a range of candidate alignment offsets. The 'best' alignment is taken to be the one that maximizes the correlation between channels.
#' 
#' @param x oce object
#' @param cast_direction Cast direction as a character vector ("downcast" or "upcast"). Passed to gapctd::loop_edit()
#' @param variable Variable to be aligned ("temperature")
#' @param offsets Numerical vector of offsets to evaluate, in seconds.
#' @param cor_method Correlation method, passed to gapctd::channel_correlation().
#' @param ... Additional arguments passed to gapctd::channel_correlation()
#' @return A named numeric vector containing the estimated optimal offset ("best_offset") and correlation for the offset ("best_corr")
#' @noRd

optim_align_par <- function(x, cast_direction, variable = "temperature", offsets = seq(-1,1, 0.01), cor_method = "pearson", ...) {

  offset_corr <- numeric(length = length(offsets))
  
  for(ii in 1:length(offsets)) {
    
    if(variable == "temperature") {
      offset_corr[ii] <- x |>
        gapctd:::align_var(variables = variable, offset = offsets[ii], interp_method = "linear") |>
        gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = cast_direction) |>
        gapctd:::channel_correlation(exclude_flag = TRUE, 
                                     min_pressure = 4, 
                                     cor_method = cor_method, 
                                     c1 = "temperature", 
                                     c2 = "conductivity")
      
      x@data["temperature"]
    }
    
    if(variable == "oxygen") {
      
    }

  }
  
  out <- c(best_offset = offsets[which.max(offset_corr)[1]],
           best_corr = offset_corr[which.max(offset_corr)[1]])
  return(out)
  
}
#' Wrapper function around estimate_alignment
#' 
#' Estimate alignment for a channel
#' 
#' @param profile_files Paths to profile files for which alignments should be estimated, as a character vector. For trawl data, should have separate upcast and downcast files.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, espeically in downcasts.
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
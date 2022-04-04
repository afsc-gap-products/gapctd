#' Wrapper function around estimate_alignment
#' 
#' @param profile_files Paths to profile files for which alignments should be estimated, as a character vector. For trawl data, should have separate upcast and downcast files.
#' @param min_pressure Minimum pressure to use for alignment correlation. It is advisable to exclude near-surface values due to spikes in conductivity, espeically in downcasts.
#' @param scan_interval Scan interval for the CTD.
#' @param make_diagnostic_plots Logical. Should diagnostic plots be saved to plots/alignment.pdf?
#' @export


run_alignment_calcs <- function(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v2"), 
                                                                  full.names = TRUE, pattern = "downcast.cnv"),
                                                       list.files(here::here("output", "sbe19plus_v2"), 
                                                                  full.names = TRUE, pattern = "upcast.cnv"))),
                                make_diagnostic_plots = TRUE,
                                min_pressure = 4,
                                # scan_interval = 0.25,
                                ...) {
  
  deploy_id <- sub("\\_raw.*", "", profile_files)
  deploy_id <- sub(pattern = sub("[^/]+$", "", deploy_id)[1], "", deploy_id)
  deploy_id[grepl(pattern = "downcast", x = profile_files)] <- paste0(deploy_id, "_downcast")[grepl(pattern = "downcast", x = profile_files)] 
  deploy_id[grepl(pattern = "upcast", x = profile_files)] <- paste0(deploy_id, "_upcast")[grepl(pattern = "upcast", x = profile_files)] 
  
  best_eval <- numeric(length = length(profile_files))
  best_corr <- numeric(length = length(profile_files))
  prof_rate <- numeric(length = length(profile_files))
  profile_id <- character(length = length(profile_files))
  
  if(make_diagnostic_plots) {
    pdf(file = here::here("plots", "alignment.pdf"), onefile = TRUE)
  }
  
  for(kk in 1:length(profile_files)) {
    
    profile_id[kk] <- deploy_id[kk]
    
    ctd_profile <- try(oce::read.ctd(file = profile_files[kk]), silent = TRUE)
    
    if(class(ctd_profile) == "try-error") next
    
    prof_rate[kk] <- mean(diff(ctd_profile@data$pressure[ctd_profile@data$pressure > min_pressure])/ctd_profile@metadata$sampleInterval)
    
    
    est <- gapctd::estimate_alignment(var1 = ctd_profile@data$conductivity[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)],
                                      time1 = ctd_profile@data$timeS[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)], 
                                      var2 = ctd_profile@data$temperature[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)],
                                      time2 =  ctd_profile@data$timeS[ctd_profile@data$pressure > min_pressure & !is.na(ctd_profile@data$flag)],
                                      only_return_adj = FALSE, ...)
    
    best_eval[kk] <- est$time2_adj
    best_corr[kk] <- est$adj_cor
    
    if(make_diagnostic_plots) {
      suppressMessages(
        print(
          plot(
            est$adj_est$time2_adjustment, 
            est$adj_est$correlation, 
            main = paste0(profile_id[kk], "\n", est$time2_adj),
            xlab = "Adjustment",
            ylab = "Correlation",
          )
        )
      )
    }
  }
  
  if(make_diagnostic_plots) {dev.off()}
  
  return(data.frame(profile_id = profile_id,
                    best_adj = best_eval,
                    best_corr = best_corr,
                    profile_rate = prof_rate))
  
}
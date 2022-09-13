#' Visually inspect profiles (R workflow)
#' 
#' Visually inspect profile data to evaluate whether they are acceptable. Profiles that are acceptable will be copied to a review directory. Unacceptable profiles will be removed from the directory and may need to be re-evaluated using manual_flag_review() or other remedial measures. This function skips profiles that have already been reviewed and accepted.
#' 
#' @param rds_dir_path Filepath to directory containing rds files to be reviewed.
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses. GTSPP uses a density threshold of 0.05.
#' @param in_pattern Character vector search pattern for input files.
#' @return Writes accepted profiles to rds_dir_path with suffix "_final.rds"
#' @export

review_profiles <- function(rds_dir_path, threshold = -1e-5, in_pattern = "_qc.rds") {
  
  rds_files <- list.files(rds_dir_path, full.names = TRUE, pattern = in_pattern)
  rds_short <- list.files(rds_dir_path, full.names = FALSE, pattern = in_pattern)
  out_files <- here::here(rds_dir_path, 
                          gsub(pattern = in_pattern, 
                               replacement = "_final.rds", 
                               x = rds_short))
  
  for(ii in 1:length(rds_files)) {
    
    if(!file.exists(out_files[ii])) {
      message("review_profiles: Reviewing ", rds_files[ii])
      ctd_dat <- readRDS(file = rds_files[ii])
      
      n_casts <- sum(names(ctd_dat) %in% c("downcast", "upcast"))
      
      keep_dc <- FALSE
      keep_uc <- FALSE
      if("downcast" %in% names(ctd_dat)) {
        par(mfrow = c(1,2))
        oce::plot(ctd_dat$downcast, which = 1, type = 'l')
        oce::plot(ctd_dat$downcast, which = 2, type = 'l')
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
        
        keep_dc <- tolower(readline(prompt = "Accept profile? (y or n): ")) == "y"
        
      }
      
      if("upcast" %in% names(ctd_dat)) {
        par(mfrow = c(1,2))
        oce::plot(ctd_dat$upcast, which = 1, type = 'l')
        oce::plot(ctd_dat$upcast, which = 2, type = 'l')
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
        
        keep_uc <- tolower(readline(prompt = "Accept profile? (y or n): ")) == "y"
      }
      
      if(!keep_dc & !keep_uc) {
        message(paste0("manual_review: Not keeping casts from ", rds_short, ". Rerun wrapper_flag_interpolate() then review_profiles() if profiles from this deployment need further correction. Otherwise, casts from this deployment will be exluded from output."))
        file.remove(rds_files[ii])
        next
      } 
      
      if(keep_dc & keep_uc) {
        file.copy(from = rds_files[ii], 
                  to = out_files[ii])
      } else {
        if(n_casts == 2) {
          continue <- tolower(readline(prompt = "Only one cast accepted. Should only one cast be accepted? (y or n): ")) == "y"
          if(!continue) {
            file.remove(rds_files[ii])
            next
          }
        }
        
        if(!keep_uc) {
          ctd_dat <- ctd_dat[which(names(ctd_dat) == "upcast")]
          saveRDS(object = ctd_dat, file = out_files[ii])
        }
        
        if(!keep_dc) {
          ctd_dat <- ctd_dat[which(names(ctd_dat) == "downcast")]
          saveRDS(object = ctd_dat, file = out_files[ii])
        }
      }
    }
  }
}
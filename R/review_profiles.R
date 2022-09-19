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
  
  dc_files <- here::here(rds_dir_path, 
                          gsub(pattern = in_pattern, 
                               replacement = "_dc_final.rds", 
                               x = rds_short))
  
  uc_files <- here::here(rds_dir_path, 
                         gsub(pattern = in_pattern, 
                              replacement = "_uc_final.rds", 
                              x = rds_short))
  
  accept_response <- function(valid_responses, prompt) {
    
    accept <- FALSE 
    
    while(!accept) {
      entry <- readline(prompt = prompt)
      
      if((entry %in% valid_responses) & length(entry) == 1) {
        accept <- TRUE
      } else {
        message("Invalid entry, please try again. Must be one of ", paste(valid_responses, sep = ", "))
      }
    }
    return(entry)
  }
  
  
  for(ii in 1:length(rds_files)) {
    
    if(!file.exists(out_files[ii])) {
      
      message("review_profiles: Reviewing ", rds_files[ii])
      ctd_dat <- readRDS(file = rds_files[ii])
      
      if(file.exists(dc_files[ii])) {
        message("reviewing_profiles: Downcast file exists. Only reviewing upcast.")
        dc_index <- which(names(ctd_dat) == c("downcast"))
        ctd_dat <- ifelse(length(dc_index) == 1, ctd_dat[-dc_index], ctd_dat)
      }
      
      if(file.exists(uc_files[ii])) {
        message("reviewing_profiles: Upcast file exists. Only reviewing downcast.")
        uc_index <- which(names(ctd_dat) == c("upcast"))
        ctd_dat <- ifelse(length(uc_index) == 1, ctd_dat[-uc_index], ctd_dat)
      }
      
      n_casts <- sum(names(ctd_dat) %in% c("downcast", "upcast"))
      
      if(!any(c("upcast", "downcast") %in% names(ctd_dat))) {
        message("reviewing_profiles: No casts to review.")
        next
      }
      
      keep_dc <- FALSE
      keep_uc <- FALSE
      
      # Both downcast and upcast are in the rds file
      if(all(c("downcast", "upcast") %in% names(ctd_dat))) {
        par(mfrow = c(2,2))
        oce::plot(ctd_dat$downcast, which = 1, type = 'l', main = "downcast")
        oce::plot(ctd_dat$downcast, which = 2, type = 'l', main = "downcast")
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
        
        oce::plot(ctd_dat$upcast, which = 1, type = 'l', main = "upcast")
        oce::plot(ctd_dat$upcast, which = 2, type = 'l', main = "upcast")
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
        
        response <- accept_response(valid_responses = c(0, 1, "d", "u"),
                                    prompt = "Accept none (0), both (1), downcast (d), upcast (u)?: ")
        
        keep_dc <- response %in% c(1,"d")
        keep_uc <- response %in% c(1,"u")
        remove_rds <- !keep_dc & !keep_uc
        
        if(keep_dc & keep_uc) {
          file.copy(from = rds_files[ii], 
                    to = out_files[ii])
          next
        }
      }
      
      # Only upcast or downcast is in the rds file
      if("downcast" %in% names(ctd_dat) & !("upcast" %in% names(ctd_dat))) {
        par(mfrow = c(1,2))
        oce::plot(ctd_dat$downcast, which = 1, type = 'l', main = "downcast")
        oce::plot(ctd_dat$downcast, which = 2, type = 'l', main = "downcast")
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
        
        response <- accept_response(valid_responses = c(0, "d"),
                                    prompt = "Accept none (0) or downcast (d)?: ")
        keep_dc <- response == "d"
        remove_rds <- !keep_dc
        
      }
      
      if("upcast" %in% names(ctd_dat)  & !("downcast" %in% names(ctd_dat))) {
        par(mfrow = c(1,2))
        oce::plot(ctd_dat$upcast, which = 1, type = 'l', main = "upcast")
        oce::plot(ctd_dat$upcast, which = 2, type = 'l', main = "upcast")
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
        
        response <- accept_response(valid_responses = c(0, "u"),
                                    prompt = "Accept none (0) or upcast (u)?: ")
        keep_uc <- response == "u"
        remove_rds <- !keep_uc
      }
      
        if(remove_rds) {
          message(paste0("manual_review: Not keeping casts from ", rds_short[ii], ". Rerun wrapper_flag_interpolate() then review_profiles() to rectify data or casts from the deployment will be exluded from output."))
          file.remove(rds_files[ii])
        }

      # Keep only an upcast or a downcast
      if(!(keep_dc == keep_uc)) {
        if(keep_dc & !keep_uc) {
          saveRDS(ctd_dat, 
                  gsub(pattern = "_final.rds", replacement = "_dc_final.rds",  x = out_files[ii]))
        } 
        
        if(!keep_dc & keep_uc) {
          saveRDS(ctd_dat, 
                  gsub(pattern = "_final.rds", replacement = "_uc_final.rds",  x = out_files[ii]))
          
        } 
      }
      
    }
  }
}

# review_profiles <- function(rds_dir_path, threshold = -1e-5, in_pattern = "_qc.rds") {
#   
#   rds_files <- list.files(rds_dir_path, full.names = TRUE, pattern = in_pattern)
#   rds_short <- list.files(rds_dir_path, full.names = FALSE, pattern = in_pattern)
#   out_files <- here::here(rds_dir_path, 
#                           gsub(pattern = in_pattern, 
#                                replacement = "_final.rds", 
#                                x = rds_short))
#   
#   for(ii in 1:length(rds_files)) {
#     
#     if(!file.exists(out_files[ii])) {
#       message("review_profiles: Reviewing ", rds_files[ii])
#       ctd_dat <- readRDS(file = rds_files[ii])
#       
#       n_casts <- sum(names(ctd_dat) %in% c("downcast", "upcast"))
#       
#       keep_dc <- FALSE
#       keep_uc <- FALSE
#       if("downcast" %in% names(ctd_dat)) {
#         par(mfrow = c(1,2))
#         oce::plot(ctd_dat$downcast, which = 1, type = 'l')
#         oce::plot(ctd_dat$downcast, which = 2, type = 'l')
#         abline(v = threshold, lwd = 3, col = "brown")
#         abline(h = 1, lwd = 3, col = "black", lty = 2)
#         
#         keep_dc <- tolower(readline(prompt = "Accept profile? (y or n): ")) == "y"
#         
#       }
#       
#       if("upcast" %in% names(ctd_dat)) {
#         par(mfrow = c(1,2))
#         oce::plot(ctd_dat$upcast, which = 1, type = 'l')
#         oce::plot(ctd_dat$upcast, which = 2, type = 'l')
#         abline(v = threshold, lwd = 3, col = "brown")
#         abline(h = 1, lwd = 3, col = "black", lty = 2)
#         
#         keep_uc <- tolower(readline(prompt = "Accept profile? (y or n): ")) == "y"
#       }
#       
#       if(!keep_dc & !keep_uc) {
#         message(paste0("manual_review: Not keeping casts from ", rds_short, ". Rerun wrapper_flag_interpolate() then review_profiles() if profiles from this deployment need further correction. Otherwise, casts from this deployment will be exluded from output."))
#         file.remove(rds_files[ii])
#         next
#       } 
#       
#       if(keep_dc & keep_uc) {
#         file.copy(from = rds_files[ii], 
#                   to = out_files[ii])
#       } else {
#         if(n_casts == 2) {
#           continue <- tolower(readline(prompt = "Only one cast accepted. Should only one cast be accepted? (y or n): ")) == "y"
#           if(!continue) {
#             file.remove(rds_files[ii])
#             next
#           }
#         }
#         
#         if(!keep_uc) {
#           ctd_dat <- ctd_dat[which(names(ctd_dat) == "upcast")]
#           saveRDS(object = ctd_dat, file = out_files[ii])
#         }
#         
#         if(!keep_dc) {
#           ctd_dat <- ctd_dat[which(names(ctd_dat) == "downcast")]
#           saveRDS(object = ctd_dat, file = out_files[ii])
#         }
#       }
#     }
#   }
# }



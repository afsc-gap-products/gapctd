#' Remedial method to fix CTM errors using the best upcast or downcast parameter
#' 
#' Reads-in an rds file containing data that has been processed using the full gapctd workflow and uses the file name and estimated CTM parameters to reprocess data using the best upcast and downcast parameters.
#' 
#' @param rds_path Path to rds file
#' @param haul_df data.frame containing haul data from RACEBASE that includes metadata for the cnv file.
#' @return An upcast and downcast rds file if both casts were found in the rds file. Moves the original rds file to bad_cnv.
#' @export

split_ctm <- function(rds_path, haul_df) {
  
  rds_dat <- readRDS(file = rds_path)
  
  if(grepl(pattern = "_dc_", x = rds_path)) {
    rds_downcast <- rds_path
  } else {
    rds_downcast <- gsub(pattern = "_raw.rds", replacement = "_dc_raw.rds", x = rds_path)
  }
  
  if(grepl(pattern = "_dc_", x = rds_path)) {
    rds_upcast <- rds_path
  } else {
    rds_upcast <- gsub(pattern = "_raw.rds", replacement = "_uc_raw.rds", x = rds_path)
  }
  
  if("downcast" %in% names(rds_dat)) {
    
    ctd_dat <- oce::read.oce(file = rds_dat$downcast@metadata$filename)
    
    if(length(rds_dat$downcast@metadata$ctm) > 1) {
      if("downcast" %in% names(rds_dat$downcast@metadata$ctm)) {
        ctm_pars <- rds_dat$downcast@metadata$ctm$downcast
      } else {
        if("upcast" %in% names(rds_dat$downcast@metadata$ctm)) {
          ctm_pars <- rds_dat$downcast@metadata$ctm$upcast
        } else {
          ctm_pars <- rds_dat$downcast@metadata$ctm
        }
      }
      
      processed_oce <- gapctd::run_gapctd(x = ctd_dat, 
                                          haul_df = haul_df, 
                                          return_stages = "final",
                                          ctd_tz = "America/Anchorage",
                                          ctm_pars = ctm_pars)
      processed_oce <- processed_oce[which(names(processed_oce) %in% c("downcast", "bottom"))]
      saveRDS(object = processed_oce, file = rds_downcast)
    }
  }
  
  if("upcast" %in% names(rds_dat)) {
    
    ctd_dat <- oce::read.oce(file = rds_dat$upcast@metadata$filename)
    
    if(length(rds_dat$upcast@metadata$ctm) > 1) {
      if("upcast" %in% names(rds_dat$upcast@metadata$ctm)) {
        ctm_pars <- rds_dat$upcast@metadata$ctm$upcast
      } else {
        if("downcast" %in% names(rds_dat$upcast@metadata$ctm)) {
          ctm_pars <- rds_dat$upcast@metadata$ctm$upcast
        } else {
          ctm_pars <- rds_dat$upcast@metadata$ctm
        }
      }
      
      processed_oce <- gapctd::run_gapctd(x = ctd_dat, 
                                          haul_df = haul_df, 
                                          return_stages = "final",
                                          ctd_tz = "America/Anchorage",
                                          ctm_pars = ctm_pars)
      processed_oce <- processed_oce[which(names(processed_oce) %in% c("upcast", "bottom"))]
      saveRDS(object = processed_oce, file = rds_upcast)
    }
  }
  
  message("Moving ", rds_path, " to /bad_cnv/")
  file.rename(rds_path,   gsub(pattern = "output/gapctd",
                               replacement = "bad_cnv",
                               x = rds_path))
}



#' UI and wrapper for split_ctm
#' 
#' Visually inspect plots and decide if raw files should be reprocessed using cell thermal mass correction parameters for individual profiles instead of the parameters estimated for both casts. Profiles selected for additional CTM are processed using split_ctm.
#' 
#' @param rds_path Path to directory containing rds files.
#' @param haul_df data.frame containing haul data from RACEBASE that includes metadata for the cnv file.
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses.
#' @return Profiles (in "_raw.rds" files) processed using CTM parameters estimated for individual casts if cell thermal mass correction parameter estimation converged for the individual casts.
#' @export

remedial_ctm <- function(rds_path = here::here("output", "gapctd"), haul_df, threshold = -1e-5) {
  
  rds_files <- list.files(path = rds_path, full.names = TRUE, pattern = "_raw.rds")
  
  # Find _raw.rds that do not have corresponding _final.rds, _dc_final.rds, _uc_final.rds, _dc_raw.rds, or _uc_raw.rds
  check_final <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_final.rds")
  check_dc <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_dc_final.rds")
  check_dc2 <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_dc_dc_final.rds")
  check_uc <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_uc_final.rds")
  check_uc2 <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_uc_uc_final.rds")
  check_dc_raw <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_dc_raw.rds")
  check_uc_raw <- gsub(x = rds_files, pattern = "_raw.rds", replacement = "_uc_raw.rds")
  
  review_vec <- character(length = 0L)
  
  for(ii in 1:length(rds_files)) {
    final_exists <- any(file.exists(check_final[ii], 
                                    check_dc[ii],
                                    check_dc2[ii],
                                    check_uc[ii], 
                                    check_uc2[ii],
                                    check_dc_raw [ii],
                                    check_uc_raw[ii]))
    
    if(!final_exists) {
      
      message("remedial_ctm: Reviewing ", rds_files[ii])
      ctd_dat <- readRDS(file = rds_files[ii])
      
      # Make plot(s) of upcast and downcast
      if(all(c("downcast", "upcast") %in% names(ctd_dat))) {
        
        if(!any(file.exists(check_uc[ii], check_uc2[ii])) & !any(file.exists(check_dc[ii], check_dc2[ii]))) {
          par(mfrow = c(2,2))
        } else {
          par(mfrow = c(1,2))
        }
        
        if(!any(file.exists(check_dc[ii], check_dc2[ii]))) {
          oce::plot(ctd_dat$downcast, which = 1, type = 'l', main = "downcast")
          oce::plot(ctd_dat$downcast, which = 2, type = 'l', main = "downcast")
          abline(v = threshold, lwd = 3, col = "brown")
          abline(h = 1, lwd = 3, col = "black", lty = 2)
        }
        
        if(!any(file.exists(check_uc[ii], check_uc2[ii]))) {
          oce::plot(ctd_dat$upcast, which = 1, type = 'l', main = "upcast")
          oce::plot(ctd_dat$upcast, which = 2, type = 'l', main = "upcast")
          abline(v = threshold, lwd = 3, col = "brown")
          abline(h = 1, lwd = 3, col = "black", lty = 2)
        }
      }
      
      # Only upcast or downcast is in the rds file
      if("downcast" %in% names(ctd_dat) & !("upcast" %in% names(ctd_dat))) {
        par(mfrow = c(1,2))
        oce::plot(ctd_dat$downcast, which = 1, type = 'l', main = "downcast")
        oce::plot(ctd_dat$downcast, which = 2, type = 'l', main = "downcast")
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
      }
      
      if("upcast" %in% names(ctd_dat)  & !("downcast" %in% names(ctd_dat))) {
        par(mfrow = c(1,2))
        oce::plot(ctd_dat$upcast, which = 1, type = 'l', main = "upcast")
        oce::plot(ctd_dat$upcast, which = 2, type = 'l', main = "upcast")
        abline(v = threshold, lwd = 3, col = "brown")
        abline(h = 1, lwd = 3, col = "black", lty = 2)
      }
      
      # prompt for review
      response <- tolower(accept_response(valid_responses = c("y", "n"),
                                          prompt = "Apply remedial cell thermal mass correction to profile(s)? Yes (y) or No (n)?: ")) == "y"
      
      if(response) {
        review_vec <- c(review_vec, rds_files[ii])
      }
    }
  }
  
  if(length(review_vec) >= 1) {
    # Start remedial cell thermal mass corrections
    for(jj in 1:length(review_vec)) {
      message("remedial_ctm: Applying alternative cell thermal mass corrections to ", review_vec[jj])
      gapctd::split_ctm(review_vec[jj], haul_df = haul_df)
    }
  } else {
    message("remedial_ctm: No profiles to correct.")
  }

}
#' Remedial method to fix CTM errors using the best upcast or downcast parameter
#' 
#' Reads-in an rds file containing data that has been processed using the full gapctd workflow and uses the file name and estimated CTM parameters to reprocess data using the best upcast and downcast parameters.
#' 
#' @param rds_path Path to rds file
#' @param haul_df data.frame containing haul data from RACEBASE that includes metadata for the cnv file.
#' @return An upcast and downcast rds file if both casts were found in the rds file. Moves the original rds file to bad_cnv.

split_ctm <- function(rds_path, haul_df) {
  
  rds_dat <- readRDS(file = rds_path)
  
  rds_downcast <- gsub(pattern = "_raw.rds", replacement = "_dc_raw.rds", x = rds_path)
  rds_upcast <- gsub(pattern = "_raw.rds", replacement = "_uc_raw.rds", x = rds_path)
  
  if(length(rds_dat$downcast@metadata$ctm) > 1) {
    ctd_dat <- oce::read.oce(file = rds_dat$downcast@metadata$filename)
    
    if("downcast" %in% names(rds_dat)) {
      if("downcast" %in% names(rds_dat$downcast@metadata$ctm)) {
        ctm_pars <- rds_dat$downcast@metadata$ctm$downcast
      } else {
        # Only upcast CTM parameters
        ctm_pars <- rds_dat$downcast@metadata$ctm$upcast
      }
      
      processed_oce <- gapctd::run_gapctd(x = ctd_dat, 
                                          haul_df = haul_df, 
                                          return_stages = "final",
                                          ctd_tz = "America/Anchorage",
                                          ctm_pars = ctm_pars)
      processed_oce <- processed_oce[which(names(processed_oce) %in% c("downcast", "bottom"))]
      saveRDS(object = processed_oce, file = rds_downcast)
    }
    
    if("upcast" %in% names(rds_dat)) {
      if("upcast" %in% names(rds_dat$upcast@metadata$ctm)) {
        ctm_pars <- rds_dat$upcast@metadata$ctm$upcast
      } else {
        # Only downcast CTM parameters
        ctm_pars <- rds_dat$upcast@metadata$ctm$downcast
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
  
  message("Moving ", rds_path, "to /bad_cnv/")
  file.rename(rds_path,   gsub(pattern = "output/gapctd",
                               replacement = "bad_cnv",
                               x = rds_path))
}
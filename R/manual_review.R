#' Visually inspect profiles (SBEDP workflow)
#' 
#' Visually inspect profile data to evaluate whether they are acceptable. Profiles that are acceptable will be copied to a review directory. Unacceptable profiles will be removed from the directory and may need to be re-evaluated using manual_flag_review() or other remedial measures. This function skips profiles that have already been reviewed and accepted.
#' 
#' @param csv_paths Path to csv files. If NULL, the function uses all csv files in output/manual_flag/
#' @param threshold Numerical. Threshold for flagging a density inversion. Must be negative. Default (-1e-5) is the buoyancy frequency threshold PMEL uses. GTSPP uses a density threshold of 0.05.
#' @export

manual_review <- function(csv_paths = NULL, threshold = -1e-5) {
  
  if(is.null(csv_paths)) {
    csv_paths <- list.files(here::here("output", "manual_flag"), full.names = TRUE)
  }
  
  for(ii in 1:length(csv_paths)) {
    
    sel_profile <- read.csv(file = csv_paths[ii])
    
    if(min(sel_profile$depth) > 1) {
      message(paste0("Skipping ", sel_profile$deploy_id[ii]))
      next
    }
    
    if(!file.exists(here::here("output", "accepted_profiles", paste0(sel_profile$deploy_id[1], ".csv")))) {
      
      profile_oce <- oce::as.ctd(salinity = sel_profile$salinity,
                            temperature = sel_profile$temperature,
                            pressure = sel_profile$pressure,
                            conductivity = sel_profile$conductivity)
      message(sel_profile$deploy_id[1])
      par(mfrow = c(1,2))
      oce::plot(profile_oce, which = 1, type = 'l')
      oce::plot(profile_oce, which = 2, type = 'l')
      abline(v = threshold, lwd = 3, col = "brown")
      abline(h = 1, lwd = 3, col = "black", lty = 2)
      
      accept <- tolower(readline(prompt = "Accept profile? (y or n): ")) == "y"
      
      if(accept) {
        file.copy(from = csv_paths[ii], 
                  to = here::here("output", "accepted_profiles", paste0(sel_profile$deploy_id[1], ".csv")))
      } else {
        message(paste0("Removing ", csv_paths[ii], ". Rerun manual_flag_review() if manual adjustment should be made to this profile. Otherwise the profile will be exluded from the output."))
        file.remove(csv_paths[ii])
      }
      
    } else {
      message(paste0("Skipping ", sel_profile$deploy_id[ii]))
    }
    
  }
  
}
#' Calculate variable averages from accepted profiles, by depth/pressure bin (SBEDP workflow)
#' 
#' Calculate temperature from the 1 db pressure bin.
#' 
#' @param accepted_dir File path to folder with data from accepted profiles.
#' @param z_var Name of the z variable ("pressure" or "depth").
#' @param depth_range Range of depth/pressure bins to use, as a 1L or 2L numeric vector.
#' @param agg_fun Function to use.
#' @export

calc_bin_summary <- function(accepted_dir = here::here("output", "accepted_profiles"),
                           z_var = "pressure",
                           z_range = 1,
                           agg_fun = mean) {
  
  f_list <- list.files(accepted_dir, full.names = TRUE, pattern = "accepted.csv")
  
  if(!(length(f_list) > 1)) {
    stop(paste("calc_surface_mean(): No files found in accepted_dir!"))
  }
  
  agg_df <- data.frame()
  
  for(jj in 1:length(accepted_dir)) {
    # Load file
    sel_dat <- read.csv(file = f_list[jj], stringsAsFactors = FALSE)
    names(sel_dat)[names(sel_dat) == z_var] <- "z_col"
    
    sel_dat <- dplyr::filter(sel_dat, z_col <= max(z_range), z_col >= min(z_range))
    
    agg_df <- dplyr::bind_rows(agg_df, sel_dat)
    
  }
  
  var_names <- names(agg_df)[!(names(agg_df) %in% c("z_col", "file"))]
  
  out_df <- agg_df %>%
    dplyr::group_by(file) %>%
    dplyr::summarise(dplyr::across(var_names, agg_fun))
  
  out_df$z_range <- paste(1, collapse = ",")
  
  return(out_df)
  
}
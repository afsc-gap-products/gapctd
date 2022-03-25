#' Move best method files to final_cnv
#' 
#' Move best method files based on spikiness of profiles.
#' 
#' @param best_df Output of gapctd::compare_methods()
#' @export

move_to_final_cnv <- function(best_df) {
  best_df$new_loc <- new_loc = NA
  
  for(ii in 1:nrow(best_df)) {
    best_df$new_loc <- here::here("final_cnv",  paste0(best_df$deploy[ii], gsub(paste0(".*", best_df$deploy[ii]), "", best_df$move[ii])))
  }
  
  file.copy(from = best_df$move, to = best_df$new_loc)
}
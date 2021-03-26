#' Make profile plots from .cnv files
#' 
#' Plots for visual data QA/QC from .cnv files. Reads data from final_cnv file.
#' 
#' @param plot_bat Name of the batch file to use. Uses the default file if not provided.
#' @export

make_cast_plots <- function(plot_bat = NA) {
  if(is.na(plot_bat)) {
    plot_bat <- list.files(pattern = "plot.bat")
  }
  
  print("Making cast plots")
  system(command = paste0("sbebatch ", getwd(), "/", plot_bat, " ", getwd()))  
}
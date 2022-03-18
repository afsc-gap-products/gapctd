#' Review manual flags
#' 
#' Review plots from gapctd::manual_flag_interpolate.
#' 
#' @param var Variable to search for point removal.
#' @param flag_dir Manual flag directory file path. Default NULL uses the directory [working_directory]/output/manual_flag/
#' @export

manual_flag_review <- function(var = c("salinity", "temperature"),
                               flag_dir = NULL) {
  
  if(is.null(flag_dir)) {
    flag_dir <- here::here("output", "manual_flag")
  }
  
  
  flag_files <- list.files(flag_dir, full.names = TRUE)
  
  for(ii in 1:length(flag_files)) {
    dat <- read.csv(file = flag_files[ii])
    
    dat <- dat |>
      tidyr::pivot_longer(cols = c(paste0(var, "_upcast"), paste0(var, "_downcast"))) |>
      dplyr::mutate(variable = stringr::str_split(name, "_", simplify = TRUE)[,1],
                    direction = stringr::str_split(name, "_", simplify = TRUE)[,2]) |>
      dplyr::arrange(pressure)
    
    plot_title <- dat$file[1]
    
    print(
      ggplot2::ggplot(data = dat,
                      ggplot2::aes(x = round(value, 2), 
                                   y = -1*pressure, 
                                   color = direction, 
                                   linetype = direction)) +
        ggplot2::geom_path(size = ggplot2::rel(1.2)) +
        ggplot2::facet_grid(~variable, scales = "free_x") +
        ggplot2::scale_y_continuous(name = "Pressure") +
        ggplot2::scale_x_continuous(name = "Value") +
        ggplot2::scale_linetype(name = "Direction") +
        ggplot2::scale_color_manual(name = "Direction", values = c("#4E79A7", "#F28E2B")) +
        ggplot2::ggtitle(paste0("Deployment: ",  plot_title)) +
        ggplot2::theme_bw())
    
    keep <- c(TRUE, FALSE)[match(tolower(readline(paste0("Accept profile (", ii, " out of ", length(flag_files), ") -- y or n?:"))), c("y", "n"))]
    
    if(!keep) {
      # Delete file
      file.remove(flag_files[ii])
      print(paste0("Removing ", flag_files[ii]))
    } else {
      # Move to output/accepted_profiles
      new_name <- gsub(pattern = "manual_flag", "accepted_profiles", x = flag_files[ii])
      new_name <- gsub(pattern = "flag_interp", "accepted", x = new_name)
      file.rename(from = flag_files[ii], new_name)
    }
  }
  
}
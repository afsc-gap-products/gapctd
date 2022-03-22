#' Multipanel plots to compare methods
#' 
#' Make multipanel plots to compare methods/workflows for processing CTD data.
#' 
#' @param prefix Character vector to include before output plots.
#' @param processing_method Character vector denoting the name of the processing method, corresponding with names of folders in /output/ that contain processed cnv files.
#' @param method_labels Character vector denoting titles to use on multipanel plots. Must be the same length as processing_method. Default (NULL) uses processing_method character vector for plot titles.
#' @param return_output Logical. If true, returns output in a data.frame.
#' @export


make_method_multipanel_plot <- function(prefix,
                                        processing_method,
                                        method_labels = NULL,
                                        return_output = FALSE) {
  
  
  make_ud_plot <- function(dat_d, dat_u) {
    
    down_df <- data.frame(temperature = dat_d@data$temperature[!is.na(dat_d@data$flag)],
                          salinity = dat_d@data$salinity[!is.na(dat_d@data$flag)],
                          pressure = dat_d@data$pressure[!is.na(dat_d@data$flag)],
                          direction = "downcast") 
    up_df <- data.frame(temperature = dat_u@data$temperature[!is.na(dat_u@data$flag)],
                        salinity = dat_u@data$salinity[!is.na(dat_u@data$flag)],
                        pressure = dat_u@data$pressure[!is.na(dat_u@data$flag)],
                        direction = "upcast")
    
    comb_df <- dplyr::bind_rows(down_df, up_df, by = "method")
    
    temp_plot <- ggplot() +
      geom_path(data = comb_df, 
                aes(x = temperature, y = pressure, color = dir)) +
      scale_y_reverse()
    
    salin_plot <- ggplot() +
      geom_path(data = comb_df, aes(x = salinity, y = pressure, color = dir)) +
      scale_y_reverse()
    
    return(list(temp_plot = temp_plot,
                salin_plot = salin_plot))
  }
  
  
  if(!is.null(method_labels)) {
    plot_labels_df <- data.frame(method = processing_method,
                                 label = method_labels)
  } else {
    plot_labels_df <- data.frame(method = processing_method,
                                 label = processing_method)
  }
  
  if(!all(dir.exists(paste0(here::here("output"), "/", processing_method)))) {
    stop(paste0("\nDirectory not found for processing_method: ",  paste0(here::here("output"), "/", processing_method[!dir.exists(paste0(here::here("output"), "/", processing_method))], "\n")))
  }
  
  processing_dirs <- paste0(here::here("output"), "/", processing_method)
  id_vec <- character(length = 0L)
  
  for(ii in 1:length(processing_method)) {
    assign(paste0("u_listcnv_",processing_method[ii]), list.files(path = here::here("output", processing_method[ii]), pattern = "upcast", full.names = TRUE))
    assign(paste0("d_listcnv_",processing_method[ii]), list.files(path = here::here("output", processing_method[ii]), pattern = "downcast", full.names = TRUE))
    id_vec <- c(id_vec, gsub("[^\\d]+", "", gsub(paste0(".*output/", processing_method[ii]), "", eval(parse(text = paste0("d_listcnv_", processing_method[ii])))), perl=TRUE))
    id_vec <- c(id_vec, gsub("[^\\d]+", "", gsub(paste0(".*output/", processing_method[ii]), "", eval(parse(text = paste0("u_listcnv_", processing_method[ii])))), perl=TRUE))
    
  }
  
  n_files <- as.data.frame(table(id_vec))
  n_files <- dplyr::filter(n_files, Freq == max(n_files$Freq))
  
  # Only files where data are available for all methods
  for(jj in 1:length(processing_method)) {
    assign(paste0("u_listcnv_", processing_method[jj]),
           eval(parse(
             text = paste0("u_listcnv_", processing_method[jj])
           ))[which(gsub("[^\\d]+", "",
                         gsub(
                           paste0(".*output/", processing_method[jj]), "",
                           eval(parse(
                             text = paste0("u_listcnv_", processing_method[jj])
                           ))
                         ), perl = TRUE) %in% n_files$id_vec)])
    
    assign(paste0("d_listcnv_", processing_method[jj]), eval(parse(
      text = paste0("d_listcnv_", processing_method[jj])
    ))[which(gsub("[^\\d]+", "", gsub(
      paste0(".*output/", processing_method[jj]), "", eval(parse(
        text = paste0("d_listcnv_", processing_method[jj])
      ))
    ), perl = TRUE) %in% n_files$id_vec)])
  }
  
  out_df <- data.frame()
  
  for(kk in 1:nrow(n_files)) {
    
    comb_df <- data.frame()
    
    for(mm in 1:length(processing_method)) {
      
      dat_d <- suppressWarnings(oce::read.oce(eval(parse(text = paste0("d_listcnv_", processing_method[mm])))[kk]))
      dat_u <- suppressWarnings(oce::read.oce(eval(parse(text = paste0("u_listcnv_", processing_method[mm])))[kk]))
      
      down_df <- data.frame(temperature = dat_d@data$temperature[!is.na(dat_d@data$flag)],
                            salinity = dat_d@data$salinity[!is.na(dat_d@data$flag)],
                            pressure = dat_d@data$pressure[!is.na(dat_d@data$flag)],
                            delta_s = mean(abs(diff(dat_d@data$salinity))),
                            direction = "down") 
      up_df <- data.frame(temperature = dat_u@data$temperature[!is.na(dat_u@data$flag)],
                          salinity = dat_u@data$salinity[!is.na(dat_u@data$flag)],
                          pressure = dat_u@data$pressure[!is.na(dat_u@data$flag)],
                          delta_s = mean(abs(diff(dat_d@data$salinity))),
                          direction = "up")
      
      comb_df <- dplyr::bind_rows(down_df, up_df) |>
        dplyr::mutate(method = processing_method[mm]) |>
        dplyr::bind_rows(comb_df) |>
        dplyr::filter(pressure > 1)
      
    }
    
    comb_df <- comb_df |>
      dplyr::inner_join(plot_labels_df, by = "method")
    
    png(file = here::here("plots", paste0(prefix, "_salinity_", kk, ".png")), width = 8, height = 8, units = "in", res = 300)
    print(ggplot() +
            geom_path(data = comb_df, 
                      aes(x = salinity, 
                          y = pressure, 
                          color = direction)) +
            facet_wrap(~label) +
            scale_y_reverse() +
            scale_color_manual(values = c("red", "black")) +
            theme_bw())
    dev.off()
    
    png(file = here::here("plots", paste0(prefix, "_temperature_", kk, ".png")), width = 8, height = 8, units = "in", res = 300)
    print(ggplot() +
            geom_path(data = comb_df, 
                      aes(x = temperature, 
                          y = pressure, 
                          color = direction)) +
            facet_wrap(~label) +
            scale_y_reverse() +
            scale_color_manual(values = c("red", "black")) +
            theme_bw())
    dev.off()
    
    if(return_output) {
      comb_df$index <- kk
      comb_df$file <-  sub(paste0(".*/", processing_method[mm]), "", sub("_raw.*", "", eval(parse(text = paste0("d_listcnv_", processing_method[mm])))[kk]))
      out_df <- dplyr::bind_rows(out_df, comb_df)
    }
    
  }
  
  if(return_output) {
    return(out_df)
  }
}
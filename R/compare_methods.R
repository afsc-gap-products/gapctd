#' Compare methods based on spikes and make multipanel plots
#' 
#' Compare processing methods based on spikiness of profiles and make multipanel plots to compare methods/workflows for processing CTD data.
#' 
#' @param prefix Character vector to include before output plots.
#' @param processing_method Character vector denoting the name of the processing method, corresponding with names of folders in /output/ that contain processed cnv files.
#' @param method_labels Character vector denoting titles to use on multipanel plots. Must be the same length as processing_method. Default (NULL) uses processing_method character vector for plot titles.
#' @param return_output Logical. If true, returns output in a data.frame.
#' @param pattern_upcast Character vector pattern for upcast file.
#' @param pattern_downcast Character vector pattern for downcast file.
#' @param min_pressure_bin Numeric vector (1L) indicating the minimum depth bin to use for comparing profiles.
#' @param area_method Should area calculations use temperature-salinity ("ts") or pressure-salinity ("ps") profiles?
#' @param scale_vars Logical. Should temperature and salinity be scaled to calculate T-S area?
#' @export

compare_methods <- function(prefix,
                            processing_method,
                            method_labels = NULL,
                            return_output = FALSE,
                            pattern_downcast = "downcast.cnv",
                            pattern_upcast = "upcast.cnv",
                            min_pressure_bin = 4,
                            area_method = "ts",
                            scale_vars = FALSE) {
  
  if(!dir.exists(here::here("plots", "binavg"))) {
    dir.create(here::here("plots", "binavg"))
  }
  
  if(!dir.exists(here::here("plots", "raw"))) {
    dir.create(here::here("plots", "raw"))
  }
  
  
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
    assign(paste0("u_listcnv_",processing_method[ii]), list.files(path = here::here("output", processing_method[ii]), pattern =  pattern_upcast, full.names = TRUE))
    assign(paste0("d_listcnv_",processing_method[ii]), list.files(path = here::here("output", processing_method[ii]), pattern = pattern_downcast, full.names = TRUE))
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
  best_df <- data.frame()
  
  for(kk in 1:nrow(n_files)) {
    
    comb_df <- data.frame()
    
    for(mm in 1:length(processing_method)) {
      dat_d <- try(oce::read.oce(eval(parse(text = paste0("d_listcnv_", processing_method[mm])))[kk]), silent = TRUE)
      dat_u <- try(oce::read.oce(eval(parse(text = paste0("u_listcnv_", processing_method[mm])))[kk]), silent = TRUE)
      
      if(class(dat_d) == "try-error" & class(dat_u) == "try-error") { next }
      
      p_max <- 0
      if(class(dat_d) == "try-error") {
        down_df <- data.frame(temperature = numeric(),
                              salinity = numeric(),
                              pressure = numeric(),
                              delta_s = numeric(),
                              direction = character())
        p_max <- 1
      } else {
        down_df <- data.frame(temperature = dat_d@data$temperature[!is.na(dat_d@data$flag)],
                              salinity = dat_d@data$salinity[!is.na(dat_d@data$flag)],
                              pressure = dat_d@data$pressure[!is.na(dat_d@data$flag)],
                              delta_s = mean(abs(diff(dat_d@data$salinity[dat_d@data$pressure > min_pressure_bin])), na.rm = TRUE),
                              direction = "down") 
        if(nrow(down_df) < 8) {
          # Handle insufficient data
          p_max <- 1
        }
      }
      
      
      
      if(class(dat_u) == "try-error") {
        up_df <- data.frame(temperature = numeric(),
                            salinity = numeric(),
                            pressure = numeric(),
                            delta_s = numeric(),
                            direction = character())
        p_max <- 2
      } else {
        up_df <- data.frame(temperature = dat_u@data$temperature[!is.na(dat_u@data$flag)],
                            salinity = dat_u@data$salinity[!is.na(dat_u@data$flag)],
                            pressure = dat_u@data$pressure[!is.na(dat_u@data$flag)],
                            delta_s = mean(abs(diff(dat_u@data$salinity[dat_d@data$pressure > min_pressure_bin])), na.rm = TRUE),
                            direction = "up")
        if(nrow(up_df) < 8) {
          # Handle insufficient data
          p_max <- 2
        }
      }
      
      if(p_max == 0) {
        max_pressure <- min(c(max(down_df$pressure, na.rm = TRUE),  max(up_df$pressure, na.rm = TRUE)))
        
        
        poly_df <- dplyr::inner_join(dplyr::select(up_df, temperature, salinity, pressure) |>
                                       dplyr::rename(temperature_up = temperature,
                                              salinity_up = salinity),
                                     dplyr::select(down_df, temperature, salinity, pressure) |>
                                       dplyr::rename(temperature_down = temperature,
                                                     salinity_down = salinity),
                                     by = "pressure")
        
        if(scale_vars) {
          poly_df$salinity_up <- scale(c(poly_df$salinity_up, poly_df$salinity_down))[,1][nrow(poly_df)]
          poly_df$salinity_down <- scale(c(poly_df$salinity_down, poly_df$poly_df$salinity_up))[,1][nrow(poly_df)]
          poly_df$temperature_up <- scale(c(poly_df$temperature_up, poly_df$temperature_down))[,1][nrow(poly_df)]
          poly_df$temperature_down <- scale(c(poly_df$temperature_down, poly_df$temperature_up))[,1][nrow(poly_df)]
        }
        
        if(area_method == "ts") {
          wkt_poly <- data.frame(geometry = paste0("LINESTRING (", apply(X = 
                                                                           cbind(
                                                                             apply(
                                                                               X = cbind(
                                                                                 poly_df$salinity_down[1:(nrow(poly_df))],
                                                                                 poly_df$temperature_down[1:(nrow(poly_df))]),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               cbind(c(poly_df$salinity_up[1:(nrow(poly_df)-1)], poly_df$salinity_up[(nrow(poly_df)-1)]),
                                                                                     c(poly_df$temperature_up[1:(nrow(poly_df)-1)], poly_df$temperature_up[(nrow(poly_df)-1)])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(c(poly_df$salinity_down[2:(nrow(poly_df))],poly_df$salinity_up[(nrow(poly_df))]),
                                                                                         c(poly_df$temperature_down[2:(nrow(poly_df))],poly_df$temperature_up[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(
                                                                                 c(poly_df$salinity_down[1:(nrow(poly_df)-1)],poly_df$salinity_down[(nrow(poly_df))]),
                                                                                 c(poly_df$temperature_down[1:(nrow(poly_df)-1)],poly_df$temperature_down[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " ")),
                                                                         MARGIN = 1,
                                                                         FUN = paste,
                                                                         collapse = ", "), ")")) 
        } else if(area_method == "ps") {
          wkt_poly <- data.frame(geometry = paste0("LINESTRING (", apply(X = 
                                                                           cbind(
                                                                             apply(
                                                                               X = cbind(
                                                                                 poly_df$salinity_down[1:(nrow(poly_df))],
                                                                                 poly_df$pressure[1:(nrow(poly_df))]),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               cbind(c(poly_df$salinity_up[1:(nrow(poly_df)-1)], poly_df$salinity_up[(nrow(poly_df)-1)]),
                                                                                     c(poly_df$pressure[1:(nrow(poly_df)-1)], poly_df$pressure[(nrow(poly_df)-1)])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(c(poly_df$salinity_down[2:(nrow(poly_df))],poly_df$salinity_up[(nrow(poly_df))]),
                                                                                         c(poly_df$pressure[2:(nrow(poly_df))],poly_df$pressure[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " "),
                                                                             apply(
                                                                               X = cbind(
                                                                                 c(poly_df$salinity_down[1:(nrow(poly_df)-1)],poly_df$salinity_down[(nrow(poly_df))]),
                                                                                 c(poly_df$pressure[1:(nrow(poly_df)-1)],poly_df$pressure[(nrow(poly_df))])),
                                                                               MARGIN = 1,
                                                                               FUN = paste, 
                                                                               collapse = " ")),
                                                                         MARGIN = 1,
                                                                         FUN = paste,
                                                                         collapse = ", "), ")")) 
        }
        
        # Calculate area
        wkt_poly <- wkt_poly |>
          dplyr::mutate(ID = row_number()) |>
          st_as_sf(wkt = "geometry") |> 
          dplyr::group_by(ID) |>
          summarise(do_union = FALSE) |>
          sf::st_cast(to = "POLYGON")
        
        ts_area <- sum(sf::st_area(wkt_poly), na.rm = TRUE)
        
        down_df$area_ts <- ts_area
        up_df$area_ts <- ts_area
        
      } else if(p_max == 1) {
        max_pressure <- min(max(up_df$pressure, na.rm = TRUE))
      } else {
        max_pressure <- min(max(down_df$pressure, na.rm = TRUE))
      }
      
      comb_df <- dplyr::bind_rows(down_df, up_df) |>
        dplyr::mutate(method = processing_method[mm],
                      method_index = mm) |>
        dplyr::bind_rows(comb_df) |>
        dplyr::filter(pressure > 1)
      
    }
    
    comb_df <- comb_df |>
      dplyr::inner_join(plot_labels_df, by = "method")
    
    if(return_output) {
      comb_df$index <- kk
      comb_df$deploy <-  sub(paste0(".*/", processing_method[mm], "/"), "", sub("_raw.*", "", eval(parse(text = paste0("d_listcnv_", processing_method[mm])))[kk]))
      out_df <- dplyr::bind_rows(out_df, comb_df)
      
      # List best files to move
      if("area_ts" %in% names(comb_df)) {
        summary_df <- comb_df |>
          dplyr::select(direction, method, label, deploy, method_index, index, delta_s, area_ts) |>
          unique()
        
        summary_df <- summary_df |>
          dplyr::group_by(direction) |>
          dplyr::summarise(area_ts = min(area_ts, na.rm = TRUE)) |>
          dplyr::inner_join(summary_df, by = c("direction", "area_ts")) |>
          dplyr::mutate(move = NA) |>
          dplyr::select(-area_ts, -delta_s)
        
      } else {
        summary_df <- comb_df |>
          dplyr::select(direction, method, label, deploy, method_index, index, delta_s) |>
          unique()
        
        summary_df <- summary_df |>
          dplyr::group_by(direction) |>
          dplyr::summarise(delta_s = min(delta_s, na.rm = TRUE)) |>
          dplyr::inner_join(summary_df) |>
          dplyr::mutate(move = NA) |>
          dplyr::select(-delta_s)
      }
      
      summary_df$move[summary_df$direction == "down"] <- try(eval(parse(text = paste0("d_listcnv_", processing_method[summary_df$method_index[summary_df$direction == "down"]])))[summary_df$index[summary_df$direction == "down"]], silent = TRUE)
      summary_df$move[summary_df$direction == "up"] <- try(eval(parse(text = paste0("u_listcnv_", processing_method[summary_df$method_index[summary_df$direction == "up"]])))[summary_df$index[summary_df$direction == "up"]], silent = TRUE)
      
      best_df <- dplyr::bind_rows(summary_df, best_df)
      
    }
    
    best_profiles_df <- comb_df |> 
      dplyr::inner_join(summary_df, by = c("direction", "method", "method_index", "label", "index", "deploy"))
    
    png(file = here::here("plots", paste0(prefix, "_salinity_", kk, ".png")), width = 8, height = 8, units = "in", res = 300)
    print(ggplot() +
            geom_path(data = comb_df, 
                      aes(x = salinity, 
                          y = pressure, 
                          color = direction)) +
            geom_path(data = best_profiles_df, 
                      aes(x = salinity, 
                          y = pressure, 
                          color = direction),
                      size = rel(1.4)) +
            ggtitle(comb_df$deploy[1]) +
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
            geom_path(data = best_profiles_df, 
                      aes(x = temperature, 
                          y = pressure, 
                          color = direction),
                      size = rel(1.4)) +
            ggtitle(comb_df$deploy[1]) +
            facet_wrap(~label) +
            scale_y_reverse() +
            scale_color_manual(values = c("red", "black")) +
            theme_bw())
    dev.off()
    
  }
  
  if(return_output) {
    return(list(compare_df = out_df,
                best_df = best_df))
  }
}
#' Select best processing method (R workflow)
#' 
#' Simultaneously review multiple temperature, salinity, and density profiles from downcasts and upcasts that were contained in .rds files that were processed using different methods (e.g., Typical, Typical CTM, TSA, SPD) then select the best profile. Renames the rds file with the best profile by replacing the method with the suffix ("_best.rds"). Run after wrapper_run_gapctd().
#' 
#' @param rds_dir_path Filepath to directory containing .rds files.
#' @export

select_best_method <- function(rds_dir_path) {
  
  rds_path <- list.files(rds_dir_path, full.names = TRUE)
  
  rds_filename <- list.files(rds_dir_path, 
                             pattern = "_typical.rds")
  
  deployment_id <- unique(
    gsub(x = rds_filename,
         pattern = "_typical.rds",
         replacement = ""))
  
  for(II in 1:length(deployment_id)) {
    
    deployment_files <- rds_path[grep(x = rds_path, pattern = deployment_id[II])]
    
    if(any(grepl(pattern = "_best.rds", x = deployment_files))) {
      message("select_best_method: Skpping deployment ", 
              deployment_id[II], 
              " because best method file (ending in _best.rds) already exists.")
      next
    }
    
    message("select_best_method: Retrieving data from deployment ", deployment_id[II], ".")
    
    deployment_df <- data.frame()
    
    for(JJ in 1:length(deployment_files)) {
      sel_dat <- readRDS(file = deployment_files[JJ])
      
      if("downcast" %in% names(sel_dat)) {
        deployment_df <- deployment_df |>
          dplyr::bind_rows(
            as.data.frame(sel_dat$downcast@data) |>
              dplyr::mutate(cast_direction = sel_dat$downcast@metadata$cast_direction,
                            gapctd_method = sel_dat$downcast@metadata$gapctd_method)
          )
      }
      
      if("upcast" %in% names(sel_dat)) {
        deployment_df <- deployment_df |>
          dplyr::bind_rows(
            as.data.frame(sel_dat$upcast@data) |>
              dplyr::mutate(cast_direction = sel_dat$upcast@metadata$cast_direction,
                            gapctd_method = sel_dat$upcast@metadata$gapctd_method)
          )
      }
    }
    
    # Make an interactive plot of variables to review
    print(
      plotly::ggplotly(
        ggplot() +
          geom_path(data =
                      tidyr::pivot_longer(data = deployment_df,
                                          cols = c("salinity", "temperature", "density")),
                    aes(x = value,
                        y = depth,
                        linetype = cast_direction,
                        color = factor(gapctd_method, 
                                       levels = c("Typical", "Typical CTM", "TSA", "SPD"),
                                       labels = c("(1) Typical", "(2) Typical CTM", "(3) T-S Area", "(4) S Path Dist")))) +
          scale_color_manual(name = "",
                             values = c("(1) Typical" = "#E69F00", 
                                        "(2) Typical CTM" = "#56B4E9", 
                                        "(3) T-S Area" = "#009E73", 
                                        "(4) S Path Dist" = "#0072B2")) +
          scale_linetype_manual(name = "",
                                values = c("downcast" = 1, "upcast" = 2)) +
          scale_y_reverse(name = "Depth (m)") +
          scale_x_continuous(name = "Value") +
          facet_grid(~factor(name, levels = c("temperature", "salinity", "density")), 
                     scales = "free_x") +
          theme_bw() +
          theme(legend.title = element_blank())
      )
    )
    
    # Prompt user to select the best method
    response <- gapctd:::accept_response(valid_responses = c(1,2,3,4),
                                         prompt = "Select the best processing method (1 = Typical, 2 = Typical CTM, 3 = T-S Area, 4 = S Path Dist): ")
    
    # Clear last plot
    dev.off()
    
    best_suffix <- c("_typical.rds", "_typical_ctm.rds", "_tsa.rds", "_spd.rds")[as.numeric(response)]
    
    #  Rename best file
    best_file <- deployment_files[grepl(pattern = best_suffix, x = deployment_files)]
    file.rename(from = best_file, to = gsub(pattern = best_suffix, replacement = "_best.rds", x = best_file))
    
  }
}



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
  
  for(ii in 1:length(rds_files)) {
      
    if(!file.exists(out_files[ii])) {
      message("review_profiles: Reviewing ", rds_files[ii])
      ctd_dat <- readRDS(file = rds_files[ii])
      
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
        
        response <- gapctd:::accept_response(valid_responses = c(0, 1, "d", "u"),
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
        
        response <- gapctd:::accept_response(valid_responses = c(0, "d"),
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
        
        response <- gapctd:::accept_response(valid_responses = c(0, "u"),
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
          saveRDS(object = ctd_dat[which(names(ctd_dat) %in% c("downcast", "bottom"))],
                  file = out_files[ii])
        }

        if(!keep_dc & keep_uc) {
          saveRDS(object = ctd_dat[which(names(ctd_dat) %in% c("upcast", "bottom"))],
                  file = out_files[ii])
        }
      }
      
    } else {
      
      message("review_profiles: Skipping ", 
              rds_files[ii], 
              " because a final profile already exists for the deployment (", out_files[ii],
              ").")
    }
  } 
}
#' Wrapper for align_oxygen
#' 
#' @param rds_dir_path Filepath to directory containing rds files for the best method. If "auto", uses files from /output/gapctd/*_best.rds
#' @param in_pattern Filename suffix for the best rds files. Default is _best.rds
#' @param haul_data_path Path to haul data rds file. If "auto" uses, the HAUL_DATA rds file in /output/
#' @param oxygen_alignment_offset_values Vector of oxygen alignment offset values to test.
#' @noRd
#' @author Sean Rohan

wrapper_align_oxygen <- function(rds_dir_path = "auto", in_pattern = "_best.rds", haul_data_path = "auto", oxygen_alignment_offset_values = -2:-7) {
  
  if(rds_dir_path == "auto") {
    
    rds_dir_path <- here::here("output", "gapctd")
    
  }
  
  rds_filename <- list.files(rds_dir_path, pattern = in_pattern, full.names = TRUE)
  
  if(haul_data_path == "auto") {
    
    haul_data_path <- list.files(path = here::here("output"),
                                 full.names = TRUE,
                                 pattern = "HAUL_DATA")
    
    stopifnot("wrapper_align_oxygen: More than one haul data file detected in /output/. Either provide a filepath to haul_data_path or move/delete the files that shouldn't be used." = length(haul_data_path) == 1)
    
  }
  
  haul_df <- readRDS(file = haul_data_path)
  
  for(ii in 1:length(rds_filename)) {
    
    gapctd:::align_oxygen(x = rds_filename[ii],
                          haul_df = haul_df,
                          alignment = oxygen_alignment_offset_values,
                          mode = "test")
    
  }
  
}



#' Align oxygen after selecting best method (gapctd)
#'
#' This function aligns oxygen channels after dynamic errors in temperature and salinity have been corrected and the best profile has been selected. Processing is performed by rerunning run_gapctd() using the parameters selected for the 'best' method.
#'
#' @param x A list or filepath to an .rds file that contains downcast and/or upcast oce objects that contain ctm and temperature alignment parameters.
#' @param alignment Numeric vector of alignment values to try.
#' @param mode 'test' or 'align'. 'test' processes data using each alignment value and saves alignment results to a data frame. 'align' processes using a single alignment value that is passed to the alignment argument.
#' @param channel Optional. An open RODBC channel.
#' @param haul_df Optional. Data frame containing haul data.
#' @param racebase_tzone Time zone for racebase data.
#' @param output_path Used when mode = 'align'. Optional output path for rds file. Overwrites input file if not provided.
#' @noRd
#' @author Sean Rohan

align_oxygen <- function(x,
                         alignment = c(-2:-7),
                         mode = "test",
                         channel = NULL,
                         vessel = NULL,
                         cruise = NULL,
                         haul_df = NULL,
                         racebase_tzone = "America/Anchorage",
                         output_path = NULL) {
  
  suppressWarnings(
    dir.create(path = here::here("output", "align_oxygen"))
  )
  
  stopifnot("align_oxygen: mode argument must be either 'test' or 'align'" = mode %in% c("test", "align"))
  
  if(mode == "align") {
    stopifnot("align_oxygen: alignment argument must be a 1L numeric vector when mode = 'align'" = length(alignment) == 1)
  }
  
  if(mode == "test" & !is.null(output_path)) {
    warning("align_oxygen: output_path argument not used when mode = 'test'")
  }
  
  if(mode == "align" & !is.null(output_path)) {
    stopifnot("align_oxygen: output_path must have file extension .rds when mode = 'align'" = tolower(tools::file_ext(output_path)) == "rds")
  }
  
  if(class(x) == "character") {
    stopifnot("align_oxygen: x argument is not a valid filepath." = file.exists(x))
    dat <- readRDS(file = x)
  }
  
  if(class(x) == "list" & mode == "test") {
    stopifnot("align_oxygen: x does not contain downcast or upcast oce objects." = any(names(x) %in% c("downcast", "upcast")))
  }
  
  if(class(x) == "list" & mode == "align") {
    stopifnot("align_oxygen: output_path argument must be provided when x is a list and mode = 'align'." = !is.null(output_path))
    
    if(!dir.exists(dirname(output_path))) {
      dir.create(dirname(output_path))
    }
  }
  
  deploy_id <- dat[[1]]@metadata$race_metadata$deploy_id
  
  if(mode == "test") {
    
    ox_path <- here::here("output", "align_oxygen", paste0(deploy_id, "_ox_align.rds"))
    
    if(file.exists(ox_path)) {
      
      message("align_oxygen: Skipping ", deploy_id, ". Output file already exists at ", ox_path)
      
      return()
      
    }
    
  }
  
  
  
  message("align_oxygen: Processing ", deploy_id, ". From file ", here::here("cnv", basename(dat[[1]]@metadata$filename)))
  
  if(is.null(haul_df)) {
    if(is.null(channel)) {
      channel <- gapctd::get_connected()
    }
    
    haul_df <- gapctd:::get_haul_data(channel = channel,
                                      vessel = vessel,
                                      cruise = cruise,
                                      tzone = racebase_tzone)
  }
  
  valid_casts <- c("downcast", "upcast")[which(c("downcast", "upcast") %in% names(dat))]
  
  out_df <- data.frame()
  
  for(kk in 1:length(alignment)) {
    
    new_obj <- list()
    
    for(ii in 1:length(valid_casts)) {
      
      cast <- valid_casts[ii]
      
      filename <-  here::here("cnv", basename(dat[[cast]]@metadata$filename))
      
      if(("both" %in% names(dat[[cast]]@metadata$ctm))) {
        
        ctm_pars <- dat[[cast]]@metadata$ctm$both
        
      } else {
        
        ctm_pars <- dat[[cast]]@metadata$ctm
        
      }
      
      oce_obj <- suppressWarnings(oce::read.oce(file = filename))
      
      new_obj[[cast]] <- suppressWarnings(gapctd:::run_gapctd(x = oce_obj,
                                                              haul_df = haul_df,
                                                              return_stage = "full",
                                                              ctd_tz = "America/Anchorage",
                                                              ctm_pars = ctm_pars,
                                                              align_pars = c(temperature = as.numeric(dat[[cast]]@metadata$align$temperature['offset']),
                                                                             oxygenRaw = alignment[kk]),
                                                              cal_rds_path = here::here("psa_xmlcon", "calibration_parameters.rds"),
                                                              cor_var = "conductivity")[[cast]])
      
      new_obj[[cast]]@metadata$align$rawOxygen['offset'] <- alignment[kk]
      
      out_df <- dplyr::bind_rows(out_df,
                                 as.data.frame(new_obj[[cast]]@data) |>
                                   dplyr::mutate(cast = cast,
                                                 deploy_id = deploy_id,
                                                 filename = filename,
                                                 rds_name = x,
                                                 oxygen_offset = alignment[kk]))
      
    }
    
  }
  
  if(mode == "align") {
    
    if("bottom" %in% names(dat)) {
      
      new_obj[["bottom"]] <- dat[["bottom"]]
      
    }
    
    if(is.null(output_path)) {
      
      out_file <- x
      
    }
    
    message("align_oxygen: Writing processed cast data to ", output_path)
    
    saveRDS(object = new_obj, file = output_path)
    
  }
  
  # 'test' mode saves a data.frame with oxygen aligned by a specified value
  if(mode == "test") {
    
    message("align_oxygen: Writing candidate oxygen profiles to ", ox_path)
    
    saveRDS(object = out_df, file = ox_path)
    
  }
  
}



#' Visually inspect and select the best oxygen profile alignment
#' 
#' @param ox_align_path Path to the directory containing oxygen alignment rds test files.
#' @param haul_data_path Path to haul data rds file. If "auto" uses, the HAUL_DATA rds file in /output/
#' @param out_pattern Filename suffix to append to the output file with corrected oxygen data added.
#' @param in_pattern Filename suffix for the input best rds files. Default is _best.rds
#' @noRd
#' @author Sean Rohan

select_best_oxygen_method <- function(ox_align_path = "auto", haul_data_path = "auto", out_pattern = "_best_oxygen.rds", in_pattern = "_best.rds") {
  
  # Get paths to oxygen alignment candidate offset data, best T-S casts, and haul data
  if(ox_align_path == "auto") {
    
    ox_align_path <- here::here("output", "align_oxygen")
    
  }
  
  ox_filename <- list.files(ox_align_path, full.names = TRUE)
  
  if(haul_data_path == "auto") {
    
    haul_data_path <- list.files(path = here::here("output"),
                                 full.names = TRUE,
                                 pattern = "HAUL_DATA")
    
    stopifnot("wrapper_align_oxygen: More than one haul data file detected in /output/. Either provide a filepath to haul_data_path or move/delete the files that shouldn't be used." = length(haul_data_path) == 1)
    
  }
  
  haul_df <- readRDS(file = haul_data_path)
  
  
  for(ii in 1:length(ox_filename)) {
    
    ox_dat <- readRDS(file = ox_filename[ii])
    
    output_path <- gsub(pattern = in_pattern,
                        replacement = out_pattern,
                        x = ox_dat$rds_name[1])
    
    if(file.exists(output_path)) {
      message("Skipping ", ox_filename[ii], ". Output file already exists at ", output_path)
      next
    }
    
    unique_offsets <- unique(ox_dat$oxygen_offset)
    
    if(all(c("upcast", "downcast") %in% ox_dat$cast)) {
      
      abs_sum_dist <- ox_dat |>
        tidyr::pivot_wider(id_cols = c("oxygen_offset", "depth"),
                           values_from = oxygen,
                           names_from = cast) |>
        dplyr::mutate(abs_delta = abs(upcast-downcast)) |>
        dplyr::filter(!is.na(abs_delta) & depth > 3) |>
        dplyr::group_by(oxygen_offset) |>
        dplyr::summarise(sum_abs_delta = signif(sum(abs_delta), 3))
      
      abs_sum_dist$lowest <- abs_sum_dist$sum_abs_delta == min(abs_sum_dist$sum_abs_delta)
      
      ox_dat <- dplyr::inner_join(ox_dat, 
                                  abs_sum_dist,
                                  by = "oxygen_offset")
      
      print(
        plotly::ggplotly(
          ggplot() +
            geom_path(data = ox_dat,
                      mapping = aes(x = oxygen,
                                    y = depth,
                                    linetype = cast,
                                    color = lowest)) +
            scale_y_reverse(name = "Depth") +
            scale_x_continuous(name = "Dissolved Oxygen (ml/l)") +
            scale_linetype(name = "Cast") +
            scale_color_manual(name = "Lowest?", values = c("black", "red")) +
            facet_wrap(~paste0(abs(oxygen_offset), " (", sum_abs_delta,")")) +
            ggtitle(ox_dat$deploy_id[1]) +
            theme_bw()
        )
      )
        
    } else {
      
      print(
        plotly::ggplotly(
          ggplot() +
            geom_path(data = ox_dat,
                      mapping = aes(x = oxygen,
                                    y = depth,
                                    linetype = cast)) +
            scale_y_reverse(name = "Depth") +
            scale_x_continuous(name = "Dissolved Oxygen (ml/l)") +
            scale_linetype(name = "Cast") +
            facet_wrap(~abs(oxygen_offset)) +
            ggtitle(ox_dat$deploy_id[1]) +
            theme_bw()
        )
      )
      
    }
    
    
    # Prompt user to select the best method
    response <- gapctd:::accept_response(valid_responses = abs(unique_offsets),
                                         prompt = paste0("Select the best offset (",
                                                         paste(abs(unique_offsets), collapse = ", "),"): ") )
    
    best_offset <- unique_offsets[match(x = response, table = abs(unique_offsets))]
    
    gapctd:::align_oxygen(x = ox_dat$rds_name[1],
                          haul_df = haul_df,
                          alignment = best_offset,
                          mode = "align",
                          output_path = output_path)
    
  }
  
}

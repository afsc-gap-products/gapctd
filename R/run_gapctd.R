#' Process a .cnv file using the R workflow (R workflow)
#' 
#' Run all gapctd modules in order to process data from a cnv file.
#' 
#' @param x oce file
#' @param haul_df data.frame containing haul data from RACEBASE that includes metadata for the cnv file.
#' @param return_stages Character vector denoting which stages of processing should be included in the output (options "typical", "split", "align", "tmcorrect", "final"). Can return multiple stages simultaneously. Default = "final"
#' @param ctd_tz timezone for CTD as a character vector or numeric that is valid for POSIXct.
#' @return A list of oce objects at stages of processing specified in return_stages.
#' @export

run_gapctd <- function(x, haul_df, return_stages = c("final"), ctd_tz = "America/Anchorage") {
  
  output_list <- list()
  
  # Force timezone to AKDT (America/Anchorage)
  x@metadata$startTime <- lubridate::force_tz(x@metadata$startTime, 
                                              tz = ctd_tz)
  
  
  # Lowpass and median filters -----------------------------------------------------------------------
  x <- x |> 
    gapctd:::append_haul_data(haul_df = haul_df) |>
    gapctd:::median_filter(variables = c("temperature", "conductivity"),
                           window = c(5,5)) |>
    gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                            time_constant = c(0.5, 0.5, 1),
                            precision = c(4, 6, 3),
                            freq_n = 0.25)
  
  # Return results based on typical settings
  if("typical" %in% return_stages) {
    
    output_list[["typical_downcast"]] <- x |>
      gapctd:::align_var(x = downcast, 
                         variables = "temperature", 
                         offset = -0.5, 
                         interp_method = "linear")
    gapctd:::assign_metadata_fields(cast_direction = "downcast") |>
      gapctd:::section_oce(by = "datetime",
                           cast_direction = "downcast") |>
      gapctd:::conductivity_correction(alpha_C = 0.04, beta_C = 1/8) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    
    output_list[["typical_upcast"]] <- x |>
      gapctd:::align_var(x = upcast, 
                         variables = "temperature", 
                         offset = -0.5, 
                         interp_method = "linear")
    gapctd:::assign_metadata_fields(cast_direction = "upcast") |>
      gapctd:::section_oce(by = "datetime",
                           cast_direction = "upcast") |>
      gapctd:::conductivity_correction(alpha_C = 0.04, beta_C = 1/8) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "upcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)  
    
  }
  
  # Split upcast, downcast, and bottom ---------------------------------------------------------------
  downcast <- x |>
    gapctd:::assign_metadata_fields(cast_direction = "downcast") |>
    gapctd:::section_oce(by = "datetime",
                         cast_direction = "downcast")
  
  upcast <- x |>
    gapctd:::assign_metadata_fields(cast_direction = "upcast") |>
    gapctd:::section_oce(by = "datetime",
                         cast_direction = "upcast")
  
  onbottom <- x|> 
    gapctd:::section_oce(by = "datetime",
                         cast_direction = "bottom")
  
  if(!is.null(onbottom)) {
    onbottom <- gapctd:::derive_eos(x = onbottom)
  }
    
  
  if("split" %in% return_stages) {
    output_list[["split_downcast"]] <- downcast |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    output_list[["split_upcast"]] <- split_upcast = upcast |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    output_list[["split_bottom"]] <- split_bottom = bottom |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
  }
  
  # Align upcast and downcast ------------------------------------------------------------------------
  if(!is.null(downcast)) {
    
  dc_align <- gapctd:::optim_align_par(x = downcast, 
                                       variable = "temperature", 
                                       offsets = seq(-1,1, 0.01), 
                                       cor_method = "pearson",
                                       cast_direction = "downcast")
  
  downcast <- gapctd:::align_var(x = downcast, 
                                 variables = "temperature", 
                                 offset = dc_align['best_offset'], 
                                 interp_method = "linear")
  
  downcast@metadata[['align']] <- dc_align
  
  } 
  
  if(!is.null(upcast)) {
    uc_align <- gapctd:::optim_align_par(x = upcast, 
                                         variable = "temperature", 
                                         offsets = seq(-1,1, 0.01), 
                                         cor_method = "pearson",
                                         cast_direction = "upcast")
    
    upcast@metadata[['align']] <- uc_align
    
    upcast <- gapctd:::align_var(x = upcast, 
                                 variables = "temperature", 
                                 offset = uc_align['best_offset'], 
                                 interp_method = "linear")
  }

  
  
  if("align" %in% return_stages) {
    output_list[["align_downcast"]] <- downcast |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    output_list[["align_upcast"]] <- split_upcast = upcast |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
  }
  
  
  # Estimate conductivity cell thermal mass correction parameters ------------------------------------
  ctm_pars <- gapctd:::optim_ctm_pars(dc = downcast, 
                                      uc = upcast,
                                      optim_method = "L-BFGS-B",
                                      optim_maxit = 500,
                                      start_alpha_C = 0.04,
                                      start_beta_C = 1/8)
  downcast_binned <-NULL
  upcast_binned <- NULL
  
  if(!is.null(downcast)) {
    
    downcast_binned <- downcast |>
      gapctd:::conductivity_correction(alpha_C = ctm_pars['alpha_C'], beta_C = ctm_pars['beta_C']) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
      gapctd:::derive_eos()|>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    
    downcast_binned@metadata[['ctm']] <- ctm_pars
    downcast_binned@metadata[['bin_average']] <- c(by = "depth", bin_width = 1)
  }
  
  if(!is.null(upcast)) {
    
  upcast_binned <- upcast |>
    gapctd:::conductivity_correction(alpha_C = ctm_pars['alpha_C'], beta_C = ctm_pars['beta_C']) |>
    gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "upcast") |>
    gapctd:::derive_eos() |>
    gapctd:::bin_average(by = "depth", bin_width = 1)
  
  upcast_binned@metadata[['ctm']] <- ctm_pars
  upcast_binned@metadata[['bin_average']] <- c(by = "depth", bin_width = 1)
  
  }
  
  if("tmcorrect" %in% return_stages) {
    output_list[["tmcorrect_downcast"]] <- downcast_binned
    output_list[["tmcorrect_upcast"]] <- upcast_binned
  }
  
  
  # Check/correct density inversion; check if data are complete  -----------------------------------
  
  if(!is.null(downcast_binned)) {
    downcast_binned <- downcast_binned |>
      gapctd:::check_density_inversion(threshold  = -1e-4, 
                                       threshold_method = "bv", 
                                       correct_inversion = TRUE) |>
      gapctd:::qc_check(prop_max_flag = 0.1,
                        prop_min_bin = 0.9)
  }

  if(!is.null(upcast_binned)) {
    upcast_binned <- upcast_binned |>
      gapctd:::check_density_inversion(threshold  = -1e-4, 
                                       threshold_method = "bv", 
                                       correct_inversion = TRUE) |>
      gapctd:::qc_check(prop_max_flag = 0.1,
                        prop_min_bin = 0.9)
  }  

  if("final" %in% return_stages) {
    output_list[["downcast"]] <- downcast_binned
    output_list[["upcast"]] <- upcast_binned
    output_list[["bottom"]] <- onbottom
  }
  
  return(output_list)
  
}


#' Wrapper function for run_ctd (R workflow)
#' 
#' Runs run_ctd() on all cnv files in a filepath.
#' 
#' @param cnv_dir_path Path to directory containing cnv files to be processed.
#' @param processing_method Name of processing method; for saving output.
#' @param haul_df Optional. data.frame containing haul data from RACEBASE. Must provide arguments for vessel and cruise if not provided.
#' @param vessel Optional. Vessel code as a 1L numeric vector.
#' @param cruise Optional. Cruise code as a numeric vector (>= 1L). 
#' @param channel Optional. RODBC channel; only used when haul_df = NULL.
#' @return Writes rds files with cast data to /output/[processing_method]
#' @export

wrapper_run_gapctd <- function(cnv_dir_path = here::here("cnv"),
                               processing_method,
                               haul_df = NULL,
                               vessel = NULL,
                               cruise = NULL,
                               channel = NULL) {
  stopifnot("wrapper_run_gapctd: Output path does not exist. Make sure output/[processing_method] was created with gapctd::setup_gapctd_directory()." = dir.exists(here::here("output", processing_method)))
  stopifnot("wrapper_run_gapctd: cnv path does not exist. Make sure cnv_dir_path was created with gapctd::setup_gapctd_directory()." = dir.exists(cnv_dir_path))
  
  if(is.null(haul_df)) {
    if(is.null(channel)) {
      channel <- gapctd::get_connected()
    }
    
    haul_df <- gapctd:::get_haul_data(channel = channel,
                                      vessel = vessel,
                                      cruise = cruise,
                                      tzone = "America/Anchorage")
  }
  
  # Input and output files
  cnv_files <- list.files(cnv_dir_path, pattern = "raw.cnv", full.names = TRUE)
  cnv_short <- list.files(cnv_dir_path, pattern = "raw.cnv", full.names = FALSE)
  rds_files <- gsub(pattern = ".cnv", replacement = ".rds", x = cnv_short)
  
  message(paste0("wrapper_run_gapctd: ", length(cnv_files), " files found in ", cnv_dir_path))
  
  for(II in 1:length(cnv_files)) {
    
    if(!file.exists(here::here("output", rds_files[II]))) {
      message(paste0("Processing ", cnv_short[II]))
      # Load CTD data
      ctd_dat <- oce::read.oce(file = cnv_files[II])
      
      processed_oce <- gapctd:::run_gapctd(x = ctd_dat, 
                                           haul_df = haul_df, 
                                           return_stages = c("final"), 
                                           ctd_tz = "America/Anchorage")
      
      saveRDS(processed_oce, file = here::here("output", processing_method, rds_files[II]))
    } else {
      message(paste0("skipping ", cnv_short[II]))
    }
  }
}
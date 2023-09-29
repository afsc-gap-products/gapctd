#' Wrapper function for run_ctd (R workflow)
#' 
#' Runs run_ctd() on all cnv files in a filepath.
#' 
#' @param cnv_dir_path Path to directory containing cnv files to be processed.
#' @param processing_method Name of processing method; for saving output.
#' @param haul_df Optional. data.frame containing  haul data from RACEBASE. Must provide arguments for vessel and cruise if not provided.
#' @param vessel Optional. Vessel code as a 1L numeric vector.
#' @param cruise Optional. Cruise code as a numeric vector (>= 1L). 
#' @param channel Optional. RODBC channel; only used when haul_df = NULL.
#' @param tzone Time zone for events and start_time in racebase/race_data tables. Passed to get_haul_data()
#' @return Writes rds files with cast data to /output/[processing_method]
#' @export

wrapper_run_gapctd <- function(cnv_dir_path = here::here("cnv"),
                               processing_method,
                               haul_df = NULL,
                               vessel = NULL,
                               cruise = NULL,
                               channel = NULL,
                               racebase_tzone = "America/Anchorage") {
  
  # Internal function to write CTD data to output files if the outputs contain data
  gapctd_write_rds <- function(x, out_path, in_path, gapctd_method, exclude_bottom = FALSE) {
    if(is.null(x)) {
      message("wrapper_run_gapctd: No data in ", in_path, ". Removing cast.")
      file.remove(in_path)
    } else {
      
      null_vec <- c()
      for(ii in 1:length(x)) {
        if(is.null(x[[ii]])) {
          null_vec <- -1*ii
        }
      }
      
      if(length(null_vec) > 0) {
        x <- x[null_vec]
      }
      
      if(exclude_bottom & ("bottom" %in% names(x))) {
        x <- x[-which(names(x) == "bottom")]
      }
      
      for(HH in 1:length(x)) {
        x[[HH]]@metadata$gapctd_method <- gapctd_method
      }
      saveRDS(x, file = out_path)
    }
  }
  
  
  stopifnot("wrapper_run_gapctd: Output path does not exist. Make sure output/[processing_method] was created with gapctd::setup_gapctd_directory()." = dir.exists(here::here("output", processing_method)))
  stopifnot("wrapper_run_gapctd: cnv path does not exist. Make sure cnv_dir_path was created with gapctd::setup_gapctd_directory()." = dir.exists(cnv_dir_path))
  
  if(is.null(haul_df)) {
    if(is.null(channel)) {
      channel <- gapctd::get_connected()
    }
    
    haul_df <- gapctd:::get_haul_data(channel = channel,
                                      vessel = vessel,
                                      cruise = cruise,
                                      tzone = racebase_tzone)
  }
  
  # Get paths to input files to be processed
  cnv_files <- list.files(cnv_dir_path, pattern = "raw.cnv", full.names = TRUE)
  cnv_short <- list.files(cnv_dir_path, pattern = "raw.cnv", full.names = FALSE)
  
  message(paste0("wrapper_run_gapctd: ", length(cnv_files), " files found in ", cnv_dir_path))
  
  # Create vectors of paths to output files
  rds_filenames_split <- here::here("data",
                                    "split",
                                    gsub(pattern = "raw.cnv", 
                                         replacement = "split.rds",
                                         x = cnv_short))
  rds_filenames_tsa <- here::here("output", 
                                  processing_method, 
                                  gsub(pattern = "raw.cnv", 
                                       replacement = "tsa.rds", 
                                       x = cnv_short))
  rds_filenames_typical_ctm <- here::here("output", 
                                          processing_method, 
                                          gsub(pattern = "raw.cnv", 
                                               replacement = "typical_ctm.rds", 
                                               x = cnv_short))
  rds_filenames_msg <- here::here("output", 
                                  processing_method, 
                                  gsub(pattern = "raw.cnv", 
                                       replacement = "msg.rds", 
                                       x = cnv_short))
  rds_filenames_typical <- here::here("output", 
                                      processing_method, 
                                      gsub(pattern = "raw.cnv", 
                                           replacement = "typical.rds", 
                                           x = cnv_short))
  rds_filenames_best <- here::here("output", 
                                   processing_method, 
                                   gsub(pattern = "raw.cnv", 
                                        replacement = "best.rds", 
                                        x = cnv_short))
  
  rds_filenames_final <- here::here("output", 
                                    processing_method, 
                                    gsub(pattern = "raw.cnv", 
                                         replacement = "final.rds", 
                                         x = cnv_short))
  
  
  for(II in 1:length(cnv_files)) {
    
    # Skip processing if typical output, best output, or final file already exists
    if(any(file.exists(rds_filenames_final[II],
                       rds_filenames_best[II],
                       rds_filenames_typical[II]))) {
      next
    }
    
    # Load CTD data
    ctd_dat <- oce::read.oce(file = cnv_files[II])
    
    # Skip files without useable data, such as files collected during between-leg maintenan
    if(length(ctd_dat@data$timeS) < 1000) {
      message(paste0("wrapper_run_gapctd: Skipping ", cnv_short[II], ". Insufficient data"))
      next
    }
    
    if(abs(diff(range(ctd_dat@data$pressure))) < 5) {
      message(paste0("wrapper_run_gapctd: Skipping ", cnv_short[II], ". Insufficient pressure range (", abs(diff(range(ctd_dat@data$pressure))), ")."))
      next
    }
    
    message(paste0("wrapper_run_gapctd: Processing ", cnv_files[II], "."))
    
    # Create files with just upcasts or downcast
    ctd_split <- gapctd::run_gapctd(x = ctd_dat, 
                                    haul_df = haul_df, 
                                    ctd_tz = "America/Anchorage",
                                    return_stage = "split")
    
    gapctd_write_rds(x = ctd_split,
                     in_path = cnv_files[II],
                     out_path = rds_filenames_split[II],
                     gapctd_method = NA,
                     exclude_bottom = FALSE)
    
    
    # TSA: Estimate temperature alignment and CTM parameters (optimization using area between T-S curves)
    
    msg_pars_dc <- list()
    msg_pars_uc <- list()
    
    if(all(c("downcast", "upcast") %in% names(ctd_split))) {
      ctd_tsa <- try(gapctd::run_gapctd(x = ctd_dat, 
                                        haul_df = haul_df, 
                                        ctd_tz = "America/Anchorage",
                                        return_stage = "full", # w/ Density inversion check and completeness check
                                        align_pars = list(),
                                        ctm_pars = list()),
                     silent = TRUE)
      
      if(is.list(ctd_tsa)) {
        gapctd_write_rds(x = ctd_tsa,
                         in_path = cnv_files[II],
                         out_path = rds_filenames_tsa[II],
                         gapctd_method = "TSA",
                         exclude_bottom = TRUE)

        msg_pars_dc <- ctd_tsa$downcast@metadata$ctm$downcast
        msg_pars_uc <- ctd_tsa$upcast@metadata$ctm$upcast
      }  
      
    }
    
    # MSG: Estimate temperature alignment and CTM parameters (optimization using S path distance)
    ctd_downcast_msg <- NULL
    ctd_upcast_msg <- NULL
    
    if("downcast" %in% names(ctd_split)) {
      sel_downcast <- oce::ctdTrim(x = ctd_dat,
                                   method = "range", 
                                   parameters = list(item = "timeS",
                                                     from = 0,
                                                     to = max(ctd_split$downcast@data$timeS + 0.25, na.rm = TRUE)))
      
      ctd_downcast_msg <- gapctd::run_gapctd(x = sel_downcast, 
                                             haul_df = haul_df, 
                                             ctd_tz = "America/Anchorage",
                                             return_stage = "full",
                                             align_pars = list(),
                                             ctm_pars = msg_pars_dc)
    }
    
    if("upcast" %in% names(ctd_split)) {
      sel_upcast <- oce::ctdTrim(x = ctd_dat,
                                 method = "range", 
                                 parameters = list(item = "timeS",
                                                   from = min(ctd_split$upcast@data$timeS - 0.25, na.rm = TRUE),
                                                   to = 5e6))
      
      ctd_upcast_msg <- gapctd::run_gapctd(x = sel_upcast, 
                                           haul_df = haul_df, 
                                           ctd_tz = "America/Anchorage",
                                           return_stage = "full", 
                                           align_pars = list(),
                                           ctm_pars = msg_pars_uc)
    }
    
    if(all("downcast" %in% names(ctd_downcast_msg), "upcast" %in% names(ctd_upcast_msg))) {
      ctd_msg <- list(downcast = ctd_downcast_msg[['downcast']],
                      upcast = ctd_upcast_msg[['upcast']])
    } 
    
    if(("downcast" %in% names(ctd_downcast_msg)) & !("upcast" %in% names(ctd_upcast_msg))) {
      ctd_msg <- ctd_downcast_msg
      
    }
    
    if(!("downcast" %in% names(ctd_downcast_msg)) & ("upcast" %in% names(ctd_upcast_msg))) {
      ctd_msg <- ctd_upcast_msg
    }
    
    gapctd_write_rds(x = ctd_msg,
                     in_path = cnv_files[II],
                     out_path = rds_filenames_msg[II],
                     gapctd_method = "MSG",
                     exclude_bottom = TRUE)
    
    
    if(any(c("downcast", "upcast") %in% names(ctd_split))) {
      # Typical CTM: Estimate temperature alignment, use manufacturer-recommended CTM parameters
      ctd_typical_ctm <- gapctd::run_gapctd(x = ctd_dat, 
                                            haul_df = haul_df, 
                                            ctd_tz = "America/Anchorage",
                                            return_stage = "full", # w/ Density inversion check and completeness check
                                            align_pars = list(),
                                            ctm_pars = list(alpha_C = 0.04, beta_C = 1/8))
      
      gapctd_write_rds(x = ctd_typical_ctm,
                       in_path = cnv_files[II],
                       out_path = rds_filenames_typical_ctm[II],
                       gapctd_method = "Typical CTM",
                       exclude_bottom = TRUE)
      
      # Typical: Manufacturer-recommended alignment and CTM parameters
      ctd_typical <- gapctd::run_gapctd(x = ctd_dat, 
                                        haul_df = haul_df, 
                                        ctd_tz = "America/Anchorage",
                                        return_stage = "full", # w/ Density inversion check and completeness check
                                        align_pars = list(temperature = -0.5),
                                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                                        cor_var = "conductivity")
      
      gapctd_write_rds(x = ctd_typical,
                       in_path = cnv_files[II],
                       out_path = rds_filenames_typical[II],
                       gapctd_method = "Typical",
                       exclude_bottom = FALSE)
    }
    
  }
  
  # Make metadata file
  gapctd:::make_metadata_file(rds_dir_path = here::here("output", processing_method),
                              in_pattern = "_typical.rds",
                              output_path = here::here("metadata", 
                                                       paste0("CTD_HAUL_DATA_", 
                                                              unique(vessel), "_", 
                                                              paste(unique(cruise), 
                                                                    collapse = "_"), ".rds")))
  
  # Move 'bad' files to bad_cnv
  move_bad_rds(rds_dir_path = here::here("data", "split"),
               in_pattern = "split.rds")
  
  # Move 'bad' files to bad_cnv
  move_bad_rds(rds_dir_path = here::here("output", processing_method),
               in_pattern = c("typical.rds", "typical_ctm.rds", "tsa.rds", "msg.rds"))
  
}



#' Process a .cnv file using the R workflow (R workflow)
#' 
#' Run all gapctd modules in order to process data from a cnv file.
#' 
#' @param x oce file
#' @param haul_df data.frame containing haul data from RACEBASE that includes metadata for the cnv file.
#' @param return_stage Character vector denoting which stages of processing should be included in the output (options "typical", "split", "align", "tmcorrect", "full"). Can return multiple stages simultaneously. Default = "full"
#' @param ctd_tz timezone for CTD as a character vector or numeric that is valid for POSIXct.
#' @param ctm_pars Used for remedial cell thermal mass corrections. Optional list of parameters to use for cell thermal mass correction. Must contain alpha_C and beta_C.
#' @param align_pars A list object with alignment parameters for a variable, e.g., list(temperature = -0.5)
#' @return A list of oce objects at stages of processing specified in return_stage.
#' @export

run_gapctd <- function(x, haul_df, return_stage = "full", ctd_tz = "America/Anchorage", ctm_pars = list(), align_pars = c(),
                       cor_var = "conductivity") {
  
  stopifnot("run_gapctd: Invalid return_stage. Must be one of 'split', 'median_filter', 'lowpass_filter', 'align', 'ctmcorrect', 'slowdown', 'bin_average', or 'full'" = 
              return_stage %in% c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown", "bin_average", "full"))
  
  output_list <- list()
  
  # Force timezone to AKDT (America/Anchorage)
  x@metadata$startTime <- lubridate::force_tz(x@metadata$startTime, 
                                              tz = ctd_tz)
  
  
  # Append haul data -------------------------------------------------------------------------------
  x <- x |> 
    gapctd:::append_haul_data(haul_df = haul_df)
  
  # Split upcast, downcast, and bottom -------------------------------------------------------------
  downcast <- x |>
    gapctd:::assign_metadata_fields(cast_direction = "downcast") |>
    gapctd:::section_oce(by = "datetime",
                         cast_direction = "downcast")
  
  upcast <- x |>
    gapctd:::assign_metadata_fields(cast_direction = "upcast") |>
    gapctd:::section_oce(by = "datetime",
                         cast_direction = "upcast")

  bottom <- x|> 
    gapctd:::section_oce(by = "datetime",
                         cast_direction = "bottom")
  
  # Return NULL if no data are available
  if(is.null(downcast) & is.null(upcast) & is.null(bottom)) {
    return(NULL)
  }
  
  # Return split cast ------------------------------------------------------------------------------
  if(return_stage == "split") {
    output_list[["downcast"]] <- downcast
    output_list[["upcast"]] <- upcast
    output_list[["bottom"]] <- bottom
    
    return(output_list)
  }
  
  # Median filter, low pass filter, and derive bottom ----------------------------------------------
  if(!is.null(bottom)) {
    bottom <- bottom |>
      gapctd:::median_filter(variables = c("temperature", "conductivity"),
                             window = c(5,5)) |>
      gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                              time_constant = c(0.5, 0.5, 1),
                              precision = c(4, 6, 3),
                              freq_n = 0.25) |>
      gapctd:::derive_eos()
  }

  # Median filter upcast and downncast -------------------------------------------------------------
  downcast <- downcast |>
    gapctd:::median_filter(variables = c("temperature", "conductivity"),
                           window = c(5,5))
  upcast <- upcast |>
    gapctd:::median_filter(variables = c("temperature", "conductivity"),
                           window = c(5,5))
  
  if(return_stage == "median_filter") {
    output_list[["downcast"]] <- downcast
    output_list[["upcast"]] <- upcast
    output_list[["bottom"]] <- bottom
    
    return(output_list)
  }
  
  # Low pass filter upcast and downncast -----------------------------------------------------------
  downcast <- downcast |>
    gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                            time_constant = c(0.5, 0.5, 1),
                            precision = c(4, 6, 3),
                            freq_n = 0.25)
  
  upcast <- upcast |>
    gapctd:::lowpass_filter(variables = c("temperature", "conductivity", "pressure"),
                            time_constant = c(0.5, 0.5, 1),
                            precision = c(4, 6, 3),
                            freq_n = 0.25)
  
  if(return_stage == "lowpass_filter") {
    output_list[["downcast"]] <- downcast
    output_list[["upcast"]] <- upcast
    output_list[["bottom"]] <- bottom
    
    return(output_list)
  }
  
  # Align upcast and downcast ----------------------------------------------------------------------
  if(!is.null(downcast)) {
    
    if(length(align_pars) == 0) {
      dc_align <- gapctd:::optim_align_par(x = downcast, 
                                           variable = "temperature", 
                                           offsets = seq(-1,1, 0.01), 
                                           cor_method = "pearson",
                                           cast_direction = "downcast")
    } else {
      dc_align <- fixed_alignment(x = downcast, 
                                  align_pars = align_pars, 
                                  cor_var = cor_var, 
                                  cast_direction = "downcast", 
                                  cor_method = "pearson")
    }

  downcast <- gapctd:::align_var(x = downcast, 
                                 variables = "temperature", 
                                 offset = dc_align[["temperature"]]['offset'], 
                                 interp_method = "linear")
  
  downcast@metadata[['align']] <- dc_align
  
  } 
  
  if(!is.null(upcast)) {
    
    if(length(align_pars) == 0) {
      uc_align <- gapctd:::optim_align_par(x = upcast, 
                                           variable = "temperature", 
                                           offsets = seq(-1,1, 0.01), 
                                           cor_method = "pearson",
                                           cast_direction = "upcast")
    } else {
      uc_align <- fixed_alignment(x = upcast, 
                                  align_pars = align_pars, 
                                  cor_var = cor_var, 
                                  cast_direction = "upcast", 
                                  cor_method = "pearson")
    }
    
    upcast <- gapctd:::align_var(x = upcast, 
                                 variables = "temperature", 
                                 offset = uc_align[["temperature"]]['offset'], 
                                 interp_method = "linear")
    
    upcast@metadata[['align']] <- uc_align
  }

  if(return_stage == "align") {
    output_list[["downcast"]] <- downcast
    output_list[["upcast"]] <- upcast
    output_list[["bottom"]] <- bottom
    
    return(output_list)
  }
  
  # Estimate conductivity cell thermal mass correction parameters ----------------------------------
  if(length(ctm_pars) == 0) {
    ctm_pars <- gapctd:::optim_ctm_pars(dc = downcast, 
                                        uc = upcast,
                                        optim_method = "L-BFGS-B",
                                        start_alpha_C = c(0.001, 0.01, 0.02, 0.04, 0.08, 0.12),
                                        start_beta_C = c(1, 1/2, 1/4, 1/8, 1/12, 1/24),
                                        default_parameters = c(alpha_C = 0.04, beta_C = 0.125),
                                        area_method = "ts") # Area between pressure-salinity curves
    
    ctm_alpha_C <- ctm_pars[['both']]['alpha_C']
    ctm_beta_C <- ctm_pars[['both']]['beta_C']
  } else {
    message("run_gapctd: Using user-specified conductivity cell thermal inertia correction parameters.")
    ctm_alpha_C <- ctm_pars[['alpha_C']]
    ctm_beta_C <- ctm_pars[['beta_C']]
  }
    
  
  # Apply conductivity cell thermal mass correction ------------------------------------------------
    downcast <- downcast |>
      gapctd:::conductivity_correction(alpha_C = ctm_alpha_C, beta_C = ctm_beta_C)
    
    upcast <- upcast |>
      gapctd:::conductivity_correction(alpha_C = ctm_alpha_C, beta_C = ctm_beta_C)
    
    if(!is.null(downcast)) {
      downcast@metadata[['ctm']] <- ctm_pars
    }

    if(!is.null(upcast)) {
      upcast@metadata[['ctm']] <- ctm_pars
    }
    
    if(return_stage == "ctmcorrect") {
      output_list[["downcast"]] <- downcast
      output_list[["upcast"]] <- upcast
      output_list[["bottom"]] <- bottom
      
      return(output_list)
    }
    
    
    # Slowdown  ------------------------------------------------------------------------------------
    
    downcast <- downcast |>
      gapctd:::slowdown(min_speed = 0.1, 
                         window = 5, 
                         cast_direction = "downcast") 
    
    upcast <- upcast |> 
      gapctd:::slowdown(min_speed = 0.1, 
                         window = 5, 
                         cast_direction = "upcast")
    
    
    if(return_stage == "slowdown") {
      output_list[["downcast"]] <- downcast
      output_list[["upcast"]] <- upcast
      output_list[["bottom"]] <- bottom
      
      return(output_list)
    }
    
    # Derive ---------------------------------------------------------------------------------------
    
    downcast <- gapctd::derive_eos(downcast)

    upcast <- gapctd::derive_eos(upcast)
    
    if(return_stage == "derive") {
      output_list[["downcast"]] <- downcast
      output_list[["upcast"]] <- upcast
      output_list[["bottom"]] <- bottom
      
      return(output_list)
    }
    
    # Bin average
    downcast <- gapctd:::bin_average(downcast, by = "depth", bin_width = 1)
    upcast <- gapctd:::bin_average(upcast, by = "depth", bin_width = 1)
    
    if(!is.null(downcast)) {
      downcast@metadata[['bin_average']] <- c(by = "depth", bin_width = 1)
    }
    
    if(!is.null(upcast)) {
      upcast@metadata[['bin_average']] <- c(by = "depth", bin_width = 1)
    }

    if(return_stage == "bin_average") {
      output_list[["downcast"]] <- downcast
      output_list[["upcast"]] <- upcast
      output_list[["bottom"]] <- bottom
      
      return(output_list)
    }
  
  # Check/correct density inversion; check if data are complete  -----------------------------------
  
    downcast <- downcast |>
      gapctd:::check_density_inversion(threshold  = -1e-4, 
                                       threshold_method = "bv", 
                                       correct_inversion = TRUE) |>
      gapctd:::qc_check(prop_max_flag = 0.1,
                        prop_min_bin = 0.9)
    
    upcast <- upcast |>
      gapctd:::check_density_inversion(threshold  = -1e-4, 
                                       threshold_method = "bv", 
                                       correct_inversion = TRUE) |>
      gapctd:::qc_check(prop_max_flag = 0.1,
                        prop_min_bin = 0.9)
    
    if(return_stage == "full") {
      output_list[["downcast"]] <- downcast
      output_list[["upcast"]] <- upcast
      output_list[["bottom"]] <- bottom
      
      return(output_list)
    }
  
  return(output_list)
  
}



#' Get haul data from RACEBASE (R workflow)
#' 
#' Retrieves vessel, cruise, haul, event times, locations, bottom depth, gear temperature, surface temperature, performance, haul type, and haul start time from racebase and race_data.
#' 
#' @param channel RODBC connection.
#' @param vessel Vessel code as a 1L numeric vector.
#' @param cruise Cruise code as a numeric vector (can include more than one).
#' @param out_path Optional. Filepath for the output R data file (.rds).
#' @param tzone Time zone for events and start_time in racebase/race_data tables.
#' @return Returns a data.frame containing haul data. Also saves haul data to an rds file.

get_haul_data <- function(channel, vessel, cruise, out_path = NULL, tzone = "America/Anchorage") {
  
  haul_dat <- RODBC::sqlQuery(channel = channel,
                              query =   paste0(
                                "select a.vessel, a.cruise, a.haul, a.bottom_depth, a.stationid, a.gear_depth, a.gear_temperature, a.surface_temperature, a.performance, a.haul_type, a.start_time, a.start_latitude, a.start_longitude, a.end_latitude, a.end_longitude, c.date_time, c.event_type_id, e.name
from racebase.haul a, race_data.cruises b, race_data.events c, race_data.hauls d, race_data.event_types e
where a.vessel = ", vessel, "and a.cruise in (", paste(cruise, collapse = ","), ") and a.vessel = b.vessel_id and a.cruise = b.cruise and c.haul_id = d.haul_id and d.haul = a.haul and d.cruise_id = b.cruise_id and c.event_type_id = e.event_type_id and c.event_type_id in (3,6,7)")) |>
    dplyr::mutate(DATE_TIME = lubridate::force_tz(DATE_TIME, tzone = "UTC"),
                  START_TIME = lubridate::force_tz(START_TIME, tzone = tzone))
  
  haul_dat <- haul_dat |>
    dplyr::inner_join(data.frame(EVENT_TYPE_ID = c(3,6,7),
                                 EVENT_NAME = c("ON_BOTTOM", "HAULBACK", "OFF_BOTTOM")),
                      by = "EVENT_TYPE_ID") |>
    tidyr::pivot_wider(id_cols = c("VESSEL", "CRUISE", "HAUL"),
                       names_from = c("EVENT_NAME"),
                       values_from = "DATE_TIME") |>
    dplyr::inner_join(haul_dat, by = c("VESSEL", "CRUISE", "HAUL")) |>
    dplyr::select(-NAME, -EVENT_TYPE_ID, -DATE_TIME) |>
    unique() |>
    as.data.frame()
  
  if(is.null(out_path)) {
    out_path <- here::here("output", paste0("HAUL_DATA_", vessel, "_", paste(cruise, collapse = "_"), ".rds"))
  }
  
  message(paste0("get_haul_data: Writing haul data to ", out_path, ". ",   nrow(haul_dat), " haul records." ))
  
  saveRDS(haul_dat, file = out_path)
  
  return(haul_dat)
}



#' Find cast times and metadata for a haul (R workflow)
#' 
#' Find haul metadata for a file and appends to oce object.
#' 
#' @param x oce object
#' @param haul_df data.frame containing haul metadata
#' @param ctd_tz timezone for the ctd as a character vector or numeric
#' @return A data.frame with haul metadata and cast times.
#' @export

append_haul_data <- function(x, haul_df, ctd_tz = "America/Anchorage") {
  # Assign CTD timezone in oce metadata
  delta_time <- abs(difftime(haul_df$START_TIME, 
                             x@metadata$startTime,
                             units = "secs"))
  min_delta_time <- min(delta_time)
  
  # Select haul data for the closest haul
  sel_haul <- haul_df[which.min(delta_time), ]
  
  
  # Create cast times data.frame
  cast_times <- data.frame(dc_start = x@metadata$startTime,
                           dc_end = sel_haul$ON_BOTTOM + 30,
                           uc_start = sel_haul$HAULBACK - 30,
                           uc_end = x@metadata$startTime + max(x@data$timeS))
  
  sel_haul <- cbind(sel_haul, cast_times)
  
  
  # Check for upcasts and downcasts
  scan_times <-   x@metadata$startTime + x@data$timeS
  n_down <- sum(scan_times < sel_haul$dc_end)
  n_up <- sum(scan_times > sel_haul$uc_start)
  n_bottom <- sum(scan_times > sel_haul$dc_end & scan_times < sel_haul$uc_start)
  
  message(paste0("append_haul_data: Scans found: ",  n_down, " downcast, ",
                 n_up, " upcast, ",
                 n_bottom, " bottom." ))
  
  sel_haul$missing_section <- any(c(n_down, n_up, n_bottom) < 1)
  sel_haul$filename <- x@metadata$filename
  
  sel_haul$deploy_id <- gsub(pattern = paste0(here::here("cnv"), "/"), 
                             replacement = "", 
                             x = gsub(pattern = "\\\\", replacement = "/", x = sel_haul$filename))
  sel_haul$deploy_id <- gsub(pattern = ".cnv", replacement = "", x = sel_haul$deploy_id)
  sel_haul$deploy_id <- gsub(pattern = "_raw", replacement = "", x = sel_haul$deploy_id)
  
  x@metadata$race_metadata <- sel_haul
  
  return(x)
}



#' Assign metadata fields (R workflow)
#' 
#' Assign metadata fields from x@metadata@race_metadata to the object.
#' 
#' @param x oce object that contains
#' @param cast_direction Cast direction ("downcast", "upcast", "bottom)
#' @return oce object with metadata fields updated with latitude, longitude, ship, bottom depth, deployment type, cruise, date, institute, scientist.
#' @export

assign_metadata_fields <- function(x, cast_direction) {
  
  cast_direction <- tolower(cast_direction)
  
  stopifnot("assign_meta_data_field: x must contain a data.frame named race_metadata in the metadata field." = "race_metadata" %in% names(x@metadata))
  stopifnot("assign_metadata_field: Argument 'cast_direction' must be \"downcast\", \"upcast\", or \"bottom\"" = cast_direction %in% c("downcast", "upcast", "bottom"))
  
  x@metadata$waterDepth <- x@metadata$race_metadata$BOTTOM_DEPTH
  x@metadata$gearDepth <- x@metadata$race_metadata$GEAR_DEPTH
  x@metadata$ship <- x@metadata$race_metadata$VESSEL
  x@metadata$deploymentType <- "trawl"
  x@metadata$cruise <- x@metadata$race_metadata$CRUISE
  x@metadata$date <- as.Date(x@metadata$startTime)
  x@metadata$institute <- "NOAA Alaska Fisheries Science Center"
  x@metadata$scientist <- "Groundfish Assessment Program, Resource Assessment and Conservation Engineering Division"
  x@metadata$cast_direction <- cast_direction
  
  if(cast_direction == "downcast") {
    x@metadata$latitude <- x@metadata$race_metadata$START_LATITUDE
    x@metadata$longitude <- x@metadata$race_metadata$START_LONGITUDE
  }
  
  if(cast_direction == "upcast") {
    x@metadata$latitude <- x@metadata$race_metadata$END_LATITUDE
    x@metadata$longitude <- x@metadata$race_metadata$END_LONGITUDE
  }
  
  if(cast_direction == "bottom") {
    x@metadata$latitude <- (x@metadata$race_metadata$START_LATITUDE + x@metadata$race_metadata$END_LATITUDE)/2
    x@metadata$longitude <- (x@metadata$race_metadata$START_LONGITUDE + x@metadata$race_metadata$END_LONGITUDE)/2
  }
  
  return(x)
  
}
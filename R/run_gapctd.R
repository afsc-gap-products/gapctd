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
  rds_files <- here::here("output", processing_method, gsub(pattern = "raw.cnv", replacement = "raw.rds", x = cnv_short))
  rds_dc <- here::here("output", processing_method, gsub(pattern = "raw.cnv", replacement = "dc_raw.rds", x = cnv_short))
  rds_uc <- here::here("output", processing_method, gsub(pattern = "raw.cnv", replacement = "uc_raw.rds", x = cnv_short))
  
  message(paste0("wrapper_run_gapctd: ", length(cnv_files), " files found in ", cnv_dir_path))
  
  for(II in 1:length(cnv_files)) {
    
    if(any(!file.exists(rds_files[II]), !file.exists(rds_dc[II]), !file.exists(rds_uc[II]))) {
      message(paste0("wrapper_run_gapctd: Processing ", cnv_short[II]))
      # Load CTD data
      ctd_dat <- oce::read.oce(file = cnv_files[II])
      
      if(length(ctd_dat@data$timeS) < 1000) {
        message(paste0("wrapper_run_gapctd: Skipping ", cnv_short[II], ". Insufficient data"))
        next
      }
      
      if(abs(diff(range(ctd_dat@data$pressure))) < 5) {
        message(paste0("wrapper_run_gapctd: Skipping ", cnv_short[II], ". Insufficient pressure range (", abs(diff(range(ctd_dat@data$pressure))), ")."))
        next
      }
      
      processed_oce <- gapctd:::run_gapctd(x = ctd_dat, 
                                           haul_df = haul_df, 
                                           return_stages = c("final"), 
                                           ctd_tz = "America/Anchorage")
      
      saveRDS(processed_oce, file = rds_files[II])
    } else {
      message(paste0("skipping ", cnv_short[II]))
    }
  }
}



#' Process a .cnv file using the R workflow (R workflow)
#' 
#' Run all gapctd modules in order to process data from a cnv file.
#' 
#' @param x oce file
#' @param haul_df data.frame containing haul data from RACEBASE that includes metadata for the cnv file.
#' @param return_stages Character vector denoting which stages of processing should be included in the output (options "typical", "split", "align", "tmcorrect", "final"). Can return multiple stages simultaneously. Default = "final"
#' @param ctd_tz timezone for CTD as a character vector or numeric that is valid for POSIXct.
#' @param ctm_pars Used for remedial cell thermal mass corrections. Optional list of parameters to use for cell thermal mass correction. Must contain alpha_C and beta_C.
#' @return A list of oce objects at stages of processing specified in return_stages.
#' @export

run_gapctd <- function(x, haul_df, return_stages = c("final"), ctd_tz = "America/Anchorage", ctm_pars = list()) {
  
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
      gapctd:::align_var(variables = "temperature", 
                         offset = -0.5, 
                         interp_method = "linear") |>
    gapctd:::assign_metadata_fields(cast_direction = "downcast") |>
      gapctd:::section_oce(by = "datetime",
                           cast_direction = "downcast") |>
      gapctd:::conductivity_correction(alpha_C = 0.04, beta_C = 1/8) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
      gapctd:::derive_eos() |>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    
    output_list[["typical_upcast"]] <- x |>
      gapctd:::align_var(variables = "temperature", 
                         offset = -0.5, 
                         interp_method = "linear") |>
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
  if(length(ctm_pars) == 0) {
    ctm_pars <- gapctd:::optim_ctm_pars(dc = downcast, 
                                        uc = upcast,
                                        optim_method = "L-BFGS-B",
                                        start_alpha_C = c(0.001, 0.01, 0.02, 0.04, 0.08, 0.12),
                                        start_beta_C = c(1, 1/2, 1/4, 1/8, 1/12, 1/24),
                                        default_parameters = c(alpha_C = 0.04, beta_C = 0.125))
    
    ctm_alpha_C <- ctm_pars[['both']]['alpha_C']
    ctm_beta_C <- ctm_pars[['both']]['beta_C']
  } else {
    message("run_gapctd: Using user-specified conductivity cell thermal inertia correction parameters.")
    ctm_alpha_C <- ctm_pars['alpha_C']
    ctm_beta_C <- ctm_pars['beta_C']
  }
  downcast_binned <-NULL
  upcast_binned <- NULL
  
  if(!is.null(downcast)) {
    
    downcast_binned <- downcast |>
      gapctd:::conductivity_correction(alpha_C = ctm_alpha_C, beta_C = ctm_beta_C) |>
      gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = "downcast") |>
      gapctd:::derive_eos()|>
      gapctd:::bin_average(by = "depth", bin_width = 1)
    
    downcast_binned@metadata[['ctm']] <- ctm_pars
    downcast_binned@metadata[['bin_average']] <- c(by = "depth", bin_width = 1)
  }
  
  if(!is.null(upcast)) {
    
  upcast_binned <- upcast |>
    gapctd:::conductivity_correction(alpha_C = ctm_alpha_C, beta_C = ctm_beta_C) |>
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



#' Get haul data from RACEBASE (R workflow)
#' 
#' Retrieves vessel, cruise, haul, event times, locations, bottom depth, gear temperature, surface temperature, performance, haul type, and haul start time from racebase and race_data.
#' 
#' @param channel RODBC connection.
#' @param vessel Vessel code as a 1L numeric vector.
#' @param cruise Cruise code as a numeric vector (can include more than one).
#' @param out_path Optional. Filepath for the output R data file (.rds).
#' @param tzone Time zone for events and star_time.
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
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

align_oxygen <- function(x,
                         alignment = c(-1:-7),
                         mode = "test",
                         channel = NULL,
                         haul_df = NULL,
                         racebase_tzone = "America/Anchorage",
                         output_path = NULL) {
  
  dir.create(path = here::here("output", "align_oxygen"))
  
  stopifnot("align_oxygen: mode argument must be either 'test' or 'align'" = mode %in% c("test", "align"))
  
  if(mode == "align") {
    stopifnot("align_oxygen: alignment argument must be a 1L numeric vector when mode = 'align'" = length(alignment) == 1)
  }
  
  if(mode == "test" & !is.null(output_path)) {
    warning("align_oxygen: output_path argument not used when mode = 'test'")
  }
  
  if(mode == "align" & !is.null(output_path)) {
    stopifnot("align_oxygen: output_path must have file extension .rds when mode = 'align'" = tolower(tools::file_ext(output_path)) == ".rds")
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
      
      new_obj[[cast]] <- suppressWarnings(gapctd:::run_gapctd(x = read.oce(file = dat[[cast]]@metadata$filename),
                                                              haul_df = haul_df,
                                                              return_stage = "full",
                                                              ctd_tz = "America/Anchorage",
                                                              ctm_pars = dat[[cast]]@metadata$ctm,
                                                              align_pars = c(temperature = as.numeric(dat[[cast]]@metadata$align$temperature['offset']),
                                                                             oxygenRaw = alignment[kk]),
                                                              cal_rds_path = here::here("psa_xmlcon", "calibration_parameters.rds"),
                                                              cor_var = "conductivity")[[cast]])
      
      deploy_id <- new_obj[[cast]]@metadata$race_metadata$deploy_id
      
      new_obj[[cast]]@metadata$align$rawOxygen['offset'] <- alignment[kk]
      
      out_df <- dplyr::bind_rows(out_df,
                                 as.data.frame(new_obj[[cast]]@data) |>
                                   dplyr::mutate(cast = cast,
                                                 deploy_id = deploy_id,
                                                 oxygen_offset = alignment[kk]))
      
    }
    
    # 'test' mode saves a data.frame with oxygen aligned by a specified value
    if(mode == "test") {
      
      ox_path <- here::here("output", "align_oxygen", paste0(deploy_id, "_ox_align.rds"))
      
      message("align_oxygen: Writing candidate oxygen profiles to ", ox_path)
      
      saveRDS(object = out_df, file = ox_path)
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
    
  }
  
}
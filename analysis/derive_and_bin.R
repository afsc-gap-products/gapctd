# Functions for plotting examples ------------------------------------------------------------------
derive_and_bin <- function(x, bin_width = NULL) {
  x$downcast <- gapctd::derive_eos(x$downcast)
  
  x$upcast <- gapctd::derive_eos(x$upcast) 
  
  
  if(!is.null(bin_width)) {
    x$downcast <- gapctd:::bin_average(x$downcast,
                                       by = "depth", 
                                       bin_width = bin_width)
    x$upcast <- gapctd:::bin_average(x$upcast,
                                     by = "depth", 
                                     bin_width = bin_width)
    
  }
  
  return(x)
  
}

make_stage_df <- function(x, stage, method) {
  
  out <- data.frame()
  
  if("downcast" %in% names(x)) {
    
    out <- dplyr::bind_rows(out, 
                            as.data.frame(x$downcast@data)|>
                              dplyr::mutate(cast_direction = x$downcast@metadata$cast_direction,
                                            processing_method = method) |>
                              dplyr::bind_cols(x$downcast@metadata$race_metadata |>
                                                 dplyr::select(VESSEL, CRUISE, HAUL, REGION))
    )
    
  }
  
  
  if("upcast" %in% names(x)) {
    
    out <- dplyr::bind_rows(out,
                            as.data.frame(x$upcast@data) |>
                              dplyr::mutate(cast_direction = x$upcast@metadata$cast_direction,
                                            processing_method = method) |>
                              dplyr::bind_cols(x$upcast@metadata$race_metadata |>
                                                 dplyr::select(VESSEL, CRUISE, HAUL, REGION)))
    
  }
  
  out$stage <- stage
  
  return(out)
}
#' Data frame to netCDF (.nc) file
#' 
#' Write 2D and/or 3D data from a data frame to a netCDF version-4 file using the RNetCDF package.
#' 
#' @param x A data frame containing data and metadata to add to write to a netCDF file.
#' @param output_filename Name of the output file name as a character vector.
#' @param dim_names_2d A vector of names of 2D spatial and time dimensions (e.g., LATITUDE, LONGITUDE, DATETIME).
#' @param dim_units_2d A vector of unit quantities for 2D dimensions.
#' @param dim_long_names_2d A vector of long_names for spatial and time dimensions.
#' @param var_names_2d A vector of variable names that for 2D variables (e.g., VESSEL, CRUISE, HAUL, CAST, STATIONID).
#' @param var_long_names_2d A vector of long_names for the 2D variable.
#' @param var_units_2d A vector of unit quantities for the 2D variables.
#' @param var_flag_values_2d Optional. A list of containing vectors of flag values for 2D flag variables where list item names correspond with variables in var_names_2d.
#' @param var_flag_meanings_2d Optional. A list of containing vectors of flag_meaning definitions for 2D flag variables where list item names correspond with variables in var_names_2d.
#' @param dim_names_3d A vector of names of 3D dimension (e.g., depth).
#' @param dim_long_names_3d A Vector of long_names of 3D dimension (e.g., "Depth in meters (positive down)").
#' @param dim_positive_3d A named list indicating which direction is down for 3D variable, i.e., list("depth" = "down")
#' @param dim_units_3d A vector of unit quantities for 3D dimensions.
#' @param dim_sort_3d Logical vector indicating whether 3D variable should be sorted prior to including in output.
#' @param var_names_3d A vector of variable names for 3D variables (e.g., TEMPERATURE, SALINITY).
#' @param var_units_3d A vector of unit quantities for the 3D variables (e.g., 'degrees Celsius', 'Practical Salinity').
#' @param var_long_names_3d A vector of long_names for the 3D variable.
#' @param var_flag_values_3d Optional. A list of containing vectors of flag values for 3D flag variables where list item names correspond with variables in var_names_3d.
#' @param var_flag_meanings_3d Optional. A list of containing vectors of flag_meaning definitions for 3D flag variables where list item names correspond with variables in var_names_3d.
#' @param instrument_attributes Optional. A vector of attribute names for the instrument variable.
#' @param instrument_values Optional. A list of attribute values for the instrument variable.
#' @param global_attributes A list of global attributes (i.e., non-dimensional) as 1L character, numeric, or date vectors, where names of list objects are the names of attributes.
#' @export
#' @author Sean Rohan

df_to_ncdf <- function(x,
                       output_filename,
                       dim_names_2d,
                       dim_long_names_2d = NULL,
                       dim_units_2d,
                       var_names_2d,
                       var_long_names_2d = NULL,
                       var_units_2d,
                       var_flag_values_2d = NULL,
                       var_flag_meanings_2d = NULL,
                       dim_names_3d = NULL,
                       dim_long_names_3d = NULL,
                       dim_positive_3d = NULL,
                       dim_units_3d = NULL,
                       dim_sort_3d = TRUE,
                       var_names_3d = NULL,
                       var_long_names_3d = NULL,
                       var_units_3d = NULL,
                       var_flag_values_3d = NULL,
                       var_flag_meanings_3d = NULL,
                       instrument_attributes = NULL,
                       instrument_values = NULL,
                       global_attributes) {
  
  # Check that all variables and dimensions are includes in the input data.frame
  if(!all(dim_names_2d %in% names(x))) {
    stop(paste0("df_to_ncdf: dim_names_2d variables (", dim_names_2d[which(!(dim_names_2d %in% names(x)))], ") not found in the input data frame."))
  }

  if(!all(var_names_2d %in% names(x))) {
    warning(paste0("df_to_ncdf: var_names_2d variables (", var_names_2d[which(!(var_names_2d %in% names(x)))], ") not found in the input data frame."))
    
    valid_2d <- which(var_names_2d %in% names(x))
    var_long_names_2d <- var_long_names_2d[valid_2d]
    var_names_2d <- var_names_2d[valid_2d]
    var_units_2d <- var_units_2d[valid_2d]
    
  }
  
  if(!all(dim_names_3d %in% names(x))) {
    stop(paste0("df_to_ncdf: dim_names_3d variables (", dim_names_3d[which(!(dim_names_3d %in% names(x)))], ") not found in the input data frame."))
  }
  
  if(!all(var_names_3d %in% names(x))) {
    warning(paste0("df_to_ncdf: var_names_3d variables (", var_names_3d[which(!(var_names_3d %in% names(x)))], ") not found in the input data frame."))
    
    valid_3d <- which(var_names_3d %in% names(x))
    var_long_names_3d <- var_long_names_3d[valid_3d]
    var_names_3d <- var_names_3d[valid_3d]
    var_units_3d <- var_units_3d[valid_3d]
    
  }
  
  if(length(dim_names_2d) != length(dim_units_2d)) {
    stop(paste0("df_to_ncdf: Length of dim_names_2d (", length(dim_names_2d), ") does not equal length of dim_units_2d (", length(dim_units_2d), ")."))
  }
  
  if(length(var_names_2d) != length(var_units_2d)) {
    stop(paste0("df_to_ncdf: Length of var_names_2d (", length(var_names_2d), ") does not equal length of var_units_2d (", length(var_units_2d), ")."))
  }
  
  if(length(dim_names_3d) != length(dim_units_3d)) {
    stop(paste0("df_to_ncdf: Length of dim_names_3d (", length(dim_names_3d), ") does not equal length of dim_units_3d (", length(dim_units_3d), ")."))
  }
  
  if(length(var_names_3d) != length(var_units_3d)) {
    stop(paste0("df_to_ncdf: Length of var_names_3d (", length(var_names_3d), ") does not equal length of var_units_3d (", length(var_units_3d), ")."))
  }
  
  if(!is.null(instrument_attributes) & !is.null(instrument_values)) {
    if(length(instrument_attributes) != length(instrument_values)) {
      stop(paste0("df_to_ncdf: Length of instrument_attributes (", length(instrument_attributes), ") does not equal length of instrument_values (", length(instrument_values), ")."))
    }
    
    if(class(instrument_values) != "list") {
      stop(paste0("df_to_ncdf: instrument_values has class ", class(instrument_values), " but should have class 'list'"))
    }
    
  }
  
  # Check for valid classes
  df_classes <- unlist(sapply(x, FUN = class))
  
  if(!all(df_classes %in% c("numeric", "integer", "character", "logical", "matrix", "factor"))) {
    stop(paste0("df_to_netcdf: Invalid class in x (",   unique(as.character(df_classes[which(!(df_classes  %in% c("numeric", "integer", "character", "logical", "matrix", "factor")))])), "). Supported classes are numeric, integer, character, logical, and factor"))
  }
  
  # Assign index value to each cast
  x <- x |> 
    dplyr::select(match(var_names_2d, names(x))) |>
    unique() |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::inner_join(x)
  
  # Create netCDF4 file
  ncout <- RNetCDF::create.nc(filename = output_filename,
                              format = "netcdf4")
  
  message("Adding index for ", length(unique(x$index)), " samples/locations.\n")
  # Define dimensions (index)
  RNetCDF::dim.def.nc(ncfile = ncout,
                      dimname = "index", 
                      dimlength = length(unique(x$index)))
  RNetCDF::var.def.nc(ncfile = ncout, 
                      varname = "index", 
                      vartype = "NC_INT", 
                      dimensions = "index")
  RNetCDF::var.put.nc(ncfile = ncout, 
                      variable = "index",
                      data = unique(x$index))
  RNetCDF::att.put.nc(ncfile = ncout,
                      variable = "index",
                      name = "long_name",
                      type = "NC_STRING",
                      value = "Sample index")
  
  if(!is.null(var_names_3d)) {
    
    for(ii in 1:length(dim_names_3d)) {
      
      dim_vals <- unique(x[,match(x = dim_names_3d[ii], names(x))])
      
      if(dim_sort_3d[ii]) {
        dim_vals <- sort(dim_vals)
      }
      
      message("Adding 3D dimension ", dim_names_3d[ii], " with units ", dim_units_3d[ii], "\n")
      
      RNetCDF::dim.def.nc(ncfile = ncout,
                          dimname = dim_names_3d[ii], 
                          dimlength = length(dim_vals))
      RNetCDF::var.def.nc(ncfile = ncout, 
                          varname = dim_names_3d[ii], 
                          vartype = vec_to_nc_class(vec = dim_vals), 
                          dimensions = dim_names_3d[ii])
      RNetCDF::var.put.nc(ncfile = ncout, 
                          variable = dim_names_3d[ii],
                          data = dim_vals)
      RNetCDF::att.put.nc(ncfile = ncout, 
                          variable = dim_names_3d[ii],
                          name = "units",
                          type = "NC_STRING",
                          value = dim_units_3d[ii])
      if(!is.null(dim_long_names_3d)){
        RNetCDF::att.put.nc(ncfile = ncout, 
                            variable = dim_names_3d[ii],
                            name = "long_name",
                            type = "NC_STRING",
                            value = dim_long_names_3d[ii])
      }
      
      if(!is.null(dim_positive_3d)) {
        if(dim_names_3d[ii] %in% names(dim_positive_3d)) {
          
          flag_index_3d <- which(names(dim_positive_3d) == dim_names_3d[ii])
          
          RNetCDF::att.put.nc(ncfile = ncout,
                              variable = dim_names_3d[ii],
                              name = "positive",
                              type = vec_to_nc_class(vec = dim_positive_3d[[flag_index_3d]]),
                              value = dim_positive_3d[[flag_index_3d]])
          
        }
      }
    }
  }
  
  # Define 2D dimensions that are indexed to hauls or casts
  combined_var_dim_names_2d <- c(dim_names_2d, var_names_2d)
  combined_var_dim_long_names_2d <- c(dim_long_names_2d, var_long_names_2d)
  combined_var_dim_units_2d <- c(dim_units_2d, var_units_2d)
  
  for(jj in 1:length(combined_var_dim_names_2d)) {
    
    message("Adding 2D variable ", combined_var_dim_names_2d[jj], " with units ", combined_var_dim_units_2d[jj], "\n")
    
    var_vals <- (x |> dplyr::select(match(x = c("index", 
                                                combined_var_dim_names_2d[jj]), 
                                          names(x))) |>
                   unique())[,2]
    
    RNetCDF::var.def.nc(ncfile = ncout, 
                        varname = combined_var_dim_names_2d[jj], 
                        vartype = vec_to_nc_class(vec = var_vals[!is.na(var_vals)]), 
                        dimensions = "index")
    RNetCDF::var.put.nc(ncfile = ncout, 
                        variable = combined_var_dim_names_2d[jj],
                        data = var_vals)
    RNetCDF::att.put.nc(ncfile = ncout, 
                        variable = combined_var_dim_names_2d[jj],
                        name = "units",
                        type = "NC_STRING",
                        value = combined_var_dim_units_2d[jj])
    if(!is.null(combined_var_dim_long_names_2d)){
      RNetCDF::att.put.nc(ncfile = ncout, 
                          variable = combined_var_dim_names_2d[jj],
                          name = "long_name",
                          type = "NC_STRING",
                          value = combined_var_dim_long_names_2d[jj])
    }
    
    if(!is.null(var_flag_meanings_2d)) {
      if(combined_var_dim_names_2d[jj] %in% names(var_flag_meanings_2d)) {
        
        flag_index_2d <- which(names(var_flag_meanings_2d) == combined_var_dim_names_2d[jj])
        
        RNetCDF::att.put.nc(ncfile = ncout,
                            variable = combined_var_dim_names_2d[jj],
                            name = "flag_values",
                            type = vec_to_nc_class(vec = var_flag_values_2d[[flag_index_2d]]),
                            value = var_flag_values_2d[[flag_index_2d]])
        
        RNetCDF::att.put.nc(ncfile = ncout,
                            variable = combined_var_dim_names_2d[jj],
                            name = "flag_meanings",
                            type = vec_to_nc_class(vec = var_flag_meanings_2d[[flag_index_2d]]),
                            value = var_flag_meanings_2d[[flag_index_2d]])
        
      }
    }
  }
  
  if(!is.null(var_names_3d)) {
    
    # Define 3D variables and assign to list
    for(kk in 1:length(var_names_3d)) {
      
      message("Adding 3D variable ", var_names_3d[kk], " with units ", var_units_3d[kk], "\n")
      
      val_matrix <- dplyr::select(x, match(c(var_names_3d[kk], 
                                             dim_names_3d, 
                                             "index"), 
                                           table = names(x))) |>
        tidyr::pivot_wider(id_cols = "index", 
                           names_from = dim_names_3d, 
                           values_from = var_names_3d[kk]) |>
        dplyr::select(-index) |>
        as.matrix() |> 
        t()
      
      mat_type <- try(vec_to_nc_class(vec = val_matrix[!is.na(val_matrix)]), silent = TRUE)
      
      if(class(mat_type) == "try-error") {
        message("val_matrix class", print(class(val_matrix)), "not supported. Returning object for inspection.\n")
        return(val_matrix)
      }
      
      if(mat_type == "NC_FLOAT") {
        val_matrix[is.na(val_matrix)] <- -9999
        fill_value <- -9999
      } else {
        fill_value <- -9999
      }
      
      RNetCDF::var.def.nc(ncfile = ncout, 
                          varname = var_names_3d[kk], 
                          vartype = mat_type, 
                          dimensions = c(dim_names_3d, "index"))
      RNetCDF::att.put.nc(ncfile = ncout, 
                          variable = var_names_3d[kk],
                          name = "_FillValue",
                          type = mat_type,
                          value = fill_value)
      RNetCDF::var.put.nc(ncfile = ncout, 
                          variable = var_names_3d[kk],
                          data = val_matrix)
      RNetCDF::att.put.nc(ncfile = ncout, 
                          variable = var_names_3d[kk],
                          name = "units",
                          type = "NC_STRING",
                          value = var_units_3d[kk])
      if(!is.null(var_long_names_3d)) {
        RNetCDF::att.put.nc(ncfile = ncout, 
                            variable = var_names_3d[kk],
                            name = "long_name",
                            type = "NC_STRING",
                            value = var_long_names_3d[kk])
      }
      
      if(!is.null(var_flag_meanings_3d)) {
        
        if(var_names_3d[kk] %in% names(var_flag_meanings_3d)) {
          
          flag_index_3d <- which(names(var_flag_meanings_3d) == var_names_3d[kk])
          
          RNetCDF::att.put.nc(ncfile = ncout,
                              variable = var_names_3d[kk],
                              name = "flag_values",
                              type = vec_to_nc_class(vec = var_flag_values_3d[[flag_index_3d]]),
                              value = var_flag_values_3d[[flag_index_3d]])
          
          RNetCDF::att.put.nc(ncfile = ncout,
                              variable = var_names_3d[kk],
                              name = "flag_meanings",
                              type = vec_to_nc_class(vec = var_flag_meanings_3d[[flag_index_3d]]),
                              value = var_flag_meanings_3d[[flag_index_3d]])
          
        }
      }
    }
  }

  # Add instrument attributes
  if(!is.null(instrument_attributes) & !is.null(instrument_values)) {
    message("Adding instrument attributes.\n")
    RNetCDF::var.def.nc(ncfile = ncout, 
                        varname = "instrument", 
                        vartype = vec_to_nc_class(vec = "NC_STRING"),
                        dimensions = NA)
    
    for(nn in 1:length(instrument_attributes)) {
      RNetCDF::att.put.nc(ncfile = ncout,
                          variable = "instrument",
                          name = instrument_attributes[nn],
                          type = vec_to_nc_class(vec = instrument_values[[nn]]),
                          value = instrument_values[[nn]])
    }
  }
  
  # Add global attributes to NetCDF object
  for(mm in 1:length(global_attributes)) {
    message(c("Adding global attribute ", names(global_attributes)[mm], "\n"))
    RNetCDF::att.put.nc(ncfile = ncout,
                        variable = "NC_GLOBAL",
                        name = names(global_attributes)[mm],
                        type = vec_to_nc_class(vec = global_attributes[[mm]]),
                        value = global_attributes[[mm]])
  }
  
  # Print summary
  RNetCDF::print.nc(ncout)
  
  # Close connection
  RNetCDF::close.nc(con = ncout)
  
}

#' Detect R vector class and return equivalent netCDF class
#' 
#' Supports numeric, integer, and character classes.
#' 
#' @param vec A vector of any type.
#' @param pkg Package to format output for, either RNetCDF or ncdf4
#' @export
#' @author Sean Rohan

vec_to_nc_class <- function(vec, pkg = "RNetCDF") {
  
  stopifnot("vec_to_nc_class: pkg must be either 'RNetCDF' or 'ncdf4'" = pkg %in% c("RNetCDF", "ncdf4"))
  
  vec_class <- class(vec)
  if(pkg == "RNetCDF") {
    if(vec_class == "numeric") {
      if(suppressWarnings(all(vec %% 1 == 0))) {
        prec <- "NC_INT"
      } else {
        prec <- "NC_FLOAT"
      }
    } else if(vec_class == "integer") {
      prec <- "NC_INT"
    } else if(vec_class == "character") {
      prec <- "NC_STRING"
    } else {
      stop(paste0("Class ", vec_class, " not supported in netCDF"))
    }
  } else if(pkg == "ncdf4") {
    if(vec_class == "numeric") {
      if(suppressWarnings(all(vec %% 1 == 0))) {
        prec <- "integer"
      } else {
        prec <- "float"
      }
    } else if(vec_class == "integer") {
      prec <- "integer"
    } else if(vec_class == "character") {
      prec <- "char"
    } else {
      stop(paste0("Class ", vec_class, " not supported in netCDF"))
    }
  }
  return(prec)
}



#' Calculate means for an oce object
#'
#' Apply function to selected variables in an oce object and return a data frame.
#' 
#' @param x oce object
#' @param prefix Character vector to append to variable names.
#' @param fun Function to apply to variable vectors.
#' @param vars Variables to apply function to. Automatically assigned when NULL (default)
#' @return A data.frame with variables calculated and race_metadata if provided (x@@metadata$race_metadata)
#' @export
#' @author Sean Rohan

calc_bottom_mean_oce <- function(x, 
                       prefix = "mean_bottom_", 
                       fun = mean, 
                       vars = NULL) {
  
  if(is.null(vars)) {
    vars <- c("temperature", "salinity", "absolute_salinity", "sound_speed", "density", "oxygen", "pH")
  }
  
  out_df <- as.data.frame(x@data) |>
    apply(MARGIN = 2, FUN = fun) |>
    t() |>
    as.data.frame() |>
    dplyr::select(matches(vars)) |>
    dplyr::rename_with(~paste0(prefix, .x)) |>
    dplyr::rename_with(toupper)
  
    out_df <- dplyr::bind_cols(out_df, 
                               x@metadata$race_metadata)
    
    calibration_date <- x@metadata$header[which(grepl(pattern = "CalibrationDate", x@metadata$header))]
    calibration_date <- gsub(pattern = ".*<CalibrationDate>", "", calibration_date)
    calibration_date <- gsub(pattern = "</CalibrationDate>.*", "", calibration_date)
    
    out_df$ctd_calibration_date <- calibration_date[1]
    
    serial_number <- x@metadata$header[which(grepl(pattern = "<SerialNumber>", x@metadata$header))]
    serial_number <- gsub(pattern = ".*<SerialNumber>", "", serial_number)
    serial_number <- gsub(pattern = "</SerialNumber>.*", "", serial_number)
  
    out_df$ctd_serial_number <- serial_number[1]
    
  return(out_df)
  
}



#' Calculate bottom variables and make metadata file from cast .rds files (R workflow)
#' 
#' Calculates means of bottom variables using calc_bottom_mean_oce and retrieves metadata from .rds files, and combines bottom and metadata for all casts in rds_dir_path.
#' 
#' @param rds_dir_path Path to a directory containing rds files that include oce objects named 'bottom'
#' @param in_pattern Character vector search pattern for input files.
#' @param output_path Output file path (.rds) where file should be saved.
#' @return data.frame containing metadata for all deployments.
#' @export
#' @author Sean Rohan

make_metadata_file <- function(rds_dir_path = here::here("output", "gapctd"), 
                               in_pattern = "_qc.rds", 
                               output_path = NULL) {
  
  rds_files <- list.files(rds_dir_path, pattern = in_pattern, full.names = TRUE)
  metadata_df <- data.frame()
  
  for(HH in 1:length(rds_files)) {
    
    ctd_dat <- readRDS(rds_files[HH])
    
    if("bottom" %in% names(ctd_dat)) {
      
      metadata_df <- dplyr::bind_rows(metadata_df,
                                      ctd_dat$bottom |>
                                        gapctd:::calc_bottom_mean_oce(prefix = "mean_bottom_"))
      
    } else {
      warning(paste0("No bottom data in ", rds_files[HH]))
    }
  }
  
  if(!is.null(output_path)) {
    saveRDS(object = metadata_df, file = output_path)
  }
  
  return(metadata_df)
}



#' Make netCDF from profile and metadata rds files (R workflow)
#' 
#' Read-in files from /accepted_profiles/ and /metadata/ subdirectories and create a netCDF file. Combines data from multiple vessels from the same cruise. Must provide the following global_attributes to the function in a list: references, id, cdm_data_type, cruise, institution, contributor_name, creator_name, creator_institution, creator_email,publisher, publisher_type, publisher_url, geospatial_bounds_crs, license, metadata_link, instrument, standard_name_vocabulary, Conventions, source.
#' 
#' @param cast_files Paths to accepted profiles.
#' @param metadata_files Paths to metadata files.
#' @param output_file Output filepath for netCDF file.
#' @param precision Precision to use for variables as a named numeric vector.
#' @param global_attributes List of global attributes that is passed to gapctd::df_to_netcdf(global_attributes).
#' @param vessel_attributes data.frame containing vessel metadata
#' @export
#' @author Sean Rohan

make_oce_ncdf <- function(cast_files = c(list.files(path = here::here("final_cnv", processing_method), full.names = TRUE, pattern = "final.rds")),
                          metadata_files = c(list.files(path = here::here("metadata"), full.names = TRUE)), 
                          output_file = "output.nc",
                          precision = c(temperature = 4,
                            conductivity = 6,
                            salinity = 4,
                            absolute_salinity = 4,
                            sound_speed = 3,
                            density = 3,
                            pressure = 3,
                            oxygen = 4,
                            ph = 3,
                            velocity = 3),
                          global_attributes = list(title = "CTD Data from AFSC 2021 EBS/NBS Bottom Trawl Survey",
                                                   references = "CTD TEAM...",
                                                   id = "doi",
                                                   cdm_data_type = "Point",
                                                   cruise = "2021 Eastern Bering Sea Continental Shelf and Northern Bering Sea Bottom-Trawl Survey",
                                                   institution = "NOAA Alaska Fisheries Science Center",
                                                   contributor_name = "",
                                                   creator_name = "",
                                                   creator_institution = "NOAA Alaska Fisheries Science Center",
                                                   creator_email = "",
                                                   publisher = "NOAA Alaska Fisheries Science Center",
                                                   publisher_type = "institution",
                                                   publisher_url = "https://www.fisheries.noaa.gov/about/alaska-fisheries-science-center",
                                                   geospatial_bounds_crs = "EPSG:4326",
                                                   license = "These data may be redistributed and used without restriction.",
                                                   metadata_link = "[DOI]",
                                                   instrument = "CTD",
                                                   Conventions = "CF-1.8",
                                                   standard_name_vocabulary = "CF Standard Name Table v79",
                                                   source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd"))),
                          vessel_attributes = data.frame(vessel = c(94, 134, 148, 162, 176),
                                                         vessel_name = c("Vesteraalen", "Northwest Explorer", "Ocean Explorer", "Alaska Knight", "Alaska Provider"),
                                                         imo = c(8010219, 7926538, 8412297, 9037769, 8213225),
                                                         call_sign = c("WDJ3586", "WCZ9007", "WCZ9006", "WDD6948", "WDG2215"),
                                                         flag = c("USA", "USA", "USA", "USA", "USA"),
                                                         year_built = c(1979, 1979, 1984, 1993, 1982))) {
  
  req_attributes <- c("title", "references", "id", "cdm_data_type", "cruise", "institution", "contributor_name",
                      "creator_name", "creator_institution", "creator_email","publisher", "publisher_type", 
                      "publisher_url", "geospatial_bounds_crs", "license", "metadata_link", "instrument", "standard_name_vocabulary", "Conventions", "source")
  
  if(!all(req_attributes %in% names(global_attributes))) {
    stop(paste0("make_ctd_ncdf: The follow required global attribute(s) were not found in global_attributes : ", req_attributes[which(!(req_attributes %in% names(global_attributes)))]))
  }
  
  metadata_df <- data.frame()
  all_profiles <- data.frame()
  
  for(ii in metadata_files) {
    metadata_df <- dplyr::bind_rows(metadata_df,
                                    readRDS(file = ii))
  }
  
  metadata_df <- dplyr::bind_rows(
    metadata_df |>
      dplyr::mutate(deploy_id = paste0(deploy_id, "_downcast"),
                    CASTTIME = ON_BOTTOM) |>
      dplyr::select(-END_LONGITUDE, -END_LATITUDE) |>
      dplyr::rename(LONGITUDE = START_LONGITUDE,
                    LATITUDE = START_LATITUDE),
    metadata_df |>
      dplyr::mutate(deploy_id = paste0(deploy_id, "_upcast"),
                    CASTTIME = HAULBACK) |>
      dplyr::select(-START_LONGITUDE, -START_LATITUDE) |>
      dplyr::rename(LONGITUDE = END_LONGITUDE,
                    LATITUDE = END_LATITUDE))
  
  for(jj in 1:length(cast_files)) {
    
    deployment_dat <- readRDS(file = cast_files[jj])
    
    cast_dir <- c("downcast", "upcast")[c("downcast", "upcast") %in% names(deployment_dat)]
    
    for(kk in 1:length(cast_dir)) {
      new_profile <- deployment_dat[[which(names(deployment_dat) == cast_dir[kk])]]
      
      new_profile@metadata$race_metadata$deploy_id <- paste0(new_profile@metadata$race_metadata$deploy_id, "_", match.arg(cast_dir[kk], c("downcast", "upcast")))
      
      if(any(table(new_profile@data$depth) > 1)) {
        # Corner case. Not sure why this happens but it likely has to do with some issue that can occur with the identify function.
        stop(paste0("make_ctd_ncdf: Duplicate depth values found in ", cast_files[jj], ". Please remove duplicates from the file then rerun make_ctd_ncdf()."))
      }
      
      if(jj > 1) {
        if(new_profile@metadata$race_metadata$deploy_id[1] %in% all_profiles$deploy_id) {
          stop(paste0("make_ctd_ncdf: ", cast_files[jj], " cannot be added to profile_df because ", new_profile@metadata$race_metadata$deploy_id[1], " has already been added. Please check that deploy_id values are unique for each profile (i.e. no two hex/cnv files can have the same file name)."))
        }
      }
      
      new_profile_df <- as.data.frame(new_profile@data)
      new_profile_df$deploy_id <- new_profile@metadata$race_metadata$deploy_id[1]
      
      profile_df <- dplyr::inner_join(new_profile_df, metadata_df, by = "deploy_id")
      
      all_profiles <- dplyr::bind_rows(all_profiles, profile_df)
    }
  }
  
  # Define temporal coverage
  time_coverage <- paste0(as.character(range(all_profiles$CASTTIME)), " UTC")
  
  # Convert time to character for netCDF NC_STRING format
  all_profiles$CASTTIME <- as.character(all_profiles$CASTTIME)
  
  # Rename columns to match CF naming conventions
  names(all_profiles) <- tolower(names(all_profiles))
  
  # Set precision
  if(!is.null(precision)) {

    precision_index <- which(names(all_profiles) %in% names(precision))
    
    for(ii in 1:length(precision_index)) {

      n_digits <- precision[which(names(precision) == names(all_profiles)[precision_index[ii]])]
      
      message("make_oce_ncdf: Setting precision of ", names(all_profiles)[precision_index[ii]], " to ", n_digits)
      
      all_profiles[, precision_index[ii]] <- round(all_profiles[, precision_index[ii]], n_digits)
      
    }
  }

  
  
  all_profiles <- all_profiles |>
    dplyr::rename(time = casttime,
                  profile = deploy_id,
                  stationid = stationid,
                  vessel = vessel,
                  cruise = cruise,
                  haul = haul,
                  haul_depth = gear_depth,
                  sea_floor_temperature = mean_bottom_temperature,
                  sea_floor_salinity = mean_bottom_absolute_salinity,
                  sea_floor_practical_salinity = mean_bottom_salinity,
                  sea_floor_sound_speed_in_sea_water = mean_bottom_sound_speed,
                  time_elapsed = times,
                  sea_water_pressure = pressure,
                  sea_water_temperature = temperature,
                  sea_water_practical_salinity = salinity,
                  sea_water_salinity = absolute_salinity,
                  sea_water_density = density,
                  sea_water_electrical_conductivity = conductivity,
                  sound_speed_in_sea_water = sound_speed,
                  buoyancy_frequency = n2,
                  quality_flag = flag)
  
  if("ph" %in% names(all_profiles)) {
    all_profiles <- all_profiles |>
      dplyr::rename(sea_water_ph_reported_on_total_scale = ph,
                    sea_floor_ph_reported_on_total_scale = mean_bottom_ph)
  }
  
  if("oxygen" %in% names(all_profiles)) {
    all_profiles <- all_profiles |>
      dplyr::rename(sea_water_dissolved_oxygen = oxygen,
                    sea_floor_dissolved_oxygen = mean_bottom_oxygen)
  }
  
  message("make_oce_ncdf: Profile columns found: ", names(all_profiles))
  
  # Define spatial extent of data set using WKT polygon
  geospatial_bounds <- cbind(
    c(
      min(all_profiles$latitude),
      min(all_profiles$latitude),
      max(all_profiles$latitude),
      max(all_profiles$latitude),
      min(all_profiles$latitude)),
    c(
      min(all_profiles$longitude), 
      max(all_profiles$longitude),
      max(all_profiles$longitude),
      min(all_profiles$longitude),
      min(all_profiles$longitude))
  )
  
  geospatial_bounds <- paste0("POLYGON ((",
                              paste(apply(X = geospatial_bounds, MARGIN = 1, FUN = paste, collapse = " "), collapse = ", "),
                              "))")
  
  instrument_df <- all_profiles |>
    dplyr::select(vessel, ctd_serial_number, ctd_calibration_date) |>
    unique()
  
  if(!is.null(vessel_attributes)) {
    instrument_df <- dplyr::left_join(instrument_df, 
                                      vessel_attributes, 
                                      by = "vessel")
  }
  
  # Set up global attributes list
  g_attributes <- list(references = global_attributes$references,
                       id = global_attributes$id,
                       cruise =  global_attributes$cruise,
                       institution = global_attributes$institution,
                       contributor_name = global_attributes$contributor_name,
                       creator_name = global_attributes$creator_name,
                       creator_institution = global_attributes$creator_institution,
                       creator_email = global_attributes$creator_email,
                       publisher = global_attributes$publisher,
                       publisher_type = global_attributes$publisher_type,
                       publisher_url = global_attributes$publisher_url,
                       geospatial_bounds = geospatial_bounds,
                       geospatial_bounds_crs = global_attributes$geospatial_bounds_crs,
                       license = global_attributes$license,
                       metadata_link = global_attributes$metadata_link,
                       date_created = as.character(Sys.Date()),
                       instrument = global_attributes$instrument,
                       Conventions = global_attributes$Conventions,
                       standard_name_vocabulary = global_attributes$standard_name_vocabulary,
                       cdm_data_type = global_attributes$cdm_data_type,
                       time_coverage_start = time_coverage[1],
                       time_coverage_end = time_coverage[2],
                       source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd")))
  
  # Convert POSIXct to character
  convert_index <- which(!(unlist(sapply(all_profiles, 
                                         FUN = function(x) class(x)[1])) %in% c("numeric", "integer", "character", "logical", "matrix", "factor")))
  
  message("make_oce_ncdf: Converting ", paste(names(all_profiles)[convert_index], sep = ", ") ," to character.\n" )
  for(ii in convert_index) {
    all_profiles[, ii] <- as.character(all_profiles[, ii])
  }
  
  
  # Create netCDF file
  gapctd::df_to_ncdf(x = all_profiles,
                     output_filename = output_file,
                     dim_names_2d = c("latitude", "longitude", "time"),
                     dim_units_2d = c("degree_north", "degree_east", "time"),
                     dim_long_names_2d = c("Latitude (decimal degrees)", "Longitude (decimal degrees)", 
                                           "Time in Coordinated Universal Time (UTC)"),
                     var_names_2d = c("stationid", "profile", "vessel", "cruise", "haul", "haul_depth", "sea_floor_temperature", "sea_floor_practical_salinity", "sea_floor_salinity", "sea_floor_sound_speed_in_sea_water", "sea_floor_ph_reported_on_total_scale", "sea_floor_dissolved_oxygen"),
                     var_long_names_2d = c("AFSC/RACE/GAP Survey Station Name", "Profile Number and Direction", "AFSC/RACE/GAP Vessel Code", "AFSC/RACE/GAP Cruise Code", "Haul Number", "Mean towed depth of CTD during haul", "Mean bottom temperature (ITS-90) at towed depth", "Mean Practical Salinity (PSS-78) at towed depth", "Mean Absolute Salinity (TEOS-10 GSW) at towed depth", "Mean speed of sound during haul (Chen-Millero)", "Acidity (pH scale) at the towed depth",  "Dissolved oxygen concentration at towed depth (SBE 43)"),
                     var_units_2d = c("1", "1", "1", "1", "1", "m", "degree_C", "1", "g kg-1", "m s-1", "1", "ml l-1"),
                     dim_names_3d = c("depth"),
                     dim_long_names_3d = c("Depth in meters"),
                     dim_positive_3d = list("depth" = "down"),
                     dim_units_3d = c("m"),
                     dim_sort_3d = c(TRUE),
                     var_names_3d = c("time_elapsed", "sea_water_pressure", "sea_water_temperature", "sea_water_practical_salinity", "sea_water_salinity", "sea_water_density", "buoyancy_frequency", "sea_water_electrical_conductivity", "sound_speed_in_sea_water", "sea_water_dissolved_oxygen", "sea_water_ph_reported_on_total_scale", "quality_flag"),
                     var_long_names_3d = c("Time Elapsed Since Start of Deployment", "Strain Gauge Pressure", "ITS-90 Temperature", "PSS-78 Practical Salinity", "TEOS-10 GSW Absolute Salinity", "TEOS-10 GSW Density", "Squared Brunt-Vaisala Buoyancy Frequency", "Electrical Conductivity", "Speed of Sound (Chen-Millero)", "Acidity (pH scale)",  "Dissolved oxygen concentration (SBE 43)",  "Data Quality Assurance Flag"),
                     var_flag_values_3d = list("quality_flag" = c(-6,-1,0,6,7)),
                     var_flag_meanings_3d = list("quality_flag" = c("Interpolated value. Density inversion error detected based on buoyancy frequency and remains after automatic removal and interpolation of point.",
                                                                    "Possible pH data quality issue due to sensor drift.",
                                                                    "Good quality, no issues detected.",
                                                                    "Interpolated value. Density inversion error detected based on buoyancy frequency and corrected using automatic removal and interpolation of point.",
                                                                    "Interpolated value. Point mannually flagged and removed during visual inspection then estimated through interpolation.")),
                     var_units_3d = c("s", "dbar", "degree_C", "1", "g kg-1", "kg m-3", "s-2", "S m-1)", "m s-1", "1", "ml l-1", "1"),
                     instrument_attributes = c("make_model", "serial_number", "calibration_date", "vessel", "vessel_name", "imo", "call_sign", "flag", "year_built"),
                     instrument_values = list(make_model = "Sea-Bird SBE19plus V2",
                                              serial_number = instrument_df$ctd_serial_number,
                                              calibration_date = instrument_df$ctd_calibration_date,
                                              vessel = instrument_df$vessel,
                                              vessel_name = instrument_df$vessel_name,
                                              imo = instrument_df$imo,
                                              call_sign = instrument_df$call_sign,
                                              flag = instrument_df$flag,
                                              year_built = instrument_df$year_built),
                     global_attributes = g_attributes)
  
}



#' Write cast data to an oce or rds file
#' 
#' @param cast_files Paths to accepted profiles.
#' @param output_file Output filepath for netCDF file.
#' @param precision Precision to use for variables as a named numeric vector.
#' @export
#' @author Sean Rohan

make_oce_table <- function(cast_files,
                           precision = c(temperature = 4,
                                         conductivity = 6,
                                         salinity = 4,
                                         absolute_salinity = 4,
                                         sound_speed = 3,
                                         density = 3,
                                         pressure = 3,
                                         oxygen = 4,
                                         ph = 3,
                                         velocity = 3),
                           output_file) {
  
  stopifnot("make_oce_table: output_file extension must be .csv or .rds" = any(grepl(pattern = ".csv", x = output_file, ignore.case = TRUE),
                                                                               grepl(pattern = ".rds", x = output_file, ignore.case = TRUE)))
  
  cast_df <- data.frame()
  
  for(ii in 1:length(cast_files)) {
    
    dat <- readRDS(file = cast_files[ii])
    
    if("downcast" %in% names(dat)) {
      sel_cast <- dat$downcast
      
      # if(sel_cast@metadata$gapctd_method == "TSA") {
        # alpha_C <- sel_cast@metadata$ctm$both['alpha_C']
        # beta_C <- sel_cast@metadata$ctm$both['beta_C']
      # } else {
        if("both" %in% names(sel_cast@metadata$ctm)) {
          alpha_C <- sel_cast@metadata$ctm$both['alpha_C']
          beta_C <- sel_cast@metadata$ctm$both['beta_C']
        } else {
          alpha_C <- unlist(sel_cast@metadata$ctm['alpha_C'])
          beta_C <- unlist(sel_cast@metadata$ctm['beta_C'])
        }
      # }
      
      sel_cast_data <- as.data.frame(sel_cast@data) |>
        dplyr::mutate(vessel = sel_cast@metadata$race_metadata$VESSEL,
                      cruise = sel_cast@metadata$race_metadata$CRUISE,
                      haul = sel_cast@metadata$race_metadata$HAUL,
                      stationid = sel_cast@metadata$race_metadata$STATIONID,
                      deploy_id = gsub(pattern = "_raw", 
                                       replacement = "", 
                                       x = gsub(pattern = ".cnv", 
                                                replacement = "", 
                                                x = basename(sel_cast@metadata$filename))),
                      latitude = sel_cast@metadata$latitude,
                      longitude = sel_cast@metadata$longitude,
                      timeS = sel_cast@metadata$startTime + timeS,
                      processing_method = sel_cast@metadata$gapctd_method,
                      alpha_C = alpha_C,
                      beta_C = beta_C,
                      temperature_offset = sel_cast@metadata$align$temperature['offset'],
                      serial_number = sel_cast@metadata$serialNumberTemperature,
                      sample_interval = sel_cast@metadata$sampleInterval,
                      cast_direction = "downcast")
      cast_df <- dplyr::bind_rows(cast_df, sel_cast_data)
    }
    
    if("upcast" %in% names(dat)) {
      sel_cast <- dat$upcast
      
      # if(sel_cast@metadata$gapctd_method == "TSA") {
      #   alpha_C <- sel_cast@metadata$ctm$both['alpha_C']
      #   beta_C <- sel_cast@metadata$ctm$both['beta_C']
      # } else {
        if("both" %in% names(sel_cast@metadata$ctm)) {
          alpha_C <- sel_cast@metadata$ctm$both['alpha_C']
          beta_C <- sel_cast@metadata$ctm$both['beta_C']
        } else {
          alpha_C <- unlist(sel_cast@metadata$ctm['alpha_C'])
          beta_C <- unlist(sel_cast@metadata$ctm['beta_C'])
        }
      # }
      
      sel_cast_data <- as.data.frame(sel_cast@data) |>
        dplyr::mutate(vessel = sel_cast@metadata$race_metadata$VESSEL,
                      cruise = sel_cast@metadata$race_metadata$CRUISE,
                      haul = sel_cast@metadata$race_metadata$HAUL,
                      stationid = sel_cast@metadata$race_metadata$STATIONID,
                      deploy_id = gsub(pattern = "_raw", 
                                       replacement = "", 
                                       x = gsub(pattern = ".cnv", 
                                                replacement = "", 
                                                x = basename(sel_cast@metadata$filename))),
                      latitude = sel_cast@metadata$latitude,
                      longitude = sel_cast@metadata$longitude,
                      timeS = sel_cast@metadata$startTime + timeS,
                      # processing_method = sel_cast@metadata$gapctd_method,
                      alpha_C = alpha_C,
                      beta_C = beta_C,
                      temperature_offset = sel_cast@metadata$align$temperature['offset'],
                      serial_number = sel_cast@metadata$serialNumberTemperature,
                      sample_interval = sel_cast@metadata$sampleInterval,
                      cast_direction = "upcast")
      cast_df <- dplyr::bind_rows(cast_df, sel_cast_data)
    }
  }
  
  # Add oxygen and ph fields if not included
  if(!("oxygen" %in% names(cast_df))) {
    cast_df$oxygen <- NA
  }
  
  if(!("pH" %in% names(cast_df))) {
    cast_df$ph <- NA 
  }
  
  if(!is.null(precision)) {
    # Set precision
    precision_index <- which(names(cast_df) %in% names(precision))
    
    for(ii in 1:length(precision_index)) {
      
      n_digits <- precision[which(names(precision) == names(cast_df)[precision_index[ii]])]
      
      message("make_oce_ncdf: Setting precision of ", names(cast_df)[precision_index[ii]], " to ", n_digits)
      
      cast_df[, precision_index[ii]] <- round(cast_df[, precision_index[ii]], n_digits)
      
    }
  }
  
  message("make_oce_ncdf: Writing ", nrow(cast_df), " records to ", output_file)
  if(grepl(pattern = ".csv", x = output_file, ignore.case = TRUE)) {
    write.csv(x = cast_df, file = output_file, row.names = FALSE)
  }
  
  if(grepl(pattern = ".rds", x = output_file, ignore.case = TRUE)) {
    saveRDS(object = cast_df, file = output_file)
  }
  
}



#' Write CTD data to a text file
#' 
#' Writes data to a csv file with metadata in a header.
#' 
#' @param x A data.frame with column names that correspond with names in column_descriptions
#' @param output_file Output filepath for text file.
#' @param column_descriptions A named character vector with descriptions of the values in each output column.
#' @param ctd_unit CTD instrument model (character vector)
#' @param auxiliary_sensors Names of auxiliary sensors, such as dissolved oxygen and pH sensors (character vector)
#' @param dataset_name Name of the data set (character vector)
#' @param cruise_name Name of the cruise (character vector)
#' @param creator_name Name of the person who created the data set (character vector)
#' @param creator_email Email address for the person who created the data set (character vector)
#' @param creator_institution Name of the institution for the cruise/data set creator (character vector)
#' @param collaborators Names of contributors/collaborators (character vector)
#' @param references Any relevant references for the data set (character vector)
#' @param dataset_doi Digital object identifier for the data set (character vector)
#' @param ncei_accession_number NCEI Accession Number for the data product (character or numeric vector)
#' @param processing_info Additional information about data processing, e.g. gapctd version used for processing (character vector)
#' @param publisher_url URL for the dataset creator, e.g., https://github.com/afsc-gap-products/gapctd (character vector)
#' @export
#' @author Sean Rohan

make_text_table <- function(x, 
                            output_file,
                            column_descriptions = c(
                              "vessel" = "vessel: AFSC/RACE vessel code",
                              "cruise" = "cruise: AFSC/RACE cruise code",
                              "haul" = "haul: AFSC/RACE haul number",
                              "stationid" = "stationid: AFSC/RACE station code",
                              "serial_number" = "serial_number: Primary instrument serial number",
                              "cast_direction" = "cast_directon: Cast direction",
                              "datetime" = "datetime: date and time in Alaska Daylight Time [UTC-9:00]",
                              "depth" = "depth: depth [m], down is positive",
                              "pressure" = "pressure: pressure, strain gauge [db]",
                              "conductivity" = "conductivity: conductivity [S/m]",
                              "temperature" = "temperature: temperature [ITS-90, degrees C]",
                              "salinity" = "salinity: salinity [PSS-78]",
                              "sound_speed" = "sound_speed: Chen-Millero sound speed [m/s]",
                              "oxygen" = "oxygen: dissolved oxygen [ml/l]",
                              "pH" = "ph: pH",
                              "flag" = "flag: data quality flag"
                            ), 
                            ctd_unit, 
                            auxiliary_sensors,
                            dataset_name, 
                            cruise_name, 
                            creator_name, 
                            creator_email,
                            creator_institution,
                            collaborators = NULL, 
                            references, 
                            dataset_doi, 
                            ncei_accession_number, 
                            processing_info, 
                            publisher_url) {
  
  stopifnot("make_text_table: One or more columns in x is missing a metadata description in column_descriptions." = all(names(x) %in% names(column_descriptions)))
  
  header_lines <- c(
    paste0("# Dataset name: ", dataset_name),
    paste0("# Cruise name: ", cruise_name),
    paste0("# Created by: ", creator_name, " <", creator_email, ">, ", paste(collaborators)),
    paste0("# Institution: ", creator_institution),
    paste0("# Creation date: ", as.POSIXct(Sys.time(), format = "%Y-%m-%d %H:%M:%S")),
    paste0("# Sampling instrument(s): ", ctd_unit),
    paste0("# Auxillary sensors: ", paste(auxiliary_sensors, collapse = ", ")),
    paste0("# Reference: ", references),
    paste0("# Dataset citation: ", dataset_doi),
    paste0("# NCEI Accession Number: ", ncei_accession_number),
    paste0("# Processing method: ", processing_info),
    paste0("# Website: ", publisher_url)
  )
  
  column_lines <- c("\n#\n# Fields:", paste0("# ", unname(column_descriptions[names(x)])))
  
  column_lines <- paste(column_lines, collapse = "\n")
  
  full_header <- c(paste(paste(header_lines, collapse = "\n"), column_lines, collapse = "\n"), "\n")
  
  message("make_text_table: Writing header to ", output_file, "\n")
  writeLines(full_header, con = output_file)
  
  length_header <- length(readLines(con = output_file))
  
  message("make_text_table: Writing ", nrow(x), " lines of data to ", output_file, "\n")
  suppressWarnings(write.table(x, file = output_file, append = TRUE, row.names = FALSE, sep = ",", quote = FALSE))
  
  # Checking output
  stopifnot("make_text_table: Error loading data " = nrow(read.csv(output_file, skip = length_header)) == nrow(x))
  stopifnot("make_text_table: Some columns in x not written to output. Check that column names in x match names in column_description." =
              all(names(x) %in% names(read.csv(output_file, skip = length_header, nrow = 1))))
  
}
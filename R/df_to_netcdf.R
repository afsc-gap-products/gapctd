#' Data frame to netCDF (.nc) file
#' 
#' Write 2D and/or 3D data from a data frame to a netCDF version-4 file using the RNetCDF package.
#' 
#' @param x A data frame containing data and metadata to add to write to a netCDF file.
#' @param dim_names_2d A vector of names of 2D spatial and time dimensions (e.g., LATITUDE, LONGITUDE, DATETIME).
#' @param dim_units_2d A vector of unit quantities for 2D dimensions.
#' @param var_names_2d A vector of variable names that for 2D variables (e.g., VESSEL, CRUISE, HAUL, CAST, STATIONID).
#' @param var_long_names_2d A vector of long_names for the 2D variable.
#' @param var_units_2d A vector of unit quantities for the 2D variables.
#' @param var_flag_values_2d Optional. A list of containing vectors of flag values for 2D flag variables where list item names correspond with variables in var_names_2d.
#' @param var_flag_meanings_2d Optional. A list of containing vectors of flag_meaning definitions for 2D flag variables where list item names correspond with variables in var_names_2d.
#' @param dim_names_3d A vector of names of 3D dimension (e.g., depth).
#' @param dim_long_names_3d A Vector of long_names of 3D dimension (e.g., "Depth in meters (positive down)").
#' @param dim_units_3d A vector of unit quantities for 3D dimensions.
#' @param var_names_3d A vector of variable names for 3D variables (e.g., TEMPERATURE, SALINITY).
#' @param var_units_3d A vector of unit quantities for the 3D variables (e.g., 'degrees Celsius', 'Practical Salinity').
#' @param var_flag_values_3d Optional. A list of containing vectors of flag values for 3D flag variables where list item names correspond with variables in var_names_3d.
#' @param var_flag_meanings_3d Optional. A list of containing vectors of flag_meaning definitions for 3D flag variables where list item names correspond with variables in var_names_3d.
#' @param instrument_attributes Optional. A vector of attribute names for the instrument variable.
#' @param instrument_values Optional. A list of attribute values for the instrument variable.
#' @param global_attributes A list of global attributes (i.e., non-dimensional) as 1L character, numeric, or date vectors, where names of list objects are the names of attributes.
#' @examples 
#' # Create an NC file using 3D data from a data frame (with missing values)
#' 
#' df_ctd <- data.frame(STATIONID = c(rep("A-02", 10), rep("X-03", 5)),
#' DEPTH = c(1:10, 1:4, 6),
#' CAST = c(rep("upcast", 10), rep("downcast", 5)),
#' TEMPERATURE = c(rep(6,4),3, rep(2,5), rep(5,4),4.5),
#' SALINITY = c(rep(32.1,4),32.3, rep(32.7,5), seq(30.4,30.8,0.1)),
#' DATETIME = c(rep(as.POSIXct("2021-06-05 13:15:21", tz = "America/Anchorage"), 10),
#'              rep(as.POSIXct("2021-08-21 06:12:05", tz = "America/Anchorage"), 5)),
#' VESSEL = c(rep(162,10), rep(94,5)),
#' CRUISE = c(rep(202101,10), rep(202102,5)),
#' HAUL = c(rep(10,10), rep(203,5)),
#' LAT = c(rep(57,10), rep(56.1,5)),
#' LON = c(rep(-163.2,10), rep(-162,5)))
#' 
#' gapctd::df_to_ncdf(x = df_ctd,
#' output_filename = "ex_3d.nc",
#' dim_names_2d = c("LAT", "LON"), #, "DATETIME")
#' dim_units_2d = c("decimal degrees north", "decimal degrees east"), #, "seconds elapsed since 1970-01-01 GMT")
#' var_names_2d = c("STATIONID", "CAST", "VESSEL", "CRUISE", "HAUL"),
#' var_units_2d = c("Station ID", "Cast direction", "Vessel number", "Cruise number", "Haul number"),
#' dim_names_3d = c("DEPTH"),
#' dim_units_3d = c("meters"),
#' dim_sort_3d = c(TRUE),
#' var_names_3d = c("TEMPERATURE", "SALINITY"),
#' var_units_3d = c("Degrees celcius", "Practical salinity units"),
#' global_attributes = list(CITATION = "Doe (2022)",
#'                       DATE_CREATED = as.character(Sys.Date())))
#'                       
#'                       
#' # Create an NC file using only the 2D data from a data frame               
#' gapctd::df_to_ncdf(x = df_ctd,
#' output_filename = "ex_2d.nc",
#' dim_names_2d = c("LAT", "LON"), #, "DATETIME")
#' dim_units_2d = c("decimal degrees north", "decimal degrees east"), #, "seconds elapsed since 1970-01-01 GMT")
#' var_names_2d = c("STATIONID", "CAST", "VESSEL", "CRUISE", "HAUL"),
#' var_units_2d = c("Station ID", "Cast direction", "Vessel number", "Cruise number", "Haul number"),
#' global_attributes = list(CITATION = "Doe (2022)",
#'                          DATE_CREATED = as.character(Sys.Date())))
#' @export

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
                       dim_sort_3d,
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
    stop(paste0("df_to_ncdf: var_names_2d variables (", var_names_2d[which(!(var_names_2d %in% names(x)))], ") not found in the input data frame."))
  }
  
  if(!all(dim_names_3d %in% names(x))) {
    stop(paste0("df_to_ncdf: dim_names_3d variables (", dim_names_3d[which(!(dim_names_3d %in% names(x)))], ") not found in the input data frame."))
  }
  
  if(!all(var_names_3d %in% names(x))) {
    stop(paste0("df_to_ncdf: var_names_3d variables (", var_names_3d[which(!(var_names_3d %in% names(x)))], ") not found in the input data frame."))
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
  
  message(paste0("Adding index for ", length(unique(x$index)), " samples/locations."))
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
      
      message(paste0("Adding 3D dimension ", dim_names_3d[ii], " with units ", dim_units_3d[ii]))
      
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
    
    message(paste0("Adding 2D variable ", combined_var_dim_names_2d[jj], " with units ", combined_var_dim_units_2d[jj]))
    
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
      
      message(paste0("Adding 3D variable ", var_names_3d[kk], " with units ", var_units_3d[kk]))
      
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
        message(paste0("val_matrix class", print(class(val_matrix)), "not supported. Returning object for inspection."))
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
    message("Adding instrument attributes")
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
    message(c("Adding global attribute ", names(global_attributes)[mm]))
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
#' @export

vec_to_nc_class <- function(vec, pkg = "RNetCDF") {
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

#' Make netCDF from profiles and metadata 
#' 
#' Read-in files from /accepted_profiles/ and /metadata/ subdirectories and create a netCDF file. Combines data from multiple vessels from the same cruise. Must provide the following global_attributes to the function in a list: references, id, cdm_data_type, cruise, institution, contributor_name, creator_name, creator_institution, creator_email,publisher, publisher_type, publisher_url, geospatial_bounds_crs, license, metadata_link, instrument, standard_name_vocabulary, Conventions, source.
#' 
#' @param fpath Path to accepted profiles.
#' @param metadata_path Path to metadata files.
#' @param global_attributes List of global attributes that is passed to gapctd::df_to_netcdf(global_attributes).
#' @export

make_ctd_ncdf <- function(fpath = c(list.files(path = here::here("output", "accepted_profiles"), full.names = TRUE)),
                          metadata_path = c(list.files(path = here::here("metadata"), full.names = TRUE, full.names = TRUE)), 
                          output_file = "output.nc",
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
                                                   source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd")))) {
  
  req_attributes <- c("title", "references", "id", "cdm_data_type", "cruise", "institution", "contributor_name",
                      "creator_name", "creator_institution", "creator_email","publisher", "publisher_type", 
                      "publisher_url", "geospatial_bounds_crs", "license", "metadata_link", "instrument", "standard_name_vocabulary", "Conventions", "source")
  
  if(!all(req_attributes %in% names(global_attributes))) {
    stop(paste0("make_ctd_ncdf: The follow required global attribute(s) were not found in global_attributes : ", req_attributes[which(!(req_attributes %in% names(global_attributes)))]))
  }

  metadata_df <- data.frame()
  all_profiles <- data.frame()
  
  for(ii in metadata_path) {
    metadata_df <- dplyr::bind_rows(metadata_df,
                                    read.csv(file = ii))
  }
  
  metadata_df$deploy_id <- gsub(pattern = "_raw.*", "", x = metadata_df$cnv_file_name)
  
  
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

  for(jj in 1:length(fpath)) {
    
    new_profile <- read.csv(file = fpath[jj])
    
    if(any(table(new_profile$depth) > 1)) {
      # Corner case. Not sure why this happens but it likely has to do with some issue that can occur with the identify function.
      stop(paste0("make_ctd_ncdf: Duplicate depth values found in ", fpath[jj], ". Please remove duplicates from the file then rerun make_ctd_ncdf()."))
    }
    
    if(jj > 1) {
      if(new_profile$deploy_id[1] %in% all_profiles$deploy_id) {
        stop(paste0("make_ctd_ncdf: ", fpath[jj], " cannot be added to profile_df because ", new_profile$deploy_id[1], " has already been added. Please check that deploy_id values are unique for each profile (i.e. no two hex/cnv files can have the same file name)."))
      }
    }
    
    profile_df <- dplyr::inner_join(new_profile, metadata_df, by = "deploy_id")
    
    all_profiles <- dplyr::bind_rows(all_profiles, profile_df)
  }
  
  
  # Convert times to UTC
  all_profiles$CASTTIME <- as.POSIXct(all_profiles$CASTTIME, tz = "America/Anchorage")
  all_profiles$CASTTIME <- lubridate::with_tz(all_profiles$CASTTIME, tz = "UTC")
  
  
  # Define temporal coverage
  time_coverage <- paste0(as.character(range(all_profiles$CASTTIME)), " UTC")
  
  
  # Convert time to character for netCDF NC_STRING format
  all_profiles$CASTTIME <- as.character(all_profiles$CASTTIME)
  
  # Rename columns to match CF naming conventions
  names(all_profiles) <- tolower(names(all_profiles))
  
  all_profiles <- all_profiles |>
    dplyr::rename(time = casttime,
                  profile = deploy_id,
                  stationid = stationid,
                  vessel = vessel,
                  cruise = cruise,
                  haul = haul,
                  haul_depth = ctd_mean_haul_depth,
                  sea_floor_temperature = ctd_mean_bottom_temperature_c,
                  sea_floor_salinity = ctd_mean_bottom_salinity_sa,
                  sea_floor_practical_salinity = ctd_mean_bottom_salinity_sp,
                  sea_floor_sound_speed_in_sea_water = ctd_mean_bottom_soundspeed,
                  time_elapsed = times,
                  sea_water_pressure = pressure,
                  sea_water_temperature = temperature,
                  sea_water_practical_salinity = salinity,
                  sea_water_salinity = gsw_saa0,
                  sea_water_density = gsw_densitya0,
                  sea_water_electrical_conductivity = conductivity,
                  sound_speed_in_sea_water = soundspeed,
                  buoyancy_frequency = n2,
                  quality_flag = flag)
  
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
  
  
  # Create netCDF file
  gapctd::df_to_ncdf(x = all_profiles,
                     output_filename = output_file,
                     dim_names_2d = c("latitude", "longitude", "time"),
                     dim_units_2d = c("degree_north", "degree_east", "time"),
                     dim_long_names_2d = c("Latitude (decimal degrees)", "Longitude (decimal degrees)", "Time in Coordinated Universal Time (UTC)"),
                     var_names_2d = c("stationid", "profile", "vessel", "cruise", "haul", "haul_depth", "sea_floor_temperature", "sea_floor_practical_salinity", "sea_floor_salinity", "sea_floor_sound_speed_in_sea_water"),
                     var_long_names_2d = c("AFSC/RACE/GAP Survey Station Name", "Profile Number and Direction", "AFSC/RACE/GAP Vessel Code", "AFSC/RACE/GAP Cruise Code", "Haul Number", "Mean towed depth of CTD during haul", "Mean bottom temperature (ITS-90) at towed depth", "Mean Practical Salinity (PSS-78) at towed depth", "Mean Absolute Salinity (TEOS-10 GSW) at towed depth", "Mean speed of sound during haul (Chen-Millero)"),
                     var_units_2d = c("1", "1", "1", "1", "1", "m", "degree_C", "1", "g kg-1", "m s-1"),
                     dim_names_3d = c("depth"),
                     dim_long_names_3d = c("Depth in meters"),
                     dim_positive_3d = list("depth" = "down"),
                     dim_units_3d = c("m"),
                     dim_sort_3d = c(TRUE),
                     var_names_3d = c("time_elapsed", "sea_water_pressure", "sea_water_temperature", "sea_water_practical_salinity", "sea_water_salinity", "sea_water_density", "buoyancy_frequency", "sea_water_electrical_conductivity", "sound_speed_in_sea_water", "quality_flag"),
                     var_long_names_3d = c("Time Elapsed Since Start of Deployment", "Strain Gauge Pressure", "ITS-90 Temperature", "PSS-78 Practical Salinity", "TEOS-10 GSW Absolute Salinity", "TEOS-10 GSW Density", "Squared Brunt-Vaisala Buoyancy Frequency", "Electrical Conductivity", "Speed of Sound (Chen-Millero)", "Data Quality Assurance Flag"),
                     var_flag_values_3d = list("quality_flag" = c(-6,0,6,7)),
                     var_flag_meanings_3d = list("quality_flag" = c("Interpolated value. Density inversion error detected based on buoyancy frequency and remains after automatic removal and interpolation of point.",
                                                               "Good quality, no issues detected.",
                                                               "Interpolated value. Density inversion error detected based on buoyancy frequency and corrected using automatic removal and interpolation of point.",
                                                               "Interpolated value. Point mannually flagged and removed during visual inspection then estimated through interpolation.")),
                     var_units_3d = c("s", "dbar", "degree_C", "1", "g kg-1", "kg m-3", "s-2", "S m-1)", "m s-1", "1"),
                     instrument_attributes = c("make_model", "serial_number", "calibration_date", "vessel"),
                     instrument_values = list(make_model = "Sea-Bird SBE19plus V2",
                                              serial_number = instrument_df$ctd_serial_number,
                                              calibration_date = instrument_df$ctd_calibration_date,
                                              vessel = instrument_df$vessel),
                     global_attributes = g_attributes)
  
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
#' @noRd

calc_bottom_mean_oce <- function(x, 
                       prefix = "mean_bottom_", 
                       fun = mean, 
                       vars = NULL) {
  
  if(is.null(vars)) {
    vars <- c("temperature", "salinity", "absolute_salinity", "sound_speed", "density")
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
#' @noRd

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
#' @param fpath Path to accepted profiles.
#' @param metadata_path Path to metadata files.
#' @param output_file Output filepath for netCDF file.
#' @param global_attributes List of global attributes that is passed to gapctd::df_to_netcdf(global_attributes).
#' @export

make_oce_ncdf <- function(fpath = c(list.files(path = here::here("output", processing_method), full.names = TRUE, pattern = "qc.rds")),
                          metadata_path = c(list.files(path = here::here("metadata"), full.names = TRUE)), 
                          output_file = "output.nc",
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
                                                   source = paste0("CTD data processed using gapctd ", packageVersion(pkg = "gapctd")))) {
  
  req_attributes <- c("title", "references", "id", "cdm_data_type", "cruise", "institution", "contributor_name",
                      "creator_name", "creator_institution", "creator_email","publisher", "publisher_type", 
                      "publisher_url", "geospatial_bounds_crs", "license", "metadata_link", "instrument", "standard_name_vocabulary", "Conventions", "source")
  
  if(!all(req_attributes %in% names(global_attributes))) {
    stop(paste0("make_ctd_ncdf: The follow required global attribute(s) were not found in global_attributes : ", req_attributes[which(!(req_attributes %in% names(global_attributes)))]))
  }
  
  metadata_df <- data.frame()
  all_profiles <- data.frame()
  
  for(ii in metadata_path) {
    metadata_df <- dplyr::bind_rows(metadata_df,
                                    readRDS(file = ii))
  }
  
  # metadata_df$deploy_id <- gsub(pattern = "_raw.*", "", x = metadata_df$cnv_file_name)
  
  
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
  
  for(jj in 1:length(fpath)) {
    
    deployment_dat <- readRDS(file = fpath[jj])
    
    cast_dir <- c("downcast", "upcast")[c("downcast", "upcast") %in% names(deployment_dat)]
    
    for(kk in 1:length(cast_dir)) {
      new_profile <- deployment_dat[[which(names(deployment_dat) == cast_dir[kk])]]
      
      new_profile@metadata$race_metadata$deploy_id <- paste0(new_profile@metadata$race_metadata$deploy_id, "_", match.arg(cast_dir[kk], c("downcast", "upcast")))
      
      if(any(table(new_profile@data$depth) > 1)) {
        # Corner case. Not sure why this happens but it likely has to do with some issue that can occur with the identify function.
        stop(paste0("make_ctd_ncdf: Duplicate depth values found in ", fpath[jj], ". Please remove duplicates from the file then rerun make_ctd_ncdf()."))
      }
      
      if(jj > 1) {
        if(new_profile@metadata$race_metadata$deploy_id[1] %in% all_profiles$deploy_id) {
          stop(paste0("make_ctd_ncdf: ", fpath[jj], " cannot be added to profile_df because ", new_profile@metadata$race_metadata$deploy_id[1], " has already been added. Please check that deploy_id values are unique for each profile (i.e. no two hex/cnv files can have the same file name)."))
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
  convert_index <- which(!(unlist(sapply(all_profiles, FUN = function(x) class(x)[1])) %in% c("numeric", "integer", "character", "logical", "matrix", "factor")))
  
  message(paste0("make_oce_ncdf: Converting ", paste(names(all_profiles)[convert_index], sep = ", ") ," to character." ))
  for(ii in convert_index) {
    all_profiles[, ii] <- as.character(all_profiles[, ii])
  }
  
  
  # Create netCDF file
  gapctd::df_to_ncdf(x = all_profiles,
                     output_filename = output_file,
                     dim_names_2d = c("latitude", "longitude", "time"),
                     dim_units_2d = c("degree_north", "degree_east", "time"),
                     dim_long_names_2d = c("Latitude (decimal degrees)", "Longitude (decimal degrees)", "Time in Coordinated Universal Time (UTC)"),
                     var_names_2d = c("stationid", "profile", "vessel", "cruise", "haul", "haul_depth", "sea_floor_temperature", "sea_floor_practical_salinity", "sea_floor_salinity", "sea_floor_sound_speed_in_sea_water"),
                     var_long_names_2d = c("AFSC/RACE/GAP Survey Station Name", "Profile Number and Direction", "AFSC/RACE/GAP Vessel Code", "AFSC/RACE/GAP Cruise Code", "Haul Number", "Mean towed depth of CTD during haul", "Mean bottom temperature (ITS-90) at towed depth", "Mean Practical Salinity (PSS-78) at towed depth", "Mean Absolute Salinity (TEOS-10 GSW) at towed depth", "Mean speed of sound during haul (Chen-Millero)"),
                     var_units_2d = c("1", "1", "1", "1", "1", "m", "degree_C", "1", "g kg-1", "m s-1"),
                     dim_names_3d = c("depth"),
                     dim_long_names_3d = c("Depth in meters"),
                     dim_positive_3d = list("depth" = "down"),
                     dim_units_3d = c("m"),
                     dim_sort_3d = c(TRUE),
                     var_names_3d = c("time_elapsed", "sea_water_pressure", "sea_water_temperature", "sea_water_practical_salinity", "sea_water_salinity", "sea_water_density", "buoyancy_frequency", "sea_water_electrical_conductivity", "sound_speed_in_sea_water", "quality_flag"),
                     var_long_names_3d = c("Time Elapsed Since Start of Deployment", "Strain Gauge Pressure", "ITS-90 Temperature", "PSS-78 Practical Salinity", "TEOS-10 GSW Absolute Salinity", "TEOS-10 GSW Density", "Squared Brunt-Vaisala Buoyancy Frequency", "Electrical Conductivity", "Speed of Sound (Chen-Millero)", "Data Quality Assurance Flag"),
                     var_flag_values_3d = list("quality_flag" = c(-6,0,6,7)),
                     var_flag_meanings_3d = list("quality_flag" = c("Interpolated value. Density inversion error detected based on buoyancy frequency and remains after automatic removal and interpolation of point.",
                                                                    "Good quality, no issues detected.",
                                                                    "Interpolated value. Density inversion error detected based on buoyancy frequency and corrected using automatic removal and interpolation of point.",
                                                                    "Interpolated value. Point mannually flagged and removed during visual inspection then estimated through interpolation.")),
                     var_units_3d = c("s", "dbar", "degree_C", "1", "g kg-1", "kg m-3", "s-2", "S m-1)", "m s-1", "1"),
                     instrument_attributes = c("make_model", "serial_number", "calibration_date", "vessel"),
                     instrument_values = list(make_model = "Sea-Bird SBE19plus V2",
                                              serial_number = instrument_df$ctd_serial_number,
                                              calibration_date = instrument_df$ctd_calibration_date,
                                              vessel = instrument_df$vessel),
                     global_attributes = g_attributes)
  
}
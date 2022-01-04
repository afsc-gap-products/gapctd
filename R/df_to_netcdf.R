#' Data frame to netCDF (.nc) file
#' 
#' Write data from a data frame to a netCDF file.
#' 
#' @param x A data frame containing gridded data and metadata to add to write to a netCDF file.
#' @param dim_names_2d A vector of names of 2D spatial and time dimensions (e.g., LATITUDE, LONGITUDE, DATETIME).
#' @param dim_units_2d A vector of unit quantities for 2D dimensions.
#' @param var_names_2d A vector of variable names that for 2D variables (e.g., VESSEL, CRUISE, HAUL, CAST, STATIONID).
#' @param var_units_2d A vector of unit quantities for the 2D variables.
#' @param dim_names_3d A vector of names of 3D dimension (e.g., DEPTH).
#' @param dim_units_3d A vector of unit quantities for 3D dimensions.
#' @param var_names_3d A vector of variable names for 3D variables (e.g., TEMPERATURE, SALINITY).
#' @param var_units_3d A vector of unit quantities for the 3D variables (e.g., 'degrees Celcius', 'Practical Salinity').
#' @param global_attributes A list of global attributes (i.e., non-dimensional) as 1L character, numeric, or date vectors, where names of list objects are the names of attributes.
#' @export

df_to_ncdf <- function(x,
                       output_filename,
                       dim_names_2d,
                       dim_units_2d,
                       var_names_2d,
                       var_units_2d,
                       dim_names_3d = NULL,
                       dim_units_3d = NULL,
                       dim_sort_3d,
                       var_names_3d = NULL,
                       var_units_3d = NULL,
                       global_attributes) {
  
  # Assign index value to each cast
  x <- x |> 
    dplyr::select(match(var_names_2d, names(x))) |>
    unique() |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::inner_join(x)
  
  # Dimensions for netCDF files
  dim_xy <- length(unique(x$LAT))
  dim_z <- length(unique(x$DEPTH))
  dim_t <- length(unique(x$DATETIME))
  
  # Create netCDF4 file
  ncout <- RNetCDF::create.nc(filename = output_filename,
                              format = "netcdf4")
  # Define dimensions (index)
  RNetCDF::dim.def.nc(ncfile = ncout,
                      dimname = "INDEX", 
                      dimlength = length(unique(x$index)))
  RNetCDF::var.def.nc(ncfile = ncout, 
                      varname = "INDEX", 
                      vartype = "NC_INT", 
                      dimensions = "INDEX")
  RNetCDF::var.put.nc(ncfile = ncout, 
                      variable = "INDEX",
                      data = unique(x$index))
  
  if(!is.null(var_names_3d)) {
    
    for(ii in 1:length(dim_names_3d)) {
      
      dim_vals <- unique(x[,match(x = dim_names_3d[ii], names(x))])
      
      if(dim_sort_3d[ii]) {
        dim_vals <- sort(dim_vals)
      }
      
      print(paste0("Adding 2D dimension ", dim_names_3d[ii], " with units ", dim_units_3d[ii]))
      
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
                          name = "Units",
                          type = "NC_STRING",
                          value = dim_units_3d[ii])
    }
  }
  
  # Define 2D dimensions that are indexed to hauls or casts
  combined_var_dim_names_2d <- c(dim_names_2d, var_names_2d)
  combined_var_dim_units_2d <- c(dim_units_2d, var_units_2d)
  
  for(jj in 1:length(combined_var_dim_names_2d)) {
    
    print(paste0("Adding 2D variable ", combined_var_dim_names_2d[jj], " with units ", combined_var_dim_units_2d[jj]))
    
    var_vals <- (x |> dplyr::select(match(x = c("index", 
                                                combined_var_dim_names_2d[jj]), 
                                          names(x))) |>
                   unique())[,2]
    
    RNetCDF::var.def.nc(ncfile = ncout, 
                        varname = combined_var_dim_names_2d[jj], 
                        vartype = vec_to_nc_class(vec = var_vals), 
                        dimensions = "INDEX")
    RNetCDF::var.put.nc(ncfile = ncout, 
                        variable = combined_var_dim_names_2d[jj],
                        data = var_vals)
    RNetCDF::att.put.nc(ncfile = ncout, 
                        variable = combined_var_dim_names_2d[jj],
                        name = "Units",
                        type = "NC_STRING",
                        value = combined_var_dim_units_2d[jj])
  }
  
  if(!is.null(var_names_3d)) {
    
    # Define 3D variables and assign to list
    for(kk in 1:length(var_names_3d)) {
      
      print(paste0("Adding 3D variable ", var_names_3d[kk], " with units ", var_units_3d[kk]))
      
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
      
      RNetCDF::var.def.nc(ncfile = ncout, 
                          varname = var_names_3d[kk], 
                          vartype = vec_to_nc_class(vec = val_matrix[!is.na(val_matrix)]), 
                          dimensions = c(dim_names_3d, "INDEX"))
      RNetCDF::var.put.nc(ncfile = ncout, 
                          variable = var_names_3d[kk],
                          data = val_matrix)
      RNetCDF::att.put.nc(ncfile = ncout, 
                          variable = var_names_3d[kk],
                          name = "Units",
                          type = "NC_STRING",
                          value = var_units_3d[kk])
    }
  }
  
  # Add global attributes to NetCDF object
  for(mm in 1:length(global_attributes)) {
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
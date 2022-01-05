#' Data frame to netCDF (.nc) file
#' 
#' Write 2D and/or 3D data from a data frame to a netCDF version-4 file using the RNetCDF package.
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
      
      print(paste0("Adding 3D dimension ", dim_names_3d[ii], " with units ", dim_units_3d[ii]))
      
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
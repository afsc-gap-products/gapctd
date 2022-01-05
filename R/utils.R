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
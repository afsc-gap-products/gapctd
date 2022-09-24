#' Correlation between two channels (R workflow)
#' 
#' Calculate correlation coefficient between two channels (e.g. temperature and conductivity)
#' 
#' @param x oce object
#' @param exclude_flag Logical denoting whether bad scans (flag < 0) should be excluded.
#' @param c1 Name of first channel as a character vector. Default = "temperature"
#' @param c2 Name of second channel as a character vector. Default = "conductivity"
#' @param min_pressure Minimum pressure to use for correlations
#' @return Correlation coefficient (1L numeric)
#' @export

channel_correlation <- function(x, exclude_flag = TRUE, c1 = "temperature", c2 = "conductivity", min_pressure = 4, cor_method = "pearson") {
  
  if(exclude_flag) {
    T_vec <- x@data[[c1]][x@data$flag >= 0 & x@data$pressure >= min_pressure]
    C_vec <- x@data[[c2]][x@data$flag >= 0 & x@data$pressure >= min_pressure]
  }
  
  if(!exclude_flag) {
    T_vec <- x@data[[c1]][x@data$pressure >= min_pressure]
    C_vec <- x@data[[c2]][x@data$pressure >= min_pressure]
  }
  
  cor_coef <- try(cor(T_vec,
                      C_vec,
                      use = "complete.obs",
                      method = cor_method), silent = TRUE)
  
  if(class(cor_coef) == "try-error") {
    cor_coef <- 0
  }
  
  return(cor_coef)
}


#' Estimate channel alignment parameter (R workflow)
#'
#' Estimate optimal alignment parameter for a channel (e.g. "temperature") within a range of candidate alignment offsets. The 'best' alignment is taken to be the one that maximizes the correlation between channels.
#' 
#' @param x oce object
#' @param cast_direction Cast direction as a character vector ("downcast" or "upcast"). Passed to gapctd::loop_edit()
#' @param variable Variable to be aligned ("temperature")
#' @param offsets Numerical vector of offsets to evaluate, in seconds.
#' @param cor_method Correlation method, passed to gapctd::channel_correlation().
#' @param ... Additional arguments passed to gapctd::channel_correlation()
#' @return A named numeric vector containing the estimated optimal offset ("best_offset") and correlation for the offset ("best_corr")
#' @export

optim_align_par <- function(x, cast_direction, variable = "temperature", offsets = seq(-1,1, 0.01), cor_method = "pearson", ...) {

  offset_corr <- numeric(length = length(offsets))
  
  for(ii in 1:length(offsets)) {
    
    if(variable == "temperature") {
      offset_corr[ii] <- x |>
        gapctd:::align_var(variables = variable, offset = offsets[ii], interp_method = "linear") |>
        gapctd:::loop_edit(min_speed = 0.1, window = 5, cast_direction = cast_direction) |>
        gapctd:::channel_correlation(exclude_flag = TRUE, 
                                     min_pressure = 4, 
                                     cor_method = cor_method, 
                                     c1 = "temperature", 
                                     c2 = "conductivity")
      
      x@data["temperature"]
    }
    
    if(variable == "oxygen") {
      
    }

  }
  
  out <- c(best_offset = offsets[which.max(offset_corr)[1]],
           best_corr = offset_corr[which.max(offset_corr)[1]])
  return(out)
  
}
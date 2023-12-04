#' Window filter
#' 
#' Applies a window filter to variable channels using a user-specified filtering method.
#' 
#' @param x ctd object
#' @param method Filtering method options are "median", "boxcar", "cosine", "gaussian", "triangle"
#' @param variables Character vector of data variable names to filter, such as "temperature", "conductivity".
#' @param window Numeric vector of windows (i.e. number of scans for the window
#' @param offset For "gaussian" filtering. Value of the offset parameter. SBEDP uses as defaults: offset = 0, halfwidth = 1
#' @param halfwidth For "gaussian" filter. Value of the halfwidth parameter.
#' @param freq_n For "gaussian" filter. Optional. Sampling interval in seconds (1L numeric). Sampling interval is inferred from timeS if not provided.
#' @return ctd object with channels filtered
#' @export
#' @author Sean Rohan
#' @importFrom stats filter

window_filter <- function(x, method, variables, window, offset = NULL, halfwidth = NULL, freq_n = NULL) {
  
  if(is.null(x)) {
    return(x)
  }
  
  method <- tolower(method)
  
  stopifnot("window_filter: Invalid method. Must be one of 'median', 'boxcar', 'cosine', 'gaussian', 'triangle'" = method %in% c("median", "boxcar", "cosine", "gaussian", "triangle"))
  
  stopifnot("window_filter: variable and window arguments must have the same length" = length(variables) == length(window))
  
  stopifnot("window_filter: Invalid window, window values must be odd." = all(window %% 2 == 1))
  
  stopifnot("window_filter: Invalid window, window values must be integers." = all(window %% 1 == 0))
  
  stopifnot("window_filter: Invalid window, window values must be odd integers from 1 and 511" = all(window >= 1 & window <= 511))
  
  stopifnot("window_filter: Must provide offset and halfwidth parameters when method = 'gaussian'" =  ifelse(method == "gaussian", !is.null(offset) & !is.null(halfwidth), TRUE))
  
  stopifnot("window_filter: x@data must include scan time in seconds (timeS) when method = 'gaussian'" =  ifelse(method == "gaussian", "timeS" %in% names(x@data), TRUE))
  
  if(filter == "median") {
    return(median_filter(x = x, variables = variables, window = window))
  }
  
  for(ii in 1:length(variables)) {
    
    half_window <- (window[ii] - 1) / 2
    
    if(method == "boxcar") {
      weights <- rep(1 / window[ii], window[ii])
    }
    
    if(method == "cosine") {
      weights <- cos((-1*half_window):(half_window)*pi/(window[ii]+1))
    }
    
    if(method == "triangle") {
      K_par <- (window[ii] - 1) / 2 + 1
      weights <- abs(c((-1*half_window):-1, K_par, 1:half_window)) / K_par
    }
    
    if(method == "gaussian") {
      
      if(is.null(freq_n)) {
        freq_n  <- table(diff(x@data[["timeS"]]))
        freq_n  <- as.numeric(names(freq_n)[freq_n == max(freq_n)])
        print(freq_n)
      }
      
      phase <- offset / freq_n
      
      scale_par <- log(2) * (2 * freq_n / halfwidth ) ^ 2
      
      weights <- c(exp(-1*(((-1*half_window):-1)-phase)^2*scale_par),
                   exp(-1*phase*phase*scale_par),
                   exp(-1*((1:half_window)-phase)^2*scale_par)
      )
      
    }
    
    weights <- weights/sum(weights)
    
    var_vec <- x@data[[match(variables[ii], names(x@data))]]
    
    decimal_digits <- find_decimal_digits(var_vec)
    
    var_out <- stats::filter(x = var_vec, 
                             filter = weights, 
                             sides = 2, 
                             method = "convolution")
    
    var_out <- c(var_vec[1:half_window],
                 var_out[(half_window + 1):(length(var_out) - half_window)],
                 var_vec[(length(var_vec) - half_window + 1):length(var_vec)])
    
    var_out <- plyr::round_any(x = var_out, 
                               accuracy = 10^(-1*decimal_digits),
                               f = round)
    
    x@data[[match(variables[ii], names(x@data))]] <- var_out
    
  }
  
  return(x)
  
}



#' Maximum of decimal digits in a numeric vector
#' 
#' Does not work for scientific notation
#' 
#' @param x A numeric vector
#' @noRd

find_decimal_digits <- function(x) {
  
  x <- as.character(x)
  
  decimal_digits <- regmatches(x, gregexpr("\\.(\\d+)", x, perl = TRUE))
  
  count_digits <- function(num) {
    num <- gsub("[^0-9]", "", num)
    nchar(num)
  }
  
  counts <- sapply(decimal_digits, 
                   function(x) {
                     if (length(x) == 0) {
                       return(0)
                     }
                     return(max(sapply(x, count_digits), na.rm = TRUE))
                   })
  
  return(max(counts, na.rm = TRUE))
}

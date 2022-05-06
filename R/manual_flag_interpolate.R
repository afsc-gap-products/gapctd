#' Manually flag and interpolate bad data
#' 
#' Visually inspect plots of depth versus salinity, and depth versus pressure. Flag data for removal in a GUI and interpolate missing data. Write interpolated data to output/manual_flag
#' 
#' @param csv_paths Path to csv files to be flagged and reviewed.
#' @export

manual_flag_interpolate <- function(csv_paths = NULL, review = c("density")) {
  
  if(is.null(csv_paths)) {
    csv_paths <- list.files(here::here("output", "density_corrected"), full.names = TRUE)
  }
  
  interp_flags <- function(x, flags) {
    if(length(flags) > 0) {
      x$temperature[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                              y = x$temperature[-flags], 
                                              xout = x$depth[flags], 
                                              method = "unesco")
      x$conductivity[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                               y = x$conductivity[-flags], 
                                               xout = x$depth[flags], 
                                               method = "unesco")
      x$salinity[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                           y = x$salinity[-flags], 
                                           xout = x$depth[flags], 
                                           method = "unesco")
      x$soundSpeed[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                             y = x$soundSpeed[-flags], 
                                             xout = x$depth[flags], 
                                             method = "unesco")
      x$gsw_saA0[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                           y = x$gsw_saA0[-flags], 
                                           xout = x$depth[flags], 
                                           method = "unesco")
      x$gsw_densityA0[flags] <- oce::oce.approx(x = x$depth[-flags], 
                                                y = x$gsw_densityA0[-flags], 
                                                xout = x$depth[flags], 
                                                method = "unesco")
      ctd_obj <- as.ctd(salinity = x$salinity,
             temperature = x$temperature,
             pressure = x$pressure,
             conductivity = x$conductivity)
      
      x$N2 <- oce::swN2(ctd_obj)
    }
    return(x)
  }
  
  for(kk in 1:length(csv_paths)) {
    
    dat <- read.csv(file = csv_paths[kk])
    
    dat <- dat |>
      dplyr::arrange(pressure)
    
    if(!file.exists(here::here("output", "manual_flag", paste0(dat$deploy_id[1], ".csv")))) {
      
      message(dat$deploy_id[1])
      
      if("density" %in% review) {
        
        loop_ind <- 1
        while(loop_ind == 1) {
          
          par(mfrow = c(1,3))
          plot(dat$temperature, -1*dat$pressure, 
               xlab = "Temperature", 
               ylab = "Pressure",
               main = " ",
               col = "red")
          lines(x = dat$temperature, 
                y = -1*dat$pressure,
                col = "red")
          plot(dat$salinity, -1*dat$pressure, 
               xlab = "Salinity", 
               ylab = "Pressure",
               main = " ",
               col = "darkgreen")
          lines(x = dat$salinity, 
                y = -1*dat$pressure,
                col = "darkgreen")
          plot(x = dat$gsw_densityA0, 
               y = -1*dat$pressure, 
               xlab = "Density", 
               ylab = "Pressure",
               main = "Left-click on points to be removed then press 'Esc'",
               col = "blue")
          lines(x = dat$gsw_densityA0, 
                y = -1*dat$pressure,
                col = "blue")
          
          new_flags <- identify(dat$gsw_densityA0, -1*dat$pressure)
          
          
          if(!(length(new_flags) > 0)) {
            loop_ind <- 0
            next
          }
          
          dat$flag[new_flags] <- 7
          dat <- interp_flags(x = dat, flags = new_flags)
        }
      }
      
      if("temperature" %in% review) {
        
        loop_ind <- 1
        while(loop_ind == 1) {
          
          par(mfrow = c(1,2))
          plot(diff(dat$temperature)/diff(dat$pressure),
               y = (nrow(dat)-1):1,
               type = 'p',
               xlab = expression(d*"T"/d*rho),
               ylab = "Index")
          lines(diff(dat$temperature)/diff(dat$pressure),
                y = (nrow(dat)-1):1)
          plot(dat$temperature, -1*dat$pressure,
               xlab = "Temperature",
               ylab = "Pressure",
               main = "Left-click on points to be removed then press 'Esc'")
          lines(x = dat$temperature, y = -1*dat$pressure)
          
          new_flags <- identify(dat$temperature, -1*dat$pressure)
          
          
          if(!(length(new_flags) > 0)) {
            loop_ind <- 0
            next
          }
          
          dat$flag[new_flags] <- 7
          dat <- interp_flags(x = dat, flags = new_flags)
        }
      }
      
      if("salinity" %in% review) {
        loop_ind <- 1
        while(loop_ind == 1) {
          
          par(mfrow = c(1,2))
          plot(diff(dat$salinity)/diff(dat$pressure),
               y = (nrow(dat)-1):1,
               type = 'p',
               xlab = expression(d*S/d*rho),
               ylab = "Index")
          lines(diff(dat$salinity)/diff(dat$pressure),
                y = (nrow(dat)-1):1,)
          plot(dat$salinity, -1*dat$pressure,
               xlab = "Salinity",
               ylab = "Pressure",
               main = "Left-click on points to be removed then press 'Esc'")
          lines(x = dat$salinity, y = -1*dat$pressure)
          
          new_flags <- identify(dat$salinity, -1*dat$pressure)
          
          if(!(length(new_flags) > 0)) {
            loop_ind <- 0
            next
          }
          
          dat$flag[new_flags] <- 7
          dat <- interp_flags(x = dat, flags = new_flags)
          
        }
      }
      
      write.csv(x = dat, 
                file = here::here("output", "manual_flag", paste0(dat$deploy_id[1], ".csv")), 
                row.names = FALSE)
      
      review_next <- tolower(readline(prompt = "Review next? (y or n): ")) == "y"
      print(review_next)
      
      if(!review_next) {
        stop("Review stopped")
      }
    } else {
      message(paste0("Skipping ", dat$deploy_id[1]))
    }
  }
}
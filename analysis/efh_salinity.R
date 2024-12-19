make_pre_2020_summary <- function() {
  
  nc_files <- list.files(system.file("extdata", "nc", package = "gapctd"), pattern = "Bottom_Trawl_Survey_CTD", full.names = TRUE)
  
  bottom_dat <- data.frame()
  
  for(ii in 1:length(nc_files)) {
    nc <- RNetCDF::open.nc(con = nc_files[ii])
    
    if(ii > 3) {
      start_date <- as.POSIXct(as.Date(gsub(pattern = "Since ", replacement = "", x = RNetCDF::att.get.nc(ncfile = nc, variable = "DATE1", attribute = "long_name")),
                                       format = "%d-%B-%Y"))
      
      new_dat <- data.frame(LATITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "LATD1"),
                            LONGITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "LOND1"),
                            BOTTOM_SAL_PSU = apply(X = RNetCDF::var.get.nc(ncfile = nc, variable = "SAL2"), MARGIN = 1, FUN = function(x) x[max(which(!is.na(x)))]),
                            BOTTOM_TEMP_C = apply(X = RNetCDF::var.get.nc(ncfile = nc, variable = "TEMP2"), MARGIN = 1, FUN = function(x) x[max(which(!is.na(x)))]),
                            YEAR = floor(as.numeric(RNetCDF::var.get.nc(ncfile = nc, variable = "CRUISE1"))/100),
                            STATIONID = gsub(pattern = " ", replacement = "", x = RNetCDF::var.get.nc(ncfile = nc, variable = "STN_NM1")),
                            DATE = as.character(start_date + (RNetCDF::var.get.nc(ncfile = nc, variable = "DATE1")*24*3600)))
    } else {
      start_date <- as.POSIXct(as.Date(gsub(pattern = "Since ", replacement = "", x = RNetCDF::att.get.nc(ncfile = nc, variable = "DATE", attribute = "long_name")),
                                       format = "%d-%B-%Y"))
      
      
      new_dat <- data.frame(LATITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "LAT"),
                            LONGITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "LON"),
                            BOTTOM_SAL_PSU = apply(X = RNetCDF::var.get.nc(ncfile = nc, variable = "SAL"), MARGIN = 2, FUN = function(x) x[max(which(!is.na(x)))]),
                            BOTTOM_TEMP_C = apply(X = RNetCDF::var.get.nc(ncfile = nc, variable = "TEMP"), MARGIN = 2, FUN = function(x) x[max(which(!is.na(x)))]),
                            YEAR = floor(as.numeric(RNetCDF::var.get.nc(ncfile = nc, variable = "CRUISE"))/100),
                            STATIONID = gsub(pattern = " ", replacement = "", x = RNetCDF::var.get.nc(ncfile = nc, variable = "STN_NM")),
                            DATE = as.character(start_date + (RNetCDF::var.get.nc(ncfile = nc, variable = "DATE")*24*3600)))
    }
    
    bottom_dat <- dplyr::bind_rows(bottom_dat, new_dat)
    
    RNetCDF::close.nc(nc)
  }
  
  return(bottom_dat)
}

make_post_2020_summary <- function() {
  
  nc_files <- list.files(here::here("data"), pattern = "_EBS.nc", full.names = TRUE)
  
  bottom_dat <- data.frame()
  
  for(jj in 1:length(nc_files)) {
    nc <- RNetCDF::open.nc(con = nc_files[jj])
    
    new_dat <- data.frame(LATITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "latitude"),
                          LONGITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "longitude"),
                          BOTTOM_SAL_PSU = apply(X = RNetCDF::var.get.nc(ncfile = nc, variable = "sea_floor_practical_salinity"), MARGIN = 1, FUN = function(x) x[max(which(!is.na(x)))]),
                          BOTTOM_TEMP_C = apply(X = RNetCDF::var.get.nc(ncfile = nc, variable = "sea_floor_temperature"), MARGIN = 1, FUN = function(x) x[max(which(!is.na(x)))]),
                          STATIONID = RNetCDF::var.get.nc(ncfile = nc, variable = "stationid"),
                          DATE = RNetCDF::var.get.nc(ncfile = nc, variable = "time")) |>
      dplyr::mutate(DATE = as.POSIXct(DATE, tz = "UTC")) |>
      dplyr::mutate(YEAR = lubridate::year(DATE))              
    
    
    bottom_dat <- dplyr::bind_rows(bottom_dat, new_dat)
    
    RNetCDF::close.nc(nc)
    
  }
  
  return(bottom_dat)
  
}


library(cowplot)
library(gapctd)
library(coldpool)
library(akgfmaps)

ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs",
                                         set.crs = coldpool::ebs_proj_crs)
# panel_extent <- data.frame(y = c(52, 64),
#                            x = c(-175, -156)) %>%
#   akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs)

channel <- coldpool::get_connected(schema = "AFSC")

haul_dat <- RODBC::sqlQuery(channel = channel, 
                            query = "select vessel, cruise, haul, start_time, haul_type, performance, region, stationid from racebase.haul where cruise > 200800 and haul_type = 3 and performance = 0 and region = 'BS'")
names(haul_dat) <- tolower(names(haul_dat))

haul_dat$year <- floor(haul_dat$cruise/100)

pre_2020_ctd_df <- make_pre_2020_summary()
names(pre_2020_ctd_df) <- tolower(names(pre_2020_ctd_df))

pre_2020_ctd_df <- dplyr::inner_join(pre_2020_ctd_df, haul_dat) |>
  dplyr::mutate(date = as.POSIXct(date)) |>
  dplyr::filter(lubridate::yday(date) == lubridate::yday(start_time))

pre_2020_ctd_df <- pre_2020_ctd_df |>
  dplyr::select(year, stationid, date) |>
  dplyr::group_by(year, stationid) |>
  dplyr::summarize(date = max(date)) |>
  dplyr::inner_join(pre_2020_ctd_df)

post_2020_ctd_df <- make_post_2020_summary()
names(post_2020_ctd_df ) <- tolower(names(post_2020_ctd_df))

post_2020_ctd_df <- post_2020_ctd_df |>
  dplyr::select(year, stationid, date) |>
  dplyr::group_by(year, stationid) |>
  dplyr::summarize(date = max(date)) |>
  dplyr::inner_join(post_2020_ctd_df)


bottom_temp_sal <- dplyr::bind_rows(pre_2020_ctd_df, post_2020_ctd_df)

sal_years <- unique(bottom_temp_sal$year)

for(kk in sal_years) {
  coldpool::interpolate_variable(dat = bottom_temp_sal |>
                                   dplyr::filter(year == kk),
                                 dat.year = kk,
                                 var.col = "bottom_sal_psu",
                                 lat.col = "latitude",
                                 lon.col = "longitude",
                                 interpolation.crs = "EPSG:3338",
                                 cell.resolution = c(5000, 5000),
                                 select.region = "ebs",
                                 methods = "ste")
}


ste_files <- list.files(here::here("output", "raster", "ebs", "bottom_sal_psu"), 
                        pattern = "ste",
                        full.names = TRUE)


psu_stack <- coldpool::make_raster_stack(file_path = "C:/Users/sean.rohan/Work/afsc/gapctd/output/raster/ebs/bottom_sal_psu/",
                                         file_name_contains = "ste_",
                                         wrap = FALSE)


# make_raster_stack <- function(file_path = "C:/Users/sean.rohan/Work/afsc/gapctd/output/raster/sebs/bottom_sal_psu/",
#                               file_name_contains = "ste_",
#                               file_type = ".tif") {
#   
#   file_paths <- dir(file_path, full.names = T)[grep(file_type, dir(file_path, full.names = T))]
#   
#   if(length(file_paths[-grep(".xml", file_paths)]) > 0) {
#     file_paths <- file_paths[-grep(".xml", file_paths)]
#   }
#   
#   if(!is.null(file_name_contains)) {
#     file_paths <- file_paths[grep(file_name_contains, file_paths)]
#   }
#   
#   for(i in 1:length(file_paths)) {
#     if(i == 1) {
#       rstack <- raster::stack(raster::raster(file_paths[i], values = TRUE))
#     } else {
#       rstack <- raster::addLayer(rstack, raster(file_paths[i], values = TRUE))
#     }
#   }
#   
#   return(rstack)
# }


psu_df <- as.data.frame(psu_stack, xy = TRUE)

psu_df <- tidyr::pivot_longer(psu_df, 
                    cols = which(!(names(psu_df) %in% c("x", "y")))) |>
  dplyr::select(x, y, 
                year = name, 
                salinity = value) |>
  dplyr::mutate(year = readr::parse_number(year))

psu_df <- dplyr::bind_rows(psu_df,
                           data.frame(x = 1, y = 1, year = 2019, salinity = NA))

sel_years <- c(2017, 2019, 2021, 2022, 2023)

panel_extent <- data.frame(y = ebs_layers$plot.boundary$y,
                           x = ebs_layers$plot.boundary$x)

label_2019 <- data.frame(x = mean(panel_extent$x),
                         y = mean(panel_extent$y),
                         label = "To be\nprocessed",
                         year = 2019)


sal_breaks <- c(-Inf, 27:34, Inf)

sal_viridis_option <- "E" # viridis turbo palette

sal_colors <- rev(viridis::viridis_pal(option = sal_viridis_option)(length(sal_breaks)-1))

sal_map_cbar <- coldpool::legend_discrete_cbar(breaks = sal_breaks,
                                                colors = sal_colors,
                                                legend_direction = "horizontal",
                                                font_size = 3,
                                                width = 2,
                                                expand_size.x = 0.3,
                                                expand_size.y = 0.3,
                                                expand.x = 0.3,
                                                expand.y = 0.9,
                                                spacing_scaling = 18,
                                                text.hjust = 0.5,
                                                font.family = "sans",
                                                neat.labels = FALSE) + 
  annotate("text", 
           x = 4, 
           y = 27.5, 
           label =  expression(bold("Bottom Salinity")), 
           size = rel(3.2)) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

map_salinity <- ggplot() +
    geom_sf(data = ebs_layers$akland, 
            fill = "grey70", 
            color = NA) +
    geom_sf(data = ebs_layers$survey.area, fill = "grey75") +
    geom_tile(data = dplyr::filter(psu_df, year %in% sel_years),
              aes(x = x, 
                  y = y,
                  fill = cut(salinity, sal_breaks))) +
    geom_sf(data = ebs_layers$survey.area, 
            fill = NA, 
            color = "black") +
    geom_sf(data = ebs_layers$bathymetry) +
    geom_polygon(data = data.frame(x = panel_extent$x[c(1,2,2,1,1)],
                                   y = panel_extent$y[c(1,1,2,2,1)],
                                   year = 2019),
                 aes(x = x,
                     y = y),
                 fill = "white") +
    geom_label(data = label_2019,
               aes(x = x,
                   y = y,
                   label = label),
               label.size = NA) +
    scale_fill_manual(values = sal_colors) +
    facet_wrap(~year, nrow = 2) +
    scale_x_continuous(name = "Longitude", 
                       breaks = c(-180, -170, -160),
                       limits = ebs_layers$plot.boundary$x) + 
    scale_y_continuous(name = "Latitude", 
                       breaks = c(54, 58, 62),
                       limits = ebs_layers$plot.boundary$y) +
    coldpool::theme_multi_map_blue_strip() +
    theme(legend.position = "none", axis.title = element_blank())


png(file = here::here("plots", "efh_salinity_2017_2024.png"), width = 5, height = 4, units = "in", res = 300)
cowplot::plot_grid(map_salinity, 
                   sal_map_cbar, 
                   rel_heights = c(0.9, 0.1),
                   nrow = 2)
dev.off()


png(file = here::here("plots", "ex_salinity_timeseries.png"), width = 6.5, height = 6, units = "in", res = 300)
print(
  cowplot::plot_grid(
    ggplot(data = salinity_df |>
             dplyr::group_by(year) |>
             dplyr::summarise(mean_salinity = mean(z)),
           aes(x = year,
               y = mean_salinity,
               group = year < 2021)) +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Salinity (PSU)") +
      geom_line() +
      geom_point() +
      theme_few(),
    ggplot() +
      geom_hline(yintercept = 0, color = "grey70") +
      geom_hline(yintercept = c(-1,1), linetype = 2, color = "grey70") +
      geom_hline(yintercept = c(-2,2), linetype = 3, color = "grey70") +
      geom_point(data = salinity_df |>
                   dplyr::group_by(year) |>
                   dplyr::summarise(mean_salinity = mean(z)),
                 aes(x = year,
                     y = scale(mean_salinity)[,1],
                     group = year < 2021)) +
      geom_line(data = salinity_df |>
                  dplyr::group_by(year) |>
                  dplyr::summarise(mean_salinity = mean(z)),
                aes(x = year,
                    y = scale(mean_salinity)[,1],
                    group = year < 2021)) +
      scale_x_continuous(name = "Year") +
      scale_y_continuous(name = "Salinity anomaly") +
      theme_few(),
    nrow = 2))
dev.off()
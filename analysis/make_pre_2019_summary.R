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

sebs_layers <- akgfmaps::get_base_layers(select.region = "ebs",
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


ste_files <- list.files(here::here("output", "raster", "sebs", "bottom_sal_psu"), 
           pattern = "ste",
           full.names = TRUE)
  

psu_stack <- coldpool::make_raster_stack(file_path = "C:/Users/sean.rohan/Work/afsc/gapctd/output/raster/sebs/bottom_sal_psu/",
                            file_name_contains = "ste_")


make_raster_stack <- function(file_path = "C:/Users/sean.rohan/Work/afsc/gapctd/output/raster/sebs/bottom_sal_psu/",
                              file_name_contains = "ste_",
                              file_type = ".tif") {
  
  file_paths <- dir(file_path, full.names = T)[grep(file_type, dir(file_path, full.names = T))]
  
  if(length(file_paths[-grep(".xml", file_paths)]) > 0) {
    file_paths <- file_paths[-grep(".xml", file_paths)]
  }
  
  if(!is.null(file_name_contains)) {
    file_paths <- file_paths[grep(file_name_contains, file_paths)]
  }
  
  for(i in 1:length(file_paths)) {
    if(i == 1) {
      rstack <- raster::stack(raster::raster(file_paths[i], values = TRUE))
    } else {
      rstack <- raster::addLayer(rstack, raster(file_paths[i], values = TRUE))
    }
  }
  
  return(rstack)
}

writeRaster(rstack, 
            "bottom_salinity.nc", 
            overwrite=TRUE, 
            format="CDF",
            varname = "sea_floor_practical_salinity", 
            varunit = "1", 
            longname="Bottom practical salinity (PSS-78)", 
            xname="x",   
            yname="y",
            zname="nbands",
            zunit="numeric")

ncfile <- RNetCDF::open.nc("bottom_salinity.nc")

salinity_df <- data.frame()

for(ii in 1:dim(rstack)[3]) {
  ex_df <- expand.grid(x = RNetCDF::var.get.nc(ncfile, variable = "x"),
                       y = RNetCDF::var.get.nc(ncfile, variable = "y"))
  ex_df$z <- as.vector(RNetCDF::var.get.nc(ncfile, variable = "sea_floor_practical_salinity")[,,ii])
  
  ex_df <- ex_df |>
    dplyr::filter(!is.na(z))
  
  ex_df$year <- sal_years[ii]
  
  salinity_df <- dplyr::bind_rows(salinity_df, ex_df)
}

png(file = here::here("plots", "ex_bottom_salinity_map.png"), width = 10, height = 8, units = "in", res = 300)
print(
ggplot() +
  geom_tile(data = salinity_df,
            aes(x = x, 
                y = y,
                fill = z)) +
  scale_fill_fermenter(name = "Salinity (PSU)",
                       palette = "YlOrBr") +
  facet_wrap(~year) +
  theme_bw()
)
dev.off()

png(file = here::here("plots", "ex_bottom_salinity_map.png"), width = 10, height = 8, units = "in", res = 300)
print(
  ggplot() +
    geom_sf(data = sebs_layers$akland, fill = "black", 
            color = NA) +
    geom_sf(data = sebs_layers$survey.area, fill = "grey75") +
    geom_tile(data = salinity_df,
              aes(x = x, 
                  y = y,
                  fill = z)) +
    geom_sf(data = sebs_layers$survey.area, 
            fill = NA, 
            color = "black") +
    geom_sf(data = sebs_layers$bathymetry) +
    scale_fill_fermenter(name = "Salinity (PSU)",
                         palette = "YlOrBr") +
    facet_wrap(~year) +
    coord_sf(xlim = panel_extent$x, 
             ylim = panel_extent$y,
             expand = c(0,0)) +
    scale_x_continuous(name = "Longitude", 
                       breaks = c(-180, -170, -160)) + 
    scale_y_continuous(name = "Latitude", 
                       breaks = c(54, 58, 62)) +
    theme_bw()
)
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



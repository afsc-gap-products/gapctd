library(cowplot)
library(gapctd)
library(ggthemes)


make_cast_pdfs <- function(channel, region, year) {
  
  dat <- readRDS(file = here::here(paste0("GAPCTD_", year, "_", region, ".rds")))
  
  deployments <- dplyr::select(dat, vessel, cruise, haul) |>
    unique()
  
  haul_dat <- RODBC::sqlQuery(channel = channel,
                              query = paste0("select * from racebase.haul where vessel in (", 
                                             paste(unique(dat$vessel), collapse = ", "), 
                                             ") and cruise in (", 
                                             paste(unique(dat$cruise), collapse = ", "), ")")) |>
    janitor::clean_names() |>
    sf::st_as_sf(coords = c("start_longitude", "start_latitude"), 
                 crs = "EPSG:4326") |>
    sf::st_transform(crs = "EPSG:3338")
  
  map_layers <- akgfmaps::get_base_layers(select.region = region, set.crs = "EPSG:3338")
  
  pdf(here::here("plots", paste0(region, "_", year, "_dat.pdf")), onefile = TRUE)
  for(ii in 1:nrow(deployments)) {
    haul_sel <- dplyr::filter(haul_dat, 
                              haul == deployments$haul[ii],
                              vessel == deployments$vessel[ii],
                              cruise == deployments$cruise[ii]
    )
    
    dat_sel <- dplyr::inner_join(haul_sel, dat,
                                 by = c("vessel", "cruise", "haul", "stationid"))
    print(
      cowplot::plot_grid(
        cowplot::plot_grid(
          ggplot() +
            geom_path(data = dat_sel,
                      aes(x = temperature, 
                          y = depth,
                          linetype = cast_direction),
                      color = "red") +
            scale_x_continuous(name = "Temperature") +
            scale_y_reverse() +
            theme_bw() +
            theme(legend.position = "none"),
          ggplot() +
            geom_path(data = dat_sel,
                      aes(x = salinity, 
                          y = depth,
                          linetype = cast_direction),
                      color = "darkgreen") +
            scale_x_continuous(name = "Salinity (PSU)") +
            scale_y_reverse() +
            theme_bw() +
            theme(legend.position = "none"),
        ggplot() +
          geom_path(data = dat_sel,
                    aes(x = abs(velocity), 
                        y = depth,
                        linetype = cast_direction),
                    color = "blue") +
          scale_y_reverse() +
          scale_x_continuous(name = "Speed (m/s)") +
          theme_bw() +
          theme(legend.position = "none"),
        ncol = 3),
        ggplot() +
          geom_sf(data = map_layers$akland) +
          geom_sf(data = map_layers$bathymetry, aes(color = factor(METERS))) +
          geom_sf(data = haul_sel, size = rel(3), color = "black") +
          coord_sf(xlim = c(sf::st_coordinates(haul_sel)[,1]-3e5, sf::st_coordinates(haul_sel)[,1]+3e5),
                   ylim = c(sf::st_coordinates(haul_sel)[,2]-1e5, sf::st_coordinates(haul_sel)[,2]+1e5)) +
          scale_x_continuous(breaks = map_layers$lon.breaks) +
          scale_y_continuous(breaks = map_layers$lat.breaks) +
          scale_color_brewer(name = "Depth (m)") +
          ggtitle(paste0(haul_sel$vessel, "-", haul_sel$haul)) +
          theme_few() +
          theme(),
        nrow = 2,
        rel_heights = c(0.6, 0.4))
    )
  }
  dev.off()
}



channel <- gapctd::get_connected(schema = "AFSC")
make_cast_pdfs(channel = channel, region = "ai", year = 2022)
make_cast_pdfs(channel = channel, region = "ebs", year = 2022)
make_cast_pdfs(channel = channel, region = "goa", year = 2021)
make_cast_pdfs(channel = channel, region = "ebs", year = 2021)


ai_layers$bathymetry

test <- readRDS(file = here::here("GAPCTD_2021_GOA.rds")) |>
  dplyr::filter(vessel == 148, cruise == 202101, haul == 214) 

test_dat <- readRDS(file = here::here("GAPCTD_2022_EBS.rds")) |>
  dplyr::filter(deploy_id == "sbe19plus_01908091_2022_07_25_0002")


ggplot() +
  geom_path(data = test_dat,
            mapping = aes(x = temperature, y = depth, linetype = cast_direction))



table(test$timeS)

difftime(test$timeS, as.POSIXct("2022-07-04 012:00:00", 
                           tz = "America/Anchorage", units = "hours"))

test[which.max(test$depth),]

readRDS(file = here::here("GAPCTD_2021_GOA.rds")) |>
  dplyr::filter(vessel == 148, haul == 214) |>
  # dplyr::summarise(max_depth = max(depth)) |>
ggplot(mapping = aes(x = salinity, y = depth*-1, color = cast_direction)) +
  geom_path()

table(test$vessel, test$serial_number)

test[which.min(test$salinity),]
range(test$depth)

readRDS(file = here::here("GAPCTD_2021_GOA.rds")) |>
  dplyr::filter(stationid == "")


readRDS(metadata_files[1])
test <- readRDS(cast_files[1])

test$downcast@metadata$race_metadata$STATIONID
test$downcast@metadata$filename
test$downcast@metadata$latitude

dirname(cast_files[1])

goa_layers <- akgfmaps::get_base_layers(select.region = "goa.east", set.crs = "EPSG:3338")



dat <- RODBC::sqlQuery(channel = channel,
                       query = "select * from racebase.haul where vessel = 148 and cruise = 202101 and haul = 214") |>
  janitor::clean_names() #|>
  sf::st_as_sf(coords = c("start_longitude", "start_latitude"), 
               crs = "EPSG:4326") #|>
  sf::st_transform(crs = "EPSG:3338")
dat$bottom_depth

ggplot() +
  geom_sf(data = goa_layers$akland) +
  geom_sf(data = goa_layers$bathymetry) +
  geom_sf(data = dat) +
  coord_sf(xlim = goa_layers$plot.boundary$x,
           ylim = goa_layers$plot.boundary$y) +
  theme_few()

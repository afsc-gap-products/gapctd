# Region map and sample sizes
library(akgfmaps)
library(ggthemes)
library(gapctd)
library(RNetCDF)
library(RODBC)

# Set up regional map layers
project_crs <- "EPSG:3338"

# Load CTD data
sample_locations_df <- dplyr::bind_rows(
  readRDS(file = here::here("paper", "data", "all_profiles", "GAPCTD_2021_EBS.rds")) |>
    dplyr::mutate(region = "EBS+NBS"),
  readRDS(file = here::here("paper", "data", "all_profiles","GAPCTD_2021_GOA.rds")) |>
    dplyr::mutate(region = "GOA"),
  readRDS(file = here::here("paper", "data", "all_profiles","GAPCTD_2022_AI.rds")) |>
    dplyr::mutate(region = "AI"),
  readRDS(file = here::here("paper", "data", "all_profiles","GAPCTD_2022_EBS.rds")) |>
    dplyr::mutate(region = "EBS+NBS")) |>
  dplyr::group_by(vessel, cruise, haul, region, latitude, longitude, serial_number, cast_direction) |>
  dplyr::summarise(max_depth = max(depth),
                   max_timeS = max(timeS)) |>
  dplyr::ungroup() |>
  unique() |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "+proj=longlat") |>
  sf::st_transform(crs = project_crs)

# Load map layers
map_layers_ebs <- akgfmaps::get_base_layers(select.region = "ebs",  set.crs = project_crs)

map_layers_goa <- akgfmaps::get_base_layers(select.region = "goa", set.crs = project_crs)
map_layers_goa$survey.area$SURVEY <- "GOA"

map_layers_ai <- akgfmaps::get_base_layers(select.region = "ai", set.crs = project_crs)
map_layers_ai$survey.area$SURVEY <- "AI"

survey_area_df <- dplyr::bind_rows(map_layers_goa$survey.area,
                                   map_layers_ebs$survey.area,
                                   map_layers_ai$survey.area)

# Get deployment locations
channel <- gapctd::get_connected(schema = "AFSC")
example_deployments <- read.csv(file = here::here("paper", "data", "example_deployments.csv"))
names(example_deployments) <- toupper(names(example_deployments))

example_hauls <- RODBC::sqlQuery(channel = channel,
                                 query = "select * from RACEBASE.HAUL where CRUISE > 202100") |>
  dplyr::select(-REGION) |>
  dplyr::inner_join(example_deployments) |>
  akgfmaps::transform_data_frame_crs(coords = c("START_LONGITUDE", "START_LATITUDE"), in.crs = "EPSG:4326", out.crs = project_crs)

# Load GEBCO bathymetry and
gebco <- raster::raster(x = here::here("paper", "data", "gebco_2022_npac.nc"),
                        z_varname = "elevation")

gebco <- raster::aggregate(gebco, 10)

gebco <- raster::rotate(gebco)

gebco <- raster::projectRaster(gebco, to = raster::raster(xmn = map_layers_ai$plot.boundary$x[1] - 1e5,
                                                          xmx = map_layers_goa$plot.boundary$x[2] + 1e5,
                                                          ymn = map_layers_goa$plot.boundary$y[1] - 1e5,
                                                          ymx = map_layers_ebs$plot.boundary$y[2] + 1e5,
                                                          crs = "EPSG:3338",
                                                          res = 5000))

bathy_df <- as.data.frame(gebco, xy = TRUE) |>
  dplyr::filter(Elevation.relative.to.sea.level < 0, 
                !is.na(Elevation.relative.to.sea.level)) |>
  dplyr::mutate(Elevation.relative.to.sea.level = Elevation.relative.to.sea.level * -1)

names(bathy_df) <- c("x", "y", "elevation")

# Make map
ragg::agg_png(filename = here::here("paper", "plots", "map_sample_locations.png"), 
              width = 165, 
              height = 75, 
              units = "mm", 
              res = 2400)
print(
  ggplot() +
    geom_tile(data = bathy_df,
              mapping = aes(x = x,
                            y = y, fill = elevation),
              color = NA) +
    # geom_sf(data = bathy_sf,
    #         mapping = aes(fill = elevation), color = NA) +
    geom_sf(data = survey_area_df |>
              dplyr::inner_join(data.frame(SURVEY = c("EBS_SHELF", "GOA", "NBS_SHELF", "AI"),
                                           region_abbv = c("EBS", "GOA", "NBS", "AI"))),
            aes(color = region_abbv),
            linewidth = rel(0.8),
            fill = NA) +
    geom_sf(data = dplyr::filter(map_layers_ebs$akland,
                                 DESC_ == "Russia"), 
            color = "black",
            fill = "grey80", 
            size = rel(0.1)) +
    geom_sf(data =  map_layers_goa$akland |>
              dplyr::filter(!is.na(POPYCOUN)) |>
              dplyr::group_by(POPYCOUN) |>
              dplyr::summarise(n = n()), 
            color = "black",
            fill = "grey80", 
            size = rel(0.1)) +
    geom_sf(data = sf::st_graticule(lat = seq(45, 65, 5), 
                                    lon = c(seq(165, 180, 5), seq(-175, -130, 5)), 
                                    margin = 1e-5),
            alpha = 0.5) +
    geom_sf(data = sample_locations_df,
            size = rel(0.2)) +
    geom_shadowtext(data = example_hauls,
                    mapping = aes(x = START_LONGITUDE, y = START_LATITUDE, label = paste0("#", EXAMPLE)),
                    color = "black", 
                    bg.color = "white",
                    size = rel(5)) +
    geom_text(data = data.frame(x = -152, y = 64, lab = "Alaska") |>
                akgfmaps::transform_data_frame_crs(out.crs = "EPSG:3338"),
              aes(x = x,
                  y = y,
                  label = lab)) +
    coord_sf(xlim = c(map_layers_ai$plot.boundary$x[1],
                      map_layers_goa$plot.boundary$x[2]),
             ylim = c(map_layers_goa$plot.boundary$y[1],
                      map_layers_ebs$plot.boundary$y[2])) +
    scale_x_continuous(breaks = c(170, -180, -170, -160, -150, -140),
                       expand = c(0.01, 0.01)) +
    scale_fill_fermenter(name = "Depth (m)", breaks = c(0, 50, 100, 200, 500, 1000, 8000), direction = 1) +
    scale_color_manual(name = "Region", values = viridis::viridis_pal(option = "C")(5)[1:4]) +
    scale_shape(name = NULL) +
    theme_bw() +
    theme(legend.position = "right",
          legend.direction = "vertical",
          legend.text = element_text(size = 7),
          legend.key.size = unit(5, 'mm'),
          legend.margin = margin(1,1,1,1, unit = "mm"),
          legend.box.margin = margin(1,1,1,1, unit = "mm"),
          legend.box.spacing = unit(1, unit = "mm"),
          legend.spacing = unit(0, unit = "mm"),
          axis.title = element_blank())
)
dev.off()

# Make sample size table
sample_table <- sample_locations_df |> 
  as.data.frame() |>
  dplyr::select(vessel, cruise, haul, region) |>
  unique() |>
  dplyr::group_by(cruise, region) |>
  dplyr::summarise(n = n()) |>
  dplyr::arrange(region, cruise) |>
  dplyr::full_join(
    sample_locations_df |> 
      as.data.frame() |>
      dplyr::group_by(cruise, region) |>
      dplyr::summarise(min_deployment_depth = min(max_depth),
                       max_deployment_depth = max(max_depth),
                       min_date = min(max_timeS),
                       max_date = max(max_timeS)) |>
      dplyr::mutate(depth_range = paste0(min_deployment_depth, "–" , max_deployment_depth),
                    date_range = paste0(lubridate::month(min_date, abbr = TRUE, label = TRUE), " ",
                                        lubridate::day(min_date),
                                        "–" , 
                                        lubridate::month(max_date, abbr = TRUE, label = TRUE), " ",
                                        lubridate::day(max_date), ", ",
                                        lubridate::year(max_date)))
  ) |>
  dplyr::ungroup() |>
  dplyr::inner_join(data.frame(region = c("AI", 
                                          "EBS+NBS", 
                                          "EBS+NBS", 
                                          "EBS+NBS", 
                                          "EBS+NBS", 
                                          "GOA"),
                               cruise = c(202201, 
                                          202101, 
                                          202102, 
                                          202201, 
                                          202202, 
                                          202101),
                               full_region = c("Aleutian Islands", 
                                               "Eastern Bering Sea", 
                                               "Northern Bering Sea", 
                                               "Eastern Bering Sea", 
                                               "Northern Bering Sea", 
                                               "Gulf of Alaska"))) |>
  dplyr::mutate(year = floor(cruise/100)) |>
  dplyr::select(full_region, 
                year,
                n, 
                depth_range,
                date_range)
  
write.csv(sample_table,
          here::here("paper", "plots", "sample_size_table.csv"),
          row.names = FALSE)

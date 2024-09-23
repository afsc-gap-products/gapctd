library(ncdf4)

print(con)

nc_path = here::here("data", "GAPCTD_2021_EBS.nc")
region = "sebs"

nc_path = here::here("data", "GAPCTD_2024_AI.nc")
region = "ai"

con <- ncdf4::nc_open(nc_path)

plot_2d_vars = c("temperature", 
                 "practical_salinity", 
                 "absolute_salinity", 
                 "dissolved_oxygen",
                 "ph", 
                 "sound_speed")

var_label <- list(temperature = expression('T'~(degree*C)), 
                  dissolved_oxygen = expression('DO'~(ml%.%l^-1)),
                  ph = "pH",
                  practical_salinity = expression('S(T) (PSS-78)'),
                  absolute_salinity = expression(S[A]~(g%.%kg^-1)),
                  sound_speed = expression('Sound speed'~(m%.%s^-1)))

var_color <- list(temperature = "H",
                  dissolved_oxygen = "A",
                  ph = "E",
                  practical_salinity = "C",
                  absolute_salinity = "C",
                  sound_speed = "B")

map_layers <- akgfmaps::get_base_layers(select.region = region, set.crs = "EPSG:3338")

var_names <- names(con$var)

data_2d <- data.frame(
  latitude = ncvar_get(con, "latitude"),
  longitude = ncvar_get(con, "longitude"),
  haul = ncvar_get(con, "haul"),
  profile = ncvar_get(con, "profile"),
  vessel = ncvar_get(con, "vessel"),
  cruise = ncvar_get(con, "cruise"),
  stationid = ncvar_get(con, "stationid"),
  haul_depth = ncvar_get(con, "haul_depth"),
  temperature = ncvar_get(con, "sea_floor_temperature"),
  practical_salinity = ncvar_get(con, "sea_floor_practical_salinity"),
  absolute_salinity = ncvar_get(con, "sea_floor_salinity"),
  sound_speed = ncvar_get(con, "sea_floor_sound_speed_in_sea_water")
)

if("sea_floor_dissolved_oxygen" %in% var_names) {
  data_2d$dissolved_oxygen <- ncvar_get(con, "sea_floor_dissolved_oxygen")
}

if("sea_floor_ph" %in% var_names) {
  data_2d$ph <- ncvar_get(con, "sea_floor_ph")
}
  
# Convert to spatial
data_2d <- data_2d |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

plot_2d_vars <- plot_2d_vars[plot_2d_vars %in% names(data_2d)]

plot_maps_2d <- vector(mode = "list", length = length(plot_2d_vars))

names(plot_maps_2d) <- plot_2d_vars

for(ii in 1:length(plot_2d_vars)) {
  
  plot_maps_2d[[plot_2d_vars[ii]]] <- 
    ggplot() +
    geom_sf(data = map_layers$akland, 
            color = "black",
            fill = "grey70") +
    geom_sf(data = data_2d,
            mapping = aes(color = 
                            eval(expr = 
                                   parse(text = plot_2d_vars[ii])
                                 )
                          )
            ) +
    geom_sf(data = map_layers$survey.area,
            fill = NA) +
    scale_x_continuous(limits = map_layers$plot.boundary$x,
                       breaks = map_layers$lon.breaks) +
    scale_y_continuous(limits = map_layers$plot.boundary$y,
                       breaks = map_layers$lat.breaks) +
    scale_color_viridis_c(name = var_label[[plot_2d_vars[ii]]],
                        option = var_color[[plot_2d_vars[ii]]]) +
    theme_bw()
  
}


for(ii in 1:length(plot_2d_vars)) {
  print(plot_maps_2d[[ii]])
}


unique_vch <- data_2d |>
  sf::st_drop_geometry() |> 
  dplyr::select(vessel, cruise, haul) |>
  unique() |>
  dplyr::arrange(vessel, haul)

n_successful_deployments <- nrow(unique_vch)

n_successful_deployments

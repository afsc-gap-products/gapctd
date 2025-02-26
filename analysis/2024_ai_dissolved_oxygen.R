
# Dissolved Oxygen maps for September 2024 GPT

library(gapctd)
library(stars)
library(akgfmaps)

map_layers <- akgfmaps::get_base_layers(select.region = "ai",
                                        set.crs = "EPSG:3338")

fig_res <- 300

do_breaks <- c(-Inf, 2:6, Inf)

ai_ctd_2024 <- readRDS(file = here::here("GAPCTD_AI_BOTTOM_2024.rds")) |>
  dplyr::mutate(LAT = LATITUDE,
                LON = LONGITUDE) |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

depth_vs_do <- ggplot() +
  geom_point(data = ai_ctd_2024,
             mapping = aes(x = BOTTOM_DEPTH, y = BOTTOM_DISSOLVED_OXYGEN_ML_L)) +
  scale_x_continuous(name = "Bottom depth (m)") +
  scale_y_continuous(name = expression('Bottom dissolved oxygen ('*ml%.%l^-1*')')) +
  theme_bw()

map_layers <- akgfmaps::get_base_layers(select.region = "ai", 
                                             set.crs = "EPSG:3338")
ai_map <- ggplot() +
  geom_sf(data = map_layers$akland) +
  geom_sf(data = map_layers$bathymetry, color = "grey70", linewidth = 0.15) +
  geom_sf(data = ai_ctd_2024,
          mapping = aes(color = cut(BOTTOM_DISSOLVED_OXYGEN_ML_L, breaks = do_breaks, right = TRUE)),
          size = rel(0.8)) +
  geom_sf(data = map_layers$graticule, linewidth = 0.3, alpha = 0.2) +
  scale_color_viridis_d(option = "rocket",
                        direction = -1,
                        na.value = NA,
                        drop = FALSE) +
  coord_sf(xlim = map_layers$plot.boundary$x,
           ylim = map_layers$plot.boundary$y) +
  scale_x_continuous(breaks = map_layers$lon.breaks) +
  scale_y_continuous(breaks = map_layers$lat.breaks) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(5,5,-5,5), units = "mm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 9))

cbar_legend <- coldpool::legend_discrete_cbar(breaks = do_breaks,
                                              colors = viridis::viridis_pal(option = "rocket",
                                                                            direction = -1),
                                              legend_direction = "horizontal",
                                              font_size = 3,
                                              width = 0.1,
                                              expand_size.x = 0.3,
                                              expand_size.y = 0.3,
                                              expand.x = 0.3,
                                              expand.y = 0.9,
                                              spacing_scaling = 1,
                                              text.hjust = 0.5,
                                              font.family = "sans",
                                              neat.labels = FALSE) + 
  annotate("text", 
           x = 1.25, 
           y = 4, 
           label = expression('Bottom dissolved oxygen ('*ml%.%l^-1*')'), 
           size = rel(3.2)) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

do_map_grid <- cowplot::plot_grid(ai_map, 
                                  cbar_legend,
                                  nrow = 2, 
                                  ncol = 1,
                                  rel_heights = c(0.85, 0.25))

ragg::agg_png(filename = here::here("plots",
                                    paste0("2024_ai_dissolved_oxygen_map.png")), width = 6, height = 3.5, units = "in", res = fig_res)
print(do_map_grid)
dev.off()

ragg::agg_png(filename = here::here("plots",
                                    paste0("2024_ai_depth_vs_dissolved_oxygen.png")), width = 5, height = 5, units = "in", res = fig_res)
print(depth_vs_do)
dev.off()


ai_ctd_2024$o2_sat <- gapctd::oxygen_saturation(temperature = ai_ctd_2024$BOTTOM_TEMPERATURE_C,
                                                salinity = ai_ctd_2024$BOTTOM_SALINITY_PSU)

ai_ctd_2024$pressure <- oce::swPressure(depth = ai_ctd_2024$BOTTOM_DEPTH,
                                        latitude = ai_ctd_2024$LAT)

ai_ctd_2024$sigma0 <- oce::swSigma0(salinity = ai_ctd_2024$BOTTOM_SALINITY_PSU,
                                    temperature = ai_ctd_2024$BOTTOM_TEMPERATURE_C,
                                    pressure = ai_ctd_2024$pressure)


cor(ai_ctd_2024$o2_sat, ai_ctd_2024$BOTTOM_DISSOLVED_OXYGEN_ML_L)
cor(ai_ctd_2024$BOTTOM_DEPTH, ai_ctd_2024$BOTTOM_DISSOLVED_OXYGEN_ML_L)
cor(ai_ctd_2024$pressure, ai_ctd_2024$BOTTOM_DISSOLVED_OXYGEN_ML_L)
cor(ai_ctd_2024$sigma0, ai_ctd_2024$BOTTOM_DISSOLVED_OXYGEN_ML_L)

ggplot() +
  geom_point(data = ai_ctd_2024,
             mapping = aes(x = o2_sat,
                           y = BOTTOM_DISSOLVED_OXYGEN_ML_L))

ggplot() +
  geom_point(data = ai_ctd_2024,
             mapping = aes(x = BOTTOM_DEPTH,
                           y = BOTTOM_DISSOLVED_OXYGEN_ML_L))

ggplot() +
  geom_point(data = ai_ctd_2024,
             mapping = aes(x = sigma0,
                           y = BOTTOM_DISSOLVED_OXYGEN_ML_L))

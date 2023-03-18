library(gapctd)

# Temperature-Salinity Area Example
ex_oce <- read.oce(file = "C:/Users/sean.rohan/Work/afsc/gapctd/paper/data/cnv/2021_07_08_0001_raw.cnv")
haul_df <- readRDS(here::here("paper", "data", "ex_hauls.rds"))

ex_tsa_init <- run_gapctd(x = ex_oce, 
                      haul_df = readRDS(here::here("paper", "data", "ex_hauls.rds")), 
                      ctd_tz = "America/Anchorage",
                      return_stage = "align") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_area_init <- gapctd::ts_area(dc = ex_tsa_init$downcast,
                                uc = ex_tsa_init$upcast,
                                return_sf = TRUE)


ex_area_init$sf$area <- sf::st_area(ex_area_init$sf)
ex_area_init$sf$L2 <- 1:nrow(ex_area_init$sf)

ex_area_poly_init <- as.data.frame(sf::st_coordinates(ex_area_init$sf)) |>
  dplyr::inner_join(ex_area_init$sf |>
                      dplyr::select(-geometry)) |>
  dplyr::mutate(L2 = factor(L2))

# ex_area_poly_init <- as.data.frame(sf::st_coordinates(ex_area_init$sf)) |>
#   dplyr::mutate(L2 = factor(L2))

ex_tsa_final <- run_gapctd(x = ex_oce, 
           haul_df = haul_df, 
           ctd_tz = "America/Anchorage",
           align_pars = list(),
           ctm_pars = list(),
           return_stage = "ctmcorrect") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_tsa_final_cast <- dplyr::bind_rows(
  data.frame(temperature = ex_tsa_final$downcast@data$temperature,
             salinity =  ex_tsa_final$downcast@data$salinity,
             cast = "downcast"),
  data.frame(temperature = ex_tsa_final$upcast@data$temperature,
             salinity =  ex_tsa_final$upcast@data$salinity,
             cast = "upcast")
)

ex_area_final <- gapctd::ts_area(dc = ex_tsa_final$downcast,
                uc = ex_tsa_final$upcast,
                return_sf = TRUE)

ex_area_final$sf$area <- sf::st_area(ex_area_final$sf)
ex_area_final$sf$L2 <- 1:nrow(ex_area_final$sf)

ex_area_poly_final <- as.data.frame(sf::st_coordinates(ex_area_final$sf)) |>
  dplyr::inner_join(ex_area_final$sf |>
                      dplyr::select(-geometry)) |>
  dplyr::mutate(L2 = factor(L2))

t_lim <- range(c(ex_area_init$cast_df$temperature, ex_area_final$cast_df$temperature))
s_lim <- range(c(ex_area_init$cast_df$salinity, ex_area_final$cast_df$salinity))
a_lim <- range(c(ex_area_init$sf$area, ex_area_init$sf$area))

plot_tsa_init <- ggplot() +
  geom_polygon(data = ex_area_poly_init,
          mapping = aes(x = X, y = Y, fill = area, group = L2)) +
  geom_point(data = ex_area_init$cast_df,
             mapping = aes(x = salinity, 
                           y = temperature, 
                           color = cast, shape = cast)) +
  geom_path(data = ex_area_init$cast_df,
            mapping = aes(x = salinity, 
                          y = temperature, 
                          color = cast)) +
  scale_color_manual(values = c("grey50", "black"), name = NULL) +
  scale_shape(name = NULL) +
  scale_fill_distiller(palette = "Purples", limits = a_lim, direction = 1) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = s_lim) +
  scale_y_continuous(name = expression('Temperature'~(degree*C)), limits = t_lim) +
  ggtitle(label = paste0("Align T (Total area: ", round(ex_area_init$area, 2), ")")) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

plot_tsa_final <- ggplot() +
  geom_polygon(data = ex_area_poly_final,
               mapping = aes(x = X, y = Y, fill = area, group = L2)) +
  geom_point(data = ex_area_final$cast_df,
             mapping = aes(x = salinity, 
                           y = temperature, 
                           color = cast, shape = cast)) +
  geom_path(data = ex_tsa_final_cast,
            mapping = aes(x = salinity, 
                          y = temperature, 
                          color = cast)) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_fill_distiller(name = "TS Area", palette = "Purples", limits = a_lim, direction = 1) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = s_lim) +
  scale_y_continuous(name = expression('Temperature'~(degree*C)), limits = t_lim) +
  ggtitle(label = paste0("CTM Corr. (Total area: ", round(ex_area_final$area, 2), ")")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

area_legend <- cowplot::get_legend(plot_tsa_final)

ragg::agg_png(here::here("paper", "plots", "ex_tsa.png"), width = 169, height = 75, units = "mm", res = 600)
print(
cowplot::plot_grid(plot_tsa_init,
                   plot_tsa_final + theme(legend.position = "none"), 
                   area_legend, nrow = 1, rel_widths = c(1,1,0.4))
)
dev.off()


# Salinity path distance

ex_spd_downcast <- oce::ctdTrim(x = ex_oce,
                             method = "range", 
                             parameters = list(item = "timeS",
                                               from = 0,
                                               to = max(ex_tsa_init$downcast@data$timeS + 0.25, na.rm = TRUE)))

ex_spd_downcast_init <- run_gapctd(x = ex_spd_downcast, 
                       haul_df = haul_df, 
                       ctd_tz = "America/Anchorage",
                       return_stage = "align") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_spd_downcast_final <- run_gapctd(x = ex_spd_downcast, 
                                   haul_df = haul_df, 
                                   ctd_tz = "America/Anchorage",
                                   return_stage = "ctmcorrect") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_spd_upcast <- oce::ctdTrim(x = ex_oce,
                           method = "range", 
                           parameters = list(item = "timeS",
                                             from = min(ex_tsa_init$upcast@data$timeS - 0.25, na.rm = TRUE),
                                             to = 5e6))

ex_spd_upcast_init <- run_gapctd(x = ex_spd_upcast, 
                          haul_df = haul_df, 
                          ctd_tz = "America/Anchorage",
                          return_stage = "align") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_spd_upcast_final <- run_gapctd(x = ex_spd_upcast, 
                                    haul_df = haul_df, 
                                    ctd_tz = "America/Anchorage",
                                    return_stage = "ctmcorrect") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_spd_init <- dplyr::bind_rows(
  data.frame(pressure = ex_spd_downcast_init$downcast@data$pressure,
             salinity = ex_spd_downcast_init$downcast@data$salinity,
             cast = "downcast"),
  data.frame(pressure = ex_spd_upcast_init$upcast@data$pressure,
             salinity = ex_spd_upcast_init$upcast@data$salinity,
             cast = "upcast"))

ex_spd_final <- dplyr::bind_rows(
  data.frame(pressure = ex_spd_downcast_final$downcast@data$pressure,
             salinity = ex_spd_downcast_final$downcast@data$salinity,
             cast = "downcast"),
  data.frame(pressure = ex_spd_upcast_final$upcast@data$pressure,
             salinity = ex_spd_upcast_final$upcast@data$salinity,
             cast = "upcast"))

spd_srange <- range(ex_spd_init$salinity, ex_spd_final$salinity)

dc_dist_init <- sprintf("%.4f", round(sum(diff(ex_spd_init$salinity[ex_spd_init$cast == "downcast"])), 4))
uc_dist_init <- sprintf("%.4f", round(sum(diff(ex_spd_init$salinity[ex_spd_init$cast == "upcast"])), 4))

plot_spd_init <- ggplot() +
  geom_path(data = ex_spd_init,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = ex_spd_init,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast,
                          shape = cast)) +
  geom_text(mapping = aes(x = 32.545, y = 82, label = paste0("DC Distance: ", dc_dist_init)), color = "grey50", hjust = 0) +
  geom_text(mapping = aes(x = 32.545, y = 89, label = paste0("UC Distance: ", uc_dist_init)), color = "black", hjust = 0) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = spd_srange) +
  scale_y_reverse(name = "Pressure (dbar)") +
  ggtitle(label = "Align T") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

dc_dist_final <- sprintf("%.4f", round(sum(diff(ex_spd_final$salinity[ex_spd_final$cast == "downcast"])), 4))
uc_dist_final <- sprintf("%.4f", round(sum(diff(ex_spd_final$salinity[ex_spd_final$cast == "upcast"])), 4))

plot_spd_final <- ggplot() +
  geom_path(data = ex_spd_final,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = ex_spd_final,
             mapping = aes(x = salinity, 
                           y = pressure, 
                           color = cast,
                           shape = cast)) +
  geom_text(mapping = aes(x = 32.545, y = 82, label = paste0("DC Distance: ", dc_dist_final)), color = "grey50", hjust = 0) +
  geom_text(mapping = aes(x = 32.545, y = 89, label = paste0("UC Distance: ", uc_dist_final)), color = "black", hjust = 0) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = spd_srange) +
  scale_y_reverse(name = "Pressure (dbar)") +
  ggtitle(label = "CTM Corr.") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

distance_legend <- cowplot::get_legend(plot_spd_final)

ragg::agg_png(here::here("paper", "plots", "ex_spd.png"), width = 169, height = 75, units = "mm", res = 600)
print(
  cowplot::plot_grid(plot_spd_init + theme(legend.position = "none"),
                     plot_spd_final + theme(legend.position = "none"), 
                     distance_legend, nrow = 1, rel_widths = c(1,1,0.4))
)
dev.off()

ragg::agg_png(here::here("paper", "plots", "ex_tsa_spd.png"), width = 169, height = 150, units = "mm", res = 600)
print(
  cowplot::plot_grid(
    cowplot::plot_grid(plot_tsa_init,
                       plot_tsa_final + theme(legend.position = "none"), 
                       area_legend, nrow = 1, rel_widths = c(1,1,0.4)),
  cowplot::plot_grid(plot_spd_init + theme(legend.position = "none"),
                     plot_spd_final + theme(legend.position = "none"), 
                     distance_legend, nrow = 1, rel_widths = c(1,1,0.4)),
  nrow = 2,
  labels = LETTERS[1:2])
)
dev.off()



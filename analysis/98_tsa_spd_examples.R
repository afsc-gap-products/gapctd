library(gapctd)

# Temperature-Salinity Area Example ----
ex_oce <- read.oce(file = "C:/Users/sean.rohan/Work/afsc/gapctd/paper/data/cnv/2021_07_08_0001_raw.cnv")
haul_df <- readRDS(here::here("paper", "data", "ex_hauls.rds"))

# T-S area after estimating alignment
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

# Using typical CTM correction parameters
ex_typctm_final <- run_gapctd(x = ex_oce, 
                           haul_df = haul_df, 
                           ctd_tz = "America/Anchorage",
                           align_pars = list(),
                           ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                           return_stage = "ctmcorrect") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_area_typctm <- gapctd::ts_area(dc = ex_typctm_final$downcast,
                                uc = ex_typctm_final$upcast,
                                return_sf = TRUE)

ex_area_typctm$sf$area <- sf::st_area(ex_area_typctm$sf)
ex_area_typctm$sf$L2 <- 1:nrow(ex_area_typctm$sf)

ex_area_poly_typctm <- as.data.frame(sf::st_coordinates(ex_area_typctm$sf)) |>
  dplyr::inner_join(ex_area_typctm$sf |>
                      dplyr::select(-geometry)) |>
  dplyr::mutate(L2 = factor(L2))

# Using T-S Area method
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

# Make T-S Area plots
t_lim <- range(c(ex_area_init$cast_df$temperature, 
                 ex_area_typctm$cast_df$temperature,
                 ex_area_final$cast_df$temperature))
s_lim <- range(c(ex_area_init$cast_df$salinity,
                 ex_area_typctm$cast_df$salinity,
                 ex_area_final$cast_df$salinity))
a_lim <- range(c(ex_area_init$sf$area, 
                 ex_area_typctm$cast_df$area,
                 ex_area_init$sf$area))

plot_tsa_init <- ggplot() +
  geom_polygon(data = ex_area_poly_init,
          mapping = aes(x = X, 
                        y = Y, 
                        fill = area, 
                        group = L2)) +
  geom_point(data = ex_area_init$cast_df,
             mapping = aes(x = salinity, 
                           y = temperature, 
                           color = cast, shape = cast)) +
  geom_path(data = ex_area_init$cast_df,
            mapping = aes(x = salinity, 
                          y = temperature, 
                          color = cast)) +
  geom_text(data = data.frame(x = 32.545, 
                              y = 2.15, 
                              label = paste0("Total area: ", round(ex_area_init$area, 2))), 
            mapping = aes(x = x, y = y, label = label), 
            color = "black", 
            hjust = 0,
            size = 3) +
  scale_color_manual(values = c("grey50", "black"), name = NULL) +
  scale_shape(name = NULL) +
  scale_fill_distiller(palette = "Purples", limits = a_lim, direction = 1) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = s_lim) +
  scale_y_continuous(name = expression('Temperature'~(degree*C)), limits = t_lim) +
  ggtitle(label = "Align T (Optimized)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none")

plot_tsa_typctm <- ggplot() +
  geom_polygon(data = ex_area_poly_typctm,
               mapping = aes(x = X, 
                             y = Y, 
                             fill = area, 
                             group = L2)) +
  geom_point(data = ex_area_typctm$cast_df,
             mapping = aes(x = salinity, 
                           y = temperature, 
                           color = cast, shape = cast)) +
  geom_path(data = ex_area_typctm$cast_df,
            mapping = aes(x = salinity, 
                          y = temperature, 
                          color = cast)) +
  geom_text(data = data.frame(x = 32.545, 
                              y = 2.15, 
                              label = paste0("Total area: ", round(ex_area_typctm$area, 2))), 
            mapping = aes(x = x, y = y, label = label), 
            color = "black", 
            hjust = 0,
            size = 3) +
  scale_color_manual(values = c("grey50", "black"), name = NULL) +
  scale_shape(name = NULL) +
  scale_fill_distiller(palette = "Purples", limits = a_lim, direction = 1) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = s_lim) +
  scale_y_continuous(name = expression('Temperature'~(degree*C)), limits = t_lim) +
  ggtitle(label = "CTM Corr. (Typical CTM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none")

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
  geom_text(data = data.frame(x = 32.545, 
                              y = 2.15, 
                              label = paste0("Total area: ", round(ex_area_final$area, 2))), 
            mapping = aes(x = x, y = y, label = label), 
                          color = "black", 
                          hjust = 0,
            size = 3) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_fill_distiller(name = "TS Area", palette = "Purples", limits = a_lim, direction = 1) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = s_lim) +
  scale_y_continuous(name = expression('Temperature'~(degree*C)), limits = t_lim) +
  ggtitle(label = "CTM Corr. (TSA)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7))

area_legend <- cowplot::get_legend(plot_tsa_final)

ragg::agg_png(here::here("paper", "plots", "ex_tsa.png"), width = 169, height = 75, units = "mm", res = 600)
print(
cowplot::plot_grid(plot_tsa_init,
                   plot_tsa_typctm,
                   plot_tsa_final + theme(legend.position = "none"), 
                   area_legend, nrow = 1, rel_widths = c(1 ,1, 1, 0.4))
)
dev.off()


# Minimum Salinity Gradient ----

# Using MSG method
ex_msg_downcast <- oce::ctdTrim(x = ex_oce,
                             method = "range", 
                             parameters = list(item = "timeS",
                                               from = 0,
                                               to = max(ex_tsa_init$downcast@data$timeS + 0.25, na.rm = TRUE)))

ex_msg_downcast_init <- run_gapctd(x = ex_msg_downcast, 
                       haul_df = haul_df, 
                       ctd_tz = "America/Anchorage",
                       return_stage = "align") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_msg_downcast_final <- run_gapctd(x = ex_msg_downcast, 
                                   haul_df = haul_df, 
                                   ctd_tz = "America/Anchorage",
                                   return_stage = "ctmcorrect") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_msg_upcast <- oce::ctdTrim(x = ex_oce,
                           method = "range", 
                           parameters = list(item = "timeS",
                                             from = min(ex_tsa_init$upcast@data$timeS - 0.25, na.rm = TRUE),
                                             to = 5e6))

ex_msg_upcast_init <- run_gapctd(x = ex_msg_upcast, 
                          haul_df = haul_df, 
                          ctd_tz = "America/Anchorage",
                          return_stage = "align") |>
  gapctd:::derive_and_bin(bin_width = 1)

ex_msg_upcast_final <- run_gapctd(x = ex_msg_upcast, 
                                    haul_df = haul_df, 
                                    ctd_tz = "America/Anchorage",
                                    return_stage = "ctmcorrect") |>
  gapctd:::derive_and_bin(bin_width = 1)

# After optimizing temperature alignment, before CTM
ex_msg_init <- dplyr::bind_rows(
  data.frame(pressure = ex_msg_downcast_init$downcast@data$pressure,
             salinity = ex_msg_downcast_init$downcast@data$salinity,
             cast = "downcast"),
  data.frame(pressure = ex_msg_upcast_init$upcast@data$pressure,
             salinity = ex_msg_upcast_init$upcast@data$salinity,
             cast = "upcast"))

# After CTM using MSG
ex_msg_final <- dplyr::bind_rows(
  data.frame(pressure = ex_msg_downcast_final$downcast@data$pressure,
             salinity = ex_msg_downcast_final$downcast@data$salinity,
             cast = "downcast"),
  data.frame(pressure = ex_msg_upcast_final$upcast@data$pressure,
             salinity = ex_msg_upcast_final$upcast@data$salinity,
             cast = "upcast"))

# After CTM  using typical parameters
ex_msg_typctm <- dplyr::bind_rows(
  data.frame(pressure = ex_typctm_final$downcast@data$pressure,
             salinity = ex_typctm_final$downcast@data$salinity,
             cast = "downcast"),
  data.frame(pressure = ex_typctm_final$upcast@data$pressure,
             salinity = ex_typctm_final$upcast@data$salinity,
             cast = "upcast"))

# Make MSG plots
msg_srange <- range(ex_msg_init$salinity, 
                    ex_msg_final$salinity, 
                    ex_msg_typctm$salinity)

dc_dist_init <- sprintf("%.4f", round(sum(abs(diff(ex_msg_init$salinity[ex_msg_init$cast == "downcast"]))), 4))
uc_dist_init <- sprintf("%.4f", round(sum(abs(diff(ex_msg_init$salinity[ex_msg_init$cast == "upcast"]))), 4))

plot_msg_init <- ggplot() +
  geom_path(data = ex_msg_init,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = ex_msg_init,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast,
                          shape = cast)) +
  geom_text(mapping = aes(x = 32.545, 
                          y = 82, 
                          label = paste0("DC Dist.: ", dc_dist_init)), 
            color = "grey50", 
            hjust = 0,
            size = 3) +
  geom_text(mapping = aes(x = 32.545, 
                          y = 89, 
                          label = paste0("UC Dist.: ", uc_dist_init)), 
            color = "black", 
            hjust = 0,
            size = 3) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = msg_srange) +
  scale_y_reverse(name = "Pressure (dbar)") +
  ggtitle(label = "Align T (Optimized)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.background = element_blank(),
        plot.background = element_blank())

dc_dist_typctm <- sprintf("%.4f", round(sum(abs(diff(ex_area_typctm$cast_df$salinity[ex_area_typctm$cast_df$cast == "downcast"]))), 4))
uc_dist_typctm <- sprintf("%.4f", round(sum(abs(diff(ex_area_typctm$cast_df$salinity[ex_area_typctm$cast_df$cast == "upcast"]))), 4))

plot_msg_typctm <- ggplot() +
  geom_path(data = ex_msg_typctm,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = ex_msg_typctm,
             mapping = aes(x = salinity, 
                           y = pressure, 
                           color = cast,
                           shape = cast)) +
  geom_text(mapping = aes(x = 32.545, 
                          y = 82, 
                          label = paste0("DC Dist.: ", dc_dist_typctm)), 
            color = "grey50", 
            hjust = 0,
            size = 3) +
  geom_text(mapping = aes(x = 32.545, 
                          y = 89, 
                          label = paste0("UC Dist.: ", uc_dist_typctm)), 
            color = "black", 
            hjust = 0,
            size = 3) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = msg_srange) +
  scale_y_reverse(name = "Pressure (dbar)") +
  ggtitle(label = "CTM Corr. (Typical CTM)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.background = element_blank(),
        plot.background = element_blank())

dc_dist_final <- sprintf("%.4f", round(sum(abs(diff(ex_msg_final$salinity[ex_msg_final$cast == "downcast"]))), 4))
uc_dist_final <- sprintf("%.4f", round(sum(abs(diff(ex_msg_final$salinity[ex_msg_final$cast == "upcast"]))), 4))

plot_msg_final <- ggplot() +
  geom_path(data = ex_msg_final,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = ex_msg_final,
             mapping = aes(x = salinity, 
                           y = pressure, 
                           color = cast,
                           shape = cast)) +
  geom_text(mapping = aes(x = 32.545, 
                          y = 82, 
                          label = paste0("DC Dist.: ", dc_dist_final)), 
            color = "grey50", 
            hjust = 0,
            size = 3) +
  geom_text(mapping = aes(x = 32.545, 
                          y = 89, 
                          label = paste0("UC Dist.: ", uc_dist_final)), 
            color = "black", 
            hjust = 0,
            size = 3) +
  scale_color_manual(name = "Cast", values = c("grey50", "black")) +
  scale_shape(name = "Cast") +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = msg_srange) +
  scale_y_reverse(name = "Pressure (dbar)") +
  ggtitle(label = "CTM Corr. (MSG)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        panel.background = element_blank(),
        plot.background = element_blank())

distance_legend <- cowplot::get_legend(plot_msg_final)

ragg::agg_png(here::here("paper", "plots", "ex_msg.png"), width = 169, height = 75, units = "mm", res = 600)
print(
  cowplot::plot_grid(plot_msg_init + theme(legend.position = "none"),
                     plot_msg_typctm  + theme(legend.position = "none"),
                     plot_msg_final + theme(legend.position = "none"), 
                     distance_legend, nrow = 1, rel_widths = c(1, 1, 1, 0.4))
)
dev.off()


# Combined MSG and TSA plot ----
ragg::agg_png(here::here("paper", "plots", "ex_tsa_msg.png"), width = 169, height = 150, units = "mm", res = 600)
print(
  cowplot::plot_grid(
    cowplot::plot_grid(plot_tsa_init,
                       plot_tsa_typctm,
                       plot_tsa_final + theme(legend.position = "none"), 
                       area_legend, 
                       nrow = 1, 
                       rel_widths = c(1,1,1,0.4),
                       labels = c(LETTERS[1:3], NULL)),
  cowplot::plot_grid(plot_msg_init + theme(legend.position = "none"),
                     plot_msg_typctm  + theme(legend.position = "none"),
                     plot_msg_final + theme(legend.position = "none"), 
                     distance_legend, 
                     nrow = 1, 
                     rel_widths = c(1,1,1,0.4),
                     labels = c(LETTERS[4:6], NULL)),
  nrow = 2)
)
dev.off()

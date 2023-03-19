library(shadowtext)

dc_df <- data.frame(pressure = 1:5,
           depth = 1:5,
           cast = rep("downcast", 5),
           temperature = c(8.25,8,7,5,4),
           salinity = c(31, 31, 31.2, 31.5, 32.2),
           spd = c(rep(FALSE,4), TRUE)) |>
  dplyr::mutate(cumsum_salinity = c(0, cumsum(abs(diff(salinity)))))

uc_df <- data.frame(pressure = 1:8,
                    depth = 1:8,
                    cast = rep("upcast", 8),
                    temperature = c(8.5, 8, 6, 4, 2.5, 2.5, 2.5, 2.5),
                    salinity = c(31.5, 31.5, 31.5, 31.75, 32, 32.25, 32.5, 32.5),
                    spd = c(rep(FALSE,7), TRUE)) |>
  dplyr::mutate(cumsum_salinity = c(0, cumsum(abs(diff(salinity)))))

dcuc_df <- dplyr::bind_rows(dc_df, uc_df)

ex_dc <- oce::as.oce(dc_df)
                       
ex_uc <- oce::as.oce(uc_df)

ex_area <- gapctd::ts_area(dc = ex_dc,
                uc = ex_uc,
                return_sf = TRUE)

ex_area$sf$area <- sf::st_area(ex_area$sf)
ex_area$sf$L2 <- 1:nrow(ex_area$sf)
ex_area$cast_df$index <- c(1:8, 1:8)

ex_area_poly <- as.data.frame(sf::st_coordinates(ex_area$sf)) |>
  dplyr::inner_join(ex_area$sf |>
                      dplyr::select(-geometry)) |>
  dplyr::mutate(L2 = factor(L2))

s_profile <- ggplot() +
  geom_path(data = dcuc_df,
            mapping = aes(x = salinity, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = dcuc_df,
             mapping = aes(x = salinity, 
                           y = pressure, 
                           color = cast, 
                           shape = cast)) +
  geom_shadowtext(data = dcuc_df,
                  mapping = aes(x = salinity, 
                                y = pressure, 
                                color = cast,
                                label = pressure),
                  bg.color = "white",
                  vjust = 1.2,
                  hjust = -0.5) +
  scale_color_manual(values = c("grey50", "black"), name = "Cast") +
  scale_shape(name = "Cast") +
  scale_y_reverse(name = expression("Pressure"~(dbar^-1)), limits = c(9.6, 0)) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = c(31, 32.7)) +
  ggtitle("Salinity Profiles") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none")

t_profile <- ggplot() +
  geom_path(data = dcuc_df,
            mapping = aes(x = temperature, 
                          y = pressure, 
                          color = cast)) +
  geom_point(data = dcuc_df,
             mapping = aes(x = temperature, 
                           y = pressure, 
                           color = cast, 
                           shape = cast)) +
  geom_shadowtext(data = dcuc_df,
                  mapping = aes(x = temperature, 
                                y = pressure, 
                                color = cast,
                                label = pressure),
                  bg.color = "white",
                  vjust = 1.2,
                  hjust = -0.4) +
  scale_color_manual(values = c("grey50", "black"), name = "Cast") +
  scale_shape(name = "Cast") +
  scale_y_reverse(name = expression("Pressure"~(dbar^-1)), limits = c(9.6, 0)) +
  scale_x_continuous(name = expression('Temperature'~(degree*C)), limits = c(2,9)) +
  ggtitle("Temperature Profiles") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "none")

ts_profile <- ggplot() +
  geom_polygon(data = ex_area_poly,
               mapping = aes(x = X, 
                             y = Y, 
                             group = L2,
                             fill = "Area"), 
               alpha = 0.3) +
  geom_path(data = dcuc_df,
            mapping = aes(x = salinity, 
                          y = temperature, 
                          color = cast)) +
  geom_point(data = dcuc_df,
                  mapping = aes(x = salinity, 
                                y = temperature, 
                                color = cast,
                                shape = cast)) +
  geom_shadowtext(data = dcuc_df,
                  mapping = aes(x = salinity,
                                y = temperature, 
                                color = cast,
                                label = pressure),
                  bg.color = "white",
                  vjust = 1.2,
                  hjust = -0.4) +
  scale_color_manual(values = c("grey50", "black"), name = "Cast", guide = "none") +
  scale_shape(name = "Cast", guide = "none") +
  scale_fill_manual(name = NULL, values = "black") +
  # scale_fill_distiller(name = "Area", palette = "Purples", direction = 1) +
  scale_x_continuous(name = "Salinity (PSS-78)", limits = c(31, 32.7)) +
  scale_y_continuous(name = expression('Temperature'~(degree*C)), limits = c(2,9)) +
  ggtitle("Temperature-Salinity Area") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = c(0.7, 0.8))

spd_profile <- ggplot() + 
  geom_path(data = dcuc_df,
                     mapping = aes(x = pressure, 
                                   y = cumsum_salinity,
                                   color = cast)) +
  geom_point(data = dcuc_df,
            mapping = aes(x = pressure, 
                          y = cumsum_salinity,
                          color = cast,
                          shape = cast)) +
  geom_text(data = dcuc_df |>
                    dplyr::filter(spd),
             mapping = aes(x = pressure, 
                           y = cumsum_salinity,
                           color = cast,
                           label = "  SPD"),
            hjust = 0) +
  geom_text_repel(data = dcuc_df,
            mapping = aes(x = pressure, 
                          y = cumsum_salinity,
                          color = cast,
                          label = pressure),
            hjust = 0) +
  geom_point(data = dcuc_df,
             mapping = aes(x = pressure, 
                           y = cumsum_salinity,
                           color = cast,
                           shape = cast)) +
  scale_color_manual(values = c("grey50", "black"), name = "Cast") +
  scale_shape(name = "Cast") +
  scale_x_continuous(name = expression("Pressure"~(dbar^-1)), limits = c(0,10.5)) +
  scale_y_continuous(name = expression("Cumulative Sum of "~abs(Delta*S/Delta*P))) +
  ggtitle("Salinity Path Distance") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        axis.title = element_text(size = 9),
        axis.text = element_text(size = 8),
        legend.position = "right")

ts_legend <- cowplot::get_legend(spd_profile)

ragg::agg_png(filename = here::here("paper", "plots", "ex_optimization.png"), width = 140, height = 120, units = "mm", res = 600)
print(
cowplot::plot_grid(
  cowplot::plot_grid(t_profile, 
                     s_profile, 
                     ts_profile, 
                     spd_profile + theme(legend.position = "none"),
                     align = "hv",
                     labels = LETTERS[1:4]),
  cowplot::plot_grid(NULL, 
                     ts_legend, 
                     NULL, 
                     nrow = 3, 
                     rel_heights = c(0.2, 0.6, 0.2)),
  rel_widths = c(1, 0.2)
)
)
dev.off()



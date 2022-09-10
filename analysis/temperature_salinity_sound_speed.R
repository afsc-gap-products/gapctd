library(oce)
library(RNetCDF)
library(ggplot2)

bs_nc <- RNetCDF::open.nc(con = here::here("data", "EBS", "GAPCTD_2021_BS.nc"))
print.nc(bs_nc)

range(RNetCDF::var.get.nc(ncfile = bs_nc,
                    variable = "sea_floor_temperature"))
range(RNetCDF::var.get.nc(ncfile = bs_nc,
                          variable = "sea_floor_salinity"))
bs_mat <- matrix(c(RNetCDF::var.get.nc(ncfile = bs_nc,
                             variable = "sea_floor_temperature"),
         RNetCDF::var.get.nc(ncfile = bs_nc,
                             variable = "sea_floor_salinity")) ,ncol = 2)
bs_hull <- chull(bs_mat)
bs_coords <- bs_mat[c(bs_hull , bs_hull[1]), ]
bs_df <- as.data.frame(bs_coords) |>
  dplyr::rename(BT = V1,
                PSU = V2) |>
  dplyr::mutate(region = "2021 EBS/NBS")


goa_nc <- RNetCDF::open.nc(con = here::here("data", "GOA", "GAPCTD_2021_GOA.nc"))
print.nc(goa_nc)

goa_mat <- matrix(c(RNetCDF::var.get.nc(ncfile = goa_nc,
                                       variable = "sea_floor_temperature"),
                   RNetCDF::var.get.nc(ncfile = goa_nc,
                                       variable = "sea_floor_salinity")) ,ncol = 2)
goa_hull <- chull(goa_mat)
goa_coords <- goa_mat[c(goa_hull , goa_hull[1]), ]
goa_df <- as.data.frame(goa_coords) |>
  dplyr::rename(BT = V1,
                PSU = V2) |>
  dplyr::mutate(region = "2021 GOA")

bs_goa_df <- dplyr::bind_rows(bs_df, goa_df)

ss_df <- expand.grid(BT = seq(-1.8, 14, length = 150),
                     PSU = seq(20,36))

ss_df$SS <- oce::swSoundSpeed(salinity = ss_df$PSU,
                  temperature = ss_df$BT,
                  pressure = 50)

png(file = here::here("plots", "temperature_salinity_sound_speed.png"), width = 8, height = 6, units = "in", res = 300)
print(
ggplot() +
  geom_tile(data = ss_df,
            aes(x = BT,
                y = PSU,
                fill = SS)) +
  geom_path(data = bs_goa_df,
            aes(x = BT,
                y = PSU,
                linetype = region),
            size = rel(1)) +
  scale_color_manual() +
  scale_fill_viridis_c(name = expression("Sound speed"~(m~s^-1)), option = "F") +
  scale_x_continuous(name = expression("Gear temperature "~(degree*C)), expand = c(0,0)) +
  scale_y_continuous(name = "Salinity (PSS-78)", expand = c(0,0)) +
  scale_linetype(name = "Region") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20))
)
dev.off()

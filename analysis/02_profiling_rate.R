# Retrieve sample locations from netCDF file
library(gapctd)
library(RNetCDF)

# Load CTD data
ctd_dat <- dplyr::bind_rows(
  readRDS(file = here::here("paper", "data", "all_profiles", "GAPCTD_2021_EBS.rds")) |>
    dplyr::mutate(region = "EBS+NBS"),
  readRDS(file = here::here("paper", "data", "all_profiles","GAPCTD_2021_GOA.rds")) |>
    dplyr::mutate(region = "GOA"),
  readRDS(file = here::here("paper", "data", "all_profiles","GAPCTD_2022_AI.rds")) |>
    dplyr::mutate(region = "AI"),
  readRDS(file = here::here("paper", "data", "all_profiles","GAPCTD_2022_EBS.rds")) |>
    dplyr::mutate(region = "EBS+NBS"))

haul_files <- list.files(here::here("paper", "data", "haul_data"), full.names = TRUE)

# Load haul data
haul_dat <- data.frame()

for(ii in 1:length(haul_files)) {
  
  haul_dat <- haul_dat |>
    dplyr::bind_rows(
      readRDS(file = haul_files[ii]) |>
        janitor::clean_names()
    )
}

haul_dat <- dplyr::select(haul_dat, vessel, cruise, haul, bottom_depth, gear_depth) |>
  unique()


n_casts_df <- ctd_dat |>
  dplyr::select(vessel, cruise, haul, cast_direction) |>
  unique() |>
  dplyr::mutate(exists = 1) |>
  tidyr::pivot_wider(id_cols = c("vessel", "cruise", "haul"),
                     names_from = "cast_direction",
                     values_from = "exists",
                     values_fill = 0) |>
  dplyr::mutate(both = ifelse(upcast+downcast == 2, 1, 0))

n_casts_df |>
  dplyr::summarise(n_downcast = sum(downcast),
                   n_upcast = sum(upcast),
                   n_both = sum(both))

ggplot(ctd_dat |>
         dplyr::filter(depth < 150) |>
         dplyr::mutate(depth = floor(depth)),
       aes(x = depth,
           y = abs(velocity),
           linetype = cast_direction,
           color = factor(vessel))) +
  geom_smooth() +
  scale_y_continuous(name = "Profiling rate (m/s)", limits = c(0, 0.85), expand = c(0,0)) +
  scale_x_continuous(name = "Bin depth (m)",
                     limits = c(0, 150),
                     expand = c(0,0)) +
  scale_color_colorblind(name = "Vessel") +
  scale_linetype(name = "Cast") +
  facet_grid(~factor(region, levels = c("AI", "GOA", "EBS+NBS"))) +
  theme_bw() +
  theme(legend.position = "bottom")

# Profiling rate in the upper 50 meters and bottom 20 m
png(file = here::here("paper", "plots", "profiling_speed_by_depth.png"), width = 160, height = 120, units = "mm", res = 600)
print(
  cowplot::plot_grid(
    ggplot(ctd_dat |>
             dplyr::filter(depth < 41) |>
             dplyr::mutate(depth = plyr::round_any(depth, 2)),
           aes(x = depth, 
               y = abs(velocity),
               group = depth)) +
      geom_boxplot(size = rel(0.2),
                   outlier.size = rel(0.2)) +
      scale_y_continuous(name = "Profiling speed (m/s)", expand = c(0,0)) +
      scale_x_continuous(name = "Depth (m)",
                         limits = c(-1, 41)) +
      scale_color_colorblind(name = "Vessel") +
      scale_linetype(name = "Cast") +
      facet_grid(~factor(paste0("Surface ", region), levels = c("Surface AI", "Surface GOA", "Surface EBS+NBS"))) +
      theme_bw() +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill = "white")),
    ggplot(ctd_dat |>
             dplyr::inner_join(haul_dat) |>
             dplyr::filter(gear_depth - depth < 21) |>
             dplyr::filter(gear_depth - depth >= 0) |>
             dplyr::mutate(diff_depth = plyr::round_any(gear_depth - depth, 2)),
           aes(x = diff_depth, 
               y = abs(velocity),
               group = diff_depth)) +
      geom_boxplot(size = rel(0.2),
                   outlier.size = rel(0.2)) +
      scale_y_continuous(name = "Profiling speed (m/s)", expand = c(0,0)) +
      scale_x_continuous(name = "Distance from bottom (m)",
                         limits = c(-1, 21)) +
      scale_color_colorblind(name = "Vessel") +
      scale_linetype(name = "Cast") +
      facet_grid(~factor(paste0("Near-bottom ", region), levels = c("Near-bottom AI", "Near-bottom GOA", "Near-bottom EBS+NBS"))) +
      theme_bw() +
      theme(legend.position = "bottom",
            strip.background = element_rect(fill = "white")),
    nrow = 2
  )
)
dev.off()

# Effects of operating conditions on dynamic correction parameters

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


optimized_params_df <- dplyr::filter(ctd_dat, 
                                     processing_method != "Typical") |>
  dplyr::select(vessel, cruise, haul, region, processing_method, alpha_C, beta_C, temperature_offset) |>
  unique()

png(filename = here::here("paper", "plots", "dynamic_correction_params.png"), width = 160, height = 60, units = "mm",res = 600)
print(
cowplot::plot_grid(
  ggplot() +
    geom_vline(xintercept = -0.5, 
               linetype = 2, 
               size = rel(0.3)) +
    geom_density(data = optimized_params_df, 
                 mapping = aes(x = temperature_offset,
                               color = factor(processing_method, 
                                              levels = c("Typical CTM", "TSA", "SPD"))),
                 alpha = 0.7) +
    scale_y_continuous(name = "Density", 
                       expand = c(0, 0)) +
    scale_x_continuous(name = expression(t[T]), 
                       expand = c(0, 0)) +
    scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(3:4,6)], 
                       drop = FALSE) +
    theme_bw() +
    theme(legend.position = c(0.7, 0.82),
          legend.title = element_blank(),
          legend.key.size = unit(c(3), units = "mm"),
          legend.text = element_text(size = 7)),
  ggplot() +
    geom_vline(xintercept = 0.04, 
               linetype = 2, 
               size = rel(0.3)) +
    geom_density(data = dplyr::filter(optimized_params_df, 
                                      processing_method != "Typical CTM"), 
                 mapping = aes(x = alpha_C,
                               color = factor(processing_method, 
                                              levels = c("Typical CTM", "TSA", "SPD"))),
                 alpha = 0.7) +
    scale_y_continuous(name = " ", 
                       expand = c(0, 0)) +
    scale_x_log10(name = expression(alpha), 
                  expand = c(0, 0)) +
    scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(3:4,6)],
                       drop = FALSE) +
    theme_bw() +
    theme(legend.position = "none"),
  ggplot() +
    geom_vline(xintercept = 8, 
               linetype = 2, 
               size = rel(0.3)) +
    geom_density(data = dplyr::filter(optimized_params_df, 
                                      processing_method != "Typical CTM"), 
                 mapping = aes(x = beta_C^-1,
                               color = factor(processing_method, 
                                              levels = c("Typical CTM", "TSA", "SPD"))),
                 alpha = 0.7) +
    scale_y_continuous(name = " ", 
                       expand = c(0, 0)) +
    scale_x_continuous(name = expression(beta^-1), 
                       expand = c(0, 0)) +
    scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(3:4,6)], 
                       drop = FALSE) +
    theme_bw() +
    theme(legend.position = "none"),
  nrow = 1,
  align = "h"
)
)
dev.off()


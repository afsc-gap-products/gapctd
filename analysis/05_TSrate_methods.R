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


# Function to calculate summary statistics for gradients
calc_gradient <- function(x, z, window = 5, fun = max) {
  
  stopifnot("window must be an odd integer. " = window%%2 == 1)
  
  half_window <- (window-1)/2
  gradient <- numeric(length = length(x) - half_window*2)
  
  for(ii in (1+half_window):(length(x)-half_window)) {
    gradient[ii-half_window] <- abs(mean(diff(x[(ii-half_window):(ii+half_window)])/diff(z[(ii-half_window):(ii+half_window)])))
  }
  
  return(fun(gradient))
  
}

ts_gradient_df <- ctd_dat |>
  dplyr::group_by(vessel, cruise, haul, cast_direction, processing_method, alpha_C, beta_C, temperature_offset, region) |>
  dplyr::arrange(depth) |>
  dplyr::summarise(temperature_max_gradient_s = calc_gradient(x = temperature, z = as.numeric(timeS), window = 5),
                   salinity_max_gradient_s = calc_gradient(x = salinity, z = as.numeric(timeS), window = 5),
                   temperature_max_gradient_z = calc_gradient(x = temperature, z = depth, window = 5),
                   salinity_max_gradient_z = calc_gradient(x = salinity, z = depth, window = 5),
                   temperature_range = max(temperature)-min(temperature),
                   salinity_range = max(salinity)-min(salinity))

ts_gradient_df |>
  dplyr::group_by(region) |>
  dplyr::filter(!is.infinite(temperature_max_gradient_z),
                !is.infinite(temperature_max_gradient_s),
                !is.infinite(salinity_max_gradient_z),
                !is.infinite(salinity_max_gradient_s)) |>
  dplyr::summarise(mean_temperature_max_gradient_z = mean(temperature_max_gradient_z, na.rm = TRUE),
                   mean_salinity_max_gradient_z = mean(salinity_max_gradient_z, na.rm = TRUE))

saveRDS(ts_gradient_df, 
        file = here::here("paper", "output", "TS_gradient_by_cast.rds"))

png(file = here::here("paper", "plots", "method_versus_TSrate.png"), width = 120, height = 120, units = "mm", res = 600)
print(
  cowplot::plot_grid(
    ggplot() +
      geom_boxplot(data = ts_gradient_df,
                   mapping = aes(x = region, 
                                 y = temperature_max_gradient_s)) +
      scale_y_continuous(name = expression("Max. T rate "~(degree*C/s)),
                         limits = c(0, 2.5)
      ) +
      scale_x_discrete(name = "Region") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8)),
    ggplot() +
      geom_boxplot(data = ts_gradient_df,
                   mapping = aes(x = region, 
                                 y = temperature_max_gradient_z)) +
      scale_y_continuous(name = expression("Max. T rate "~(degree*C/s)),
                         limits = c(0, 2.5)
      ) +
      scale_x_discrete(name = "Region") +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8)),
    ggplot() +
      geom_boxplot(data = ts_gradient_df,
                   mapping = aes(x = factor(processing_method, 
                                            levels = c("All", "Typical", "Typical CTM", "TSA", "SPD"),
                                            labels = c("All", "Typ.", "Typ. CTM", "TSA", "SPD")), 
                                 y = temperature_max_gradient_s,
                                 color = factor(processing_method, 
                                                levels = c("All", "Typical", "Typical CTM", "TSA", "SPD"),
                                                labels = c("All", "Typ.", "Typ. CTM", "TSA", "SPD")))) +
      scale_y_continuous(name = expression("Max. T rate "~(degree*C/s)),
                         limits = c(0, 2.5)
      ) +
      scale_x_discrete(name = "Processing method") +
      scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(1:4,6)], drop = FALSE) +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8)),
    ggplot() +
      geom_boxplot(data = ts_gradient_df,
                   mapping = aes(x = factor(processing_method, 
                                            levels = c("All", "Typical", "Typical CTM", "TSA", "SPD"),
                                            labels = c("All", "Typ.", "Typ. CTM", "TSA", "SPD")), 
                                 y = temperature_max_gradient_z,
                                 color = factor(processing_method, 
                                                levels = c("All", "Typical", "Typical CTM", "TSA", "SPD"),
                                                labels = c("All", "Typ.", "Typ. CTM", "TSA", "SPD")))) +
      scale_y_continuous(name = expression("Max. T rate "~(degree*C/m)),
                         limits = c(0, 2.5)
      ) +
      scale_x_discrete(name = "Processing method") +
      scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(1:4,6)], drop = FALSE) +
      theme_bw() +
      theme(legend.position = "none",
            axis.title = element_text(size = 9),
            axis.text = element_text(size = 8)),
    ncol = 2,
    labels = LETTERS[1:4]
  )
)
dev.off()


write.csv(
ts_gradient_df |>
  dplyr::ungroup() |>
  dplyr::group_by(processing_method) |>
  dplyr::filter(!is.infinite(temperature_max_gradient_z),
                !is.infinite(temperature_max_gradient_s),
                !is.infinite(salinity_max_gradient_z),
                !is.infinite(salinity_max_gradient_s)) |>
  dplyr::summarise(mean_temperature_max_gradient_z = mean(temperature_max_gradient_z, na.rm = TRUE),
                   mean_temperature_max_gradient_s = mean(temperature_max_gradient_s, na.rm = TRUE),
                   mean_salinity_max_gradient_z = mean(salinity_max_gradient_z, na.rm = TRUE),
                   mean_salinity_max_gradient_s = mean(salinity_max_gradient_s, na.rm = TRUE)),
file = here::here("paper", "output", "max_gradient_by_method.csv"),
row.names = FALSE
)



ts_gradient_df |>
  dplyr::filter(processing_method != "Typical") |>
  dplyr::ungroup() |>
  dplyr::group_by(processing_method) |>
  dplyr::summarise(tgrad_vs_alpha = cor(temperature_max_gradient_s, alpha_C, use = "complete.obs"),
                   tgrad_vs_beta = cor(temperature_max_gradient_s, beta_C, use = "complete.obs"),
                   tgrad_vs_ab = cor(temperature_max_gradient_s, beta_C*alpha_C, use = "complete.obs"),
                   tgrad_vs_toffset = cor(temperature_max_gradient_s, temperature_offset, use = "complete.obs"))




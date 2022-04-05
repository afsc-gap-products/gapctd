# Estimate CTD temperature channel alignment for 2021 EBS/NBS vessels
# Sean Rohan <sean.rohan@noaa.gov>
# Last update: April 4, 2022
# gapctd version 0.1.2

library(gapctd)
library(ggthemes)
library(cowplot)
library(sf)

vessel <- 162
year <- 2021
region <- "BS"
ctd_dir <- "G:/RACE_CTD/data/2021/ebs/v162"
# Establish Oracle connection connection
channel <- gapctd::get_connected(schema = "AFSC")

gapctd::run_method(vessel = vessel,
                   year = year,
                   region = region,
                   ctd_dir = ctd_dir,
                   channel = channel,
                   processing_method = "sbe19plus_v0")


alignment_df <- gapctd::run_alignment_calcs(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                      full.names = TRUE, pattern = "downcast.cnv"),
                                                           list.files(here::here("output", "sbe19plus_v0"), 
                                                                      full.names = TRUE, pattern = "upcast.cnv"))),
                                    make_diagnostic_plots = TRUE,
                                    min_pressure = 4)

png(filename = "./plots/alignment_162_2021.png", width = 8, height = 8, units = "in", res = 120)
print(
cowplot::plot_grid(
  ggplot(data = alignment_df,
         mapping = aes(x = best_adj, 
                       fill = cast_direction)) +
    geom_histogram(bins = 50) +
    geom_vline(data = alignment_df |>
                 dplyr::group_by(cast_direction) |>
                 dplyr::summarise(best_adj = mean(best_adj, na.rm = TRUE)),
               mapping = aes(xintercept = best_adj, group = cast_direction),
               color = "black",
               linetype = 3) +
    facet_grid(~cast_direction) +
    scale_fill_tableau(name = "Direction") +
    scale_color_tableau(name = "Direction") +
    scale_x_continuous(name = "Est. temperature alignment (s)") +
    scale_y_continuous(name = "Casts (#)") +
    ggtitle("2021 EBS/NBS Alaska Knight") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)),
  cowplot::plot_grid(ggplot() +
                       geom_histogram(data = alignment_df,
                                      aes(x = profile_rate,
                                          fill = cast_direction),
                                      bins = 100) +
                       geom_vline(data = alignment_df |>
                                    dplyr::group_by(cast_direction) |>
                                    dplyr::summarise(profile_rate = mean(profile_rate, na.rm = TRUE)),
                                  mapping = aes(xintercept = profile_rate, group = cast_direction),
                                  color = "black",
                                  linetype = 3) +
                       scale_x_continuous(name = expression("Mean profile velocity"~(m~s^-1))) +
                       scale_color_tableau(name = "Direction") +
                       scale_fill_tableau(name = "Direction") +
                       scale_y_continuous(name = "Casts (#)") +
                       theme_bw() +
                       theme(legend.position = "none"),
                     ggplot(data = alignment_df,
                            mapping = aes(x = abs(profile_rate),
                                          y = best_adj,
                                          color = cast_direction)) +
                       geom_point() +
                       geom_smooth() +
                       scale_y_continuous(name = "Est. temperature alignment (s)") +
                       scale_x_continuous(name = expression("Mean profile speed "~(m~s^-1))) +
                       scale_color_tableau(name = "Direction") +
                       theme_bw() +
                       theme(legend.position = "none"),
                     ncol = 2),
  nrow = 2)
)
dev.off()

vessel <- 94
year <- 2021
region <- "BS"
ctd_dir <- "G:/RACE_CTD/data/2021/ebs/v94"
# Establish Oracle connection connection
channel <- gapctd::get_connected(schema = "AFSC")

gapctd::run_method(vessel = vessel,
                   year = year,
                   region = region,
                   ctd_dir = ctd_dir,
                   channel = channel,
                   processing_method = "sbe19plus_v0")

alignment_df <- gapctd::run_alignment_calcs(profile_files = sort(c(list.files(here::here("output", "sbe19plus_v0"), 
                                                                      full.names = TRUE, pattern = "downcast.cnv"),
                                                           list.files(here::here("output", "sbe19plus_v0"), 
                                                                      full.names = TRUE, pattern = "upcast.cnv"))),
                                    make_diagnostic_plots = TRUE,
                                    min_pressure = 4)

png(filename = here::here("plots", "alignment_94_2021.png"), width = 8, height = 8, units = "in", res = 120)
print(
cowplot::plot_grid(
  ggplot(data = alignment_df,
         mapping = aes(x = best_adj, 
                       fill = cast_direction)) +
    geom_histogram(bins = 50) +
    geom_vline(data = alignment_df |>
                 dplyr::group_by(cast_direction) |>
                 dplyr::summarise(best_adj = mean(best_adj, na.rm = TRUE)),
               mapping = aes(xintercept = best_adj, group = cast_direction),
               color = "black",
               linetype = 3) +
    facet_grid(~cast_direction) +
    scale_fill_tableau(name = "Direction") +
    scale_color_tableau(name = "Direction") +
    scale_x_continuous(name = "Est. temperature alignment (s)") +
    scale_y_continuous(name = "Casts (#)") +
    ggtitle("2021 EBS/NBS Vesteraalen") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)),
  cowplot::plot_grid(ggplot() +
                       geom_histogram(data = alignment_df,
                                      aes(x = profile_rate,
                                          fill = cast_direction),
                                      bins = 100) +
                       geom_vline(data = alignment_df |>
                                    dplyr::group_by(cast_direction) |>
                                    dplyr::summarise(profile_rate = mean(profile_rate, na.rm = TRUE)),
                                  mapping = aes(xintercept = profile_rate, group = cast_direction),
                                  color = "black",
                                  linetype = 3) +
                       scale_x_continuous(name = expression("Mean profile velocity"~(m~s^-1))) +
                       scale_color_tableau(name = "Direction") +
                       scale_fill_tableau(name = "Direction") +
                       scale_y_continuous(name = "Casts (#)") +
                       theme_bw() +
                       theme(legend.position = "none"),
                     ggplot(data = alignment_df,
                            mapping = aes(x = abs(profile_rate),
                                          y = best_adj,
                                          color = cast_direction)) +
                       geom_point() +
                       geom_smooth() +
                       scale_y_continuous(name = "Est. temperature alignment (s)") +
                       scale_x_continuous(name = expression("Mean profile speed "~(m~s^-1))) +
                       scale_color_tableau(name = "Direction") +
                       theme_bw() +
                       theme(legend.position = "none"),
                     ncol = 2),
  nrow = 2)
)
dev.off()
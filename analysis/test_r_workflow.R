library(gapctd)

vessel <- 94
cruise <- c(202101, 202202)

# Load haul data
# gapctd:::get_haul_data(channel = gapctd::get_connected(schema = "AFSC"),
#                        vessel = vessel,
#                        cruise = cruise,
#                        tzone = "America/Anchorage")

# Load haul data
haul_df <- readRDS(file = here::here("output", paste0("HAUL_DATA_", vessel, "_", paste(cruise, collapse = "_"), ".rds")))

# Load CTD data
ctd_dat <- read.oce(file = here::here("data", "sbe19plus_01908103_2021_06_03_94_0001_raw.cnv"))

test <- gapctd:::run_gapctd(x = ctd_dat, 
                            haul_df = haul_df, 
                            return_stages = c("final"), 
                            ctd_tz = "America/Anchorage")


test <- dplyr::bind_rows(
  as.data.frame(downcast_uncorr@data) |>
    dplyr::mutate(direction = "downcast", corr = "uncorrected"),
  as.data.frame(downcast_binned@data) |>
    dplyr::mutate(direction = "downcast", corr = "corrected"),
  as.data.frame(upcast_uncorr@data) |>
    dplyr::mutate(direction = "upcast", corr = "uncorrected"),
  as.data.frame(upcast_binned@data) |>
    dplyr::mutate(direction = "upcast", corr = "corrected")
)
x <- downcast_binned






ggplot() +
  geom_path(data = test |>
              dplyr::filter(depth > 1), 
            aes(x = salinity, 
                             y = depth,
                             color = corr)) +
  scale_y_reverse() +
  facet_grid(~direction)





    
    
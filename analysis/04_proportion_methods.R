# Make a table of gapctd data processing methods used by survey and year

# Retrieve sample locations from netCDF file
library(gapctd)


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

proportion_direction_df <- ctd_dat |>
  dplyr::select(vessel, cruise, haul, processing_method, region) |>
  dplyr::mutate(year = floor(cruise/100)) |>
  unique() |>
  dplyr::group_by(region, year) |>
  dplyr::summarise(n_cruise = n()) |>
  dplyr::inner_join(
    ctd_dat |>
      dplyr::select(vessel, cruise, haul, processing_method, region, cast_direction) |>
      dplyr::mutate(year = floor(cruise/100)) |>
      unique() |>
      dplyr::group_by(region, year, cast_direction) |>
      dplyr::summarise(n_direction = n()) |>
      tidyr::pivot_wider(values_from = "n_direction", names_from = "cast_direction", id_cols = c("region", "year")),
    by = c("region", "year")
  )

sum(proportion_direction_df$downcast) / sum(proportion_direction_df$n_cruise)
sum(proportion_direction_df$upcast) / sum(proportion_direction_df$n_cruise)

(sum(proportion_direction_df$downcast)+sum(proportion_direction_df$upcast)-sum(proportion_direction_df$n_cruise)) / sum(proportion_direction_df$n_cruise)


# Make a table of survey x method with proportion of the method for 
proportion_method_df <- ctd_dat |>
  dplyr::select(vessel, cruise, haul, processing_method, region) |>
  dplyr::mutate(year = floor(cruise/100)) |>
  unique() |>
  dplyr::group_by(region, processing_method, year) |>
  dplyr::summarise(n_method = n()) |>
  dplyr::inner_join(
    ctd_dat |>
      dplyr::select(vessel, cruise, haul, processing_method, region) |>
      dplyr::mutate(year = floor(cruise/100)) |>
      unique() |>
      dplyr::group_by(region, year) |>
      dplyr::summarise(n_cruise = n()),
    by = c("region", "year")
  ) |>
  dplyr::mutate(proportion = n_method/n_cruise) |>
  dplyr::mutate(Survey = paste0(region, " ", year),
                proportion = formatC(proportion*100, format = "f", digits = 1)) |>
  tidyr::pivot_wider(id_cols = "Survey",
                     names_from = "processing_method",
                     values_from = "proportion",
                     values_fill = "0.0") |>
  dplyr::select(Survey, Typical, `Typical CTM`, TSA, SPD)

ctd_dat |>
  dplyr::select(vessel, cruise, haul, processing_method, region) |>
  dplyr::mutate(year = floor(cruise/100)) |>
  unique() |>
  dplyr::group_by(processing_method) |>
  dplyr::summarise(n_method = n()) |>
  dplyr::mutate(p_method = n_method/sum(n_method))

proportion_method_df

write.csv(x = proportion_method_df,
          file = here::here("paper", "plots", "method_by_region.csv"),
          row.names = FALSE)

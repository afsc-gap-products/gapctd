# Packaging historical CTD data for GAP_PRODUCTS
library(gapctd)

channel <- gapctd::get_connected("AFSC")

hauljoins <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "select h.hauljoin, h.haul, c.vessel_id vessel, c.cruise 
  from gap_products.akfin_haul h, gap_products.akfin_cruise c 
  where c.cruise > 202100 
  and c.cruisejoin = h.cruisejoin"
  )

names(hauljoins) <- tolower(names(hauljoins))

# Lookup table for assigning direction codes
direction_lut <-
  data.frame(
    cast_direction = c("downcast", "bottom", "upcast"),
    direction = c(-1, 0, 1)
    )

# Lookup table for variable codes

variable_codes <- 
  RODBC::sqlQuery(
    channel = channel,
    query = "select * from gap_products.ctd_variable_codes"
  ) |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(
    var_name = tolower(var_name),
    var_name = stringr::str_replace(var_name, pattern = " ", replacement = "_")
  ) |>
  dplyr::filter(variable %in% c(1, 2, 5, 7, 8, 12))


rds_paths <- list.files(path = here::here("data"), pattern = ".rds", full.names = TRUE)

cast_data <- 
  lapply(X = rds_paths, FUN = function(x) readRDS(x)) |>
  do.call(what = bind_rows) |>
  dplyr::select(any_of(c("vessel", "cruise", "haul", "depth", "cast_direction", 
                         "temperature", "conductivity", "salinity", "pressure", 
                         "pH", "oxygen"))) |>
  dplyr::rename(dissolved_oxygen = oxygen, ph = pH) |>
  tidyr::pivot_longer(
    cols = any_of(c("temperature", "conductivity", "salinity", "pressure", 
                    "ph", "dissolved_oxygen")),
    names_to = "var_name",
    values_to = "value"
  ) |>
  dplyr::filter(!is.na(value))

cast_data <- 
  dplyr::inner_join(
    cast_data,
    variable_codes
  ) |>
  dplyr::inner_join(
    hauljoins
  ) |>
  dplyr::inner_join(
    direction_lut
  ) |>
  dplyr::select(hauljoin, direction, depth_m = depth, variable, value) |>
  unique()

# Ensure there's are a maximum of two casts per haul
checklen <- dplyr::select(cast_data, hauljoin, direction) |>
  unique()

check_vector_length <- 
  table(checklen) |>
  as.vector()

any(check_vector_length > 2)

object.size(cast_data)


# Bottom data ----

library(ncdf4)

nc_file_paths <- list.files(here::here("data"), pattern = ".nc", full.names = TRUE)

bottom_data <- 
  lapply(X = nc_file_paths, FUN = function(x) {
  
  con <- ncdf4::nc_open(filename = x)
  
  out <- 
    data.frame(
      vessel = ncdf4::ncvar_get(con, "vessel"),
      cruise = ncdf4::ncvar_get(con, "cruise"),
      haul = ncdf4::ncvar_get(con, "haul"),
      depth_m = ncdf4::ncvar_get(con, "haul_depth"),
      salinity = ncdf4::ncvar_get(con, "sea_floor_practical_salinity"),
      temperature = ncdf4::ncvar_get(con, "sea_floor_temperature")
    )
  
  dissolved_oxygen <- try(ncdf4::ncvar_get(con, "sea_floor_dissolved_oxygen"), silent = TRUE)
  
  if(!is(dissolved_oxygen, "try-error")) {
    dissolved_oxygen[dissolved_oxygen > 1000] <- NA
    out$dissolved_oxygen <- dissolved_oxygen
  }
  
  ph <- try(ncdf4::ncvar_get(con, "sea_floor_ph_reported_on_total_scale"), silent = TRUE)
  
  if(!is(ph, "try-error")) {
    ph[ph > 10] <- NA
    out$ph <- ph
  }
  
  nc_close(con)
  
  out
  
} ) |>
  do.call(what = bind_rows) |>
  tidyr::pivot_longer(
    cols = any_of(c("temperature", "conductivity", "salinity", "pressure", 
                    "ph", "dissolved_oxygen")),
    names_to = "var_name",
    values_to = "value"
  ) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(cast_direction = "bottom")


bottom_data <- 
  dplyr::inner_join(
    bottom_data,
    variable_codes
  ) |>
  dplyr::inner_join(
    hauljoins
  ) |>
  dplyr::inner_join(
    direction_lut
  ) |>
  dplyr::select(hauljoin, direction, depth_m, variable, value) |>
  unique() 
  
ctd_data_2021_2024 <-
  dplyr::bind_rows(bottom_data, cast_data) |>
  dplyr::arrange(hauljoin, direction, depth_m)

saveRDS(object = ctd_data_2021_2024, file = here::here("data", "GAPCTD_all_casts_2021_2024.rds"))


library(gapctd)

channel <- gapctd::get_connected("AFSC")

ctd_data_2021_2024 <- 
  readRDS(file = "GAPCTD_all_casts_2021_2024.rds")

RODBC::sqlSave(
  channel = channel, 
  dat = final_values, 
  tablename = "RACE_DATA.EDIT_CTD_DATA",
  append = TRUE,
  rownames = FALSE, 
  colnames = FALSE, 
  verbose = FALSE,
  safer = FALSE, 
  addPK = FALSE, 
  fast = TRUE, 
  test = FALSE, 
  nastring = NULL
)


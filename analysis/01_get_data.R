# This script retrieves data from AFSC's database and can only be run internally.

library(gapctd) # install from github: remotes::install_github("afsc-gap-products/gapctd")
library(lme4)
library(rstanarm)
library(ggthemes)
library(RNetCDF)

if(!dir.exists(here::here("plots"))) {
  dir.create(here::here("plots"))
}

nc_path <- here::here("data", "GOA", "GAPCTD_2021_GOA.nc")  # Change to your path if you run

# Connect to oracle
channel <- gapctd::get_connected(schema = "AFSC")

# Retrieve trawl height data
trawl_height_df <- RODBC::sqlQuery(channel = channel, query = 
                                     "select a.vessel_id vessel, a.cruise, b.haul, b.net_number,
			d.date_time,
            d.edit_value net_height
			from race_data.cruises a, 
            race_data.hauls b, 
            race_data.net_mensuration_headers c, 
			race_data.net_mensurations d, 
            race_data.datum_codes e, 
            race_data.surveys f, 
			race_data.survey_definitions g 
            where g.survey_definition_id = f.survey_definition_id 
			and year = 2021
            and g.survey_definition_id = 47 
            and f.survey_id = a.survey_id 
			and b.cruise_id = a.cruise_id 
            and c.haul_id = b.haul_id
            and c.data_type_id = 3
            and g.survey_definition_id = 47
            and d.net_mensuration_header_id = c.net_mensuration_header_id
            and e.datum_code = d.datum_code
			and e.use_in_analysis = 'Y'")

names(trawl_height_df) <- tolower(names(trawl_height_df))

# Convert time zone
trawl_height_df$date_time <- lubridate::force_tz(trawl_height_df$date_time, tz = "UTC")

# Get unique net numbers
net_number_df <- trawl_height_df |>
  dplyr::select(vessel, cruise, haul, net_number) |>
  unique()

# Get eq and haulback events
events_df <- 
  RODBC::sqlQuery(channel = channel, 
                  query = paste0("select a.vessel_id vessel, 
                        a.cruise cruise, 
                         b.haul haul,
                         c.edit_date_time event_time, 
                         c.event_type_id
                         from 
                         race_data.cruises a,
                         race_data.hauls b,
                         race_data.events c,
                         race_data.datum_codes d
                         where 
                         a.cruise = 202101 
                                 and a.vessel_id = 176
                                 and b.cruise_id = a.cruise_id
                          and c.haul_id = b.haul_id
                          and c.datum_code = d.datum_code
                          and d.use_in_analysis = 'Y'")) |>
  dplyr::inner_join(data.frame(EVENT_TYPE_ID = c(4,6),
                               EVENT_NAME = c("eq", "hb"))) |>
  dplyr::mutate(EVENT_TIME = lubridate::force_tz(EVENT_TIME, tz = "America/Anchorage"))

names(events_df) <- tolower(names(events_df))


# Filter net mensuration based on eq and haulback
trawl_height_df <- events_df |>
  dplyr::select(-event_type_id) |>
  tidyr::pivot_wider(names_from = event_name, values_from = event_time) |>
  dplyr::inner_join(trawl_height_df)

trawl_height_df <- trawl_height_df |>
  dplyr::filter(date_time > eq & date_time < hb)

# Get 2021 GOA cTD data
nc <- RNetCDF::open.nc(con = nc_path)

goa_haul_df <- data.frame(vessel = RNetCDF::var.get.nc(ncfile = nc, variable = "vessel"),
                          haul = RNetCDF::var.get.nc(ncfile = nc, variable = "haul"),
                          sound_speed = RNetCDF::var.get.nc(ncfile = nc, variable = "sea_floor_sound_speed_in_sea_water")) |>
  unique()


# Find hauls with and without CTD data
trawl_height_summary <- trawl_height_df |>
  dplyr::group_by(vessel, cruise, haul) |>
  dplyr::summarise(duration = max(date_time) - min(date_time),
                   n = n(),
                   mean_net_height = mean(net_height),
                   median_net_height = median(net_height)) |>
  dplyr::mutate(duration_sec = as.numeric(duration)) |>
  dplyr::ungroup() |>
  dplyr::filter(duration_sec > 0)

trawl_height_summary <- dplyr::bind_rows(
  dplyr::inner_join(trawl_height_summary, 
                    goa_haul_df) |>
    dplyr::mutate(ctd = "yes"),
  dplyr::anti_join(trawl_height_summary, 
                   goa_haul_df) |>
    dplyr::mutate(ctd = "no"))

saveRDS(trawl_height_df, file = here::here("paper", "data", "trawl_height_df.rds"))
saveRDS(net_number_df, file = here::here("paper", "data", "net_number_df.rds"))
saveRDS(trawl_height_summary, file = here::here("paper", "data", "trawl_height_summary.rds"))

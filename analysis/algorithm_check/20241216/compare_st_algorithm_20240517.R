# Example temperature algorithm
#
# This script is intended to demonstrate surface temperature calculations for the survey.
# Created by Sean Rohan
# May 17, 2024

library(gapctd) # GitHub: afsc-gap-products/gapctd (Version 2.1.3)

channel <- gapctd::get_connected(schema = "AFSC")

dirpath <- here::here("analysis", "algorithm_check", "20240517")

fpath <- list.files(path = dirpath,
                    full.names = TRUE,
                    pattern = ".BTD")

test_hauls <- readxl::read_xlsx(here::here(dirpath,
                                      "Surface_Temp_Algorithm_Testing_R_to_Python_Javascript.xlsx"))

names(test_hauls) <- toupper(names(test_hauls))

test_hauls$RACEBASE_ST <- -999
test_hauls$RACE_DATA_ST <- -999

for(ii in 1:nrow(test_hauls)) {
  
  haul <- RODBC::sqlQuery(channel = channel,
                          query = paste0("select vessel, cruise, haul, surface_temperature 
                                         from 
                                         racebase.haul 
                                         where cruise = ", test_hauls$CRUISE[ii],
                                         "and vessel = ", test_hauls$VESSEL[ii],
                                         "and haul = ", test_hauls$HAUL[ii])
  )
  
  events <- RODBC::sqlQuery(channel = channel,
                            query = paste0("select c.vessel_id vessel, c.cruise, h.haul, e.date_time, e.event_type_id, et.name 
                          from race_data.hauls h, race_data.cruises c, race_data.events e, 
                          race_data.event_types et
                          where c.cruise = ", test_hauls$CRUISE[ii],
                                           "and c.vessel_id = ", test_hauls$VESSEL[ii],
                                           "and h.haul = ", test_hauls$HAUL[ii], 
                                           "and c.cruise_id = h.cruise_id 
                          and h.haul_id = e.haul_id 
                          and e.event_type_id = et.event_type_id 
                          and e.event_type_id = 6;"))
  
  upcast_data <- RODBC::sqlQuery(channel = channel,
                                 query = paste0("
                select c.vessel_id vessel, c.cruise, h.haul, bt.date_time, bt.temperature, bt.depth 
                from race_data.hauls h, race_data.cruises c, race_data.bathythermics bt, 
                race_data.bathythermic_headers bth 
                where c.cruise = ", test_hauls$CRUISE[ii],
                                                "and c.vessel_id = ", test_hauls$VESSEL[ii],
                                                "and h.haul = ", test_hauls$HAUL[ii], 
                                                "and bt.depth >= 0 
                and bt.temperature < 30 
                and bt.temperature > -1.94
                and c.cruise_id = h.cruise_id
                and h.haul_id = bth.haul_id
                and bth.bathythermic_header_id = bt.bathythermic_header_id 
                order by date_time;")
  ) |>
    dplyr::filter(DATE_TIME > events$DATE_TIME[events$EVENT_TYPE_ID == 6])
  
  test_hauls$RACE_DATA_ST[ii] <- gapctd::calc_fixed_depth_var(depth = upcast_data$DEPTH,
                                                              var = upcast_data$TEMPERATURE,
                                                              ref_depth = 1)
  
  test_hauls$RACEBASE_ST[ii] <- haul$SURFACE_TEMPERATURE
  
}


test_hauls_btd <- data.frame(VESSEL = NA, 
                             CRUISE = NA, 
                             HAUL = NA, 
                             BTD_ST = rep(-999, length(fpath)))


for(jj in 1:length(fpath)) {
  
  btd <- read.csv(file = fpath[jj]) |>
    dplyr::mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%m/%d/%Y %H:%M:%S")+3600*8) 
  
  test_hauls_btd$CRUISE[jj] <- btd$CRUISE[1]
  
  test_hauls_btd$VESSEL[jj] <- btd$VESSEL[1]
  
  test_hauls_btd$HAUL[jj] <- btd$HAUL[1]

  
  events <- RODBC::sqlQuery(channel = channel,
                            query = paste0("select c.vessel_id vessel, c.cruise, h.haul, 
                            e.date_time, e.event_type_id, et.name 
                          from race_data.hauls h, race_data.cruises c, race_data.events e, 
                          race_data.event_types et
                          where c.cruise = ", btd$CRUISE[1],
                                           "and c.vessel_id = ", btd$VESSEL[1],
                                           "and h.haul = ", btd$HAUL[1], 
                                           "and c.cruise_id = h.cruise_id 
                          and h.haul_id = e.haul_id 
                          and e.event_type_id = et.event_type_id 
                          and e.event_type_id = 6;"))
  
  btd_upcast <- btd |>
    dplyr::filter(DATE_TIME > events$DATE_TIME[events$EVENT_TYPE_ID == 6],
                  TEMPERATURE < 30,
                  TEMPERATURE > -1.94,
                  DEPTH >= 0)
  
  test_hauls_btd$BTD_ST[jj] <- gapctd::calc_fixed_depth_var(depth = btd_upcast$DEPTH,
                                                            var = btd_upcast$TEMPERATURE,
                                                            ref_depth = 1)
  
  
}


dplyr::full_join(test_hauls, 
                 test_hauls_btd) |>
write.csv(file = here::here(dirpath,  "st_comparison_20240517.csv"), 
          row.names = FALSE)


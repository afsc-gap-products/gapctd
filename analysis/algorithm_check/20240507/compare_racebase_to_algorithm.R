# Example temperature algorithm
#
# This script is intended to demonstrate surface temperature calculations for the survey.


# Created by Sean Rohan
# May 7, 2024

library(gapctd) # Version 2.1.3

channel <- gapctd::get_connected(schema = "AFSC")

test_hauls <- data.frame(vessel = c(148, 162, 134), 
           cruise = c(202201, 202301, 202301), 
           haul = c(162, 176, 2),
           btd_file = c("HAUL0162.BTD", "HAUL0176.BTD", "HAUL0002.BTD"),
           racebase_st = NA,
           race_data_st = NA,
           btd_st = NA)

for(ii in 1:nrow(test_hauls)) {
  
  haul <- RODBC::sqlQuery(channel = channel,
                          query = paste0("select vessel, cruise, haul, surface_temperature 
                                         from 
                                         racebase.haul 
                                         where cruise = ", test_hauls$cruise[ii],
                                         "and vessel = ", test_hauls$vessel[ii],
                                         "and haul = ", test_hauls$haul[ii])
  )
  
  events <- RODBC::sqlQuery(channel = channel,
                            query = paste0("select c.vessel_id vessel, c.cruise, h.haul, e.date_time, e.event_type_id, et.name 
                          from race_data.hauls h, race_data.cruises c, race_data.events e, 
                          race_data.event_types et
                          where c.cruise = ", test_hauls$cruise[ii],
                                           "and c.vessel_id = ", test_hauls$vessel[ii],
                                           "and h.haul = ", test_hauls$haul[ii], 
                                           "and c.cruise_id = h.cruise_id 
                          and h.haul_id = e.haul_id 
                          and e.event_type_id = et.event_type_id 
                          and e.event_type_id = 6;"))
  
  btd <- read.csv(file = here::here("assets", test_hauls$btd_file[ii])) |>
    dplyr::mutate(DATE_TIME = as.POSIXct(DATE_TIME, format = "%m/%d/%Y %H:%M:%S")+3600*8) |>
    dplyr::filter(DATE_TIME > events$DATE_TIME[events$EVENT_TYPE_ID == 6],
                  TEMPERATURE < 30,
                  TEMPERATURE > -1.94,
                  DEPTH >= 0)
  
  upcast_data <- RODBC::sqlQuery(channel = channel,
                                 query = paste0("
                select c.vessel_id vessel, c.cruise, h.haul, bt.date_time, bt.temperature, bt.depth 
                from race_data.hauls h, race_data.cruises c, race_data.bathythermics bt, 
                race_data.bathythermic_headers bth 
                where c.cruise = ", test_hauls$cruise[ii],
                                                "and c.vessel_id = ", test_hauls$vessel[ii],
                                                "and h.haul = ", test_hauls$haul[ii], 
                                                "and bt.depth >= 0 
                and bt.temperature < 30 
                and bt.temperature > -1.94
                and c.cruise_id = h.cruise_id
                and h.haul_id = bth.haul_id
                and bth.bathythermic_header_id = bt.bathythermic_header_id 
                order by date_time;")
  ) |>
    dplyr::filter(DATE_TIME > events$DATE_TIME[events$EVENT_TYPE_ID == 6])
  
  test_hauls$race_data_st[ii] <- gapctd::calc_fixed_depth_var(depth = upcast_data$DEPTH,
                                                              var = upcast_data$TEMPERATURE,
                                                              ref_depth = 1)
  
  test_hauls$btd_st[ii] <- gapctd::calc_fixed_depth_var(depth = btd$DEPTH,
                                                        var = btd$TEMPERATURE,
                                                        ref_depth = 1)
  
  test_hauls$racebase_st[ii] <- haul$SURFACE_TEMPERATURE
  
}

write.csv(test_hauls, file = here::here("assets", "st_comparison_20240507.csv"), row.names = FALSE)






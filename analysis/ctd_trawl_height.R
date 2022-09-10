# Load packages

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

# Cumulative trawls on the haul

n_trawls_df <- trawl_height_df |>
  dplyr::select(haul, net_number, eq) |>
  unique()

cumulative_hauls_df <- n_trawls_df |>
  dplyr::group_by(net_number) |>
  dplyr::mutate(n_hauls = order(haul)) |>
  dplyr::ungroup()
               
# Set up data for mixed effects model 
lme_df <- dplyr::inner_join(trawl_height_summary, net_number_df)

# --- Sample size
sample_size_df <- lme_df |>
  dplyr::group_by(ctd, net_number) |>
  dplyr::summarise(n_net = n()) |>
  tidyr::pivot_wider(names_from = ctd, values_from = n_net, values_fill = 0)

write.csv(sample_size_df, file = "akp_ctd_sample_size.csv", row.names = FALSE)

lme_df <- lme_df |>
  dplyr::inner_join(lme_df |>
                      dplyr::group_by(ctd, net_number) |>
                      dplyr::summarise(n_net = n()) |>
                      tidyr::pivot_wider(names_from = ctd, values_from = n_net, values_fill = 0) |>
                      dplyr::mutate(min_treatment_n = pmin(no, yes))) |>
  dplyr::filter(min_treatment_n > 2) |>
  dplyr::inner_join(cumulative_hauls_df)


# Fit net height mixed effects model
net_height_model <- rstanarm::stan_lmer(formula = mean_net_height ~ ctd + (1 | net_number), data = lme_df)

# View coefficients and credible intervals
net_height_model$coefficients
net_height_model$stan_summary


# Fit net height mixed effects model with random slope for cumulative hauls
net_height_cumulative_hauls_model <- rstanarm::stan_lmer(formula = mean_net_height ~ ctd + (1 + n_hauls | net_number), data = lme_df)

# View coefficients and credible intervals
net_height_cumulative_hauls_model$coefficients
net_height_cumulative_hauls_model$stan_summary

plot(net_height_cumulative_hauls_model)


# Plot net height params
png(file = here::here("plots", "net_height_model_params.png"), width = 6, height = 6, units = "in", res = 120)
print(
plot(net_height_model)
)
dev.off()


# Posterior predictive distribution for net x ctd combinations
pred_df <- lme_df |>
  dplyr::select(net_number, ctd) |>
  unique()
pred_df$index <- 1:nrow(pred_df)


pred_df <- dplyr::inner_join(pred_df,
                  pivot_longer(as.data.frame(rstanarm::posterior_predict(net_height_model, newdata = pred_df)), cols = 1:14) |>
                    dplyr::rename(index = name) |>
                    dplyr::mutate(index = as.numeric(index)))


credible_interval_df <- pred_df |>
  dplyr::group_by(net_number, ctd) |>
  dplyr::summarise(median_fit = median(value),
                   min_fit = min(value),
                   max_fit = max(value))


png(file = here::here("plots", "ci_net_ctd.png"), width = 6, height = 6, units = "in", res = 120)
print(
ggplot(data = credible_interval_df,
       aes(x = interaction(ctd, net_number),
           y = median_fit,
           ymax = max_fit,
           ymin = min_fit,
           color = ctd)) +
  geom_point(size = rel(3)) +
  geom_errorbar(size = rel(2)) +
  scale_x_discrete() +
  scale_color_colorblind() +
  scale_y_continuous(name = "Net height (m)") +
  theme_bw()
)
dev.off()


# Fit ping rate model
ping_rate_model <- rstanarm::stan_lmer(formula = I(n/duration_sec) ~ ctd + (1 | net_number), data = lme_df)

# View coefficients and credible intervals
ping_rate_model$coefficients
ping_rate_model$stan_summary

# Plot net height parameters
png(file = here::here("plots", "ping_rate_model_params.png"), width = 6, height = 6, units = "in", res = 120)
print(
  plot(ping_rate_model)
)
dev.off()


# Bonus plots
ctd_vs_no_ctd_df <- trawl_height_summary |>
  dplyr::group_by(ctd) |>
  dplyr::summarise(pings_per_second = mean(n/duration_sec),
                   grand_mean_net_height = mean(mean_net_height),
                   grand_sd_net_height = sd(mean_net_height),
                   grand_median_net_height = mean(median_net_height))

ggplot() +
  geom_density(data = trawl_height_summary,
               aes(x = mean_net_height,
                   color = ctd)) +
  scale_x_continuous(name = "Mean net height") +
  scale_y_continuous(name = "Density")

ggplot() +
  geom_density(data = trawl_height_summary,
               aes(x = n/duration_sec,
                   color = ctd)) +
  scale_x_continuous(name = "Pings per second") +
  scale_y_continuous(name = "Density")

png(file = here::here("plots", "cumulative_hauls_by_net.png"), width = 6, height = 6, units = "in", res = 120)
    print(
      ggplot() +
        geom_point(data = lme_df,
                   aes(x = n_hauls,
                       y = factor(net_number),
                       color = ctd),
                   size = rel(3)) +
        scale_x_continuous(name = "Cumulative hauls (#)") +
        scale_y_discrete(name = "Net Number") +
        scale_color_colorblind() +
        theme_bw()
    )
    dev.off()

    
    range(goa_haul_df$sound_speed[goa_haul_df$vessel == 176])

    
    1467
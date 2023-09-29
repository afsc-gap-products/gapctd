# Process and plot deployment examples 
# Sean Rohan, November 2022
#
# Processes example deployments using the gapctd package with the following methods:
#    - Typical: Manfacturer recommendend temperature offset (-0.5) and CTM parameters (alpha_C = 0.04, beta_C = 1/8)
#    - Typ. CTM: Estimate temperature offset and manufacturer recommended CTM parameters (alpha_C = 0.04, beta_C = 1/8)
#    - Temperature-Salinity Curve Area (TSA): Estimated temperature offset and CTM parameters. CTM optimization based on area between temperature-salinity curves.
#    - Minimum Salinity Gradient (MSG): Estimated temperature offset and CTM parameters. CTM optimization based on path distance of salinity curves.
#


# Prepare data -------------------------------------------------------------------------------------
library(gapctd)

# Get haul data
haul_files <- list.files(here::here("paper", "data", "haul_data"), full.names = TRUE)
ex_deployments <- read.csv(file = here::here("paper", "data", "example_deployments.csv"))

deployment_dat <- data.frame()

for(ii in 1:length(haul_files)) {
  
  deployment_dat <- deployment_dat |>
    dplyr::bind_rows(
      readRDS(file = haul_files[ii]) |>
        janitor::clean_names() |>
        dplyr::inner_join(ex_deployments)
    )
}

names(deployment_dat) <- toupper(names(deployment_dat))

saveRDS(deployment_dat,
        here::here("paper", "data", "ex_hauls.rds"))

# Functions for plotting examples ------------------------------------------------------------------
derive_and_bin <- function(x, bin_width = NULL) {
  x$downcast <- gapctd::derive_eos(x$downcast)
  
  x$upcast <- gapctd::derive_eos(x$upcast) 
  
  
  if(!is.null(bin_width)) {
    x$downcast <- gapctd:::bin_average(x$downcast,
                                       by = "depth", 
                                       bin_width = bin_width)
    x$upcast <- gapctd:::bin_average(x$upcast,
                                     by = "depth", 
                                     bin_width = bin_width)
    
  }
  
  return(x)
  
}

make_stage_df <- function(x, stage, method) {
  
  out <- data.frame()
  
  if("downcast" %in% names(x)) {
    
    out <- dplyr::bind_rows(out, 
                            as.data.frame(x$downcast@data)|>
                              dplyr::mutate(cast_direction = x$downcast@metadata$cast_direction,
                                            processing_method = method) |>
                              dplyr::bind_cols(x$downcast@metadata$race_metadata |>
                                                 dplyr::select(VESSEL, CRUISE, HAUL, REGION))
    )
    
  }
  
  
  if("upcast" %in% names(x)) {
    
    out <- dplyr::bind_rows(out,
                            as.data.frame(x$upcast@data) |>
                              dplyr::mutate(cast_direction = x$upcast@metadata$cast_direction,
                                            processing_method = method) |>
                              dplyr::bind_cols(x$upcast@metadata$race_metadata |>
                                                 dplyr::select(VESSEL, CRUISE, HAUL, REGION)))
    
  }
  
  out$stage <- stage
  
  return(out)
}


# Get names of example deployment files
ex_files <- list.files(here::here("paper", "data", "cnv"), 
                       pattern = ".cnv", 
                       full.names = TRUE)

haul_df <- readRDS(here::here("paper", "data", "ex_hauls.rds"))

# Make table of representative stations
ts_gradient <- readRDS(here::here("paper", "output", "TS_gradient_by_cast.rds")) |>
  dplyr::group_by(vessel, cruise, haul) |>
  dplyr::summarise(temperature_range = round(mean(temperature_range), 2),
                   salinity_range = round(mean(salinity_range), 2),
                   temperature_max_gradient = round(mean(temperature_max_gradient_z), 3),
                   salinity_max_gradient = round(mean(salinity_max_gradient_s), 3))

names(ts_gradient) <- toupper(names(ts_gradient))

representative_station_df <- haul_df |>
  dplyr::mutate(LATITUDE = (START_LATITUDE+END_LATITUDE)/2,
                LONGITUDE = (START_LONGITUDE+END_LONGITUDE)/2) |>
  dplyr::inner_join(ts_gradient,
                    by = c("VESSEL", "CRUISE", "HAUL")) |>
  dplyr::inner_join(
    data.frame(VESSEL = c(148,
                          176,
                          148,
                          176,
                          162,
                          94,
                          162,
                          94,
                          162,
                          94,
                          162,
                          94),
               CRUISE = c(202201,
                          202201,
                          202101,
                          202101,
                          202102,
                          202102,
                          202202, 
                          202202, 
                          202201, 
                          202201, 
                          202101, 
                          202101),
               FULL_REGION = c("Aleutian Islands", 
                               "Aleutian Islands", 
                               "Gulf of Alaska", 
                               "Gulf of Alaska",
                               "Northern Bering Sea", 
                               "Northern Bering Sea", 
                               "Northern Bering Sea", 
                               "Northern Bering Sea",
                               "Eastern Bering Sea",
                               "Eastern Bering Sea",
                               "Eastern Bering Sea",
                               "Eastern Bering Sea"
               )),
    by = c("VESSEL", "CRUISE")
  ) |>
  dplyr::mutate(DATE = paste0(lubridate::month(START_TIME, label = TRUE, abbr = TRUE), " ",
                              lubridate::day(START_TIME), ", ",
                              lubridate::year(START_TIME)),
                LATITUDE = format(round(LATITUDE, 2), nsmall = 2),
                LONGITUDE = format(round(LONGITUDE, 2), nsmall = 2)) |>
  dplyr::select(EXAMPLE, REGION, DATE, LATITUDE, LONGITUDE, 
                TEMPERATURE_RANGE, SALINITY_RANGE, TEMPERATURE_MAX_GRADIENT, SALINITY_MAX_GRADIENT,
                DESCRIPTION) |>
  dplyr::arrange(EXAMPLE)

write.csv(representative_station_df,
          here::here("paper", "plots", "representative_station_table.csv"),
          row.names = FALSE)


# Run data processing methods on each example deployment -------------------------------------------

process_df <- data.frame()
param_df <- data.frame()

for(ii in 1:length(ex_files)) {
  
  sel_ctd <- read.oce(file = ex_files[ii])
  
  # Steps that are the same across methods
  stage_1 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "split") |>
    derive_and_bin(bin_width = 1)
  
  stage_2 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "median_filter") |>
    derive_and_bin(bin_width = 1)
  
  stage_3 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "lowpass_filter") |>
    derive_and_bin(bin_width = 1)
  
  
  # Typical method
  stage_4_typical <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "align",
                        align_pars = list(temperature = -0.5),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                        cor_var = "conductivity") |>
    derive_and_bin(bin_width = 1)
  
  stage_5_typical <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "ctmcorrect",
                        align_pars = list(temperature = -0.5),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                        cor_var = "conductivity") |>
    derive_and_bin(bin_width = 1)
  
  stage_6_typical <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "slowdown",
                        align_pars = list(temperature = -0.5),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                        cor_var = "conductivity") |>
    derive_and_bin(bin_width = 1)
  
  stage_7_typical <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "bin_average",
                        align_pars = list(temperature = -0.5),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                        cor_var = "conductivity")
  
  stage_8_typical <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "full", # w/ Density inversion check and completeness check
                        align_pars = list(temperature = -0.5),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                        cor_var = "conductivity")
  
  
  # Estimated temperature alignment, typical CTM parameters
  round1_4 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "align",
                        align_pars = list(),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8)) |>
    derive_and_bin(bin_width = 1)
  
  round1_5 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        align_pars = list(),
                        ctm_pars = list(alpha_C = 0.04, beta_C = 1/8),
                        return_stage = "ctmcorrect") |>
    derive_and_bin(bin_width = 1)
  
  round1_6 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "slowdown",
                        ctm_pars = round1_5$downcast@metadata$ctm$both) |>
    derive_and_bin(bin_width = 1)
  
  round1_7 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "bin_average",
                        ctm_pars = round1_5$downcast@metadata$ctm$both)
  
  round1_8 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        return_stage = "full", # w/ Density inversion check and completeness check
                        ctm_pars = round1_5$downcast@metadata$ctm$both)
  
  
  # Estimated alignment and CTM parameters, with optimization based on T-S Area (TSA)
  round2_4 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        align_pars = list(),
                        ctm_pars = list(),
                        return_stage = "align") |>
    derive_and_bin(bin_width = 1)
  
  round2_5 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        align_pars = list(),
                        ctm_pars = list(),
                        return_stage = "ctmcorrect") |>
    derive_and_bin(bin_width = 1)
  
  round2_6 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        align_pars = list(),
                        ctm_pars = round2_5$downcast@metadata$ctm$both,
                        return_stage = "slowdown") |>
    derive_and_bin(bin_width = 1)
  
  round2_7 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        ctd_tz = "America/Anchorage",
                        align_pars = list(),
                        ctm_pars = round2_5$downcast@metadata$ctm$both,
                        return_stage = "bin_average")
  
  round2_8 <- run_gapctd(x = sel_ctd, 
                        haul_df = haul_df, 
                        align_pars = list(),
                        ctm_pars = round2_5$downcast@metadata$ctm$both,
                        ctd_tz = "America/Anchorage",
                        return_stage = "full")
  
  # Estimated alignment and CTM parameters, with optimization based on Minimum Salinity Gradient (MSG)
  sel_downcast <- oce::ctdTrim(x = sel_ctd,
                               method = "range", 
                               parameters = list(item = "timeS",
                                                 from = 0,
                                                 to = max(stage_1$downcast@data$timeS + 0.25, na.rm = TRUE)))
  
  round3_4 <- run_gapctd(x = sel_downcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "align") |>
    derive_and_bin(bin_width = 1)
  
  round3_5 <- run_gapctd(x = sel_downcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "ctmcorrect") |>
    derive_and_bin(bin_width = 1)
  
  round3_6 <- run_gapctd(x = sel_downcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "slowdown",
                         ctm_pars = round3_5$downcast@metadata$ctm$both) |>
    derive_and_bin(bin_width = 1)
  
  round3_7 <- run_gapctd(x = sel_downcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "bin_average",
                         ctm_pars = round3_5$downcast@metadata$ctm$both)
  
  round3_8 <- run_gapctd(x = sel_downcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "full", # w/ Density inversion check and completeness check
                         ctm_pars = round3_5$downcast@metadata$ctm$both)
  
  
  sel_upcast <- oce::ctdTrim(x = sel_ctd,
                             method = "range", 
                             parameters = list(item = "timeS",
                                               from = min(stage_1$upcast@data$timeS - 0.25, na.rm = TRUE),
                                               to = 5e6))
  
  round3_4_uc <- run_gapctd(x = sel_upcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "align") |>
    derive_and_bin(bin_width = 1)
  
  round3_5_uc <- run_gapctd(x = sel_upcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "ctmcorrect") |>
    derive_and_bin(bin_width = 1)
  
  round3_6_uc <- run_gapctd(x = sel_upcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "slowdown",
                         ctm_pars = round3_5_uc$upcast@metadata$ctm$both) |>
    derive_and_bin(bin_width = 1)
  
  round3_7_uc <- run_gapctd(x = sel_upcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "bin_average",
                         ctm_pars = round3_5_uc$upcast@metadata$ctm$both)
  
  round3_8_uc <- run_gapctd(x = sel_upcast, 
                         haul_df = haul_df, 
                         ctd_tz = "America/Anchorage",
                         return_stage = "full", # w/ Density inversion check and completeness check
                         ctm_pars = round3_5_uc$upcast@metadata$ctm$both)
  
  # Make data frame with all outputs
  all_steps <- dplyr::bind_rows(make_stage_df(stage_1, stage = "split", method = "All"),
                                make_stage_df(stage_2, stage = "median_filter", method = "All"),
                                make_stage_df(stage_3, stage = "lowpass_filter", method = "All"),
                                make_stage_df(round1_4, stage = "align", method = "Typ. CTM"),
                                make_stage_df(round1_5, stage = "ctmcorrect", method = "Typ. CTM"),
                                make_stage_df(round1_6, stage = "slowdown", method = "Typ. CTM"),
                                make_stage_df(round1_7, stage = "bin_average", method = "Typ. CTM"),
                                make_stage_df(round1_8, stage = "slowdown", method = "Typ. CTM"),
                                make_stage_df(round2_4, stage = "align", method = "TSA"),
                                make_stage_df(round2_5, stage = "ctmcorrect", method = "TSA"),
                                make_stage_df(round2_6, stage = "slowdown", method = "TSA"),
                                make_stage_df(round2_7, stage = "bin_average", method = "TSA"),
                                make_stage_df(round2_8, stage = "slowdown", method = "TSA"),
                                make_stage_df(round3_4, stage = "align", method = "MSG"),
                                make_stage_df(round3_5, stage = "ctmcorrect", method = "MSG"),
                                make_stage_df(round3_6, stage = "slowdown", method = "MSG"),
                                make_stage_df(round3_7, stage = "bin_average", method = "MSG"),
                                make_stage_df(round3_8, stage = "slowdown", method = "MSG"),
                                make_stage_df(round3_4_uc, stage = "align", method = "MSG"),
                                make_stage_df(round3_5_uc, stage = "ctmcorrect", method = "MSG"),
                                make_stage_df(round3_6_uc, stage = "slowdown", method = "MSG"),
                                make_stage_df(round3_7_uc, stage = "bin_average", method = "MSG"),
                                make_stage_df(round3_8_uc, stage = "slowdown", method = "MSG"),
                                make_stage_df(stage_4_typical, stage = "align", method = "Typical"),
                                make_stage_df(stage_5_typical, stage = "ctmcorrect", method = "Typical"),
                                make_stage_df(stage_6_typical, stage = "slowdown", method = "Typical"),
                                make_stage_df(stage_7_typical, stage = "bin_average", method = "Typical"),
                                make_stage_df(stage_8_typical, stage = "slowdown", method = "Typical")
  )
  
  process_df <- dplyr::bind_rows(process_df, all_steps)
  
  
  # Data frame with parameters used for each method
  param_df <- dplyr::bind_rows(param_df,
                               dplyr::bind_rows(
                                 data.frame(
                                   T_offset_dc = round1_4$downcast@metadata$align[["temperature"]]['offset'],
                                   TC_corr_dc = round1_4$downcast@metadata$align[["temperature"]]['corr'],
                                   T_offset_uc = round1_4$upcast@metadata$align[["temperature"]]['offset'],
                                   TC_corr_uc = round1_4$upcast@metadata$align[["temperature"]]['corr'],
                                   alpha_C = round1_5$downcast@metadata$ctm['alpha_C'],
                                   beta_C = round1_5$downcast@metadata$ctm['beta_C'],
                                   round = 1)|>
                                   dplyr::bind_cols(round1_8$downcast@metadata$race_metadata),
                                 data.frame(
                                   T_offset_dc = round2_4$downcast@metadata$align[["temperature"]]['offset'],
                                   TC_corr_dc = round2_4$downcast@metadata$align[["temperature"]]['corr'],
                                   T_offset_uc = round2_4$upcast@metadata$align[["temperature"]]['offset'],
                                   TC_corr_uc = round2_4$upcast@metadata$align[["temperature"]]['corr'],
                                   alpha_C = round2_5$downcast@metadata$ctm$both['alpha_C'],
                                   beta_C = round2_5$downcast@metadata$ctm$both['beta_C'],
                                   round = 2)|>
                                   dplyr::bind_cols(round1_8$downcast@metadata$race_metadata),
                                 data.frame(
                                   T_offset_dc = round3_4$downcast@metadata$align[["temperature"]]['offset'],
                                   TC_corr_dc = round3_4$downcast@metadata$align[["temperature"]]['corr'],
                                   T_offset_uc = NA,
                                   TC_corr_uc = NA,
                                   alpha_C = round3_5$downcast@metadata$ctm$both['alpha_C'],
                                   beta_C = round3_5$downcast@metadata$ctm$both['beta_C'],
                                   round = 3)|> 
                                   dplyr::bind_cols(round1_8$downcast@metadata$race_metadata)
                               ) 
                               
  )
  
}

# Save outputs
saveRDS(process_df, file = here::here("paper", "data", "ex_process_df.rds"))
saveRDS(param_df, file = here::here("paper", "data", "param_df.rds"))


# Plot results -------------------------------------------------------------------------------------
library(gapctd)
process_df <- readRDS(here::here("paper", "data", "ex_process_df.rds"))
param_df <- readRDS(here::here("paper", "data", "param_df.rds"))
# profile_df <- readRDS(here::here("paper", "data", "ex_profiles.rds"))

representative_station_df <- read.csv(here::here("paper", "plots", "representative_station_table.csv"))

unique_deployments <- process_df |>
  dplyr::select(VESSEL, CRUISE, HAUL) |>
  unique()

process_df <- dplyr::filter(process_df, depth > 1)

offset_curves <- data.frame(processing_method = c("All", NA, "Typ. CTM", "TSA", "MSG", "Typical"),
                            shift_var = c(0, 0, 0.125, 0.25, 0.375, 0))

for(jj in 1:nrow(unique_deployments)) {
  
  sel_dat <- process_df |>
    dplyr::filter(VESSEL == unique_deployments$VESSEL[jj],
                  CRUISE == unique_deployments$CRUISE[jj],
                  HAUL == unique_deployments$HAUL[jj],
                  !(stage %in% c("bin_average"))) |>
    dplyr::arrange(depth)
  
  sel_dat$sigmat <- oce::swSigmaT(salinity = sel_dat$salinity,
                                  temperature = sel_dat$temperature,
                                  pressure = sel_dat$pressure,
                                  longitude = representative_station_df$LONGITUDE[jj],
                                  latitude = representative_station_df$LATITUDE[jj])
  
  sel_dat$sigmatheta <- oce::swSigmaTheta(salinity = sel_dat$salinity,
                                          temperature = sel_dat$temperature,
                                          pressure = sel_dat$pressure,
                                          longitude = representative_station_df$LONGITUDE[jj],
                                          latitude = representative_station_df$LATITUDE[jj])
  
  sel_dat$range_temperature <- diff(range(sel_dat$temperature, na.rm = TRUE))
  sel_dat$range_salinity <- diff(range(sel_dat$salinity, na.rm = TRUE))
  sel_dat$range_sigmat <- diff(range(sel_dat$sigmat, na.rm = TRUE))
  sel_dat$range_sigmatheta <- diff(range(sel_dat$sigmat, na.rm = TRUE))
  sel_dat$range_density <- diff(range(sel_dat$density-1000, na.rm = TRUE))
  
  sel_dat <- dplyr::inner_join(sel_dat, 
                               offset_curves,
                               by = "processing_method")
  
  names(sel_dat) <- tolower(names(sel_dat))
  
  plot_T <- ggplot() +
    geom_path(data = sel_dat,
              mapping = aes(y = depth,
                            x = temperature + range_temperature*shift_var,
                            color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                            linetype = cast_direction),
              size = rel(0.3)) +
    scale_x_continuous(name = expression("Temperature ("*degree*C*')'), 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_y_reverse(name = "Depth (m)") +
    scale_color_manual(guide = "none", values = ggthemes::colorblind_pal()(6)[c(1:4,6)]) +
    facet_wrap(~factor(stage,
                       levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                       labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 8.5),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          legend.text = element_text(size = 8),
          legend.margin=margin())
  
  plot_S <- ggplot() +
    geom_path(data = sel_dat,
              mapping = aes(y = depth,
                            x = salinity + range_salinity*shift_var,
                            color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                            linetype = cast_direction),
              size = rel(0.3)) +
    scale_x_continuous(name = "Salnity (PSS-78)", 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_linetype(guide = "none") +
    scale_y_reverse(name = "Depth (m)") +
    scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(1:4,6)]) +
    facet_wrap(~factor(stage,
                       levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                       labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
    theme_bw() +
    guides(color=guide_legend(nrow=2)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 8.5),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          legend.text = element_text(size = 8),
          legend.margin=margin(),
          legend.box = "vertical")
  
  plot_density <- ggplot() +
    geom_path(data = sel_dat |>
                dplyr::filter(stage == "slowdown"),
              mapping = aes(y = depth,
                            x = density-1000 + range_density*shift_var,
                            color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                            linetype = cast_direction),
              size = rel(0.3)) +
    scale_x_continuous(name = expression(sigma~'('*kg~m^-3*')'), 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_y_reverse(name = "Depth (m)") +
    scale_linetype(name = "Cast") +
    scale_color_manual(name = "Method", values = ggthemes::colorblind_pal()(6)[c(1:4,6)], drop = FALSE) +
    facet_wrap(~factor(stage,
                       levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                       labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 8.5),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          legend.text = element_text(size = 8),
          legend.margin=margin())
  
  plot_direction_legend <- cowplot::get_legend(plot_T)
  plot_method_legend <- cowplot::get_legend(plot_S)
  
  plot_sigmat <- ggplot() +
    geom_path(data = sel_dat |>
                dplyr::filter(stage == "slowdown"),
              mapping = aes(y = depth,
                            x = sigmat + range_sigmat*shift_var,
                            color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                            linetype = cast_direction),
              size = rel(0.3)) +
    scale_x_continuous(name = expression(sigma[t]~'('*kg~m^-3*')'), 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_y_reverse(name = "Depth (m)") +
    scale_linetype(name = "Cast") +
    scale_color_manual(name = "Method", values = ggthemes::colorblind_pal()(6)[c(1:4,6)], drop = FALSE) +
    facet_wrap(~factor(stage,
                       levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                       labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 8.5),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          legend.text = element_text(size = 8),
          legend.margin=margin())
  
  plot_direction_legend <- cowplot::get_legend(plot_T)
  plot_method_legend <- cowplot::get_legend(plot_S)
  
  plot_sigmatheta <- ggplot() +
    geom_path(data = sel_dat |>
                dplyr::filter(stage == "slowdown"),
              mapping = aes(y = depth,
                            x = sigmatheta + range_sigmatheta*shift_var,
                            color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                            linetype = cast_direction),
              size = rel(0.3)) +
    scale_x_continuous(name = expression(sigma[theta]~'('*kg~m^-3*')'), 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_y_reverse(name = "Depth (m)") +
    scale_linetype(name = "Cast") +
    scale_color_manual(name = "Method", values = ggthemes::colorblind_pal()(6)[c(1:4,6)], drop = FALSE) +
    facet_wrap(~factor(stage,
                       levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                       labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
    theme_bw() +
    theme(legend.position = "none",
          legend.title = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = 8.5),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          legend.text = element_text(size = 8),
          legend.margin=margin())
  
  plot_direction_legend <- cowplot::get_legend(plot_T)
  plot_method_legend <- cowplot::get_legend(plot_S)
  
  # Sigma-theta
  ragg::agg_png(file = here::here("paper", "plots", 
                                  paste0("process_sigmatheta_", 
                                         unique_deployments$VESSEL[jj], "_", 
                                         unique_deployments$CRUISE[jj], "_", 
                                         unique_deployments$HAUL[jj], ".png")), 
                width = 220, 
                height = 110, 
                res = 600, 
                units = "mm")
  
  print(
    cowplot::plot_grid(
      cowplot::plot_grid(
        plot_T + theme(legend.position = "none"),
        plot_S + theme(legend.position = "none"),
        align = "hv", labels = c("A", "B")),
      plot_sigmatheta + theme(legend.position = "right",
                          legend.title = element_text(size = 8, hjust = 0)),
      rel_widths = c(2,0.8),
      labels = c(NA, "C")
    )
  )
  dev.off()
  
  
  # Sigma-t
  ragg::agg_png(file = here::here("paper", "plots", 
                                  paste0("process_sigmat", 
                                         unique_deployments$VESSEL[jj], "_", 
                                         unique_deployments$CRUISE[jj], "_", 
                                         unique_deployments$HAUL[jj], ".png")), 
                width = 220, 
                height = 110, 
                res = 600, 
                units = "mm")
  
  print(
      cowplot::plot_grid(
        cowplot::plot_grid(
          plot_T + theme(legend.position = "none"),
          plot_S + theme(legend.position = "none"),
          align = "hv", labels = c("A", "B")),
        plot_sigmat + theme(legend.position = "right",
                            legend.title = element_text(size = 8, hjust = 0)),
        rel_widths = c(2,0.8),
        labels = c(NA, "C")
      )
  )
  dev.off()
  
  # Sigma
  ragg::agg_png(file = here::here("paper", "plots", 
                                  paste0("process_sigma_", 
                                         unique_deployments$VESSEL[jj], "_", 
                                         unique_deployments$CRUISE[jj], "_", 
                                         unique_deployments$HAUL[jj], ".png")), 
                width = 220, 
                height = 110, 
                res = 600, 
                units = "mm")
  print(
    cowplot::plot_grid(
      cowplot::plot_grid(
        plot_T + theme(legend.position = "none"),
        plot_S + theme(legend.position = "none"),
        align = "hv", labels = c("A", "B")),
      plot_density + theme(legend.position = "right",
                          legend.title = element_text(size = 8, hjust = 0)),
      rel_widths = c(2,0.8),
      labels = c(NA, "C")
    )
  )
  dev.off()
}

# Inspect plots ------------------------------------------------------------------------------------

library(plotly)


sel_dat <- process_df |>
  dplyr::filter(VESSEL == 148,
                CRUISE == 202201,
                HAUL == 186,
                !(stage %in% c("slowdown", "bin_average")))

sel_dat$range_temperature <- diff(range(sel_dat$temperature, na.rm = TRUE))
sel_dat$range_salinity <- diff(range(sel_dat$salinity, na.rm = TRUE))

sel_dat <- dplyr::inner_join(sel_dat, 
                             offset_curves,
                             by = "processing_method")

names(sel_dat) <- tolower(names(sel_dat))

plotly::ggplotly(
    ggplot() +
      geom_path(data = sel_dat,
                mapping = aes(y = depth,
                              x = temperature + range_temperature*shift_var,
                              color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                              linetype = cast_direction),
                size = rel(0.3)) +
      scale_x_continuous(name = expression("Temperature,"~degree*C), 
                         guide = guide_axis(check.overlap = TRUE)) +
      scale_y_reverse(name = "Depth (m)") +
      scale_color_manual(guide = "none", values = ggthemes::colorblind_pal()(6)[c(1:4,6)]) +
      facet_wrap(~factor(stage,
                         levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                         labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.background = element_blank(),
            axis.text = element_text(size = 8, color = "black"),
            axis.title = element_text(size = 9, color = "black"),
            legend.text = element_text(size = 8),
            legend.margin=margin())
)

plotly::ggplotly(
    ggplot() +
      geom_path(data = sel_dat,
                mapping = aes(y = depth,
                              x = salinity + range_salinity*shift_var,
                              color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                              linetype = cast_direction),
                size = rel(0.3)) +
      scale_x_continuous(name = "Salnity, PSS-78", 
                         guide = guide_axis(check.overlap = TRUE)) +
      scale_linetype(guide = "none") +
      scale_y_reverse(name = "Depth (m)") +
      scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(1:4,6)]) +
      facet_wrap(~factor(stage,
                         levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                         labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
      theme_bw() +
      guides(color=guide_legend(nrow=2)) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.background = element_blank(),
            axis.text = element_text(size = 8, color = "black"),
            axis.title = element_text(size = 9, color = "black"),
            legend.text = element_text(size = 8),
            legend.margin=margin(),
            legend.box = "vertical")
)



plotly::ggplotly(
  ggplot() +
    geom_path(data = sel_dat,
              mapping = aes(y = depth,
                            x = density,
                            color = factor(processing_method, levels = c("All", "Typical", "Typ. CTM", "TSA", "MSG")),
                            linetype = cast_direction),
              size = rel(0.3)) +
    scale_x_continuous(name = "Density", 
                       guide = guide_axis(check.overlap = TRUE)) +
    scale_linetype(guide = "none") +
    scale_y_reverse(name = "Depth (m)") +
    scale_color_manual(values = ggthemes::colorblind_pal()(6)[c(1:4,6)]) +
    facet_wrap(~factor(stage,
                       levels = c("split", "median_filter", "lowpass_filter", "align", "ctmcorrect", "slowdown"),
                       labels = c("Split", "Median Filter", "Low Pass Filter", "Align T", "CTM Corr.", "Slowdown"))) +
    theme_bw() +
    guides(color=guide_legend(nrow=2)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          strip.background = element_blank(),
          axis.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          legend.text = element_text(size = 8),
          legend.margin=margin(),
          legend.box = "vertical")
)

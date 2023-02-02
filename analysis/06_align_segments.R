# Can alignment be optimized based on profiling rate? If so, we would expect a close association between optimal alignment and profiling speed.

library(gapctd)
library(mgcv)

optim_align_segment <- function(x, cast_direction, window, offsets = seq(-1,1,0.01)) {
  
  if(is.null(x)) {
    return(x)
  }
  
  scan_split <- oce::ctdTrim(x, method = "range", 
                             parameters = list(item = "pressure", 
                                               from = 1, 
                                               to = 1e6))@data
  
  if(cast_direction == "upcast") {
    scan_index <- c(1, max(scan_split$scan))
  }
  
  if(cast_direction == "downcast") {
    scan_index <- c(min(scan_split$scan), 1e6)
  }
  
  profile_dat <- oce::ctdTrim(x, 
                              method = "range", 
                              parameters = list(item = "scan", 
                                                from = scan_index[1], 
                                                to = scan_index[2]))
  
  profile_dat@data$scan_interval <- cut(profile_dat@data$scan, 
                                        breaks = seq(min(profile_dat@data$scan)-1, max(profile_dat@data$scan)+window, 
                                                     window))
  
  profile_dat@data$scan_interval <- as.numeric(factor(profile_dat@data$scan_interval))
  
  segments <- unique(profile_dat@data$scan_interval)
  
  segment_par_df <- data.frame()
  
  for(jj in segments) {
    sel_segment <- oce::ctdTrim(profile_dat,
                                method = "range",
                                parameters = list(item = "scan_interval",
                                                  from = jj,
                                                  to = jj))
    
    if(length(sel_segment@data$timeS) > 12) {
      optim_pars <- gapctd::optim_align_par(x = sel_segment, 
                                            cast_direction = cast_direction,
                                            variable = "temperature",
                                            offsets = offsets)
      
      segment_par_df <- dplyr::bind_rows(segment_par_df,
                                         data.frame(segment = ii,
                                                    velocity = mean(diff(sel_segment@data$pressure)/diff(sel_segment@data$timeS)),
                                                    n_scans = length(sel_segment@data$timeS),
                                                    T_offset = optim_pars$temperature['offset'],
                                                    T_corr = optim_pars$temperature['corr'])
      )
    }
  }
  
  return(segment_par_df)
  
}

# # Haul files
# haul_files <- list.files(here::here("paper", "data", "haul_data"), full.names = TRUE)
# ex_deployments <- read.csv(file = here::here("paper", "data", "example_deployments.csv"))
# 
# deployment_dat <- data.frame()
# 
# for(ii in 1:length(haul_files)) {
#   
#   deployment_dat <- deployment_dat |>
#     dplyr::bind_rows(
#       readRDS(file = haul_files[ii]) |>
#         janitor::clean_names() |>
#         dplyr::inner_join(ex_deployments)
#     )
# }
# 
# names(deployment_dat) <- toupper(names(deployment_dat))
# 
# saveRDS(deployment_dat,
#         here::here("paper", "data", "ex_hauls.rds"))



haul_path <- c(
"C:/Users/sean.rohan/Work/afsc/WIP/2022_AKK/output/HAUL_DATA_162_202201_202202.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_VEST_8091/output/HAUL_DATA_94_202201_202202.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_VEST_8100/output/HAUL_DATA_94_202201_202202.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST_1/output/HAUL_DATA_94_202101_202102.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST/output/HAUL_DATA_94_202101_202102.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_AP/output/HAUL_DATA_176_202101.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_AP_2/output/HAUL_DATA_176_202101.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_OEX/output/HAUL_DATA_148_202101.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_AP_8102/output/HAUL_DATA_176_202201.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_OEX_8103/output/HAUL_DATA_148_202201.rds",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_AKK/output/HAUL_DATA_162_202101_202102.rds"
)

cnv_path <-
c(
"C:/Users/sean.rohan/Work/afsc/WIP/2022_AKK/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_VEST_8091/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_VEST_8100/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST_1/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_VEST/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_AP/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_AP_2/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_OEX/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_AP_8102/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2022_OEX_8103/cnv",
"C:/Users/sean.rohan/Work/afsc/WIP/2021_AKK/cnv"
)

# Check that files and directories exist and have matching length
file.exists(haul_path)
dir.exists(cnv_path)
length(haul_path) == length(cnv_path)

param_df <- data.frame()
window_vec <- c(16, 24, 40, 60)

for(hh in 1:length(haul_path)) {
  haul_df <- dplyr::bind_rows(readRDS(haul_path[hh]))
  ex_files <- c(list.files(cnv_path[hh], full.names = TRUE))
  
  for(ii in 1:length(window_vec)) {
    
    for(jj in 1:length(ex_files)) {
      
      sel_ctd <- read.oce(file = ex_files[jj])
      
      
      stage_3 <- run_gapctd(x = sel_ctd, 
                            haul_df = haul_df, 
                            ctd_tz = "America/Anchorage",
                            return_stage = "lowpass_filter")
      
      for(kk in c("downcast", "upcast")) {
        T_align_pars <- optim_align_segment(x = stage_3[[kk]],
                                            cast_direction = kk,
                                            window = window_vec[ii],
                                            offsets = seq(-1,1,0.01))
        
        if(!is.null(T_align_pars)) {
          row.names(T_align_pars) <- 1:nrow(T_align_pars)
          T_align_pars$cast_direction <- stage_3[[kk]]@metadata$cast_direction
          T_align_pars$filename <- ex_files[jj]
          T_align_pars$window <- window_vec[ii]
          T_align_pars$max_pressure <- max(stage_3[[kk]]@data$pressure)
          T_align_pars$min_temp <- min(stage_3[[kk]]@data$temperature[stage_3[[kk]]@data$pressure > 3])
          T_align_pars$max_temp <- min(stage_3[[kk]]@data$temperature[stage_3[[kk]]@data$pressure > 3])
          
          param_df <- dplyr::bind_rows(param_df, T_align_pars)
        }
        
      }
      
    }
  }
  
}

saveRDS(param_df, here::here("paper", "output", "T_alignment_by_segment.rds"))

param_df <- readRDS(here::here("paper", "output", "T_alignment_by_segment.rds"))

gam_16_window <- gam(T_offset ~ s(velocity, bs = "cs"), 
                     data = dplyr::filter(param_df, window == 16), 
                     family = "gaussian")
gam_24_window <- gam(T_offset ~ s(velocity, bs = "cs"), 
                     data = dplyr::filter(param_df, window == 24), 
                     family = "gaussian")
gam_40_window <- gam(T_offset ~ s(velocity, bs = "cs"), 
                     data = dplyr::filter(param_df, window == 40), 
                     family = "gaussian")
gam_60_window <- gam(T_offset ~ s(velocity, bs = "cs"), 
                     data = dplyr::filter(param_df, window == 60), 
                     family = "gaussian")

deviance_df <- data.frame(window = c(16, 24, 40, 60),
                          deviance_explained = round(c((gam_16_window$null.deviance-gam_16_window$deviance)/gam_16_window$null.deviance,
                                                       (gam_24_window$null.deviance-gam_24_window$deviance)/gam_24_window$null.deviance,
                                                       (gam_40_window$null.deviance-gam_40_window$deviance)/gam_40_window$null.deviance,
                                                       (gam_60_window$null.deviance-gam_60_window$deviance)/gam_60_window$null.deviance)*100, 2))

gam_results_path <- here::here("paper", "output", "profiling_speed_gam_results.txt")
sink(file = gam_results_path)
print("<----- 16 scan window ---->")
print(summary(gam_16_window))

print("")
print("")
print("<----- 24 scan window ---->")
summary(gam_24_window)

print("")
print("")
print("<----- 40 scan window ---->")
summary(gam_40_window)

print("")
print("")
print("<----- 60 scan window ---->")
summary(gam_60_window)
sink()


png(file = here::here("paper", "plots", "T_alignment_by_segment.png"), width = 80, height = 80, units = "mm", res = 600)
print(
ggplot() +
  geom_point(data = param_df,
             mapping = aes(x = abs(velocity), 
                           y = T_offset), 
             alpha = 0.7, size = rel(0.3), shape = 21, color = "grey50") +
  geom_text(data = deviance_df, 
            mapping = aes(x = 2, 
                          y = 0.88,
                          label = paste0("DE: ", deviance_explained, "%")),
            size = rel(2.5),
            hjust = 1) +
  geom_smooth(data = param_df,
              mapping = aes(x = abs(velocity), 
                            y = T_offset)) +
  facet_wrap(~window, nrow = 2) +

  scale_x_continuous(name = expression("Profiling rate ("*dbar~s^-1*")"), limits = c(0, 2)) +
  scale_y_continuous(name = expression("Temperature offset, "*t[T]*" (s)"), limits = c(-1, 1)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))
)
dev.off()

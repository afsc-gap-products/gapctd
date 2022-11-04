# Load packages

library(gapctd) # install from github: remotes::install_github("afsc-gap-products/gapctd")
library(lme4)
library(rstanarm)
library(ggthemes)
library(RNetCDF)

trawl_height_df <- readRDS(file = here::here("paper", "data", "trawl_height_df.rds"))
net_number_df <- readRDS(file = here::here("paper", "data", "net_number_df.rds"))
trawl_height_summary <- readRDS(file = here::here("paper", "data", "trawl_height_summary.rds"))

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

write.csv(sample_size_df, file = here::here("paper", "output", "akp_ctd_sample_size.csv"), row.names = FALSE)

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
# net_height_cumulative_hauls_model <- rstanarm::stan_lmer(formula = mean_net_height ~ ctd + (1 + n_hauls | net_number), data = lme_df)
# 
# View coefficients and credible intervals
# net_height_cumulative_hauls_model$coefficients
# net_height_cumulative_hauls_model$stan_summary
# 
# plot(net_height_cumulative_hauls_model)

# Plot net height params
# png(file = here::here("plots", "net_height_model_params.png"), width = 6, height = 6, units = "in", res = 120)
# print(
# plot(net_height_model)
# )
# dev.off()


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


png(file = here::here("paper", "plots", "ctd_on_trawl_effects.png"), width = 120, height = 40, units = "mm", res = 600)
print(
ggplot(data = credible_interval_df,
       aes(x = interaction(ctd, net_number),
           y = median_fit,
           ymax = max_fit,
           ymin = min_fit,
           color = ctd)) +
  geom_point() +
  geom_errorbar() +
  scale_x_discrete(name = "Net number", labels = sort(rep(unique(credible_interval_df$net_number), 2))) +
  scale_color_colorblind(name = "CTD?") +
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
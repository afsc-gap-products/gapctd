library(gapctd)
library(ggplot2)


ctm_vals <- expand.grid(alpha = seq(-10,4,0.01),
                        inv_beta = c(4,8,16,24),
                        delta_t = c(-10^seq(-4,-1,1), 10^seq(-4,-1,1)))
ctm_vals$ctm <- as.numeric(NA)

ctm_vec <- numeric(length = length(alpha_vec))

for(uu in 1:nrow(ctm_vals)) {
  aa <- gapctd::ctm_par_a(alpha = ctm_vals$alpha[uu], inv_beta = ctm_vals$inv_beta[uu], f_n = 0.25)
  bb <- gapctd::ctm_par_b(alpha = ctm_vals$alpha[uu], a = aa)
  ctm_vals$ctm[uu] <- gapctd::ctm_correct_c_t(a = aa, b = bb, temperature = c(0, ctm_vals$delta_t[uu]))[2]
}


ggplot() +
  geom_path(data = ctm_vals, aes(x = alpha, y = ctm, color = factor(delta_t)), size = rel(1.1)) +
  scale_color_viridis_d(name = expression(Delta*T~(degree*C))) +
  geom_vline(xintercept = c(-4, 0.04), linetype = 2) +
  scale_y_log10(name = expression(C[tm]~(S~m^-1))) +
  scale_x_continuous(name = expression(alpha~(s))) +
  facet_grid(~inv_beta) +
  theme_bw()

ggplot() +
  geom_path(data = ctm_vals, aes(x = alpha, y = ctm, color = factor(delta_t)), size = rel(1.1)) +
  scale_color_viridis_d(name = expression(Delta*T~(degree*C))) +
  geom_vline(xintercept = c(-4, 0.04), linetype = 2) +
  scale_y_continuous(name = expression(C[tm]~(S~m^-1))) +
  scale_x_continuous(name = expression(alpha~(s))) +
  facet_grid(~inv_beta) +
  theme_bw()

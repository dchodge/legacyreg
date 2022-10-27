


all_ind_posteriors <- dt_posterior_long[parameter %in% ind_level_params] %>% 
  ggplot() + 
  geom_density_ridges(aes(x = value, y = factor(id), fill = parameter), alpha = 0.5) +
  facet_wrap(~parameter, scales = "free") +
  theme_minimal()

all_noise_posteriors <- dt_posterior_long[parameter %in% noise_params] %>% 
  ggplot() + 
  geom_density(aes(x = value, fill = parameter), alpha = 0.5) +
  facet_wrap(~parameter, scales = "free") +
  theme_minimal()

all_pop_posteriors
all_ind_posteriors
all_noise_posteriors

ggsave("outputs/all_ind_posteriors.pdf",
       all_ind_posteriors,
       width = 8,
       height = 20,
       bg = "white")

#--- posterior-predictive plots


# plotting the fitted trajectories and the raw data
p_posterior_pred <- ggplot() + 
  geom_point(data = dt_data, aes(x = time_until_bleed, y = info3), colour = "red") +
  # geom_line(data = dt_posterior_pred[.draw %in% 1:100],
  #           aes(x = t,
  #               y = exp_titre,
  #               group = factor(.draw)), alpha = 0.05) +
  geom_ribbon(data = dt_posterior_pred_summary[lo > 0 & me > 0 & hi > 0],
              aes(x = t,
                  ymin = lo,
                  ymax = hi), alpha = 0.5) + 
  facet_wrap(~id) + 
  theme_minimal()

ggsave("outputs/posterior_pred_new.pdf",
       p_posterior_pred,
       width = 10,
       height = 15,
       bg = "white")

#--- diagnostic plots
bayesplot::mcmc_pairs(fit_current$draws(), pars = pop_level_params)

bayesplot::mcmc_pairs(fitA$draws(), pars = noise_params)

bayesplot::mcmc_pairs(fitA$draws(), pars = ind_level_params)

bayesplot::mcmc_trace(fit_current$draws(), pars = "t_p") + 
  facet_grid(~chain, scales = "free")

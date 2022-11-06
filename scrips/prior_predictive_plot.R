source("scrips/setup.R")

p_prior_pred_plot <- plot_prior_predictive(n_samples = 10000)

ggsave("outputs/figs_tim/prior_pred.pdf",
       p_prior_pred_plot,
       width = 8,
       height = 6,
       bg = "white")

ggsave("outputs/figs_tim/prior_pred.png",
       p_prior_pred_plot,
       width = 8,
       height = 6,
       bg = "white")

dt_ind_priors <- sample_ind_priors(n = 20, k = 1000, struct_arg = "long")

dt_ind_priors[parameter %in% c("t_p", "t_p_ind")] %>% 
  ggplot() +
  geom_density_ridges(aes(x = value, y = factor(id), fill = parameter), alpha = 0.5) +
  lims(x = c(0, 50)) +
  facet_wrap(~parameter) +
  labs(x = "Time since event", y = "id") +
  theme_minimal()

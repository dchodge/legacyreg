# defining the population level parameters
pop_level_params <- c("t_p", "boost_i", "boost_s", "wane_s")

#--- plotting population-level posteriors of the boost and wane parameters
dt_pop_posterior_draws <- extract_all_pop_posteriors(structure_arg = "long")

p_all_pop_posteriors <- dt_pop_posterior_draws[parameter %in% pop_level_params] %>% 
  ggplot() + 
  geom_density_ridges(aes(x = value,
                          y = factor(fit_type),
                          fill = parameter),
                      alpha = 0.5) +
  facet_wrap(~parameter, scales = "free") +
  theme_minimal()

ggsave("outputs/figs_tim/all_pop_posteriors.pdf",
       p_all_pop_posteriors,
       height = 12,
       width = 12,
       bg = "white")


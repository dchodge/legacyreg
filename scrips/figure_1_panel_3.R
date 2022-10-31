library(ggsci)

dt_gtr <- extract_all_pop_posteriors(structure_arg = "wide")

dt_gtr[, titre_at_peak := boost_wane_fun(t_p, t_p, boost_i, boost_s, wane_s), 
       by = c(".draw", "titre_type", "event_type", "exposure_type")]

dt_gtr[, titre_100_days_after_peak := boost_wane_fun(t_p + 100,
                                                     t_p, 
                                                     boost_i,
                                                     boost_s,
                                                     wane_s), 
       by = c(".draw", "titre_type", "event_type", "exposure_type")]

dt_gtr[, gtr := titre_100_days_after_peak/titre_at_peak, 
       by = c(".draw", "titre_type", "event_type", "exposure_type")]

dt_gtr[, titre_at_peak_nat := 32*2^titre_at_peak, c(".draw", "titre_type", 
                                                    "event_type", "exposure_type")]


dt_gtr[(event_type == "Wildtype infection" |
        event_type == "BA.1 infection" |
        event_type == "BA.2 infection") & 
         exposure_type == "Naive",
       `Number of exposures` := 3]

dt_gtr[(event_type == "Wildtype infection" |
        event_type == "BA.1 infection" |
        event_type == "BA.2 infection") & 
         exposure_type == "Exposed",
       `Number of exposures` := 4]

dt_gtr[event_type == "Vaccination" & exposure_type == "Naive",
       `Number of exposures` := 2]

dt_gtr[event_type == "Vaccination" & exposure_type == "Exposed",
       `Number of exposures` := 3]

dt_gtr[, `Number of exposures` := factor(`Number of exposures`)]

dt_gtr[, event_type := fct_relevel(event_type, "Vaccination")]
dt_gtr[, titre_type := fct_relevel(titre_type, "Wildtype")]

dt_gtr[, `:=` (gtr_me = quantile(gtr, 0.5),
               titre_at_peak_nat_me = quantile(titre_at_peak_nat, 0.5)),
       by = c("titre_type", "event_type", "exposure_type")]

p_summary <- dt_gtr[!event_type == "BA.5 infection"] %>% 
  ggplot() + 
  # geom_bin2d(aes(x = gtr, y = titre_at_peak_nat, colour = event_type,
  #                shape = exposure_type), alpha = 0.05) + 
  geom_point(aes(x = gtr, 
                 y = titre_at_peak_nat,
                 colour = event_type),
             alpha = 0.01) + 
  geom_point(aes(x = gtr_me,
                 y = titre_at_peak_nat_me,
                 shape = exposure_type), size = 0.5) +
  # geom_point(aes(x = gtr_me, 
  #                y = titre_at_peak_nat_me, 
  #                shape = exposure_type), colour = "black", pch = 21) +
  # facet_wrap(~titre_type) +
  coord_cartesian(ylim = c(10, 16384)) +
  facet_grid(`Number of exposures` ~ titre_type) +
  scale_y_continuous(trans = "log2") +
  labs(x = "GTR between peak titre and titre value 100 days after peak",
       y = "Titre value at peak",
       colour = "Most recent exposure",
       shape = "Infection history") + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) + 
  lims(x = c(0.3, 1)) +
  theme_minimal() 

ggsave("summary_plot.pdf",
       p_summary, 
       width = 8,
       height = 8)
 

library(tidyverse)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)
library(scales)

# defining the population level parameters
pop_level_params <- c("t_p", "boost_i", "boost_s", "wane_s")

# defining
titre_type_options <- c("Wildtype", "BA.1", "BA.2", "BA.5")
event_type_options <- c("Vaccination", "BA.1 infection",
                        "BA.2 infection", "BA.5 infection")

exposure_type_options <- c("Naive", "Exposed")


dt_pop_post_preds_sum <- extract_pop_post_pred_plot_data()
dt_raw_data_pop_plot <- extract_raw_data_pop_post_pred()

p_out <- ggplot() + 
  geom_ribbon(data = dt_pop_post_preds_sum[!event_type == "BA.5 infection"],
              aes(x = t,
                  ymin = lo_nat,
                  ymax = hi_nat,
                  fill = titre_type),
              alpha = 0.35) +
  geom_line(data = dt_pop_post_preds_sum[!event_type == "BA.5 infection"],
            aes(x = t,
                y = me_nat,
                group = titre_type),
            alpha = 0.5, linetype = "dashed") +
  geom_point(data = dt_raw_data_pop_plot[t <= 150],
             aes(x = t,
                 y = observed_titre_nat,
                 colour = titre_type,
                 group = titre_type), alpha = 0.2) +
  coord_cartesian(ylim = c(10, 8192)) +
  labs(fill = "Titre type", colour = "Titre type", 
       x = "Days since event (infection or vaccination)",
       y = "Titre value") +
  facet_grid(event_type ~ exposure_type, scales = "free") +
  # coord_trans(y = "log2", clip = "on"``, ylim = c(0, 10)) + 
  scale_y_continuous(trans = "log2") +
  # scale_y_continuous(breaks = trans_breaks("log2", 
  #                                          function(x) 2^x)) +
  theme_minimal() + 
  theme(legend.position = "bottom")

ggsave("outputs/figs_tim/pop_post_preds.pdf",
       p_out,
       width = 8,
       height = 10)

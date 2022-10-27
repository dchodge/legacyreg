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

dt_pop_post_preds <- extract_all_pop_post_preds(time_range = seq(0, 300, 1))

dt_all_raw_data <- extract_all_raw_data()

dt_pop_post_preds_sum <- summarise_trajectories(dt_pop_post_preds, 
                                                ind_flag = FALSE)

dt_raw_data_pop_plot <- dt_all_raw_data[, c("time_until_bleed", "info3", 
                                            "titre_type", "event_type", 
                                            "exposure_type")]
setnames(dt_raw_data_pop_plot, 
         c("time_until_bleed", "info3"), 
         c("t", "observed_titre"))
  
dt_pop_post_preds_sum[,  `:=` (me_nat = 32*2^me,
                               lo_nat = 32*2^lo,
                               hi_nat = 32*2^hi)]
  
dt_raw_data_pop_plot[, observed_titre_nat := 32*2^(observed_titre)]


dt_pop_post_preds_sum[, `:=` (titre_type = fct_relevel(titre_type, 
                                                       c("Wildtype")),
                              event_type = fct_relevel(event_type, 
                                                       c("Vaccination")),
                              exposure_type = fct_relevel(exposure_type, 
                                                          c("Naive")))]

dt_raw_data_pop_plot[, `:=` (titre_type = fct_relevel(titre_type, 
                                                      c("Wildtype")),
                             event_type = fct_relevel(event_type, 
                                                      c("Vaccination")),
                             exposure_type = fct_relevel(exposure_type, 
                                                         c("Naive")))]


dt_raw_data_pop_plot[, .(max_time = max(t)),
                     by = c("titre_type", "event_type", "exposure_type")][order(max_time)]

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
  geom_point(data = dt_raw_data_pop_plot,
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


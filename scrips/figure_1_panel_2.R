library(tidyverse)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)
library(scales)

dt_pop_post_preds_sum <- extract_pop_post_pred_plot_data(n_samples = 2000)
dt_raw_data_pop_plot <- extract_raw_data_pop_post_pred()

update_labels_panel_b <- function(dt_in) {
  
  dt_out <- copy(dt_in)
  
  dt_out[titre_type == "Wildtype", titre_type := "Ancestral"]
  dt_out[titre_type == "BA.1", titre_type := "Omicron BA.1"]
  dt_out[titre_type == "BA.2", titre_type := "Omicron BA.2"]
  dt_out[titre_type == "BA.5", titre_type := "Omicron BA.5"]
  
  dt_out[event_type == "Vaccination", event_type := "Third vaccine dose"]
  
  dt_out[exposure_type == "Naive", exposure_type := "Infection naive"]
  dt_out[exposure_type == "Exposed", exposure_type := "Previously infected"]
  
  dt_out[, event_type := fct_relevel(event_type, "Third vaccine dose")]
  
  return(dt_out)
  
} 

dt_pop_post_preds_sum <- update_labels_panel_b(dt_pop_post_preds_sum)
dt_raw_data_pop_plot <- update_labels_panel_b(dt_raw_data_pop_plot)

p2 <- ggplot() + 
  geom_ribbon(data = dt_pop_post_preds_sum[!event_type == "BA.5 infection"],
              aes(x = t,
                  ymin = lo_nat,
                  ymax = hi_nat,
                  fill = titre_type, group = interaction(titre_type, event_type, exposure_type)),
              alpha = 0.35, show.legend = FALSE) +
  # geom_line(data = dt_pop_post_preds_sum[!event_type == "BA.5 infection"],
  #           aes(x = t,
  #               y = me_nat,
  #               group = interaction(titre_type, event_type, exposure_type)),
  #           alpha = 0.5, linetype = "dashed") +
  geom_point(data = dt_raw_data_pop_plot[t <= 150],
             aes(x = t,
                 y = observed_titre_nat,
                 colour = titre_type,
                 group = titre_type), alpha = 0.2)  +
  labs(fill = "Titre type", colour = "Titre type",
       x = "Days since event (infection or vaccination)",
       y = "Titre value") +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(trans = "log2",
                     breaks = c(40, 80, 160, 320, 640, 1280, 2560),
                     labels = c(expression(""<=40),
                                "80", "160", "320", "640", "1280",
                                expression("">=2560))) +
  coord_cartesian(ylim = c(40, 8192)) +
  # geom_hline(aes(yintercept = 40), linetype = "dashed", colour = "gray30") +
  # geom_hline(aes(yintercept = 2560), linetype = "dashed", colour = "gray30") + 
  facet_grid(event_type ~ exposure_type, scales = "fixed") +
  theme(legend.position = "bottom")

p2 +
  geom_hline(aes(yintercept = 40), linetype = "dashed", colour = "gray30") 
  # geom_hline(aes(yintercept = 2560), linetype = "dashed", colour = "gray30")

ggsave("outputs/figs_tim/pop_post_preds.pdf",
       p2,
       width = 8,
       height = 10)

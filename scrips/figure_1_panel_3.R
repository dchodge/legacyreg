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

dt_gtr[, `:=` (gtr_me = quantile(gtr, 0.5),
               titre_at_peak_nat_me = quantile(titre_at_peak_nat, 0.5)),
       by = c("titre_type", "event_type", "exposure_type")]


update_labels_panel_c <- function(dt_in) {
  
  dt_out <- copy(dt_in)
  
  dt_out[titre_type == "Wildtype", titre_type := "Ancestral"]
  dt_out[titre_type == "BA.1", titre_type := "Omicron BA.1"]
  dt_out[titre_type == "BA.2", titre_type := "Omicron BA.2"]
  dt_out[titre_type == "BA.5", titre_type := "Omicron BA.5"]
  
  dt_out[(event_type == "Wildtype infection" |
            event_type == "BA.1 infection" |
            event_type == "BA.2 infection") & 
           exposure_type == "Naive",
         `Number of exposures` := "3 previous \n antigen exposures"]
  
  dt_out[(event_type == "Wildtype infection" |
            event_type == "BA.1 infection" |
            event_type == "BA.2 infection") & 
           exposure_type == "Exposed",
         `Number of exposures` := "4 previous \n antigen exposures"]
  
  dt_out[event_type == "Vaccination" & exposure_type == "Naive",
         `Number of exposures` := "2 previous \n antigen exposures"]
  
  dt_out[event_type == "Vaccination" & exposure_type == "Exposed",
         `Number of exposures` := "3 previous \n antigen exposures"]
  
  dt_out[exposure_type == "Naive", exposure_type := "Infection naive"]
  dt_out[exposure_type == "Exposed", exposure_type := "Previously infected"]
  
  dt_out[event_type == "Vaccination", event_type := "Third vaccine dose"]
  
  # dt_gtr[, `Number of exposures` := factor(`Number of exposures`)]
  
  dt_out[, event_type := fct_relevel(event_type, "Third vaccine dose")]
  dt_out[, titre_type := fct_relevel(titre_type, "Wildtype")]
  dt_out[, `Number of exposures` := fct_relevel(`Number of exposures`,
                                                "2 previous \n antigen exposures")]
  
  return(dt_out)
}

dt_gtr_plot <- update_labels_panel_c(dt_gtr)

p3 <- dt_gtr_plot[!event_type == "BA.5 infection"] %>% 
  ggplot() + 
  # geom_bin2d(aes(x = gtr, y = titre_at_peak_nat, colour = event_type,
  #                shape = exposure_type), alpha = 0.05, bins = 500) +
  geom_point(aes(x = gtr,
                 y = titre_at_peak_nat,
                 colour = event_type), alpha = 0.05) +
  geom_point(aes(x = gtr_me,
                 y = titre_at_peak_nat_me,
                 shape = exposure_type)) +
  scale_shape(solid = FALSE) + 
  # geom_density_2d(aes(x = gtr, 
  #                     y = titre_at_peak_nat,
  #                     colour = event_type)) + 
  geom_hline(aes(yintercept = 40), linetype = "dashed", colour = "gray30") + 
  geom_hline(aes(yintercept = 2560), linetype = "dashed", colour = "gray30") + 
  facet_grid(`Number of exposures` ~ titre_type) +
  scale_y_continuous(trans = "log2") +
  theme_light() + 
  # breaks = c(40, 80, 160, 320, 640, 1280, 2560),
  # labels = c("40", "80", "160", "320", "640", "1280", "2560")) +
  labs(x = "GTR between peak titre and titre value 100 days after peak",
       y = "Titre value at peak",
       colour = "Most recent exposure",
       shape = "Infection history") + 
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "vertical",
        # legend.margin = margin(-10),
        legend.spacing.y = unit(-0.25, "cm")) +
  scale_y_continuous(trans = "log2", 
                     breaks = c(40, 80, 160, 320, 640, 1280, 2560),
                     labels = c(expression(""<=40),
                                "80", "160", "320", "640", "1280",
                                expression("">=2560))) +
  guides(color = guide_legend(override.aes = list(alpha = 1), order = 1),
         shape = guide_legend(order = 2)) 
  # lims(x = c(0.4, 1))
        

ggsave("summary_plot.pdf",
       p3, 
       width = 8,
       height = 6,
       bg = "white")
 

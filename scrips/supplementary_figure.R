library(tidyverse)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)
library(scales)
library(cowplot)
library(ggtext)

source("scrips/setup.R")

#--- plotting panel B
dt_pop_post_preds_sum <- extract_pop_post_pred_plot_data(time_range = seq(0, 300, 1),
                                                         n_samples = 2000)

dt_raw_data_pop_plot <- extract_raw_data_pop_post_pred()

dt_pop_post_preds_sum <- update_labels_panel_b(dt_pop_post_preds_sum)
dt_raw_data_pop_plot <- update_labels_panel_b(dt_raw_data_pop_plot)

p_supp_1 <- plot_panel_supp_preds(dt_pop_post_preds_sum, 
                                  dt_raw_data_pop_plot,
                                  event_type_arg = "Third vaccine dose",
                                  exposure_type_arg = "Infection naive", 
                                  title_arg = "Third vaccine dose and infection naive",
                                  subtitle_arg = "Third antigenic exposure \nExposure history: two vaccine doses") 

p_supp_2 <- plot_panel_supp_preds(dt_pop_post_preds_sum, 
                                  dt_raw_data_pop_plot,
                                  event_type_arg = "Third vaccine dose",
                                  exposure_type_arg = "Previously infected",
                                  title_arg = "Third vaccine dose and previously infected",
                                  subtitle_arg = "Fourth antigenic exposure \nExposure history: two vaccine doses and one infection")

p_supp_3 <- plot_panel_supp_preds(dt_pop_post_preds_sum, 
                                  dt_raw_data_pop_plot,
                                  event_type_arg = "BA.1 infection",
                                  exposure_type_arg = "Infection naive",
                                  title_arg = "Omicron BA.1 infection and infection naive",
                                  subtitle_arg = "Forth antigenic exposure \nExposure history: three vaccine doses")

p_supp_4 <- plot_panel_supp_preds(dt_pop_post_preds_sum, 
                                  dt_raw_data_pop_plot,
                                  event_type_arg = "BA.1 infection",
                                  exposure_type_arg = "Previously infected",
                                  title_arg = "Omicron BA.1 infection and previously infected",
                                  subtitle_arg = "Fifth antigenic exposure \nExposure history: three vaccine doses and one infection")

p_supp_5 <- plot_panel_supp_preds(dt_pop_post_preds_sum, 
                                  dt_raw_data_pop_plot,
                                  event_type_arg = "BA.2 infection",
                                  exposure_type_arg = "Infection naive",
                                  title_arg = "Omicron BA.2 infection and infection naive",
                                  subtitle_arg = "Forth antigenic exposure \nExposure history: three vaccine doses")

p_supp_6 <- plot_panel_supp_preds(dt_pop_post_preds_sum, 
                                  dt_raw_data_pop_plot,
                                  event_type_arg = "BA.2 infection",
                                  exposure_type_arg = "Previously infected",
                                  title_arg = "Omicron BA.2 infection and previously infected",
                                  subtitle_arg = "Fifth antigenic exposure \nExposure history: three vaccine doses and one infection")

figure_supplementary_post <- (p_supp_1 + p_supp_2)/(p_supp_3 + p_supp_4)/(p_supp_5 + p_supp_6)

#--- saving PDF version
ggsave("outputs/figs_tim/figure_supp_2.pdf",
       figure_supplementary_post,
       width = 10,
       height = 12,
       bg = "white")

#--- saving PNG version
ggsave("outputs/figs_tim/figure_supp_2.png",
       figure_supplementary_post,
       width = 10,
       height = 12,
       bg = "white")

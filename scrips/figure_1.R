library(tidyverse)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)
library(scales)
library(cowplot)
library(ggtext)

#--- plotting panel A 
p_a <- plot_panel_a()

#--- plotting panel B
dt_pop_post_preds_sum <- extract_pop_post_pred_plot_data(n_samples = 2000)
dt_raw_data_pop_plot <- extract_raw_data_pop_post_pred()

dt_pop_post_preds_sum <- update_labels_panel_b(dt_pop_post_preds_sum)
dt_raw_data_pop_plot <- update_labels_panel_b(dt_raw_data_pop_plot)

p_b <- plot_panel_b(dt_pop_post_preds_sum,
                    dt_raw_data_pop_plot)

#--- plotting panel C
dt_panel_c_data <- extract_panel_c_data()
p_c <- plot_panel_c(dt_panel_c_data)

#--- putting it all together
p_figure_1 <- plot_grid(p_a, p_b, p_c, 
                        ncol = 1,
                        nrow = 3,
                        rel_heights = c(0.4, 1, 1))

ggsave("outputs/figs_tim/figure_1.pdf",
       p_figure_1,
       width = 8,
       height = 12)

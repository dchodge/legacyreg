library(tidyverse)
library(tidybayes)
library(data.table)
library(ggh4x)
library(ggridges)
library(lemon)
library(scales)
library(cowplot)
library(ggtext)
library(ggsci)
library(patchwork)

source("scrips/setup.R")

#--- plotting panel A 
p_a <- plot_panel_a()

#--- plotting panel B
dt_pop_post_preds_sum <- extract_pop_post_pred_plot_data(time_range = seq(0, 300, 1),
                                                         n_samples = 2000)
dt_raw_data_pop_plot <- extract_raw_data_pop_post_pred()

dt_pop_post_preds_sum <- update_labels_panel_b(dt_pop_post_preds_sum)
dt_raw_data_pop_plot <- update_labels_panel_b(dt_raw_data_pop_plot)

p_b <- plot_panel_b(dt_pop_post_preds_sum,
                    dt_raw_data_pop_plot)

#--- plotting panel C
dt_panel_c_data <- extract_panel_c_data()
p_c <- plot_panel_c(dt_panel_c_data) + scale_colour_jama()

layout_matrix <- c("A
                    A
                    B
                    B
                    B
                    B
                    B
                    C
                    C
                    C
                    C
                    C")

p_figure_1 <- p_a/p_b/p_c + 
  plot_layout(design = layout_matrix)
  
#--- saving PDF version
ggsave("outputs/figs_tim/figure_1.pdf",
       p_figure_1,
       width = 8,
       height = 12,
       bg = "white")

#--- saving PNG version
ggsave("outputs/figs_tim/figure_1.png",
       p_figure_1,
       width = 8,
       height = 12,
       bg = "white")


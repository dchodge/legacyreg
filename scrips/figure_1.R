# script to put together panels for figure 1

library(cowplot)

plot_grid(p1, p2, p3, 
          ncol = 1,
          nrow = 3,
          rel_heights = c(0.4, 1, 1)) + 
  theme_minimal_grid(
    color = "grey70"
  )

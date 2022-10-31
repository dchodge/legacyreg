library(ggtext)

labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")
labs_plot_y_gtr <- c("1", "2", "4", "8", "16", "32", "64", "128")
colorsdefined <- c("Third vaccine dose" = "#AA4A44", "BA1 infection" = "#0d98ba", "BA2 infection" = "#1c39bb")

label_text_size <- 3

dt_panel_1 <- data.table(
  x = c(1, 20, 120),
  y = c(3, 6, 4))

p1 <- dt_panel_1 %>% 
  ggplot() + 
  geom_hline(yintercept = 5*2^7, linetype = "dashed", color = "gray30") + 
  geom_hline(yintercept = 5*2^1, linetype = "dashed", color = "gray30") + 
  geom_line(aes(x, 5*2^y), size = 2) + 
  scale_y_continuous(trans = "log2",  breaks = 2^(1:7)*5, labels = labs_plot_y, limits = c(40, 1200)) + 
  scale_x_continuous(breaks = seq(0, 120, 20)) + 
  # theme_bw() + 
  geom_segment(aes(x = 12, y = 5*2^3, xend = 12, yend = 5*2^6),
               arrow = arrow(length = unit(0.5, "cm")), size = 2, color = "white") +
  geom_segment(aes(x = 12, y = 5*2^3, xend = 12, yend = 5*2^6),
               arrow = arrow(length = unit(0.5, "cm")), size = .5) +
  geom_richtext(aes(x = 6, y = 5*2^4.5, label = "Peak titre value"), size = label_text_size, angle = 90) +
  geom_segment(aes(x = 20, y = 5 * 2^6.5, xend = 120, yend = 5*2^6.5),
               arrow = arrow(ends='both', length = unit(0.5, "cm")), size = 2, color = "white") +
  geom_segment(aes(x = 20, y = 5*2^6.5, xend = 120, yend = 5*2^6.5),
               arrow = arrow(ends='both', length = unit(0.5, "cm")), size = .5) +
  geom_richtext(aes(x = 65, y = 5*2^6.9, label = "100 days post peak"), size = label_text_size) +
  geom_segment(aes(x = 120, y = 5*2^6, xend = 120, yend = 5*2^4),
               arrow = arrow(length = unit(0.5, "cm")), size = 2, color = "white") +
  geom_segment(aes(x = 120, y = 5*2^6, xend = 120, yend = 5*2^4),
               arrow = arrow(length = unit(0.5, "cm")), size = .5) +
  geom_segment(aes(x = 110, y = 5*2^6, xend = 130, yend = 5*2^6), linetype = "dashed", 
               color = "gray30") +
  geom_segment(aes(x = 110, y = 5*2^4, xend = 130, yend = 5*2^4), linetype = "dashed", 
               color = "gray30") +
  geom_segment(aes(x = 2, y = 5*2^6, xend = 22, yend = 5*2^6), linetype = "dashed", 
               color = "gray30") +
  geom_richtext(aes(x = 115, y = 5*2^5, label = "Titre wane"), size = label_text_size, angle = 90) +
  labs(x = "Days post infection", y = "Titre value") 


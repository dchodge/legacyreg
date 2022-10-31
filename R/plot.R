plot_pop_posterior <- function(i, j, k) {
  
  fit_current <- extract_saved_fit(i, j, k)
  
  dt_posterior_long <- extract_posterior_draws(fit_current,
                                               structure_arg = "long")
  
  # individual-level posteriors
  p_pop_posterior <- dt_posterior_long[parameter %in% pop_level_params] %>% 
    ggplot() + 
    geom_density(aes(x = value, fill = parameter), alpha = 0.5) +
    facet_wrap(~parameter, scales = "free") +
    theme_minimal()
  
  return(p_pop_posterior)
}

plot_ind_posterior_pred <- function(dt_all_post_pred,
                                    dt_all_raw_data,
                                    event_type_arg,
                                    include_data = TRUE) {
  
  dt_plot_pred <- dt_all_post_pred[event_type == event_type_arg]
  
  if(include_data == TRUE) {
    dt_plot_data <- dt_all_raw_data[event_type == event_type_arg]
  }
  
  p_out <- ggplot() + 
    geom_ribbon(data = dt_plot_pred,
                aes(x = t,
                    ymin = lo,
                    ymax = hi,
                    fill = interaction(titre_type, 
                                       exposure_type)),
                alpha = 0.2) +
    geom_line(data = dt_plot_pred,
              aes(x = t,
                  y = me,
                  group = interaction(titre_type)),
              alpha = 0.2, linetype = "dashed") +
    labs(title = paste0("Event type: ", event_type_arg),
         fill = "Titre type") +
    facet_wrap(~id, ncol = 12) +
    coord_cartesian(ylim = c(0, 10)) + 
    theme_minimal() + 
    theme(legend.position = "bottom")
  
  if(include_data == TRUE) {
    p_out <- p_out +
      geom_point(data = dt_plot_data,
                 aes(x = time_until_bleed,
                     y = info3,
                     colour = interaction(titre_type,
                                          exposure_type))) +
      labs(colour = "Titre type")
  }
  
  return(p_out)
}

plot_panel_a <- function() {
  
  labs_plot_y <- c("\u2264 40", "80", "160", "320", "640", "1280", "\u2265 2560")
  labs_plot_y_gtr <- c("1", "2", "4", "8", "16", "32", "64", "128")
  colorsdefined <- c("Third vaccine dose" = "#AA4A44", "BA1 infection" = "#0d98ba", "BA2 infection" = "#1c39bb")
  
  label_text_size <- 3
  
  dt_panel_1 <- data.table(
    x = c(1, 20, 120),
    y = c(3, 6, 4))
  
  p_out <- dt_panel_1 %>% 
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
    labs(x = "Days post infection", y = "Titre value") +
    theme_light()
  
  return(p_out)
}

plot_panel_b <- function(dt_pop_post_preds_sum, 
                         dt_raw_data_pop_plot) {
  
  p_out <- ggplot() + 
    geom_ribbon(data = dt_pop_post_preds_sum[!event_type == "BA.5 infection"],
                aes(x = t,
                    ymin = lo_nat,
                    ymax = hi_nat,
                    fill = titre_type, group = interaction(titre_type, event_type, exposure_type)),
                alpha = 0.5, show.legend = FALSE) +
    geom_line(data = dt_pop_post_preds_sum[!event_type == "BA.5 infection"],
              aes(x = t,
                  y = me_nat,
                  group = interaction(titre_type, event_type, exposure_type)),
              alpha = 0.75, linetype = "dashed") +
    geom_point(data = dt_raw_data_pop_plot[t <= 150],
               aes(x = t,
                   y = observed_titre_nat,
                   colour = titre_type,
                   group = titre_type), alpha = 0.15)  +
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
    theme_light() + 
    theme(legend.position = "bottom") 
  
  return(p_out)
}

plot_panel_c <- function(dt_in) {
  
  p_out <- dt_gtr_plot[!event_type == "BA.5 infection"] %>% 
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
  
  return(p_out)
}
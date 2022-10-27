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

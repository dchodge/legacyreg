extract_saved_fit <- function(i_arg, j_arg, k) {
  
  exposure_arg = exposure_loop[k]
  file_name_type_2 <- paste0(file_name_type[j_arg], "_", exposure_arg)
  
  out <- readRDS(paste0("outputs/stan_fits_tim/fit_",
                        file_name_var[i_arg], "_",
                        file_name_type_2, ".Rdata"))
  
  return(out)
  
}

extract_posterior_draws <- function(fit,
                                    structure_arg,
                                    ind_flag = FALSE) {
  
  if(ind_flag == FALSE) {  
    if(structure_arg == "wide") {
      # gathering posterior draws into a data.table using tidybayes package
      dt_posterior <- data.table(
        spread_draws(fit, t_p,
                     boost_i, boost_s,
                     wane_s,
                     sigma_t_p, sigma_b_i, sigma_b_s, sigma_w, sigma))[,
        type := "posterior"]
    }
    
    if(structure_arg == "long") {
      dt_posterior <- data.table(
        gather_draws(fit, t_p,
                     boost_i, boost_s,
                     wane_s,
                     sigma_t_p, sigma_b_i, sigma_b_s, sigma_w, sigma))[,
        type := "posterior"]
      
      # changing the names of the columns in the data.table
      setnames(dt_posterior,
               c(".variable", ".value"),
               c("parameter", "value"))
      
    }
  } else if(ind_flag == TRUE) {
    if(structure_arg == "wide") {
      # gathering posterior draws into a data.table using tidybayes package
      dt_posterior <- data.table(
        spread_draws(fit, t_p_ind[i], boost_i_ind[i], boost_s_ind[i],
                     wane_s_ind[i]))[,
        type := "posterior"]
      
      # changing the names of the columns in the data.table
      setnames(dt_posterior,
               c("i"),
               c("id"))
    }
    
    if(structure_arg == "long") {
      dt_posterior <- data.table(
        gather_draws(fit, t_p_ind[i], boost_i_ind[i], boost_s_ind[i],
                     wane_s_ind[i]))[,
        type := "posterior"]
      
      # changing the names of the columns in the data.table
      setnames(dt_posterior,
               c(".variable", ".value", "i"),
               c("parameter", "value", "id"))
      
    }
  }
  return(dt_posterior)
}

extract_posterior_pred_samples <- function(i, j, k,
                                           time_range = seq(0, 200, 1),
                                           ind_flag,
                                           n_samples = 500,
                                           by_args = c()) {
  # load fit
  fit_current <- extract_saved_fit(i, j, k)
  
  # extract posterior samples in wide format
  dt_posterior_wide <- extract_posterior_draws(fit_current,
                                               structure_arg = "wide",
                                               ind_flag)
  
  # simulate trajectories
  dt_posterior_pred <- simulate_trajectories(time_range, 
                                             dt_posterior_wide,
                                             n_samples,
                                             ind_flag,
                                             by_args)
  
  
  dt_posterior_pred[, `:=` (titre_type = titre_type_options[i],
                            event_type = event_type_options[j],
                            exposure_type = exposure_type_options[k])]
  
  return(dt_posterior_pred)
  
}


extract_all_pop_posteriors <- function(structure_arg = structure_arg) {
  
  dt_posterior_all <- data.table()
  
  for(i_arg in 1:4) {
    for(j_arg in 1:4) {
      for(k in 1:2)     {
        
        exposure_arg = exposure_loop[k]
        file_name_type_2 <- paste0(file_name_type[j_arg], "_", exposure_arg)
        
        
        if(j_arg != 1) {
          adjust_times <- TRUE
        } else if (j_arg == 1) {
          adjust_times <- FALSE
        }
        
        current_data <- subset_data(i_arg, j_arg, exposure_arg, adjust_times)
        
        if(nrow(current_data) == 0) {
          next
        } else {
          
          fit_current <- extract_saved_fit(i_arg, j_arg, k)
          dt_posterior_current <- extract_posterior_draws(fit_current,
                                                          structure_arg = structure_arg,
                                                          ind_flag = FALSE)
          
          fit_name <- paste0("fit_",
                             file_name_var[i_arg], "_",
                             file_name_type_2)
          
          dt_posterior_current[, fit_type := fit_name]
          
          dt_posterior_current[, `:=` (titre_type = titre_type_options[i_arg],
                                       event_type = event_type_options[j_arg],
                                       exposure_type = exposure_type_options[k])]
          
          dt_posterior_all <- rbind(dt_posterior_current, 
                                    dt_posterior_all)
          
        }
      }
    }     
  }
  return(dt_posterior_all)
}

extract_all_pop_post_preds <- function(time_range, n_samples) {

  dt_out <- data.table()

  for(i in 1:4) {
    for(j in 1:4) {
      for(k in 1:2) {

        dt_current <- extract_posterior_pred_samples(i, j, k,
                                                     time_range,
                                                     n_samples,
                                                     ind_flag = FALSE,
                                                     by_args = c())

        dt_out <- rbind(dt_out, dt_current)
      }
    }
  }
  return(dt_out)
}

extract_all_ind_posterior_preds <- function(by_args = c()) {
  
  dt_posterior_all <- data.table()
  
  for(i_arg in 1:4) {
    for(j_arg in 1:4) {
      for(k in 1:2)     {
        
        print(i_arg)
        print(j_arg)
        print(k)
        
        exposure_arg = exposure_loop[k]
        file_name_type_2 <- paste0(file_name_type[j_arg], "_", exposure_arg)
        
        if(j_arg != 1) {
          adjust_times <- TRUE
        } else if (j_arg == 1) {
          adjust_times <- FALSE
        }
        
        dt_current <- subset_data(i_arg, j_arg, exposure_arg, adjust_times)
        
        if(nrow(dt_current) == 0) {
          next
        } else {
          
          # fit_current <- extract_saved_fit(i_arg, j_arg, k)
          dt_posterior_current <- extract_posterior_pred_samples(i_arg, 
                                                                 j_arg,
                                                                 k, 
                                                                 ind_flag = TRUE,
                                                                 by_args = c())
          
          dt_posterior_summary <- summarise_trajectories(dt_posterior_current, 
                                                         ind_flag = TRUE)
          
          dt_posterior_all <- rbind(dt_posterior_summary, 
                                    dt_posterior_all)
          
        }
      }
    }     
  }
  setnames(dt_posterior_all, "id", "stan_id")
  return(dt_posterior_all)
}

extract_all_raw_data <- function() {
  
  out <- data.table()
  for(i_arg in 1:4) {
    for(j_arg in 1:4) {
      for(k in 1:2)     {
        
        exposure_arg = exposure_loop[k]
        
        
        if(j_arg != 1) {
          adjust_times <- TRUE
        } else if (j_arg == 1) {
          adjust_times <- FALSE
        }
        
        dt_current <- subset_data(i_arg, j_arg, exposure_arg, adjust_times)
        if(nrow(dt_current) == 0) {
          next
        } else {
          dt_current[, `:=` (titre_type = titre_type_options[i_arg],
                             event_type = event_type_options[j_arg],
                             exposure_type = exposure_type_options[k])]
          
          out <- rbind(dt_current, out)
        }
      }
    }
  }
  return(out)
}

extract_pop_post_pred_plot_data <- function(time_range = seq(0, 150, 1),
                                            n_samples) {
  
  dt_pop_post_preds <- extract_all_pop_post_preds(time_range,
                                                  n_samples = n_samples)
  
  out <- summarise_trajectories(dt_pop_post_preds, 
                                ind_flag = FALSE)
  
  out[,  `:=` (me_nat = 32*2^me,
               lo_nat = 32*2^lo,
               hi_nat = 32*2^hi)]
  
  
  out[, `:=` (titre_type = fct_relevel(titre_type, c("Wildtype")),
              event_type = fct_relevel(event_type, c("Vaccination")),
              exposure_type = fct_relevel(exposure_type, c("Naive")))]
  
  return(out)
}

extract_raw_data_pop_post_pred <- function() {
  
  dt_raw_data <- extract_all_raw_data()
  
  out <- dt_raw_data[, c("time_until_bleed", "info3", 
                         "titre_type", "event_type", 
                         "exposure_type")]
  setnames(out, 
           c("time_until_bleed", "info3"), 
           c("t", "observed_titre"))
  
  out[, observed_titre_nat := 32*2^(observed_titre)]
  
  out[, `:=` (titre_type = fct_relevel(titre_type, c("Wildtype")),
              event_type = fct_relevel(event_type, c("Vaccination")),
              exposure_type = fct_relevel(exposure_type, c("Naive")))]
  
  return(out)
}

extract_panel_c_data <- function() {
  
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
  
  dt_gtr_plot <- update_labels_panel_c(dt_gtr)
  
  return(dt_gtr_plot)
}


# plotting the fitted curves
boost_wane_fun <- function(t, t_p, boost_i, boost_s, wane_s) {
  
  if (t < t_p) { 
    mu = boost_i + boost_s * t; 
  } else {
    mu = wane_s * (t - t_p) + 
      boost_i +
      boost_s * t_p; 
  }
  
  return(mu)
}

# function that simulates the boost and wane trajectories over time, give
# a data.table of posterior samples
simulate_trajectories <- function(time_range,
                                  dt_posterior,
                                  n_samples,
                                  ind_flag,
                                  by_args) {
  
  times <- data.table::data.table(
    t = time_range
  )

  dt_samples <- dt_posterior[, .SD[.draw %in% 1:n_samples], by = by_args]
  
  by_args_complete = c(by_args, "t", ".draw")
  
  if(ind_flag == TRUE) {
    by_args_complete <- c(by_args_complete, "id")  
  }
  
  if(ind_flag == TRUE) {
    trajectories <- merge(
      dt_samples[, tid := 1], times[, tid := 1], by = "tid",
      allow.cartesian = TRUE
    )[,
      tid := NULL][,
                   exp_titre := boost_wane_fun(
                     t, t_p_ind, boost_i_ind, boost_s_ind, wane_s_ind),
                   by = by_args_complete
      ]
  } else if(ind_flag == FALSE) {
    trajectories <- merge(
      dt_samples[, tid := 1], times[, tid := 1], by = "tid",
      allow.cartesian = TRUE
    )[,
      tid := NULL][,
                   exp_titre := boost_wane_fun(
                     t, t_p, boost_i, boost_s, wane_s),
                   by = by_args_complete
      ]
  }
  
  return(trajectories[])
}

summarise_trajectories <- function(dt_in, 
                                   ind_flag,
                                   by_args = c("titre_type", 
                                               "event_type",
                                               "exposure_type")) {
  by_args_complete = c("t", by_args)
  if(ind_flag == TRUE) {
    by_args_complete <- c(by_args_complete, "id")
  }
  
    out <- dt_in[, .(me = quantile(exp_titre, 0.5),
                     lo = quantile(exp_titre, 0.025),
                     hi = quantile(exp_titre, 0.975)), 
                 by = by_args_complete]
    
  return(out)
  
}

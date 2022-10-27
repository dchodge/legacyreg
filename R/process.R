# wrapper for the filtering and fitting functions
subset_and_fit <- function(i, j,
                           exposure,
                           initialise_from_priors, 
                           adjust_times) {
  
  # running functions to filter data and create list for Stan
  dt_data <- subset_data(i, j, exposure, adjust_times)
  data_list_stan <- stan_data_fun(dt_data)
  
  # set initialisation either from priors or set to zero  
  if(initialise_from_priors == TRUE) {
    inits <- list(stan_inits_fun(dt_data)(seed = 5),
                  stan_inits_fun(dt_data)(seed = 6),
                  stan_inits_fun(dt_data)(seed = 7),
                  stan_inits_fun(dt_data)(seed = 9))
  } else {
    inits <- 0
  }
  
  fit_out <- model_A$sample(
    data = data_list_stan,    # named list of data
    iter_warmup = 1000,          # number of warmup iterations per chain
    iter_sampling = 1000,            # total number of iterations per chain
    init = inits,
    chains = 4,            # number of cores (could use one per chain)
    parallel_chains = 4,
    adapt_delta = 0.9999,
    max_treedepth = 15,
    refresh = 200
  )
  
  return(fit_out)
}
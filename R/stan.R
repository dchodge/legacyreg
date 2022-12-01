# putting together filtered data into list for Stan 
stan_data_fun <- function(dt_titres) {
  
  out <- list(
    N = nrow(dt_titres),
    N_ind = length(dt_titres$id %>% unique()),
    # ids = dt_titres[, stan_id := .GRP, by = id]$stan_id,
    ids = dt_titres[, stan_id],
    t = dt_titres$time_until_bleed,
    y = dt_titres$info3
  )
  
  return(out)
}

# initial values function
stan_inits_fun <- function(dt) {
  
  n = dt[, uniqueN(id)]
  
  function(seed) {
    
    set.seed(seed)
    
    inits <- list(
      
      # population-level parameters
      t_p = rnorm(1, 8, 2),
      boost_i = rnorm(1, 4, 2),
      boost_s = rnorm(1, 0.5, 1),
      wane_s = rnorm(1, -0.01, 0.5),
      # individual-level parameters — centered parameterisation
      # t_p_ind = rep(sigma_t_p_eval, n),
      # boost_i_ind = rep(sigma_b_i_eval, n),
      # boost_s_ind = rep(sigma_b_s_eval, n),
      # wane_s_ind = rep(sigma_w_eval, n),
      # individual-level parameters — non-centered parameterisation
      z_p = rnorm(n, 0, 1),
      z_b_i = rnorm(n, 0, 1),
      z_s_i = rnorm(n, 0, 1),
      z_w_s = rnorm(n, 0, 1),
      # noise parameters
      sigma_t_p = truncnorm::rtruncnorm(1, a = 0, b = Inf, mean = 0, sd = 3),
      sigma_b_i = truncnorm::rtruncnorm(1, a = 0, b = Inf, mean = 0, sd = 2),
      sigma_b_s = truncnorm::rtruncnorm(1, a = 0, b = Inf, mean = 0, sd = 1.0),
      sigma_w = truncnorm::rtruncnorm(1, a = 0, b = Inf, mean = 0, sd = 0.2),
      sigma = truncnorm::rtruncnorm(1, a = 0, b = Inf, mean = 0, sd = 2.0)
    )
    
    return(inits)
  }
}


# sample from the population-level priors k times
sample_pop_priors <- function(k = 1000) {
  
  out <- data.table(.draw = 1:k)
  
  #--- population-level priors
  out[, t_p := rnorm(k, 20, 3.0)]
  out[, boost_i := rnorm(k, 3.5, 2.5)]
  out[, boost_s := truncnorm::rtruncnorm(k, a = 0, mean = 0.02, sd = 0.05)]
  out[, wane_s := truncnorm::rtruncnorm(k, b = 0, mean = -0.02, sd = 0.05)]
  out[, sigma := truncnorm::rtruncnorm(k, a = 0, mean = 0, sd = 2)]
  
  return(out[])
  
}

# sample from the population-level priors k times
sample_ind_priors <- function(n, k, struct_arg = "wide") {
  
  pop_draws_list <- replicate(n = n, sample_pop_priors(k), simplify = FALSE)
  
  out <- data.table(do.call(rbind, pop_draws_list))
  out[, id := sort(rep(1:n, k))]
  
  setcolorder(out, "id")
  
  #---individual-level params
  # non-centered parameterisation
  out[, z_p := rnorm(k, 0, 1), by = "id"]
  out[, z_b_i := rnorm(k, 0, 1), by = "id"]
  out[, z_s_i := rnorm(k, 0, 1), by = "id"]
  out[, z_w_s := rnorm(k, 0, 1), by = "id"]
  
  # priors for noise parameters
  out[, sigma_t_p := rtrunc(k, "cauchy", a = 0, b = Inf, location = 0, scale = 5), by = "id"]
  out[, sigma_b_i := rtrunc(k, "cauchy", a = 0, b = Inf, location = 0, scale = 5), by = "id"]
  out[, sigma_b_s := rtrunc(k, "cauchy", a = 0, b = Inf, location = 0, scale = 5), by = "id"]
  out[, sigma_w   := rtrunc(k, "cauchy", a = 0, b = Inf, location = 0, scale = 5), by = "id"]
  
  # priors for noise parameters
  out[, t_p_ind :=  t_p + z_p*sigma_t_p]
  out[, boost_i_ind := boost_i + z_b_i*sigma_b_i]
  out[, boost_s_ind := boost_s + z_s_i*sigma_b_s]
  out[, wane_s_ind := wane_s + z_w_s*sigma_w]
  
  if(struct_arg == "long") {
    out <- melt(out,
                id.vars = c("id", ".draw"),
                variable.name = "parameter")
  }
  
  return(out[])
  
}
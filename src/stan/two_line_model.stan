functions {
  
// // Function to estimate the titre_boost_wane across 6 variants (wt, a, d, om1, om2, om5)
  real titre_boost_wane(real boost_i,
                        real boost_s,
                        real wane_s,
                        real t_p,
                        real t) {
    
    // mu is the expected titre at time t, given the current parameters
    real mu;
    
    // if time is less than the timing of the peak parameter, to determine whether the individual is
    // boosting or waning
    if (t < t_p) { 
        mu = boost_i + boost_s * t; 
    } else {
        mu = wane_s * (t - t_p) + boost_i + boost_s * t_p; 
    }
    
    // returning only positive values or zeros
    mu = fmax(mu, 0);
    
    return(mu);
  } 
}

data {
    int N; // number of observations
    int N_ind; // number of individuals
    array[N] int ids; // ids of each individual
    vector[N] t; // timing of bleed
    vector[N] y;  // titre value at bleed
}

parameters {

  //--- parameters for the boost/wane model
  // population-level parameters
  real<lower = 0> t_p; // timing of peak parameter
  real boost_i; // initial titre value
  real<lower = 0> boost_s; // gradient of boost parameter
  real<upper = 0> wane_s; // gradient of wane parameter
  
  // individual-level parameters, same interpretations as population-level parameters
  // on an additive scale: parameter + individual variation
  // vector[N_ind] t_p_ind;
  // vector[N_ind] boost_i_ind;
  // vector[N_ind] boost_s_ind;
  // vector[N_ind] wane_s_ind;

  vector[N_ind] z_p_ind;
  vector[N_ind] z_b_i_ind;
  vector[N_ind] z_s_i_ind;
  vector[N_ind] z_w_s_ind;

  //--- measurement error parameters
  real<lower = 0> sigma_t_p;
  real<lower = 0> sigma_b_i;
  real<lower = 0> sigma_b_s;
  real<lower = 0> sigma_w;
  real<lower = 0> sigma;

  // real sigma_t_p;
  // real sigma_b_i;
  // real sigma_b_s;
  // real sigma_w;
  // real<lower = 0> sigma;
}

transformed parameters{
  
  // using a non-centered parameterisation, i.e. individual-level parameters
  // given by: p_ind = p_pop + std_normal*sigma_pop
  vector[N_ind] t_p_ind;
  vector[N_ind] boost_i_ind;
  vector[N_ind] boost_s_ind;
  vector[N_ind] wane_s_ind;

  t_p_ind  =  t_p + z_p_ind*sigma_t_p;
  boost_i_ind = boost_i + z_b_i_ind*sigma_b_i;
  boost_s_ind = boost_s + z_s_i_ind*sigma_b_s;
  wane_s_ind = wane_s + z_w_s_ind*sigma_w;
  
  vector[N] titre_est;
  
  // Get the estimated titre landscape given the parameters
  for (i in 1:N) {
    // titre_est[i] = titre_boost_wane(boost_i,
    //                                 boost_i_ind[ids[i]],
    //                                 boost_s,
    //                                 boost_s_ind[ids[i]],
    //                                 wane_s,
    //                                 wane_s_ind[ids[i]],
    //                                 t_p,
    //                                 t_p_ind[ids[i]],
    //                                 t[i]);
                                    
      titre_est[i] = titre_boost_wane(boost_i_ind[ids[i]],
                                      boost_s_ind[ids[i]],
                                      wane_s_ind[ids[i]],
                                      t_p_ind[ids[i]],
                                      t[i]);
  }
}

model {
  // likelihood
  for (i in 1:N) {
    if (y[i] >= 7) {
      target += normal_lccdf(7 | titre_est[i], sigma); // if titre value is above upper censoring threshold
    } else if (y[i] <= 1) {
      target += normal_lcdf(1 | titre_est[i], sigma); // if titre value is below lower censoring threshold
    } else {
      y[i] ~ normal(titre_est[i], sigma); // normally distributed likelihood function
    }
  }

  //--- priors
  // population-level priors
  t_p ~ normal(20, 3.0);
  boost_i ~ normal(3.5, 2.5);
  boost_s ~ normal(0.02, 0.05) T[0, ];
  wane_s ~ normal(-0.02, 0.05) T[, 0];
  
  // centered parameterisation
  // t_p_ind ~ normal(t_p, sigma_t_p);
  // boost_i_ind ~ normal(boost_i, sigma_b_i);
  // boost_s_ind ~ normal(boost_s, sigma_b_s);
  // wane_s_ind ~ normal(wane_s, sigma_w);
  
  // non-centered parameterisation
  z_p_ind ~ std_normal();
  z_b_i_ind ~ std_normal();
  z_s_i_ind ~ std_normal();
  z_w_s_ind ~ std_normal();

  sigma_t_p ~ cauchy(0, 5);
  sigma_b_i ~ cauchy(0, 5); 
  sigma_b_s ~ cauchy(0, 0.5);
  sigma_w   ~ cauchy(0, 0.5);
    
  sigma ~ normal(0, 2) T[0,];
}

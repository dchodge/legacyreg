functions {

// Function to estimate the titre_boost_wane across 6 variants (wt, a, d, om1, om2, om5)
  real titre_boost_wane(real boost_i, real boost_i_ind, real boost_s, real boost_s_ind, real wane_s, real wane_s_ind, 
    real t_p, real t_p_ind,  real t) {

    // For the six variants
    real mu;
    //print(boost_s);
    if (t < (t_p + t_p_ind)) { 
        mu = (boost_i + boost_i_ind) + (boost_s + boost_s_ind) * t; 
    } else {
        mu = (wane_s + wane_s_ind) * (t - (t_p + t_p_ind)) + (boost_i + boost_i_ind) + (boost_s + boost_s_ind) * (t_p + t_p_ind); 
    }
    mu = fmax(mu, 0);
    return(mu);
  } 
}

data {
    int N;  // number of indiviiduals (217)
    int N_ind;
    array[N] int ids; // 217 x 4 matrix of the time (days) of vaccine dose. -1 means didn't get one
    vector[N] t; // 217 x 4 matrix of the type of vaccine dose. -1 means didn't get one. 1 = PM, 2 = AZ
    vector[N] y;  // 217 vector of the type of vaccine  dose mix at 3rd dose. -1 means didn't get one. 1 = PM/PM, 2 = AZ/AZ
}
parameters {
  // array is 6: PM 1st dose/PM 2nd dose/ AZ 1st dose/ AZ 2nd dose/ PM 3rd dose after PMPM/ PM 3rd dose after AZAZ
  // vector is 6 variatns
  real<lower = 0, upper = 100> t_p;
  vector[N_ind] t_p_ind;

  real boost_i;
  vector[N_ind] boost_i_ind;
  real<lower = 0, upper = 0.3> boost_s;
  vector<lower = -0.3, upper = 0.3>[N_ind] boost_s_ind;
  real<lower = -0.1, upper = 0> wane_s;
  vector<lower = -0.1, upper = 0.1>[N_ind] wane_s_ind;
  // normal distribution
  real<lower = 0> sigma_t_p;
  real<lower = 0> sigma_b_i;
  real<lower = 0> sigma_b_s;
  real<lower = 0> sigma_w;

  real<lower = 0> sigma;
}
transformed parameters{
  vector[N] titre_est;
  for (i in 1:N) {
    titre_est[i] = titre_boost_wane(boost_i, boost_i_ind[ids[i]], boost_s, boost_s_ind[ids[i]], wane_s, wane_s_ind[ids[i]], t_p, t_p_ind[ids[i]], t[i]);
    //print(titre_est[i]);
  }
  // Get the estimated titre landscape given the parameters
}
model {
  // likelihood
  for (i in 1:N) {
    if (y[i] >= 7) {
      target += normal_lccdf(7 | titre_est[i], sigma);
    } else if (y[i] <= 1) {
      target += normal_lcdf(1 | titre_est[i], sigma);
    } else {
      y[i] ~ normal(titre_est[i], sigma);
    }
    //print("titre_est[i]: ", titre_est[i], ". y[i]: ", y[i], ". target", target());
  }

  t_p ~ uniform(0, 40);
  t_p_ind ~ normal(0, sigma_t_p);
  sigma_t_p ~ exponential(0.1);

  boost_i ~ uniform(0, 7);  // normal(5, 3)
  boost_i_ind ~ normal(0, sigma_b_i);
  sigma_b_i ~ exponential(1);

  boost_s ~ uniform(0, 0.3); // normal(0, 0.1)
  boost_s_ind ~ normal(0, sigma_b_s); // vector
  sigma_b_s ~ exponential(1);

  wane_s ~ uniform(-0.1, 0); // normal(0, 0.02)
  wane_s_ind ~ normal(0, sigma_w); // vector
  sigma_w ~ exponential(1);

  sigma ~ exponential(1);
}

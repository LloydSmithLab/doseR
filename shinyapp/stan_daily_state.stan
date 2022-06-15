
data {
  
  // experimental conditions
  int<lower = 0> n_points; // number of data points
  int<lower = 0> n_individuals; // number of individuals
  int<lower = 0> n_timepoints; // number of timepoints
  int timepoint[n_timepoints]; // time points, with observed data
  int D_target[n_points]; // doses
  int individual[n_points]; // individual identity
  int time[n_points]; // time points, with observed data
  
  
  // observations
  int obs_diseased[n_points]; // response as individual state at time t (not diseased/diseased)
  
  // priors
  real<lower = 0, upper = 1> pinf_prior_mean;
  real<lower = 0> pinf_prior_sd;
  //real<lower = 0, upper = 1> pcont_prior_mean;
  //real<lower = 0> pcont_prior_sd;
  real<lower = 0> kk_prior_mean;
  real<lower = 0> kk_prior_sd;
  real<lower = 0> cc_prior_mean;
  real<lower = 0> cc_prior_sd;
  
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////

parameters {
  
  real<lower = 0, upper = 1> pinf; // probability of successful initial infections, p_entry * p_infect
  //real<lower = 0, upper = 1> pcont;
  real<lower = 0> kk; // viral population growth rate linking V_t to V_0
  real<lower = 0> cc; // tolerance, linking symptom occurrence to viral load
  //vector<lower = 0>[n_individuals] V_0; // successful initial infections, in numbers of virions
  
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////

transformed parameters {
  
  //vector[n_points] V_eff; 
  //vector[n_points] V_t; // viral population size at time t, in number of virions
  //vector[n_points] lambda_t; // instantaneous rate of transition to symptomatic disease (given V_t)
  //vector[n_points] Lambda_t; // cumulative rate of transition to symptomatic disease (given V_t) = integral V_t from 0 to t
  vector[n_points] f_t; // function f(t)
  
  for (i_point in 1:n_points){ // loop on all data points
    
    //int i_individual = individual[i_point]; // moved to model
    //int i_D_target = D_target[i_point];
    //int i_time = time[i_point];
    // int i_time = timepoint[i_point];
    
    //V_t[i_point] = V_0[i_individual] * exp(kk * time[i_time]);
    
    //lambda_t[i_point] = (1/exp(cc)) * V_t[i_point];
    
    //Lambda_t[i_time] = ((1/exp(cc)) * V_0[i_individual] / kk) * (exp(kk * time[i_time]) - 1);
    

      f_t[i_point] = - ((1/exp(cc)) * (1/kk) * exp(kk * time[i_point]) - 1);
    
  }
  
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////

model {
  
  for (i_point in 1:n_points){ // loop on all data points
    
    //int i_individual = individual[i_point];
    //int i_time = time[i_point];
    //int dose = D_target[i_point];
    
    //V_0[i_individual] ~ poisson(D_target[i_individual] * p_success);
    
    if(obs_diseased[i_point] == 0) // P(y < t_E | t_E) = observation happened before end of incubation 
      target += D_target[i_point] * pinf * (exp(f_t[i_point]) - 1);
    
    //log(P(tE> t)) =Dp(ea(t)-1)
    
    else // P(y - 1 < t_E <= y | t_E) = end of incubation was reached (need to remove the following 1s?) 
      target += 1 - (D_target[i_point] * pinf * (exp(f_t[i_point]) - 1));
    //target += log_diff_exp(
    //  D_target[i_point] * p_success * (exp(a_t[i_point - 1]) - 1), // issue with time 1?
    // D_target[i_point] * p_success * (exp(a_t[i_point]) - 1));
  }
  
  // priors
  pinf ~ normal(pinf_prior_mean, pinf_prior_mean);
  kk ~ normal(kk_prior_mean, kk_prior_sd);
  cc ~ normal(cc_prior_mean, cc_prior_sd);
  
}

// need to deal with absence of data after death... truncate the dataset after first symptom observation?
// check whether it deals well with the end of the experiment issue?
// export true incubation time?


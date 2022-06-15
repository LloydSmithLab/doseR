set.seed(8)

targets = c(10^1, 10^2, 10^3, 10^4, 10^5)
nn = 20
tt = seq(0, 14, 1)

f_infection_exp_cont = function(D_target, pinf, pcont, kk, tt, cc) {
  
  V_0 = rpois(1, D_target * pinf)
  V_eff = rbinom(1, V_0, (1 - pcont))
  V_t = V_eff * exp(kk * tt)
  lambda_t = cc * V_t  
  #P_transition_t = lambda_t * exp(-lambda_t)
  P_disease_t = 1 - exp(-(1/exp(cc) * V_0 / kk) * (exp(kk * tt) - 1))
  
  data = as.data.frame(cbind(D_target, pinf, pcont, kk, tt, cc, V_0, V_eff, V_t, lambda_t,  P_disease_t))
  
  return(data)
  
}

f_simulate = function(targets, nn, pinf, pcont, kk, tt, cc){
  
  parameters = c(pinf = pinf, pcont = pcont, kk = kk, cc = cc, nn = nn)
  
  data = NULL
  
  for (ii in 1:length(targets)){
    
    D_target = targets[ii]
    
    for (jj in 1:nn){
      
      data_temp = f_infection_exp_cont(D_target,  pinf = pinf, pcont = pcont, kk = kk, tt = tt, cc = cc)
      data_temp$sim = paste(targets[ii], jj, sep = "_")
      data_temp$nn = nn
      
      data = rbind(data, data_temp)
      
    }
    
  }
  
  return(data)
  
}

input = NA
input$pinf = 0.5
input$pcont = 0.5
input$kk = 1
input$cc = 0.001

data = f_simulate(targets, nn, input$pinf, input$pcont, input$kk, tt, input$cc)
data = f_simulate(targets, nn, 0.1, 0.6, 0.3, tt, 12)

write.table(data, "data_example.csv", row.names = F, sep = ";", dec = ".", quote = F)

plot(a$tt, xlim = c(0, 14), ylim = c(0,14))

# Approximated by histology
data[data$V_0 > 0, "infected"] = 1
data[data$V_0 == 0, "infected"] = 0

# From weight loss and symptoms
data$diseased = NA
for (ii in 1:nrow(data)){
  data$diseased[ii] = rbinom(1, 1, data$P_disease_t[ii])
  if (ii > 1){
    if (data$sim[ii] == data$sim[ii - 1] &
        data$diseased[ii - 1] == 1 & !is.na(data$diseased[ii - 1] == 1)
    ){
      data$diseased[ii] = 1
    }
  }
}

data[is.na(data$diseased), "diseased"] = 0

data[data$lambda_t<0 | is.na(data$lambda_t<0), "lambda_t"] = 0
#data[data$Lambda_t<0 | is.na(data$Lambda_t<0), "Lambda_t"] = 0
data[data$P_disease_t<0 | is.na(data$P_disease_t<0), "P_disease_t"] = 0

f_summary = function(data){
  
  infected = unique(data[data$infected == 1, "sim"])
  diseased = unique(data[data$diseased == 1, "sim"])
  
  summary = unique(data[, c("D_target", "sim")])
  
  summary$infected = summary$diseased = 0
  
  summary[is.element(summary$sim, infected), "infected"] = 1
  summary[is.element(summary$sim, diseased), "diseased"] = 1
  
  summary$incubation = NA
  for (ii in 1:nrow(summary)){
    if (nrow(data[data$sim == summary$sim[ii] & data$diseased == 1, ]) > 0){
      summary$incubation[ii] = min(data[data$sim == summary$sim[ii] & data$diseased == 1, "tt"])
    }else{
      summary$incubation[ii] = Inf
    }
  }
  
  return(summary)
}

f_summary(data)




cat(
  "

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

"
, file = "shinyapp/www/stan_daily_state.stan", sep = " ", fill = FALSE, labels = NULL,
append = FALSE)

daily_state <- stan_model("shinyapp/www/stan_daily_state.stan")

# Realistc simulated dataset
stan_data = list(D_target = as.integer(data$D_target), 
                 individual = as.integer(as.factor(data$sim)),
                 time = as.integer(data$t),
                 obs_diseased = as.integer(data$diseased), 
                 last_obs_time = as.integer(max(data$t)),
                 n_points = nrow(data),
                 n_individuals = length(unique(data$sim)),
                 n_timepoints = length(unique(data$t)),
                 timepoint = unique(data$t),
                 pinf_prior_mean = 0.1*0.6,
                 pinf_prior_sd = 0.1,
                 kk_prior_mean = 3,
                 kk_prior_sd = 0.5,
                 cc_prior_mean = 12,
                 cc_prior_sd = 0.5) # add priors

#data_comp = data

data = subset(data, !is.na(diseased))
#data = subset(data, treatment_num != 1000 | day <8)

fit1 <- sampling(daily_state, data = stan_data, chains = 4, iter = 1000, refresh = 0)

print(fit1, pars = c("pinf", "kk", "cc"), probs = c(0.025, 0.5, 0.975), digits = 5)

traceplot(pars = c("pinf", "kk", "cc"), fit1)

stan_dens(pars = c("pinf", "kk", "cc"), 
          fit1)

stan_hist(pars = c("pinf", "kk", "cc"), 
          fit1)

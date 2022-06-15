targets = c(10^1, 10^2, 10^3, 10^4, 10^5)
nn = 5
tt = seq(0, 14, 0.1)

f_infection_exp_cont = function(D_target, pinf, pcont, kk, tt, cc) {
  
  V_0 = rpois(1, D_target * pinf)
  V_eff = rbinom(1, V_0, (1 - pcont))
  V_t = V_eff * exp(kk * tt)
  lambda_t = cc * V_t  
  P_transition_t = lambda_t * exp(-lambda_t)
  P_disease_t = 1 - exp(-(cc * V_0 / kk) * (exp(kk * tt) - 1))
  
  data = as.data.frame(cbind(D_target, pinf, pcont, kk, tt, cc, V_0, V_eff, V_t, lambda_t, P_transition_t, P_disease_t))
  
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
data[data$Lambda_t<0 | is.na(data$Lambda_t<0), "Lambda_t"] = 0
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

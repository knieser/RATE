# Simulation script
runSimulations = function(N, p, a1, tau, delta_vals, loops){
  
  Ymodel = y~(1+g)*(trt+z1+z2+z3)
  grps = 1:length(p)
  marg_tau = sum(p*tau) # marginal treatment effect
  
  # initialize
  strat_est = matrix(data=NA,nrow=loops,ncol=length(grps))
  strat_var = matrix(data=NA,nrow=loops,ncol=length(grps))
  re = matrix(data=NA,nrow=loops,ncol=length(grps))
  minimax = array(data=NA,dim=c(length(delta_vals),loops,length(grps)))

  for(l in 1:loops){
   
    # simulate data
    smp = simulateData(N,p,a1,tau)
    
    # stratified estimates 
    strat_output <- estStratified(smp,"trt","g",Ymodel)
    strat_est[l,] = strat_output$estimates[,"estimate"]
    strat_var[l,] = (strat_output$estimates[,"std_err"])^2
    varY = strat_output$resid_var

    # random effects estimates
    re_model <- lmer(y ~ trt + z1 + z2 + z3 + (trt|g), data=smp)
    re[l,] = coef(re_model)$g[,2]
    
    # minimax-based synthetic sample estimates
    minimax[,l,] = estSynthetic_minimax(strat_est[l,], strat_var[l,], varY, delta_vals)$estimates
  }
  
  # compile results
  results = list(
    "parameters" = list("N" = N, "p" = p, "a1" = a1, "tau" = tau, 
                        "delta" = delta_vals,
                        "Ymodel" = Ymodel,"loops" = loops),
    "marg_tau" = marg_tau,
    "strat" = strat_est, 
    "re" = re, 
    "minimax" = minimax)
  
  return(results)
}
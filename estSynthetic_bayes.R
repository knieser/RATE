# Bayes-based synthetic sample estimates

estSynthetic_bayes <- function(strat_est, strat_var, varY, var_vals, rho_vals) {

  # get list of groups
  grps = 1:length(strat_est)
  
  # pre-allocate
  q = array(data=NA, dim=c(length(var_vals),length(rho_vals),length(grps),length(grps)))
  est = array(data=NA, dim=c(length(var_vals),length(rho_vals),length(grps)))
  
  # get synthetic sample estimates for each group, var, and rho
  for (j in 1:length(grps)){
    for (s in 1:length(var_vals)) {
      for (r in 1:length(rho_vals)){
        V = sum(1/(var_vals[s] + strat_var)) - 1/(var_vals[s] + strat_var[j])
        q[s,r,j,j] = (var_vals[s]*(1-rho_vals[r]) + 1/V) /
          (var_vals[s]*(1-rho_vals[r]) + 1/V + strat_var[j])
        q[s,r,j,-j] = (q[s,r,j,j]*(strat_var[j] + var_vals[s]*(1-rho_vals[r])) - var_vals[s]*(1-rho_vals[r])) /
          (var_vals[s] + strat_var[-j])
        est[s,r,j] = sum(q[s,r,j,]*strat_est)
      }
    }
  }
  
  output <- list('estimates' = est,
                 'var_tau' = var_vals,
                 'rho' = rho_vals,
                 'q' = q)
  
  return(output)
}

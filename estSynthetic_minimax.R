# Minimax-based synthetic sample estimates

estSynthetic_minimax <- function (strat_est, strat_var, delta_vals) {

  # get list of groups
  grps = 1:length(strat_est)

  # pre-allocate
  q = array(data=NA, dim=c(length(delta_vals),length(grps),length(grps)))
  est = array(data=NA, dim=c(length(delta_vals),length(grps)))
  
  # get synthetic sample estimates for each group and delta value
  for (j in 1:length(grps)){
    V = (sum(1/strat_var) - 1/strat_var[j])^(-1)
    for (d in 1:length(delta_vals)) {
      q[d,j,j] = (delta_vals[d]^2 + V) / 
        (delta_vals[d]^2 + V + strat_var[j])
      q[d,j,-j] = (q[d,j,j]*(strat_var[j] + delta_vals[d]^2) - delta_vals[d]^2) / 
        strat_var[-j]
      est[d,j] = sum(q[d,j,]*strat_est)
    }
  }
  
  output <- list('estimates' = est,
                 'delta' = delta_vals,
                 'q' = q)
  return(output)
}
estRATE <- function(strat_est, strat_SE, decision_rule, delta) {

  grps = 1:length(strat_est)
  strat_var = strat_SE^2
  
  # pre-allocate
  q = matrix(data=NA, ncol = length(grps), nrow = length(grps))
  est = 1:length(grps)
  SE = 1:length(grps)
  
  if (decision_rule == 1){
    # Bayes avg risk
    for (j in 1:length(grps)){
      B       = (sum(1/(delta^2 + strat_var)) - 1/(delta^2 + strat_var[j]))^(-1)
      q[j,j]  = (delta^2 + B) / (delta^2 + B + strat_var[j])
      q[j,-j] = (1 - q[j,j]) * B / (delta^2 + strat_var[-j])
      est[j]  = sum(q[j,]*strat_est)
      SE[j]   = sqrt(sum(q[j,]^2*strat_var))
    }
  } else if (decision_rule == 2){
      # minimax risk
      for (j in 1:length(grps)){
        V       = (sum(1/strat_var) - 1/strat_var[j])^(-1)
        q[j,j]  = (delta^2 + V) / (delta^2 + V + strat_var[j])
        q[j,-j] = (1 - q[d,j,j]) * V / strat_var[-j]
        est[j]  = sum(q[j,]*strat_est)
        SE[j]   = sqrt(sum(q[j,]^2*strat_SE^2))
      }
    }
  
  output = data.frame(
    group = grps,
    estimates = est,
    SE = SE
  )
  output <- list('q' = q,
                 'estimates' = data.frame(
                   group = grps,
                   est = est,
                   SE = SE, 
                   ci_lower = est - 1.96*SE,
                   ci_upper = est + 1.96*SE)
                 )
  return(output)
}

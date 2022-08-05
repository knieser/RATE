estRATE <- function(strat_est, strat_sigma, decision_rule, phi) {

  grps = 1:length(strat_est)
  ngrps = length(grps)
  Q = matrix(data = NA, ncol = ngrps, nrow = ngrps)
  
  if (decision_rule == 1){
    # Bayes avg risk
    W = phi^2 * solve(2*strat_sigma + phi^2*diag(ngrps))
    for (j in 1:ngrps){
      Q[j,] = (1 - sum(W[,j])) * rowSums(W) / sum(W) + W[,j]
    }
  } else if (decision_rule == 2){
    # minimax risk
    for (j in 1:ngrps){
      eeT = diag(ngrps)[j,] %*% t(diag(ngrps)[j,])
      W = phi^2 * solve(strat_sigma + phi^2*eeT)
      Q[j,] = (1 - sum(W[,j])) * rowSums(W) / sum(W) + W[,j]
    }
  }
  
  RATE_est = Q %*% strat_est
  RATE_sigma = Q %*% strat_sigma %*% t(Q)
  RATE_SE = sqrt(diag(RATE_sigma))
  
  output <- list('Q' = Q,
                 'estimates' = data.frame(
                   group = grps,
                   est = RATE_est,
                   SE = RATE_SE, 
                   ci_lower = RATE_est - 1.96*RATE_SE,
                   ci_upper = RATE_est + 1.96*RATE_SE)
                 )
  return(output)
}

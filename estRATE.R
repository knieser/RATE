estRATE <- function(estimates, sigma, decision_rule, phi) {

  grps = 1:length(estimates)
  ngrps = length(grps)
  Q = matrix(data = NA, ncol = ngrps, nrow = ngrps)
  
  if (decision_rule == 1){
    # Bayes avg risk
    for (j in 1:ngrps){
      W = (phi^2) * solve(2*sigma + phi^2*diag(ngrps))
      Q[j,] = (1 - sum(W[,j])) * rowSums(W) / sum(W) + W[,j]
    }
  } else if (decision_rule == 2){
    # minimax risk
    for (j in 1:ngrps){
      eeT = diag(ngrps)[j,] %*% t(diag(ngrps)[j,])
      W = phi^2 * solve(sigma + phi^2*eeT)
      Q[j,] = (1 - sum(W[,j])) * rowSums(W) / sum(W) + W[,j]
    }
  }
  
  RATE_est = Q %*% estimates
  RATE_sigma = Q %*% sigma %*% t(Q)
  RATE_SE = sqrt(diag(RATE_sigma))
  
  output <- list('q' = diag(Q),
                 'fullQ' = Q,
                 'estimates' = data.frame(
                   group = grps,
                   est = RATE_est,
                   SE = RATE_SE, 
                   ci_lower = RATE_est - 1.96*RATE_SE,
                   ci_upper = RATE_est + 1.96*RATE_SE,
                   ci_width = 2*1.96*RATE_SE)
                 )
  return(output)
}

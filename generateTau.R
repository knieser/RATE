generateTau <- function(tau_dist, ngrps, tau_draws){
  
  ntau = ngrps*tau_draws
  
  if (tau_dist == "normal"){
    x = rnorm(ntau, 0, 1)
  } else if (tau_dist == "mixture"){
    id = runif(ntau)
    x = sapply(id, 
        function(x) ifelse(x < .8, rnorm(1,0.5,1), rnorm(1,-3,.5)))
  } else if (tau_dist == "gamma"){
    x = rgamma(ntau,3,3)
  } else if (tau_dist == 'uniform'){
    x = runif(ntau, -2, 2)
  }

  if (tau_dist == "none"){
    x = rep(0, ntau) 
  } else {
    # rescale so that true phi = 1
    x = x / sd(x) / sqrt(2)
  }
  
  # reshape tau
  tau = matrix(x, nrow = tau_draws)
  
  return(tau)
}
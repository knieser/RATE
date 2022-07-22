generate_tau <- function(tau_dist, groups, tau_draws){
  
  x = matrix(rnorm(groups*tau_draws,0,1), nrow = tau_draws)
  
  if (tau_dist == "Normal"){
    tau = x
  }
  
  if (tau_dist == "Heavy-tailed"){
    tau = sinh(1.3*asinh(x))
  }
  
  if (tau_dist == "Skewed"){
    tau = sinh(asinh(x) - .3)
  }
  
  return(tau)
  
}
# : https://www.jstor.org/stable/27798865
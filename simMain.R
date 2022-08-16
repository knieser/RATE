simMain <- function(N, p, tau_dist, tau_draws,
                    decision_rule, phi, loops){
  
  ngrps = length(p)
  est = vector(mode = 'list', length = tau_draws)

  # draw tau
  tau = generateTau(tau_dist, ngrps, tau_draws) 
  
  # run many simulations for each tau draw 
  for (j in 1:tau_draws){
    message("Running simulation ", j, " out of ", tau_draws)
    tic()
    est[[j]] <- runSimulations(N, p, tau[j,], decision_rule, phi, loops)
    toc()
    }
 
 # compile results for each tau draw     
 results <- compileMSE(est)

 return(results)
  
}
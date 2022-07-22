simMain <- function(sample_size, p, a1, tau_dist, tau_draws,
                    decision_rule, delta, loops){
  
  groups = nrow(p)
  
  # calculate # different simulations
  num_sims = length(sample_size) * ncol(p) * ncol(a1) * 
             length(tau_dist) * tau_draws 
  
  # initialize
  tau    = array(data = NA, dim = c(length(tau_dist), tau_draws, groups))
  est    = vector(mode = 'list', length = num_sims)
  lookup = data.frame(est_id = 1:num_sims)

  # draw tau
  for (d in 1:length(tau_dist)){
      tau[d,,] = generate_tau(tau_dist[d], groups, tau_draws) 
  }
  
  # run simulations
  i = 1
  for (s in 1:length(sample_size)){
    for (r in 1:ncol(p)){
      for (a in 1:ncol(a1)){
        for (d in 1:length(tau_dist)){
          for (j in 1:tau_draws){
            lookup[i,"groups"] = groups
            lookup[i,"N"] = sample_size[s]
            lookup[i,"p"] = paste(round(p[,r],3), collapse = ', ')
            lookup[i,"a1"] = paste(a1[,a], collapse = ', ')
            lookup[i,"tau_dist"] = tau_dist[d]
            lookup[i,"draw"] = j
            lookup[i,"tau"] = paste(round(tau[d,j,],3), collapse = ', ')
            message("Running simulation ", i, " out of ", num_sims)
            tic()
            est[[i]] <- runSimulations(sample_size[s], p[,r], a1[,a], tau[d,j,], 
                                 decision_rule, delta, loops)
            toc()
            i <- i + 1
          }
        }
      }
    }
  }
 
 results <- compileMSE(est, lookup)
 
 output = list("estimates" = results$est,
               "lookup" = results$lookup)
 
 return(output)
  
}
compileMSE <- function(sim_output, lookup){

  num_grps = lookup$groups
 
  for (j in 1:nrow(lookup)){
    est       = sim_output[[j]]
    tau       = as.numeric(unlist(strsplit(lookup[j,"tau"], ", ")))
    SATE_MSE  = computeMSE(est$SATE, tau)
    strat_MSE = computeMSE(est$strat, tau)
    re_MSE    = computeMSE(est$re, tau)
    RATE1_MSE = computeMSE(est$RATE_1, tau)
    RATE2_MSE = computeMSE(est$RATE_2, tau)
    RATE3_MSE = computeMSE(est$RATE_3, tau)
    
    sim_output[[j]]$strat_MSE = strat_MSE
    sim_output[[j]]$re_MSE = re_MSE
    sim_output[[j]]$RATE1_MSE = RATE1_MSE
    sim_output[[j]]$RATE2_MSE = RATE2_MSE
    sim_output[[j]]$RATE3_MSE = RATE3_MSE
 

    lookup[j,"Overall MSE"] = paste(round(SATE_MSE,3), collapse = ', ')
    lookup[j,"Strat MSE"]   = paste(round(strat_MSE,3), collapse = ', ')
    lookup[j,"RE MSE"]      = paste(round(re_MSE,3), collapse = ', ')
    lookup[j,"RATE_1 MSE"]  = paste(round(RATE1_MSE,3), collapse = ', ')
    lookup[j,"RATE_2 MSE"]  = paste(round(RATE2_MSE,3), collapse = ', ')
    lookup[j,"RATE_3 MSE"]  = paste(round(RATE3_MSE,3), collapse = ', ')
  }
  
  results = list(est = sim_output, lookup = lookup)
  return(results)
}

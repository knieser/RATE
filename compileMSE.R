compileMSE <- function(est){

  for (j in 1:length(est)){
    sim       = est[[j]]
    tau       = sim$parameters$tau
    SATE_MSE  = computeMSE(sim$SATE, tau)
    strat_MSE = computeMSE(sim$strat, tau)
    reg_MSE   = computeMSE(sim$reg, tau)
    re_MSE    = computeMSE(sim$re, tau)
    RATE1_MSE = computeMSE(sim$RATE1, tau)
    RATE2_MSE = computeMSE(sim$RATE2, tau)
    RATE3_MSE = computeMSE(sim$RATE3, tau)
    
    # store MSEs with estimates
    est[[j]]$strat_MSE = strat_MSE
    est[[j]]$reg_MSE   = reg_MSE
    est[[j]]$re_MSE    = re_MSE
    est[[j]]$RATE1_MSE = RATE1_MSE
    est[[j]]$RATE2_MSE = RATE2_MSE
    est[[j]]$RATE3_MSE = RATE3_MSE
  }
  
  return(est)
}

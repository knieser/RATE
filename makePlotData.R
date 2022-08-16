makePlotData <- function(estimates){
  
  nsims = length(estimates)
  ngrps = length(estimates[[1]]$parameters$p)
  phi   = estimates[[1]]$parameters$phi
  estimators = c("Random effects",
                 "Interaction",
                 paste0("RATE (",phi,")")
  )
  
  # initialize
  diff_RATE1 = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  diff_RATE2 = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  diff_RATE3 = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  diff_reg   = matrix(data=NA, nrow = nsims, ncol = ngrps)
  diff_RE    = matrix(data=NA, nrow = nsims, ncol = ngrps) 
    
  # calculate differences in MSE       
  for (j in 1:nsims){
    est            = estimates[[j]]
    diff_RATE1[j,] = est$RATE1_MSE - est$strat_MSE
    diff_RATE2[j,] = est$RATE2_MSE - est$strat_MSE
    diff_RATE3[j,] = est$RATE3_MSE - est$strat_MSE
    diff_reg[j,]   = est$reg_MSE - est$strat_MSE
    diff_RE[j,]    = est$re_MSE - est$strat_MSE
  }
  
  # reshape diff matrices to long format
  diff_RATE1_long = matrix(diff_RATE1, ncol=1)
  diff_RATE2_long = matrix(diff_RATE2, ncol=1)
  diff_RATE3_long = matrix(diff_RATE3, ncol=1)
  diff_reg_long   = matrix(diff_reg, ncol=1)
  diff_RE_long    = matrix(diff_RE, ncol=1)
  
  # stack long diff matrices
  df = data.frame(
    'group'     = rep(rep(paste("Group",1:ngrps), each = nsims), length(estimators)),
    'estimator' = as.factor(rep(estimators, each = ngrps*nsims)),
    'd'         = c(diff_RE_long, diff_reg_long,
                    diff_RATE1_long, diff_RATE2_long, 
                    diff_RATE3_long)
  )
  
  # reorder estimators for plot
  df$estimator <- factor(df$estimator, 
                         c("Random effects","Interaction",
                           paste0("RATE (",phi,")"))
                         )

  return(df)
}
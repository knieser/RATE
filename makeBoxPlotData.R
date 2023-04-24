makeBoxPlotData <- function(estimates){
  
  nsims = length(estimates)
  ngrps = length(estimates[[1]]$parameters$p)
  phi   = estimates[[1]]$parameters$phi
  estimators = c("SATE", "Stratification", 
                 "Interaction",
                 "Random effects",
                 "RATE-shared",
                 paste0("RATE-opt (", phi[3], ")"),
                 paste0("RATE-opt (", phi[2], ")"),
                 paste0("RATE-opt (", phi[1], ")")
                 )
                 
  
  # initialize
  RATE1      = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  RATE2      = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  RATE3      = matrix(data=NA, nrow = nsims, ncol = ngrps) 
  RATEshared = matrix(data=NA, nrow = nsims, ncol = ngrps) 
  reg        = matrix(data=NA, nrow = nsims, ncol = ngrps)
  SATE       = matrix(data=NA, nrow = nsims, ncol = ngrps) 
  RE         = matrix(data=NA, nrow = nsims, ncol = ngrps) 
  strat      = matrix(data=NA, nrow = nsims, ncol = ngrps) 
  
  # calculate differences in MSE       
  for (j in 1:nsims){
    est            = estimates[[j]]
    RATE1[j,]      = est$RATE1_MSE
    RATE2[j,]      = est$RATE2_MSE
    RATE3[j,]      = est$RATE3_MSE
    RATEshared[j,] = est$RATEequal_MSE
    reg[j,]        = est$reg_MSE 
    RE[j,]         = est$re_MSE 
    strat[j,]      = est$strat_MSE
    SATE[j,]       = est$SATE_MSE 
  }

  # reshape matrices to long format
  RATE1_long      = matrix(RATE1, ncol=1)
  RATE2_long      = matrix(RATE2, ncol=1)
  RATE3_long      = matrix(RATE3, ncol=1)
  RATEshared_long = matrix(RATEshared, ncol=1)
  reg_long        = matrix(reg, ncol=1)
  RE_long         = matrix(RE, ncol=1)
  SATE_long       = matrix(SATE, ncol=1)
  strat_long      = matrix(strat, ncol=1)
  
  
  # stack long matrices
  df = data.frame(
    'group'     = rep(rep(paste("Group",1:ngrps), each = nsims), length(estimators)),
    'estimator' = as.factor(rep(estimators, each = ngrps*nsims)),
    'd'         = c(SATE_long, strat_long, 
                    reg_long, RE_long, 
                    RATEshared_long, 
                    RATE3_long, RATE1_long, RATE2_long)
  )
  
  # reorder estimators for plot
  df$estimator <- factor(df$estimator, 
                         c("SATE", "Stratification", 
                           "Interaction",
                           "Random effects",
                           "RATE-shared",
                           paste0("RATE-opt (", phi[3], ")"),
                           paste0("RATE-opt (", phi[2], ")"),
                           paste0("RATE-opt (", phi[1], ")")
                         )
  )

  return(df)
}
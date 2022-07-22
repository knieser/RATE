runSimulations = function(N, p, a1, tau, decision_rule, delta, loops){
  
  grps = 1:length(p)
  
  # model to use for stratified estimates
  Ymodel = y~trt+z1+z2+z3
  
  # initialize
  overall_est      = array(data=NA, dim=c(loops, length(grps)))
  strat_est        = array(data=NA, dim=c(loops, length(grps)))
  strat_SE         = array(data=NA, dim=c(loops, length(grps)))
  re_est           = array(data=NA, dim=c(loops, length(grps)))
  rate_est_1       = array(data=NA, dim=c(loops, length(grps)))
  rate_est_2       = array(data=NA, dim=c(loops, length(grps)))
  rate_est_3       = array(data=NA, dim=c(loops, length(grps)))
  rate_est_1_se    = array(data=NA, dim=c(loops, length(grps)))
  rate_est_2_se    = array(data=NA, dim=c(loops, length(grps)))
  rate_est_3_se    = array(data=NA, dim=c(loops, length(grps)))

  for(l in 1:loops){
   
    # simulate data
    smp = simulateData(N,p,a1,tau)
    
    # overall estimates
    overall_est[l,] = rep(summary(lm(Ymodel,data=smp))$coef["trt",1],length(grps))

    # stratified estimates 
    strat_output <- estStratified(smp,"trt","g",Ymodel)
    strat_est[l,] = strat_output$est
    strat_SE[l,]  = strat_output$SE

    # random effects estimates
    re_model <- lmer(y ~ trt + z1 + z2 + z3 + (trt|g), data=smp)
    re_est[l,] = coef(re_model)$g[,2]
    
    # RATE estimates
    rate_1_output <- estRATE(strat_est[l,], strat_SE[l,], decision_rule, delta[1])
    rate_est_1[l,] = rate_1_output$estimates$est
    rate_est_1_se[l,] = rate_1_output$estimates$SE
    
    rate_2_output <- estRATE(strat_est[l,], strat_SE[l,], decision_rule, delta[2])
    rate_est_2[l,] = rate_2_output$estimates$est
    rate_est_2_se[l,] = rate_2_output$estimates$SE
    
    rate_3_output <- estRATE(strat_est[l,], strat_SE[l,], decision_rule, delta[3])
    rate_est_3[l,] = rate_3_output$estimates$est
    rate_est_3_se[l,] = rate_3_output$estimates$SE
  }
  
  # compile results
  output = list(
    "SATE" = overall_est,
    "strat" = strat_est,
    "strat_SE" = strat_SE,
    "re" = re_est,
    "RATE_1" = rate_est_1,
    "RATE_1_SE" = rate_est_1_se,
    "RATE_2" = rate_est_2,
    "RATE_2_SE" = rate_est_2_se,
    "RATE_3" = rate_est_3,
    "RATE_3_SE" = rate_est_3_se
  )
  
  return(output)
}
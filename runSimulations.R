runSimulations = function(N, p, a1, tau, decision_rule, phi, loops){
  
  grps = 1:length(p)
  
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
    smp = simulateData(N, p, a1, tau)
    
    # overall estimates
    overall_est[l,] = rep(summary(lm(y ~ trt + g + x1 + x2,
                        data=smp))$coef["trt",1],length(grps))

    # stratified estimates 
    strat_output <- estStratified(smp,"trt","g",y ~ trt + x1 + x2)
    strat_est[l,] = strat_output$est
    strat_SE[l,]  = strat_output$SE

    # random effects estimates
    re_model <- lmer(y ~ trt + g + x1 + x2 + (trt|g), data=smp,
        control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
    re_est[l,] = coef(re_model)$g[,2]
    
    # RATE estimates
    strat_sigma = diag(strat_SE[l,]^2) 

    rate_1_output <- estRATE(strat_est[l,], strat_sigma, decision_rule, phi[1])
    rate_est_1[l,] = rate_1_output$estimates$est
    rate_est_1_se[l,] = rate_1_output$estimates$SE
    
    rate_2_output <- estRATE(strat_est[l,], strat_sigma, decision_rule, phi[2])
    rate_est_2[l,] = rate_2_output$estimates$est
    rate_est_2_se[l,] = rate_2_output$estimates$SE
    
    rate_3_output <- estRATE(strat_est[l,], strat_sigma, decision_rule, phi[3])
    rate_est_3[l,] = rate_3_output$estimates$est
    rate_est_3_se[l,] = rate_3_output$estimates$SE
  }
  
  # compile results
  parameters = list(
    N = N,
    p = p,
    a1 = a1,
    tau = tau,
    phi = phi
  )
  
  output = list(
    parameters = parameters,
    SATE = overall_est,
    strat = strat_est,
    strat_SE = strat_SE,
    re = re_est,
    RATE_1 = rate_est_1,
    RATE_1_SE = rate_est_1_se,
    RATE_2 = rate_est_2,
    RATE_2_SE = rate_est_2_se,
    RATE_3 = rate_est_3,
    RATE_3_SE = rate_est_3_se
  )
  
  return(output)
}
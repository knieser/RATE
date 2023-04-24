runSimulations = function(N, p, tau, noise, decision_rule, phi, loops){
  
  grps = 1:length(p)
  ngrps = length(grps)
  
  # models to fit
  Ymodel_overall = y ~ trt + g + x1 + x2
  Ymodel_strat   = y ~ trt + x1 + x2
  Ymodel_reg     = y ~ trt + g + x1 + x2 + g*trt
  Ymodel_re      = y ~ trt + g + x1 + x2 + (trt|g)
  
  # initialize
  overall_est   = array(data=NA, dim=c(loops, ngrps))
  strat_est     = array(data=NA, dim=c(loops, ngrps))
  strat_SE      = array(data=NA, dim=c(loops, ngrps))
  reg_est       = array(data=NA, dim=c(loops, ngrps))
  reg_SE        = array(data=NA, dim=c(loops, ngrps))
  re_est        = array(data=NA, dim=c(loops, ngrps))
  rate1_q       = array(data=NA, dim=c(loops, ngrps))
  rate2_q       = array(data=NA, dim=c(loops, ngrps))
  rate3_q       = array(data=NA, dim=c(loops, ngrps))
  rate1_est     = array(data=NA, dim=c(loops, ngrps))
  rate2_est     = array(data=NA, dim=c(loops, ngrps))
  rate3_est     = array(data=NA, dim=c(loops, ngrps))
  rate1_se      = array(data=NA, dim=c(loops, ngrps))
  rate2_se      = array(data=NA, dim=c(loops, ngrps))
  rate3_se      = array(data=NA, dim=c(loops, ngrps))
  rateequal_q   = array(data=NA, dim=c(loops, ngrps))
  rateequal_est = array(data=NA, dim=c(loops, ngrps))
  rateequal_se  = array(data=NA, dim=c(loops, ngrps))
  
  for(l in 1:loops){

    # simulate data
    df <- simulateData(N, p, tau, noise)
    
    # overall estimates
    m0 <- lm(Ymodel_overall, data = df)
    overall_est[l,] = rep(summary(m0)$coef["trt",1],length(grps))

    # stratified estimates 
    strat_output <- estStratified(df,"trt","g", Ymodel_strat)
    strat_est[l,] = strat_output$est
    strat_SE[l,]  = strat_output$SE
    
    # regression estimates
    m1 <- lm(Ymodel_reg, data = df)
    trt_coefs = grep("trt:g*",names(coef(m1)),value=1)
    reg_output <- glht(m1, linfct = c("trt=0", 
                                      paste0("trt+",trt_coefs, "=0")))
    reg_est[l,] = as.vector(coef(reg_output))
    reg_vcov    = vcov(reg_output)
    reg_SE[l,]  = sqrt(diag(reg_vcov))

    # random effects estimates
    re_model <- lmer(Ymodel_re, data=df,
        control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
    re_est[l,] = coef(re_model)$g[,2]
    
    # RATE estimates
    rate1_output  = estRATE(reg_est[l,], reg_vcov, decision_rule, phi[1])
    rate1_q[l,]   = rate1_output$q
    rate1_est[l,] = rate1_output$estimates$est
    rate1_se[l,]  = rate1_output$estimates$SE

    rate2_output  = estRATE(reg_est[l,], reg_vcov, decision_rule, phi[2])
    rate2_q[l,]   = rate2_output$q
    rate2_est[l,] = rate2_output$estimates$est
    rate2_se[l,]  = rate2_output$estimates$SE

    rate3_output  = estRATE(reg_est[l,], reg_vcov, decision_rule, phi[3])
    rate3_q[l,]   = rate3_output$q
    rate3_est[l,] = rate3_output$estimates$est
    rate3_se[l,]  = rate3_output$estimates$SE
    
    # RATE with equal weights
    rateequal_output  = estRATEequal(reg_est[l,], reg_vcov, phi[1])
    rateequal_q[l,]   = rateequal_output$q
    rateequal_est[l,] = rateequal_output$estimates$est
    rateequal_se[l,]  = rateequal_output$estimates$SE
  }
  
  # compile results
  parameters = list(
    N = N,
    p = p,
    tau = tau,
    phi = phi
  )
  
  output = list(
    parameters   = parameters,
    SATE         = overall_est,
    strat        = strat_est,
    strat_SE     = strat_SE,
    reg          = reg_est,
    reg_SE       = reg_SE,
    re           = re_est,
    RATE1_q      = rate1_q,
    RATE1        = rate1_est,
    RATE1_SE     = rate1_se,
    RATE2_q      = rate2_q,
    RATE2        = rate2_est,
    RATE2_SE     = rate2_se,
    RATE3_q      = rate3_q,
    RATE3        = rate3_est,
    RATE3_SE     = rate3_se,
    RATEequal_q  = rateequal_q,
    RATEequal    = rateequal_est,
    RATEequal_SE = rateequal_se
  )
  
  return(output)
}
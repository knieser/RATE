# stratfied estimates
estStratified = function(df, trt, g, Ymodel){
  
  # get list of groups
  grps = sort(unique(df$g))

  # initialize
  strat_est = 1:length(grps)
  strat_var = 1:length(grps)
    
  # get stratified estimates for each group
  for (j in 1:length(grps)){
    # subset df
    sub_df = df[df$g==grps[j],]
    # estimates
    strat_est[j] = summary(lm(Ymodel,data=sub_df))$coef[trt,1]
    # estimated variance
    strat_var[j] = (summary(lm(Ymodel,data=sub_df))$coef[trt,2])^2
    
    # g-computation estimates
    # strat_est[j] = estOM(df, Ymodel)$ATE
  }
  
  estimates = data.frame(
    group = grps,
    estimate = strat_est,
    std_err = sqrt(strat_var)
  )
  
  output = list(estimates=estimates)
  
  return(output)
  
}
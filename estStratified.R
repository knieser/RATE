# simple stratified regression
# would like to add g-formula and IPW estimators and their SEs.

estStratified = function(df, trt, g, Ymodel){
  
  # get list of groups
  grps = sort(unique(df$g))

  # initialize
  est = 1:length(grps)
  SE = 1:length(grps)
    
  # get stratified estimates for each group
  for (j in 1:length(grps)){
    sub_df = df[df$g==grps[j],]
    fit = lm(Ymodel,data=sub_df)
    est[j] = summary(fit)$coef[trt,1]
    SE[j]  = summary(fit)$coef[trt,2]
  }
  
  output = data.frame(
    group = grps,
    est = est,
    SE = SE,
    ci_lower = est - 1.96*SE,
    ci_upper = est + 1.96*SE)
  return(output)
}
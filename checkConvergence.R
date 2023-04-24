checkConvergence <- function(df){

  df$group_est = paste(df$group, df$estimator)
  sims = table(df$group_est)
  running_25 = data.frame(iter = 1:max(sims))
  running_50 = data.frame(iter = 1:max(sims))
  running_75 = data.frame(iter = 1:max(sims))
  
  for (s in 1:length(sims)){
    group_est = names(sims)[s]
    df_sub = df$d[df$group_est == group_est]
    for (i in 1:length(df_sub)){
      running_25[i,s] = quantile(df_sub[1:i],.25)
      running_50[i,s] = quantile(df_sub[1:i],.5)
      running_75[i,s] = quantile(df_sub[1:i],.75)
    }
  }
  
  par(mfrow = c(3,1))
  matplot(running_25, type = 'l', lwd = 2)
  matplot(running_50, type = 'l', lwd = 2)
  matplot(running_75, type = 'l', lwd = 2)
  
}


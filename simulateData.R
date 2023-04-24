simulateData = function(N, p, tau, noise){
  
  grps = 1:length(p)
  grp_size = N*p
  
  # generate group and baseline covariates
  df <- data.frame(
    g = as.factor(rep(grps, grp_size)),
    trt = rep(0,N),
    x1 = rnorm(N,1,1),
    x2 = rbinom(N,1,.3)
  )

  # random allocation of treatment by group
  for (g in grps){
    subdf = df[df$g==g,]
    half = round(nrow(subdf)/2,0)
    subdf$trt[sample(nrow(subdf),half)] <- 1
    df$trt[df$g==g] <- subdf$trt
  }
  
  # simulate outcomes 
  df$beta = tau[df$g]
  noise_sd = noise[df$g]
  
  df$y = 1 + df$beta*df$trt + 2*as.numeric(df$g %in% c(1,3)) +
   1*df$x1 + 2.5*df$x2 + rnorm(N,0,noise_sd)
  
  return(df)
}
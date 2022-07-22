simulateData = function(N,p,a1,tau){
  
  grps = 1:length(p)
  
  # generate group and baseline covariates
  df <- data.frame(
    g = sample(grps,N,replace=T,prob=p),
    z1 = rnorm(N,0,1),
    z2 = rnorm(N,0,1),
    z3 = rnorm(N,0,1)
  )
  
  # assign treatment by group
  df$trt_p = a1[df$g]
  df$trt = rbinom(N,1,df$trt_p)
  
  # simulate outcomes 
  df$beta = tau[df$g]
  df$y = 1 + df$beta*df$trt + df$g + df$z1 + 2*df$z2 - df$z3 + rnorm(N,0,1)
  
  return(df)
}
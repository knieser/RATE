computeMSE <- function(est, tau, n = nrow(est)){
  
  est <- est[1:n, ]
  
  num_grps = ncol(est)
  bias = abs(apply(est,2,mean) - tau)
  se = apply(est,2,sd)
  mse = bias^2 + se^2
  formatted_mse = t(matrix(mse, nrow=num_grps))
  
  return(formatted_mse)
}
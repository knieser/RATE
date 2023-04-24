estRATEequal <- function(estimates, sigma, phi) {
  
  grps = 1:length(estimates)
  ngrps = length(grps)
  Q = matrix(data = NA, ncol = ngrps, nrow = ngrps)
  
  avgSE = mean(diag(sigma))
  
  V1 = 1:length(grps)
  V2 = 1:length(grps)
  for (j in 1:ngrps){
    V1[j] = sum(sigma[j,-j])
    V2[j] = sum(sigma) - 2*sum(sigma[j,]) + sigma[j,j]
  }
  avgV1 = mean(V1)
  avgV2 = mean(V2)
  
  leading_coef = (ngrps-1)^2 * avgSE - 2*(ngrps-1)*avgV1 + avgV2 + phi^2*ngrps*(ngrps-1)/2
  if (leading_coef <= 0){
    stop ("regularity condition not satisfied for obtaining a minimum")
  }
  
  gamma_num = avgSE * (ngrps - 1) - avgV1
  gamma_denom = phi^2 * ngrps / 2 + avgV2 / (ngrps - 1) - avgV1
  gamma = gamma_num / gamma_denom
  
  if (gamma < 0) {stop ("weights greater than 1")}
  w <- 1 / (1 + gamma)
  
  for (j in 1:ngrps){
    Q[j,j] = w
    Q[j,-j] = (1-w) / (ngrps - 1)
  }
  
  RATEequal_est = Q %*% estimates
  RATEequal_sigma = Q %*% sigma %*% t(Q)
  RATEequal_SE = sqrt(diag(RATEequal_sigma))
  
  output <- list('q' = diag(Q),
                 'fullQ' = Q,
                 'estimates' = data.frame(
                   group = grps,
                   est = RATEequal_est,
                   SE = RATEequal_SE, 
                   ci_lower = RATEequal_est - 1.96*RATEequal_SE,
                   ci_upper = RATEequal_est + 1.96*RATEequal_SE,
                   ci_width = 2*1.96*RATEequal_SE)
  )
  return(output)
}




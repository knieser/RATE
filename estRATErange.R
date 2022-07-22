estRATErange <- function(strat_est, strat_SE, decision_rule, delta){
  
  grps = 1:length(strat_est)
  est = matrix(data = NA, nrow = length(delta), ncol = length(grps))
  ci_lower = matrix(data = NA, nrow = length(delta), ncol = length(grps))
  ci_upper = matrix(data = NA, nrow = length(delta), ncol = length(grps))
  
  for (d in 1:length(delta)){
    estimates   <- estRATE(strat_est, strat_SE, 1, delta[d])$estimates
    est[d,]      = estimates[,"est"]
    ci_lower[d,] = estimates[,"ci_lower"]
    ci_upper[d,] = estimates[,"ci_upper"]
  }
  
  plot_data <- data.frame(
    group = as.factor(rep(grps, each = length(delta))),
    delta = rep(sqrt(2)*delta, length(grps)),
    est = matrix(est, ncol = 1),
    ci_lower = matrix(ci_lower, ncol = 1),
    ci_upper = matrix(ci_upper, ncol = 1)
  )
 
  fig <- ggplot(data = plot_data, aes(delta, est, group = group)) + 
    geom_line(aes(color = group), size = 2) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = group), 
                alpha = 0.3) + 
    geom_hline(yintercept = 0, lty = 'dashed', lwd = 1.1, color = 'black') + 
    theme_classic() +
    xlab('Prior belief in HTE (SD of Difference in ATEs)') +
    ylab('Treatment effect') +
    theme(plot.title=element_text(size=18,face="bold"),
          axis.text.x=element_text(size=18),
          axis.ticks.length=unit(.25,"cm"),
          axis.text.y=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          legend.position = "right",
          legend.title = element_text(size=18,face='bold'),
          legend.text=element_text(size=18)
    )
  return(fig)
}
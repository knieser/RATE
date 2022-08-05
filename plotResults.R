plotResults <- function(results){
  
  nsims = length(results)
  ngrps = length(results[[1]]$parameters$p)
  phi = results[[1]]$parameters$phi
  estimators = c("Random effects",
                 paste0(expression(phi), " = ",phi,""))
  
  # initialize
  diff_RE    = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  diff_RATE1 = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  diff_RATE2 = matrix(data=NA, nrow = nsims, ncol = ngrps)  
  diff_RATE3 = matrix(data=NA, nrow = nsims, ncol = ngrps)  
    
  # calculate differences in MSE       
  for (j in 1:nsims){
    est            = results[[j]]
    diff_RE[j,]    = est$re_MSE - est$strat_MSE
    diff_RATE1[j,] = est$RATE1_MSE - est$strat_MSE
    diff_RATE2[j,] = est$RATE2_MSE - est$strat_MSE
    diff_RATE3[j,] = est$RATE3_MSE - est$strat_MSE
  }
  
  # reshape diff matrices to long format
  diff_RE_long    = matrix(diff_RE, ncol=1)
  diff_RATE1_long = matrix(diff_RATE1, ncol=1)
  diff_RATE2_long = matrix(diff_RATE2, ncol=1)
  diff_RATE3_long = matrix(diff_RATE3, ncol=1)
  
  # stack long diff matrices
  plot_df = data.frame(
    'group'     = rep(rep(paste("Group",1:ngrps), each = nsims), length(estimators)),
    'estimator' = rep(estimators, each = ngrps*nsims),
    'd'         = c(diff_RE_long, diff_RATE1_long, 
                    diff_RATE2_long, diff_RATE3_long)
  )

  fig <- ggplot(data = plot_df, aes(x = estimator, y = d)) + 
    #geom_violin(aes(fill=estimator),
    #            trim=FALSE,
    #            draw_quantiles = c(.25,.5,.75),
    #            scale="count") +
    geom_boxplot(aes(fill=estimator)) +
    scale_fill_viridis(discrete=T, alpha=.6) + 
    facet_grid(. ~ group) +
    geom_hline(yintercept=0, linetype=2, lwd=1, col=1,alpha=.3)+
    ylab('Difference in MSE relative to stratification') + 
    xlab('') +
    theme(plot.title=element_text(size=16,face="bold"),
          axis.text.y=element_text(size=16),
          axis.ticks.length=unit(.25,"cm"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title=element_text(size=18,face="bold"),
          strip.text=element_text(size=18,face="bold",angle=0),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=18)
    )
  
  return(fig)
}
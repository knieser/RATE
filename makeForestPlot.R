# make forest plot

makeForestPlot = function(estimates) {

  marg_tau = estimates$marg_tau
  strat = estimates$strat 
  re = estimates$re 
  minimax1 = estimates$minimax[1,,]
  minimax2 = estimates$minimax[2,,]
  minimax3 = estimates$minimax[3,,]
  bayes11 = estimates$bayes1[1,,]
  bayes12 = estimates$bayes1[2,,]
  bayes13 = estimates$bayes1[3,,]
  grps = 1:ncol(strat)
  grp_names = paste("Group", grps)
  
  # organize plot data
  plot_data <- data.frame(
    estimator = rep(c("Stratified","Random Effects",
                      paste0("Minimax d=",estimates$parameters$delta[1],''),
                      paste0("Minimax d=",estimates$parameters$delta[2],''),
                      paste0("Minimax d=",estimates$parameters$delta[3],''),
                      paste0("Bayes v=",estimates$parameters$var_tau[1],''),
                      paste0("Bayes v=",estimates$parameters$var_tau[2],''),
                      paste0("Bayes v=",estimates$parameters$var_tau[3],'')),
                      each = length(grps)),
    group = rep(grp_names,8),
    mean = c(apply(strat,2,mean),
             apply(re,2,mean),
             apply(minimax1,2,mean),
             apply(minimax2,2,mean),
             apply(minimax3,2,mean),
             apply(bayes11,2,mean),
             apply(bayes12,2,mean),
             apply(bayes13,2,mean)),
    lower = c(apply(strat,2,mean) - 2*apply(strat,2,sd),
              apply(re,2,mean) - 2*apply(re,2,sd),
              apply(minimax1,2,mean) - 2*apply(minimax1,2,sd),
              apply(minimax2,2,mean) - 2*apply(minimax2,2,sd),
              apply(minimax3,2,mean) - 2*apply(minimax3,2,sd),
              apply(bayes11,2,mean) - 2*apply(bayes11,2,sd),
              apply(bayes12,2,mean) - 2*apply(bayes12,2,sd),
              apply(bayes13,2,mean) - 2*apply(bayes13,2,sd)),
    upper = c(apply(strat,2,mean) + 2*apply(strat,2,sd),
              apply(re,2,mean) + 2*apply(re,2,sd),
              apply(minimax1,2,mean) + 2*apply(minimax1,2,sd),
              apply(minimax2,2,mean) + 2*apply(minimax2,2,sd),
              apply(minimax3,2,mean) + 2*apply(minimax3,2,sd),
              apply(bayes11,2,mean) + 2*apply(bayes11,2,sd),
              apply(bayes12,2,mean) + 2*apply(bayes12,2,sd),
              apply(bayes13,2,mean) + 2*apply(bayes13,2,sd))
  )
  
  # make plot
  fig <- ggplot(data=plot_data,
         aes(x = estimator,y = mean, ymin = lower, ymax = upper))+
    geom_pointrange(aes(col=estimator),size=1.5)+
    scale_color_manual(values = c("#5DCAF1","#806CE2", "#462AD4",
                                  "#FF2525","#C80000","#8A0000",
                                  "#9F9B9E","black")) +
    xlab('Group')+ 
    ylab("Average Treatment Effect Estimates (+/- 2 SEs)")+
    geom_errorbar(aes(ymin=lower,ymax=upper,col=estimator),width=0.5,cex=1)+ 
    facet_wrap(~group,strip.position="left",nrow=length(grps),scales = "free_y") +
    geom_hline(yintercept=0,linetype=2,lwd=1,alpha=0.2)+
    geom_hline(yintercept=marg_tau,linetype=2,color='blue',lwd=1,alpha=.5)+
    theme(plot.title=element_text(size=16,face="bold"),
          axis.text.x=element_text(size=16),
          axis.ticks.length=unit(.25,"cm"),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title=element_text(size=18,face="bold"),
          strip.text.y=element_text(size=18,hjust=0.5,vjust=1,angle=180,face="bold"),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=18)
          )+
    coord_flip()
  
    print(fig)

  return(fig)
 
}
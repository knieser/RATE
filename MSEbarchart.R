MSEbarchart <- function(sim_output){

  estimates = sim_output$estimates
  results = sim_output$results
  
  for (j in 1:length(estimates)){
    est = estimates[[j]]
    strat = est$strat 
    re = est$re 
    rate1 = est$RATE_1
    rate2 = est$RATE_2
    rate3 = est$RATE_3
    tau = as.numeric(unlist(strsplit(results[j,"tau"], ", ")))
    grps = 1:length(p)
    grp_names = grps
    num_estimators = 5
    
    plot_data <- data.frame(
      estimator = rep(c("Stratified","Random effects",
                        paste0("zRATE 1"),
                        paste0("zRATE 2"),
                        paste0("zRATE 3")),
                      each = length(grps)),
      group = rep(grp_names,num_estimators),
      bias = c(
        abs(apply(strat,2,mean) - tau),
        abs(apply(re,2,mean) - tau),
        abs(apply(rate1,2,mean) - tau),
        abs(apply(rate2,2,mean) - tau),
        abs(apply(rate3,2,mean) - tau)
      ),
      se = c(
        apply(strat,2,sd),
        apply(re,2,sd),
        apply(rate1,2,sd),
        apply(rate2,2,sd),
        apply(rate3,2,sd)
      )
    )
    
    plot_data$bias_sqrd = (plot_data$bias)^2
    plot_data$var = (plot_data$se)^2
    plot_data$MSE = plot_data$bias_sqrd + plot_data$var
    plot_title = paste('Simulation',j)
    
    fig <- ggplot(data=plot_data, aes(x=group, y=MSE, fill=estimator)) +
      geom_bar(stat="identity", position=position_dodge(), colour="black") +
      theme_classic() +
      xlab('Group') +
      ylab('MSE') +
      ggtitle(plot_title) +
      theme(plot.title=element_text(size=18,face="bold"),
            axis.text.x=element_text(size=18),
            axis.ticks.length=unit(.25,"cm"),
            axis.text.y=element_text(size=18),
            axis.title=element_text(size=20,face="bold"),
            legend.position = "right",
            legend.title = element_text(size=18,face='bold'),
            legend.text=element_text(size=18)
      )
    
    filename = paste0('output/MSE_barcharts/sim_',j,'.png')
    ggsave(fig, filename = filename,
           width = 12, height = 8, device='png', dpi=700)
  }
}
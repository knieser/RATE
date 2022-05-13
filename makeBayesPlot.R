# Make Bayes plot
makeBayesPlot = function(N,p,a1,tau,var_vals,rho_vals){
  
  grps = 1:length(p)
  grp_names = paste("Group", grps)
  Ymodel = y~(1+g)*(trt+z1+z2+z3)
  
  # simulate data
  smp <- simulateData(N,p,a1,tau)
  
  # stratified estimates 
  strat_output <- estStratified(smp,"trt","g",Ymodel)
  strat_est = strat_output$estimates[,"estimate"]
  strat_var = (strat_output$estimates[,"std_err"])^2
  varY = strat_output$resid_var
  
  # synthetic sample estimates
  est = estSynthetic_bayes(strat_est, strat_var, varY, var_vals, rho_vals)

  # organize plot data
  plot_data = data.frame(
    group = rep(grp_names,each=length(var_vals)),
    sigma = rep(sqrt(var_vals), length(grps)),
    q = c(est$q[,,1,1], est$q[,,1,2], est$q[,,1,3])
  )
  
  # make plot
  fig <- ggplot(data=plot_data, aes(x=sigma, y=q, group=group)) +
    geom_line(aes(color=group),size=6) +
    theme_classic() +
    scale_color_manual(values = c('#7030A0','#049A68', '#FFC000')) +
    coord_cartesian(xlim = c(0,1.1), ylim = c(0,1), expand = FALSE) +
    labs(x = 'Prior standard deviation of subgroup ATEs',
         y = '',
         title = 'Synthetic sample proportions for Group 1') +
    theme(text = element_text(size=25, face = "bold"),
          axis.line = element_line(size=1),
          axis.text = element_text(size=28, face = "plain",color = "black"),
          axis.ticks.length = unit(8,"pt"),
          plot.title = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = "top")
  
  print(fig)
  
  # save plot
  ggsave(fig, filename = 'output/bayes_varying_var_tau.png',
         width = 12, height = 8, device='png', dpi=700)
  
  return(plot_data)
}

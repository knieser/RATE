summarizeMSE <- function(sim_output){
  
  lookup = sim_output$lookup
  num_grps = lookup[1, "groups"]
  
  # a list of data frames, one per group
  group_data = vector(mode = "list", length(num_grps))

  for (g in 1:num_grps){
    grp_df = data.frame(
      'Estimate' = 1:nrow(lookup)
    )
    for (j in 1:nrow(lookup)){
      est = sim_output$estimates[[j]]
      grp_df[j,"Strat_MSE"] = est$strat_MSE[g]
      grp_df[j,"RE_MSE"] = est$re_MSE[g]
      grp_df[j,"RATE1_MSE"] = est$RATE1_MSE[g]
      grp_df[j,"RATE2_MSE"] = est$RATE2_MSE[g]
      grp_df[j,"RATE3_MSE"] = est$RATE3_MSE[g]
    }
    grp_df[,"Diff_RE"] = grp_df[,"RE_MSE"] - grp_df[,"Strat_MSE"]
    grp_df[,"Diff_RATE1"] = grp_df[,"RATE1_MSE"] - grp_df[,"Strat_MSE"]
    grp_df[,"Diff_RATE2"] = grp_df[,"RATE2_MSE"] - grp_df[,"Strat_MSE"]
    grp_df[,"Diff_RATE3"] = grp_df[,"RATE3_MSE"] - grp_df[,"Strat_MSE"]
    
    group_data[[g]] = grp_df
  }
  
  diff = c(group_data[[g]]$Diff_RE, group_data[[g]]$Diff_RATE1,
           group_data[[g]]$Diff_RATE2, group_data[[g]]$Diff_RATE3)
  
  for (g in 1:(num_grps-1)){
    diff = c(diff, group_data[[g+1]]$Diff_RE, group_data[[g+1]]$Diff_RATE1,
             group_data[[g+1]]$Diff_RATE2, group_data[[g+1]]$Diff_RATE3)
  }

  plot_df = data.frame(
    'group' = rep(1:num_grps, each = nrow(lookup)*4),
    'estimator' = rep(rep(c("Random effects", "RATE1", "RATE2", "RATE3"), 
                      each = nrow(lookup)), num_grps),
    'd' = diff
  )

  fig <- ggplot(data = plot_df, aes(d)) + 
    geom_histogram() + 
    facet_grid(group ~ estimator, switch = "y") +
    geom_vline(xintercept=0, linetype=2, lwd=1, col=2)+
    xlab('Difference in MSE relative to stratification') + 
    ylab('') +
    theme(plot.title=element_text(size=16,face="bold"),
          axis.text.x=element_text(size=16),
          axis.ticks.length=unit(.25,"cm"),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title=element_text(size=18,face="bold"),
          strip.text=element_text(size=18,face="bold",angle=0),
          legend.position = "top",
          legend.title = element_blank(),
          legend.text=element_text(size=18)
    )

  return(fig)
  
}
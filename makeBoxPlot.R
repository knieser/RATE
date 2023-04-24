makeBoxPlot <- function(estimates, outfile){
  
  # make plot dataframe
  df <- makeBoxPlotData(estimates)
  
  # remove SATE
  df <- df[df$estimator != 'SATE',]
  
  # make figure
  fig <- ggplot(data = df, aes(x = estimator, y = sqrt(d))) + 
    geom_boxplot() +
    scale_x_discrete(position='top') + 
    coord_flip() + 
    facet_grid(group ~ ., switch = 'y') +
    ylab('RMSE') + 
    xlab('') +
    theme_bw() + 
    theme(plot.title=element_text(size=16,face="bold"),
          axis.text.y=element_text(size=16),
          axis.ticks.length=unit(.25,"cm"),
          axis.text.x = element_text(size=16),
          axis.title=element_text(size=18,face="bold"),
          strip.text=element_text(size=18,face="bold",angle=0),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size=18)
    )
  
  # save figure
  ggsave(fig, 
         filename = outfile,
         width = 12, height = 8, device='pdf', dpi=700)
  
  return(df)
  
}
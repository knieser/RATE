# clear variables
rm(list=ls())

# set seed
set.seed(515)

# Create output folder in working directory 
# if one doesn't already exist
if (file.exists('output')){
  message("Output folder already exists")
} else{
  message("Creating an output folder")
  dir.create('output')
}

# libraries
library(lme4) # for random effects model
library(multcomp) # for linear combination for reg. coefs.
library(ggplot2) # plots
library(tictoc) # runtime tracking

# functions
source("generateTau.R")
source("simulateData.R")
source("computeMSE.R")
source("compileMSE.R")
source("simMain.R")
source("runSimulations.R")
source("estStratified.R")
source("estRATE.R")
source("estRATErange.R")
source("makePlotData.R")


##### Simulations ####

N         = 300
p         = list(c(.67, .15, .1, .05, .03),
                 c(.75, .15, .1))
phi       = c(1.5, 1, .75)
tau_dist  = c("normal", "mixture", "gamma")
tau_draws = 500
loops     = 250
num_sims  = length(p) * length(tau_dist)
results   = vector(mode = "list", length = num_sims)

i = 1
for (t in 1:length(tau_dist)){
  for (k in 1:length(p)){
    ngrps = length(p[[k]])
    message('Drawing tau from ',tau_dist[t],' with ',ngrps,' groups...')
    estimates <- simMain(N, p[[k]], tau_dist[t], tau_draws, 
                    decision_rule = 1, phi, loops)
    plot_df <- makePlotData(estimates)
    
    # save results
    results[[i]] <- list(
      tau_dist  = tau_dist[t],
      ngrps     = ngrps,
      phi       = phi,
      estimates = estimates,
      plot_df   = plot_df
    )
    
    # make figure
    fig <- ggplot(data = plot_df, aes(x = estimator, y = d)) + 
      geom_boxplot() +
      scale_x_discrete(position='top') + 
      coord_flip() + 
      facet_grid(group ~ ., switch = 'y') +
      geom_hline(yintercept=0, linetype=2, lwd=1, col=1,alpha=.3)+
      ylab('Difference in MSE relative to stratification') + 
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
    outfile = paste0("output/",tau_dist[d],"_sims_",ngrps,"_v3.pdf")
    
    # save figure
    ggsave(fig, 
       filename = outfile,
       width = 12, height = 8, device='pdf', dpi=700)
    i = i+1
  }
}

#save.image(file = 'output/v3.RData')

#### Example ####
#fig_example <- estRATErange(c(2, 1.6, .6), c(.05, .07, .4), 1, seq(0, 2, .01))
#ggsave(fig_example, 
#       filename = 'output/example.png',
#       width = 12, height = 8, device='png', dpi=700)

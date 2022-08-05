# clear variables
rm(list=ls())

# set seed
set.seed(123)

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
library(ggplot2) # plots
library(viridis) # color scheme
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
source("plotResults.R")


##### Simulations ####

N = c(300)
p = list(
  c(.67, .15, .1, .05, .03),
  c(.75, .15, .1))
a1 = c(.5, .5, .5, .5, .5)
phi = c(0.75,1,1.5)
tau_dist = c("normal", "mixture", "gamma", "none")
tau_draws = 50
loops = 50

num_sims = length(p) * length(tau_dist)
results = vector(mode = "list", length = num_sims)

i = 1
for (d in 1:length(tau_dist)){
  for (k in 1:length(p)){
    ngrps = length(p[[k]])
    message("Drawing tau from ",tau_dist[d],' with ',ngrps,' groups...')
    estimates <- simMain(N, p[[k]], a1, tau_dist[d], tau_draws, 
                    decision_rule = 1, phi, loops)
    results[[i]] <- list(
      tau_dist = tau_dist[d],
      ngrps = ngrps,
      phi = phi,
      estimates = estimates
    )
    fig <- plotResults(results[[i]]$estimates)
    outfile = paste0("output/",tau_dist[d],"_sims_",ngrps,"_v3.pdf")
    ggsave(fig, 
       filename = outfile,
       width = 12, height = 8, device='pdf', dpi=700)
    i = i+1
  }
}


#### Example ####
#fig_example <- estRATErange(c(2, 1.6, .6), c(.05, .07, .4), 1, seq(0, 2, .01))
#ggsave(fig_example, 
#       filename = 'output/example.png',
#       width = 12, height = 8, device='png', dpi=700)


##### save workspace
save.image(file = 'output/v3.RData')

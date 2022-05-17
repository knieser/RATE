##### Main script #####

# clear variables
rm(list=ls())

# set seed
set.seed(123)

# set working directory
setwd("~/Documents/research/Dissertation/Aim_3/code_repo")
# load('output/.RData')

# libraries
library(lme4) 
library(ggplot2)

# functions
source("estOM.R")
source("estStratified.R")
source("estSynthetic_minimax.R")
source("estSynthetic_bayes.R")
source("simulateData.R")
source("runSimulations.R")
source("makeMinimaxPlot.R")
source("makeBayesPlot.R")
source("makeForestPlot.R")

# run simulations
estimates1 <- runSimulations(N = 500, 
                             p = c(0.10, 0.70, 0.20), 
                             a1 = c(.5, .5, .5), 
                             tau = c(1,-0.5,0.1),
                             delta_vals = c(.5,1,1.5),
                             var_vals = c(.5,1,1.5),
                             loops = 1e4)

estimates2 <- runSimulations(N = 500, 
                             p = c(0.10, 0.70, 0.20), 
                             a1 = c(.5, .5, .5), 
                             tau = c(1,1,1), 
                             delta_vals = c(0.5,1,1.5),
                             var_vals = c(.5,1,1.5),
                             loops = 1e4)

estimates3 <- runSimulations(N = 500, 
                             p = c(0.10, 0.85, 0.05), 
                             a1 = c(.5, .5, .5), 
                             tau = c(.5,-1,-2), 
                             delta_vals = c(0.5,1,1.5),
                             var_vals = c(.5,1,1.5),
                             loops = 1e4)

# make plots
fig1 <- makeMinimaxPlot(N = 500, 
                        p = c(0.10, 0.70, 0.20), 
                        a1 = c(.5, .5, .5), 
                        tau = c(1,-0.5,0),
                        delta_vals = seq(0,2,.001))
fig2 <- makeBayesPlot(N = 500, 
                      p = c(0.10, 0.70, 0.20), 
                      a1 = c(.5, .5, .5), 
                      tau = c(1,-0.5,0), 
                      var_vals = seq(0,3,.001), 
                      rho_vals = 0.3)
fig3 <- makeForestPlot(estimates1)
fig4 <- makeForestPlot(estimates2)
fig5 <- makeForestPlot(estimates3)

# save plots
ggsave(fig1, filename = 'output/minimax_varying_delta.png',
       width = 12, height = 8, device='png', dpi=700)
ggsave(fig2, filename = 'output/bayes_varying_var_tau.png',
       width = 12, height = 8, device='png', dpi=700)
ggsave(fig3, filename = 'output/sim1_results.png',
       width = 12, height = 8, device='png', dpi=700)
ggsave(fig4, filename = 'output/sim2_results.png',
       width = 12, height = 8, device='png', dpi=700)

# save workspace
save.image(file = 'output/.RData')

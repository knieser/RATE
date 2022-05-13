##### Main script #####

# clear variables
rm(list=ls())

# set seed
set.seed(123)

# set working directory
setwd("~/Documents/research/Dissertation/Aim_3/code_repo")

# libraries
library(lme4) 
library(forestplot)
library(ggplot2)

# functions
source("estStratified.R")
source("estSynthetic_minimax.R")
source("estSynthetic_bayes.R")
source("simulateData.R")
source("runSimulations.R")
source("makeMinimaxPlot.R")
source("makeBayesPlot.R")
source("makeForestPlot.R")

# make example plots for synthetic group probabilities vs parameter values
minimax_plot_df <- makeMinimaxPlot(N = 500, 
                     p = c(0.10, 0.70, 0.20), 
                     a1 = c(.5, .5, .5), 
                     tau = c(1,-0.5,0),
                     delta_vals = seq(0,2,.001))

bayes_var_plot_df <- makeBayesPlot(N = 500, 
                                      p = c(0.10, 0.70, 0.20), 
                                      a1 = c(.5, .5, .5), 
                                      tau = c(1,-0.5,0), 
                                      seq(0,3,.001), 0)

bayes_rho_plot_df <- makeBayesPlot(N = 500, 
                                      p = c(0.10, 0.70, 0.20), 
                                      a1 = c(.5, .5, .5), 
                                      tau = c(1,-0.5,0), 
                                      0.5, seq(-1,1,.001))

# run simulations
estimates1 <- runSimulations(N = 500, 
                             p = c(0.10, 0.70, 0.20), 
                             a1 = c(.5, .5, .5), 
                             tau = c(1,-0.5,0.1),
                             delta_vals = c(.5,1,1.5),
                             loops = 1e2)

estimates2 <- runSimulations(N = 500, 
                             p = c(0.10, 0.70, 0.20), 
                             a1 = c(.5, .5, .5), 
                             tau = c(1,1,1), 
                             delta_vals = c(0.5, .75, 1),
                             loops = 1e2)

# make plots
fig1 <- makeForestPlot(estimates1)
fig2 <- makeForestPlot(estimates2)

# save data
save(estimates1, file = 'output/estimates1.RData')
save(estimates2, file = 'output/estimates2.RData')


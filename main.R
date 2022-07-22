# libraries
library(lme4)
library(ggplot2) # plots
library(viridis) # color scheme
library(tictoc) # runtime tracking

# functions
source("generate_tau.R")
source("simulateData.R")
source("computeMSE.R")
source("compileMSE.R")
source("simMain.R")
source("runSimulations.R")
source("estStratified.R")
source("estRATE.R")
source("estRATErange.R")
source("MSEbarchart.R")
source("summarizeMSE.R")

# Create output folder in working directory 
# if one doesn't already exist
if (file.exists('output')){
  cat("Output folder exists")
} else{
  cat("Creating an output folder")
  dir.create('output')
}


#### Simulations ####

# simulated example
fig_example <- estRATErange(c(2, 1.6, .6), c(.05, .07, .4), 1, seq(0, 2, .01))
ggsave(fig_example, 
       filename = 'output/example.png',
       width = 12, height = 8, device='png', dpi=700)


# Monte Carlo simulations
sample_size = c(600)
p = matrix(data = c(.7, .1, .1, .05, .05),
           nrow = 5, ncol = 1)
a1 = matrix(data = c(.5, .5, .5, .5, .5),
            nrow = 5, ncol = 1)
tau_draws = 10
loops = 10


# normal distribution
tic()
tau_dist = c("Normal")
sim_normal <- simMain(sample_size, p, a1, tau_dist, tau_draws, 
                    decision_rule = 1, delta = c(1,2,3)/sqrt(2), loops)
fig_normal <- summarizeMSE(sim_normal)
ggsave(fig_normal, 
       filename = 'output/normal_sims.png',
       width = 12, height = 8, device='png', dpi=700)
toc()


# Skewed normal distribution
tic()
tau_dist = c("Skewed")
sim_skewed <- simMain(sample_size, p, a1, tau_dist, tau_draws, 
                      decision_rule = 1, delta = c(1,2,3)/sqrt(2), loops)
fig_skewed <- summarizeMSE(sim_skewed)
ggsave(fig_skewed, 
       filename = 'output/skewed_sims.png',
       width = 12, height = 8, device='png', dpi=700)
toc()

# save workspace
save.image(file = 'output/.RData')


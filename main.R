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
library(multcomp) # for linear combination for regression coefs.
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
source("makeBoxPlotData.R")
source("makeBoxPlot.R")
source("estRATEequal.R")
source("checkConvergence.R")


##### Simulations ####

version   = "v8"
N         = 300
p         = list(c(.67, .15, .1, .05, .03),
                 c(.75, .15, .1))
noise     = list(rep(1, 5),
                 rep(1, 3))
tau_dist  = c("normal", "mixture", "gamma")
#tau_dist  = c("normal", "uniform")
phi       = c(.75, 1, 1.5)
tau_draws = 500
loops     = 500
num_sims  = length(p) * length(tau_dist)
results   = vector(mode = "list", length = num_sims)

i = 1
for (t in 1:length(tau_dist)){
  for (k in 1:length(p)){
    ngrps = length(p[[k]])
    message('Drawing tau from ',tau_dist[t],' with ',ngrps,' groups...')
    estimates <- simMain(N, p[[k]], tau_dist[t], tau_draws, noise[[k]],
                    decision_rule = 1, phi, loops)
    
    # make box plot
    outfile = paste0("output/boxplot_",tau_dist[t],"_sims_",ngrps,"_", version,".pdf")
    boxplotdf <- makeBoxPlot(estimates, outfile)
    
    # save results
    results[[i]] <- list(
      tau_dist  = tau_dist[t],
      ngrps     = ngrps,
      phi       = phi,
      boxplotdf = boxplotdf,
      estimates = estimates
     )
    
    i = i+1
  }
}

# check convergence
for (j in 1:length(results)){
  checkConvergence(results[[j]]$boxplotdf)  
}

save.image(file = paste0('output/convergence_',version,'.RData'))


# Summary table of RMSE values
library(dplyr)
boxplotdf <- makeBoxPlotData(results[[2]]$estimates)
boxplotdf$rmse <- sqrt(boxplotdf$d)
RMSE_table <- boxplotdf %>% group_by(group, estimator) %>% 
  summarize(min = min(rmse),
            p25 = quantile(rmse, .25),
            median = median(rmse),
            p75 = quantile(rmse, .75),
            max = max(rmse))
write.csv(RMSE_table, file = paste0('output/RMSE_table_', version, '.csv'))



# investigate large and small values of RATE RMSE
RATE_results <- boxplotdf[boxplotdf$estimator == "RATE-opt (1)" & boxplotdf$group == 'Group 3',]

high_rmse <- RATE_results %>% slice_max(n=5, order_by = d)
idx_max = sapply(high_rmse$d, function(x) which(RATE_results$d == x))
results[[2]]$estimates[[idx_max[1]]]$parameters$tau
results[[2]]$estimates[[idx_max[2]]]$parameters$tau
results[[2]]$estimates[[idx_max[3]]]$parameters$tau
results[[2]]$estimates[[idx_max[4]]]$parameters$tau
results[[2]]$estimates[[idx_max[5]]]$parameters$tau

low_rmse <- RATE_results %>% slice_min(n=5, order_by = d)
idx_min = sapply(low_rmse$d, function(x) which(RATE_results$d == x))
results[[2]]$estimates[[idx_min[1]]]$parameters$tau
results[[2]]$estimates[[idx_min[2]]]$parameters$tau
results[[2]]$estimates[[idx_min[3]]]$parameters$tau
results[[2]]$estimates[[idx_min[4]]]$parameters$tau
results[[2]]$estimates[[idx_min[5]]]$parameters$tau



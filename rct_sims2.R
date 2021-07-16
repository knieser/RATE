
# set working directory
# setwd("~/Documents/research/Dissertation/Aim_3/code_repository")
# load("~/Documents/research/Dissertation/Aim_3/code_repository/.RData")

# libraries
library(tictoc)
library(dplyr)
library(gtools)
library(forestplot)

# clear variables
rm(list=ls())

# set seed
set.seed(91)

tic("code running")
# set parameters
n = 800
loops = 1e3

# pre-allocate
reg_estimates = 1:loops
z1_estimates = 1:loops
z2_estimates = 1:loops
z3_estimates = 1:loops
z4_estimates = 1:loops
z5_estimates = 1:loops
ripw_estimates = matrix(data=NA,nrow=loops,ncol=5)

z1_values = c(1,2,3,4,5)
perms = permutations(length(z1_values),length(z1_values),z1_values)
z1_probs = c(0.60,0.20,0.10,0.08,0.02)

for(l in 1:loops){
  # simulate a sample
  smp <- data.frame(
    id = 1:n,
    #z1 = rbinom(n,1,0.8),
    z1 = sample(z1_values,n,replace=T,prob = z1_probs),
    z2 = rbinom(n,1,0.4),
    z3 = rnorm(n,3,1)
  )
  
  # simulate potential outcomes
  smp$y0 = 0 + 1*smp$z1 + 0.5*smp$z2 + rnorm(n,0,1)
  smp$y1 = -2 + 2*smp$z1 + 0.5*smp$z2 + rnorm(n,0,1)
  
  # randomly assign treatment
  smp$trt = rbinom(n,1,0.5)
  
  # observe one of the potential outcomes
  smp$y = ifelse(smp$trt==1,smp$y1,smp$y0)
  
  # standard calculation of treatment difference
  reg_estimates[l] = summary(lm(y~trt,data=smp))$coef[2]
  
  # stratum effects
  z1_estimates[l] = summary(lm(y~trt,data=smp[smp$z1==1,]))$coef[2]
  z2_estimates[l] = summary(lm(y~trt,data=smp[smp$z1==2,]))$coef[2]
  z3_estimates[l] = summary(lm(y~trt,data=smp[smp$z1==3,]))$coef[2]
  z4_estimates[l] = summary(lm(y~trt,data=smp[smp$z1==4,]))$coef[2]
  z5_estimates[l] = summary(lm(y~trt,data=smp[smp$z1==5,]))$coef[2]
  
  
  
  
  ###### Our variation ########
 
  # estimate sample proportions
  pz = 1:length(z1_values)
  for (j in 1:length(z1_values)){
    pz[j] = mean(smp$z1==z1_values[j])
  }
  
  # find max sample proportion and its position
  m = max(pz)
  m_idx = which(m %in% pz)
  
  # algorithm to get possible weightings
  weights = matrix(data=NA, nrow=length(pz),ncol=length(pz))
  for (k in 1:length(pz)){
    # get fresh copy of pz
    perm_pz = pz
    # put indexed pz where the max is
    perm_pz[m_idx] <- perm_pz[k]
    # move max into indexed pz
    perm_pz[k] <- m
    # record this possible weighting
    weights[k,] = perm_pz / pz
  }
  
  # # get possible permutations of pz and possible weights
  # perm_pz = matrix(pz[perms[1:nrow(perms),]],nrow=nrow(perms))
  # weights = t(apply(perm_pz, 1, "/", pz))

  # get matrix of weights, columns are different weights
  ripw = matrix(data=NA,nrow=n,ncol=nrow(weights))
  for (j in 1:ncol(ripw)){
    for (i in 1:nrow(ripw)){
      z1 = smp$z1[i]
      ripw[i,j] = weights[j,z1] 
    }
    ripw_estimates[l,j] = summary(lm(y~trt,data=smp,weights=ripw[,j]))$coef[2]
  }
}


# aggregate results
reg_results <- c(mean(reg_estimates), sd(reg_estimates))
z1_results <- c(mean(z1_estimates), sd(z1_estimates))
z2_results <- c(mean(z2_estimates), sd(z2_estimates))
z3_results <- c(mean(z3_estimates), sd(z3_estimates))
z4_results <- c(mean(z4_estimates), sd(z4_estimates))
z5_results <- c(mean(z5_estimates), sd(z5_estimates))
ripw_results <- c(mean(ripw_estimates), sd(ripw_estimates))
results <- rbind(reg_results, ripw_results)

mean_1 = mean(ripw_estimates[,1])
mean_2 = mean(ripw_estimates[,2])
mean_3 = mean(ripw_estimates[,3])
mean_4 = mean(ripw_estimates[,4])
mean_5 = mean(ripw_estimates[,5])

lb_1 = mean(ripw_estimates[,1]) - 
  2*sd(ripw_estimates[,1])
lb_2 = mean(ripw_estimates[,2]) - 
  2*sd(ripw_estimates[,2])
lb_3 = mean(ripw_estimates[,3]) - 
  2*sd(ripw_estimates[,3])
lb_4 = mean(ripw_estimates[,4]) - 
  2*sd(ripw_estimates[,4])
lb_5 = mean(ripw_estimates[,5]) - 
  2*sd(ripw_estimates[,5])

ub_1 = mean(ripw_estimates[,1]) + 
  2*sd(ripw_estimates[,1])
ub_2 = mean(ripw_estimates[,2]) + 
  2*sd(ripw_estimates[,2])
ub_3 = mean(ripw_estimates[,3]) + 
  2*sd(ripw_estimates[,3])
ub_4 = mean(ripw_estimates[,4]) + 
  2*sd(ripw_estimates[,4])
ub_5 = mean(ripw_estimates[,5]) + 
  2*sd(ripw_estimates[,5])
# 

# meta_data <- 
#   structure(list(
#     mean  = cbind(
#           c(mean_1, mean_2, mean_3, mean_4, mean_5,
#               NA, reg_results[1]),
#           c(-1,0,1,2,3,NA,reg_results[1])
#           ),
#     lower = cbind(
#           c(lb_1, lb_2, lb_3, lb_4, lb_5, 
#               NA, reg_results[1]-2*reg_results[2]),
#           c(-1.5,-0.5,0.5,1.5,2.5,NA,reg_results[1]-2*reg_results[2])),
#     upper = cbind(
#           c(ub_1, ub_2, ub_3, ub_4, ub_5,
#               NA, reg_results[1]+2*reg_results[2]),
#           c(-0.5,0.5,1.5,2.5,3.5,NA,reg_results[1]+2*reg_results[2]))
#           ), 
#     .Names = c("mean", "lower", "upper"), 
#     row.names = c(-5L), 
#     class = "data.frame")

tabletext <- cbind(
  c("Z=1", "Z=2", 
    "Z=3", "Z=4", "Z=5", "Observed ATE")
  )

forestplot(
  legend = c("Fair Estimates","Stratified Estimates"),        
  tabletext,
          mean  = cbind(
             c(mean_1, mean_2, mean_3, mean_4, mean_5,
               reg_results[1]),
             c(z1_results[1],
               z2_results[1],
               z3_results[1],
               z4_results[1],
               z5_results[1],
               reg_results[1])
           ),
           lower = cbind(
             c(lb_1, lb_2, lb_3, lb_4, lb_5, 
               reg_results[1]-2*reg_results[2]),
             c(z1_results[1]-2*z1_results[2],
               z2_results[1]-2*z2_results[2],
               z3_results[1]-2*z3_results[2],
               z4_results[1]-2*z4_results[2],
               z5_results[1]-2*z5_results[2],
               reg_results[1]-2*reg_results[2])),
           upper = cbind(
             c(ub_1, ub_2, ub_3, ub_4, ub_5,
               reg_results[1]+2*reg_results[2]),
             c(z1_results[1]+2*z1_results[2],
               z2_results[1]+2*z2_results[2],
               z3_results[1]+2*z3_results[2],
               z4_results[1]+2*z4_results[2],
               z5_results[1]+2*z5_results[2],
               reg_results[1]+2*reg_results[2])),
           boxsize=0.2,
           line.margin = 0.25,
           lwd.ci = c(1,3),
           lty.ci = c(2,2),
          xlab = "Average Treatment Difference",
           col = fpColors(box = c("purple", "black"),
                          line =c("purple", "black")),
           txt_gp = 
             fpTxtGp(
              label = gpar(fontfamily = "sans", cex = 1.5),
              ticks = gpar(fontfamily = "sans", cex = 1.5),
              xlab  = gpar(fontfamily = "sans", cex = 1.5)),
           )

# output
results
toc()

# 
# 
# # distributions of ATEs for each level
# par(mfrow=c(3,2))
# 
# for (k in 1:5){
#   hist(ripw_estimates[,max.col(perm_pz)==k],
#        main = paste('Z =', k),
#        breaks=25,
#        xlab = ''
#   )
# }
# 
# # forest plot
# mean_1 = mean(ripw_estimates[,max.col(perm_pz)==1])
# mean_2 = mean(ripw_estimates[,max.col(perm_pz)==2])
# mean_3 = mean(ripw_estimates[,max.col(perm_pz)==3])
# mean_4 = mean(ripw_estimates[,max.col(perm_pz)==4])
# mean_5 = mean(ripw_estimates[,max.col(perm_pz)==5])
# 
# lb_1 = mean(ripw_estimates[,max.col(perm_pz)==1]) - 
#             2*sd(ripw_estimates[,max.col(perm_pz)==1])
# lb_2 = mean(ripw_estimates[,max.col(perm_pz)==2]) - 
#             2*sd(ripw_estimates[,max.col(perm_pz)==2])
# lb_3 = mean(ripw_estimates[,max.col(perm_pz)==3]) - 
#             2*sd(ripw_estimates[,max.col(perm_pz)==3])
# lb_4 = mean(ripw_estimates[,max.col(perm_pz)==4]) - 
#             2*sd(ripw_estimates[,max.col(perm_pz)==4])
# lb_5 = mean(ripw_estimates[,max.col(perm_pz)==5]) - 
#             2*sd(ripw_estimates[,max.col(perm_pz)==5])
# 
# ub_1 = mean(ripw_estimates[,max.col(perm_pz)==1]) + 
#             2*sd(ripw_estimates[,max.col(perm_pz)==1])
# ub_2 = mean(ripw_estimates[,max.col(perm_pz)==2]) + 
#             2*sd(ripw_estimates[,max.col(perm_pz)==2])
# ub_3 = mean(ripw_estimates[,max.col(perm_pz)==3]) + 
#             2*sd(ripw_estimates[,max.col(perm_pz)==3])
# ub_4 = mean(ripw_estimates[,max.col(perm_pz)==4]) + 
#             2*sd(ripw_estimates[,max.col(perm_pz)==4])
# ub_5 = mean(ripw_estimates[,max.col(perm_pz)==5]) + 
#             2*sd(ripw_estimates[,max.col(perm_pz)==5])


# # distributions of ATEs for each level
# par(mfrow=c(3,2))
# 
# for (k in 1:5){
#   hist(ripw_estimates[,k],
#        main = paste('Z =', k),
#        breaks=25,
#        xlab = ''
#   )
# }

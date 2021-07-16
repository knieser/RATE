
# set working directory
# setwd("~/Documents/research/Dissertation/Aim_3/code_repository")
# load("~/Documents/research/Dissertation/Aim_3/code_repository/.RData")

# libraries
library(tictoc)

# clear variables
rm(list=ls())

# set seed
set.seed(91)

tic("code running")
# set parameters
pop_n = 1e4
smp_n = 500
loops = 1e3

# simulate a population
pop <- data.frame(
  id = 1:pop_n,
  z1 = rbinom(pop_n,1,0.8),
  #z1 = sample(c(0,1,2),pop_n,replace=T,prob = c(0.6,0.3,0.1)),
  z2 = rbinom(pop_n,1,0.4) 
)

# simulate potential outcomes
# TE=2 for z1=1
# TE=0 for z1=0
pop$y0 = 0 + 1*pop$z1 + 0.5*pop$z2 + rnorm(pop_n,0,1)
pop$y1 = -2 + 5*pop$z1 + 0.5*pop$z2 + rnorm(pop_n,0,1)

# calculate true PATE
PATE = mean(pop$y1) - mean(pop$y0)

# see distribution of TEs for z1=1 and z1=0
# hist(pop[pop$z1==0,]$y1-pop[pop$z1==0,]$y0)
# hist(pop[pop$z1==1,]$y1-pop[pop$z1==1,]$y0)
# hist(pop[pop$z1==2,]$y1-pop[pop$z1==2,]$y0)
# hist(pop$y1-pop$y0)

# pre-allocate
reg_estimates = 1:loops
reg_estimates_z0 = 1:loops
reg_estimates_z1 = 1:loops
ipw_estimates = 1:loops
sipw_estimates = 1:loops
ripw_estimates = 1:loops


# take non-random samples that depend on z1 (the effect modifier)
# compute estimates for standard regression, IPW, and SIPW
for(i in 1:loops){
  # take a sample
  smp <- pop[sample(pop$id, smp_n, 
                   replace=F, prob = 0.3+0.4*pop$z1), ]

  # randomly assign treatment
  smp$treatment = rbinom(smp_n,1,0.5)
  
  # observe one of the potential outcomes
  smp$y = ifelse(smp$treatment==1,smp$y1,smp$y0)
  
  # label individuals sampled
  pop$sampled = ifelse(pop$id %in% smp$id, 1, 0)
  
  # fit models for marginal and conditional prob of being sampled
  marg_logodds = summary(glm(sampled ~ 1,data=pop,family=binomial(link="logit")))$coef[1]
  marg_prob = exp(marg_logodds)/(1+exp(marg_logodds))
  cond_model = glm(sampled ~ z1,data=pop,family=binomial(link="logit"))
  smp$p_hat_S = predict(cond_model,newdata=smp,type="response")
 
  # calculate weights
  ipw = marg_prob/smp$p_hat_S
  sipw = ipw/sum(ipw)


  ###### Our variation ########
  # 
  # for (j in 1:smp_n){
  #   z1 = smp$z1[j]
  #   smp$pz[j] = mean(smp$z1==z1)
  # }
  # 
  # theta <- replicate(1e3,{
  #   smp$pzR = sample(smp$pz,smp_n,replace=T)
  #   smp$weight = smp$pzR/smp$pz
  #   weight_model = lm(weight ~ as.factor(z1), data=smp)
  #   smp$est_weight = predict(weight_model,newdata=smp,type="response")
  #   ripw = smp$est_weight
  #   summary(lm(y~treatment,data=smp,weights=ripw))$coef[2]
  # })
  
  pz0 = mean(smp$z1==0)
  pz1 = mean(smp$z1==1) 

  ripw = ifelse(smp$z1==0, 0.8/pz0, 0.2/pz1)
 

    # # save off original sample
  # smp0 <- smp
  # 
  # # get list of covariate values
  # possible_z1 = unique(smp0$z1)
  # 
  # # randomly select a covariate value, with equal prob.
  # # replace data for j with data 
  # # from another random person with the selected covariate value
  # # repeat many times so that we have a balanced distribution of covariate values
  # for (iter in 1:1e2){
  #   for (j in 1:smp_n){
  #         z1 = sample(possible_z1,1)
  #         tx = smp$treatment[j]
  #         smp[j,2:ncol(smp)] = smp[smp$id==sample(smp$id[smp$z1==z1 & smp$treatment==tx],1,repl=T),2:ncol(smp)]
  #   }
  # }
  # 
  # # take resamples and calculate the ATE in each. 
  # theta <- replicate(1e3, {
  #   for (j in 1:smp_n){
  #     z1 = sample(possible_z1,1)
  #     tx = smp$treatment[j]
  #     smp[j,2:ncol(smp)] = smp[smp$id==sample(smp$id[smp$z1==z1 & smp$treatment==tx],1,repl=T),2:ncol(smp)]
  #   }
  #   summary(lm(y~treatment,data=smp))$coef[2]
  # })
  
  # replace data for individual j with 
  # the data from a person who has different covariates
  # theta <- replicate(1e3, {
  #   for (j in 1:smp_n){
  #     z1 = smp0$z1[j]
  #     tx = smp0$treatment[j]
  #     smp[j,] = smp0[smp0$id==sample(smp0$id[smp0$z1!=z1 & smp0$treatment==tx],1,repl=T),]
  #   }
  #   summary(lm(y~treatment,data=smp))$coef[2]
  # })
  
  # M = 1e2 #alt target pop scaling factor
  # nz0 = sum(smp$z1==0)
  # nz1 = sum(smp$z1==1)
  # nz2 = sum(smp$z1==2)
  # # randomly permute
  # perms_x <- permutations(3,3,c(nz0,nz1,nz2))
  # 
  # logoddsz0 = log((nz0/nz1/M)/(1-(nz0/nz1/M)))
  # logor = log((nz1/nz0/M)/(1-(nz1/nz0/M)))-logoddsz0
  # smp$p_hat_R = exp(logoddsz0 + logor*smp$z1) / 
  #         (1 + exp(logoddsz0 + logor*smp$z1))
  # 
  # ripw = marg_prob/smp$p_hat_R
  
    # phatR = 1:smp_n
  # for (j in 1:smp_n){
  #   # get covariate value for individual j
  #   z1 = smp$z1[j]
  #   #z2 = smp$z2[j]
  #   # sample phatR from the list of p_hat_S among people with different covariates
  #   phatR[j] = sample(smp$p_hat_S[smp$z1!=z1],1,replace=T)
  # }
  # smp$p_hat_R = phatR
  # 
  # ripw = ipw*smp$p_hat_R/marg_prob
  
  # get estimates
  reg_estimates[i] = summary(lm(y~treatment,data=smp))$coef[2]
  reg_ixn_model <- lm(y~treatment+z1+treatment*z1,data=smp)
  reg_estimates_z0[i] = summary(reg_ixn_model)$coef[2]
  reg_estimates_z1[i] = summary(reg_ixn_model)$coef[2] + summary(reg_ixn_model)$coef[4]
  ipw_estimates[i] = summary(lm(y~treatment,data=smp,weights=ipw))$coef[2]
  sipw_estimates[i] = summary(lm(y~treatment,data=smp,weights=sipw))$coef[2]
  ripw_estimates[i] = summary(lm(y~treatment,data=smp,weights=ripw))$coef[2]
}

reg_results <- c(mean(reg_estimates), sd(reg_estimates))
reg_z0_results <- c(mean(reg_estimates_z0), sd(reg_estimates_z0))
reg_z1_results <- c(mean(reg_estimates_z1), sd(reg_estimates_z1))
ipw_results <- c(mean(ipw_estimates), sd(ipw_estimates))
sipw_results <- c(mean(sipw_estimates), sd(sipw_estimates))
ripw_results <- c(mean(ripw_estimates), sd(ripw_estimates))
results <- rbind(reg_results, ipw_results, 
      reg_z0_results, reg_z1_results, ripw_results)

# output
PATE
results
toc()







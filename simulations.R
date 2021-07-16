
n = 1e3

data1 <- data.frame(
  id = 1:n,
  x1 = rbinom(n,1,0.8),
  z = rbinom(n,1,0.5)
)

# simulate response based on tx, group, and noise
# for most people (x1=1), tx is beneficial, beta = +2
# however, for x1=0, tx is harmful, beta = -2
data1$y = -2*data1$z + 4*data1$z*data1$x1 + rnorm(n,0,1)

# estimate treatment diff for aggregate sample
summary(lm(y~z, data=data1))
beta = lm(y ~ z, data=data1)$coef[2]

# add interaction term
summary(lm(y~z + x1 + x1*z, data=data1))

# bootstrap
beta_boot <- replicate(1e4, {
  data_resample <- data1[sample(nrow(data1), nrow(data1), replace=T), ]
  lm(y ~ z, data=data_resample)$coef[2]
})

# plot bootstrap distribution and vertical line at point estimate
hist(beta_boot)
abline(v=beta, col=2, lwd=2)
mean(beta_boot)
sqrt(var(beta_boot))

# bootstrap interaction term
beta_boot_interaction <- replicate(1e4, {
  data_resample <- data1[sample(nrow(data1), nrow(data1), replace=T), ]
  lm(y ~ z + x1 + z*x1, data=data_resample)$coef[2]
})

# plot bootstrap distribution and vertical line at point estimate
hist(beta_boot_interaction)
abline(v=beta, col=2, lwd=2)
mean(beta_boot_interaction)
sqrt(var(beta_boot_interaction))


# what if we had oversampled the group where x1=0?
library(sampling)
data1 <- data1[order(data1$x1),] #sort data
beta_oversample <- replicate(1e4, {
  data_oversample <- getdata(data1, strata(data1, "x1", size = c(800, 200), 
                                           method = "srswr"))
  lm(y ~ z, data=data_oversample)$coef[2]
})

hist(beta_oversample)
mean(beta_oversample)



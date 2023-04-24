
source('estRATE.R')
source('estRATEequal.R')

# NV girls, V girls, NV boys, V boys
n = c(875, 551, 761, 642)
p = n/sum(n)
estimates = c(-.207, 0.016, 0.039, 0.262)
sigma = diag((c((-0.071 + 0.342)/2/1.96, 
               (0.182 + 0.149)/2/1.96,
               (0.172 + 0.094)/2/1.96,
               (0.437 - 0.087)/2/1.96
               ))^2)
phi = 0.125


RATE_est = estRATE(estimates, sigma, 1, phi)
RATEequal_est = estRATEequal(estimates, sigma, phi)

RATE_est
RATEequal_est


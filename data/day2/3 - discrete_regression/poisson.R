############################################################
# Initial setup
############################################################

library(rstan)
library(bayesplot)
stopifnot(packageVersion("bayesplot") < "1.3.0")
# Run install.packages("bayesplot") if your version is < 1.3.0


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('C:/Users/jlandesman/Documents/Probabilistic_Programming/data/stan_utility.R')

############################################################
# Fit initial Stan program
############################################################

input_data <- read_rdump("discrete_regression.data.R")

fit   <- stan(file='poisson.stan', data=input_data, seed=4938483)
params <- extract(fit)

print(fit)

par(mfrow=c(2, 2))

hist(params$y_ppc[,5], main="", xlab="y[5]")
abline(v=input_data$y[5], col=2, lty=1)

hist(params$y_ppc[,10], main="", xlab="y[10]")
abline(v=input_data$y[10], col=2, lty=1)

hist(params$y_ppc[,15], main="", xlab="y[15]")
abline(v=input_data$y[15], col=2, lty=1)

hist(params$y_ppc[,20], main="", xlab="y[20]")
abline(v=input_data$y[20], col=2, lty=1)

y_ppcs <- as.matrix(fit, pars = "y_ppc")
mcmc_recover_hist(
  y_ppcs[, c(5, 10, 15, 20)],
  true = input_data$y[c(5, 10, 15, 20)]
)

ppc_dens_overlay(
  y = input_data$y,
  yrep = y_ppcs[sample(nrow(y_ppcs), 50),]
)


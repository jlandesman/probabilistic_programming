############################################################
# Initial setup
############################################################

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('stan_utility.R')

############################################################
# Fit Poisson model
############################################################

input_data <- read_rdump('discrete_regression.data.R')
fit <- stan(file='poisson.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

params = extract(fit)

# Plot marginal posteriors
par(mfrow=c(2, 2))

hist(params$beta[,1], main="", xlab="beta[1]")
hist(params$beta[,2], main="", xlab="beta[2]")
hist(params$beta[,3], main="", xlab="beta[3]")
hist(params$alpha, main="", xlab="alpha")

# Plot a few visual posterior predictive checks
par(mfrow=c(2, 2))

hist(params$y_ppc[,5], main="", xlab="y[5]")
abline(v=input_data$y[5], col=2, lty=1)

hist(params$y_ppc[,10], main="", xlab="y[10]")
abline(v=input_data$y[10], col=2, lty=1)

hist(params$y_ppc[,15], main="", xlab="y[15]")
abline(v=input_data$y[15], col=2, lty=1)

hist(params$y_ppc[,20], main="", xlab="y[20]")
abline(v=input_data$y[20], col=2, lty=1)

# Plot aggregated predictive distribution
y_ppc = params$y_ppc
dim(y_ppc) <- NULL

p1 <- hist(y_ppc, breaks=(0:50), main="", xlab="y")
p1$counts = p1$counts / sum(p1$counts)
p2 <- hist(input_data$y, breaks=(0:50), main="", xlab="y")
p2$counts = p2$counts / sum(p2$counts)
plot(p1, col=rgb(0, 0, 1, 0.25))
plot(p2, col=rgb(1, 0, 0, 0.25), add=T)

############################################################
# Fit overdispersed Negative Binomial model
############################################################

fit <- stan(file='negative_binomial.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

params = extract(fit)

# Plot marginal posteriors
par(mfrow=c(2, 3))

hist(params$beta[,1], main="", xlab="beta[1]")
abline(v=-0.4,col=2,lty=1)

hist(params$beta[,2], main="", xlab="beta[2]")
abline(v=0.375,col=2,lty=1)

hist(params$beta[,3], main="", xlab="beta[3]")
abline(v=0.5,col=2,lty=1)

hist(params$alpha, main="", xlab="alpha")
abline(v=2,col=2,lty=1)

hist(params$phi, main="", xlab="phi")
abline(v=8,col=2,lty=1)

# Plot a few visual posterior predictive checks
par(mfrow=c(2, 2))

hist(params$y_ppc[,5], main="", xlab="y[5]")
abline(v=input_data$y[5], col=2, lty=1)

hist(params$y_ppc[,10], main="", xlab="y[10]")
abline(v=input_data$y[10], col=2, lty=1)

hist(params$y_ppc[,15], main="", xlab="y[15]")
abline(v=input_data$y[15], col=2, lty=1)

hist(params$y_ppc[,20], main="", xlab="y[20]")
abline(v=input_data$y[20], col=2, lty=1)

# Plot aggregated predictive distribution
y_ppc = params$y_ppc
dim(y_ppc) <- NULL

p1 <- hist(y_ppc, breaks=(0:50), main="", xlab="y")
p1$counts = p1$counts / sum(p1$counts)
p2 <- hist(input_data$y, breaks=(0:50), main="", xlab="y")
p2$counts = p2$counts / sum(p2$counts)
plot(p1, col=rgb(0, 0, 1, 0.25))
plot(p2, col=rgb(1, 0, 0, 0.25), add=T)

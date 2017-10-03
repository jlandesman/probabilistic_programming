############################################################
# Initial setup
############################################################

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('stan_utility.R')

############################################################
# Ungrouped logistic regression
############################################################

input_data <- read_rdump('grouped_logistic_regression.data.R')
fit <- stan(file='logistic_regression.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

params = extract(fit)

# Aggregate fit looks okay, but...
hist(params$p_hat_ppc, main="", xlab="p_ppc")
abline(v=sum(input_data$y) / input_data$N, col=2, lty=1)

# Individual groups are very different from aggregate response
hist(params$p_hat_ppc, main="", xlab="p_ppc", breaks=0.01*(0:100)+0.0)
abline(v=sum(input_data$y * (input_data$h == 1)) / sum(input_data$h == 1), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$h == 2)) / sum(input_data$h == 2), col=2, lty=1)

############################################################
# Grouped logistic regression
############################################################

fit <- stan(file='grouped_logistic_regression.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

params = extract(fit)

# Plot marginal posteriors
par(mfrow=c(2, 4))

hist(params$beta[,1], main="", xlab="beta[1]")
abline(v=2.5,col=2,lty=1)

hist(params$beta[,2], main="", xlab="beta[2]")
abline(v=-1.5,col=2,lty=1)

hist(params$beta[,3], main="", xlab="beta[3]")
abline(v=1,col=2,lty=1)

hist(params$beta[,4], main="", xlab="beta[3]")
abline(v=-0.5,col=2,lty=1)

hist(params$beta[,5], main="", xlab="beta[3]")
abline(v=-3,col=2,lty=1)

hist(params$alpha[,1], main="", xlab="alpha[1]")
abline(v=1,col=2,lty=1)

hist(params$alpha[,2], main="", xlab="alpha[2]")
abline(v=-1,col=2,lty=1)

# Now both the aggregate fit
hist(params$p_hat_ppc, main="", xlab="p_ppc")
abline(v=sum(input_data$y) / input_data$N, col=2, lty=1)

# And individual fits look good
p_left <- hist(params$p_hat_left_ppc, breaks=0.01*(0:100)+0.0)
p_left$counts = p_left$counts / sum(p_left$counts)

p_right <- hist(params$p_hat_right_ppc, breaks=0.01*(0:100)+0.0)
p_right$counts = p_right$counts / sum(p_right$counts)

plot(p_left, main="", xlab="p_hat")
plot(p_right, add=T)
abline(v=sum(input_data$y * (input_data$h == 1)) / sum(input_data$h == 1), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$h == 2)) / sum(input_data$h == 2), col=2, lty=1)

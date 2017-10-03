############################################################
# Initial setup
############################################################

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('stan_utility.R')

############################################################
#    A simple logistic regression
############################################################

input_data <- read_rdump('hierarchical_logistic_regression.data.R')
fit <- stan(file='logistic_regression.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

# Now let's look at the PPCs
params = extract(fit)

# Aggregate fit looks okay, but...
hist(params$p_ppc, main="", xlab="p_ppc")
abline(v=sum(input_data$y) / input_data$N, col=2, lty=1)

# Individual groups are very different from aggregrate response
hist(params$p_ppc, main="", xlab="p_age_ppc", breaks=0.01*(0:100)+0.0)
abline(v=sum(input_data$y * (input_data$age == 1)) / sum(input_data$age == 1), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$age == 2)) / sum(input_data$age == 2), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$age == 3)) / sum(input_data$age == 3), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$age == 4)) / sum(input_data$age == 4), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$age == 5)) / sum(input_data$age == 5), col=2, lty=1)

hist(params$p_ppc, main="", xlab="p_income_ppc", breaks=0.01*(0:100)+0.0)
abline(v=sum(input_data$y * (input_data$income == 1)) / sum(input_data$income == 1), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$income == 2)) / sum(input_data$income == 2), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$income == 3)) / sum(input_data$income == 3), col=2, lty=1)
abline(v=sum(input_data$y * (input_data$income == 4)) / sum(input_data$income == 4), col=2, lty=1)

############################################################
#    One-level, centered parameterization
############################################################

fit <- stan(file='one_level_cp.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

# sigma_beta trace is no good,
traceplot(fit, pars=c("sigma_beta"))

# and divergences concentrate where the chain is getting stuck at small values of sigma_beta
partition <- partition_div(fit)
div_params <- partition[[1]]
nondiv_params <- partition[[2]]

c_dark_trans <- c("#8F272780")
c_green_trans <- c("#00FF0080")

par(mar = c(4, 4, 0.5, 0.5))
plot(nondiv_params$"beta_income[1]", log(nondiv_params$sigma_beta),
     col=c_dark_trans, pch=16, cex=0.8, xlab="beta_income[1]", ylab="log(sigma_beta)")
points(div_params$"beta_income[1]", log(div_params$sigma_beta),
       col=c_green_trans, pch=16, cex=0.8)

# Any better with a higher adapt_delta? Not much
fit <- stan(file='one_level_cp.stan', data=input_data, seed=4938483,
            control=list(adapt_delta=0.99))

check_div(fit)

partition <- partition_div(fit)
div_params <- partition[[1]]
nondiv_params <- partition[[2]]

par(mar = c(4, 4, 0.5, 0.5))
plot(nondiv_params$"beta_income[1]", log(nondiv_params$sigma_beta),
     col=c_dark_trans, pch=16, cex=0.8, xlab="beta_income[1]", ylab="log(sigma_beta)")
points(div_params$"beta_income[1]", log(div_params$sigma_beta),
       col=c_green_trans, pch=16, cex=0.8)

############################################################
#    One-level, non-centered parameterization
############################################################

fit <- stan(file='one_level_ncp.stan', data=input_data, seed=4938483,
            control=list(adapt_delta=0.85))

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

# _And_ the PPCs look great
params_ncp = extract(fit)

par(mfrow=c(2, 2))

hist(params_ncp$p_income_ppc[,1], main="Income = 1", xlab="p_income_ppc",
     breaks=0.04*(0:25), col=rgb(1, 0, 0, 0.25))
abline(v=sum(input_data$y * (input_data$income == 1)) / sum(input_data$income == 1), col=2, lty=1)

hist(params_ncp$p_income_ppc[,2], main="Income = 2", xlab="p_income_ppc",
     breaks=0.04*(0:25), col=rgb(1, 0, 0, 0.25))
abline(v=sum(input_data$y * (input_data$income == 2)) / sum(input_data$income == 2), col=2, lty=1)

hist(params_ncp$p_income_ppc[,3], main="Income = 3", xlab="p_income_ppc",
     breaks=0.04*(0:25), col=rgb(1, 0, 0, 0.25))
abline(v=sum(input_data$y * (input_data$income == 3)) / sum(input_data$income == 3), col=2, lty=1)

hist(params_ncp$p_income_ppc[,4], main="Income = 4", xlab="p_income_ppc",
     breaks=0.04*(0:25), col=rgb(1, 0, 0, 0.25))
abline(v=sum(input_data$y * (input_data$income == 4)) / sum(input_data$income == 4), col=2, lty=1)

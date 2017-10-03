############################################################
# Initial setup
############################################################

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('stan_utility.R')

############################################################
#    Multi-level, non-centered (no interactions)
############################################################

input_data <- read_rdump('vote.data')
fit <- stan(file='hier_voting.stan', data=input_data, seed=4938483)

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

# Let's remove those divergences
fit <- stan(file='hier_voting.stan', data=input_data, seed=4938483,
            control=list(adapt_delta=0.95))
check_div(fit)

# Now we can check the PPCs
params = extract(fit)

par(mfrow=c(3, 2))

hist(params$p_state_ppc[,1], main="State = 1", xlab="p_state_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$state == 1)) / sum(input_data$state == 1), col=2, lty=1)

hist(params$p_eth_ppc[,2], main="Ethnicity = 2", xlab="p_eth_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth == 2)) / sum(input_data$eth == 2), col=2, lty=1)

hist(params$p_inc_ppc[,1], main="Income = 1", xlab="p_inc_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$inc == 1)) / sum(input_data$inc == 1), col=2, lty=1)

hist(params$p_age_ppc[,2], main="Age = 2", xlab="p_age_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$age == 2)) / sum(input_data$age == 2), col=2, lty=1)

hist(params$p_sex_ppc[,1], main="Sex = 1", xlab="p_sex_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$sex == 1)) / sum(input_data$sex == 1), col=2, lty=1)

hist(params$p_edu_ppc[,2], main="Education = 2", xlab="p_edu_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$edu == 2)) / sum(input_data$edu == 2), col=2, lty=1)

# Ethnicities
par(mfrow=c(2, 2))

hist(params$p_eth_ppc[,1], main="Ethnicity = 1", xlab="p_eth_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth == 1)) / sum(input_data$eth == 1), col=2, lty=1)

hist(params$p_eth_ppc[,2], main="Ethnicity = 2", xlab="p_eth_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth == 2)) / sum(input_data$eth == 2), col=2, lty=1)

hist(params$p_eth_ppc[,3], main="Ethnicity = 3", xlab="p_eth_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth == 3)) / sum(input_data$eth == 3), col=2, lty=1)

hist(params$p_eth_ppc[,4], main="Ethnicity = 4", xlab="p_eth_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth == 4)) / sum(input_data$eth == 4), col=2, lty=1)

############################################################
#    Multi-level, non-centered
#   (interactions only in PPCs)
############################################################

input_data <- read_rdump('vote.inter.data')
fit <- stan(file='hier_voting_inter_ppc.stan', data=input_data, seed=4938483,
            control=list(adapt_delta=0.95))

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

# Check interaction PPCs
params = extract(fit)

par(mfrow=c(2, 2))

hist(params$p_eth_inc_ppc[,1], main="Ethnicity X Income = 1", xlab="p_eth_inc_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_inc == 1)) / sum(input_data$eth_inc == 1), col=2, lty=1)

hist(params$p_eth_age_ppc[,2], main="Ethnicity X Age = 2", xlab="p_eth_age", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_age == 2)) / sum(input_data$eth_age == 2), col=2, lty=1)

hist(params$p_eth_sex_ppc[,1], main="Ethnicity X Sex = 1", xlab="p_eth_sex_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_sex == 1)) / sum(input_data$eth_sex == 1), col=2, lty=1)

hist(params$p_eth_edu_ppc[,2], main="Ethnicity X Education = 2", xlab="p_eth_edu_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_edu == 2)) / sum(input_data$eth_edu == 2), col=2, lty=1)

############################################################
#    Multi-level, non-centered (interactions)
############################################################

fit <- stan(file='hier_voting_inter.stan', data=input_data, seed=4938483,
            control=list(adapt_delta=0.99))

# Check diagnostics
print(fit)
check_treedepth(fit)
check_energy(fit)
check_div(fit)

pairs(fit, pars=c("sigma_alpha[1]", "alpha_state[1]", "sigma_alpha[2]","alpha_eth[1]", "sigma_alpha[7]","alpha_state_eth[1]"))

params = extract(fit)

par(mfrow=c(2, 2))

hist(params$p_eth_inc_ppc[,1], main="Ethnicity X Income = 1", xlab="p_eth_inc_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_inc == 1)) / sum(input_data$eth_inc == 1), col=2, lty=1)

hist(params$p_eth_age_ppc[,2], main="Ethnicity X Age = 2", xlab="p_eth_age", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_age == 2)) / sum(input_data$eth_age == 2), col=2, lty=1)

hist(params$p_eth_sex_ppc[,1], main="Ethnicity X Sex = 1", xlab="p_eth_sex_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_sex == 1)) / sum(input_data$eth_sex == 1), col=2, lty=1)

hist(params$p_eth_edu_ppc[,2], main="Ethnicity X Education = 2", xlab="p_eth_edu_ppc", breaks=0.025*(0:40))
abline(v=sum(input_data$y * (input_data$eth_edu == 2)) / sum(input_data$eth_edu == 2), col=2, lty=1)

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
# Read in data
############################################################
input_data <- read_rdump("hierarchical_logistic_regression.data.R")
str(input_data)

#############################################
#Run Stan
############################################

fit<-stan(file = 'one_level_ncp.stan', data = input_data, seed=4938483)
params<-extract(fit)

############################################
#Evaluation
############################################

# Diagnostics
print(fit)

check_treedepth(fit)
check_energy(fit)
check_div(fit)

div <- partition_div(fit)

fit_2<-stan(file = 'one_level_cp.stan', data = input_data, seed=4938483, control = list(adapt_delta=0.99))




library(dplyr)

div2 = bind_rows(div[1], div[2])
div2 <- div2 %>% mutate(divergent = c(rep(1,53), rep(0, 4000-53)))

library(ggplot2)
ggplot(div2, aes(x=mu_alpha, y = sigma_alpha, fill=divergent))+geom_point()                        

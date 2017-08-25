###############################################
##Load rstan
###############################################

library(rstan)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('C:/Users/jlandesman/Documents/Probabilistic_Programming/data/stan_utility.R')

##############################################
#Load data
##############################################

input_data <- read_rdump("grouped_logistic_regression.data.R")
str(input_data)

#############################################
#Run Stan
############################################

fit<-stan(file = 'grouped_logistic_regression.stan', data = input_data, seed=4938483)
params<-extract(fit)
str(params)
############################################
#Evaluation
############################################

# Diagnostics
print(fit)

check_treedepth(fit)
check_energy(fit)
check_div(fit)

## Compare responses
library(dplyr)

response = data.frame(hand = input_data$hand, y = input_data$y)
response = tbl_df(response)

# Overall
mean(response$y)
mean(params$p_hat_ppc)

#Left vs right
comparison <- response %>% group_by(hand) %>% summarize(true = mean(y)) %>% ungroup
comparison %>% mutate(model = 
                        c(mean(params$p_hat_left_ppc), 
                          mean(params$p_hat_right_ppc)))

left_hist<-hist(params$p_hat_left_ppc, breaks = 50, col='light blue')
right_hist<-hist(params$p_hat_right_ppc, breaks = 50, col='red')
plot(left_hist, col='red')
plot(right_hist, col='light blue')

hist_plot = data.frame(left = params$p_hat_left_ppc, right = params$p_hat_right_ppc)
hist_plot = tidyr::gather(hist_plot)

## Looks good!
library(ggplot2)
ggplot(hist_plot, aes(x=value, fill = key)) + 
  geom_density(alpha=0.4) + 
  geom_vline(aes(xintercept = comparison %>% filter(hand==1) %>% select(true)))+
  geom_vline(aes(xintercept = comparison %>% filter(hand==2) %>% select(true)))


y_ppcs <- as.matrix(fit, pars = "p_hat_ppc")
ppc_dens_overlay(
  y = input_data$y,
  yrep = y_ppcs[sample(nrow(y_ppcs), 50),]
)
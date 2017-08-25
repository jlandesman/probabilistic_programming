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
library(dplyr)
set.seed(100)

input_data <- read_rdump("logistic_regression.data.R")

# data <-tbl_df(t(as.matrix(input_data$X)))
# data<- data %>% mutate(y = t(input_data$y))
# train <- sample_frac(data, size = 0.5)
# test <- setdiff(data, train)
# 
# train_data <- list(x=t((train[,1:4])), y=list(train$y), M=5, n=nrow(train))
# test_data <- list(x=as.matrix(test[,1:4]), y=list(test$y), M=5, n=nrow(test))


input_data$N_in<-250
input_data$N_out<-250
input_data$X_in<-input_data$X[,1:250]
input_data$X_out<-input_data$X[,251:500]
input_data$y_in<-input_data$y[1:250]
input_data$y_out<-input_data$y[251:500]

fit   <- stan(file='logistic_regression1.stan', data=input_data, seed=4938483)
fit_1 <- stan(file='logistic_regression1.stan', data=train_data, seed=4938438)
fit_2 <- stan(file='logistic_regression1.stan', data=input_data, seed=4938483)

print(fit_2)
params <- extract(fit_2)

phat_in<-hist(params$p_hat_ppc_in, breaks=50, col='light blue')
phat_out<-hist(params$p_hat_ppc_out, breaks=50, col='red')
plot(phat_in, col='light blue')
plot(phat_out, add=T, col='red')


# Check diagnostics
print(fit_2)

mcmc_recover_hist(
  as.matrix(fit, pars = c("sigma", "alpha", "beta")),
  true = c(1, 10, 5, -3, 2),
  facet_args = list(ncol = 2)
)


par(mfrow = c(1,1))
hist(params$p_hat_ppc, breaks = 100)
abline(v=mean(input_data$y), col='red')

ggplot(as.data.frame(params), aes(x=p_hat_ppc_in)) + 
  geom_density(aes(alpha=0.5), fill="light blue") + 
  geom_vline(xintercept = mean(input_data$y_in), col='red')


data{
  int<lower=1> N;
  int<lower=1> N_income;
  int<lower=1, upper=N_income> income[N];
  
  int<lower=1> N_age;
  int<lower=1, upper=N_age> age[N]
  
  vector[N] x;
  int y[N];
}

parameters{
  real mu_alpha; // population parametres
  real mu_beta; 
  
  // Hierarchical  model for slope and intercept
  
  real<lower=0> sigma_alpha_income;
  vector[N_income] alpha_income;
  
  real<lower=0> sigma_beta_income;
  vector[N_income] beta_income;
  
  real<lower=0> sigma_alpha_age;
  vector[N_age] alpha_age;
  
  real<lower=0> sigma_beta_age;
  vector[N_age] beta_age;
}

model{
  mu_alpha ~ normal(0,2.5);
  sigma_alpha_income ~ normal(0,2.5);
  alpha_income ~ normal(mu_alpha, sigma_alpha_income);
  
  sigma_alpha_age ~ normal(0,2.5);
  alpha_age ~ normal(mu_alpha, sigma_alpha_age)
  
  mu_beta ~ normal(0, 2.5);
  sigma_beta_income ~ normal(0, 2.5);
  beta_income ~ normal(mu_beta, sigma_beta_income);
 
  sigma_beta_age ~ normal(0,2.5);
  beta_age ~ normal(mu_beta, sigma_beta_age)
  
  y~bernoulli_logit(beta_income[income] .* x + beta_age[age] .* x + alpha_income[income] + alpha_age[age]);
}

generated quantities{
  real p_ppc = 0;
  vector[N_income] p_income_ppc = rep_vector(0, N_income);
  vector[N_age] p_age = rep_vector(0, N_age);
  {
    vector[N_income] income_counts = rep_vector(0, N_income);
    
    for (n in 1:N){
      real logit_theta = beta_income[income[n]] * x[n] + alpha_income[income[n]];

      if (bernoulli_logit_rng(logit_theta)){
        p_ppc = p_ppc + 1;
        p_income_ppc[income[n]] = p_income_ppc[income[n]] + 1;
      }
      
      income_counts[income[n]] = income_counts[income[n]] + 1;
    }
  p_ppc = p_ppc/N;
  p_income_ppc = p_income_ppc ./ income_counts;
  
  }  
}    
  
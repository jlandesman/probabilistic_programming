data{
  int<lower=1> N;
  int<lower=1> N_income;
  int<lower=1, upper=N_income> income[N];
  
  vector[N] x;
  int y[N];
}

parameters{
  real mu_alpha; // population parametres
  real mu_beta; 
  
  // Hierarchical  model for slope and intercept
  
  real<lower=0> sigma_alpha;
  vector[N_income] alpha_income_tilde;
  
  real<lower=0> sigma_beta;
  vector[N_income] beta_income_tilde;
}

transformed parameters{
  vector[N_income] alpha_income = mu_alpha +  sigma_alpha * alpha_income_tilde;
  vector[N_income] beta_income  = mu_beta +  sigma_beta * beta_income_tilde;
}

model{
  mu_alpha ~ normal(0,2.5);
  sigma_alpha ~ normal(0,2.5);
  alpha_income_tilde ~ normal(0, 1);
  
  mu_beta ~ normal(0, 2.5);
  sigma_beta ~ normal(0, 2.5);
  beta_income_tilde ~ normal(0, 1);
  
  y~bernoulli_logit(beta_income[income] .* x + alpha_income[income]);
}

generated quantities{
  real p_ppc = 0;
  vector[N_income] p_income_ppc = rep_vector(0, N_income);
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

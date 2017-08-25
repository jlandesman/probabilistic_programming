data{
  int<lower=1> N_in;
  int<lower=1> N_out;
  int<lower=1> M;
  matrix [M, N_in] X_in;
  matrix [M, N_out] X_out;
  int y_in[N_in];
}

parameters{
  vector[M] beta;
  real alpha;
}

model{
  vector[N_in] theta_logit = X_in'*beta + alpha;
  
  beta ~ normal(0, 5);
  alpha ~ normal(0, 5);
  
   for (n in 1:N_in) // No curly braces necessary for one line for loop
    y_in[n] ~ bernoulli(inv_logit(theta_logit[n]));
  
}

generated quantities{
  real p_hat_ppc_in = 0;
  real p_hat_ppc_out = 0;
  
  for (n in 1:N_in){
    int y_ppc = bernoulli_rng(inv_logit(X_in[1:M, n]'*beta + alpha));
    p_hat_ppc_in = p_hat_ppc_in +  y_ppc;
  }

   for (n in 1:N_out){
    int y_ppc = bernoulli_rng(inv_logit(X_out[1:M, n]'*beta + alpha));
    p_hat_ppc_out = p_hat_ppc_out +  y_ppc;
  }

  p_hat_ppc_in = p_hat_ppc_in/N_in;
  p_hat_ppc_out = p_hat_ppc_out/N_out;

}
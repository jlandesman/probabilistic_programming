data{
  int<lower=1> N;
  int<lower=1> M;
  matrix [M,N] X;
  real y[N];
}

parameters{
  vector[M] beta;
  real alpha;
  real <lower=0> sigma;
  
}

model{
  // Everything you declare in generated quantities gets passed on!
  vector[N] mu = X' * beta + alpha;
  
//  for (m in 1:M) 
  beta ~ normal(0, 10);
  alpha ~ normal(0, 10);
  sigma ~ normal(0,5);
  
  for (n in 1:N) // No curly braces necessary for one line for loop
    y[n] ~ normal(mu[n], sigma);
}

generated quantities{
  real y_ppc[N];
  { // This open brackets mean a local scope!  Any variables defined within are only local
    vector[N] mu = X' *beta + alpha;
    for (n in 1:N)
      y_ppc[n] = normal_rng(mu[n], sigma);
  }
  
}
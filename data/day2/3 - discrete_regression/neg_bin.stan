data{
  int<lower=1> N;
  int<lower=2> M;
  matrix[M, N] X;
  int y[N];
}

parameters {
  real<lower=0> phi; // neg. binomial dispersion parameter
  real b0;  // intercept
  vector[M] b1;  // slope
}
model {
  // priors:
  phi ~ cauchy(0, 3);
  b0 ~ normal(0, 5);
  b1 ~ normal(0, 5);

  // data model:
  y ~ neg_binomial_2_log(X' * b1 + b0, phi);
}

generated quantities{
  int y_ppc[N];
  
  for (n in 1:N)
    y_ppc[n] = neg_binomial_2_log_rng(X[1:M, n]' * b1 + b0, phi);
}

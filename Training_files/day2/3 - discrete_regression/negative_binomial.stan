data {
  int<lower=1> N;
  int<lower=1> M;
  matrix[M, N] X;
  int y[N];
}

parameters {
  vector[M] beta;
  real alpha;
  real<lower=0> phi;
}

model {
  beta ~ normal(0, 10);
  alpha ~ normal(0, 10);
  phi ~ normal(0, 5);

  y ~ neg_binomial_2_log(X' * beta + alpha, phi);
}

generated quantities {
  int y_ppc[N];
  for (n in 1:N)
    y_ppc[n] = neg_binomial_2_log_rng(X[1:M, n]' * beta + alpha, phi);
}

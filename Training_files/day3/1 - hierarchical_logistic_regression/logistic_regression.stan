data {
  int<lower=1> N;
  vector[N] x;
  int y[N];
}

parameters {
  real beta;
  real alpha;
}

model {
  beta ~ normal(0, 5);
  alpha ~ normal(0, 5);

  y ~ bernoulli_logit(beta * x + alpha);
}

generated quantities {
  real p_ppc = 0;
  
  for (n in 1:N)
    p_ppc = p_ppc + bernoulli_logit_rng(beta * x[n] + alpha);

  p_ppc = p_ppc / N;
}

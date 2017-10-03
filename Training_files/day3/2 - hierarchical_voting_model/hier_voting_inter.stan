// Hierarchical Logistic Regression
// Multilevel, non-centered parameterization
// Effect interactions included in fit

data {
  int<lower=0> N; // Number of voters
  int<lower=0> M; // Number of parameters

  int y[N];       // Republican vote

  int<lower=0> N_state; // State
  int state[N];

  int<lower=0> N_eth;   // Ethnicity
  int eth[N];

  int<lower=0> N_inc;   // Income
  int inc[N];

  int<lower=0> N_age;   // Age
  int age[N];

  int<lower=0> N_sex;   // Sex
  int sex[N];

  int<lower=0> N_edu;   // Education
  int edu[N];

  // Interactions
  int<lower=0> N_state_eth;
  int state_eth[N];

  int<lower=0> N_state_inc;
  int state_inc[N];

  int<lower=0> N_state_age;
  int state_age[N];

  int<lower=0> N_state_sex;
  int state_sex[N];

  int<lower=0> N_state_edu;
  int state_edu[N];

  int<lower=0> N_eth_inc;
  int eth_inc[N];

  int<lower=0> N_eth_age;
  int eth_age[N];

  int<lower=0> N_eth_sex;
  int eth_sex[N];

  int<lower=0> N_eth_edu;
  int eth_edu[N];

  int<lower=0> N_inc_age;
  int inc_age[N];

  int<lower=0> N_inc_sex;
  int inc_sex[N];

  int<lower=0> N_inc_edu;
  int inc_edu[N];

  int<lower=0> N_age_sex;
  int age_sex[N];

  int<lower=0> N_age_edu;
  int age_edu[N];

  int<lower=0> N_sex_edu;
  int sex_edu[N];

}

parameters {
  real alpha;
  vector[N_state] alpha_state;
  vector[N_eth] alpha_eth;
  vector[N_inc] alpha_inc;
  vector[N_age] alpha_age;
  vector[N_sex] alpha_sex;
  vector[N_edu] alpha_edu;

  vector[N_state_eth] alpha_state_eth;
  vector[N_state_inc] alpha_state_inc;
  vector[N_state_age] alpha_state_age;
  vector[N_state_sex] alpha_state_sex;
  vector[N_state_edu] alpha_state_edu;
  vector[N_eth_inc] alpha_eth_inc;
  vector[N_eth_age] alpha_eth_age;
  vector[N_eth_sex] alpha_eth_sex;
  vector[N_eth_edu] alpha_eth_edu;
  vector[N_inc_age] alpha_inc_age;
  vector[N_inc_sex] alpha_inc_sex;
  vector[N_inc_edu] alpha_inc_edu;
  vector[N_age_sex] alpha_age_sex;
  vector[N_age_edu] alpha_age_edu;
  vector[N_sex_edu] alpha_sex_edu;

  real<lower=0> sigma_alpha[M];    // Local parameter standard deviation
  real<lower=0> sigma_sigma_alpha; // Global standard deviation
}

model {
  sigma_sigma_alpha ~ normal(0, 2.5);
  sigma_alpha ~ normal(0, 1);

  alpha_state ~ normal(0, 1);
  alpha_eth ~ normal(0, 1);
  alpha_inc ~ normal(0, 1);
  alpha_age ~ normal(0, 1);
  alpha_sex ~ normal(0, 1);
  alpha_edu ~ normal(0, 1);
  alpha_state_eth ~ normal(0, 1);
  alpha_state_inc ~ normal(0, 1);
  alpha_state_age ~ normal(0, 1);
  alpha_state_sex ~ normal(0, 1);
  alpha_state_edu ~ normal(0, 1);
  alpha_eth_inc ~ normal(0, 1);
  alpha_eth_age ~ normal(0, 1);
  alpha_eth_sex ~ normal(0, 1);
  alpha_eth_edu ~ normal(0, 1);
  alpha_inc_age ~ normal(0, 1);
  alpha_inc_sex ~ normal(0, 1);
  alpha_inc_edu ~ normal(0, 1);
  alpha_age_sex ~ normal(0, 1);
  alpha_age_edu ~ normal(0, 1);
  alpha_sex_edu ~ normal(0, 1);

  y ~ bernoulli_logit(  alpha
                      + alpha_state[state]         * sigma_alpha[1] * sigma_sigma_alpha
                      + alpha_eth[eth]             * sigma_alpha[2] * sigma_sigma_alpha
                      + alpha_inc[inc]             * sigma_alpha[3] * sigma_sigma_alpha
                      + alpha_age[age]             * sigma_alpha[4] * sigma_sigma_alpha
                      + alpha_sex[sex]             * sigma_alpha[5] * sigma_sigma_alpha
                      + alpha_edu[edu]             * sigma_alpha[6] * sigma_sigma_alpha
                      + alpha_state_eth[state_eth] * sigma_alpha[7] * sigma_sigma_alpha
                      + alpha_state_inc[state_inc] * sigma_alpha[8] * sigma_sigma_alpha
                      + alpha_state_age[state_age] * sigma_alpha[9] * sigma_sigma_alpha
                      + alpha_state_sex[state_sex] * sigma_alpha[10] * sigma_sigma_alpha
                      + alpha_state_edu[state_edu] * sigma_alpha[11] * sigma_sigma_alpha
                      + alpha_eth_inc[eth_inc]     * sigma_alpha[12] * sigma_sigma_alpha
                      + alpha_eth_age[eth_age]     * sigma_alpha[13] * sigma_sigma_alpha
                      + alpha_eth_sex[eth_sex]     * sigma_alpha[14] * sigma_sigma_alpha
                      + alpha_eth_edu[eth_edu]     * sigma_alpha[15] * sigma_sigma_alpha
                      + alpha_inc_age[inc_age]     * sigma_alpha[16] * sigma_sigma_alpha
                      + alpha_inc_sex[inc_sex]     * sigma_alpha[17] * sigma_sigma_alpha
                      + alpha_inc_edu[inc_edu]     * sigma_alpha[18] * sigma_sigma_alpha
                      + alpha_age_sex[age_sex]     * sigma_alpha[19] * sigma_sigma_alpha
                      + alpha_age_edu[age_edu]     * sigma_alpha[20] * sigma_sigma_alpha
                      + alpha_sex_edu[sex_edu]     * sigma_alpha[21] * sigma_sigma_alpha);
}

generated quantities {
  vector[N_state] p_state_ppc = rep_vector(0, N_state);
  vector[N_eth] p_eth_ppc = rep_vector(0, N_eth);
  vector[N_inc] p_inc_ppc = rep_vector(0, N_inc);
  vector[N_age] p_age_ppc = rep_vector(0, N_age);
  vector[N_sex] p_sex_ppc = rep_vector(0, N_sex);
  vector[N_edu] p_edu_ppc = rep_vector(0, N_edu);
  vector[N_state_eth] p_state_eth_ppc = rep_vector(0, N_state_eth);
  vector[N_state_inc] p_state_inc_ppc = rep_vector(0, N_state_inc);
  vector[N_state_age] p_state_age_ppc = rep_vector(0, N_state_age);
  vector[N_state_sex] p_state_sex_ppc = rep_vector(0, N_state_sex);
  vector[N_state_edu] p_state_edu_ppc = rep_vector(0, N_state_edu);
  vector[N_eth_inc] p_eth_inc_ppc = rep_vector(0, N_eth_inc);
  vector[N_eth_age] p_eth_age_ppc = rep_vector(0, N_eth_age);
  vector[N_eth_sex] p_eth_sex_ppc = rep_vector(0, N_eth_sex);
  vector[N_eth_edu] p_eth_edu_ppc = rep_vector(0, N_eth_edu);
  vector[N_inc_age] p_inc_age_ppc = rep_vector(0, N_inc_age);
  vector[N_inc_sex] p_inc_sex_ppc = rep_vector(0, N_inc_sex);
  vector[N_inc_edu] p_inc_edu_ppc = rep_vector(0, N_inc_edu);
  vector[N_age_sex] p_age_sex_ppc = rep_vector(0, N_age_sex);
  vector[N_age_edu] p_age_edu_ppc = rep_vector(0, N_age_edu);
  vector[N_sex_edu] p_sex_edu_ppc = rep_vector(0, N_sex_edu);

  {
    vector[N_state] state_counts = rep_vector(0, N_state);
    vector[N_eth] eth_counts = rep_vector(0, N_eth);
    vector[N_inc] inc_counts = rep_vector(0, N_inc);
    vector[N_age] age_counts = rep_vector(0, N_age);
    vector[N_sex] sex_counts = rep_vector(0, N_sex);
    vector[N_edu] edu_counts = rep_vector(0, N_edu);
    vector[N_state_eth] state_eth_counts = rep_vector(0, N_state_eth);
    vector[N_state_inc] state_inc_counts = rep_vector(0, N_state_inc);
    vector[N_state_age] state_age_counts = rep_vector(0, N_state_age);
    vector[N_state_sex] state_sex_counts = rep_vector(0, N_state_sex);
    vector[N_state_edu] state_edu_counts = rep_vector(0, N_state_edu);
    vector[N_eth_inc] eth_inc_counts = rep_vector(0, N_eth_inc);
    vector[N_eth_age] eth_age_counts = rep_vector(0, N_eth_age);
    vector[N_eth_sex] eth_sex_counts = rep_vector(0, N_eth_sex);
    vector[N_eth_edu] eth_edu_counts = rep_vector(0, N_eth_edu);
    vector[N_inc_age] inc_age_counts = rep_vector(0, N_inc_age);
    vector[N_inc_sex] inc_sex_counts = rep_vector(0, N_inc_sex);
    vector[N_inc_edu] inc_edu_counts = rep_vector(0, N_inc_edu);
    vector[N_age_sex] age_sex_counts = rep_vector(0, N_age_sex);
    vector[N_age_edu] age_edu_counts = rep_vector(0, N_age_edu);
    vector[N_sex_edu] sex_edu_counts = rep_vector(0, N_sex_edu);

    for (n in 1:N) {
      real voter_alpha_state = alpha_state[state[n]];
      real voter_alpha_eth = alpha_eth[eth[n]];
      real voter_alpha_inc = alpha_inc[inc[n]];
      real voter_alpha_age = alpha_age[age[n]];
      real voter_alpha_sex = alpha_sex[sex[n]];
      real voter_alpha_edu = alpha_edu[edu[n]];
      real logit_theta =  alpha
                        + alpha_state[state[n]]         * sigma_alpha[1] * sigma_sigma_alpha
                        + alpha_eth[eth[n]]             * sigma_alpha[2] * sigma_sigma_alpha
                        + alpha_inc[inc[n]]             * sigma_alpha[3] * sigma_sigma_alpha
                        + alpha_age[age[n]]             * sigma_alpha[4] * sigma_sigma_alpha
                        + alpha_sex[sex[n]]             * sigma_alpha[5] * sigma_sigma_alpha
                        + alpha_edu[edu[n]]             * sigma_alpha[6] * sigma_sigma_alpha
                        + alpha_state_eth[state_eth[n]] * sigma_alpha[7] * sigma_sigma_alpha
                        + alpha_state_inc[state_inc[n]] * sigma_alpha[8] * sigma_sigma_alpha
                        + alpha_state_age[state_age[n]] * sigma_alpha[9] * sigma_sigma_alpha
                        + alpha_state_sex[state_sex[n]] * sigma_alpha[10] * sigma_sigma_alpha
                        + alpha_state_edu[state_edu[n]] * sigma_alpha[11] * sigma_sigma_alpha
                        + alpha_eth_inc[eth_inc[n]]     * sigma_alpha[12] * sigma_sigma_alpha
                        + alpha_eth_age[eth_age[n]]     * sigma_alpha[13] * sigma_sigma_alpha
                        + alpha_eth_sex[eth_sex[n]]     * sigma_alpha[14] * sigma_sigma_alpha
                        + alpha_eth_edu[eth_edu[n]]     * sigma_alpha[15] * sigma_sigma_alpha
                        + alpha_inc_age[inc_age[n]]     * sigma_alpha[16] * sigma_sigma_alpha
                        + alpha_inc_sex[inc_sex[n]]     * sigma_alpha[17] * sigma_sigma_alpha
                        + alpha_inc_edu[inc_edu[n]]     * sigma_alpha[18] * sigma_sigma_alpha
                        + alpha_age_sex[age_sex[n]]     * sigma_alpha[19] * sigma_sigma_alpha
                        + alpha_age_edu[age_edu[n]]     * sigma_alpha[20] * sigma_sigma_alpha
                        + alpha_sex_edu[sex_edu[n]]     * sigma_alpha[21] * sigma_sigma_alpha;

      if (bernoulli_logit_rng(logit_theta)) {
        p_state_ppc[state[n]] = p_state_ppc[state[n]] + 1;
        p_eth_ppc[eth[n]] = p_eth_ppc[eth[n]] + 1;
        p_inc_ppc[inc[n]] = p_inc_ppc[inc[n]] + 1;
        p_age_ppc[age[n]] = p_age_ppc[age[n]] + 1;
        p_sex_ppc[sex[n]] = p_sex_ppc[sex[n]] + 1;
        p_edu_ppc[edu[n]] = p_edu_ppc[edu[n]] + 1;
        p_state_eth_ppc[state_eth[n]] = p_state_eth_ppc[state_eth[n]] + 1;
        p_state_inc_ppc[state_inc[n]] = p_state_inc_ppc[state_inc[n]] + 1;
        p_state_age_ppc[state_age[n]] = p_state_age_ppc[state_age[n]] + 1;
        p_state_sex_ppc[state_sex[n]] = p_state_sex_ppc[state_sex[n]] + 1;
        p_state_edu_ppc[state_edu[n]] = p_state_edu_ppc[state_edu[n]] + 1;
        p_eth_inc_ppc[eth_inc[n]] = p_eth_inc_ppc[eth_inc[n]] + 1;
        p_eth_age_ppc[eth_age[n]] = p_eth_age_ppc[eth_age[n]] + 1;
        p_eth_sex_ppc[eth_sex[n]] = p_eth_sex_ppc[eth_sex[n]] + 1;
        p_eth_edu_ppc[eth_edu[n]] = p_eth_edu_ppc[eth_edu[n]] + 1;
        p_inc_age_ppc[inc_age[n]] = p_inc_age_ppc[inc_age[n]] + 1;
        p_inc_sex_ppc[inc_sex[n]] = p_inc_sex_ppc[inc_sex[n]] + 1;
        p_inc_edu_ppc[inc_edu[n]] = p_inc_edu_ppc[inc_edu[n]] + 1;
        p_age_sex_ppc[age_sex[n]] = p_age_sex_ppc[age_sex[n]] + 1;
        p_age_edu_ppc[age_edu[n]] = p_age_edu_ppc[age_edu[n]] + 1;
        p_sex_edu_ppc[sex_edu[n]] = p_sex_edu_ppc[sex_edu[n]] + 1;
      }

      state_counts[state[n]] = state_counts[state[n]] + 1;
      eth_counts[eth[n]] = eth_counts[eth[n]] + 1;
      inc_counts[inc[n]] = inc_counts[inc[n]] + 1;
      age_counts[age[n]] = age_counts[age[n]] + 1;
      sex_counts[sex[n]] = sex_counts[sex[n]] + 1;
      edu_counts[edu[n]] = edu_counts[edu[n]] + 1;
      state_eth_counts[state_eth[n]] = state_eth_counts[state_eth[n]] + 1;
      state_inc_counts[state_inc[n]] = state_inc_counts[state_inc[n]] + 1;
      state_age_counts[state_age[n]] = state_age_counts[state_age[n]] + 1;
      state_sex_counts[state_sex[n]] = state_sex_counts[state_sex[n]] + 1;
      state_edu_counts[state_edu[n]] = state_edu_counts[state_edu[n]] + 1;
      eth_inc_counts[eth_inc[n]] = eth_inc_counts[eth_inc[n]] + 1;
      eth_age_counts[eth_age[n]] = eth_age_counts[eth_age[n]] + 1;
      eth_sex_counts[eth_sex[n]] = eth_sex_counts[eth_sex[n]] + 1;
      eth_edu_counts[eth_edu[n]] = eth_edu_counts[eth_edu[n]] + 1;
      inc_age_counts[inc_age[n]] = inc_age_counts[inc_age[n]] + 1;
      inc_sex_counts[inc_sex[n]] = inc_sex_counts[inc_sex[n]] + 1;
      inc_edu_counts[inc_edu[n]] = inc_edu_counts[inc_edu[n]] + 1;
      age_sex_counts[age_sex[n]] = age_sex_counts[age_sex[n]] + 1;
      age_edu_counts[age_edu[n]] = age_edu_counts[age_edu[n]] + 1;
      sex_edu_counts[sex_edu[n]] = sex_edu_counts[sex_edu[n]] + 1;
    }

    for (n in 1:N_state)
      if (state_counts[n] == 0) state_counts[n] = 1;

    for (n in 1:N_state_eth)
      if (state_eth_counts[n] == 0) state_eth_counts[n] = 1;

    for (n in 1:N_state_inc)
      if (state_inc_counts[n] == 0) state_inc_counts[n] = 1;

    for (n in 1:N_state_age)
      if (state_age_counts[n] == 0) state_age_counts[n] = 1;

    for (n in 1:N_state_sex)
      if (state_sex_counts[n] == 0) state_sex_counts[n] = 1;

    for (n in 1:N_state_edu)
      if (state_edu_counts[n] == 0) state_edu_counts[n] = 1;

    for (n in 1:N_eth_inc)
      if (eth_inc_counts[n] == 0) eth_inc_counts[n] = 1;

    p_state_ppc = p_state_ppc ./ state_counts;
    p_eth_ppc = p_eth_ppc ./ eth_counts;
    p_inc_ppc = p_inc_ppc ./ inc_counts;
    p_age_ppc = p_age_ppc ./ age_counts;
    p_sex_ppc = p_sex_ppc ./ sex_counts;
    p_edu_ppc = p_edu_ppc ./ edu_counts;
    p_state_eth_ppc = p_state_eth_ppc ./ state_eth_counts;
    p_state_inc_ppc = p_state_inc_ppc ./ state_inc_counts;
    p_state_age_ppc = p_state_age_ppc ./ state_age_counts;
    p_state_sex_ppc = p_state_sex_ppc ./ state_sex_counts;
    p_state_edu_ppc = p_state_edu_ppc ./ state_edu_counts;
    p_eth_inc_ppc = p_eth_inc_ppc ./ eth_inc_counts;
    p_eth_age_ppc = p_eth_age_ppc ./ eth_age_counts;
    p_eth_sex_ppc = p_eth_sex_ppc ./ eth_sex_counts;
    p_eth_edu_ppc = p_eth_edu_ppc ./ eth_edu_counts;
    p_inc_age_ppc = p_inc_age_ppc ./ inc_age_counts;
    p_inc_sex_ppc = p_inc_sex_ppc ./ inc_sex_counts;
    p_inc_edu_ppc = p_inc_edu_ppc ./ inc_edu_counts;
    p_age_sex_ppc = p_age_sex_ppc ./ age_sex_counts;
    p_age_edu_ppc = p_age_edu_ppc ./ age_edu_counts;
    p_sex_edu_ppc = p_sex_edu_ppc ./ sex_edu_counts;
  }
}
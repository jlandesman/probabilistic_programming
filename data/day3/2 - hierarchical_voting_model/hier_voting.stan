// Hierarchical Logistic Regression
// Multilevel, non-centered parameterization

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
}

parameters {
}

model {
}

generated quantities {
  vector[N_state] p_state_ppc = rep_vector(0, N_state);
  vector[N_eth] p_eth_ppc = rep_vector(0, N_eth);
  vector[N_inc] p_inc_ppc = rep_vector(0, N_inc);
  vector[N_age] p_age_ppc = rep_vector(0, N_age);
  vector[N_sex] p_sex_ppc = rep_vector(0, N_sex);
  vector[N_edu] p_edu_ppc = rep_vector(0, N_edu);

  {
    vector[N_state] state_counts = rep_vector(0, N_state);
    vector[N_eth] eth_counts = rep_vector(0, N_eth);
    vector[N_inc] inc_counts = rep_vector(0, N_inc);
    vector[N_age] age_counts = rep_vector(0, N_age);
    vector[N_sex] sex_counts = rep_vector(0, N_sex);
    vector[N_edu] edu_counts = rep_vector(0, N_edu);

    for (n in 1:N) {
      real voter_alpha_state = alpha_state[state[n]];
      real voter_alpha_eth = alpha_eth[eth[n]];
      real voter_alpha_inc = alpha_inc[inc[n]];
      real voter_alpha_age = alpha_age[age[n]];
      real voter_alpha_sex = alpha_sex[sex[n]];
      real voter_alpha_edu = alpha_edu[edu[n]];
      real logit_theta =  alpha
                        + voter_alpha_state * sigma_alpha[1] * sigma_sigma_alpha
                        + voter_alpha_eth * sigma_alpha[2] * sigma_sigma_alpha
                        + voter_alpha_inc * sigma_alpha[3] * sigma_sigma_alpha
                        + voter_alpha_age * sigma_alpha[4] * sigma_sigma_alpha
                        + voter_alpha_sex * sigma_alpha[5] * sigma_sigma_alpha
                        + voter_alpha_edu * sigma_alpha[6] * sigma_sigma_alpha;

      if (bernoulli_logit_rng(logit_theta)) {
        p_state_ppc[state[n]] = p_state_ppc[state[n]] + 1;
        p_eth_ppc[eth[n]] = p_eth_ppc[eth[n]] + 1;
        p_inc_ppc[inc[n]] = p_inc_ppc[inc[n]] + 1;
        p_age_ppc[age[n]] = p_age_ppc[age[n]] + 1;
        p_sex_ppc[sex[n]] = p_sex_ppc[sex[n]] + 1;
        p_edu_ppc[edu[n]] = p_edu_ppc[edu[n]] + 1;
      }

      state_counts[state[n]] = state_counts[state[n]] + 1;
      eth_counts[eth[n]] = eth_counts[eth[n]] + 1;
      inc_counts[inc[n]] = inc_counts[inc[n]] + 1;
      age_counts[age[n]] = age_counts[age[n]] + 1;
      sex_counts[sex[n]] = sex_counts[sex[n]] + 1;
      edu_counts[edu[n]] = edu_counts[edu[n]] + 1;
    }

    for (n in 1:N_state)
      if (state_counts[n] == 0) state_counts[n] = 1;

    p_state_ppc = p_state_ppc ./ state_counts;
    p_eth_ppc = p_eth_ppc ./ eth_counts;
    p_inc_ppc = p_inc_ppc ./ inc_counts;
    p_age_ppc = p_age_ppc ./ age_counts;
    p_sex_ppc = p_sex_ppc ./ sex_counts;
    p_edu_ppc = p_edu_ppc ./ edu_counts;
  }

}

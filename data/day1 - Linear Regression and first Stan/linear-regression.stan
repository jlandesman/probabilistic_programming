data {
  //Dimensions
  int<lower=1> N; // N = number of observations in my dataset.  Minimum bound is 1.
  int<lower=1> K; //K = Number of covariates in the model, excluding the intercept
  
  //Variables
  matrix[N,K] X;
  vector[N] y;
}

parameters{    

// K+2 parameters.  
//Under the hood it expects all parameters to be unbound.  
//Any constraints have to go in there. If its a correlation matrix, have to contrain lower to -1, upper to +1 etc

  real alpha; // intercept
  
  // Coefficients.  Said earlier that there are K of them. K must be declared in the data block.
  vector[K] beta;
  
  // Simplex type = all covariate betas have to add up to one. other types include covariance matrixes etc
  
  real<lower=0> sigma; // error standard deviation.  Constraint is built in
  
  
}

model{
  // Statements defining the posterior density
  
  // Priors: (uniform if omitted)
  
  
  //Prior for sigma is an exponential distribution with a rate of one.  Sigma is a positive standard deviation (If an exponential is suitable, then this is fine)
  sigma ~ exponential(1); 
  
  //Prior for alpha is a normal distribution with mean 0 and standard deviation 10.
  alpha ~ normal(0,10); 
  
  for (k in 1:K) beta[k] ~ normal(0,5); // Normal 0,5 prior on each of Beta[k]
  
  //Modeling relationship betwewen Y and beta, alpha and sigma.  This is the likelihood (Y|D, beta)
  for (n in 1:N){
    y[n] ~ normal(X[n,] * beta + alpha, sigma);
  }
}

generated quantities {
  //Have model - manipulate results!  Generate predictions, evaulate probability theta>7 (or whatever) etc
  
  // Returns a result for every state in the markov chain - allows us to evaluate 
  
  vector[N] y_rep; //rep for replication. In sample (feed in X, look at in sample Y_hats's, compare)
  
  for (n in 1:N){
    real y_hat = X[n,] * beta + alpha; // temporary variable declared inside the scope of the loop
    
    // Generates a single value from the normal distributoin with the mean y_hat, and sigma = sigma.
    y_rep[n] = normal_rng(y_hat, sigma);  
    
    
  }
  
  
}

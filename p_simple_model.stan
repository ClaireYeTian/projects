data {
  int<lower=0> N;             // number of data items
  int<lower=0> J;             // number of predictors: 16
  vector[J] x[N];             // predictor matrix (NxJ)
  vector[N] y;                // univariate response vector
}

parameters {
  vector[J] beta;             // coefficients for predictors (Jx1)
  real<lower=0> sigma;        // residual standard deviation
  real alpha;                 // intercept
}

model {
  // Priors
  beta ~ normal(0, 1);        // noninformative prior for beta
  sigma ~ student_t(3, 0, 2); // prior for residual SD

  vector[N] mu;               // predicted values for response variable

  for (n in 1:N) {
    mu[n] = alpha + dot_product(beta, x[n]); // linear predictor for each observation
  }

  y ~ normal(mu, sigma);      // likelihood with normal residuals
}

generated quantities {
  vector[N] y_pred;           // predicted values for y
  vector[N] log_lik;          // log-likelihood for each observation

  for (n in 1:N) {
    y_pred[n] = normal_rng(alpha + dot_product(beta, x[n]), sigma);
    log_lik[n] = normal_lpdf(y[n] | alpha + dot_product(beta, x[n]), sigma); // log-likelihood
  }
}

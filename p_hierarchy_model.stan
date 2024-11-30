data {
  int<lower=0> N;             // number of data items
  int<lower=0> J;             // number of predictors
  int<lower=1> G;             // number of groups
  int<lower=1, upper=G> group[N];  // group index for each data item
  vector[J] x[N];             // predictor matrix (NxJ)
  vector[N] y;                // univariate response vector
}

parameters {
  // Group-level beta coefficients
  matrix[G, J] beta_raw;      // raw beta coefficients for non-centered parameterization
  vector[G] alpha_raw;        // raw intercepts for each group (non-centered)

  real<lower=0> sigma;        // residual standard deviation for y
  real mu_alpha;              // overall mean intercept
  real<lower=0> tau_alpha;    // standard deviation of group-level intercepts
  vector[J] mu_beta;          // overall mean for group-level betas
  vector<lower=0>[J] tau_beta; // standard deviation for group-level betas
}

transformed parameters {
  matrix[G, J] beta;          // actual group-level beta coefficients
  vector[G] alpha;            // actual intercepts for each group

  // Transform raw parameters to obtain actual group-level coefficients
  for (g in 1:G) {
    alpha[g] = mu_alpha + tau_alpha * alpha_raw[g];
    for (j in 1:J) {
      beta[g, j] = mu_beta[j] + tau_beta[j] * beta_raw[g, j];
    }
  }
}

model {
  // Priors for hyperparameters
  mu_alpha ~ normal(0, 2);               // prior for overall intercept mean
  tau_alpha ~ normal(0, 2);              // prior for intercept SD across groups
  mu_beta ~ normal(0, 2);                // prior for mean of beta coefficients across groups
  tau_beta ~ normal(0, 2);               // prior for SD of beta coefficients across groups
  
  // Priors for group-level raw parameters (non-centered parameterization)
  alpha_raw ~ normal(0, 1);
  to_vector(beta_raw) ~ normal(0, 1);

  // Prior for residual standard deviation
  sigma ~ student_t(3, 0, 2);

  vector[N] mu;               // predicted values for response variable

  for (n in 1:N) {
    int g = group[n];         // group index for the observation
    mu[n] = alpha[g] + dot_product(beta[g], x[n]); // group-specific linear predictor
  }

  // Likelihood
  y ~ normal(mu, sigma);      // normal likelihood with residuals
}

generated quantities {
  vector[N] y_pred;           // predicted values for y
  vector[N] log_lik;          // log-likelihood for each observation

  for (n in 1:N) {
    int g = group[n];         // group index for the observation
    y_pred[n] = normal_rng(alpha[g] + dot_product(beta[g], x[n]), sigma);
    log_lik[n] = normal_lpdf(y[n] | alpha[g] + dot_product(beta[g], x[n]), sigma); // log-likelihood
  }
}

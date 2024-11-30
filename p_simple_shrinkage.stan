data{
   int<lower=0> N;             // number of data items
  int<lower=0> J;             // number of predictors: 16
  //vector[J] x[N];             // predictor matrix (NxJ)
  matrix[N,J] x;
  vector[N] y;   
  
  
  real<lower=0> scale_icept; //prior std for the intercept
  real<lower=0> scale_global; //scale for the half-t prior for tau
  
  real<lower=1> nu_global;
  real<lower=1> nu_local;
}

parameters{
  real  logsigma;        // residual standard deviation
  real alpha;                 // intercept
  
  vector[J] z;
  real<lower=0> r1_global;
  real<lower=0> r2_global;
  
  vector<lower=0>[J] r1_local;
  vector<lower=0>[J] r2_local;
}

transformed parameters{
  real<lower=0> tau;
  vector<lower=0>[J] lambda;
  vector[J] beta;
  vector[N] mu;               // predicted values for response variable
  real sigma;
  
  sigma=exp(logsigma);
  lambda=r1_local.*sqrt(r2_local);
  tau=r1_global*sqrt(r2_global);
  beta=z .*lambda*tau;
  mu=alpha+x*beta;
  
  // for (n in 1:N) {
  //   mu[n] = alpha + dot_product(beta, x[n]); // linear predictor for each observation
  // }
}

model{
  z~normal(0,1);
  r1_local~normal(0.0,1.0);
  r2_local~inv_gamma(0.5*nu_local,0.5*nu_local);
  
  
  r1_global~normal(0.0,scale_global*sigma);
  r2_global~inv_gamma(0.5*nu_global,0.5*nu_global);
  
  alpha~ normal(0,scale_icept);
  
  y ~ normal(mu, sigma);      // likelihood with normal residuals
  
}
generated quantities {
  vector[N] y_pred;           // predicted values for y

  for (n in 1:N) {
    y_pred[n] = normal_rng(alpha + dot_product(beta, x[n]), sigma);
  }
}










data {
  int<lower=0> J; // number of schools
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J];  // standard errors of effect estimate
}

parameters {
  real mu;  //  population mean
  real<lower=0> tau;  // population sd
  vector[J] eta;  // school-level errors
}

transformed parameters {
  vector[J] theta;  // school effects
  theta = mu + tau * eta;
}

model {
  eta ~ normal(0, 1);
  y ~ normal(theta, sigma);
  tau ~ cauchy(0, 25); // half-cauchy for sigma
}

data {
  int<lower=0> J; // number of schools
  real y[J]; // estimated treatment effects
  real<lower=0> sigma[J];  // standard errors of effect estimate
  real<lower=1> nu;
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
  eta ~ student_t(nu, 0, 1);  // t distribution
  y ~ normal(theta, sigma);
}

data {
  int<lower=0> J; 
  real<lower=0> tt[J];
}
parameters {
  real<lower=0.0001> m;
  real<lower=0.0001,upper=1000000> eta;
}
model {
  for(j in 1:J)
    tt[j] ~ weibull(m, eta);
}
generated quantities{
  real<lower=0> mu;
  real<lower=0> mode;
  mu = eta * tgamma(1+(1/m));
  if(m > 1){
    mode = eta * (1-(1/m))^(1/m);
  } else {
    mode = 0;
  }
}

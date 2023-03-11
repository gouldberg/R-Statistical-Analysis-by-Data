data { 
  int<lower=0> h;
  int<lower=0> f;
  int<lower=0> MI;
  int<lower=0> CR;
}

transformed data{
  int<lower=0> s;
  int<lower=0> n;
  s = h + MI;
  n = f + CR; 
}

parameters {
  real d;
  real c;
} 

transformed parameters {
  real<lower=0,upper=1> thetah;
  real<lower=0,upper=1> thetaf;
  
    thetah = Phi(d / 2 - c);
    thetaf = Phi(-d / 2 - c);
}

model {
  d ~ normal(0, sqrt(2));
  c ~ normal(0, inv_sqrt(2));
  h ~ binomial(s, thetah);
  f ~ binomial(n, thetaf);
}

data { 
  int<lower=1> k;
  int<lower=0> h[k];
  int<lower=0> f[k];
  int<lower=0> s;
  int<lower=0> n;
}

parameters {
  vector[k] d;
  vector[k] c;
  real mud;
  real muc;
  real<lower=0> sigmad;
  real<lower=0> sigmac;
} 

transformed parameters {
  real<lower=0,upper=1> thetah[k];
  real<lower=0,upper=1> thetaf[k];
  
  for(i in 1:k) {
    thetah[i] = Phi(d[i] / 2 - c[i]);
    thetaf[i] = Phi(-d[i] / 2 - c[i]);
  }
}

model {
  mud ~ normal(0, sqrt(1000));
  muc ~ normal(0, sqrt(1000));
  
  d ~ normal(mud, sigmad);
  c ~ normal(muc, sigmac);
  h ~ binomial(s, thetah);
  f ~ binomial(n, thetaf);
}


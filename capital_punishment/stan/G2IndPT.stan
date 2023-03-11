data {
  int<lower=0>	n1;
  int<lower=0>	n2;
  real x1[n1];
  real x2[n2];
  real EQU;
  real mL;
  real mH;
  real sL;
  real sH;
}

parameters {
  vector<lower=mL,upper=mH> [2] mu;
  real<lower=sL,upper=sH> sigma1;
  real<lower=sL,upper=sH> dummy;
}

transformed parameters {
  real<lower=0>           sigma2;
  sigma2 =EQU>0.5 ? sigma1 : dummy;
}

model {
  x1 ~ normal(mu[1],sigma1);
  x2 ~ normal(mu[2],sigma2);
}

generated quantities{
  real xaste[2];
  real log_lik;
  xaste[1] = normal_rng(mu[1],sigma1);        //—\‘ª•ª•z1
  xaste[2] = normal_rng(mu[2],sigma2);        //—\‘ª•ª•z2
  log_lik = normal_lpdf(x1 | mu[1],sigma1)+ normal_lpdf(x2 | mu[2],sigma2);       //‘Î”–Ş“x
}

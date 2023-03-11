data { 
  int N;
  int T;
  int x[T];
  int S;
  real xdif[T];
}

parameters {
  real alpha;
  real<lower=0> beta;
} 

model {
      for (j in 1:T) { 
      x[j] ~ binomial(S, inv_logit(alpha + beta * xdif[j]));
    }
  }

generated quantities{
  real PSE;
  real JND;
  real p1;
  real p2;
  real U;
  PSE = -alpha / beta;
  JND = (1.098612 - alpha) / beta - PSE;
  p1 = PSE < 1.2 ? 0: 1;
  p2 = PSE > 1.8 ? 0: 1;
  U = p1 * p2;
}


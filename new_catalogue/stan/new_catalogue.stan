data { 
	int<lower=0>  N;    
	real<lower=0> x[N]; 
}

parameters {
	real                mu;
	real<lower=0>    sigma;
}

model {
	for(n in 1:N) {
		x[n] ~ normal(mu,sigma);
	}
}

generated quantities{
      real<lower=0,upper=1> mu_over;
      real<lower=0,upper=1> mu_over2;
      real                  es;
      real<lower=0,upper=1> es_over;

      mu_over  = step(mu - 2500);
      mu_over2 = step(mu - 3000);      
      es       = (mu - 2500) / sigma;   
      es_over  = step(es - 0.8);
}


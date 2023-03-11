data {                       
	int<lower=0>     N;    
	real<lower=0> x[N];    
}

parameters {                  
	real                mu; 
	real<lower=0>    sigma; 
}

model {
	for(n in 1:N){
		x[n] ~ normal(mu, sigma);
	}
}

generated quantities {        
      real                quantile_3rd; 
      real<lower=0, upper=1> prob_over;

      quantile_3rd <- mu + 0.675 * sigma;
      prob_over <- 1 - normal_cdf(quantile_3rd, 805, 10); 
}

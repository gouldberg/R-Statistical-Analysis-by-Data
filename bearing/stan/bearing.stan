data { 
	int<lower=0>  N;    
	real<lower=0> x[N];  
}
parameters {
	real<lower=0>    sigma;
}

transformed parameters {
	real<lower=0> sigmasq;
     	sigmasq = pow(sigma, 2);
}

model {
	for(n in 1:N){
		x[n] ~ normal(145.00, sigma);
	}
}

generated quantities{
	real<lower=0,upper=1> sigmasq_over;
	real<lower=0,upper=1> sigmasq_over2;
 
	sigmasq_over  = step(sigmasq - 0.10);
	sigmasq_over2 = step(sigmasq - 0.15);
}


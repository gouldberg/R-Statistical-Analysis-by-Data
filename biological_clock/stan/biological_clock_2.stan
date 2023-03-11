data { 
	int<lower=0>  N1;    
	int<lower=0>  N2;    
	real<lower=0> x1[N1]; 
	real<lower=0> x2[N2]; 
}

parameters {
	real          mu1;
	real          mu2;
	real<lower=0> sigma;
}

transformed parameters {
	real<lower=0> sigmasq;

	sigmasq = pow(sigma, 2);
}

model {
	x1 ~ normal(mu1, sigma);
	x2 ~ normal(mu2, sigma);
}

generated quantities{
	real delta;
	real delta_over;
	real delta_over1;

	delta = mu2 - mu1;
	delta_over = step(delta);
	delta_over1 = delta > 1 ? 1 : 0 ;
}
